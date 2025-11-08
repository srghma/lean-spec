import Spec.Tree
/- import Spec.WriterT -/
import Mathlib.Control.Monad.Writer

namespace Spec

-- Spec tree type alias
abbrev SpecTree (g : Type -> Type) (i : Type) := Tree Name (ActionWith g i) (Item g i)

-- SpecT monad transformer
abbrev SpecT (g : Type -> Type) (i : Type) (m : Type -> Type) (a: Type) := WriterT (Array (SpecTree g i)) m a

end Spec

-- Spec type alias
abbrev Spec := Spec.SpecT IO Unit Id

namespace Spec

-- Map functions
def mapSpec [Functor m'] (f : ∀ {α}, m α → m' α) (spec : SpecT g i m a) : SpecT g i m' a :=
  WriterT.mk (f spec.run)

def mapSpecTree [Functor m]
  (xg : ∀ {α}, m α → m' α)
  (xf : SpecTree g i → SpecTree g' i')
  (spec : SpecT g i m a)
  : SpecT g' i' m' a := Id.run do
  let inner : m (a × Array (SpecTree g i)) := spec.run
  let f : a × Array (SpecTree g i) → a × Array (SpecTree g' i') := fun (a, trees) => (a, trees.map xf)
  let inner2 : m (a × Array (SpecTree g' i')) := Functor.map f inner
  let transformed : m' (a × Array (SpecTree g' i')) :=
    xg inner2
  return WriterT.mk transformed

-- Computation type for hoisting
inductive ComputationType where
  | cleanUpWithContext : Array Name → ComputationType
  | testWithName : Array Name → ComputationType
  deriving Inhabited, Repr

-- Bimap for trees with paths
def bimapTreeWithPaths (onCleanUp : Array Name → ActionWith a i → ActionWith b i)
    (onTest : Array Name → Item a i → Item b i) : SpecTree a i → SpecTree b i :=
  let rec go (path : Array Name) : SpecTree a i → SpecTree b i
    | Tree.leaf name item =>
        Tree.leaf name (item.map (onTest (path ++ #[name])))
    | Tree.node (Sum.inl suiteName) children =>
        let newPath := path ++ #[suiteName]
        Tree.node (Sum.inl suiteName) (children.map (go newPath))
    | Tree.node (Sum.inr action) children =>
        Tree.node (Sum.inr (onCleanUp path action)) (children.map (go path))
  go #[]

-- Hoist spec function
def hoistSpec [Functor m] [Monad m'] (onM : ∀ {α}, m α → m' α) (f : ComputationType -> forall {x}, a x -> b x)
    (spec : SpecT a i m a') : SpecT b i m' a' :=
  mapSpecTree onM (bimapTreeWithPaths onCleanUp onTest) spec
  where
    onCleanUp (name : Array Name) (around' : ActionWith a i) : ActionWith b i :=
      fun i => f (ComputationType.cleanUpWithContext name) (around' i)
    onTest (name : Array Name) (item : Item a i) : Item b i :=
      { item with
        -- example_ := fun g => g (f (ComputationType.testWithName name) ∘ item.example_ ∘ (fun q f => f q)) }
        example_ := fun g => f (ComputationType.testWithName name) (item.example_ g) }

def anyFocused (t : SpecTree g i) : Bool :=
  match t with
  -- | Tree.node _ children => Array.any children anyFocused -- doesnt work like this
  -- | Tree.node _ children => List.any children.toList anyFocused -- doesnt work like this
  | Tree.node _ children => anyFocusedList children.toList
  | Tree.leaf _ (some item) => item.isFocused
  | Tree.leaf _ none => false
  -- termination_by structural t
  where
    anyFocusedList : List (SpecTree g i) → Bool
    | [] => false
    | x :: xs => anyFocused x || anyFocusedList xs


def filterFocused : SpecTree g i → SpecTree g i
  | Tree.leaf name (some item) =>
      if item.isFocused then Tree.leaf name (some item) else Tree.leaf name none
  | Tree.leaf name none => Tree.leaf name none
  | Tree.node label children => Tree.node label (children.map filterFocused)

-- Discard unfocused tests if focused tests exist
def discardUnfocused (specs : Array (SpecTree g i)) : Array (SpecTree g i) :=
  let hasFocused := specs.any anyFocused
  if hasFocused then
    specs.map filterFocused
  else
    specs

-- Collect all specs
def collect [Monad m] [Functor m] (spec : SpecT g i m a) : m (Array (SpecTree g i)) := do
  let (_, specs) ← spec.run
  pure (discardUnfocused specs)

-- Example class for different test types
class Example (t : Type) (arg : Type) (m : Type → Type) where
  -- evaluateExample : t → (ActionWith m arg → m Unit) → m Unit
  evaluateExample : t → ExampleFn m arg

-- Instance for function types
instance [Monad m] : Example (arg → m Unit) arg m where
  -- evaluateExample t around' := around' t
  evaluateExample t := t

-- Instance for unit computations
instance [Monad m] : Example (m Unit) Unit m where
  -- evaluateExample t around' := around' (fun _ => t)
  evaluateExample t := fun .unit => t

-- Focus warning (simplified in Lean)
class FocusWarning where

def setAllFocused (focused : Bool) : SpecTree g i → SpecTree g i
  | Tree.leaf name (some item) => Tree.leaf name (some { item with isFocused := focused })
  | Tree.leaf name none => Tree.leaf name none
  | Tree.node label children => Tree.node label (children.map (setAllFocused focused))

-- Focus function
def focus [Monad m] [FocusWarning] : SpecT g i m a -> SpecT g i m a :=
  mapSpecTree id (fun specs =>
    if anyFocused specs then
      specs
    else
      setAllFocused true specs
  )

-- Describe function
def describe [Monad m] (name : Name) (spec : SpecT g i m a) : SpecT g i m a :=
  WriterT.mk do
    let (a, t) ← WriterT.run spec
    pure (a, #[Tree.node (Sum.inl name) t])

-- Describe only (focused describe)
def describeOnly [Monad m] [FocusWarning] (name : Name) (spec : SpecT g i m a) : SpecT g i m a :=
  focus (describe name spec)

-- Helper function to set parallelizable flag
def SpecTree.setParallelizable (value : Bool) : SpecTree g i → SpecTree g i
  | Tree.leaf name (some item) =>
      Tree.leaf name (some (Item.setParallelizable value item))
  | Tree.leaf name none => Tree.leaf name none
  | Tree.node label children => Tree.node label (children.map (setParallelizable value))

-- Parallel marking
def parallel [Monad m] (spec : SpecT g i m a) : SpecT g i m a :=
  mapSpecTree id (SpecTree.setParallelizable true) spec

-- Sequential marking
def sequential [Monad m] (spec : SpecT g i m a) : SpecT g i m a :=
  mapSpecTree id (SpecTree.setParallelizable false) spec

-- Pending test
def pending [Monad m] (name : Name) : SpecT g i m Unit :=
  tell #[Tree.leaf name none]

-- Pending test with ignored body
def pending' [Monad m] (name : Name) (_body : g Unit) : SpecT g i m Unit :=
  pending name

-- It function for creating tests
def it [Monad m] [Example t arg g] (name : Name) (test : t) : SpecT g arg m Unit :=
  tell #[Tree.leaf name (some {
    isParallelizable := none,
    isFocused := false,
    example_ := Example.evaluateExample test
  })]

-- It only (focused test)
def itOnly [Monad m] [FocusWarning] [Example t arg g] (name : String) (test : t) : SpecT g arg m Unit :=
  focus (it name test)

def SpecTree.modifyAroundAction (f : ActionWith g i → ActionWith g i') : SpecTree g i → SpecTree g i'
  | Tree.leaf name item => Tree.leaf name (item.map (Spec.Item.modifyAroundAction f))
  | Tree.node (Sum.inl label) children =>
      Tree.node (Sum.inl label) (children.map (modifyAroundAction f))
  | Tree.node (Sum.inr around') children =>
      Tree.node (Sum.inr (f around')) (children.map (modifyAroundAction f))

-- Hook functions
def aroundWith [Monad m] (action : ActionWith g i → ActionWith g i')
    (spec : SpecT g i m a) : SpecT g i' m a :=
  mapSpecTree id (SpecTree.modifyAroundAction action) spec

-- Around with unit action
def around_ [Monad m] (action : g Unit → g Unit) (spec : SpecT g i m a) : SpecT g i m a :=
  aroundWith (fun e a => action (e a)) spec

-- After hook
def after [Monad m] [Functor g] [MonadFinally g] (action : ActionWith g i) (spec : SpecT g i m a) : SpecT g i m a :=
  aroundWith (fun e x => tryFinally (e x) (action x)) spec

-- After hook with unit action
def after_ [Monad m] [Functor g] [MonadFinally g] (action : g Unit) (spec : SpecT g i m a) : SpecT g i m a :=
  after (fun _ => action) spec

-- Around hook
def around [Monad m] (action : ActionWith g i → g Unit) (spec : SpecT g i m a) : SpecT g Unit m a :=
  aroundWith (fun e _ => action e) spec

-- Before hook
def before [Monad m] [Monad g] (action : g i) (spec : SpecT g i m a) : SpecT g Unit m a :=
  around (fun e => do let i ← action; e i) spec

-- Before hook with unit action
def before_ [Monad m] [Monad g] (action : g Unit) (spec : SpecT g i m a) : SpecT g i m a :=
  around_ (fun comp => do action; comp) spec

-- Before with transformation
def beforeWith [Monad m] [Monad g] (action : i' → g i) (spec : SpecT g i m a) : SpecT g i' m a :=
  aroundWith (fun e x => do let i ← action x; e i) spec

-- Memoization for beforeAll
inductive Memoized (a : Type) where
  | empty : Memoized a
  | memoized : a → Memoized a
  | failed : String → Memoized a
  deriving Inhabited

-- Memoize function (simplified without AVar)
def memoize [Monad m] [MonadExcept IO.Error m] [MonadLiftT (ST IO.RealWorld) m] (var : IO.Ref (Memoized a)) (action : m a) : m a := do
  let state ← var.get
  match state with
  | Memoized.failed msg => throw (IO.userError s!"exception in beforeAll-hook: {msg}")
  | Memoized.memoized x => pure x
  | Memoized.empty => do
      try
        let result ← action
        var.set (Memoized.memoized result)
        pure result
      catch e =>
        var.set (Memoized.failed (toString e))
        throw e

-- Before all hook
def beforeAll [MonadLiftT BaseIO Id] [MonadExcept IO.Error g] [MonadLiftT (ST IO.RealWorld) g] [MonadLiftT IO m] [Monad m] [Monad g] (action : g i) (spec : SpecT g i m a) : SpecT g Unit m a := Id.run do
  let var ← liftM (IO.mkRef Memoized.empty)
  before (memoize var action) spec

-- Before all with unit action
def beforeAll_ [MonadLiftT BaseIO Id] [MonadExcept IO.Error g] [MonadLiftT (ST IO.RealWorld) g] [MonadLiftT IO m] [Monad m] [Monad g] [MonadLiftT IO m] [Monad g] (action : g Unit) (spec : SpecT g i m a) : SpecT g i m a := Id.run do
  let var ← liftM (IO.mkRef Memoized.empty)
  before_ (memoize var action) spec

-- After all hook
def afterAll [Monad m] (action : ActionWith g i) (spec : SpecT g i m a) : SpecT g i m a :=
  mapSpecTree id (fun tree => Tree.node (Sum.inr action) #[tree]) spec

-- After all with unit action
def afterAll_ [Monad m] (action : g Unit) (spec : SpecT g i m a) : SpecT g i m a :=
  afterAll (fun _ => action) spec

/-
-- Example DSL usage
def myexample : Spec Unit := do
  describe "Math operations" do
    it "should add numbers correctly" do
      (pure () : Id Unit) -- test implementation

  describe "String operations" do
    pending "should concatenate strings"
-/
