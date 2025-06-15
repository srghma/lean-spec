import Spec.Tree
import Spec.WriterT

namespace Spec

-- Spec tree type alias
abbrev SpecTree g i := Tree String (ActionWith g i) (Item g i)

-- SpecT monad transformer
def SpecT g i m a := WriterT (Array (SpecTree g i)) m a

-- Spec type alias
abbrev Spec := SpecT IO PUnit Id

-- Map functions
def mapSpec [Functor m'] (f : ∀ {α}, m α → m' α) (spec : SpecT g i m a) : SpecT g i m' a :=
  fun s => f (spec s)

def mapSpecTree [Functor m'] (xg : ∀ {α}, m α → m' α) (xf : SpecTree g i → SpecTree g' i')
    (spec : SpecT g i m a) : SpecT g' i' m' a :=
  fun s_orig : Array (SpecTree g' i') =>
    let x := StateT.run spec sorry
    sorry
    -- let mval : m' (a × Array (SpecTree g i)) := xg (spec s_orig)
    -- let x : m' (a × Array (SpecTree g' i')) := Functor.map (fun (a, specs) => (a, specs.map xf)) mval
    -- x

-- Computation type for hoisting
inductive ComputationType where
  | cleanUpWithContext : Array String → ComputationType
  | testWithName : Array String → ComputationType
  deriving Inhabited, Repr

-- Bimap for trees with paths
def bimapTreeWithPaths (onCleanUp : Array String → ActionWith a i → ActionWith b i)
    (onTest : Array String → Item a i → Item b i) : SpecTree a i → SpecTree b i :=
  let rec go (path : Array String) : SpecTree a i → SpecTree b i
    | Tree.leaf name item =>
        Tree.leaf name (item.map (onTest (path ++ #[name])))
    | Tree.node (Either.left suiteName) children =>
        let newPath := path ++ #[suiteName]
        Tree.node (Either.left suiteName) (children.map (go newPath))
    | Tree.node (Either.right action) children =>
        Tree.node (Either.right (onCleanUp path action)) (children.map (go path))
  go #[]

-- Hoist spec function
def hoistSpec [Monad m'] (onM : m ~> m') (f : ComputationType → a ~> b)
    (spec : SpecT a i m a') : SpecT b i m' a' :=
  mapSpecTree onM (bimapTreeWithPaths onCleanUp onTest) spec
  where
    onCleanUp (name : Array String) (around' : ActionWith a i) : ActionWith b i :=
      fun i => f (ComputationType.cleanUpWithContext name) (around' i)
    onTest (name : Array String) (item : Item a i) : Item b i :=
      { item with
        example := fun g => g (f (ComputationType.testWithName name) ∘ item.example ∘ (· |>.)) }

-- Collect all specs
def collect [Functor m] (spec : SpecT g i m a) : m (Array (SpecTree g i)) := do
  let (_, specs) ← spec.runSpecT.runWriterT
  pure (discardUnfocused specs)

-- Discard unfocused tests if focused tests exist
def discardUnfocused (specs : Array (SpecTree g i)) : Array (SpecTree g i) :=
  let hasFocused := specs.any (anyFocused)
  if hasFocused then
    specs.map filterFocused
  else
    specs
  where
    anyFocused : SpecTree g i → Bool
      | Tree.leaf _ (some item) => item.isFocused
      | Tree.leaf _ none => false
      | Tree.node _ children => children.any anyFocused

    filterFocused : SpecTree g i → SpecTree g i
      | Tree.leaf name (some item) =>
          if item.isFocused then Tree.leaf name (some item) else Tree.leaf name none
      | Tree.leaf name none => Tree.leaf name none
      | Tree.node label children => Tree.node label (children.map filterFocused)

-- Example class for different test types
class Example (t : Type) (arg : Type) (m : Type → Type) where
  evaluateExample : t → (ActionWith m arg → m Unit) → m Unit

-- Instance for function types
instance [Monad m] : Example (arg → m Unit) arg m where
  evaluateExample t around' := around' t

-- Instance for unit computations
instance [Monad m] : Example (m Unit) Unit m where
  evaluateExample t around' := around' (fun _ => t)

-- Focus warning (simplified in Lean)
class FocusWarning where

-- Focus function
def focus [Monad m] [FocusWarning] (spec : SpecT g i m a) : SpecT g i m a :=
  mapSpecTree id (fun specs =>
    if specs.any (anyFocused) then
      specs
    else
      specs.map (setAllFocused true)
  ) spec
  where
    anyFocused : SpecTree g i → Bool
      | Tree.leaf _ (some item) => item.isFocused
      | Tree.leaf _ none => false
      | Tree.node _ children => children.any anyFocused

    setAllFocused (focused : Bool) : SpecTree g i → SpecTree g i
      | Tree.leaf name (some item) => Tree.leaf name (some { item with isFocused := focused })
      | Tree.leaf name none => Tree.leaf name none
      | Tree.node label children => Tree.node label (children.map (setAllFocused focused))

-- Describe function
def describe [Monad m] (name : String) (spec : SpecT g i m a) : SpecT g i m a := do
  let a ← spec
  tell #[Tree.node (Either.left name) []]
  pure a

-- Describe only (focused describe)
def describeOnly [Monad m] [FocusWarning] (name : String) (spec : SpecT g i m a) : SpecT g i m a :=
  focus (describe name spec)

-- Parallel marking
def parallel [Monad m] (spec : SpecT g i m a) : SpecT g i m a :=
  mapSpecTree id (Array.map (setParallelizable true)) spec

-- Sequential marking
def sequential [Monad m] (spec : SpecT g i m a) : SpecT g i m a :=
  mapSpecTree id (Array.map (setParallelizable false)) spec

-- Helper function to set parallelizable flag
def setParallelizable (value : Bool) : SpecTree g i → SpecTree g i
  | Tree.leaf name (some item) =>
      Tree.leaf name (some { item with isParallelizable := some value })
  | Tree.leaf name none => Tree.leaf name none
  | Tree.node label children => Tree.node label (children.map (setParallelizable value))

-- Pending test
def pending [Monad m] (name : String) : SpecT g i m Unit :=
  tell #[Tree.leaf name none]

-- Pending test with ignored body
def pending' [Monad m] (name : String) (body : g Unit) : SpecT g i m Unit :=
  pending name

-- It function for creating tests
def it [Monad m] [Example t arg g] (name : String) (test : t) : SpecT g arg m Unit :=
  tell #[Tree.leaf name (some {
    isParallelizable := none,
    isFocused := false,
    example := Example.evaluateExample test
  })]

-- It only (focused test)
def itOnly [Monad m] [FocusWarning] [Example t arg g] (name : String) (test : t) : SpecT g arg m Unit :=
  focus (it name test)

-- Hook functions
def aroundWith [Monad m] (action : ActionWith g i → ActionWith g i')
    (spec : SpecT g i m a) : SpecT g i' m a :=
  mapSpecTree id (Array.map (modifyAroundAction action)) spec
  where
    modifyAroundAction (f : ActionWith g i → ActionWith g i') : SpecTree g i → SpecTree g i'
      | Tree.leaf name item => Tree.leaf name item
      | Tree.node (Either.left label) children =>
          Tree.node (Either.left label) (children.map (modifyAroundAction f))
      | Tree.node (Either.right around') children =>
          Tree.node (Either.right (f around')) (children.map (modifyAroundAction f))

-- Around with unit action
def around_ [Monad m] (action : g Unit → g Unit) (spec : SpecT g i m a) : SpecT g i m a :=
  aroundWith (fun e a => action (e a)) spec

-- After hook
def after [Monad m] (action : ActionWith g i) (spec : SpecT g i m a) : SpecT g i m a :=
  aroundWith (fun e x => do e x; action x) spec

-- After hook with unit action
def after_ [Monad m] (action : g Unit) (spec : SpecT g i m a) : SpecT g i m a :=
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
def memoize [Monad m] (var : IO.Ref (Memoized a)) (action : m a) : m a := do
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
def beforeAll [MonadLiftT IO m] [Monad g] (action : g i) (spec : SpecT g i m a) : SpecT g Unit m a := do
  var ← liftM (IO.mkRef Memoized.empty)
  before (memoize var action) spec

-- Before all with unit action
def beforeAll_ [MonadLiftT IO m] [Monad g] (action : g Unit) (spec : SpecT g i m a) : SpecT g i m a := do
  var ← liftM (IO.mkRef Memoized.empty)
  before_ (memoize var action) spec

-- After all hook
def afterAll [Monad m] (action : ActionWith g i) (spec : SpecT g i m a) : SpecT g i m a :=
  mapSpecTree id (fun specs => #[Tree.node (Either.right action) specs.toList]) spec

-- After all with unit action
def afterAll_ [Monad m] (action : g Unit) (spec : SpecT g i m a) : SpecT g i m a :=
  afterAll (fun _ => action) spec

-- Example DSL usage
example : Spec Unit := do
  describe "Math operations" do
    it "should add numbers correctly" do
      pure () -- test implementation

    it "should multiply numbers correctly" do
      pure () -- test implementation

  describe "String operations" do
    pending "should concatenate strings"

    it "should reverse strings" do
      pure () -- test implementation
