module
import Init.System.IO
public import Std.Time.Time.Unit.Millisecond
public import Std.Data.TreeSet.Basic
public import Spec.Assert

@[expose] public section
namespace Spec

structure NodeOpts where
  focus : Bool := false
  timeoutMs? : Option Std.Time.Millisecond.Offset := none
  deriving Repr, BEq

inductive SpecTree (α : Type) where
  | group (name : String) (opts : NodeOpts) (children : List (SpecTree α))
  | test (name : String) (opts : NodeOpts) (action : α → IO Unit)
  | pending (name : String)

/-- The sibling trees at the root of a spec or inside a `describe` block. -/
abbrev SpecForest (α : Type) := List (SpecTree α)

inductive SpecMErrors where
  | duplicateNames (names : Std.TreeSet String)
  | userError (message : String)

instance : CoeOut String SpecMErrors where
  coe := .userError

/-- During construction the forest is stored in reverse order, so adding a node is `O(1)`. -/
abbrev SpecM (α : Type) := ExceptT SpecMErrors (StateM (SpecForest α))
abbrev Spec := SpecM Unit Unit

instance : MonadExceptOf String (SpecM α) where
  throw message := throwThe SpecMErrors (.userError message)
  tryCatch x handle :=
    tryCatchThe SpecMErrors x fun
    | .userError message => handle message
    | err => throwThe SpecMErrors err

def SpecTree.name : SpecTree α → String
  | .group name _ _ => name
  | .test name _ _ => name
  | .pending name => name

def SpecForest.duplicateNames (forest : SpecForest α) : Std.TreeSet String :=
  go forest ∅ ∅
where
  go : SpecForest α → Std.TreeSet String → Std.TreeSet String → Std.TreeSet String
    | [], _, duplicates => duplicates
    | tree :: trees, seen, duplicates =>
      let name := tree.name
      let duplicates := if seen.contains name then duplicates.insert name else duplicates
      go trees (seen.insert name) duplicates

mutual
  def SpecForest.validate : SpecForest α → Except SpecMErrors Unit
    | forest => do
      let duplicates := forest.duplicateNames
      if !duplicates.isEmpty then
        throw (.duplicateNames duplicates)
      for tree in forest do
        tree.validate

  def SpecTree.validate : SpecTree α → Except SpecMErrors Unit
    | .group _ _ children => SpecForest.validate children
    | .test _ _ _ => pure ()
    | .pending _ => pure ()
end

def SpecTree.mapAction (f : (α → IO Unit) → (β → IO Unit)) : SpecTree α → SpecTree β
  | .group name opts children => .group name opts (children.map (mapAction f))
  | .test name opts action => .test name opts (f action)
  | .pending name => .pending name

def SpecTree.focus : SpecTree α → SpecTree α
  | .group name opts children => .group name { opts with focus := true } children
  | .test name opts action => .test name { opts with focus := true } action
  | .pending name => .pending name

/-! ## Building blocks: describe / it / pending -/

def describe (name : String)
    (specs : SpecM α Unit) (focus : Bool := false)
    (timeoutMs? : Option Std.Time.Millisecond.Offset := none) : SpecM α Unit := do
  let (result, children) := specs.run []
  match result with
  | .ok _ => pure ()
  | .error err => throwThe SpecMErrors err
  let children := children.reverse
  SpecForest.validate children
  modify fun forest => .group name { focus, timeoutMs? } children :: forest

class ItAction (α : Type) (action : Type) where
  add : NodeOpts → String → action → SpecM α Unit

instance : ItAction Unit (IO Unit) where
  add opts name action := modify fun forest => .test name opts (fun _ => action) :: forest

instance {α : Type} : ItAction α (α → IO Unit) where
  add opts name action := modify fun forest => .test name opts action :: forest

/-- `it` accepts `IO Unit` or `α → IO Unit`. -/
def it (name : String) {action : Type} [ItAction α action] (a : action)
    (focus : Bool := false)
    (timeoutMs? : Option Std.Time.Millisecond.Offset := none) : SpecM α Unit :=
  ItAction.add { focus, timeoutMs? } name a

/-- `pending` creates a pending spec item. -/
def pending (name : String) : SpecM α Unit :=
  modify fun forest => .pending name :: forest

def focus (specs : SpecM α Unit) : SpecM α Unit := do
  let (result, children) := specs.run []
  match result with
  | .ok _ => pure ()
  | .error err => throwThe SpecMErrors err
  modify fun forest => children.foldr (fun tree forest => tree.focus :: forest) forest

/-! ## `only` detection -/

partial def SpecTree.hasOnly : SpecTree α → Bool
  | .group _ opts children => opts.focus || children.any hasOnly
  | .test _ opts _ => opts.focus
  | .pending _ => false

end Spec
end
