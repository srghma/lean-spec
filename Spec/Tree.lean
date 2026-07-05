module
import Init.System.IO
public import Spec.Assert

@[expose] public section

namespace Spec

/-! ## Spec tree -/

structure NodeOpts where
  focus : Bool := false
  timeout : Option Nat := none
  deriving Inhabited, Repr, BEq

mutual
  /-- A spec item carries an action that receives the value produced by the
  enclosing `before`/`around` hooks (`Unit` when there are none). -/
  inductive SpecTree (α : Type) where
    | group (name : String) (opts : NodeOpts) (children : Array (SpecTree α))
    | test (name : String) (opts : NodeOpts) (action : α → IO Unit)
    | pending (name : String)
    deriving Nonempty
end

/-- The writer/state monad we accumulate spec items in.

It is parameterised by the input type `α` threaded in from hooks. -/
abbrev SpecM (α : Type) := StateM (Array (SpecTree α))

/-- A full top-level spec produces no input (`Unit`). -/
abbrev Spec := SpecM Unit Unit

/-! ## Building blocks: describe / it / pending -/

def describe (focus : Bool := false) (timeout : Option Nat := none) (name : String)
    (specs : SpecM α Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  modify fun s => s.push (SpecTree.group name { focus, timeout } children)

/-- `it` for specs that take an input value from a hook. -/
def itWith (focus : Bool := false) (timeout : Option Nat := none) (name : String)
    (action : α → IO Unit) : SpecM α Unit :=
  modify fun s => s.push (SpecTree.test name { focus, timeout } action)

/-- `it` for the common case where the spec takes no input (`Unit`). -/
def it (focus : Bool := false) (timeout : Option Nat := none) (name : String)
    (action : IO Unit) : SpecM Unit Unit :=
  itWith (focus := focus) (timeout := timeout) name (fun _ => action)

def pending (name : String) : SpecM α Unit :=
  modify fun s => s.push (SpecTree.pending name)

def focus (specs : SpecM α Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  let focusedChildren := children.map fun
    | .group n opts c => .group n { opts with focus := true } c
    | .test n opts a => .test n { opts with focus := true } a
    | .pending n => .pending n
  modify fun s => s ++ focusedChildren

/-! ## `only` detection -/

partial def SpecTree.hasOnly : SpecTree α → Bool
  | .group _ opts children => opts.focus || children.any hasOnly
  | .test _ opts _ => opts.focus
  | .pending _ => false

end Spec
end
