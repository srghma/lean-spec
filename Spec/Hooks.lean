module
public import Spec.Tree

@[expose] public section

namespace Spec

/-! ## Hooks

Hooks transform the action of every leaf inside the wrapped spec. We map
over the tree, replacing the `α → IO Unit` actions.
-/

mutual
  partial def mapAction (f : (α → IO Unit) → (β → IO Unit)) : SpecTree α → SpecTree β
    | .group n opts c => .group n opts (mapActionArr f c)
    | .test n opts a => .test n opts (f a)
    | .pending n => .pending n

  partial def mapActionArr (f : (α → IO Unit) → (β → IO Unit)) (c : Array (SpecTree α)) :
      Array (SpecTree β) :=
    c.map (mapAction f)
end

/-- Run `action` before every spec item (no value passed in). -/
def before_ (action : IO Unit) (specs : SpecM α Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  modify fun s => s ++ children.map (mapAction fun run a => do action; run a)

/-- Run `action` after every spec item (even when it throws). -/
def after_ (action : IO Unit) (specs : SpecM α Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  modify fun s => s ++ children.map (mapAction fun run a => do
    try run a finally action)

/-- `around_ withResource spec` wraps each item with setup/teardown. -/
def around_ (around : IO Unit → IO Unit) (specs : SpecM α Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  modify fun s => s ++ children.map (mapAction fun run a => around (run a))

/-- `before` produces a value that is passed to each item. -/
def before (acquire : IO β) (specs : SpecM β Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  modify fun s => s ++ children.map (mapAction fun run _ => do let b ← acquire; run b)

/-- `after` receives the value (from an enclosing `before`/`around`) for teardown. -/
def after (release : β → IO Unit) (specs : SpecM β Unit) : SpecM β Unit := do
  let (_, children) := specs.run #[]
  modify fun s => s ++ children.map (mapAction fun run b => do
    try run b finally release b)

/-- `around` performs setup, runs the item with the acquired value, then teardown. -/
def around (around : (β → IO Unit) → IO Unit) (specs : SpecM β Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  modify fun s => s ++ children.map (mapAction fun run _ => around run)

/-- `beforeWith` maps the incoming value into a new one. -/
def beforeWith (f : α → IO β) (specs : SpecM β Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  modify fun s => s ++ children.map (mapAction fun run a => do let b ← f a; run b)

/-- `aroundWith` wraps each item, mapping the incoming value. -/
def aroundWith (f : (β → IO Unit) → (α → IO Unit)) (specs : SpecM β Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  modify fun s => s ++ children.map (mapAction fun run a => f run a)

end Spec
end
