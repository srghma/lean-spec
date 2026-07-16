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

/-- Run `before` before every spec item and `after` even if the item throws. -/
def each (before : IO Unit := pure ())
    (after : IO Unit := pure ())
    (specs : SpecM α Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  modify fun trees => children.foldl (init := trees) fun trees child =>
    trees.push (mapAction (fun run a => do
      before
      try
        run a
      finally
        after) child)

/-- Acquire a value for each item, run the item with it, and optionally release it afterward. -/
def withValue (acquire : α → IO β)
    (release : β → IO Unit := fun _ => pure ())
    (specs : SpecM β Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  modify fun trees => children.foldl (init := trees) fun trees child =>
    trees.push (mapAction (fun run a => do
      let b ← acquire a
      try
        run b
      finally
        release b) child)

/-- `aroundWith` wraps each item, mapping the incoming value. -/
def aroundWith (f : (β → IO Unit) → (α → IO Unit)) (specs : SpecM β Unit) : SpecM α Unit := do
  let (_, children) := specs.run #[]
  modify fun trees => children.foldl (init := trees) fun trees child =>
    trees.push (mapAction (fun run a => f run a) child)

/-- Run `action` before every spec item (no value passed in). -/
def before_ (action : IO Unit) (specs : SpecM α Unit) : SpecM α Unit :=
  each (before := action) (specs := specs)

/-- Run `action` after every spec item (even when it throws). -/
def after_ (action : IO Unit) (specs : SpecM α Unit) : SpecM α Unit :=
  each (after := action) (specs := specs)

/-- `around_ withResource spec` wraps each item with setup/teardown. -/
def around_ (around : IO Unit → IO Unit) (specs : SpecM α Unit) : SpecM α Unit :=
  aroundWith (fun run => fun a => around (run a)) (specs := specs)

/-- `before` produces a value that is passed to each item. -/
def before (acquire : IO β) (specs : SpecM β Unit) : SpecM α Unit :=
  withValue (fun _ => do acquire) (specs := specs)

/-- `after` receives the value (from an enclosing `before`/`around`) for teardown. -/
def after (release : β → IO Unit) (specs : SpecM β Unit) : SpecM β Unit :=
  withValue (fun b => pure b) (release := release) (specs := specs)

/-- `around` performs setup, runs the item with the acquired value, then teardown. -/
def around (around : (β → IO Unit) → IO Unit) (specs : SpecM β Unit) : SpecM α Unit :=
  aroundWith (fun run => fun _ => around run) (specs := specs)

/-- `beforeWith` maps the incoming value into a new one. -/
def beforeWith (f : α → IO β) (specs : SpecM β Unit) : SpecM α Unit :=
  withValue f (specs := specs)

end Spec
end
