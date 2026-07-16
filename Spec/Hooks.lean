module
public import Spec.Tree

@[expose] public section

namespace Spec

/-! ## Hooks

Hooks transform the action of every leaf inside the wrapped spec. We map
over the tree, replacing the `α → IO Unit` actions.
-/

/-- Run `before` before every spec item and `after` even if the item throws. -/
def each (before : IO Unit := pure ())
    (after : IO Unit := pure ())
    (specs : SpecM α Unit) : SpecM α Unit := do
  let (result, children) := specs.run []
  match result with
  | .ok _ => pure ()
  | .error err => throwThe SpecMErrors err
  modify fun forest => children.foldr (fun tree forest =>
    tree.mapAction (fun run a => do
      before
      try
        run a
      finally
        after) :: forest) forest

/-- Acquire a value for each item, run the item with it, and optionally release it afterward. -/
def withValue (acquire : α → IO β)
    (release : β → IO Unit := fun _ => pure ())
    (specs : SpecM β Unit) : SpecM α Unit := do
  let (result, children) := specs.run []
  match result with
  | .ok _ => pure ()
  | .error err => throwThe SpecMErrors err
  modify fun forest => children.foldr (fun tree forest =>
    tree.mapAction (fun run a => do
      let b ← acquire a
      try
        run b
      finally
        release b) :: forest) forest

/-- `aroundWith` wraps each item, mapping the incoming value. -/
def aroundWith (f : (β → IO Unit) → (α → IO Unit)) (specs : SpecM β Unit) : SpecM α Unit := do
  let (result, children) := specs.run []
  match result with
  | .ok _ => pure ()
  | .error err => throwThe SpecMErrors err
  modify fun forest => children.foldr (fun tree forest =>
    tree.mapAction (fun run a => f run a) :: forest) forest

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
