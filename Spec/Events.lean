module
public import Spec.Config
public import Spec.Tree

@[expose] public section

namespace Spec

/-! ## Results & events -/

inductive Outcome where
  | success
  | failure (err : String)
  | pending
  deriving Inhabited

structure ItemResult where
  /-- Suite names from the root down to (but not including) the leaf. -/
  path : Array String
  name : String
  outcome : Outcome
  durationMs : Nat
  deriving Inhabited

/-- A reporter is fed each finished item plus a summary at the end.
Per-item printing happens atomically (one `reportItem` call at a time), so
parallel runs stay readable even though completion order is non-deterministic. -/
structure Reporter where
  /-- Called once before any items run. -/
  start : (total : Nat) → IO Unit := fun _ => pure ()
  /-- Called once per finished item, atomically. -/
  reportItem : ItemResult → IO Unit
  /-- Called once after all items finish. -/
  reportSummary : Array ItemResult → IO Unit := fun _ => pure ()

/-- Reporters that need state (e.g. "last printed suite", a counter) are built
in `IO` so they can allocate refs. `Bool` selects ANSI color output. -/
abbrev ReporterBuilder := Bool → IO Reporter

/-! ## Flattening the tree into runnable items -/

/-- A leaf paired with the suite path and whether it's selected by `only`. -/
structure Leaf (α : Type) where
  path : Array String
  name : String
  kind : Sum (α → IO Unit) Unit  -- `inl action` = test, `inr ()` = pending
  selected : Bool

partial def flatten (globalHasOnly : Bool) (ancestorOnly : Bool) (path : Array String)
    (t : SpecTree Unit) : Array (Leaf Unit) :=
  match t with
  | .group name isOnly children =>
    let currentOnly := ancestorOnly || isOnly
    if globalHasOnly && !currentOnly && !t.hasOnly then #[]
    else children.foldl (init := #[]) fun acc c =>
      acc ++ flatten globalHasOnly currentOnly (path.push name) c
  | .test name isOnly action =>
    let sel := !globalHasOnly || ancestorOnly || isOnly
    #[{ path, name, kind := .inl action, selected := sel }]
  | .pending name =>
    let sel := !globalHasOnly || ancestorOnly
    #[{ path, name, kind := .inr (), selected := sel }]

/-- Full dotted name used for `--example` filtering. -/
def Leaf.fullName (l : Leaf α) : String :=
  String.intercalate " » " (l.path.toList ++ [l.name])

def matchesFilters (cfg : Config) (failedNames : Array String) (l : Leaf α) : Bool :=
  let full := l.fullName
  let exMatch := match cfg.example? with
    | some s => (full.splitOn s).length > 1
    | none => true
  let eMatch := match cfg.exampleMatches? with
    | some s => (full.splitOn s).length > 1
    | none => true
  let failMatch := !cfg.onlyFailures || failedNames.contains full
  l.selected && exMatch && eMatch && failMatch

end Spec
end
