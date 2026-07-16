module
public import Spec.Config
public import Spec.Tree
public import Std.Data.HashSet.Basic

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

def isFailure : Outcome → Bool
  | .failure _ => true
  | _ => false

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
  timeoutMs? : Option Nat
  kind : Sum (α → IO Unit) Unit  -- `inl action` = test, `inr ()` = pending
  selected : Bool

partial def flatten (globalHasOnly : Bool) (ancestorOnly : Bool) (inheritedTimeout? : Option Nat)
    (path : Array String)
    (t : SpecTree Unit) : Array (Leaf Unit) :=
  match t with
  | .group name opts children =>
    let currentOnly := ancestorOnly || opts.focus
    let currentTimeout? :=
      match opts.timeoutMs? with
      | some ms => some ms.toInt.toNat
      | none => inheritedTimeout?
    if globalHasOnly && !currentOnly && !t.hasOnly then #[]
    else children.foldl (init := #[]) fun acc c =>
      acc ++ flatten globalHasOnly currentOnly currentTimeout? (path.push name) c
  | .test name opts action =>
    let sel := !globalHasOnly || ancestorOnly || opts.focus
    let effectiveTimeout? :=
      match opts.timeoutMs? with
      | some ms => some ms.toInt.toNat
      | none => inheritedTimeout?
    #[{ path, name, timeoutMs? := effectiveTimeout?, kind := .inl action, selected := sel }]
  | .pending name =>
    let sel := !globalHasOnly || ancestorOnly
    #[{ path, name, timeoutMs? := none, kind := .inr (), selected := sel }]

/-- Full dotted name used for `--example` filtering. -/
def Leaf.fullName (l : Leaf α) : String :=
  String.intercalate " » " (l.path.toList ++ [l.name])

def matchesFilters (cfg : Config) (failedNames : Std.HashSet String) (l : Leaf α) : Bool :=
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
