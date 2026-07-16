module
public import Spec.Config
public import Spec.Speed
public import Spec.Tree
public import Std.Data.TreeSet.Basic

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
  previousDurationMs? : Option Nat := none
  speed? : Option Speed := none
  queueIndex : Nat := 0
  queueCount : Nat := 1
  nonParallel : Bool := false
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
  reportSummary : Array ItemResult → Option Nat → Nat → IO Unit := fun _ _ _ => pure ()

/-- Reporters that need state (e.g. "last printed suite", a counter) are built
in `IO` so they can allocate refs. `Bool` selects ANSI color output. -/
abbrev ReporterBuilder := Bool → IO Reporter

/-! ## Flattening the tree into runnable items -/

/-- A leaf paired with the suite path and whether it's selected by `only`. -/
structure Leaf (α : Type) where
  path : Array String
  name : String
  timeoutMs? : Option Nat
  parallel : Bool
  kind : Sum (α → IO Unit) Unit  -- `inl action` = test, `inr ()` = pending
  selected : Bool

inductive SpecTreeWithCachedOnly (α : Type) where
  | group (name : String) (opts : NodeOpts) (children : Array (SpecTreeWithCachedOnly α)) (hasOnly : Bool)
  | test (name : String) (opts : NodeOpts) (action : α → IO Unit) (hasOnly : Bool)
  | pending (name : String)
  deriving Nonempty

def SpecTreeWithCachedOnly.hasOnly : SpecTreeWithCachedOnly α → Bool
  | .group _ _ _ hasOnly => hasOnly
  | .test _ _ _ hasOnly => hasOnly
  | .pending _ => false

partial def SpecTree.cacheOnly : SpecTree α → SpecTreeWithCachedOnly α
  | .group name opts children =>
    let children := children.map cacheOnly
    .group name opts children (opts.focus || children.any SpecTreeWithCachedOnly.hasOnly)
  | .test name opts action => .test name opts action opts.focus
  | .pending name => .pending name

mutual
partial def flattenCachedOnlyInto (globalHasOnly : Bool) (ancestorOnly : Bool)
    (inheritedTimeout? : Option Nat) (path : Array String) (t : SpecTreeWithCachedOnly Unit)
    (leaves : Array (Leaf Unit)) : Array (Leaf Unit) :=
  flattenCachedOnlyIntoWithParallel globalHasOnly ancestorOnly inheritedTimeout? true path t leaves

partial def flattenCachedOnlyIntoWithParallel (globalHasOnly : Bool) (ancestorOnly : Bool)
    (inheritedTimeout? : Option Nat) (inheritedParallel : Bool) (path : Array String)
    (t : SpecTreeWithCachedOnly Unit) (leaves : Array (Leaf Unit)) : Array (Leaf Unit) :=
  match t with
  | .group name opts children hasOnly =>
    let currentOnly := ancestorOnly || opts.focus
    let currentTimeout? :=
      match opts.timeoutMs? with
      | some ms => some ms.toInt.toNat
      | none => inheritedTimeout?
    if globalHasOnly && !currentOnly && !hasOnly then leaves
    else children.foldl (init := leaves) fun leaves child =>
      flattenCachedOnlyIntoWithParallel globalHasOnly currentOnly currentTimeout?
        (inheritedParallel && opts.parallel) (path.push name) child leaves
  | .test name opts action _ =>
    let sel := !globalHasOnly || ancestorOnly || opts.focus
    let effectiveTimeout? :=
      match opts.timeoutMs? with
      | some ms => some ms.toInt.toNat
      | none => inheritedTimeout?
    leaves.push {
      path
      name
      timeoutMs? := effectiveTimeout?
      parallel := inheritedParallel && opts.parallel
      kind := .inl action
      selected := sel }
  | .pending name =>
    let sel := !globalHasOnly || ancestorOnly
    leaves.push {
      path
      name
      timeoutMs? := none
      parallel := inheritedParallel
      kind := .inr ()
      selected := sel }
end

def flatten (globalHasOnly : Bool) (ancestorOnly : Bool) (inheritedTimeout? : Option Nat)
    (path : Array String) (t : SpecTree Unit) : Array (Leaf Unit) :=
  flattenCachedOnlyInto globalHasOnly ancestorOnly inheritedTimeout? path t.cacheOnly #[]

def formatSpecName (path : Array String) (name : String) : String :=
  String.intercalate " » " (path.toList ++ [name])

def matchesFilters (cfg : Config) (failedNames : Std.TreeSet String) (fullName : String)
    (leaf : Leaf α) : Bool :=
  let exMatch := match cfg.example? with
    | some s => (fullName.find? s).isSome
    | none => true
  let eMatch := match cfg.exampleMatches? with
    | some s => (fullName.find? s).isSome
    | none => true
  let failMatch := !cfg.onlyFailures || failedNames.contains fullName
  leaf.selected && exMatch && eMatch && failMatch

end Spec
end
