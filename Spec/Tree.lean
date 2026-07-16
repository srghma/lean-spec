module
import Init.System.IO
public import Std.Time.Time.Unit.Millisecond
public import Std.Data.TreeSet.Lemmas
public import Spec.Assert

@[expose] public section
namespace Spec

structure NodeOpts where
  focus : Bool := false
  timeoutMs? : Option Std.Time.Millisecond.Offset := none
  deriving Repr, BEq

/-- The value associated with a child name in a `SpecTree`. -/
inductive SpecTreeData (α children : Type) where
  | group (opts : NodeOpts) (children : children)
  | test (opts : NodeOpts) (action : α → IO Unit)
  | pending

-- same as `def RawSpecTree (α : Type) := List (String × SpecTreeData α (RawSpecTree α))`
inductive RawSpecTree (α : Type) where
  | nil : RawSpecTree α
  | cons
    (name : String)
    (data : SpecTreeData α (RawSpecTree α))
    (tail : RawSpecTree α) :
    RawSpecTree α
  deriving Inhabited

abbrev RawSpecForest (α : Type) := List (RawSpecTree α)

inductive NameIsNotInRawSpecTree {α : Type} (name : String) : RawSpecTree α → Prop where
  | nil : NameIsNotInRawSpecTree name .nil
  | cons
      (head : String)
      (data : SpecTreeData α (RawSpecTree α))
      (tail : RawSpecTree α)
      (headIsNotName : name ≠ head)
      (tailProof : NameIsNotInRawSpecTree name tail) :
      NameIsNotInRawSpecTree name (.cons head data tail)

inductive NameIsNotInRawSpecForest {α : Type} (name : String) : RawSpecForest α → Prop where
  | nil : NameIsNotInRawSpecForest name []
  | cons (hd : RawSpecTree α) (tl : RawSpecForest α) :
      NameIsNotInRawSpecTree name hd →
      NameIsNotInRawSpecForest name tl →
      NameIsNotInRawSpecForest name (hd :: tl)

mutual
  def RawSpecTree.WF {α : Type} : RawSpecTree α → Prop
    | .nil => True
    | .cons name data tail =>
        NameIsNotInRawSpecTree name tail ∧ RawSpecTreeData.WF data ∧ RawSpecTree.WF tail

  def RawSpecTreeData.WF {α : Type} : SpecTreeData α (RawSpecTree α) → Prop
    | .group _ children => RawSpecTree.WF children
    | .test _ _ => True
    | .pending => True
end

def RawSpecForest.WF {α : Type} : RawSpecForest α → Prop
  | [] => True
  | hd :: tl => hd.WF ∧ RawSpecForest.WF tl

structure SpecTree (α : Type) where
  toRawSpecTree : RawSpecTree α
  isWF : toRawSpecTree.WF := by trivial

def SpecTree.empty : SpecTree α := ⟨.nil, True.intro⟩

structure SpecForest (α : Type) where
  toRawSpecForest : RawSpecForest α
  isWF : toRawSpecForest.WF := by trivial

def SpecForest.empty : SpecForest α := ⟨[], True.intro⟩

def RawSpecTree.NamesAreIn (tree : RawSpecTree α) (names : Std.TreeSet String) : Prop :=
  match tree with
  | .nil => True
  | .cons name _ tail => name ∈ names ∧ tail.NamesAreIn names

theorem RawSpecTree.nameIsNotIn_of_namesAreIn {name : String} {tree : RawSpecTree α}
    {names : Std.TreeSet String} (namesAreIn : tree.NamesAreIn names) (nameIsNotIn : name ∉ names) :
    NameIsNotInRawSpecTree name tree :=
  match tree, namesAreIn with
  | .nil, _ => .nil
  | .cons head data tail, ⟨headIsIn, tailNamesAreIn⟩ =>
      .cons head data tail (by
        intro h
        apply nameIsNotIn
        simpa [h] using headIsIn)
        (RawSpecTree.nameIsNotIn_of_namesAreIn tailNamesAreIn nameIsNotIn)

theorem RawSpecTree.NamesAreIn.weaken {tree : RawSpecTree α} {names names' : Std.TreeSet String}
    (namesAreIn : tree.NamesAreIn names) (subset : ∀ name, name ∈ names → name ∈ names') :
    tree.NamesAreIn names' :=
  match tree, namesAreIn with
  | .nil, _ => trivial
  | .cons name _ _, ⟨nameIsIn, tailNamesAreIn⟩ =>
      ⟨subset name nameIsIn, RawSpecTree.NamesAreIn.weaken tailNamesAreIn subset⟩

structure SpecBuilder (α : Type) where
  toRawSpecTree : RawSpecTree α
  names : Std.TreeSet String
  namesAreIn : toRawSpecTree.NamesAreIn names
  isWF : toRawSpecTree.WF

def SpecBuilder.empty : SpecBuilder α := ⟨.nil, ∅, trivial, trivial⟩

def SpecBuilder.toSpecTree (builder : SpecBuilder α) : SpecTree α :=
  ⟨builder.toRawSpecTree, builder.isWF⟩

inductive SpecMErrors where
  | duplicateNames (names : Std.TreeSet String)
  | userError (message : String)

instance : CoeOut String SpecMErrors where
  coe := .userError

/-- During construction the tree is stored in reverse order, with sibling names indexed for fast
duplicate detection. -/
abbrev SpecM (α : Type) := ExceptT SpecMErrors (StateM (SpecBuilder α))
abbrev Spec := SpecM Unit Unit

instance : MonadExceptOf String (SpecM α) where
  throw message := throwThe SpecMErrors (.userError message)
  tryCatch x handle :=
    tryCatchThe SpecMErrors x fun
    | .userError message => handle message
    | err => throwThe SpecMErrors err

def RawSpecTree.nameIsUnique (name : String) : RawSpecTree α → Bool
  | .nil => true
  | .cons head _ tail => (name != head) && tail.nameIsUnique name

theorem RawSpecTree.nameIsUnique_sound (name : String) (tree : RawSpecTree α)
    (h : tree.nameIsUnique name = true) : NameIsNotInRawSpecTree name tree :=
  match tree with
  | .nil => .nil
  | .cons head data tail =>
    have h' : (name != head) = true ∧ tail.nameIsUnique name = true := by
      simpa only [RawSpecTree.nameIsUnique, Bool.and_eq_true] using h
    .cons head data tail (by
      intro hEq
      have hh := h'.1
      simp only [hEq, bne_self_eq_false, Bool.false_eq_true] at hh) (tail.nameIsUnique_sound name h'.2)
termination_by sizeOf tree

def RawSpecTree.mapAction {α β : Type} (f : (α → IO Unit) → (β → IO Unit)) : RawSpecTree α → RawSpecTree β
  | .nil => .nil
  | .cons name (.group opts children) tail =>
      .cons name (.group opts (children.mapAction f)) (tail.mapAction f)
  | .cons name (.test opts action) tail => .cons name (.test opts (f action)) (tail.mapAction f)
  | .cons name .pending tail => .cons name .pending (tail.mapAction f)
termination_by tree => sizeOf tree

theorem RawSpecTree.NamesAreIn.mapAction {α β : Type} (f : (α → IO Unit) → (β → IO Unit))
    {tree : RawSpecTree α} {names : Std.TreeSet String} :
    tree.NamesAreIn names → (tree.mapAction f).NamesAreIn names :=
  match tree with
  | .nil => fun _ => by simp [RawSpecTree.mapAction, RawSpecTree.NamesAreIn]
  | .cons _ (.group _ _) _ => fun ⟨nameIsIn, tailNamesAreIn⟩ =>
      by simpa [RawSpecTree.mapAction, RawSpecTree.NamesAreIn] using
        ⟨nameIsIn, RawSpecTree.NamesAreIn.mapAction f tailNamesAreIn⟩
  | .cons _ (.test _ _) _ => fun ⟨nameIsIn, tailNamesAreIn⟩ =>
      by simpa [RawSpecTree.mapAction, RawSpecTree.NamesAreIn] using
        ⟨nameIsIn, RawSpecTree.NamesAreIn.mapAction f tailNamesAreIn⟩
  | .cons _ .pending _ => fun ⟨nameIsIn, tailNamesAreIn⟩ =>
      by simpa [RawSpecTree.mapAction, RawSpecTree.NamesAreIn] using
        ⟨nameIsIn, RawSpecTree.NamesAreIn.mapAction f tailNamesAreIn⟩

theorem NameIsUniquePerGroup.mapAction {α β : Type} (f : (α → IO Unit) → (β → IO Unit)) {name : String}
    {tree : RawSpecTree α} : NameIsNotInRawSpecTree name tree → NameIsNotInRawSpecTree name (tree.mapAction f)
  | .nil => by simpa [RawSpecTree.mapAction] using (.nil : NameIsNotInRawSpecTree name .nil)
  | .cons head (.group opts children) tail h p =>
      by simpa [RawSpecTree.mapAction] using
        (.cons head (.group opts (children.mapAction f)) (tail.mapAction f) h
          (NameIsUniquePerGroup.mapAction f p))
  | .cons head (.test opts action) tail h p =>
      by simpa [RawSpecTree.mapAction] using
        (.cons head (.test opts (f action)) (tail.mapAction f) h
          (NameIsUniquePerGroup.mapAction f p))
  | .cons head .pending tail h p =>
      by simpa [RawSpecTree.mapAction] using
        (.cons head .pending (tail.mapAction f) h (NameIsUniquePerGroup.mapAction f p))

theorem RawSpecTree.WF.mapAction {α β : Type} (f : (α → IO Unit) → (β → IO Unit))
    {tree : RawSpecTree α} (h : tree.WF) : (tree.mapAction f).WF :=
  match tree, h with
  | .nil, _ => by simp [RawSpecTree.mapAction, RawSpecTree.WF]
  | .cons name (.group opts children) tail, ⟨unique, childrenWF, tailWF⟩ =>
      by simpa [RawSpecTree.mapAction, RawSpecTree.WF] using
        ⟨NameIsUniquePerGroup.mapAction f unique,
          RawSpecTree.WF.mapAction f childrenWF,
          RawSpecTree.WF.mapAction f tailWF⟩
  | .cons name (.test opts action) tail, ⟨unique, _, tailWF⟩ =>
      by simpa [RawSpecTree.mapAction, RawSpecTree.WF] using
        ⟨NameIsUniquePerGroup.mapAction f unique, trivial, RawSpecTree.WF.mapAction f tailWF⟩
  | .cons name .pending tail, ⟨unique, _, tailWF⟩ =>
      by simpa [RawSpecTree.mapAction, RawSpecTree.WF] using
        ⟨NameIsUniquePerGroup.mapAction f unique, trivial, RawSpecTree.WF.mapAction f tailWF⟩
termination_by sizeOf tree

def SpecTree.mapAction (f : (α → IO Unit) → (β → IO Unit)) (tree : SpecTree α) : SpecTree β :=
  ⟨tree.toRawSpecTree.mapAction f, tree.isWF.mapAction f⟩

def SpecBuilder.mapAction (f : (α → IO Unit) → (β → IO Unit)) (builder : SpecBuilder α) : SpecBuilder β :=
  ⟨builder.toRawSpecTree.mapAction f, builder.names, builder.namesAreIn.mapAction f,
    builder.isWF.mapAction f⟩

theorem RawSpecForest.WF.mapAction {α β : Type} (f : (α → IO Unit) → (β → IO Unit))
    {forest : RawSpecForest α} (h : forest.WF) :
    RawSpecForest.WF (forest.map (RawSpecTree.mapAction f)) :=
  match forest, h with
  | [], _ => trivial
  | _ :: _, ⟨treeWF, treesWF⟩ =>
      ⟨treeWF.mapAction f, RawSpecForest.WF.mapAction f treesWF⟩

def SpecForest.mapAction (f : (α → IO Unit) → (β → IO Unit)) (forest : SpecForest α) : SpecForest β :=
  ⟨forest.toRawSpecForest.map (RawSpecTree.mapAction f), forest.isWF.mapAction f⟩

def SpecTree.add (tree : SpecTree α) (name : String) (data : SpecTreeData α (RawSpecTree α))
    (dataWF : RawSpecTreeData.WF data) : Except SpecMErrors (SpecTree α) :=
  if h : tree.toRawSpecTree.nameIsUnique name then
    pure ⟨.cons name data tree.toRawSpecTree,
      ⟨tree.toRawSpecTree.nameIsUnique_sound name h, dataWF, tree.isWF⟩⟩
  else
    throw (.duplicateNames (Std.TreeSet.insert (∅ : Std.TreeSet String) name))

def SpecTree.mergeRaw (tree : SpecTree α) (other : RawSpecTree α)
    (otherWF : other.WF) : Except SpecMErrors (SpecTree α) :=
  match other, otherWF with
  | .nil, _ => pure tree
  | .cons name data tail, ⟨_, dataWF, tailWF⟩ => do
    let tree ← tree.mergeRaw tail tailWF
    tree.add name data dataWF

def SpecTree.merge (tree other : SpecTree α) : Except SpecMErrors (SpecTree α) :=
  tree.mergeRaw other.toRawSpecTree other.isWF

def SpecForest.toSpecTree (forest : SpecForest α) : Except SpecMErrors (SpecTree α) :=
  go forest.toRawSpecForest forest.isWF
where
  go : (forest : RawSpecForest α) → forest.WF → Except SpecMErrors (SpecTree α)
    | [], _ => pure .empty
    | tree :: trees, ⟨treeWF, treesWF⟩ => do
      let trees ← go trees treesWF
      trees.mergeRaw tree treeWF

def SpecForest.merge (forest other : SpecForest α) : Except SpecMErrors (SpecForest α) := do
  let tree ← forest.toSpecTree
  let other ← other.toSpecTree
  let tree ← tree.merge other
  pure ⟨[tree.toRawSpecTree], ⟨tree.isWF, trivial⟩⟩

def SpecBuilder.add (builder : SpecBuilder α) (name : String)
    (data : SpecTreeData α (RawSpecTree α)) (dataWF : RawSpecTreeData.WF data) :
    Except SpecMErrors (SpecBuilder α) :=
  let pair := builder.names.containsThenInsert name
  let duplicate := pair.1
  let names := pair.2
  if h : duplicate then
    throw (.duplicateNames (Std.TreeSet.insert (∅ : Std.TreeSet String) name))
  else
    have namesEq : names = builder.names.insert name := by
      simp [names, pair]
    have containsFalse : builder.names.contains name = false := by
      have duplicateFalse : duplicate = false := by simpa using h
      calc
        builder.names.contains name = duplicate := by
          simp [duplicate, pair, Std.TreeSet.containsThenInsert_fst]
        _ = false := duplicateFalse
    have nameIsNotIn : name ∉ builder.names := by
      simp [Membership.mem, containsFalse]
    pure ⟨.cons name data builder.toRawSpecTree, names, by
      constructor
      · rw [namesEq]
        exact Std.TreeSet.mem_insert_self
      · rw [namesEq]
        exact RawSpecTree.NamesAreIn.weaken builder.namesAreIn fun _ isIn =>
          Std.TreeSet.mem_insert.mpr (.inr isIn),
      ⟨RawSpecTree.nameIsNotIn_of_namesAreIn builder.namesAreIn nameIsNotIn,
        dataWF, builder.isWF⟩⟩

def SpecBuilder.mergeRaw (builder : SpecBuilder α) (other : RawSpecTree α)
    (otherWF : other.WF) : Except SpecMErrors (SpecBuilder α) :=
  match other, otherWF with
  | .nil, _ => pure builder
  | .cons name data tail, ⟨_, dataWF, tailWF⟩ => do
    let builder ← builder.mergeRaw tail tailWF
    builder.add name data dataWF

def SpecBuilder.merge (builder other : SpecBuilder α) : Except SpecMErrors (SpecBuilder α) :=
  builder.mergeRaw other.toRawSpecTree other.isWF

def SpecM.add (name : String) (data : SpecTreeData α (RawSpecTree α))
    (dataWF : RawSpecTreeData.WF data) : SpecM α Unit := do
  let builder ← get
  set (← builder.add name data dataWF)

def RawSpecTree.focus : RawSpecTree α → RawSpecTree α
  | .nil => .nil
  | .cons name (.group opts children) tail =>
      .cons name (.group { opts with focus := true } children) tail.focus
  | .cons name (.test opts action) tail =>
      .cons name (.test { opts with focus := true } action) tail.focus
  | .cons name .pending tail => .cons name .pending tail.focus

theorem NameIsUniquePerGroup.focus {name : String} {tree : RawSpecTree α} :
    NameIsNotInRawSpecTree name tree → NameIsNotInRawSpecTree name tree.focus
  | .nil => .nil
  | .cons head (.group opts children) tail h p =>
      .cons head (.group { opts with focus := true } children) tail.focus h
        (NameIsUniquePerGroup.focus p)
  | .cons head (.test opts action) tail h p =>
      .cons head (.test { opts with focus := true } action) tail.focus h
        (NameIsUniquePerGroup.focus p)
  | .cons head .pending tail h p =>
      .cons head .pending tail.focus h (NameIsUniquePerGroup.focus p)

theorem RawSpecTree.WF.focus {tree : RawSpecTree α} (h : tree.WF) : tree.focus.WF := by
  cases tree with
  | nil => trivial
  | cons name data tail =>
    rcases h with ⟨unique, dataWF, tailWF⟩
    cases data with
    | group => exact ⟨NameIsUniquePerGroup.focus unique, dataWF, tailWF.focus⟩
    | test => exact ⟨NameIsUniquePerGroup.focus unique, trivial, tailWF.focus⟩
    | pending => exact ⟨NameIsUniquePerGroup.focus unique, trivial, tailWF.focus⟩

def SpecTree.focus (tree : SpecTree α) : SpecTree α :=
  ⟨tree.toRawSpecTree.focus, tree.isWF.focus⟩

theorem RawSpecTree.NamesAreIn.focus {tree : RawSpecTree α} {names : Std.TreeSet String}
    (namesAreIn : tree.NamesAreIn names) : tree.focus.NamesAreIn names :=
  match tree, namesAreIn with
  | .nil, _ => trivial
  | .cons _ (.group _ _) _, ⟨nameIsIn, tailNamesAreIn⟩ =>
      ⟨nameIsIn, RawSpecTree.NamesAreIn.focus tailNamesAreIn⟩
  | .cons _ (.test _ _) _, ⟨nameIsIn, tailNamesAreIn⟩ =>
      ⟨nameIsIn, RawSpecTree.NamesAreIn.focus tailNamesAreIn⟩
  | .cons _ .pending _, ⟨nameIsIn, tailNamesAreIn⟩ =>
      ⟨nameIsIn, RawSpecTree.NamesAreIn.focus tailNamesAreIn⟩

def SpecBuilder.focus (builder : SpecBuilder α) : SpecBuilder α :=
  ⟨builder.toRawSpecTree.focus, builder.names, builder.namesAreIn.focus, builder.isWF.focus⟩

/-! ## Building blocks: describe / it / pending -/

def describe (name : String)
    (specs : SpecM α Unit) (focus : Bool := false) (timeoutMs? : Option Std.Time.Millisecond.Offset := none) : SpecM α Unit := do
  let (result, children) := specs.run .empty
  match result with
  | .ok _ => pure ()
  | .error err => throwThe SpecMErrors err
  let children := children.toSpecTree
  SpecM.add name (.group { focus, timeoutMs? } children.toRawSpecTree) children.isWF

class ItAction (α : Type) (action : Type) where
  add : NodeOpts → String → action → SpecM α Unit

instance : ItAction Unit (IO Unit) where
  add opts name action := SpecM.add name (.test opts fun _ => action) trivial

instance {α : Type} : ItAction α (α → IO Unit) where
  add opts name action := SpecM.add name (.test opts action) trivial

/-- `it` accepts `IO Unit` or `α → IO Unit`. -/
def it (name : String)
    {action : Type} [ItAction α action] (a : action) (focus : Bool := false) (timeoutMs? : Option Std.Time.Millisecond.Offset := none) : SpecM α Unit :=
  ItAction.add { focus, timeoutMs? } name a

/-- `pending` creates a pending spec item. -/
def pending (name : String) : SpecM α Unit :=
  SpecM.add name .pending trivial

def focus (specs : SpecM α Unit) : SpecM α Unit := do
  let (result, children) := specs.run .empty
  match result with
  | .ok _ => pure ()
  | .error err => throwThe SpecMErrors err
  let children := children.focus
  let builder ← get
  set (← builder.merge children)

/-! ## `only` detection -/

partial def RawSpecTree.hasOnly : RawSpecTree α → Bool
  | .nil => false
  | .cons _ (.group opts children) tail => opts.focus || children.hasOnly || tail.hasOnly
  | .cons _ (.test opts _) tail => opts.focus || tail.hasOnly
  | .cons _ .pending tail => tail.hasOnly

def SpecTree.hasOnly (tree : SpecTree α) : Bool := tree.toRawSpecTree.hasOnly

end Spec
end
