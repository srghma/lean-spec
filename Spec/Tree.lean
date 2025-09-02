import Spec.Utils

namespace Spec

def Name := String
deriving Repr, BEq, Ord, Inhabited, Hashable

def Name.toString (n : Name) : String := n

def NumberOfTests := Nat
deriving Repr, BEq, Ord, Inhabited, Hashable

def ActionWith (m : Type → Type) a := a → m Unit

-- abbrev ExampleFn (m : Type → Type) a := (ActionWith m a → m Unit) → m Unit
def ExampleFn (m : Type → Type) a := a → m Unit

instance : Repr (ExampleFn (m : Type → Type) (a : Type)) where
  reprPrec _ _ := "<example function>"
instance : BEq (ExampleFn (m : Type → Type) (a : Type)) where
  beq _ _ := true

structure PathItem where
  index : Nat
  name : Option Name
  deriving Repr, BEq, Ord, Inhabited, Hashable

abbrev Path := Array PathItem
abbrev TestLocator := Path × Name

structure Item (m : Type → Type) (a : Type) where
  isFocused : Bool
  isParallelizable : Option Bool -- TODO: bool with true as default. Or leave Option and add to config `parallelizationMode := force_not_parallel | not_parallel_by_default | parallel_by_default (default) | force_parallel (just for fun?)`
  example_ : ExampleFn m a
  deriving Repr, BEq

-- Helper function to set parallelizable flag
def Item.setParallelizable (value : Bool) (item : Item m a) : Item m a :=
  { item with isParallelizable := value }

inductive Tree (nodeAnnotation computeNode a : Type)
  | node
    : nodeAnnotation ⊕ computeNode
    → Array (Tree nodeAnnotation computeNode a)
    → Tree nodeAnnotation computeNode a
  | leaf : nodeAnnotation → Option a → Tree nodeAnnotation computeNode a
  deriving Repr, BEq, Inhabited

instance [Repr n] [Repr c] [Repr a] : ToString (Tree n c a) where
  toString tree := repr tree |>.pretty

-- #guard toString (Tree.node
--   (Sum.inl "Root")
--   #[
--   Tree.leaf "Test1" (some "✓"),
--   Tree.node (Sum.inr "Group") #[Tree.leaf "Test2" none]
-- ]) = r#"Spec.Tree.node
--   (Sum.inl "Root")
--   #[Spec.Tree.leaf "Test1" (some "✓"), Spec.Tree.node (Sum.inr "Group") #[Spec.Tree.leaf "Test2" none]]"#

namespace Tree

def mapTreeAnnotations (f : n → m) : Tree n c a → Tree m c a
  | node ec cs => node (ec.map f id) (cs.map (mapTreeAnnotations f))
  | leaf n a   => leaf (f n) a

def bimapTreeWithPaths
    (g : Array n → a → b)
    (f : Array n → c → d)
    : Tree n a c → Tree n b d :=
  let rec go (path : Array n) : Tree n a c → Tree n b d
    | node e children =>
      let path' := e.elim (fun n => path.push n) (fun _ => path)
      let e' := e.map id (g path')
      node e' (children.map (go path'))
    | leaf n x =>
      leaf n (x.map (f (path.push n)))
  go #[]

def countTests : Array (Tree n c a) → Nat
  | trees =>
    trees.foldl (fun acc t =>
      let rec go : Tree n c a → Nat
        | node _ cs => cs.foldl (fun s t => s + go t) 0
        | leaf _ _  => acc + 1
      acc + go t) 0

def sizeOf : Tree n c (Item m a) → Nat
  | Tree.leaf _ _ => 1
  | Tree.node _ cs => 1 + cs.foldl (fun acc t => acc + sizeOf t) 0

partial def isAllParallelizable : Tree n c (Item m a) → Bool
  | node _ cs => cs.all isAllParallelizable
  | leaf _ x => x.map (·.isParallelizable == some true) |>.getD true

-- termination_by t => Tree.sizeOf t
-- decreasing_by
--   simp only [Tree.sizeOf]
--   rename_i a_1 a_2
--   simp_all only [gt_iff_lt]
--   cases a_1 with
--   | inl val =>
--     -- We need a_2 ∈ cs (the children array)
--     have h_mem : a_2 ∈ cs.toList := sorry -- a_2 is one of the children in recursive call
--     have sum_all := List.foldl_sum_nonneg (List.map Tree.sizeOf cs.toList)
--     -- Tree.sizeOf a_2 ≤ sum of Tree.sizeOf over cs
--     have h_le : Tree.sizeOf a_2 ≤ List.foldl (· + ·) 0 (List.map Tree.sizeOf cs.toList) :=
--       List.le_of_mem_foldl_sum h_mem (List.map Tree.sizeOf cs.toList)
--     -- So
--     calc
--       Tree.sizeOf a_2 < 1 + List.foldl (· + ·) 0 (List.map Tree.sizeOf cs.toList) := by
--         apply Nat.lt_add_of_pos_right
--         exact Nat.zero_lt_one
--   | inr val_1 => aesop?

def filterTree (p : n → Option a → Bool) : Tree n c a → Option (Tree n c a)
  | node e children =>
    let children' := children.filterMap (filterTree p)
    if children'.isEmpty then none else some (node e children')
  | leaf n x =>
    if p n x then some (leaf n x) else none

def filterTrees (p : n → Option a → Bool) (xs : Array (Tree n c a)) : Array (Tree n c a) :=
  xs.filterMap (filterTree p)

def discardUnfocused : Array (Tree n c (Item m a)) → Array (Tree n c (Item m a)) :=
  let isFocused (_name : n) (x : Option (Item m a)) :=
    match x with
    | some item => item.isFocused
    | none => false
  fun trees =>
    let focused := filterTrees isFocused trees
    if focused.isEmpty then trees else focused

partial def annotateWithPathsGo (path : Array PathItem) (index : Nat) : Tree Name c a → Tree TestLocator c a
  | node e cs =>
    let name := e.elim some (fun _ => none)
    let path' := path.push ⟨index, name⟩
    node (e.map (path, ·) id) (cs.mapIdx (annotateWithPathsGo path'))
  | leaf n x => leaf (path, n) x

  -- termination_by t => t
  -- decreasing_by
  --   simp [Tree.sizeOf]
  --   cases e with
  --   | inl val =>
  --     simp_all only [Sum.inl.sizeOf_spec]
  --     aesop?
  --   | inr val_1 =>
  --     simp_all only [Sum.inr.sizeOf_spec, sizeOf_default, Nat.add_zero, Nat.reduceAdd]
  --     grind

def annotateWithPaths
    (trees : Array (Tree Name c a))
    : Array (Tree TestLocator c a) :=
  trees.mapIdx (annotateWithPathsGo #[])

end Tree

def parentSuiteName (path : Path) : Array Name :=
  path.filterMap (·.name)

def parentSuite (path : Path) : Option TestLocator :=
  match Array.popLast? path with
  | some (init, last) =>
    last.name.map fun name => (init, name)
  | none => none

def Item.modifyAroundAction
    (f : (a → m Unit) → b → m Unit)
    (item : Item m a) : Item m b :=
  { isFocused := item.isFocused
  , isParallelizable := item.isParallelizable
  -- , example_ := fun around => item.example_ (around ∘ f)
  , example_ := f item.example_
  }

end Spec
