import Spec.Tree
import Spec.Result

namespace Spec

structure Summary where
  passed  : Nat := 0
  failed  : Nat := 0
  pending : Nat := 0
deriving Inhabited, Repr

def Summary.count (s : Summary) : Nat := s.passed + s.failed + s.pending

def Summary.append (s1 s2 : Summary) : Summary :=
  { passed := s1.passed + s2.passed,
    failed := s1.failed + s2.failed,
    pending := s1.pending + s2.pending }

partial def Summary.summarize {n} (trees : Array (Tree n a Result)) : Summary :=
  trees.foldl
    (fun acc tree =>
      let current :=
        match tree with
        | Tree.leaf _ (some (Result.success _ _)) => { passed := 1 }
        | Tree.leaf _ (some (Result.failure _)) => { failed := 1 }
        | Tree.leaf _ none => { pending := 1 }
        | Tree.node _ subtrees => summarize subtrees
      Summary.append acc current)
    {}
-- termination_by trees

def Summary.successful? (groups : Array (Tree n a Result)) : Bool :=
  (summarize groups).failed == 0
