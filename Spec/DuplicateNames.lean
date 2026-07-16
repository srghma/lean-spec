module
public import Spec.Tree
public import Std.Data.HashMap.Basic

@[expose] public section

namespace Spec

inductive SpecTreeItemKind where
  | it
  | describe

structure DuplicateSpecTreeName where
  name : String
  itCount : Nat := 0
  describeCount : Nat := 0
  deriving Inhabited

structure DuplicateSpecTreeNamesInGroup where
  path : Array String
  names : Array DuplicateSpecTreeName

def DuplicateSpecTreeName.add (duplicate : DuplicateSpecTreeName) (kind : SpecTreeItemKind) :
    DuplicateSpecTreeName :=
  match kind with
  | .it => { duplicate with itCount := duplicate.itCount + 1 }
  | .describe => { duplicate with describeCount := duplicate.describeCount + 1 }

partial def SpecTree.findDuplicateNames (trees : Array (SpecTree α)) :
    Array DuplicateSpecTreeNamesInGroup :=
  go #[] trees #[]
where
  go (path : Array String) (trees : Array (SpecTree α))
    (groups : Array DuplicateSpecTreeNamesInGroup) : Array DuplicateSpecTreeNamesInGroup :=
    Id.run do
      let mut indices : Std.HashMap String Nat := Std.HashMap.emptyWithCapacity
      let mut names : Array DuplicateSpecTreeName := #[]
      let mut children : Array (String × Array (SpecTree α)) := #[]
      for tree in trees do
        match tree with
        | .pending _ => pure ()
        | .test name _ _ =>
          match indices[name]? with
          | some index => names := names.set! index (names[index]!.add .it)
          | none =>
            indices := indices.insert name names.size
            names := names.push { name, itCount := 1 }
        | .group name _ nested =>
          match indices[name]? with
          | some index => names := names.set! index (names[index]!.add .describe)
          | none =>
            indices := indices.insert name names.size
            names := names.push { name, describeCount := 1 }
          children := children.push (name, nested)
      let duplicates := names.filter fun duplicate => duplicate.itCount + duplicate.describeCount > 1
      let groups := if duplicates.isEmpty then groups else groups.push { path, names := duplicates }
      return children.foldl (fun groups (name, nested) => go (path.push name) nested groups) groups

def DuplicateSpecTreeName.format (duplicate : DuplicateSpecTreeName) : String :=
  let count := duplicate.itCount + duplicate.describeCount
  let times := if count == 1 then "time" else "times"
  let kinds :=
    (if duplicate.itCount == 0 then [] else [s!"{duplicate.itCount} 🧪 it"]) ++
    (if duplicate.describeCount == 0 then [] else [s!"{duplicate.describeCount} 📂 describe"])
  s!"{duplicate.name} (duplicated {count} {times}. {String.intercalate " + " kinds})"

def DuplicateSpecTreeNamesInGroup.format (group : DuplicateSpecTreeNamesInGroup) : String :=
  let path := if group.path.isEmpty then "root" else String.intercalate " >> " group.path.toList
  s!"{path}: {String.intercalate ", " (group.names.toList.map DuplicateSpecTreeName.format)}"

def SpecTree.duplicateNamesWarning (trees : Array (SpecTree α)) : Option String :=
  let groups := SpecTree.findDuplicateNames trees
  if groups.isEmpty then none
  else some <| String.intercalate "\n"
    (["⚠️ Found duplicate names of 🧪 it/📂 describe items.",
      "This makes timings inside .lean-spec-timings-and-last-failures inconsistent, so only the first declared item will be written there.",
      "Please make names unique per group:"] ++
      groups.toList.map fun group => s!"  {group.format}")

end Spec
end
