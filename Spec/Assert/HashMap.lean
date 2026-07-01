module
public import Spec.Assert.Base
import Std.Data.HashMap.Basic

@[expose] public section

namespace Spec.Assert.HashMap

def shouldContain [BEq α] [Hashable α] [Repr α] [Repr β] (m : Std.HashMap α β) (k : α) : IO Unit :=
  unless (m.contains k) do
    Spec.Assert.fail s!"❌ {repr k} ∉ {repr m}"

def shouldNotContain [BEq α] [Hashable α] [Repr α] [Repr β] (m : Std.HashMap α β) (k : α) : IO Unit :=
  if m.contains k then
    Spec.Assert.fail s!"❌ {repr k} ∈ {repr m}"
  else
    pure ()

def shouldBeEmpty [BEq α] [Hashable α] [Repr α] [Repr β] (m : Std.HashMap α β) : IO Unit :=
  unless m.isEmpty do
    Spec.Assert.fail s!"❌ expected empty hash map, got {repr m}"

def shouldNotBeEmpty [BEq α] [Hashable α] [Repr α] [Repr β] (m : Std.HashMap α β) : IO Unit :=
  if m.isEmpty then
    Spec.Assert.fail "❌ expected non-empty hash map"
  else
    pure ()

def assertEq [BEq α] [Hashable α] [BEq β] [Repr α] [Repr β]
    (name : String) (expected actual : Std.HashMap α β) : IO Unit := do
  if actual.size != expected.size then
    IO.println s!"❌ {name} failed: expected {repr expected}, got {repr actual}"
    throw <| IO.Error.userError s!"Assertion failed: {name}"

  let mismatch :=
    expected.fold (init := none) fun bad k v =>
      match bad with
      | some _ => bad
      | none =>
        match actual[k]? with
        | some actualV =>
          if actualV == v then
            none
          else
            some s!"key {repr k} expected {repr v}, got {repr actualV}"
        | none =>
          some s!"missing key {repr k} with expected value {repr v}"

  match mismatch with
  | none => pure ()
  | some msg =>
    IO.println s!"❌ {name} failed: {msg}"
    throw <| IO.Error.userError s!"Assertion failed: {name}"

def assertIsNotEmpty [BEq α] [Hashable α] [Repr α] [Repr β]
    (name : String) (m : Std.HashMap α β) : IO Unit := do
  unless !m.isEmpty do
    IO.println s!"❌ {name} failed: expected non-empty hash map, got empty."
    throw <| IO.Error.userError s!"Assertion failed: {name}"

def assertIsEmpty [BEq α] [Hashable α] [Repr α] [Repr β]
    (name : String) (m : Std.HashMap α β) : IO Unit := do
  unless m.isEmpty do
    IO.println s!"❌ {name} failed: expected empty hash map, got {repr m}."
    throw <| IO.Error.userError s!"Assertion failed: {name}"

end Spec.Assert.HashMap
end
