module
public import Spec.Assert.Base

@[expose] public section

namespace Spec.Assert

class SortedEq (γ : Type _) where
  sortedEq : γ → γ → Bool

instance {α : Type _} [Ord α] [BEq α] : SortedEq (Array α) where
  sortedEq := fun arr1 arr2 =>
    arr1.insertionSort (fun a b => compare a b == Ordering.lt) ==
      arr2.insertionSort (fun a b => compare a b == Ordering.lt)

instance {α : Type _} [Ord α] [BEq α] : SortedEq (List α) where
  sortedEq := fun xs ys =>
    xs.mergeSort (fun a b => compare a b == Ordering.lt) ==
      ys.mergeSort (fun a b => compare a b == Ordering.lt)

def assertEq [BEq α] [Repr α] (name : String) (expected actual : α) : IO Unit := do
  unless actual == expected do
    IO.println s!"❌ {name} failed: expected {repr expected}, got {repr actual}"
    throw <| IO.Error.userError s!"Assertion failed: {name}"

def assertEqAfterSort [SortedEq γ] [Repr γ] (name : String) (expected actual : γ) : IO Unit := do
  unless SortedEq.sortedEq actual expected do
    IO.println s!"❌ {name} failed: expected {repr expected}, got {repr actual}"
    throw <| IO.Error.userError s!"Assertion failed: {name}"

def shouldContain [Membership α β] [ToString α] [ToString β]
    (actual : β) (expected : α) [Decidable (expected ∈ actual)] : IO Unit := do
  unless expected ∈ actual do
    Spec.Assert.fail s!"❌ {expected} ∉ {actual}"

def shouldNotContain [Membership α β] [ToString α] [ToString β]
    (actual : β) (expected : α) [Decidable (expected ∈ actual)] : IO Unit := do
  if expected ∈ actual then
    Spec.Assert.fail s!"❌ {expected} ∈ {actual}"
  else
    pure ()

def shouldBeEmpty [BEq α] [EmptyCollection α] [Repr α] (actual : α) : IO Unit := do
  unless actual == EmptyCollection.emptyCollection do
    Spec.Assert.fail s!"❌ expected empty value, got {repr actual}"

def shouldNotBeEmpty [BEq α] [EmptyCollection α] [Repr α] (actual : α) : IO Unit := do
  if actual == EmptyCollection.emptyCollection then
    Spec.Assert.fail "❌ expected non-empty value"
  else
    pure ()

def assertIsNotEmpty [BEq α] [EmptyCollection α] [Repr α] (name : String) (actual : α) : IO Unit := do
  unless actual != EmptyCollection.emptyCollection do
    IO.println s!"❌ {name} failed: expected non-empty value, got empty."
    throw <| IO.Error.userError s!"Assertion failed: {name}"

def assertIsEmpty [BEq α] [EmptyCollection α] [Repr α] (name : String) (actual : α) : IO Unit := do
  unless actual == EmptyCollection.emptyCollection do
    IO.println s!"❌ {name} failed: expected empty value, got {repr actual}."
    throw <| IO.Error.userError s!"Assertion failed: {name}"

def assertThrows {α} (name : String) (ioAction : IO α) : IO Unit := do
  let result ← try
    discard ioAction
    pure none
  catch e =>
    IO.println s!"✅ {name} caught expected error: {e}"
    pure (some e)
  unless result.isSome do
    IO.println s!"❌ {name} failed: expected an error, but no error was thrown."
    throw <| IO.Error.userError s!"Assertion failed: {name}"

end Spec.Assert
end
