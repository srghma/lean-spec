import Std
open Std

namespace Spec

-- | Fail the current test with an error message
def fail : String -> IO Unit := throw ∘ IO.userError

-- | Assert equality
def shouldEqual [BEq α] [ToString α] (x y : α) : IO Unit := unless (x == y) do fail s!"{x} ≠ {y}"
def shouldNotEqual [BEq α] [ToString α] (x y : α) : IO Unit := do if (x == y) then fail s!"{x} = {y}"

-- | Assert predicate holds
def shouldSatisfy [ToString α] (x : α) (pred : α → Bool) : IO Unit := unless (pred x) do fail s!"{x} doesn't satisfy predicate"
def shouldNotSatisfy [ToString α] (x : α) (pred : α → Bool) : IO Unit := do if (pred x) then fail s!"{x} satisfies predicate, but should not"

-- | Assert that an exception is thrown
def expectError (act : IO α) : IO Unit := do
  match (← act.toBaseIO) with
  | Except.ok _ => fail "Expected error"
  | Except.error _ => pure ()

-- | Assert that an IO action returns the expected value
def shouldReturn [BEq α] [ToString α] (act : IO α) (expected : α) : IO Unit := do shouldEqual (← act) expected

-- | Assert that an IO action does not return the given value
def shouldNotReturn [BEq α] [ToString α] (act : IO α) (unexpected : α) : IO Unit := do shouldNotEqual (← act) unexpected
