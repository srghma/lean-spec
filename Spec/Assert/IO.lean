module
public import Spec.Assert.Base

@[expose] public section

namespace Spec.Assert

def shouldEqual [BEq α] [ToString α] (actual expected : α) : IO Unit := do
  if !(actual == expected) then
    fail s!"❌ expected {toString expected}, got {toString actual}"

def shouldNotEqual [BEq α] [ToString α] (actual expected : α) : IO Unit := do
  if actual == expected then
    fail s!"❌ did not expect {toString expected}"

def shouldReturn [BEq α] [ToString α] (actual : IO α) (expected : α) : IO Unit := do
  shouldEqual (← actual) expected

def shouldNotReturn [BEq α] [ToString α] (actual : IO α) (unexpected : α) : IO Unit := do
  shouldNotEqual (← actual) unexpected

def expectError (act : IO α) : IO Unit := do
  let gotError ← try
    discard act
    pure false
  catch _ =>
    pure true
  unless gotError do
    fail "❌ expected error"

end Spec.Assert
end
