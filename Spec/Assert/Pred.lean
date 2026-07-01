module
public import Spec.Assert.Base

@[expose] public section

namespace Spec.Assert

def shouldSatisfy [ToString α] (actual : α) (pred : α → Bool) : IO Unit := do
  unless pred actual do
    fail s!"❌ {toString actual} doesn't satisfy predicate"

def shouldNotSatisfy [ToString α] (actual : α) (pred : α → Bool) : IO Unit := do
  if pred actual then
    fail s!"❌ {toString actual} satisfies predicate, but should not"

end Spec.Assert
end
