module
public import Spec.Assert.Base

@[expose] public section

namespace Spec.Assert

def shouldBeGreaterThan [LT α] [DecidableRel (fun a b : α => a < b)] [ToString α]
    (actual expected : α) : IO Unit := do
  unless expected < actual do
    fail s!"❌ expected {toString actual} to be greater than {toString expected}"

def shouldBeGreaterOrEqual [LE α] [DecidableRel (fun a b : α => a ≤ b)] [ToString α]
    (actual expected : α) : IO Unit := do
  unless expected ≤ actual do
    fail s!"❌ expected {toString actual} to be greater than or equal to {toString expected}"

def shouldBeLessThan [LT α] [DecidableRel (fun a b : α => a < b)] [ToString α]
    (actual expected : α) : IO Unit := do
  unless actual < expected do
    fail s!"❌ expected {toString actual} to be less than {toString expected}"

def shouldBeLessOrEqual [LE α] [DecidableRel (fun a b : α => a ≤ b)] [ToString α]
    (actual expected : α) : IO Unit := do
  unless actual ≤ expected do
    fail s!"❌ expected {toString actual} to be less than or equal to {toString expected}"

end Spec.Assert
end
