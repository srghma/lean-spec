import Spec.Assertions
namespace Spec

namespace List
def shouldContain [BEq α] [ToString α] (xs : List α) (e : α) : IO Unit := unless (xs.contains e) do fail s!"{e} ∉ {xs}"
def shouldNotContain [BEq α] [ToString α] (xs : List α) (e : α) : IO Unit := do if (xs.contains e) then fail s!"{e} ∉ {xs}"
end List
