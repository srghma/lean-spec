import Spec.Assertions.String.Util
import Spec.Assertions
namespace Spec

namespace String

-- | Assert that a string starts with a given prefix
def shouldStartWith (s _prefix : String) : IO Unit :=
  unless (s.startsWith _prefix) do
    fail s!"{s} does not start with {_prefix}"

-- | Assert that a string ends with a given suffix
def shouldEndWith (s suffix : String) : IO Unit :=
  unless (s.endsWith suffix) do
    fail s!"{s} does not end with {suffix}"

-- | Assert that a string contains a given substring
def shouldContain (s substr : String) : IO Unit :=
  unless (s.containsSubstring substr) do
    fail s!"{substr} ∉ {s}"

-- | Assert that a string does *not* contain a given substring
def shouldNotContain (s substr : String) : IO Unit := do
  if s.containsSubstring substr then
    fail s!"{substr} ∈ {s}"

end String
