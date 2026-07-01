module
public import Spec.Assert.Base
import Init.Data.String.Search

@[expose] public section

namespace Spec.Assert.String

def containsSubstr (s substr : _root_.String) : Bool :=
  (s.find? substr).isSome

def shouldStartWith (s : _root_.String) (pfx : _root_.String) : IO Unit :=
  unless (s.startsWith pfx) do
    Spec.Assert.fail s!"❌ {s} does not start with {pfx}"

def shouldEndWith (s : _root_.String) (sfx : _root_.String) : IO Unit :=
  unless (s.endsWith sfx) do
    Spec.Assert.fail s!"❌ {s} does not end with {sfx}"

def shouldContain (s : _root_.String) (substr : _root_.String) : IO Unit :=
  unless containsSubstr s substr do
    Spec.Assert.fail s!"❌ {substr} ∉ {s}"

def shouldNotContain (s : _root_.String) (substr : _root_.String) : IO Unit :=
  if containsSubstr s substr then
    Spec.Assert.fail s!"❌ {substr} ∈ {s}"
  else
    pure ()

def isEmptyString (s : _root_.String) : Bool :=
  s.isEmpty

def shouldBeEmpty (s : _root_.String) : IO Unit :=
  unless isEmptyString s do
    Spec.Assert.fail s!"❌ expected empty string, got {s}"

def shouldNotBeEmpty (s : _root_.String) : IO Unit :=
  if isEmptyString s then
    Spec.Assert.fail "❌ expected non-empty string"
  else
    pure ()

end Spec.Assert.String
end
