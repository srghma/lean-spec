import Spec
import Std.Time.Time.Unit.Basic
import Std.Time.Time.Unit.Second

open Spec

def oneSecondMs : Std.Time.Millisecond.Offset :=
  Std.Time.Millisecond.Offset.ofSeconds (1 : Std.Time.Second.Offset)

example : Spec := it "sometest" (do IO.println "ok")
example : Spec := it (focus := true) "sometest" (do IO.println "ok")
example : Spec := it (timeoutMs? := .some oneSecondMs) "sometest" (do IO.println "ok")
example : Spec := it (focus := true) (timeoutMs? := .some oneSecondMs) "sometest" (do IO.println "ok")
example : Spec := it (focus := true) "sometest" (timeoutMs? := .some oneSecondMs) (do IO.println "ok")
example : Spec := it "sometest" (timeoutMs? := .some oneSecondMs) (focus := true) (do IO.println "ok")

example : Spec := Spec.it "sometest" (do IO.println "ok")
example : Spec := Spec.it (focus := true) "sometest" (do IO.println "ok")
example : Spec := Spec.it (timeoutMs? := .some oneSecondMs) "sometest" (do IO.println "ok")
example : Spec := Spec.it (focus := true) (timeoutMs? := .some oneSecondMs) "sometest" (do IO.println "ok")
example : Spec := Spec.it (timeoutMs? := .some oneSecondMs) (focus := true) "sometest" (do IO.println "ok")
example : Spec := Spec.it "sometest" (timeoutMs? := .some oneSecondMs) (focus := true) (do IO.println "ok")

/-! Duplicate `it` and `describe` names within one group are reported before the
spec runs. `pending` items do not participate in this check. The first declared
non-pending item is the one written to `.lean-spec-timings-and-last-failures`. -/

example : Spec := do
  it "foo" (pure () : IO Unit)
  it "foo" (pure () : IO Unit)
  pending "foo"
  describe "foo" do it "lala" (pure () : IO Unit)
  describe "xxx" do
    describe "yyy" do
      it "bar" (pure () : IO Unit)
      it "bar" (pure () : IO Unit)
      describe "baz" do pure ()
      describe "baz" do pure ()

/-! This reports:
```text
⚠️ Found duplicate names of 🧪 it/📂 describe items.
This makes timings inside .lean-spec-timings-and-last-failures inconsistent, so only the first declared item will be written there.
Please make names unique per group:
  root: foo (duplicated 3 times. 2 🧪 it + 1 📂 describe)
  xxx >> yyy: bar (duplicated 2 times. 2 🧪 it), baz (duplicated 2 times. 2 📂 describe)
``` -/
