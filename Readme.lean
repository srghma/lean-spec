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
