module
public import Std.Time.Time.Unit.Millisecond

@[expose] public section

namespace Spec

inductive Speed where
  | fast
  | medium
  | slow
  deriving Repr, DecidableEq, Ord, Hashable

namespace Speed

protected def speedOf_Int (threshold ms : Int) : Speed :=
  if ms > threshold then
    .slow
  else if threshold / 2 < ms then
    .medium
  else
    .fast

protected def speedOf_Millisecond (threshold ms : Std.Time.Millisecond.Offset) : Speed :=
  Speed.speedOf_Int threshold.toInt ms.toInt

protected def speedOf_Nat (threshold ms : Nat) : Speed :=
  Speed.speedOf_Int threshold ms

protected def toString : Speed → String
  | .fast => "Speed.fast"
  | .medium => "Speed.medium"
  | .slow => "Speed.slow"

end Speed

instance : ToString Speed := ⟨Speed.toString⟩

end Spec
end
