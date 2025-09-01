import Spec.Style
import Std.Time

namespace Spec


inductive Speed where
  | fast -- fast__below_or_eq_half_threshold
  | medium -- medium__more_half__and__less_or_eq_threshold
  | slow -- slow_above_threshold
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

protected def toStyle : Speed → List StyleModifier
  | .fast   => Style.dim
  | .medium => Style.yellow
  | .slow   => Style.red

protected def toString : Speed -> String
  | .fast => "Speed.fast"
  | .medium => "Speed.medium"
  | .slow => "Speed.slow"

end Speed

instance : ToString Speed := ⟨Speed.toString⟩

-- #guard Speed.speedOf_Nat 1000 1001 = Speed.slow
-- #guard Speed.speedOf_Nat 1000 1000 = Speed.medium
-- #guard Speed.speedOf_Nat 1000 600 = Speed.medium
-- #guard Speed.speedOf_Nat 1000 501 = Speed.medium
-- #guard Speed.speedOf_Nat 1000 500 = Speed.fast
