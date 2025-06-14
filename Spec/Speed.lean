import Spec.Style
namespace Spec

inductive Speed where
  | fast
  | medium
  | slow
  deriving Repr, DecidableEq, Ord, Hashable

namespace Speed

def speedOf (threshold : Float) (ms : Float) : Speed :=
  if ms > threshold then
    .slow
  else if ms > threshold / 2.0 then
    .medium
  else
    .fast

def toStyle : Speed → List StyleModifier
  | .fast   => Style.dim
  | .medium => Style.yellow
  | .slow   => Style.red

def toString : Speed -> String
  | .fast => "Speed.fast"
  | .medium => "Speed.medium"
  | .slow => "Speed.slow"

end Speed

instance : ToString Speed := ⟨Speed.toString⟩
