import Spec.Style

namespace Spec

inductive Speed where
  | fast
  | medium
  | slow
  deriving Repr, DecidableEq, Ord

namespace Speed

def speedOf (threshold : Float) (ms : Float) : Speed :=
  if ms > threshold then
    .slow
  else if ms > threshold / 2.0 then
    .medium
  else
    .fast

def toStyle : Speed → List Modifier
  | .fast   => dim
  | .medium => yellow
  | .slow   => red

def toString : Speed -> String
  | .fast => "Fast"
  | .medium => "Medium"
  | .slow => "Slow"

end Speed

instance : ToString Speed where
  toString := Speed.toString
