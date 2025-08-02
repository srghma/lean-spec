import Spec.Speed
import Std.Time

namespace Spec
open Spec

local instance : Hashable IO.Error where
  hash e := e.toString.hash
local instance : Hashable Std.Time.Millisecond.Offset where
  hash e := e.toInt.toNat.toUInt64

local instance : Repr Std.Time.Millisecond.Offset where
  reprPrec e _ := toString e
local instance : Repr IO.Error where
  reprPrec e _ := toString e

local instance : BEq IO.Error where
  beq e1 e2 := e1.toString == e2.toString

inductive Result where
  | success (speed : Speed) (duration : Std.Time.Millisecond.Offset)
  | failure (err : IO.Error)
  deriving Hashable, Repr, BEq, Inhabited

instance : ToString Result where
  toString x := repr x |>.pretty
