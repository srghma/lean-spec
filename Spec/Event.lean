import Spec.Tree
import Spec.Result

namespace Spec

open Std

inductive Execution where
  | parallel
  | sequential
deriving Repr, BEq, DecidableEq, Ord, Inhabited

instance : ToString Execution where
  toString x := repr x |>.pretty

inductive Event where
  | start    : Nat → Event
  | suite    : Execution → TestLocator → Event
  | suiteEnd : TestLocator → Event
  | test     : Execution → TestLocator → Event
  | testEnd  : TestLocator → Result → Event
  | pending  : TestLocator → Event
  -- | end_      : Array (Tree Name Empty Result) → Event
  | end_      : Array (Tree TestLocator Empty Result) → Event

deriving Repr

instance : ToString Event where
  toString x := repr x |>.pretty

#guard toString (Event.start 1) = r#"Spec.Event.start 1"#
