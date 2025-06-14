import Spec.Speed
import Std.Time

namespace Spec

-- assuming these are already defined
-- `SpecTree` is the test tree, analogous to `Tree` in your other modules

structure Config (SpecTree : Type → Type → Type) where
  slow       : Std.Time.Millisecond.Offset
  timeout    : Option Std.Time.Millisecond.Offset
  exit       : Bool
  failFast   : Bool
  filterTree : {g i : Type} → Array (SpecTree g i) → Array (SpecTree g i)

namespace Config

-- `identity` for filterTree is default
def default (SpecTree : Type → Type → Type) : Config SpecTree :=
  { slow       := ⟨75⟩
  , timeout    := some ⟨2000⟩
  , exit       := true
  , failFast   := false
  , filterTree := fun {g i} (ts : Array (SpecTree g i)) => ts
  }
