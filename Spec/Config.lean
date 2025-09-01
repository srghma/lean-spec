import Spec.Speed
import Spec.Basic
import Std.Time

namespace Spec

-- assuming these are already defined
-- `SpecTree` is the test tree, analogous to `Tree` in your other modules

def TreeFilter := {g : Type -> Type} -> {i : Type} -> Array (SpecTree g i) -> Array (SpecTree g i)

structure Config where
  slow       : Std.Time.Millisecond.Offset -- ?
  timeout    : Option Std.Time.Millisecond.Offset
  exit       : Bool
  failFast   : Bool
  filterTree : TreeFilter

namespace Config

-- `identity` for filterTree is default
def default : Config :=
  { slow       := ⟨75⟩
  , timeout    := some ⟨2000⟩
  , exit       := true
  , failFast   := false
  , filterTree := id
  }
