module
public import Init.System.IO
public import Std.Data.TreeSet
public import Lean
public import Std.Data.TreeMap
public import Std.Data.HashSet
public import Lean.Elab.Term
public import Lean.Parser.Term
public import Init.Data.Repr

@[expose] public section

namespace Spec.Assert

def fail (msg : String) : IO Unit := throw <| IO.Error.userError msg

end Spec.Assert
end
