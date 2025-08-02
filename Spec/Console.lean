import Std
/- import Spec.WriterT -/
import Mathlib.Control.Monad.Writer

namespace Spec
namespace Console

-- Run a writer and print the result
def logWriter (w : WriterT String IO Unit) : IO Unit := do
  let out ← WriterT.run w
  IO.println out

-- Add a single line with newline
def tellLn [Monad m] (line : String) : WriterT String m Unit :=
  tell (line ++ "\n")

-- Add multiple lines with newlines
def tellLns [Monad m] [Monad (WriterT String m)] (lines : Array String) : WriterT String m Unit :=
  lines.forM (fun l => tell (l ++ "\n"))
