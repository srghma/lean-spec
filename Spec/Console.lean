import Std
import Spec.WriterT

namespace Spec
namespace Console

-- Run a writer and print the result
def logWriter (w : WriterT String IO Unit) : IO Unit := do
  let out ← WriterT.exec w
  IO.println out

-- Add a single line with newline
def tellLn [Monad m] (line : String) : WriterT String m Unit :=
  WriterT.tell (line ++ "\n")

-- Add multiple lines with newlines
def tellLns [Monad m] (lines : Array String) : WriterT String m Unit :=
  lines.forM (fun l => WriterT.tell (l ++ "\n"))
