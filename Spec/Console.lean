import Std
open Std

namespace Spec

-- Internal Monoid class
class Monoid (α : Type) where
  empty : α
  append : α → α → α

-- String Monoid instance
instance : Monoid String where
  empty := ""
  append := (· ++ ·)

-- Minimal WriterT using StateT with Monoid
abbrev WriterT (w : Type) (m : Type -> Type) (α : Type) := StateT w m α

namespace Writer

variable {w : Type} [Monoid w] {m : Type → Type} [Monad m]

def tell (x : w) : WriterT w m Unit :=
  modify (Monoid.append · x)

def exec {α} [monoid : Monoid w] (ma : WriterT w m α) (init := monoid.empty) : m w :=
  Prod.snd <$> ma init

end Writer

-- Write to stdout (effect)
def write (s : String) : IO Unit := IO.println s

-- Run a writer and print the result
def logWriter (w : WriterT String IO Unit) : IO Unit := do
  let out ← Writer.exec w
  write out

-- Add a single line with newline
def tellLn [Monad m] (line : String) : WriterT String m Unit :=
  Writer.tell (line ++ "\n")

-- Add multiple lines with newlines
def tellLns [Monad m] (lines : Array String) : WriterT String m Unit :=
  lines.forM (fun l => Writer.tell (l ++ "\n"))
