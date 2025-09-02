namespace Spec

def Array.popLast? (xs : Array α) : Option (Array α × α) :=
  if h : xs.size > 0 then
    let last := xs.back
    let init := xs.pop
    some (init, last)
  else
    none
