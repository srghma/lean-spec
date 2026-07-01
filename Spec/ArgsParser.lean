module
public import Spec.Config

@[expose] public section

namespace Spec

def parseArgs (args : List String) : Config := Id.run do
  let mut cfg : Config := {}
  let mut rest := args
  while !rest.isEmpty do
    match rest with
    | "--example" :: v :: tl | " / -e" :: v :: tl =>
      cfg := { cfg with example? := some v }; rest := tl
    | "--example-matches" :: v :: tl | " / -E" :: v :: tl =>
      cfg := { cfg with exampleMatches? := some v }; rest := tl
    | "--fail-fast" :: tl =>
      cfg := { cfg with failFast := true }; rest := tl
    | "--only-failures" :: tl =>
      cfg := { cfg with onlyFailures := true }; rest := tl
    | "--next-failure" :: tl | " / -n" :: tl =>
      cfg := { cfg with failFast := true, onlyFailures := true }; rest := tl
    | "--timeout" :: v :: tl =>
      cfg := { cfg with timeoutMs := (v.toNat?.map (· * 1000)) }; rest := tl
    | "--no-timeout" :: tl =>
      cfg := { cfg with timeoutMs := none }; rest := tl
    | "--color" :: tl =>
      cfg := { cfg with color := .Always }; rest := tl
    | "--no-color" :: tl =>
      cfg := { cfg with color := .Never }; rest := tl
    | "--reporter" :: name :: tl =>
      cfg := { cfg with reporterNames := cfg.reporterNames ++ [name] }; rest := tl
    | "--sequential" :: tl =>
      cfg := { cfg with parallel := false }; rest := tl
    | _ :: tl => rest := tl
    | [] => rest := []
  return cfg

end Spec
end
