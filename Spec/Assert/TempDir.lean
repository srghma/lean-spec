module
public import Spec.Assert.Base

@[expose] public section

namespace Spec.Assert

open IO.FS
open System (FilePath)

-- So that if we cd in prev test - `withTempDir` will still be usable (still can find correct dir)
initialize initialCwd : IO.Ref FilePath ← do
  let cwd ← IO.currentDir
  IO.mkRef cwd

/-- Run `act` inside a fresh temporary directory, restoring the previous working
directory afterwards. Each parallel test gets its own isolated dir. -/
def withTempDir (act : FilePath → IO α) : IO α := do
  let prev ← initialCwd.get
  let stamp ← IO.monoNanosNow
  let dir : FilePath := prev / s!".spec-tmp-{stamp}"
  IO.FS.createDirAll dir
  try
    act dir
  finally
    try IO.FS.removeDirAll dir catch _ => pure ()

end Spec.Assert
end
