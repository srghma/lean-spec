/-
Copyright (c) 2025 Your Name. All rights reserved.
Released under Apache 2.0 license.
-/
import Lake
-- import System

namespace Spec

open System Lake

/-- Get all Lean file paths in a directory recursively -/
partial def getLeanFilePaths (fp : FilePath) (acc : Array FilePath := #[]) :
    IO $ Array FilePath := do
  if ← fp.isDir then
    (← fp.readDir).foldlM (fun acc dir => getLeanFilePaths dir.path acc) acc
  else return if fp.extension == some "lean" then acc.push fp else acc

/-- Get all Lean files in a directory as a sorted list of strings -/
def getAllFiles (rootDir : String) : ScriptM $ List String := do
  let paths := (← getLeanFilePaths ⟨rootDir⟩).map toString
  let paths : Lean.RBTree String compare := Lean.RBTree.ofList paths.toList -- ordering
  return paths.toList

/-- Generate import statements for all Lean files in a directory -/
def getImportsString (rootDir : String) : ScriptM String := do
  let paths ← getAllFiles rootDir
  let imports := paths.map fun p =>
    "import " ++ (p.splitOn ".").head!.replace "/" "."
  return s!"{"\n".intercalate imports}\n"

/-- Generate import file for a given root directory -/
def generateImportFile (rootDir : String) (outputFile : String) : ScriptM Unit := do
  let importsString ← getImportsString rootDir
  IO.FS.writeFile ⟨outputFile⟩ importsString

/-- Check if import file is up to date -/
def checkImportFile (rootDir : String) (importFile : String) : ScriptM Bool := do
  if ← System.FilePath.pathExists ⟨importFile⟩ then
    let importsFromUser ← IO.FS.readFile ⟨importFile⟩
    let expectedImports ← getImportsString rootDir
    return importsFromUser == expectedImports
  else
    return false

/-- Generate a test main file with given content -/
def generateTestMain (testDir : String) (content : String) : ScriptM Unit := do
  let testDirPath : FilePath := testDir
  if !(← testDirPath.pathExists) then
    IO.FS.createDirAll testDirPath
  IO.FS.writeFile (testDirPath / "Main.lean") content

/-- Run a shell command and return the result -/
def runCmd (cmd : String) : ScriptM $ Except String String := do
  let cmd := cmd.splitOn " "
  if h : cmd ≠ [] then
    let (cmd, args) := match h' : cmd with
      | cmd :: args => (cmd, ⟨args⟩)
      | []          => absurd h' (h' ▸ h)
    let out ← IO.Process.output {
      cmd  := cmd
      args := args
    }
    return if out.exitCode != 0
      then .error out.stderr
      else .ok out.stdout
  else return .ok ""
