import Lean
open Lean Elab

def leanPaths (dir : System.FilePath) : IO LeanPaths := do
  let out ← IO.Process.output {
    cmd := "lake"
    args := #["print-paths"]
    cwd := some dir
  } 
  let .ok paths := Json.parse out.stdout | 
    throw <| .userError "Failed to parse paths as json."
  let .ok (paths : LeanPaths) := fromJson? paths |
    throw <| .userError "Failed to parse paths as Lean paths."
  let paths : LeanPaths := {
    srcPath := paths.srcPath.map dir.join
    oleanPath := paths.oleanPath.map dir.join
    loadDynlibPaths := paths.loadDynlibPaths.map dir.join
  }
  return paths

unsafe def compileEnv (dir : System.FilePath) (file : System.FilePath) : 
    IO Environment := do
  let contents ← IO.FS.readFile <| dir / file   
  let inputCtx := Parser.mkInputContext contents "<input>"
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  enableInitializersExecution
  initSearchPath (← findSysroot) (← leanPaths dir).oleanPath
  let (env, messages) ← processHeader header {} messages inputCtx
  let cmdState : Command.State := Command.mkState env messages {}
  let frontEndState ← IO.processCommands inputCtx parserState cmdState
  let env := frontEndState.commandState.env
  return env