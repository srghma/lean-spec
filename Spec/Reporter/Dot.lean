import Spec.Reporter.Base
import Spec.Result
import Spec.Event
import Spec.Style
import Spec.Speed

namespace Spec.Reporter.Dot

open Spec.Reporter.Base

-- Configuration for dot reporter
structure DotReporterConfig where
  width : Nat
deriving Repr

def wrapOutput (config : DotReporterConfig) (output : String) : StateT Int (WriterT String IO) Unit := do
    let n ← get
    let newN := n + 1
    set newN
    if (newN % config.width == 0) then
      liftM $ tell "\n"
    liftM $ tell output

-- Dot reporter implementation
def dotReporter (config : DotReporterConfig) : Reporter :=
  Base.defaultReporter (-1 : Int) fun event => do
    match event with
    | Event.testEnd _ (Result.success speed _) => do
      wrapOutput config $ Style.styled (Speed.toStyle speed) "."
    | Event.testEnd _ (Result.failure _) => do
      wrapOutput config $ Style.styled Style.red "!"
    | Event.pending _ => do
      wrapOutput config $ Style.styled Style.dim ","
    | Event.end _ => do
      liftM $ tell "\n"
    | _ =>
      pure ()

end Spec.Reporter.Dot
