import Std.Internal.Async

namespace Spec

open Std.Internal.IO.Async -- (Async sleep BaseAsync)

inductive MaybeTimedOut (α : Type)
  | timedOut
  | notTimedOut (result : α)
  deriving Repr, BEq, DecidableEq, Hashable, Inhabited, Ord

namespace MaybeTimedOut

def isTimedOut : MaybeTimedOut α → Bool
  | .timedOut => true
  | .notTimedOut _ => false

end MaybeTimedOut

/--
Run an IO action with a timeout.

- `duration` is in milliseconds
- Returns:
  - `.timedOut` if the action didn't finish in time
  - `.notTimedOut a` if it finished normally
-/
def timeout
  -- [Monad m]
  -- [MonadLiftT IO m]
  -- [MonadLiftT BaseIO m]
  -- [MonadAwait Task m]
  -- [Monad i_m]
  -- [MonadLiftT BaseIO i_m]
  -- [MonadAsync i_task i_m]
  -- [Nonempty a]
  (maxDuration : Std.Time.Millisecond.Offset)
  -- (duration : UInt32)
  (action : BaseIO a) -- lowest is BaseIO, and it's good, use toBaseIO if You dont like
  (prio := Task.Priority.default)
  -- : m (MaybeTimedOut a) := do -- I dont want this variant, bc then I have to use `Async.toIO`, then it's same as `IO (Task (Except IO.Error (MaybeTimedOut a)))` which is not true, real is `IO (Task (MaybeTimedOut a))`
  -- : m (Task (MaybeTimedOut a)) := do
  : BaseIO (Task (MaybeTimedOut a)) := do
  let promise ← IO.Promise.new -- uses lift to BaseIO

  -- discard $ async (t := i_task) (prio := prio) (Bind.bind action (liftM ∘ promise.resolve))

  let action': BaseIO Unit := do
    promise.resolve (MaybeTimedOut.notTimedOut (<- action))
  let _task1 <- action'.asTask (prio := prio)

  let sleepTask: BaseIO Unit := do
    -- (← sleep duration).block
    IO.sleep maxDuration.toInt.toNat.toUInt32
    promise.resolve MaybeTimedOut.timedOut
  let _task2 <- sleepTask.asTask (prio := prio)

  -- await promise.result!
  return promise.result!

-- #eval ((do
--   -- let t <- timeout (1000 : Std.Time.Millisecond.Offset) (return "Hello")
--   let t <- timeout 1000 (return "Hello" : BaseIO String)
--   -- return t.get) : IO (MaybeTimedOut String))
--   return t.get) : IO (MaybeTimedOut (Except Empty String)))
--
-- #eval return (← timeout 500 do
--   IO.sleep 1000
--   return "This won't be seen").get

def measureAction (action : BaseIO α) : BaseIO (Nat × α) := do
  let start ← IO.monoMsNow
  let r ← action
  let end_ ← IO.monoMsNow
  return (end_ - start, r)

def executeWithTimeoutAndMeasureTime
  (maxDuration : Std.Time.Millisecond.Offset)
  (action : BaseIO a) :
  BaseIO (Task (MaybeTimedOut (Nat × a))) :=
  timeout maxDuration (measureAction action)

def executeWithMTimeoutAndMeasureTime
  (maxDuration? : Option Std.Time.Millisecond.Offset)
  (action : BaseIO a) :
  BaseIO (Task (MaybeTimedOut (Nat × a))) := do
  match maxDuration? with
  | .some maxDuration => executeWithTimeoutAndMeasureTime maxDuration action
  | .none => (return MaybeTimedOut.notTimedOut (← measureAction action)).asTask

  -- let action : BaseIO (MaybeTimedOut (Nat × Except ε a)) := do
  --   let r := t.get
  --   match r with
  --   | MaybeTimedOut.timedOut => return MaybeTimedOut.timedOut
  --   | MaybeTimedOut.notTimedOut result => do
  --     let end_ ← IO.monoMsNow
  --     let duration := end_ - start
  --     return (MaybeTimedOut.notTimedOut (duration, result))
  -- let t' ← action.asTask
  -- return t'

-- def logResult {ε α}
--   [ToString ε] [ToString α]
--   (label : String)
--   (res : MaybeTimedOut (Nat × Except ε α)) : IO Unit := do
--   match res with
--   | MaybeTimedOut.timedOut =>
--     IO.println s!"[{label}] -> TIMED OUT"
--   | MaybeTimedOut.notTimedOut (elapsed, Except.ok v) =>
--     IO.println s!"[{label}] -> finished in {elapsed}ms with result: {v}"
--   | MaybeTimedOut.notTimedOut (elapsed, Except.error e) =>
--     IO.println s!"[{label}] -> failed in {elapsed}ms with error: {e}"
--
-- def tests : IO Unit := do
--   -- Test 1: finishes before timeout
--   let t1 ← executeWithTimeoutAndMeasureTime 1000 (do
--     IO.sleep 200
--     return "Hello" : IO String)
--   logResult "Test 1" t1.get
--
--   -- Test 2: times out before finishing
--   let t2 ← executeWithTimeoutAndMeasureTime 500 (do
--     IO.sleep 1000
--     return "World" : IO String)
--   logResult "Test 2" t2.get
--
--   -- Test 3: longer timeout, finishes successfully
--   let t3 ← executeWithTimeoutAndMeasureTime 2000 (do
--     IO.sleep 1000
--     return "Done in 1s" : IO String)
--   logResult "Test 3" t3.get
--
-- #eval tests
