module

public import Std.Sync.Mutex
public import Spec.CTestLikeState

@[expose] public section

namespace Spec

/-- Work queue for a parallel spec run. `parallelLeaves` are claimed dynamically
by one of `queueCount` workers; `sequentialLeaves` run afterward on queue 1. -/
structure ParallelQueue where
  haveSequentialTests : Bool
  haveParallelTests : Bool
  queueCount : Nat
  parallelLeaves : Array ScheduledLeaf
  sequentialLeaves : Array ScheduledLeaf
  next : IO.Ref Nat
  lock : Std.BaseMutex

def ParallelQueue.create (leaves : Array ScheduledLeaf) : IO ParallelQueue := do
  let parallelLeaves := leaves.filter (·.leaf.parallel)
  let sequentialLeaves := leaves.filter (!·.leaf.parallel)
  let next ← IO.mkRef 0
  let lock ← Std.BaseMutex.new
  return {
    haveSequentialTests := !sequentialLeaves.isEmpty
    haveParallelTests := !parallelLeaves.isEmpty
    queueCount := max 1 <| min leaves.size (System.Platform.Internal.getHardwareConcurrency ()).toNat
    parallelLeaves
    sequentialLeaves
    next
    lock }

/-- Atomically claim the next parallel item, preserving the slow-first order. -/
def ParallelQueue.takeParallel? (queue : ParallelQueue) : IO (Option ScheduledLeaf) := do
  queue.lock.lock
  try
    let index ← queue.next.get
    if h : index < queue.parallelLeaves.size then
      queue.next.set (index + 1)
      return some queue.parallelLeaves[index]
    else
      return none
  finally
    queue.lock.unlock

end Spec
end
