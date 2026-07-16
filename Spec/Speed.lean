module
public import Std.Time.Time.Unit.Millisecond

@[expose] public section

namespace Spec

inductive Speed where
  | fast
  | medium
  | slow
  deriving Repr, DecidableEq, Ord, Hashable

namespace Speed

/-- The slow cutoff used before the state file contains timing history. -/
def defaultSlowThresholdMs : Nat := 75

protected def speedOf_Int (threshold ms : Int) : Speed :=
  if ms > threshold then
    .slow
  else if threshold / 2 < ms then
    .medium
  else
    .fast

protected def speedOf_Millisecond (threshold ms : Std.Time.Millisecond.Offset) : Speed :=
  Speed.speedOf_Int threshold.toInt ms.toInt

protected def speedOf_Nat (threshold ms : Nat) : Speed :=
  Speed.speedOf_Int threshold ms

/-- Summary of the per-test running averages stored in the last-run state. -/
structure HistoricalTimingStats where
  meanMs : Nat
  standardDeviationMs : Nat
  fastThresholdMs : Nat
  slowThresholdMs : Nat

/-- Calculate a compact bell-curve summary from the historical per-test averages.
The state file keeps an average per test, not every individual run, so this is
the most detailed distribution it can represent. -/
def historicalTimingStats? (timings : Array Nat) : Option HistoricalTimingStats :=
  if timings.isEmpty then
    none
  else
    let meanMs := timings.foldl (init := 0) (· + ·) / timings.size
    let variance := timings.foldl (init := 0) fun total timing =>
      let difference := if timing > meanMs then timing - meanMs else meanMs - timing
      total + difference * difference
    let sorted := timings.mergeSort (· ≤ ·)
    some {
      meanMs
      standardDeviationMs := Nat.sqrt (variance / timings.size)
      fastThresholdMs := sorted[timings.size / 3]!
      slowThresholdMs := sorted[(2 * timings.size) / 3]! }

/-- Classify a duration from historical timing distribution: yellow inside one
standard deviation of the mean, blue below it, and red above it. -/
def classify (stats? : Option HistoricalTimingStats) (_testAverageMs? : Option Nat)
    (durationMs : Nat) : Speed :=
  match stats? with
  | none => Speed.speedOf_Nat defaultSlowThresholdMs durationMs
  | some stats =>
    let lowerBound := stats.meanMs - stats.standardDeviationMs
    let upperBound := stats.meanMs + stats.standardDeviationMs
    if durationMs < lowerBound then .fast
    else if durationMs > upperBound then .slow
    else .medium

protected def toString : Speed → String
  | .fast => "Speed.fast"
  | .medium => "Speed.medium"
  | .slow => "Speed.slow"

end Speed

instance : ToString Speed := ⟨Speed.toString⟩

end Spec
end
