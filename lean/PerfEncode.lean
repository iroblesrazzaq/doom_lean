import Doom.Harness.Stubs
import Doom.Harness.TraceFormat

open Doom.Harness.Stubs
open Doom.Harness.TraceFormat

/-- Encode a synthetic 1000-tic trace in a loop; print measured tics/second. -/
def main (_args : List String) : IO UInt32 := do
  let nTics : Nat := 1000
  let records := syntheticTrace nTics
  -- Warm up once so Lake/runtime noise is less dominant.
  let _ := encodeTraceFile records
  let iters : Nat := 50
  let t0 ← IO.monoNanosNow
  let mut _sink : Nat := 0
  for _ in [0:iters] do
    let bytes := encodeTraceFile records
    _sink := _sink + bytes.size
  let t1 ← IO.monoNanosNow
  let elapsedNs := t1 - t0
  let elapsedSec := elapsedNs.toFloat / 1e9
  let totalTics := (nTics * iters).toFloat
  let tps := if elapsedSec == 0.0 then 0.0 else totalTics / elapsedSec
  IO.println s!"tics_per_second: {tps}"
  IO.println s!"encoded_bytes_last_sink: {_sink}"
  pure 0
