import Doom.Harness.DisplaySim
import Doom.Harness.Fnv
import Doom.Harness.TraceFormat
import Doom.Harness.TraceReader
import Doom.Harness.Stubs
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Random
import Doom.Playsim.Tick

open Doom.Harness.DisplaySim
open Doom.Harness.Fnv
open Doom.Harness.TraceFormat
open Doom.Harness.TraceReader
open Doom.Harness.Stubs
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Random
open Doom.Playsim.Tick

private def fail (msg : String) : IO UInt32 := do
  IO.eprintln s!"FAIL: {msg}"
  pure 1

private def bytesOfAscii (s : String) : ByteArray :=
  s.toUTF8

def main (_args : List String) : IO UInt32 := do
  -- (a) encode → decode round-trip
  let syn := syntheticTic 42
  let encoded := encodeTicRecord syn
  match (do
      let s : ParseState := { data := encoded, pos := 0 }
      let (decoded, s) ← readTicRecord s
      if s.pos != encoded.size then
        Except.error s!"trailing bytes after record: pos={s.pos} size={encoded.size}"
      else if decoded != syn then
        Except.error "decoded record ≠ original"
      else
        Except.ok ()) with
  | Except.error e => return (← fail s!"round-trip: {e}")
  | Except.ok () => IO.println "ok: encode/decode round-trip"

  -- (b) FNV-1a64 test vectors
  let hEmpty := fnv1a64 ByteArray.empty
  if hEmpty != 0xcbf29ce484222325 then
    return (← fail s!"fnv empty: got {hEmpty}")
  let hA := fnv1a64 (bytesOfAscii "a")
  if hA != 0xaf63dc4c8601ec8c then
    return (← fail s!"fnv 'a': got {hA}")
  let hFoobar := fnv1a64 (bytesOfAscii "foobar")
  if hFoobar != 0x85944171f73967e8 then
    return (← fail s!"fnv 'foobar': got {hFoobar}")
  IO.println "ok: fnv1a64 vectors"

  -- (c) write small trace+digest pair, re-read, verify digests match records
  let dir ← IO.currentDir
  let base := dir / "_harness_test_tmp"
  let records := #[syntheticTic 0, syntheticTic 1, zeroTicRecord]
  writeTracePair base records
  let trc ← IO.FS.readBinFile (base.toString ++ ".trc")
  let dig ← IO.FS.readBinFile (base.toString ++ ".dig")
  match parseTraceBytes trc, parseDigestBytes dig with
  | Except.error e, _ => return (← fail s!"re-read trc: {e}")
  | _, Except.error e => return (← fail s!"re-read dig: {e}")
  | Except.ok gotRecords, Except.ok gotDigests => do
    if gotRecords != records then
      return (← fail "re-read records ≠ written records")
    if gotDigests.size != records.size then
      return (← fail s!"digest count {gotDigests.size} ≠ record count {records.size}")
    let mut i : Nat := 0
    for r in records do
      let expect := fnv1a64 (encodeTicRecord r)
      match gotDigests[i]? with
      | none => return (← fail s!"missing digest at {i}")
      | some d =>
        if d != expect then
          return (← fail s!"digest mismatch at {i}: got {d} expect {expect}")
      i := i + 1
    IO.println "ok: trace/digest pair self-consistent"
  -- cleanup best-effort
  try IO.FS.removeFile (base.toString ++ ".trc") catch _ => pure ()
  try IO.FS.removeFile (base.toString ++ ".dig") catch _ => pure ()

  -- (d) wipeInitMelt: rndindex 2 → 66, then one M_Random (ST_Ticker) → 67
  let rng2 : RandomState := { prndindex := 0, rndindex := 2 }
  let rng66 := wipeInitMelt rng2
  if rng66.rndindex != 66 then
    return (← fail s!"wipeInitMelt 2→66: got {rng66.rndindex}")
  let emptyLevel : LevelData := {
    vertexes := #[]
    sectors := #[]
    sides := #[]
    lines := #[]
    segs := #[]
    subsectors := #[]
    nodes := #[]
    things := #[]
    blockmap := {
      originX := 0
      originY := 0
      width := 0
      height := 0
      lump := #[]
    }
    reject := ByteArray.empty
  }
  let gsBase := initFromLevel emptyLevel 2 (Array.replicate 4 false) 0
  let gsAfter := stTicker { gsBase with rng := rng66 }
  if gsAfter.rng.rndindex != 67 then
    return (← fail s!"stTicker after wipe →67: got {gsAfter.rng.rndindex}")
  -- onFrame: demoscreen → level triggers wipe once; second call is no-op.
  let (d1, rngW) := onFrame initDisplay rng2 gsLevel
  if d1.wipegamestate != gsLevel then
    return (← fail "onFrame did not sync wipegamestate to GS_LEVEL")
  if rngW.rndindex != 66 then
    return (← fail s!"onFrame wipe rndindex: got {rngW.rndindex}")
  let (d2, rngW2) := onFrame d1 rngW gsLevel
  if d2.wipegamestate != gsLevel || rngW2.rndindex != 66 then
    return (← fail "onFrame second call should be RNG no-op")
  IO.println "ok: wipeInitMelt / onFrame DEMO1 path"

  IO.println "harness-test: all passed"
  pure 0
