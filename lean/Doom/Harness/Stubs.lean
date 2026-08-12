import Doom.Harness.TraceFormat
import Doom.Harness.TraceReader

namespace Doom.Harness.Stubs

open Doom.Harness.TraceFormat
open Doom.Harness.TraceReader

/--
Emit `n` well-formed zero tic records to `pathBase.trc` / `pathBase.dig`.
Every field is zero (including `sectors_digest`); digests are true FNV of those bytes.
-/
def runStubZero (n : Nat) (pathBase : System.FilePath) : IO Unit := do
  let records := Array.replicate n zeroTicRecord
  writeTracePair pathBase records

/-- Perturb first `THF_MOBJ` thinker at tic index 500: `momx += 1` (wrapping). -/
def perturbTic500 (records : Array TicRecord) : Except String (Array TicRecord × UInt32) :=
  if records.size < 501 then
    Except.error s!"ref trace has {records.size} tics; need at least 501 for tic-500 drift"
  else
    match records[500]? with
    | none => Except.error "internal: missing tic 500"
    | some tic =>
      let (thinkers', traceId?, _) :=
        tic.thinkers.foldl
          (init := ((#[] : Array ThinkerRec), (none : Option UInt32), false))
          fun (acc, foundId, done) t =>
            if done then
              (acc.push t, foundId, true)
            else if t.func == 1 then
              match t.mobj with
              | none =>
                (acc.push t, foundId, false)
              | some m =>
                let m' := { m with momx := m.momx + (1 : UInt32) }
                let t' := { t with mobj := some m' }
                (acc.push t', some t.traceId, true)
            else
              (acc.push t, foundId, false)
      match traceId? with
      | none =>
        Except.error "tic 500 has no mobj thinker (func == 1) to perturb"
      | some tid =>
        let tic' := { tic with thinkers := thinkers' }
        -- Rebuild records with tic 500 replaced, without bang accessors.
        let records' :=
          records.mapIdx fun i r =>
            if i == 500 then tic' else r
        Except.ok (records', tid)

/--
Parse `refTrace`, re-emit byte-identically except momx+1 on first mobj at tic 500.
Recomputes digests from the re-encoded records.
-/
def runStubDrift (refTrace : System.FilePath) (pathBase : System.FilePath) :
    IO (Except String UInt32) := do
  let parsed ← readTraceFile refTrace
  match parsed with
  | Except.error e => pure (Except.error e)
  | Except.ok records =>
    match perturbTic500 records with
    | Except.error e => pure (Except.error e)
    | Except.ok (records', tid) => do
      writeTracePair pathBase records'
      pure (Except.ok tid)

/-- Synthetic tic with a few players/thinkers/sectors (for perf + tests). -/
def syntheticTic (gametic : UInt32) : TicRecord where
  gametic := gametic
  inLevel := 1
  leveltime := gametic
  rndindex := gametic &&& 0xff
  prndindex := (gametic + 1) &&& 0xff
  players := #[{
    playerIndex := 0
    moTraceId := 1
    x := 0x01000000
    y := 0x02000000
    z := 0
    momx := gametic
    momy := 0
    momz := 0
    angle := 0x40000000
    viewz := 0x00290000
    health := 100
    armorpoints := 0
    readyweapon := 1
    pendingweapon := 9
    ammo0 := 50
    ammo1 := 0
    ammo2 := 0
    ammo3 := 0
    weaponowned0 := 1
    weaponowned1 := 1
    weaponowned2 := 0
    weaponowned3 := 0
    weaponowned4 := 0
    weaponowned5 := 0
    weaponowned6 := 0
    weaponowned7 := 0
    weaponowned8 := 0
    cmdForwardmove := 1
    cmdSidemove := 0
    cmdAngleturn := 0
    cmdButtons := 0
  }]
  thinkers := #[
    {
      traceId := 1
      func := 1
      mobj := some {
        x := 0x01000000
        y := 0x02000000
        z := 0
        momx := gametic
        momy := 0
        momz := 0
        angle := 0x40000000
        state := 0
        tics := 1
        health := 100
        flags := 0
        type_ := 0
      }
    },
    { traceId := 2, func := 2, mobj := none }
  ]
  sectors := #[{
    sectorIndex := 0
    floorheight := 0
    ceilingheight := 0x01000000
  }]
  sectorsDigest := 0xcbf29ce484222325

def syntheticTrace (n : Nat) : Array TicRecord :=
  Array.ofFn (n := n) fun i => syntheticTic i.toNat.toUInt32

end Doom.Harness.Stubs
