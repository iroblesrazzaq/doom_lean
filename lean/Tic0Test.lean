import Doom.Harness.TraceFormat
import Doom.Playsim.Demo
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Spawn
import Doom.Playsim.Tick
import Doom.Playsim.TraceEmit
import Doom.Wad

/-!
P2a-iii behavior lock: DEMO1 tic-0 digest parity after full BSP sight.

E2E contract (public surface `verify --impl real --tics 1`):
- candidate.dig tic 0 must match fixtures/demo1.dig tic 0 (no "TIC 0" field
  mismatch; pure length mismatch alone is OK).
- Fixture goldens: gametic=0 in_level=1 leveltime=1 rndindex=1 prndindex=121
  player_count=1 thinker_count=230 active_sector_count=0
  sectors_digest=0x6e561cdacf836d38 viewz=2686976 pendingweapon=10.
-/

open Doom.Wad
open Doom.Harness.TraceFormat
open Doom.Playsim.Demo
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Spawn
open Doom.Playsim.Tick
open Doom.Playsim.TraceEmit

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" => pure (cwd.parent.getD cwd)
  | _ => pure cwd

def loadIwad : IO WadDirectory := do
  let root ← defaultRoot
  loadFile (root / "fixtures" / "wads" / "doom1.wad")

def loadMap (wad : WadDirectory) (label : String) : Except String LevelData := do
  match checkNumForName wad label with
  | none => throw s!"missing {label}"
  | some idx =>
    let things ← mapLumpData wad idx ML_THINGS
    let linedefs ← mapLumpData wad idx ML_LINEDEFS
    let sidedefs ← mapLumpData wad idx ML_SIDEDEFS
    let vertexes ← mapLumpData wad idx ML_VERTEXES
    let segs ← mapLumpData wad idx ML_SEGS
    let ssectors ← mapLumpData wad idx ML_SSECTORS
    let nodes ← mapLumpData wad idx ML_NODES
    let sectors ← mapLumpData wad idx ML_SECTORS
    let reject ← mapLumpData wad idx ML_REJECT
    let blockmap ← mapLumpData wad idx ML_BLOCKMAP
    buildLevel things linedefs sidedefs vertexes segs ssectors nodes sectors reject blockmap

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let root ← defaultRoot
  let wad ← loadIwad
  let demoBytes ←
    match checkNumForName wad "DEMO1" with
    | none =>
      ok := (← assert "DEMO1 lump" false) && ok
      pure ByteArray.empty
    | some idx =>
      match lumpData wad idx with
      | Except.error e =>
        ok := (← assert s!"DEMO1 bytes ({e})" false) && ok
        pure ByteArray.empty
      | Except.ok b => pure b
  match parseHeader demoBytes with
  | Except.error e =>
    ok := (← assert s!"demo header ({e})" false) && ok
  | Except.ok hdr =>
    match loadMap wad s!"E{hdr.episode}M{hdr.map}" with
    | Except.error e =>
      ok := (← assert s!"load map ({e})" false) && ok
    | Except.ok level =>
      match setupSpawnedLevel level (hdr.skill.toUInt32.toInt32) hdr.playeringame
          hdr.consoleplayer.toNat with
      | Except.error e =>
        ok := (← assert s!"spawn ({e})" false) && ok
      | Except.ok gs0 =>
        let gs0 := {
          gs0 with
          demoBytes
          demoCursor := 13
          demoplayback := true
          gametic := 0
        }
        match gTicker gs0 with
        | Except.error e =>
          ok := (← assert s!"G_Ticker ({e})" false) && ok
        | Except.ok gs =>
          ok := (← assert "leveltime=1" (gs.leveltime == 1)) && ok
          ok := (← assert "rndindex=1" (gs.rng.rndindex == 1)) && ok
          ok := (← assert "prndindex=121" (gs.rng.prndindex == 121)) && ok
          ok := (← assert "validcount advanced (BSP sight ran)"
            (gs.validcount > 0)) && ok
          match gs.players[gs.consoleplayer]? with
          | none => ok := (← assert "console player" false) && ok
          | some pl =>
            ok := (← assert "viewz=2686976" (pl.viewz == 2686976)) && ok
            ok := (← assert "pendingweapon=10" (pl.pendingweapon == 10)) && ok
          let sd := sectorsDigest gs
          ok := (← assert "sectors_digest"
            (sd == (0x6e561cdacf836d38 : UInt64))) && ok
          match emitTicRecord gs with
          | Except.error e =>
            ok := (← assert s!"emit ({e})" false) && ok
          | Except.ok rec =>
            let outDir := root / ".agent_tmp" / "tic0_parity"
            IO.FS.createDirAll outDir
            let candBase := outDir / "candidate"
            writeTracePair candBase #[rec]
            let diff ← IO.Process.output {
              cmd := "python3"
              args := #[
                (root / "tools" / "tracediff").toString,
                (root / "fixtures" / "demo1.dig").toString,
                (outDir / "candidate.dig").toString,
                "--ref-trace",
                (root / "fixtures" / "demo1.trc").toString,
                "--cand-trace",
                (outDir / "candidate.trc").toString
              ]
            }
            IO.println diff.stdout
            if diff.stderr.isEmpty then pure () else IO.eprintln diff.stderr
            let out := diff.stdout ++ diff.stderr
            let tic0Mismatch := out.contains "TIC 0"
            let lengthOnly :=
              out.contains "trace length mismatch" && !tic0Mismatch
            ok := (← assert "tracediff: no TIC 0 mismatch" (!tic0Mismatch)) && ok
            ok := (← assert "tracediff: length-only or clean exit"
              ((diff.exitCode == 0 && !tic0Mismatch) || (diff.exitCode == 1 && lengthOnly))) && ok

  -- Also exercise public verify surface for the same contract.
  let verifyOut := root / ".agent_tmp" / "tic0_verify"
  IO.FS.createDirAll verifyOut
  let v ← IO.Process.output {
    cmd := "lake"
    args := #[
      "exe", "verify", "--",
      "--iwad", (root / "fixtures" / "wads" / "doom1.wad").toString,
      "--demo", "DEMO1",
      "--ref-digest", (root / "fixtures" / "demo1.dig").toString,
      "--impl", "real",
      "--tics", "1",
      "--out-dir", verifyOut.toString,
      "--ref-trace", (root / "fixtures" / "demo1.trc").toString,
      "--root", root.toString
    ]
    cwd := root / "lean"
  }
  IO.println v.stdout
  if !v.stderr.isEmpty then IO.eprintln v.stderr
  let vout := v.stdout ++ v.stderr
  let vTic0 := vout.contains "TIC 0"
  let vLengthOnly := vout.contains "trace length mismatch" && !vTic0
  let vWrote := vout.contains "real: wrote"
  ok := (← assert "verify: ran real impl" vWrote) && ok
  ok := (← assert "verify: no TIC 0 mismatch" (!vTic0)) && ok
  ok := (← assert "verify: length-only or clean exit"
    ((v.exitCode == 0 && !vTic0) || (v.exitCode == 1 && vLengthOnly))) && ok

  if ok then
    IO.println "ALL TIC0 PARITY CHECKS PASSED"
    pure 0
  else
    IO.eprintln "SOME TIC0 CHECKS FAILED"
    pure 1
