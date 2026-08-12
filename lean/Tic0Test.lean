import Doom.Harness.Real
import Doom.Playsim.Demo
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Spawn
import Doom.Playsim.Tick
import Doom.Wad

/-!
P2a-ii behavior lock: DEMO1 tic-0 after first `G_Ticker`.

E2E contract (public surface `verify --impl real --tics 1`):
- candidate.dig tic 0 must match fixtures/demo1.dig tic 0 (no "TIC 0" field
  mismatch; length mismatch alone is OK for this chunk).
- Fixture goldens: gametic=0 in_level=1 leveltime=1 rndindex=1 prndindex=121
  player_count=1 thinker_count=230 active_sector_count=0
  sectors_digest=0x6e561cdacf836d38 viewz=2686976 pendingweapon=10.

This chunk's approved soft-split: REJECT-only sight must loud-error when BSP
is required rather than porting `p_sight` BSP here.
-/

open Doom.Wad
open Doom.Playsim.Demo
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Spawn
open Doom.Playsim.Tick

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
        -- Soft-split negative check: REJECT-clear must not silently continue.
        match gTicker gs0 with
        | Except.ok _ =>
          ok := (← assert "soft-split: expected BSP sight error" false) && ok
        | Except.error msg =>
          ok := (← assert "soft-split loud error"
            (msg.contains "BSP sight required")) && ok
          IO.println s!"INFO: soft-split message: {msg}"

  if ok then
    IO.println "ALL TIC0 SOFT-SPLIT CHECKS PASSED"
    IO.println "NOTE: full tic-0 digest E2E blocked until BSP sight is authorized"
    pure 0
  else
    IO.eprintln "SOME TIC0 CHECKS FAILED"
    pure 1
