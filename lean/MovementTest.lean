import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.PlayerThink
import Doom.Playsim.Tables
import Doom.Playsim.Think
import Doom.Wad

/-!
P2c-i implementation tests: P_Thrust, FRICTION FixedMul, P_LineOpening on E1M5.
-/

open Doom.Wad
open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Level
open Doom.Playsim.MapUtil
open Doom.Playsim.PlayerThink
open Doom.Playsim.Tables
open Doom.Playsim.Think

-- Qualify Mobj / Player / GameState helpers to avoid `empty` / `arrSet` clashes.

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

/-- Minimal GS with one player mobj for thrust tests. -/
def thrustFixture : Doom.Playsim.GameState.GameState :=
  let emptyLevel : LevelData := {
    vertexes := #[]
    sectors := #[]
    sides := #[]
    lines := #[]
    segs := #[]
    subsectors := #[]
    nodes := #[]
    things := #[]
    blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
    reject := ByteArray.empty
  }
  let gs0 := Doom.Playsim.GameState.initFromLevel emptyLevel 2 #[true, false, false, false] 0
  let mo := { Doom.Playsim.Mobj.empty with player := 0 }
  let p := { Doom.Playsim.Player.empty with mo := 0, playerstate := Doom.Playsim.Player.PST_LIVE }
  { gs0 with
    mobjs := #[mo]
    players := Doom.Playsim.GameState.arrSet gs0.players 0 p
  }

def main (_args : List String) : IO UInt32 := do
  let mut ok := true

  -- FRICTION FixedMul samples (from DEMO1 tic-26 moms) ----------------
  ok := (← assert "FixedMul FRICTION 801403"
    (fixedMul (801403 : Int32) FRICTION == (726271 : Int32))) && ok
  ok := (← assert "FixedMul FRICTION -47542"
    (fixedMul (-47542 : Int32) FRICTION == (-43085 : Int32))) && ok

  -- P_Thrust at angle 0: cos=finesine[2048], sin=finesine[0] ----------
  let move : Int32 := 50 * 2048
  match finecosine[0]?, finesine[0]? with
  | some cos0, some sin0 =>
    match thrust thrustFixture 0 (0 : UInt32) move with
    | Except.error e =>
      ok := (← assert s!"P_Thrust angle0 ({e})" false) && ok
    | Except.ok gs =>
      match gs.mobjs[0]? with
      | none => ok := (← assert "P_Thrust angle0 mo" false) && ok
      | some mo =>
        ok := (← assert "P_Thrust angle0 momx"
          (mo.momx == fixedMul move cos0)) && ok
        ok := (← assert "P_Thrust angle0 momy"
          (mo.momy == fixedMul move sin0)) && ok
  | _, _ =>
    ok := (← assert "fine tables present" false) && ok

  -- P_Thrust at ANG90: fine index 2048 --------------------------------
  match finecosine[2048]?, finesine[2048]? with
  | some cos90, some sin90 =>
    match thrust thrustFixture 0 ANG90 move with
    | Except.error e =>
      ok := (← assert s!"P_Thrust ANG90 ({e})" false) && ok
    | Except.ok gs =>
      match gs.mobjs[0]? with
      | none => ok := (← assert "P_Thrust ANG90 mo" false) && ok
      | some mo =>
        ok := (← assert "P_Thrust ANG90 momx"
          (mo.momx == fixedMul move cos90)) && ok
        ok := (← assert "P_Thrust ANG90 momy"
          (mo.momy == fixedMul move sin90)) && ok
        -- finecosine[2048] == finesine[4096] == -25-ish near -1.0? actually ~0 for cos(90)?
        -- ANG90>>19 = 2048; cos(90°)=0 approx, sin=1
        ok := (← assert "P_Thrust ANG90 nearly north"
          (mo.momx == 0 || wabs mo.momx < 100)) && ok
  | _, _ =>
    ok := (← assert "fine tables ANG90" false) && ok

  -- P_LineOpening on E1M5 ---------------------------------------------
  let root ← defaultRoot
  let wad ← loadFile (root / "fixtures" / "wads" / "doom1.wad")
  match loadMap wad "E1M5" with
  | Except.error e =>
    ok := (← assert s!"load E1M5 ({e})" false) && ok
  | Except.ok level =>
    let gs := Doom.Playsim.GameState.initFromLevel level 3 #[true, false, false, false] 0
    -- line 706: floors -16 / 0 → openbottom=0, lowfloor=-16*FRACUNIT
    match level.lines[706]? with
    | none => ok := (← assert "line 706 present" false) && ok
    | some ld =>
      match lineOpening gs ld with
      | Except.error e =>
        ok := (← assert s!"lineOpening 706 ({e})" false) && ok
      | Except.ok op =>
        ok := (← assert "line706 openbottom=0" (op.openbottom == 0)) && ok
        ok := (← assert "line706 lowfloor=-16*FRACUNIT"
          (op.lowfloor == (-16 : Int32) * FRACUNIT)) && ok
        ok := (← assert "line706 opentop=72*FRACUNIT"
          (op.opentop == (72 : Int32) * FRACUNIT)) && ok
        ok := (← assert "line706 openrange"
          (op.openrange == op.opentop - op.openbottom)) && ok
    -- flat 2-sided line 49: floors 0/0 ceils 72/72
    match level.lines[49]? with
    | none => ok := (← assert "line 49 present" false) && ok
    | some ld =>
      match lineOpening gs ld with
      | Except.error e =>
        ok := (← assert s!"lineOpening 49 ({e})" false) && ok
      | Except.ok op =>
        ok := (← assert "line49 openbottom=0" (op.openbottom == 0)) && ok
        ok := (← assert "line49 lowfloor=0" (op.lowfloor == 0)) && ok
        ok := (← assert "line49 opentop=72*FRACUNIT"
          (op.opentop == (72 : Int32) * FRACUNIT)) && ok
    -- Blockmap link round-trip at player-ish coords from DEMO1 spawn
    let px : Int32 := -14680064
    let py : Int32 := -40894464
    let mo := {
      Doom.Playsim.Mobj.empty with
      x := px, y := py, radius := 16 * FRACUNIT, height := 56 * FRACUNIT
    }
    let gs1 := { gs with mobjs := #[mo] }
    match setThingPosition gs1 0 with
    | Except.error e =>
      ok := (← assert s!"setThingPosition ({e})" false) && ok
    | Except.ok gs2 =>
      let bmap := gs2.level.blockmap
      let bx := ashrMapBlock (px - bmap.originX)
      let byCoord := ashrMapBlock (py - bmap.originY)
      let bi := (byCoord * bmap.width + bx).toNatClampNeg
      let head := match gs2.blocklinks[bi]? with | some v => v | none => (-2 : Int32)
      ok := (← assert "blocklinks head after set" (head == 0)) && ok
      match gs2.mobjs[0]? with
      | none => ok := (← assert "mo after set" false) && ok
      | some mo2 =>
        ok := (← assert "bprev null after set" (mo2.bprev == -1)) && ok
      match unsetThingPosition gs2 0 with
      | Except.error e =>
        ok := (← assert s!"unsetThingPosition ({e})" false) && ok
      | Except.ok gs3 =>
        let head3 := match gs3.blocklinks[bi]? with | some v => v | none => (-2 : Int32)
        ok := (← assert "blocklinks empty after unset" (head3 == -1)) && ok

  if ok then
    IO.println "movement-test: all passed"
    pure 0
  else
    IO.eprintln "movement-test: SOME FAILED"
    pure 1
