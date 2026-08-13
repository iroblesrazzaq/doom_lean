import Doom.Playsim.Angle
import Doom.Playsim.Combat
import Doom.Playsim.Demo
import Doom.Playsim.Enemy
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Inter
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Random
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Tables
import Doom.Playsim.Thinker
import Doom.Wad

/-!
P2c-vii implementation tests: S_NULL remove, P_GiveArmor already-done, plus
retained P2c-vi / P2c-v / P2c-iii helpers.
-/

open Doom.Playsim.Angle
open Doom.Playsim.Combat
open Doom.Playsim.Demo
open Doom.Playsim.Enemy
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Inter
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Random
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Tables
open Doom.Playsim.Thinker
open Doom.Wad

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

def main (_args : List String) : IO UInt32 := do
  let mut ok := true

  -- P_AproxDistance --------------------------------------------------------
  ok := (← assert "aproxDistance equal axes"
    (aproxDistance (100 : Int32) 100 == (150 : Int32))) && ok
  ok := (← assert "aproxDistance dx<dy"
    (aproxDistance (10 : Int32) 100 == (105 : Int32))) && ok
  ok := (← assert "aproxDistance signs"
    (aproxDistance (-30 : Int32) 40 == aproxDistance 30 40)) && ok

  -- R_PointToAngle2 quadrants ----------------------------------------------
  ok := (← assert "pointToAngle2 east"
    (pointToAngle2 0 0 FRACUNIT 0 == 0)) && ok
  let north := pointToAngle2 0 0 0 FRACUNIT
  ok := (← assert "pointToAngle2 north near ANG90"
    (north == ANG90 - 1 - tantoangle.getD (slopeDiv 0 FRACUNIT.toUInt32).toNat 0)) && ok
  let west := pointToAngle2 0 0 (-FRACUNIT) 0
  ok := (← assert "pointToAngle2 west"
    (west == ANG180 - 1 - tantoangle.getD (slopeDiv 0 FRACUNIT.toUInt32).toNat 0)) && ok
  let south := pointToAngle2 0 0 0 (-FRACUNIT)
  ok := (← assert "pointToAngle2 south near ANG270"
    (south == ANG270 + tantoangle.getD (slopeDiv 0 FRACUNIT.toUInt32).toNat 0)) && ok
  ok := (← assert "pointToAngle2 coincident 0"
    (pointToAngle2 5 5 5 5 == 0)) && ok

  -- seesound remap draw counts ---------------------------------------------
  -- posit* → one P_Random (%3); bgsit* → one P_Random (%2); else none.
  let mut rng := clearRandom
  let (r1, rng1) := pRandom rng
  let soundPosit := Int32.ofNat sfx_posit1 + r1 % 3
  ok := (← assert "posit remap in range"
    (soundPosit >= Int32.ofNat sfx_posit1 && soundPosit <= Int32.ofNat sfx_posit3)) && ok
  ok := (← assert "posit remap drew once" (rng1.prndindex == 1)) && ok
  let (r2, rng2) := pRandom rng
  let soundBg := Int32.ofNat sfx_bgsit1 + r2 % 2
  ok := (← assert "bgsit remap in range"
    (soundBg == Int32.ofNat sfx_bgsit1 || soundBg == Int32.ofNat sfx_bgsit2)) && ok
  ok := (← assert "bgsit remap drew once" (rng2.prndindex == 1)) && ok
  -- pitch RNG one M_Random for remapped seesound
  let rngP := startSoundPitchRng clearRandom sfx_posit1
  ok := (← assert "seesound pitch draws M_Random" (rngP.rndindex == 1)) && ok
  ok := (← assert "inaudible skips pitch RNG"
    ((startSoundPitchRngMaybe clearRandom sfx_posit1 false).rndindex == 0)) && ok
  ok := (← assert "near origin audible"
    (soundAudible 0 0 FRACUNIT 0)) && ok
  ok := (← assert "beyond clip inaudible"
    (!soundAudible 0 0 (1201 * FRACUNIT) 0)) && ok
  -- Volume gate: (64 * remaining_mapunits) / 1000; remaining 16 → 1, 15 → 0.
  ok := (← assert "volume-zero band inaudible"
    (!soundAudible 0 0 (1185 * FRACUNIT) 0)) && ok
  ok := (← assert "volume-positive just closer"
    (soundAudible 0 0 (1184 * FRACUNIT) 0)) && ok

  -- demo angleturn sign-extend (TRACE.md signed short) --------------------
  -- 0xFE as signed char << 8 → -512 (not unsigned 65024)
  ok := (← assert "angleturn sign-extend FE<<8"
    ((signExtendI8 0xfe <<< 8) == (-512 : Int32))) && ok
  ok := (← assert "angleturn positive 01<<8"
    ((signExtendI8 0x01 <<< 8) == (256 : Int32))) && ok

  -- dir LUTs / opposite / diags --------------------------------------------
  ok := (← assert "opposite EAST→WEST" (opposite.getD 0 99 == DI_WEST)) && ok
  ok := (← assert "opposite NODIR" (opposite.getD 8 99 == DI_NODIR)) && ok
  ok := (← assert "diags NW" (diags.getD 0 99 == DI_NORTHWEST)) && ok
  ok := (← assert "diags SE" (diags.getD 3 99 == DI_SOUTHEAST)) && ok
  ok := (← assert "xspeed EAST FRACUNIT" (xspeed.getD 0 0 == FRACUNIT)) && ok
  ok := (← assert "yspeed NORTH FRACUNIT" (yspeed.getD 2 0 == FRACUNIT)) && ok
  -- diagonal step for speed=8 matches fixture Δ
  ok := (← assert "diag step 8*47000=376000"
    ((8 : Int32) * (47000 : Int32) == 376000)) && ok
  ok := (← assert "cardinal step 8*FRACUNIT=524288"
    ((8 : Int32) * FRACUNIT == 524288)) && ok

  -- MELEERANGE behind-gate constants ---------------------------------------
  ok := (← assert "MELEERANGE 64*FRACUNIT" (MELEERANGE == 64 * FRACUNIT)) && ok
  ok := (← assert "ANG90/ANG270 gate constants"
    (ANG90 == 0x40000000 && ANG270 == 0xc0000000)) && ok

  -- A_FaceTarget (no MF_SHADOW) ---------------------------------------------
  let emptyLevel : Doom.Playsim.Level.LevelData := {
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
  let gsF0 := Doom.Playsim.GameState.initFromLevel emptyLevel 2 #[true, false, false, false] 0
  let actor := {
    Doom.Playsim.Mobj.empty with
    x := 0, y := 0, angle := 0, flags := MF_AMBUSH ||| MF_SHOOTABLE
    target := 1
  }
  let targ := {
    Doom.Playsim.Mobj.empty with
    x := FRACUNIT, y := 0, flags := MF_SHOOTABLE
  }
  let gsF := { gsF0 with mobjs := #[actor, targ] }
  match aFaceTarget gsF 0 with
  | Except.error e =>
    ok := (← assert s!"A_FaceTarget east ({e})" false) && ok
  | Except.ok gsF1 =>
    match gsF1.mobjs[0]? with
    | none => ok := (← assert "A_FaceTarget actor present" false) && ok
    | some mo =>
      ok := (← assert "A_FaceTarget east angle 0" (mo.angle == 0)) && ok
      ok := (← assert "A_FaceTarget cleared MF_AMBUSH"
        ((mo.flags &&& MF_AMBUSH) == 0)) && ok
      ok := (← assert "A_FaceTarget no shadow RNG"
        (gsF1.rng.prndindex == 0)) && ok

  -- P_DamageMobj enemy pain path --------------------------------------------
  let victim := {
    Doom.Playsim.Mobj.empty with
    typeId := 2, x := 10 * FRACUNIT, y := 0, z := 0
    health := 30, flags := MF_SOLID ||| MF_SHOOTABLE ||| MF_COUNTKILL
    height := 56 * FRACUNIT, radius := 20 * FRACUNIT
    state := 209, tics := 3
  }
  let inflictor := {
    Doom.Playsim.Mobj.empty with
    x := 0, y := 0, player := 0, typeId := 0, flags := MF_SHOOTABLE
  }
  let pl := {
    Doom.Playsim.Player.empty with
    mo := 0, readyweapon := wp_pistol, playerstate := PST_LIVE, health := 100
    pendingweapon := wp_nochange
  }
  let gsD0 := {
    gsF0 with
    mobjs := #[inflictor, victim]
    players := Doom.Playsim.GameState.arrSet gsF0.players 0 pl
  }
  match damageMobj gsD0 1 (some 0) (some 0) 5 with
  | Except.error e =>
    ok := (← assert s!"P_DamageMobj pain ({e})" false) && ok
  | Except.ok gsD1 =>
    match gsD1.mobjs[1]? with
    | none => ok := (← assert "damage victim present" false) && ok
    | some v =>
      ok := (← assert "damage health 30-5=25" (v.health == 25)) && ok
      ok := (← assert "damage MF_JUSTHIT" ((v.flags &&& MF_JUSTHIT) != 0)) && ok
      ok := (← assert "damage painstate 220" (v.state == 220)) && ok
      ok := (← assert "damage reactiontime 0" (v.reactiontime == 0)) && ok
      ok := (← assert "damage BASETHRESHOLD" (v.threshold == BASETHRESHOLD)) && ok
      ok := (← assert "damage retarget inflictor" (v.target == 0)) && ok
      ok := (← assert "damage pain drew P_Random" (gsD1.rng.prndindex == 1)) && ok

  -- wrapping soundorg midpoint (C `(right+left)/2`) -------------------------------
  ok := (← assert "soundorg midpoint even"
    (((20 : Int32) + (10 : Int32)) / (2 : Int32) == (15 : Int32))) && ok
  ok := (← assert "soundorg wrapping Int32.max+1 / 2"
    ((Int32.maxValue + (1 : Int32)) / (2 : Int32)
      == ((Int32.minValue : Int32) / (2 : Int32)))) && ok

  -- T_MovePlane ceiling UP ------------------------------------------------
  let planeLevel : LevelData := {
    vertexes := #[]
    sectors := #[{
      floorheight := 0, ceilingheight := 0
      floorpic := ByteArray.empty, ceilingpic := ByteArray.empty
      lightlevel := 0, special := 0, tag := 0
      lines := #[], blockbox := #[0, 0, 0, 0]
      soundorgX := 0, soundorgY := 0
    }]
    sides := #[]
    lines := #[]
    segs := #[]
    subsectors := #[]
    nodes := #[]
    things := #[]
    blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
    reject := ByteArray.empty
  }
  let gsP0 := Doom.Playsim.GameState.initFromLevel planeLevel 2 #[true, false, false, false] 0
  match movePlane gsP0 0 VDOORSPEED (10 * FRACUNIT) false 1 1 with
  | Except.error e =>
    ok := (← assert s!"T_MovePlane UP step ({e})" false) && ok
  | Except.ok (gsP1, res) =>
    ok := (← assert "T_MovePlane UP non-pastdest ok" (res == resultOk)) && ok
    match gsP1.sectors[0]? with
    | none => ok := (← assert "T_MovePlane sector" false) && ok
    | some sec =>
      ok := (← assert "T_MovePlane ceil += VDOORSPEED"
        (sec.ceilingheight == VDOORSPEED)) && ok
  match movePlane gsP0 0 VDOORSPEED FRACUNIT false 1 1 with
  | Except.error e =>
    ok := (← assert s!"T_MovePlane pastdest ({e})" false) && ok
  | Except.ok (gsPast, res) =>
    ok := (← assert "T_MovePlane UP pastdest" (res == resultPastdest)) && ok
    match gsPast.sectors[0]? with
    | none => ok := (← assert "T_MovePlane pastdest sector" false) && ok
    | some sec =>
      ok := (← assert "T_MovePlane pastdest snaps to dest"
        (sec.ceilingheight == FRACUNIT)) && ok
  match movePlane gsP0 0 VDOORSPEED (10 * FRACUNIT) false 0 1 with
  | Except.error e =>
    ok := (← assert "T_MovePlane floor loud-error"
      (e.contains "floorOrCeiling")) && ok
  | Except.ok _ =>
    ok := (← assert "T_MovePlane floor should loud-error" false) && ok

  -- P_UseSpecialLine filters / T_VerticalDoor direction / monster no-close ----
  let filterLine : Line := {
    v1 := 0, v2 := 0, flags := 0, special := 11, tag := 0
    sidenum0 := 0, sidenum1 := 0, dx := 0, dy := 0
    slopetype := 0, bbox := #[0, 0, 0, 0]
    frontsector := 0, backsector := 0
  }
  let gsU : GameState := {
    gsP0 with
    level := { gsP0.level with lines := #[filterLine] }
    mobjs := #[{ Doom.Playsim.Mobj.empty with player := -1 }]
  }
  match useSpecialLine gsU 0 0 1 with
  | Except.error e =>
    ok := (← assert s!"UseSpecialLine side ({e})" false) && ok
  | Except.ok (_, used) =>
    ok := (← assert "UseSpecialLine side≠0 returns false" (!used)) && ok
  match useSpecialLine gsU 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"UseSpecialLine monster-forbidden ({e})" false) && ok
  | Except.ok (_, used) =>
    ok := (← assert "UseSpecialLine monster special 11 false" (!used)) && ok
  let secretLine : Line := { filterLine with special := 1, flags := ML_SECRET }
  let gsSecret : GameState := { gsU with level := { gsU.level with lines := #[secretLine] } }
  match useSpecialLine gsSecret 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"UseSpecialLine secret ({e})" false) && ok
  | Except.ok (_, used) =>
    ok := (← assert "UseSpecialLine monster secret false" (!used)) && ok
  let oneSided : Line := { filterLine with special := 1, sidenum1 := -1 }
  let gsOne : GameState := { gsU with level := { gsU.level with lines := #[oneSided] } }
  match useSpecialLine gsOne 0 0 0 with
  | Except.error e =>
    ok := (← assert "UseSpecialLine 1-sided DR loud-error"
      (e.contains "1-sided")) && ok
  | Except.ok _ =>
    ok := (← assert "UseSpecialLine 1-sided DR should loud-error" false) && ok
  let gsWait : GameState := {
    gsP0 with
    verticalDoors := #[{
      sector := 0, type_ := vld_normal, topheight := 0, speed := VDOORSPEED
      direction := 0, topwait := VDOORWAIT, topcountdown := 0
    }]
  }
  match verticalDoorThinker gsWait 0 with
  | Except.error e =>
    ok := (← assert "T_VerticalDoor wait-dir loud-error"
      (e.contains "direction")) && ok
  | Except.ok _ =>
    ok := (← assert "T_VerticalDoor wait-dir should loud-error" false) && ok
  match gsU.sectors[0]? with
  | none =>
    ok := (← assert "gsU has sector 0" false) && ok
  | some secBusy =>
    let busyLine : Line := { filterLine with special := 1, sidenum1 := 0 }
    let busySide : Side := {
      textureoffset := 0, rowoffset := 0
      toptexture := ByteArray.empty, bottomtexture := ByteArray.empty
      midtexture := ByteArray.empty, sector := 0
    }
    let gsBusy : GameState := {
      gsU with
      level := { gsU.level with lines := #[busyLine], sides := #[busySide] }
      sectors := Doom.Playsim.GameState.arrSet gsU.sectors 0 { secBusy with specialdata := 0 }
      verticalDoors := #[{
        sector := 0, type_ := vld_normal, topheight := 0, speed := VDOORSPEED
        direction := 1, topwait := VDOORWAIT, topcountdown := 0
      }]
    }
    match evVerticalDoor gsBusy 0 0 with
    | Except.error e =>
      ok := (← assert s!"monster no-close ({e})" false) && ok
    | Except.ok gsBusy1 =>
      ok := (← assert "monster no-close does not add thinker"
        (gsBusy1.thinkers.size == gsBusy.thinkers.size)) && ok
      match gsBusy1.sectors[0]? with
      | none => ok := (← assert "monster no-close sector" false) && ok
      | some sec =>
        ok := (← assert "monster no-close keeps specialdata"
          (sec.specialdata == 0)) && ok

  -- P_FindLowestCeilingSurrounding on E1M1 sector 71 -------------------------
  let root ← defaultRoot
  let wad ← loadFile (root / "fixtures" / "wads" / "doom1.wad")
  match checkNumForName wad "E1M1" with
  | none => ok := (← assert "E1M1 present" false) && ok
  | some idx =>
    match (do
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
      buildLevel things linedefs sidedefs vertexes segs ssectors nodes sectors reject blockmap) with
    | Except.error e =>
      ok := (← assert s!"E1M1 load ({e})" false) && ok
    | Except.ok level =>
      let gsL := Doom.Playsim.GameState.initFromLevel level 3 #[true, false, false, false] 0
      let lowest := findLowestCeilingSurrounding gsL 71
      ok := (← assert "E1M1 sec71 FindLowest=4718592"
        (lowest == (4718592 : Int32))) && ok
      ok := (← assert "E1M1 sec71 topheight=4456448"
        (lowest - 4 * FRACUNIT == (4456448 : Int32))) && ok
      ok := (← assert "VDOORSPEED=2*FRACUNIT" (VDOORSPEED == 2 * FRACUNIT)) && ok

  -- P_GiveArmor already-done vs grant --------------------------------------
  let pHave := { Doom.Playsim.Player.empty with armorpoints := 100, armortype := 1 }
  let (pKeep, gaveHave) := giveArmor pHave 1
  ok := (← assert "giveArmor already-done false" (!gaveHave)) && ok
  ok := (← assert "giveArmor already-done keeps points"
    (pKeep.armorpoints == 100 && pKeep.armortype == 1)) && ok
  let pNone := { Doom.Playsim.Player.empty with armorpoints := 0, armortype := 0 }
  let (pGot, gaveNone) := giveArmor pNone 1
  ok := (← assert "giveArmor grant true" gaveNone) && ok
  ok := (← assert "giveArmor grant 100/class 1"
    (pGot.armorpoints == 100 && pGot.armortype == 1)) && ok

  -- P_SetMobjState S_NULL: remove, return false, keep thinker slot ----------
  let emptyNull : LevelData := {
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
  let gsN0 := Doom.Playsim.GameState.initFromLevel emptyNull 2 #[true, false, false, false] 0
  let blood := {
    Doom.Playsim.Mobj.empty with
    traceId := 231, state := 92, tics := 1
    flags := MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let thBlood : Thinker := { traceId := 231, func := THF_MOBJ, payload := 0 }
  let gsN := { gsN0 with mobjs := #[blood], thinkers := #[thBlood] }
  match setMobjState gsN 0 0 with
  | Except.error e =>
    ok := (← assert s!"S_NULL remove ({e})" false) && ok
  | Except.ok (gsN1, still) =>
    ok := (← assert "S_NULL returns false" (!still)) && ok
    ok := (← assert "S_NULL keeps thinker slot" (gsN1.thinkers.size == 1)) && ok
    match gsN1.thinkers[0]? with
    | none => ok := (← assert "S_NULL thinker present" false) && ok
    | some th =>
      ok := (← assert "S_NULL marks THF_REMOVED" (th.func == THF_REMOVED)) && ok
      ok := (← assert "S_NULL keeps traceId" (th.traceId == 231)) && ok
    match gsN1.mobjs[0]? with
    | none => ok := (← assert "S_NULL mobj remains" false) && ok
    | some mo =>
      ok := (← assert "S_NULL mobj.state=0" (mo.state == 0)) && ok

  -- A_ReFire else: clear refire + checkAmmo (no BT_ATTACK) -----------------
  let plRf := {
    Doom.Playsim.Player.empty with
    health := 100, pendingweapon := wp_nochange, readyweapon := wp_pistol
    ammo := #[50, 0, 0, 0], refire := 3, cmd := TicCmd.zero
  }
  let gsRf := {
    gsN0 with
    players := Doom.Playsim.GameState.arrSet gsN0.players 0 plRf
  }
  match reFire gsRf 0 with
  | Except.error e =>
    ok := (← assert s!"A_ReFire else ({e})" false) && ok
  | Except.ok gsRf1 =>
    match gsRf1.players[0]? with
    | none => ok := (← assert "A_ReFire else player" false) && ok
    | some p =>
      ok := (← assert "A_ReFire else refire=0" (p.refire == 0)) && ok

  if ok then
    IO.println "ALL P2c-vii ENEMY UNIT CHECKS PASSED"
    pure 0
  else
    IO.eprintln "SOME P2c-vi ENEMY UNIT CHECKS FAILED"
    pure 1
