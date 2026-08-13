import Doom.Playsim.Angle
import Doom.Playsim.Combat
import Doom.Playsim.Demo
import Doom.Playsim.Enemy
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Player
import Doom.Playsim.Random
import Doom.Playsim.Sound
import Doom.Playsim.Tables

/-!
P2c-v implementation tests: A_FaceTarget, P_DamageMobj pain path, plus
retained P2c-iii helpers.
-/

open Doom.Playsim.Angle
open Doom.Playsim.Combat
open Doom.Playsim.Demo
open Doom.Playsim.Enemy
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Player
open Doom.Playsim.Random
open Doom.Playsim.Sound
open Doom.Playsim.Tables

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

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
    players := arrSet gsF0.players 0 pl
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

  if ok then
    IO.println "ALL P2c-v ENEMY UNIT CHECKS PASSED"
    pure 0
  else
    IO.eprintln "SOME P2c-v ENEMY UNIT CHECKS FAILED"
    pure 1
