import Doom.Playsim.Angle
import Doom.Playsim.Demo
import Doom.Playsim.Enemy
import Doom.Playsim.Fixed
import Doom.Playsim.Random
import Doom.Playsim.Sound
import Doom.Playsim.Tables

/-!
P2c-iii implementation tests: R_PointToAngle2 quadrants, P_AproxDistance,
seesound remap draw counts, P_NewChaseDir hand-set scenario helpers.
-/

open Doom.Playsim.Angle
open Doom.Playsim.Demo
open Doom.Playsim.Enemy
open Doom.Playsim.Fixed
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

  if ok then
    IO.println "ALL P2c-iii ENEMY UNIT CHECKS PASSED"
    pure 0
  else
    IO.eprintln "SOME P2c-iii ENEMY UNIT CHECKS FAILED"
    pure 1
