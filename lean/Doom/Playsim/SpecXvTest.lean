import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xv unit checks: `T_VerticalDoor` wait-expire + ceiling DOWN `T_MovePlane`
+ pastdest remove. Kept out of `EnemyTest.lean` so that file stays under 1k
lines. Flips the former wait-expire / down-dir loud-error units.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXvTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def planeLevel : LevelData := {
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

private def door (direction topcountdown ceilingheight : Int32) (type_ : Int32 := vld_normal) :
    GameState :=
  let gs0 := initFromLevel planeLevel 2 #[true, false, false, false] 0
  match gs0.sectors[0]? with
  | none => gs0
  | some sec =>
    {
      gs0 with
      sectors := GameState.arrSet gs0.sectors 0 { sec with ceilingheight, specialdata := 0 }
      verticalDoors := #[{
        sector := 0, type_, topheight := 10 * FRACUNIT, speed := VDOORSPEED
        direction, topwait := VDOORWAIT, topcountdown
      }]
      thinkers := #[{ traceId := 232, func := THF_VERTICALDOOR, payload := 0 }]
    }

def checkP2cXvUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  let gsP0 := initFromLevel planeLevel 2 #[true, false, false, false] 0

  -- T_MovePlane ceiling DOWN ------------------------------------------------
  match movePlane gsP0 0 VDOORSPEED (0 : Int32) false 1 (-1) with
  | Except.error e =>
    ok := (← assert s!"T_MovePlane DOWN pastdest from 0 ({e})" false) && ok
  | Except.ok (gsD0, res) =>
    ok := (← assert "T_MovePlane DOWN 0→0 pastdest" (res == resultPastdest)) && ok
    match gsD0.sectors[0]? with
    | none => ok := (← assert "T_MovePlane DOWN 0 sector" false) && ok
    | some sec =>
      ok := (← assert "T_MovePlane DOWN 0 snaps dest"
        (sec.ceilingheight == 0)) && ok
  match movePlane gsP0 0 VDOORSPEED (10 * FRACUNIT) false 1 (-1) with
  | Except.error e =>
    ok := (← assert s!"T_MovePlane DOWN dest-above ({e})" false) && ok
  | Except.ok (gsAbove, res) =>
    ok := (← assert "T_MovePlane DOWN dest-above pastdest" (res == resultPastdest)) && ok
    match gsAbove.sectors[0]? with
    | none => ok := (← assert "T_MovePlane DOWN dest-above sector" false) && ok
    | some sec =>
      ok := (← assert "T_MovePlane DOWN dest-above snaps dest"
        (sec.ceilingheight == 10 * FRACUNIT)) && ok
  match gsP0.sectors[0]? with
  | none => ok := (← assert "gsP0 sector 0" false) && ok
  | some sec0 =>
    let gsHi := {
      gsP0 with
      sectors := GameState.arrSet gsP0.sectors 0 { sec0 with ceilingheight := 10 * FRACUNIT }
    }
    match movePlane gsHi 0 VDOORSPEED (0 : Int32) false 1 (-1) with
    | Except.error e =>
      ok := (← assert s!"T_MovePlane DOWN step ({e})" false) && ok
    | Except.ok (gsStep, res) =>
      ok := (← assert "T_MovePlane DOWN non-pastdest ok" (res == resultOk)) && ok
      match gsStep.sectors[0]? with
      | none => ok := (← assert "T_MovePlane DOWN step sector" false) && ok
      | some sec =>
        ok := (← assert "T_MovePlane DOWN ceil -= VDOORSPEED"
          (sec.ceilingheight == 10 * FRACUNIT - VDOORSPEED)) && ok
    match movePlane gsHi 0 VDOORSPEED FRACUNIT false 1 (-1) with
    | Except.error e =>
      ok := (← assert s!"T_MovePlane DOWN step toward dest ({e})" false) && ok
    | Except.ok (gsToward, res) =>
      ok := (← assert "T_MovePlane DOWN toward dest ok" (res == resultOk)) && ok
      match gsToward.sectors[0]? with
      | none => ok := (← assert "T_MovePlane DOWN toward sector" false) && ok
      | some sec =>
        ok := (← assert "T_MovePlane DOWN toward dest does not snap"
          (sec.ceilingheight == 10 * FRACUNIT - VDOORSPEED)) && ok
    let gsOver := {
      gsP0 with
      sectors := GameState.arrSet gsP0.sectors 0 { sec0 with ceilingheight := VDOORSPEED }
    }
    match movePlane gsOver 0 VDOORSPEED FRACUNIT false 1 (-1) with
    | Except.error e =>
      ok := (← assert s!"T_MovePlane DOWN pastdest snap ({e})" false) && ok
    | Except.ok (gsPast, res) =>
      ok := (← assert "T_MovePlane DOWN pastdest" (res == resultPastdest)) && ok
      match gsPast.sectors[0]? with
      | none => ok := (← assert "T_MovePlane DOWN pastdest sector" false) && ok
      | some sec =>
        ok := (← assert "T_MovePlane DOWN pastdest snaps to dest"
          (sec.ceilingheight == FRACUNIT)) && ok

  -- wait-expire (flipped loud-error): direction=0, countdown 1→0, sfx_dorcls
  let gsWaitZero := door 0 1 (10 * FRACUNIT)
  match verticalDoorThinker gsWaitZero 0 with
  | Except.error e =>
    ok := (← assert s!"T_VerticalDoor wait-expire ({e})" false) && ok
  | Except.ok gsExp =>
    match gsExp.verticalDoors[0]?, gsExp.sectors[0]? with
    | some d, some sec =>
      ok := (← assert "T_VerticalDoor wait-expire direction=-1"
        (d.direction == (-1 : Int32) && d.topcountdown == 0)) && ok
      ok := (← assert "T_VerticalDoor wait-expire no T_MovePlane"
        (sec.ceilingheight == 10 * FRACUNIT)) && ok
      ok := (← assert "T_VerticalDoor wait-expire sfx_dorcls M_Random"
        (gsExp.rng.rndindex == gsWaitZero.rng.rndindex + 1)) && ok
      match gsExp.thinkers[0]? with
      | none => ok := (← assert "wait-expire thinker present" false) && ok
      | some th =>
        ok := (← assert "wait-expire thinker stays VERTICALDOOR"
          (th.func == THF_VERTICALDOOR)) && ok
    | _, _ =>
      ok := (← assert "wait-expire door/sector present" false) && ok
  let gsWaitBlaze := door 0 1 (10 * FRACUNIT) (type_ := (1 : Int32))
  match verticalDoorThinker gsWaitBlaze 0 with
  | Except.error e =>
    ok := (← assert "T_VerticalDoor wait-expire other type loud-error"
      (e.contains "type")) && ok
  | Except.ok _ =>
    ok := (← assert "T_VerticalDoor wait-expire other type should loud-error" false) && ok

  -- DOWN (flipped loud-error): one step, then pastdest remove
  let gsDown := door (-1) 0 (10 * FRACUNIT)
  match verticalDoorThinker gsDown 0 with
  | Except.error e =>
    ok := (← assert s!"T_VerticalDoor down-dir step ({e})" false) && ok
  | Except.ok gsDn1 =>
    match gsDn1.sectors[0]?, gsDn1.thinkers[0]? with
    | some sec, some th =>
      ok := (← assert "T_VerticalDoor DOWN step ceil -= VDOORSPEED"
        (sec.ceilingheight == 10 * FRACUNIT - VDOORSPEED)) && ok
      ok := (← assert "T_VerticalDoor DOWN step keeps specialdata"
        (sec.specialdata == 0)) && ok
      ok := (← assert "T_VerticalDoor DOWN step thinker live"
        (th.func == THF_VERTICALDOOR)) && ok
    | _, _ =>
      ok := (← assert "DOWN step sector/thinker" false) && ok
  let gsDownPast := door (-1) 0 (FRACUNIT / (2 : Int32))
  match verticalDoorThinker gsDownPast 0 with
  | Except.error e =>
    ok := (← assert s!"T_VerticalDoor DOWN pastdest ({e})" false) && ok
  | Except.ok gsPast =>
    match gsPast.sectors[0]?, gsPast.thinkers[0]? with
    | some sec, some th =>
      ok := (← assert "T_VerticalDoor DOWN pastdest snaps floor"
        (sec.ceilingheight == 0)) && ok
      ok := (← assert "T_VerticalDoor DOWN pastdest specialdata=-1"
        (sec.specialdata == -1)) && ok
      ok := (← assert "T_VerticalDoor DOWN pastdest THF_REMOVED"
        (th.func == THF_REMOVED)) && ok
    | _, _ =>
      ok := (← assert "DOWN pastdest sector/thinker" false) && ok
  -- dest is sector.floorheight, not hardcoded 0
  let gsNz :=
    let d0 := door (-1) 0 (3 * FRACUNIT + FRACUNIT / (2 : Int32))
    match d0.sectors[0]? with
    | none => d0
    | some sec =>
      { d0 with
        sectors := GameState.arrSet d0.sectors 0 { sec with floorheight := 3 * FRACUNIT } }
  match verticalDoorThinker gsNz 0 with
  | Except.error e =>
    ok := (← assert s!"T_VerticalDoor DOWN dest=floorheight ({e})" false) && ok
  | Except.ok gsNz1 =>
    match gsNz1.sectors[0]? with
    | none => ok := (← assert "nonzero-floor result sector" false) && ok
    | some sec =>
      ok := (← assert "T_VerticalDoor DOWN pastdest snaps to floorheight not 0"
        (sec.ceilingheight == 3 * FRACUNIT && sec.specialdata == -1)) && ok
  let gsDownType := door (-1) 0 (10 * FRACUNIT) (type_ := (1 : Int32))
  match verticalDoorThinker gsDownType 0 with
  | Except.error e =>
    ok := (← assert "T_VerticalDoor DOWN other type loud-error"
      (e.contains "type")) && ok
  | Except.ok _ =>
    ok := (← assert "T_VerticalDoor DOWN other type should loud-error" false) && ok
  let gsDir2 := door (2 : Int32) 0 (10 * FRACUNIT)
  match verticalDoorThinker gsDir2 0 with
  | Except.error e =>
    ok := (← assert "T_VerticalDoor direction 2 loud-error"
      (e.contains "direction")) && ok
  | Except.ok _ =>
    ok := (← assert "T_VerticalDoor direction 2 should loud-error" false) && ok

  pure ok

end Doom.Playsim.SpecXvTest
