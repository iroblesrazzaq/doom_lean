import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-v unit checks: `EV_VerticalDoor` / `P_UseSpecialLine` special 26
(blue DR). Keyed open matches special 1 (`vld_normal`, `sfx_doropn`, does
not clear `line.special`). No-key deny is `S_StartSound(NULL, sfx_oof)`
(always one pitch draw). Monster `useSpecialLine` 26 is false. Direct
`evVerticalDoor` with no player is identity. Lock check before 1-sided /
specialdata. 28/32–34/117/118 stay unimplemented.
Kept out of `EnemyTest.lean` so that file stays under 1k.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXxxviBTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def emptyBlockmap : BlockMap :=
  { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }

private def dummySec (ceilingheight : Int32) : Sector := {
  floorheight := 0, ceilingheight
  floorpic := ByteArray.empty, ceilingpic := ByteArray.empty
  lightlevel := 0, special := 0, tag := 0
  lines := #[], blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def dummySide : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := ByteArray.empty, bottomtexture := ByteArray.empty
  midtexture := ByteArray.empty, sector := 0
}

private def mkLine (special : Int32) : Line := {
  v1 := 0, v2 := 0, flags := 0, special, tag := 0
  sidenum0 := 0, sidenum1 := 0, dx := 0, dy := 0
  slopetype := 0, bbox := #[0, 0, 0, 0]
  frontsector := 0, backsector := 0
}

private def dummyPlat : Plat := {
  sector := 0, speed := 0, low := 0, high := 0, wait := 0, count := 0
  status := 0, oldstatus := 0, crush := false, tag := 0, type_ := 0
}

private def planeLevel (special : Int32) : LevelData := {
  vertexes := #[]
  sectors := #[dummySec (10 * FRACUNIT)]
  sides := #[dummySide]
  lines := #[mkLine special]
  segs := #[]
  subsectors := #[]
  nodes := #[]
  things := #[]
  blockmap := emptyBlockmap
  reject := ByteArray.empty
}

private def idleGs (player special : Int32) : GameState :=
  let gs0 := initFromLevel (planeLevel special) 2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player }] }

private def setCard (gs : GameState) (card : Nat) (playerIdx : Nat := 0) : GameState :=
  match gs.players[playerIdx]? with
  | none => gs
  | some p =>
    { gs with
      players := GameState.arrSet gs.players playerIdx
        { p with cards := GameState.arrSet p.cards card true } }

private def busyDoorGs (player direction special : Int32) : GameState :=
  let gs0 := idleGs player special
  match gs0.sectors[0]? with
  | none => gs0
  | some sec =>
    {
      gs0 with
      sectors := GameState.arrSet gs0.sectors 0 { sec with specialdata := 0 }
      verticalDoors := #[{
        sector := 0, type_ := vld_normal
        topheight := 20 * FRACUNIT, speed := VDOORSPEED
        direction, topwait := VDOORWAIT, topcountdown := 0
      }]
      thinkers := #[{ traceId := 232, func := THF_VERTICALDOOR, payload := 0 }]
    }

private def doorOpenOk (before after : GameState) : Bool :=
  match after.verticalDoors[0]?, after.sectors[0]?, after.level.lines[0]? with
  | some door, some sec, some ld =>
    door.type_ == vld_normal
      && door.direction == (1 : Int32)
      && door.speed == VDOORSPEED
      && after.verticalDoors.size == before.verticalDoors.size + 1
      && after.thinkers.size == before.thinkers.size + 1
      && after.rng.rndindex == before.rng.rndindex + 1
      && sec.specialdata == (0 : Int32)
      && ld.special == 26
  | _, _, _ => false

private def reopenOk (before after : GameState) : Bool :=
  match after.verticalDoors[0]?, after.sectors[0]? with
  | some door, some sec =>
    door.direction == (1 : Int32)
      && after.thinkers.size == before.thinkers.size
      && after.verticalDoors.size == before.verticalDoors.size
      && after.rng.rndindex == before.rng.rndindex
      && sec.ceilingheight == (10 * FRACUNIT)
      && sec.specialdata == (0 : Int32)
  | _, _ => false

private def playerCloseOk (before after : GameState) : Bool :=
  match after.verticalDoors[0]?, before.verticalDoors[0]?, after.sectors[0]? with
  | some door, some door0, some sec =>
    door.direction == (-1 : Int32)
      && door.topheight == door0.topheight
      && door.speed == door0.speed
      && door.type_ == door0.type_
      && after.thinkers.size == before.thinkers.size
      && after.verticalDoors.size == before.verticalDoors.size
      && after.rng.rndindex == before.rng.rndindex
      && sec.ceilingheight == (10 * FRACUNIT)
      && sec.specialdata == (0 : Int32)
  | _, _, _ => false

def checkP2cXxxviBUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsCard := setCard (idleGs 0 26) it_bluecard
  match useSpecialLine gsCard 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"bluecard open ({e})" false) && ok
  | Except.ok (gs1, used) =>
    ok := (← assert "bluecard UseSpecialLine used" used) && ok
    ok := (← assert "bluecard open fields" (doorOpenOk gsCard gs1)) && ok
    match verticalDoorThinker gs1 0 with
    | Except.error e =>
      ok := (← assert s!"bluecard same-tic UP ({e})" false) && ok
    | Except.ok gsUp =>
      match gsUp.sectors[0]? with
      | none => ok := (← assert "bluecard UP sector" false) && ok
      | some sec =>
        ok := (← assert "bluecard same-tic UP += VDOORSPEED"
          (sec.ceilingheight == 10 * FRACUNIT + VDOORSPEED)) && ok

  let gsSkull := setCard (idleGs 0 26) it_blueskull
  match evVerticalDoor gsSkull 0 0 with
  | Except.error e =>
    ok := (← assert s!"blueskull open ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "blueskull open fields" (doorOpenOk gsSkull gs1)) && ok

  let gsP1 := setCard (idleGs 1 26) it_bluecard 1
  match evVerticalDoor gsP1 0 0 with
  | Except.error e =>
    ok := (← assert s!"player1 bluecard open ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "player1 bluecard open fields" (doorOpenOk gsP1 gs1)) && ok
  let gsWrong := setCard (idleGs 1 26) it_bluecard 0
  match evVerticalDoor gsWrong 0 0 with
  | Except.error e =>
    ok := (← assert s!"player1 ignores player0 card ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "player1 ignores player0 card deny oof"
      (gs1.rng.rndindex == gsWrong.rng.rndindex + 1
        && gs1.verticalDoors.size == 0)) && ok

  let gsNo := idleGs 0 26
  match useSpecialLine gsNo 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"no-key deny ({e})" false) && ok
  | Except.ok (gs1, used) =>
    ok := (← assert "no-key deny used" used) && ok
    ok := (← assert "no-key deny oof rnd+1"
      (gs1.rng.rndindex == gsNo.rng.rndindex + 1)) && ok
    ok := (← assert "no-key deny no thinker special stays 26"
      (gs1.verticalDoors.size == 0 && gs1.thinkers.size == gsNo.thinkers.size
        && (match gs1.level.lines[0]? with
            | some ld => ld.special == 26
            | none => false))) && ok

  let gsFar0 := idleGs 0 26
  let gsFar :=
    match gsFar0.players[0]? with
    | none => gsFar0
    | some p =>
      { gsFar0 with
        players := GameState.arrSet gsFar0.players 0 { p with mo := 0 }
        mobjs := #[{ Mobj.empty with player := 0, x := 2000 * FRACUNIT, y := 0 }]
      }
  match evVerticalDoor gsFar 0 0 with
  | Except.error e =>
    ok := (← assert s!"far no-key deny ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "far no-key still oof rnd+1"
      (gs1.rng.rndindex == gsFar.rng.rndindex + 1
        && gs1.verticalDoors.size == 0)) && ok

  let gsMon := idleGs (-1) 26
  match useSpecialLine gsMon 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster useSpecialLine 26 ({e})" false) && ok
  | Except.ok (gs1, used) =>
    ok := (← assert "monster useSpecialLine 26 false" (!used)) && ok
    ok := (← assert "monster useSpecialLine 26 identity"
      (gs1.thinkers.size == gsMon.thinkers.size
        && gs1.rng.rndindex == gsMon.rng.rndindex
        && gs1.verticalDoors.size == 0)) && ok
  match evVerticalDoor gsMon 0 0 with
  | Except.error e =>
    ok := (← assert s!"direct no-player identity ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "direct no-player identity no sound"
      (gs1.rng.rndindex == gsMon.rng.rndindex
        && gs1.verticalDoors.size == 0
        && gs1.thinkers.size == gsMon.thinkers.size)) && ok

  let gs1s :=
    let gs := idleGs 0 26
    { gs with level := { gs.level with lines := #[{ mkLine 26 with sidenum1 := -1 }] } }
  match evVerticalDoor gs1s 0 0 with
  | Except.error e =>
    ok := (← assert s!"no-key 1-sided should deny not 1-sided ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "no-key 1-sided deny oof before 1-sided"
      (gs1.rng.rndindex == gs1s.rng.rndindex + 1
        && gs1.verticalDoors.size == 0)) && ok

  let gs1sKey := setCard gs1s it_bluecard
  match evVerticalDoor gs1sKey 0 0 with
  | Except.error e =>
    ok := (← assert "key 1-sided loud-error" (e.contains "1-sided")) && ok
  | Except.ok _ =>
    ok := (← assert "key 1-sided should loud-error" false) && ok

  let gsClose := setCard (busyDoorGs 0 (-1) 26) it_bluecard
  match evVerticalDoor gsClose 0 0 with
  | Except.error e =>
    ok := (← assert s!"key closing reopen ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "key closing reopen" (reopenOk gsClose gs1)) && ok

  let gsRaise := setCard (busyDoorGs 0 1 26) it_bluecard
  match evVerticalDoor gsRaise 0 0 with
  | Except.error e =>
    ok := (← assert s!"key raising close ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "key raising close dir=1→-1" (playerCloseOk gsRaise gs1)) && ok

  let gsMiss := { setCard (busyDoorGs 0 (-1) 26) it_bluecard with verticalDoors := #[] }
  match evVerticalDoor gsMiss 0 0 with
  | Except.error e =>
    ok := (← assert "key missing payload loud-error"
      (e.contains "existing specialdata not implemented")) && ok
  | Except.ok _ =>
    ok := (← assert "key missing payload should loud-error" false) && ok

  let gsPlat := {
    setCard (busyDoorGs 0 (-1) 26) it_bluecard with
    verticalDoors := #[]
    plats := #[dummyPlat]
  }
  match evVerticalDoor gsPlat 0 0 with
  | Except.error e =>
    ok := (← assert "key plat-misuse loud-error"
      (e.contains "existing specialdata not implemented")) && ok
  | Except.ok _ =>
    ok := (← assert "key plat-misuse should loud-error" false) && ok

  let mut si : Nat := 0
  let stay : Array Int32 := #[117, 118]
  while si < stay.size do
    match stay[si]? with
    | none => pure ()
    | some spec =>
      match useSpecialLine (idleGs 0 spec) 0 0 0 with
      | Except.error e =>
        ok := (← assert s!"special {spec} loud-error"
          (e.contains s!"door special {spec}")) && ok
      | Except.ok _ =>
        ok := (← assert s!"special {spec} should loud-error" false) && ok
    si := si + 1

  pure ok

end Doom.Playsim.SpecXxxviBTest
