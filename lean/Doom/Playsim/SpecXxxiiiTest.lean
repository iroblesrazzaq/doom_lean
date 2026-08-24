import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xxxiii unit checks: `EV_VerticalDoor` closing-door reopen
(`direction -1 → 1`) for player and monster. No sound, no new thinker, no
`T_MovePlane` in the use. Same-tic `T_VerticalDoor` then UPs. Monster
no-close stays identity only when `direction != -1`. Player reverse-close
on busy raise (`direction 1→-1`). Plat-misuse, missing payload, and specials
28/117 stay loud-error.
Special 26/27 with no key is deny (`sfx_oof`), not loud-error.
Kept out of `EnemyTest.lean` so that file stays under 1k.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXxxiiiTest

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

private def planeLevel : LevelData := {
  vertexes := #[]
  sectors := #[dummySec (10 * FRACUNIT)]
  sides := #[dummySide]
  lines := #[mkLine 1]
  segs := #[]
  subsectors := #[]
  nodes := #[]
  things := #[]
  blockmap := emptyBlockmap
  reject := ByteArray.empty
}

private def dummyPlat : Plat := {
  sector := 0, speed := 0, low := 0, high := 0, wait := 0, count := 0
  status := 0, oldstatus := 0, crush := false, tag := 0, type_ := 0
}

/-- Sector 0 busy with a `vld_normal` door at `verticalDoors[0]`. -/
private def busyDoorGs (player direction : Int32) (special : Int32 := 1) : GameState :=
  let gs0 := initFromLevel planeLevel 2 #[true, false, false, false] 0
  match gs0.sectors[0]? with
  | none => gs0
  | some sec =>
    {
      gs0 with
      level := { gs0.level with lines := #[mkLine special] }
      sectors := GameState.arrSet gs0.sectors 0 { sec with specialdata := 0 }
      verticalDoors := #[{
        sector := 0, type_ := vld_normal
        topheight := 20 * FRACUNIT, speed := VDOORSPEED
        direction, topwait := VDOORWAIT, topcountdown := 0
      }]
      thinkers := #[{ traceId := 232, func := THF_VERTICALDOOR, payload := 0 }]
      mobjs := #[{ Mobj.empty with player }]
    }

private def reopenOk (before after : GameState) : Bool :=
  match after.verticalDoors[0]?, after.sectors[0]?, before.sectors[0]? with
  | some door, some sec, some sec0 =>
    door.direction == (1 : Int32)
      && door.topheight == 20 * FRACUNIT
      && door.speed == VDOORSPEED
      && door.type_ == vld_normal
      && after.thinkers.size == before.thinkers.size
      && after.verticalDoors.size == before.verticalDoors.size
      && after.rng.rndindex == before.rng.rndindex
      && after.rng.prndindex == before.rng.prndindex
      && sec.ceilingheight == sec0.ceilingheight
      && sec.specialdata == (0 : Int32)
  | _, _, _ => false

private def playerCloseOk (before after : GameState) : Bool :=
  match after.verticalDoors[0]?, before.verticalDoors[0]?, after.sectors[0]?, before.sectors[0]? with
  | some door, some door0, some sec, some sec0 =>
    door.direction == (-1 : Int32)
      && door.topheight == door0.topheight
      && door.speed == door0.speed
      && door.type_ == door0.type_
      && after.thinkers.size == before.thinkers.size
      && after.verticalDoors.size == before.verticalDoors.size
      && after.rng.rndindex == before.rng.rndindex
      && after.rng.prndindex == before.rng.prndindex
      && sec.ceilingheight == sec0.ceilingheight
      && sec.specialdata == sec0.specialdata
  | _, _, _, _ => false

def checkP2cXxxiiiUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsM := busyDoorGs (-1) (-1)
  match evVerticalDoor gsM 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster reopen ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "monster reopen direction -1→1" (reopenOk gsM gs1)) && ok
    match verticalDoorThinker gs1 0 with
    | Except.error e =>
      ok := (← assert s!"monster reopen same-tic UP ({e})" false) && ok
    | Except.ok gsUp =>
      match gsUp.sectors[0]? with
      | none => ok := (← assert "monster reopen UP sector" false) && ok
      | some sec =>
        ok := (← assert "monster reopen same-tic UP += VDOORSPEED"
          (sec.ceilingheight == 10 * FRACUNIT + VDOORSPEED)) && ok

  let gsP := busyDoorGs 0 (-1)
  match evVerticalDoor gsP 0 0 with
  | Except.error e =>
    ok := (← assert s!"player reopen ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "player reopen direction -1→1" (reopenOk gsP gs1)) && ok
    match useSpecialLine gsP 0 0 0 with
    | Except.error e =>
      ok := (← assert s!"player reopen via UseSpecialLine ({e})" false) && ok
    | Except.ok (gsU, used) =>
      ok := (← assert "player reopen UseSpecialLine used" used) && ok
      ok := (← assert "player reopen UseSpecialLine fields" (reopenOk gsP gsU)) && ok

  let gsNoClose := busyDoorGs (-1) 1
  match evVerticalDoor gsNoClose 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster no-close dir=1 ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "monster no-close dir=1 identity"
      (match gs1.verticalDoors[0]? with
       | some d =>
         d.direction == (1 : Int32) && gs1.thinkers.size == gsNoClose.thinkers.size
           && gs1.rng.rndindex == gsNoClose.rng.rndindex
       | none => false)) && ok

  let gsWait := busyDoorGs (-1) 0
  match evVerticalDoor gsWait 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster no-close dir=0 ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "monster no-close dir=0 identity"
      (match gs1.verticalDoors[0]? with
       | some d => d.direction == (0 : Int32)
       | none => false)) && ok

  let gsCloseRaise := busyDoorGs 0 1
  match evVerticalDoor gsCloseRaise 0 0 with
  | Except.error e =>
    ok := (← assert s!"player close dir=1 ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "player close dir=1→-1" (playerCloseOk gsCloseRaise gs1)) && ok

  let gsMiss := { busyDoorGs 0 (-1) with verticalDoors := #[] }
  match evVerticalDoor gsMiss 0 0 with
  | Except.error e =>
    ok := (← assert "player missing payload loud-error"
      (e.contains "existing specialdata not implemented")) && ok
  | Except.ok _ =>
    ok := (← assert "player missing payload should loud-error" false) && ok

  let gsMissM := { busyDoorGs (-1) (-1) with verticalDoors := #[] }
  match evVerticalDoor gsMissM 0 0 with
  | Except.error e =>
    ok := (← assert "monster missing payload loud-error"
      (e.contains "existing specialdata not implemented")) && ok
  | Except.ok _ =>
    ok := (← assert "monster missing payload should loud-error" false) && ok

  let gsPlat := {
    busyDoorGs 0 (-1) with
    verticalDoors := #[]
    plats := #[dummyPlat]
  }
  match evVerticalDoor gsPlat 0 0 with
  | Except.error e =>
    ok := (← assert "plat-misuse loud-error"
      (e.contains "existing specialdata not implemented")) && ok
  | Except.ok _ =>
    ok := (← assert "plat-misuse should loud-error" false) && ok

  let gsNot := { busyDoorGs 0 1 with verticalDoors := #[] }
  match evVerticalDoor gsNot 0 0 with
  | Except.error e =>
    ok := (← assert "not a door or plat loud-error"
      (e.contains "existing specialdata not implemented")) && ok
  | Except.ok _ =>
    ok := (← assert "not a door or plat should loud-error" false) && ok

  let gs26 := busyDoorGs 0 (-1) 26
  match useSpecialLine gs26 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"special 26 deny ({e})" false) && ok
  | Except.ok (gsDeny, used) =>
    ok := (← assert "special 26 deny used" used) && ok
    ok := (← assert "special 26 deny oof rnd+1"
      (gsDeny.rng.rndindex == gs26.rng.rndindex + 1)) && ok
    ok := (← assert "special 26 deny keeps special=26 no reopen"
      (match gsDeny.level.lines[0]?, gsDeny.verticalDoors[0]? with
       | some ld, some d =>
         ld.special == 26 && d.direction == (-1 : Int32)
           && gsDeny.thinkers.size == gs26.thinkers.size
           && gsDeny.verticalDoors.size == 1
       | _, _ => false)) && ok
  let gs27 := busyDoorGs 0 (-1) 27
  match useSpecialLine gs27 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"special 27 deny ({e})" false) && ok
  | Except.ok (gsDeny, used) =>
    ok := (← assert "special 27 deny used" used) && ok
    ok := (← assert "special 27 deny oof rnd+1"
      (gsDeny.rng.rndindex == gs27.rng.rndindex + 1)) && ok
    ok := (← assert "special 27 deny keeps special=27 no reopen"
      (match gsDeny.level.lines[0]?, gsDeny.verticalDoors[0]? with
       | some ld, some d =>
         ld.special == 27 && d.direction == (-1 : Int32)
           && gsDeny.thinkers.size == gs27.thinkers.size
           && gsDeny.verticalDoors.size == 1
       | _, _ => false)) && ok
  match useSpecialLine (busyDoorGs 0 (-1) 28) 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"special 28 deny ({e})" false) && ok
  | Except.ok (gsDeny, used) =>
    ok := (← assert "special 28 deny used" used) && ok
    ok := (← assert "special 28 deny oof rnd+1"
      (gsDeny.rng.rndindex == (busyDoorGs 0 (-1) 28).rng.rndindex + 1)) && ok
    ok := (← assert "special 28 deny keeps special=28 no reopen"
      (match gsDeny.level.lines[0]?, gsDeny.verticalDoors[0]? with
       | some ld, some d =>
         ld.special == 28 && d.direction == (-1 : Int32)
           && gsDeny.thinkers.size == (busyDoorGs 0 (-1) 28).thinkers.size
           && gsDeny.verticalDoors.size == 1
       | _, _ => false)) && ok
  match useSpecialLine (busyDoorGs 0 (-1) 117) 0 0 0 with
  | Except.error e =>
    ok := (← assert "special 117 loud-error" (e.contains "door special 117")) && ok
  | Except.ok _ =>
    ok := (← assert "special 117 should loud-error" false) && ok

  let gsTwo0 := busyDoorGs (-1) 0
  let gsTwo :=
    match gsTwo0.sectors[0]? with
    | none => gsTwo0
    | some sec =>
      let d0 := {
        sector := 0, type_ := vld_normal, topheight := 20 * FRACUNIT
        speed := VDOORSPEED, direction := 0, topwait := VDOORWAIT
        topcountdown := 0
      }
      let d1 := { d0 with direction := (-1 : Int32) }
      {
        gsTwo0 with
        sectors := GameState.arrSet gsTwo0.sectors 0 { sec with specialdata := (1 : Int32) }
        verticalDoors := #[d0, d1]
      }
  match evVerticalDoor gsTwo 0 0 with
  | Except.error e =>
    ok := (← assert s!"payload 1 reopen ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "payload 1 reopen flips door 1 only"
      (match gs1.verticalDoors[0]?, gs1.verticalDoors[1]? with
       | some d0, some d1 =>
         d0.direction == (0 : Int32) && d1.direction == (1 : Int32)
       | _, _ => false)) && ok

  pure ok

end Doom.Playsim.SpecXxxiiiTest
