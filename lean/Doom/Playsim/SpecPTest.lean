import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-p unit checks: `P_UseSpecialLine` / `EV_VerticalDoor` special 31
(manual open door). Player → `vld_open`, `sfx_doropn`, clears `line.special`.
Monster → `(gs0, false)`. Busy-sector reopen only for raise types; 31 falls
through to new door. Kept out of `EnemyTest.lean` so that file stays under 1k.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecPTest

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

private def openDoorOk (before after : GameState) : Bool :=
  match after.verticalDoors[0]?, after.sectors[0]?, after.level.lines[0]? with
  | some door, some sec, some ld =>
    door.type_ == vld_open
      && door.direction == (1 : Int32)
      && door.speed == VDOORSPEED
      && after.verticalDoors.size == before.verticalDoors.size + 1
      && after.thinkers.size == before.thinkers.size + 1
      && after.rng.rndindex == before.rng.rndindex + 1
      && sec.specialdata == (0 : Int32)
      && ld.special == 0
  | _, _, _ => false

private def hasDoorThinker (gs : GameState) : Bool :=
  Id.run do
    let mut found := false
    for th in gs.thinkers do
      if th.func == THF_VERTICALDOOR && th.payload == 0 then
        found := true
    pure found

def checkP2cPUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsP := idleGs 0 31
  match useSpecialLine gsP 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player useSpecialLine 31 ({e})" false) && ok
  | Except.ok (gs1, used) =>
    ok := (← assert "player useSpecialLine 31 used" used) && ok
    ok := (← assert "player useSpecialLine 31 fields" (openDoorOk gsP gs1)) && ok
    ok := (← assert "player useSpecialLine 31 THF_VERTICALDOOR" (hasDoorThinker gs1)) && ok

  match evVerticalDoor gsP 0 0 with
  | Except.error e =>
    ok := (← assert s!"EV_VerticalDoor 31 ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "EV_VerticalDoor 31 fields" (openDoorOk gsP gs1)) && ok

  let gsMon := idleGs (-1) 31
  match useSpecialLine gsMon 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster useSpecialLine 31 ({e})" false) && ok
  | Except.ok (gs1, used) =>
    ok := (← assert "monster useSpecialLine 31 false" (!used)) && ok
    ok := (← assert "monster useSpecialLine 31 identity"
      (gs1.thinkers.size == gsMon.thinkers.size
        && gs1.rng.rndindex == gsMon.rng.rndindex
        && gs1.verticalDoors.size == 0)) && ok

  let gsBusy := busyDoorGs 0 (-1) 31
  match evVerticalDoor gsBusy 0 0 with
  | Except.error e =>
    ok := (← assert s!"busy sector 31 fallthrough ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "busy sector 31 spawns new door not reopen"
      (gs1.verticalDoors.size == gsBusy.verticalDoors.size + 1
        && match gs1.verticalDoors[0]? with
           | some d => d.direction == (-1 : Int32)
           | none => false)) && ok
    ok := (← assert "busy sector 31 clears line special"
      (match gs1.level.lines[0]? with
       | some ld => ld.special == 0
       | none => false)) && ok

  pure ok

end Doom.Playsim.SpecPTest
