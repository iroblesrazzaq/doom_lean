import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xlix unit checks: `P_CrossSpecialLine` special 91 WR → `EV_DoFloor(raiseFloor)`
(line special retained); `T_MoveFloor` UP pastdest removes thinker.
Unhandled spec 5 stays loud-error.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXlixTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def emptyBlockmap : BlockMap :=
  { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }

private def dummySec (floorheight ceilingheight tag : Int32) (lines : Array UInt32) :
    Sector := {
  floorheight, ceilingheight
  floorpic := ByteArray.empty, ceilingpic := ByteArray.empty
  lightlevel := 0, special := 0, tag
  lines, blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def dummySide (sec : UInt32) : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := ByteArray.empty, bottomtexture := ByteArray.empty
  midtexture := ByteArray.empty, sector := sec
}

private def mkLine (special : Int32) (front back : Int32) (tag : Int32 := 0) : Line := {
  v1 := 0, v2 := 0, flags := ML_TWOSIDED, special, tag
  sidenum0 := 0, sidenum1 := 1, dx := 0, dy := 0
  slopetype := 0, bbox := #[0, 0, 0, 0]
  frontsector := front, backsector := back
}

private def emptyLevel (sectors : Array Sector) (sides : Array Side) (lines : Array Line) :
    LevelData := {
  vertexes := #[]
  sectors
  sides
  lines
  segs := #[]
  subsectors := #[]
  nodes := #[]
  things := #[]
  blockmap := emptyBlockmap
  reject := ByteArray.empty
}

private def floorTagGs (player : Int32) (special : Int32 := 91) (tag : Int32 := 1)
    (neighborCeiling : Int32 := 68 * FRACUNIT) : GameState :=
  let gs0 := initFromLevel
    (emptyLevel
      #[dummySec (64 * FRACUNIT) (72 * FRACUNIT) tag #[0], dummySec (64 * FRACUNIT) neighborCeiling 0 #[]]
      #[dummySide 0, dummySide 1]
      #[mkLine special 0 1 tag])
    2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player }] }

private def raiseFloorFieldsOk (gs : GameState) (dest : Int32) : Bool :=
  match gs.floors[0]?, gs.sectors[0]? with
  | some floor, some sec =>
    floor.type_ == raiseFloor
      && floor.direction == (1 : Int32)
      && floor.speed == FLOORSPEED
      && floor.floordestheight == dest
      && !floor.crush
      && sec.specialdata == 0
  | _, _ => false

private def hasFloorThinker (gs : GameState) : Bool :=
  Id.run do
    let mut found := false
    for th in gs.thinkers do
      if th.func == THF_MOVEFLOOR && th.payload == 0 then
        found := true
    pure found

def checkP2cXlixUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsP := floorTagGs 0
  let wantDest := 68 * FRACUNIT
  match evDoFloor gsP 0 raiseFloor with
  | Except.error e =>
    ok := (← assert s!"EV_DoFloor raiseFloor ({e})" false) && ok
  | Except.ok gsD =>
    ok := (← assert "EV_DoFloor raiseFloor one floor" (gsD.floors.size == 1)) && ok
    ok := (← assert "EV_DoFloor raiseFloor fields" (raiseFloorFieldsOk gsD wantDest)) && ok
    ok := (← assert "EV_DoFloor raiseFloor THF_MOVEFLOOR" (hasFloorThinker gsD)) && ok
    match Spec.crossSpecialLine gsP 0 0 0 with
    | Except.error e =>
      ok := (← assert s!"player walk spec 91 WR ({e})" false) && ok
    | Except.ok gsW =>
      ok := (← assert "player walk spec 91 WR special retained"
        (match gsW.level.lines[0]? with
         | some ld => ld.special == 91 && gsW.floors.size == 1
         | none => false)) && ok
      ok := (← assert "player walk spec 91 WR floor fields"
        (raiseFloorFieldsOk gsW wantDest)) && ok
    let gsNearDest :=
      match gsD.sectors[0]?, gsD.floors[0]? with
      | some sec, some floor =>
        {
          gsD with
          leveltime := 1
          sectors := GameState.arrSet gsD.sectors 0
            { sec with floorheight := floor.floordestheight - 1 }
        }
      | _, _ => gsD
    let rndBeforePast := gsNearDest.rng.rndindex
    match floorMoveThinker gsNearDest 0 with
    | Except.error e =>
      ok := (← assert s!"T_MoveFloor raiseFloor UP pastdest ({e})" false) && ok
    | Except.ok gsPast =>
      match gsPast.sectors[0]? with
      | some sec =>
        ok := (← assert "T_MoveFloor raiseFloor UP pastdest specialdata=-1"
          (sec.specialdata == -1)) && ok
      | none =>
        ok := (← assert "T_MoveFloor raiseFloor UP pastdest sector" false) && ok
      let mut foundRemoved := false
      for th in gsPast.thinkers do
        if th.payload == 0 && th.func == THF_REMOVED then
          foundRemoved := true
      ok := (← assert "T_MoveFloor raiseFloor UP pastdest THF_REMOVED" foundRemoved) && ok
      ok := (← assert "T_MoveFloor raiseFloor UP pastdest sfx_pstop rnd+1"
        (gsPast.rng.rndindex == rndBeforePast + 1)) && ok

  let gsMon := { gsP with mobjs := #[{ Mobj.empty with player := -1 }] }
  match Spec.crossSpecialLine gsMon 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster walk spec 91 identity ({e})" false) && ok
  | Except.ok gsM =>
    ok := (← assert "monster walk spec 91 identity via !monsterOk"
      (gsM.floors.size == 0
        && gsM.thinkers.size == gsMon.thinkers.size
        && (match gsM.level.lines[0]? with | some ld => ld.special == 91 | none => false))) && ok

  let gs5 := floorTagGs 0 5
  match Spec.crossSpecialLine gs5 0 0 0 with
  | Except.error e =>
    ok := (← assert "unhandled walk spec 5 loud-error"
      (e.contains "P_CrossSpecialLine")) && ok
  | Except.ok _ =>
    ok := (← assert "unhandled walk spec 5 should loud-error" false) && ok

  pure ok

end Doom.Playsim.SpecXlixTest
