import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xlvi unit checks: `P_CrossSpecialLine` special 98 WR → `EV_DoFloor(turboLower)`
(line special retained); `T_MoveFloor` DOWN pastdest removes thinker.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXlviTest

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

private def floorTagGs (player : Int32) (special : Int32 := 98) (tag : Int32 := 1)
    (neighborFloor : Int32 := 60 * FRACUNIT) : GameState :=
  let gs0 := initFromLevel
    (emptyLevel
      #[dummySec (64 * FRACUNIT) (72 * FRACUNIT) tag #[0], dummySec neighborFloor (72 * FRACUNIT) 0 #[]]
      #[dummySide 0, dummySide 1]
      #[mkLine special 0 1 tag])
    2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player }] }

private def turboLowerFieldsOk (gs : GameState) (dest : Int32) : Bool :=
  match gs.floors[0]?, gs.sectors[0]? with
  | some floor, some sec =>
    floor.type_ == turboLower
      && floor.direction == (-1 : Int32)
      && floor.speed == FLOORSPEED * 4
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

def checkP2cXlviUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsP := floorTagGs 0
  let wantDest := 60 * FRACUNIT + 8 * FRACUNIT
  match evDoFloor gsP 0 turboLower with
  | Except.error e =>
    ok := (← assert s!"EV_DoFloor turboLower ({e})" false) && ok
  | Except.ok gsD =>
    ok := (← assert "EV_DoFloor turboLower one floor" (gsD.floors.size == 1)) && ok
    ok := (← assert "EV_DoFloor turboLower fields" (turboLowerFieldsOk gsD wantDest)) && ok
    ok := (← assert "EV_DoFloor turboLower THF_MOVEFLOOR" (hasFloorThinker gsD)) && ok
    match Spec.crossSpecialLine gsP 0 0 0 with
    | Except.error e =>
      ok := (← assert s!"player walk spec 98 WR ({e})" false) && ok
    | Except.ok gsW =>
      ok := (← assert "player walk spec 98 WR special retained"
        (match gsW.level.lines[0]? with
         | some ld => ld.special == 98 && gsW.floors.size == 1
         | none => false)) && ok
      ok := (← assert "player walk spec 98 WR floor fields"
        (turboLowerFieldsOk gsW wantDest)) && ok
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
      ok := (← assert s!"T_MoveFloor turboLower DOWN pastdest ({e})" false) && ok
    | Except.ok gsPast =>
      match gsPast.sectors[0]? with
      | some sec =>
        ok := (← assert "T_MoveFloor turboLower DOWN pastdest specialdata=-1"
          (sec.specialdata == -1)) && ok
      | none =>
        ok := (← assert "T_MoveFloor turboLower DOWN pastdest sector" false) && ok
      let mut foundRemoved := false
      for th in gsPast.thinkers do
        if th.payload == 0 && th.func == THF_REMOVED then
          foundRemoved := true
      ok := (← assert "T_MoveFloor turboLower DOWN pastdest THF_REMOVED" foundRemoved) && ok
      ok := (← assert "T_MoveFloor turboLower DOWN pastdest sfx_pstop rnd+1"
        (gsPast.rng.rndindex == rndBeforePast + 1)) && ok

  let gsTwo := initFromLevel
    (emptyLevel
      #[
        dummySec (64 * FRACUNIT) (72 * FRACUNIT) 1 #[0],
        dummySec (64 * FRACUNIT) (72 * FRACUNIT) 1 #[1],
        dummySec (60 * FRACUNIT) (72 * FRACUNIT) 0 #[]
      ]
      #[dummySide 0, dummySide 2, dummySide 1]
      #[
        mkLine 98 0 2 1,
        { mkLine 0 1 2 0 with sidenum0 := 2, sidenum1 := 1 }
      ])
    2 #[true, false, false, false] 0
  let gsTwo := { gsTwo with mobjs := #[{ Mobj.empty with player := 0 }] }
  match Spec.crossSpecialLine gsTwo 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player walk spec 98 two tagged ({e})" false) && ok
  | Except.ok gsT =>
    ok := (← assert "player walk spec 98 two tagged floors"
      (gsT.floors.size == 2)) && ok
    ok := (← assert "player walk spec 98 WR special retained two tagged"
      (match gsT.level.lines[0]? with
       | some ld => ld.special == 98
       | none => false)) && ok

  pure ok

end Doom.Playsim.SpecXlviTest
