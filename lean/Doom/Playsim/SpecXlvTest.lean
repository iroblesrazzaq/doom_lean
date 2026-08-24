import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xlv unit checks: `P_CrossSpecialLine` special 2 W1 → `EV_DoDoor(vld_open)`
+ clear `line.special`; `T_VerticalDoor` UP pastdest removes thinker (no wait).
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXlvTest

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

private def doorTagGs (player : Int32) (special : Int32 := 2) (tag : Int32 := 4)
    (neighborCeil : Int32 := 20 * FRACUNIT) : GameState :=
  let gs0 := initFromLevel
    (emptyLevel
      #[dummySec 0 (10 * FRACUNIT) tag #[0], dummySec 0 neighborCeil 0 #[]]
      #[dummySide 0, dummySide 1]
      #[mkLine special 0 1 tag])
    2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player }] }

private def openDoorFieldsOk (gs : GameState) (topheight : Int32) : Bool :=
  match gs.verticalDoors[0]?, gs.sectors[0]? with
  | some door, some sec =>
    door.type_ == vld_open
      && door.direction == 1
      && door.speed == VDOORSPEED
      && door.topwait == VDOORWAIT
      && door.topheight == topheight
      && sec.specialdata == 0
  | _, _ => false

private def hasDoorThinker (gs : GameState) : Bool :=
  Id.run do
    let mut found := false
    for th in gs.thinkers do
      if th.func == THF_VERTICALDOOR && th.payload == 0 then
        found := true
    pure found

def checkP2cXlvUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsP := doorTagGs 0
  match evDoDoor gsP 0 vld_open with
  | Except.error e =>
    ok := (← assert s!"EV_DoDoor vld_open ({e})" false) && ok
  | Except.ok (gsD, doorOk) =>
    ok := (← assert "EV_DoDoor vld_open rtn" doorOk) && ok
    let wantTop := 20 * FRACUNIT - 4 * FRACUNIT
    ok := (← assert "EV_DoDoor vld_open one door" (gsD.verticalDoors.size == 1)) && ok
    ok := (← assert "EV_DoDoor vld_open fields" (openDoorFieldsOk gsD wantTop)) && ok
    ok := (← assert "EV_DoDoor vld_open THF_VERTICALDOOR" (hasDoorThinker gsD)) && ok
    ok := (← assert "EV_DoDoor vld_open sfx_doropn rnd+1"
      (gsD.rng.rndindex == gsP.rng.rndindex + 1)) && ok
    match Spec.crossSpecialLine gsP 0 0 0 with
    | Except.error e =>
      ok := (← assert s!"player walk spec 2 W1 ({e})" false) && ok
    | Except.ok gsW =>
      ok := (← assert "player walk spec 2 W1 special=0 door spawned"
        (match gsW.level.lines[0]? with
         | some ld => ld.special == 0 && gsW.verticalDoors.size == 1
         | none => false)) && ok
      ok := (← assert "player walk spec 2 W1 door fields"
        (openDoorFieldsOk gsW wantTop)) && ok
    let gsNearDest :=
      match gsD.sectors[0]?, gsD.verticalDoors[0]? with
      | some sec, some door =>
        {
          gsD with
          sectors := GameState.arrSet gsD.sectors 0
            { sec with ceilingheight := door.topheight - 1 }
        }
      | _, _ => gsD
    let rndBeforePast := gsNearDest.rng.rndindex
    match verticalDoorThinker gsNearDest 0 with
    | Except.error e =>
      ok := (← assert s!"T_VerticalDoor vld_open UP pastdest ({e})" false) && ok
    | Except.ok gsPast =>
      match gsPast.sectors[0]? with
      | some sec =>
        ok := (← assert "T_VerticalDoor vld_open UP pastdest specialdata=-1"
          (sec.specialdata == -1)) && ok
      | none =>
        ok := (← assert "T_VerticalDoor vld_open UP pastdest sector" false) && ok
      let mut foundRemoved := false
      for th in gsPast.thinkers do
        if th.payload == 0 && th.func == THF_REMOVED then
          foundRemoved := true
      ok := (← assert "T_VerticalDoor vld_open UP pastdest THF_REMOVED" foundRemoved) && ok
      ok := (← assert "T_VerticalDoor vld_open UP pastdest no wait rnd"
        (gsPast.rng.rndindex == rndBeforePast)) && ok

  let gsTwo := initFromLevel
    (emptyLevel
      #[
        dummySec 0 (10 * FRACUNIT) 4 #[0],
        dummySec 0 (10 * FRACUNIT) 4 #[1],
        dummySec 0 (20 * FRACUNIT) 0 #[]
      ]
      #[dummySide 0, dummySide 2, dummySide 1]
      #[
        mkLine 2 0 2 4,
        { mkLine 0 1 2 0 with sidenum0 := 2, sidenum1 := 1 }
      ])
    2 #[true, false, false, false] 0
  let gsTwo := { gsTwo with mobjs := #[{ Mobj.empty with player := 0 }] }
  match Spec.crossSpecialLine gsTwo 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player walk spec 2 two tagged ({e})" false) && ok
  | Except.ok gsT =>
    ok := (← assert "player walk spec 2 two tagged doors"
      (gsT.verticalDoors.size == 2)) && ok
    ok := (← assert "player walk spec 2 two tagged sfx twice"
      (gsT.rng.rndindex == gsTwo.rng.rndindex + 2)) && ok
    ok := (← assert "player walk spec 2 W1 clears special"
      (match gsT.level.lines[0]? with
       | some ld => ld.special == 0
       | none => false)) && ok

  pure ok

end Doom.Playsim.SpecXlvTest
