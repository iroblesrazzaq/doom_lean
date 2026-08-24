import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xxx unit checks: `EV_DoDoor(vld_normal)` + walk special=90 WR (does not
clear `line.special`). Tag-loop skip of busy `specialdata`; conditional
`sfx_doropn`; other door types loud-error. Spec 1/22 unchanged. Kept out of
`EnemyTest.lean` so that file stays under 1k.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXxxTest

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

/-- Tagged sector 0 (ceil 10fu) adjoining neighbor 1. -/
private def doorTagGs (player : Int32) (special : Int32 := 90)
    (neighborCeil : Int32 := 20 * FRACUNIT) : GameState :=
  let gs0 := initFromLevel
    (emptyLevel
      #[dummySec 0 (10 * FRACUNIT) 5 #[0], dummySec 0 neighborCeil 0 #[]]
      #[dummySide 0, dummySide 1]
      #[mkLine special 0 1 5])
    2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player }] }

private def doorFieldsOk (gs : GameState) (topheight : Int32) : Bool :=
  match gs.verticalDoors[0]?, gs.sectors[0]? with
  | some door, some sec =>
    door.type_ == vld_normal
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

def checkP2cXxxUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  match evDoDoor (doorTagGs 0) 0 1 with
  | Except.error e =>
    ok := (← assert "EV_DoDoor other type loud-error" (e.contains "type")) && ok
  | Except.ok (_, doorOk) =>
    ok := (← assert "EV_DoDoor other type should loud-error" (!doorOk)) && ok

  let gsP := doorTagGs 0
  match evDoDoor gsP 0 vld_normal with
  | Except.error e =>
    ok := (← assert s!"EV_DoDoor vld_normal ({e})" false) && ok
  | Except.ok (gsD, doorOk) =>
    ok := (← assert "EV_DoDoor vld_normal rtn" doorOk) && ok
    let wantTop := 20 * FRACUNIT - 4 * FRACUNIT
    ok := (← assert "EV_DoDoor one door" (gsD.verticalDoors.size == 1)) && ok
    ok := (← assert "EV_DoDoor fields" (doorFieldsOk gsD wantTop)) && ok
    ok := (← assert "EV_DoDoor THF_VERTICALDOOR" (hasDoorThinker gsD)) && ok
    ok := (← assert "EV_DoDoor sfx_doropn rnd+1"
      (gsD.rng.rndindex == gsP.rng.rndindex + 1)) && ok
    match Spec.crossSpecialLine gsP 0 0 0 with
    | Except.error e =>
      ok := (← assert s!"player walk spec 90 WR ({e})" false) && ok
    | Except.ok gsW =>
      ok := (← assert "player walk spec 90 leaves special=90"
        (match gsW.level.lines[0]? with
         | some ld => ld.special == 90 && gsW.verticalDoors.size == 1
         | none => false)) && ok
      ok := (← assert "player walk spec 90 door fields"
        (doorFieldsOk gsW wantTop)) && ok
      match Spec.crossSpecialLine gsW 0 0 0 with
      | Except.error e =>
        ok := (← assert s!"player walk spec 90 WR retrigger ({e})" false) && ok
      | Except.ok gsW2 =>
        ok := (← assert "WR retrigger keeps special=90 one door"
          (match gsW2.level.lines[0]? with
           | some ld =>
             ld.special == 90 && gsW2.verticalDoors.size == 1
               && gsW2.rng.rndindex == gsW.rng.rndindex
           | none => false)) && ok
    match evDoDoor gsD 0 vld_normal with
    | Except.error e =>
      ok := (← assert s!"EV_DoDoor busy skip ({e})" false) && ok
    | Except.ok (gsBusy, busyOk) =>
      ok := (← assert "EV_DoDoor busy skip rtn false" (!busyOk)) && ok
      ok := (← assert "EV_DoDoor busy skip no second door"
        (gsBusy.verticalDoors.size == 1)) && ok
      ok := (← assert "EV_DoDoor busy skip no extra sfx"
        (gsBusy.rng.rndindex == gsD.rng.rndindex)) && ok
    match verticalDoorThinker gsD 0 with
    | Except.error e =>
      ok := (← assert s!"same-tic T_VerticalDoor UP ({e})" false) && ok
    | Except.ok gsUp =>
      match gsUp.sectors[0]? with
      | none => ok := (← assert "same-tic UP sector" false) && ok
      | some sec =>
        ok := (← assert "same-tic UP ceil += VDOORSPEED"
          (sec.ceilingheight == 10 * FRACUNIT + VDOORSPEED)) && ok

  let gsSilent := doorTagGs 0 90 (14 * FRACUNIT)
  match evDoDoor gsSilent 0 vld_normal with
  | Except.error e =>
    ok := (← assert s!"EV_DoDoor silent ({e})" false) && ok
  | Except.ok (gsS, doorOk) =>
    ok := (← assert "EV_DoDoor silent rtn" doorOk) && ok
    ok := (← assert "EV_DoDoor silent still allocates" (gsS.verticalDoors.size == 1)) && ok
    ok := (← assert "EV_DoDoor silent topheight == ceiling"
      (doorFieldsOk gsS (10 * FRACUNIT))) && ok
    ok := (← assert "EV_DoDoor silent no sfx"
      (gsS.rng.rndindex == gsSilent.rng.rndindex)) && ok

  let gsTwo := initFromLevel
    (emptyLevel
      #[
        dummySec 0 (10 * FRACUNIT) 5 #[0],
        dummySec 0 (10 * FRACUNIT) 5 #[1],
        dummySec 0 (20 * FRACUNIT) 0 #[]
      ]
      #[dummySide 0, dummySide 2, dummySide 1]
      #[
        mkLine 90 0 2 5,
        { mkLine 0 1 2 0 with sidenum0 := 2, sidenum1 := 1 }
      ])
    2 #[true, false, false, false] 0
  match evDoDoor gsTwo 0 vld_normal with
  | Except.error e =>
    ok := (← assert s!"EV_DoDoor two tagged ({e})" false) && ok
  | Except.ok (gsT, doorOk) =>
    ok := (← assert "EV_DoDoor two tagged rtn" doorOk) && ok
    ok := (← assert "EV_DoDoor two tagged doors" (gsT.verticalDoors.size == 2)) && ok
    ok := (← assert "EV_DoDoor two tagged sfx twice"
      (gsT.rng.rndindex == gsTwo.rng.rndindex + 2)) && ok

  let gsMiss := doorTagGs 0
  let gsMiss :=
    { gsMiss with level := { gsMiss.level with lines := #[mkLine 90 0 1 99] } }
  match evDoDoor gsMiss 0 vld_normal with
  | Except.error e =>
    ok := (← assert s!"EV_DoDoor missing tag ({e})" false) && ok
  | Except.ok (gs0, doorOk) =>
    ok := (← assert "EV_DoDoor missing tag rtn false" (!doorOk)) && ok
    ok := (← assert "EV_DoDoor missing tag no door"
      (gs0.verticalDoors.size == 0 && gs0.rng.rndindex == gsMiss.rng.rndindex)) && ok

  let gsM := doorTagGs (-1)
  match Spec.crossSpecialLine gsM 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster walk spec 90 no-op ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "monster walk spec 90 identity"
      (match gs1.level.lines[0]?, gs1.sectors[0]? with
       | some ld, some sec =>
         ld.special == 90 && gs1.verticalDoors.size == 0
           && gs1.thinkers.size == 0 && sec.specialdata == (-1 : Int32)
       | _, _ => false)) && ok

  let gs1 := doorTagGs 0 1
  match Spec.crossSpecialLine gs1 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player walk spec 1 no-op ({e})" false) && ok
  | Except.ok gsN =>
    ok := (← assert "player walk spec 1 identity"
      (match gsN.level.lines[0]? with
       | some ld => ld.special == 1 && gsN.verticalDoors.size == 0
       | none => false)) && ok

  let frontPic := ByteArray.mk #[70, 76, 79, 79, 82, 49, 0, 0]
  let gs22 := initFromLevel
    (emptyLevel
      #[
        dummySec 0 (10 * FRACUNIT) 0 #[] ,
        dummySec 0 (10 * FRACUNIT) 5 #[0],
        dummySec (5 * FRACUNIT) (10 * FRACUNIT) 0 #[]
      ]
      #[dummySide 0]
      #[{ mkLine 22 1 2 5 with sidenum1 := 0 }])
    2 #[true, false, false, false] 0
  let gs22 := {
    gs22 with
    sectors :=
      match gs22.sectors[0]? with
      | some sec => GameState.arrSet gs22.sectors 0 { sec with floorpic := frontPic }
      | none => gs22.sectors
    mobjs := #[{ Mobj.empty with player := 0 }]
  }
  match Spec.crossSpecialLine gs22 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player spec 22 W1 ({e})" false) && ok
  | Except.ok gsW1 =>
    ok := (← assert "player spec 22 W1 special=0 plat spawned"
      (match gsW1.level.lines[0]? with
       | some ld => ld.special == 0 && gsW1.plats.size == 1
       | none => false)) && ok

  let gs99 : GameState :=
    { gsP with level := { gsP.level with lines := #[mkLine 99 0 1] } }
  match Spec.crossSpecialLine gs99 0 0 0 with
  | Except.error e =>
    ok := (← assert "unhandled walk spec 99 loud-error"
      (e.contains "P_CrossSpecialLine")) && ok
  | Except.ok _ =>
    ok := (← assert "unhandled walk spec 99 should loud-error" false) && ok
  let gs5 : GameState :=
    { gsP with level := { gsP.level with lines := #[mkLine 5 0 1] } }
  match Spec.crossSpecialLine gs5 0 0 0 with
  | Except.error e =>
    ok := (← assert "unhandled walk spec 5 loud-error"
      (e.contains "P_CrossSpecialLine")) && ok
  | Except.ok _ =>
    ok := (← assert "unhandled walk spec 5 should loud-error" false) && ok

  pure ok

end Doom.Playsim.SpecXxxTest
