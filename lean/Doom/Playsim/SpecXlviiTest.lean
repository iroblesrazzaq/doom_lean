import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xlvii unit checks: `P_UseSpecialLine` special 103 → `EV_DoDoor(vld_open)`
+ `P_ChangeSwitchTexture(useAgain=0)`; `EV_DoDoor` Bool rtn; busy skip no-op.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXlviiTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def emptyBlockmap : BlockMap :=
  { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }

private def texName (s : String) : ByteArray :=
  Id.run do
    let mut arr := ByteArray.mk #[]
    for b in s.toUTF8 do
      if arr.size < 8 then arr := arr.push b
    while arr.size < 8 do arr := arr.push 0
    return arr

private def dummySec (floorheight ceilingheight tag : Int32) (lines : Array UInt32) :
    Sector := {
  floorheight, ceilingheight
  floorpic := ByteArray.empty, ceilingpic := ByteArray.empty
  lightlevel := 0, special := 0, tag
  lines, blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def dummySide (sec : UInt32) (top mid bot : ByteArray) : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := top, bottomtexture := bot, midtexture := mid, sector := sec
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

private def doorTagGs (player : Int32) (special : Int32 := 103) (tag : Int32 := 4)
    (neighborCeil : Int32 := 20 * FRACUNIT) (top : ByteArray := texName "SW1BRCOM") :
    GameState :=
  let gs0 := initFromLevel
    (emptyLevel
      #[dummySec 0 (10 * FRACUNIT) tag #[0], dummySec 0 neighborCeil 0 #[]]
      #[dummySide 0 top ByteArray.empty ByteArray.empty, dummySide 1 ByteArray.empty ByteArray.empty ByteArray.empty]
      #[mkLine special 0 1 tag])
    2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player }] }

private def hasDoorThinker (gs : GameState) : Bool :=
  Id.run do
    let mut found := false
    for th in gs.thinkers do
      if th.func == THF_VERTICALDOOR && th.payload == 0 then
        found := true
    pure found

def checkP2cXlviiUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsP := doorTagGs 0
  match changeSwitchTexture gsP 0 0 with
  | Except.error e =>
    ok := (← assert s!"changeSwitchTexture ({e})" false) && ok
  | Except.ok gsSw =>
    ok := (← assert "changeSwitchTexture special=0"
      (match gsSw.level.lines[0]? with | some ld => ld.special == 0 | none => false)) && ok
    ok := (← assert "changeSwitchTexture flip top"
      (match gsSw.level.sides[0]? with
       | some sd => sd.toptexture == texName "SW2BRCOM"
       | none => false)) && ok
    ok := (← assert "changeSwitchTexture sfx_swtchn rnd+1"
      (gsSw.rng.rndindex == gsP.rng.rndindex + 1)) && ok

  let gsP := doorTagGs 0
  match evDoDoor gsP 0 vld_open with
  | Except.error e =>
    ok := (← assert s!"EV_DoDoor vld_open ({e})" false) && ok
  | Except.ok (gsD, doorOk) =>
    ok := (← assert "EV_DoDoor vld_open rtn true" doorOk) && ok
    ok := (← assert "EV_DoDoor vld_open one door" (gsD.verticalDoors.size == 1)) && ok
    ok := (← assert "EV_DoDoor vld_open THF_VERTICALDOOR" (hasDoorThinker gsD)) && ok
    match useSpecialLine gsP 0 0 0 with
    | Except.error e =>
      ok := (← assert s!"useSpecialLine 103 ({e})" false) && ok
    | Except.ok (gsU, used) =>
      ok := (← assert "useSpecialLine 103 used" used) && ok
      ok := (← assert "useSpecialLine 103 special=0 door spawned"
        (match gsU.level.lines[0]? with
         | some ld => ld.special == 0 && gsU.verticalDoors.size == 1
         | none => false)) && ok
      ok := (← assert "useSpecialLine 103 switch flipped"
        (match gsU.level.sides[0]? with
         | some sd => sd.toptexture == texName "SW2BRCOM"
         | none => false)) && ok
      ok := (← assert "useSpecialLine 103 doropn+swtchn rnd+2"
        (gsU.rng.rndindex == gsP.rng.rndindex + 2)) && ok

  let gsBusy :=
    match (doorTagGs 0).sectors[0]? with
    | none => doorTagGs 0
    | some sec =>
      let gs0 := doorTagGs 0
      { gs0 with sectors := GameState.arrSet gs0.sectors 0 { sec with specialdata := 0 } }
  match useSpecialLine gsBusy 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"useSpecialLine 103 busy ({e})" false) && ok
  | Except.ok (gsB, used) =>
    ok := (← assert "useSpecialLine 103 busy not used" (!used)) && ok
    ok := (← assert "useSpecialLine 103 busy keeps special"
      (match gsB.level.lines[0]? with | some ld => ld.special == 103 | none => false)) && ok
    ok := (← assert "useSpecialLine 103 busy no door"
      (gsB.verticalDoors.size == 0)) && ok

  let gsMon := doorTagGs (-1)
  match useSpecialLine gsMon 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster useSpecialLine 103 ({e})" false) && ok
  | Except.ok (gsM, used) =>
    ok := (← assert "monster useSpecialLine 103 not used" (!used)) && ok
    ok := (← assert "monster useSpecialLine 103 identity"
      (gsM.verticalDoors.size == 0 && gsM.rng.rndindex == gsMon.rng.rndindex)) && ok

  pure ok

end Doom.Playsim.SpecXlviiTest
