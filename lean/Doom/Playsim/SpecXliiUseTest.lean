import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P3a / P2c-xlii use checks: `P_UseSpecialLine` special 62 →
`EV_DoPlat(downWaitUpStay,1)` + `P_ChangeSwitchTexture(useAgain=1)`;
special stays 62. Monster → `(gs0, false)`.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXliiUseTest

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

private def dummySec (floorheight tag : Int32) (lines : Array UInt32) : Sector := {
  floorheight, ceilingheight := 10 * FRACUNIT
  floorpic := ByteArray.empty, ceilingpic := ByteArray.empty
  lightlevel := 0, special := 0, tag
  lines, blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def twoSided (front back : Int32) : Line := {
  v1 := 0, v2 := 0, flags := ML_TWOSIDED, special := 0, tag := 0
  sidenum0 := 0, sidenum1 := 0, dx := 0, dy := 0
  slopetype := 0, bbox := #[0, 0, 0, 0]
  frontsector := front, backsector := back
}

private def dummySide (sec : UInt32) (top : ByteArray) : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := top, bottomtexture := ByteArray.empty, midtexture := ByteArray.empty
  sector := sec
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

private def platTagGs (player : Int32) (special : Int32 := 62) (tag : Int32 := 5)
    (top : ByteArray := texName "SW1BRCOM") : GameState :=
  let platLine : Line := {
    twoSided 0 1 with special, tag, sidenum0 := 0
  }
  let gs0 := initFromLevel
    (emptyLevel
      #[
        dummySec 0 0 #[],
        dummySec 0 tag #[0]
      ]
      #[dummySide 0 top]
      #[platLine])
    2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player }] }

private def hasPlatThinker (gs : GameState) : Bool :=
  Id.run do
    let mut found := false
    for th in gs.thinkers do
      if th.func == THF_PLATRAISE && th.payload == 0 then
        found := true
    pure found

def checkP3aXliiUseUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsP := platTagGs 0
  match evDoPlat gsP 0 platDownWaitUpStay 1 with
  | Except.error e =>
    ok := (← assert s!"EV_DoPlat downWaitUpStay ({e})" false) && ok
  | Except.ok (gsD, platOk) =>
    ok := (← assert "EV_DoPlat downWaitUpStay rtn true" platOk) && ok
    ok := (← assert "EV_DoPlat downWaitUpStay one plat" (gsD.plats.size == 1)) && ok
    ok := (← assert "EV_DoPlat downWaitUpStay THF_PLATRAISE" (hasPlatThinker gsD)) && ok

  let gsP := platTagGs 0
  match useSpecialLine gsP 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"useSpecialLine 62 ({e})" false) && ok
  | Except.ok (gsU, used) =>
    ok := (← assert "useSpecialLine 62 used" used) && ok
    ok := (← assert "useSpecialLine 62 special=62 plat spawned"
      (match gsU.level.lines[0]? with
       | some ld => ld.special == 62 && gsU.plats.size == 1
       | none => false)) && ok
    ok := (← assert "useSpecialLine 62 switch flipped"
      (match gsU.level.sides[0]? with
       | some sd => sd.toptexture == texName "SW2BRCOM"
       | none => false)) && ok
    ok := (← assert "useSpecialLine 62 pstart+swtchn rnd+2"
      (gsU.rng.rndindex == gsP.rng.rndindex + 2)) && ok

  let gsBusy :=
    match (platTagGs 0).sectors[1]? with
    | none => platTagGs 0
    | some sec =>
      let gs0 := platTagGs 0
      { gs0 with sectors := GameState.arrSet gs0.sectors 1 { sec with specialdata := 0 } }
  match useSpecialLine gsBusy 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"useSpecialLine 62 busy ({e})" false) && ok
  | Except.ok (gsB, used) =>
    ok := (← assert "useSpecialLine 62 busy not used" (!used)) && ok
    ok := (← assert "useSpecialLine 62 busy keeps special"
      (match gsB.level.lines[0]? with | some ld => ld.special == 62 | none => false)) && ok
    ok := (← assert "useSpecialLine 62 busy switch unchanged"
      (match gsB.level.sides[0]? with
       | some sd => sd.toptexture == texName "SW1BRCOM"
       | none => false)) && ok
    ok := (← assert "useSpecialLine 62 busy no plat"
      (gsB.plats.size == 0)) && ok

  let gsMon := platTagGs (-1)
  match useSpecialLine gsMon 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster useSpecialLine 62 ({e})" false) && ok
  | Except.ok (gsM, used) =>
    ok := (← assert "monster useSpecialLine 62 not used" (!used)) && ok
    ok := (← assert "monster useSpecialLine 62 identity"
      (gsM.plats.size == 0 && gsM.rng.rndindex == gsMon.rng.rndindex)) && ok

  pure ok

end Doom.Playsim.SpecXliiUseTest
