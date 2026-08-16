import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-s unit checks: `P_UseSpecialLine` special 20 → `EV_DoPlat(raiseToNearestAndChange)`
+ `P_ChangeSwitchTexture(useAgain=0)`; `EV_DoPlat` Bool rtn; busy skip no-op.
Monster → `(gs0, false)`.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecSTest

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

private def dummySec (floorheight tag : Int32) (lines : Array UInt32)
    (floorpic : ByteArray := ByteArray.empty) : Sector := {
  floorheight, ceilingheight := 10 * FRACUNIT
  floorpic, ceilingpic := ByteArray.empty
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

private def dummySide (sec : UInt32) (top mid bot : ByteArray) : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := top, bottomtexture := bot, midtexture := mid, sector := sec
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

private def platTagGs (player : Int32) (special : Int32 := 20) (tag : Int32 := 5)
    (top : ByteArray := texName "SW1BRCOM") : GameState :=
  let frontPic := ByteArray.mk #[70, 76, 79, 79, 82, 49, 0, 0]
  let platLine : Line := {
    twoSided 0 1 with special, tag, sidenum0 := 0
  }
  let gs0 := initFromLevel
    (emptyLevel
      #[
        dummySec 0 0 #[] frontPic,
        dummySec 0 tag #[0]
      ]
      #[dummySide 0 top ByteArray.empty ByteArray.empty]
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

def checkP2cSUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsP := platTagGs 0
  match evDoPlat gsP 0 platRaiseToNearestAndChange 0 with
  | Except.error e =>
    ok := (← assert s!"EV_DoPlat raiseToNearestAndChange ({e})" false) && ok
  | Except.ok (gsD, platOk) =>
    ok := (← assert "EV_DoPlat raiseToNearestAndChange rtn true" platOk) && ok
    ok := (← assert "EV_DoPlat raiseToNearestAndChange one plat" (gsD.plats.size == 1)) && ok
    ok := (← assert "EV_DoPlat raiseToNearestAndChange THF_PLATRAISE" (hasPlatThinker gsD)) && ok

  let gsP := platTagGs 0
  match useSpecialLine gsP 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"useSpecialLine 20 ({e})" false) && ok
  | Except.ok (gsU, used) =>
    ok := (← assert "useSpecialLine 20 used" used) && ok
    ok := (← assert "useSpecialLine 20 special=0 plat spawned"
      (match gsU.level.lines[0]? with
       | some ld => ld.special == 0 && gsU.plats.size == 1
       | none => false)) && ok
    ok := (← assert "useSpecialLine 20 switch flipped"
      (match gsU.level.sides[0]? with
       | some sd => sd.toptexture == texName "SW2BRCOM"
       | none => false)) && ok
    ok := (← assert "useSpecialLine 20 stnmov+swtchn rnd+2"
      (gsU.rng.rndindex == gsP.rng.rndindex + 2)) && ok

  let gsBusy :=
    match (platTagGs 0).sectors[1]? with
    | none => platTagGs 0
    | some sec =>
      let gs0 := platTagGs 0
      { gs0 with sectors := GameState.arrSet gs0.sectors 1 { sec with specialdata := 0 } }
  match useSpecialLine gsBusy 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"useSpecialLine 20 busy ({e})" false) && ok
  | Except.ok (gsB, used) =>
    ok := (← assert "useSpecialLine 20 busy not used" (!used)) && ok
    ok := (← assert "useSpecialLine 20 busy keeps special"
      (match gsB.level.lines[0]? with | some ld => ld.special == 20 | none => false)) && ok
    ok := (← assert "useSpecialLine 20 busy switch unchanged"
      (match gsB.level.sides[0]? with
       | some sd => sd.toptexture == texName "SW1BRCOM"
       | none => false)) && ok
    ok := (← assert "useSpecialLine 20 busy no plat"
      (gsB.plats.size == 0)) && ok

  let gsMon := platTagGs (-1)
  match useSpecialLine gsMon 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster useSpecialLine 20 ({e})" false) && ok
  | Except.ok (gsM, used) =>
    ok := (← assert "monster useSpecialLine 20 not used" (!used)) && ok
    ok := (← assert "monster useSpecialLine 20 identity"
      (gsM.plats.size == 0 && gsM.rng.rndindex == gsMon.rng.rndindex)) && ok

  pure ok

end Doom.Playsim.SpecSTest
