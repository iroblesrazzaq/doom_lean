import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xxv unit checks: player/monster walk of `special=1` is identity (C has no
`case 1` in `P_CrossSpecialLine`); spec 22 W1 plat unchanged; unhandled walk
(99 and 5) still loud-errors. Kept out of `EnemyTest.lean` so that file stays
under 1k.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXxvTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def emptyBlockmap : BlockMap :=
  { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }

private def dummySec (floorheight tag : Int32) (lines : Array UInt32)
    (floorpic : ByteArray := ByteArray.empty) : Sector := {
  floorheight, ceilingheight := 10 * FRACUNIT
  floorpic, ceilingpic := ByteArray.empty
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

/-- Two-sided special=1 line that would open if `EV_VerticalDoor` were called. -/
private def doorWalkGs (player : Int32) : GameState :=
  let gs0 := initFromLevel
    (emptyLevel
      #[dummySec 0 0 #[], dummySec 0 0 #[]]
      #[dummySide 0, dummySide 1]
      #[mkLine 1 0 1])
    2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player }] }

private def walkOk (gs : GameState) : Bool :=
  match gs.level.lines[0]?, gs.sectors[1]? with
  | some ld, some sec =>
    ld.special == 1
      && gs.verticalDoors.size == 0
      && gs.plats.size == 0
      && gs.thinkers.size == 0
      && gs.rng.rndindex == 0
      && sec.specialdata == (-1 : Int32)
  | _, _ => false

def checkP2cXxvUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsP := doorWalkGs 0
  match Spec.crossSpecialLine gsP 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player walk spec 1 no-op ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "player walk spec 1 identity" (walkOk gs1)) && ok

  let gsM := doorWalkGs (-1)
  match Spec.crossSpecialLine gsM 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster walk spec 1 no-op ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "monster walk spec 1 identity" (walkOk gs1)) && ok

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

  let frontPic := ByteArray.mk #[70, 76, 79, 79, 82, 49, 0, 0]
  let gs22 := initFromLevel
    (emptyLevel
      #[dummySec 0 0 #[] frontPic, dummySec 0 5 #[0], dummySec (5 * FRACUNIT) 0 #[]]
      #[dummySide 0]
      #[{ mkLine 22 1 2 5 with sidenum1 := 0 }])
    2 #[true, false, false, false] 0
  let gs22 := { gs22 with mobjs := #[{ Mobj.empty with player := 0 }] }
  match Spec.crossSpecialLine gs22 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player spec 22 W1 ({e})" false) && ok
  | Except.ok gsW1 =>
    ok := (← assert "player spec 22 W1 special=0 plat spawned"
      (match gsW1.level.lines[0]? with
       | some ld => ld.special == 0 && gsW1.plats.size == 1
       | none => false)) && ok

  pure ok

end Doom.Playsim.SpecXxvTest
