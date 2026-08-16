import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-q unit checks: vanilla `P_CrossSpecialLine` special 31 W1 walk no-op.
Player walk returns identity (special stays 31, no door/plat/thinker/rng).
Monster walk already identity via `!monsterOk`. Unhandled spec 5 stays loud-error.
Use-path special 31 unchanged (P2c-p).
-/

open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecQTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def emptyBlockmap : BlockMap :=
  { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }

private def dummySec : Sector := {
  floorheight := 0, ceilingheight := 0
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
  sectors := #[dummySec]
  sides := #[dummySide]
  lines := #[mkLine special]
  segs := #[]
  subsectors := #[]
  nodes := #[]
  things := #[]
  blockmap := emptyBlockmap
  reject := ByteArray.empty
}

private def mkGs (player special : Int32) : GameState :=
  let gs0 := initFromLevel (planeLevel special) 2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player }] }

private def identityWalk (before after : GameState) (special : Int32) : Bool :=
  after.thinkers.size == before.thinkers.size
    && after.plats.size == before.plats.size
    && after.verticalDoors.size == before.verticalDoors.size
    && after.rng.rndindex == before.rng.rndindex
    && (match after.level.lines[0]? with
        | some ld => ld.special == special
        | none => false)

def checkP2cQUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gs31 := mkGs 0 31
  match Spec.crossSpecialLine gs31 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player walk spec 31 identity ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "player walk spec 31 identity no door/plat/rng"
      (identityWalk gs31 gs1 31
        && gs1.plats.size == 0
        && gs1.verticalDoors.size == 0)) && ok

  let gsMon := { gs31 with mobjs := #[{ Mobj.empty with player := -1 }] }
  match Spec.crossSpecialLine gsMon 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster walk spec 31 identity ({e})" false) && ok
  | Except.ok gsM =>
    ok := (← assert "monster walk spec 31 identity via !monsterOk"
      (identityWalk gsMon gsM 31)) && ok

  let gs5 := mkGs 0 5
  match Spec.crossSpecialLine gs5 0 0 0 with
  | Except.error e =>
    ok := (← assert "unhandled walk spec 5 loud-error"
      (e.contains "P_CrossSpecialLine")) && ok
  | Except.ok _ =>
    ok := (← assert "unhandled walk spec 5 should loud-error" false) && ok

  pure ok

end Doom.Playsim.SpecQTest
