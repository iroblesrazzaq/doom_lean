import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-y unit checks: vanilla `P_UseSpecialLine` special 90 player no-op
(C fallthrough `return true`). Player use returns `(gs0, true)` identity;
special stays 90; no `EV_DoDoor`, no switch, no rng/sfx. Monster use stays
`(gs0, false)` via whitelist. Walk spec 90 unchanged (`P2c-xxx` / SpecXxxTest).
Kept out of `EnemyTest.lean` so that file stays under 1k.
-/

open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecYTest

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

private def identityUse (before after : GameState) (special : Int32) : Bool :=
  after.thinkers.size == before.thinkers.size
    && after.plats.size == before.plats.size
    && after.verticalDoors.size == before.verticalDoors.size
    && after.rng.rndindex == before.rng.rndindex
    && (match after.level.lines[0]? with
        | some ld => ld.special == special
        | none => false)

def checkP2cYUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gs90 := mkGs 0 90
  match useSpecialLine gs90 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player useSpecialLine 90 ({e})" false) && ok
  | Except.ok (gs1, used) =>
    ok := (← assert "player useSpecialLine 90 used true" used) && ok
    ok := (← assert "player useSpecialLine 90 identity"
      (identityUse gs90 gs1 90)) && ok

  let gsMon := mkGs (-1) 90
  match useSpecialLine gsMon 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster useSpecialLine 90 ({e})" false) && ok
  | Except.ok (gs1, used) =>
    ok := (← assert "monster useSpecialLine 90 false" (!used)) && ok
    ok := (← assert "monster useSpecialLine 90 identity"
      (identityUse gsMon gs1 90)) && ok

  let gs5 := mkGs 0 5
  match useSpecialLine gs5 0 0 0 with
  | Except.error e =>
    ok := (← assert "unhandled use spec 5 loud-error"
      (e.contains "P_UseSpecialLine")) && ok
  | Except.ok _ =>
    ok := (← assert "unhandled use spec 5 should loud-error" false) && ok

  pure ok

end Doom.Playsim.SpecYTest
