import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Spawn
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xxxvii unit checks: vanilla 1.9 `P_CrossSpecialLine` missile early-out.
Non-player `MT_BRUISERSHOT`/`MT_TROOPSHOT`/`MT_HEADSHOT`/`MT_ROCKET`/
`MT_PLASMA`/`MT_BFG` return identity before monster-ok and before the special
switch. Type-id list, not `MF_MISSILE`. Player with those types still hits
the special switch. Kept out of `EnemyTest.lean` so that file stays under 1k.
-/

open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Spawn
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXxxviiTest

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

private def mkGs (player typeId special : Int32) (flags : UInt32 := 0) : GameState :=
  let gs0 := initFromLevel (planeLevel special) 2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player, typeId, flags }] }

private def identityOk (before after : GameState) (special : Int32) : Bool :=
  after.thinkers.size == before.thinkers.size
    && after.plats.size == before.plats.size
    && after.verticalDoors.size == before.verticalDoors.size
    && after.rng.rndindex == before.rng.rndindex
    && (match after.level.lines[0]? with
        | some ld => ld.special == special
        | none => false)

private def missileTypes : Array Int32 :=
  #[MT_BRUISERSHOT, MT_TROOPSHOT, MT_HEADSHOT, MT_ROCKET, MT_PLASMA, MT_BFG]

def checkP2cXxxviiUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  ok := (← assert "MT_* ordinals 16/31/32/33/34/35"
    (MT_BRUISERSHOT == (16 : Int32)
      && MT_TROOPSHOT == (31 : Int32)
      && MT_HEADSHOT == (32 : Int32)
      && MT_ROCKET == (33 : Int32)
      && MT_PLASMA == (34 : Int32)
      && MT_BFG == (35 : Int32))) && ok

  let mut ti : Nat := 0
  while ti < missileTypes.size do
    match missileTypes[ti]? with
    | none => pure ()
    | some ty =>
      let gs88 := mkGs (-1) ty 88
      match Spec.crossSpecialLine gs88 0 0 0 with
      | Except.error e =>
        ok := (← assert s!"missile type {ty} spec 88 identity ({e})" false) && ok
      | Except.ok gs1 =>
        ok := (← assert s!"missile type {ty} spec 88 identity before monster-ok"
          (identityOk gs88 gs1 88)) && ok
      let gs22 := mkGs (-1) ty 22
      match Spec.crossSpecialLine gs22 0 0 0 with
      | Except.error e =>
        ok := (← assert s!"missile type {ty} spec 22 identity ({e})" false) && ok
      | Except.ok gs1 =>
        ok := (← assert s!"missile type {ty} spec 22 no plat"
          (identityOk gs22 gs1 22 && gs1.plats.size == 0)) && ok
      let gs5 := mkGs (-1) ty 5
      match Spec.crossSpecialLine gs5 0 0 0 with
      | Except.error e =>
        ok := (← assert s!"missile type {ty} spec 5 identity ({e})" false) && ok
      | Except.ok gs1 =>
        ok := (← assert s!"missile type {ty} spec 5 identity before switch"
          (identityOk gs5 gs1 5)) && ok
      let gs27 := mkGs (-1) ty 27
      match Spec.crossSpecialLine gs27 0 0 0 with
      | Except.error e =>
        ok := (← assert s!"missile type {ty} spec 27 identity ({e})" false) && ok
      | Except.ok gs1 =>
        ok := (← assert s!"missile type {ty} spec 27 identity not walk no-op special-case"
          (identityOk gs27 gs1 27)) && ok
    ti := ti + 1

  let gsBare := mkGs (-1) MT_ROCKET 88 0
  match Spec.crossSpecialLine gsBare 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"rocket no MF_MISSILE still identity ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "rocket flags=0 still identity (not MF_MISSILE)"
      (identityOk gsBare gs1 88)) && ok

  let gsFlag := mkGs (-1) 0 88 MF_MISSILE
  match Spec.crossSpecialLine gsFlag 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"non-missile MF_MISSILE spec 88 ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "non-missile MF_MISSILE spec 88 activates plat"
      (gs1.plats.size == 1
        && (match gs1.level.lines[0]? with | some ld => ld.special == 88 | none => false))) && ok

  let gsMon := mkGs (-1) 0 88
  match Spec.crossSpecialLine gsMon 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster type 0 spec 88 ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "monster type 0 spec 88 activates plat"
      (gs1.plats.size == 1
        && (match gs1.level.lines[0]? with | some ld => ld.special == 88 | none => false))) && ok

  let gs15 := mkGs (-1) 15 88
  match Spec.crossSpecialLine gs15 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"MT_BRUISER 15 spec 88 ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "MT_BRUISER 15 spec 88 activates plat"
      (gs1.plats.size == 1
        && (match gs1.level.lines[0]? with | some ld => ld.special == 88 | none => false))) && ok

  let gs36 := mkGs (-1) 36 88
  match Spec.crossSpecialLine gs36 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"MT_ARACHPLAZ 36 spec 88 ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "MT_ARACHPLAZ 36 spec 88 activates plat"
      (gs1.plats.size == 1
        && (match gs1.level.lines[0]? with | some ld => ld.special == 88 | none => false))) && ok

  let gsP5 := mkGs 0 MT_ROCKET 5
  match Spec.crossSpecialLine gsP5 0 0 0 with
  | Except.error e =>
    ok := (← assert "player rocket spec 5 still loud-error"
      (e.contains "P_CrossSpecialLine")) && ok
  | Except.ok _ =>
    ok := (← assert "player rocket spec 5 should loud-error" false) && ok

  pure ok

end Doom.Playsim.SpecXxxviiTest
