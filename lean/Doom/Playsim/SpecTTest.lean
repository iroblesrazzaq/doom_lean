import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Spec

/-!
P2c-t unit checks: `EV_LightTurnOn` tag scan + bright/neighbor max; `P_CrossSpecialLine`
special 35 W1 → `EV_LightTurnOn(35)` + clear line special. Monster walk identity via
`!monsterOk`. Instant, no thinkers/rng/sound.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Spec

namespace Doom.Playsim.SpecTTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def emptyBlockmap : BlockMap :=
  { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }

private def dummySec (light tag : Int32) (lines : Array UInt32 := #[]) : Sector := {
  floorheight := 0, ceilingheight := 0
  floorpic := ByteArray.empty, ceilingpic := ByteArray.empty
  lightlevel := light, special := 0, tag
  lines, blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def dummySide (sec : UInt32) : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := ByteArray.empty, bottomtexture := ByteArray.empty
  midtexture := ByteArray.empty, sector := sec
}

private def twoSided (front back : Int32) : Line := {
  v1 := 0, v2 := 0, flags := ML_TWOSIDED, special := 0, tag := 0
  sidenum0 := 0, sidenum1 := 0, dx := 0, dy := 0
  slopetype := 0, bbox := #[0, 0, 0, 0]
  frontsector := front, backsector := back
}

private def mkLine (special tag : Int32) : Line := {
  v1 := 0, v2 := 0, flags := 0, special, tag
  sidenum0 := 0, sidenum1 := 0, dx := 0, dy := 0
  slopetype := 0, bbox := #[0, 0, 0, 0]
  frontsector := 0, backsector := 0
}

private def lightLevelGs (player special tag : Int32) (taggedLight : Int32) : GameState :=
  let tagged := dummySec taggedLight tag #[1]
  let neighbor := dummySec 160 0 #[1]
  let gs0 := initFromLevel
    ({
      vertexes := #[]
      sectors := #[tagged, neighbor]
      sides := #[dummySide 0, dummySide 1]
      lines := #[
        { mkLine special tag with sidenum0 := 0 },
        twoSided 0 1
      ]
      segs := #[]
      subsectors := #[]
      nodes := #[]
      things := #[]
      blockmap := emptyBlockmap
      reject := ByteArray.empty
    } : LevelData)
    2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player }] }

private def identityWalk (before after : GameState) : Bool :=
  after.thinkers.size == before.thinkers.size
    && after.plats.size == before.plats.size
    && after.verticalDoors.size == before.verticalDoors.size
    && after.rng.rndindex == before.rng.rndindex
    && (match after.level.lines[0]? with
        | some ld => ld.special == 35
        | none => false)

def checkP2cTUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsTag := lightLevelGs 0 35 7 200
  match evLightTurnOn gsTag 0 35 with
  | Except.error e =>
    ok := (← assert s!"EV_LightTurnOn bright=35 ({e})" false) && ok
  | Except.ok gsL =>
    ok := (← assert "EV_LightTurnOn bright=35 sets tagged sector"
      (match gsL.sectors[0]? with | some sec => sec.lightlevel == 35 | none => false)) && ok
    ok := (← assert "EV_LightTurnOn bright=35 no thinkers/rng"
      (gsL.thinkers.size == gsTag.thinkers.size
        && gsL.rng.rndindex == gsTag.rng.rndindex)) && ok

  let gsMax := lightLevelGs 0 0 9 50
  match evLightTurnOn gsMax 0 0 with
  | Except.error e =>
    ok := (← assert s!"EV_LightTurnOn bright=0 ({e})" false) && ok
  | Except.ok gsM =>
    ok := (← assert "EV_LightTurnOn bright=0 max neighbor"
      (match gsM.sectors[0]? with | some sec => sec.lightlevel == 160 | none => false)) && ok

  let gs35 := lightLevelGs 0 35 7 200
  match Spec.crossSpecialLine gs35 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player walk spec 35 ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "player walk spec 35 light=35 special=0"
      (match gs1.sectors[0]?, gs1.level.lines[0]? with
       | some sec, some ld => sec.lightlevel == 35 && ld.special == 0
       | _, _ => false)) && ok
    ok := (← assert "player walk spec 35 no thinkers/rng"
      (gs1.thinkers.size == gs35.thinkers.size
        && gs1.rng.rndindex == gs35.rng.rndindex)) && ok

  let gsMon := { gs35 with mobjs := #[{ Mobj.empty with player := -1 }] }
  match Spec.crossSpecialLine gsMon 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster walk spec 35 identity ({e})" false) && ok
  | Except.ok gsM =>
    ok := (← assert "monster walk spec 35 identity via !monsterOk"
      (identityWalk gsMon gsM
        && (match gsM.sectors[0]? with | some sec => sec.lightlevel == 200 | none => false))) && ok

  let gs5 := lightLevelGs 0 5 0 0
  match Spec.crossSpecialLine gs5 0 0 0 with
  | Except.error e =>
    ok := (← assert "unhandled walk spec 5 loud-error"
      (e.contains "P_CrossSpecialLine")) && ok
  | Except.ok _ =>
    ok := (← assert "unhandled walk spec 5 should loud-error" false) && ok

  pure ok

end Doom.Playsim.SpecTTest
