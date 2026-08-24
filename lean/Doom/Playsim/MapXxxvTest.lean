import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Level
import Doom.Playsim.Map
import Doom.Playsim.Mobj

/-!
P2c-xxxv unit checks: `PIT_ChangeSector` corpse gibs (`S_GIBS` 895).
Height-clip hit returns immediately. Miss + `health<=0` applies
`states[895]`, always `flags &= ~MF_SOLID`, `height=0`, `radius=0`,
keep-checking, does not set `nofit`, does not clear `MF_SHOOTABLE`.
Dropped-item remove and crush-spray stay loud-error.
Kept out of `EnemyTest.lean` so that file stays under 1k.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Level
open Doom.Playsim.Map
open Doom.Playsim.Mobj

namespace Doom.Playsim.MapXxxvTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

/-- Fixture corpse flags: `MF_AMBUSH|MF_DROPOFF|MF_CORPSE|MF_COUNTKILL`. -/
private def fixtureCorpseFlags : UInt32 := (0x500420 : UInt32)

private def crushLevel : LevelData := {
  vertexes := #[]
  sectors := #[{
    floorheight := 0, ceilingheight := 8 * FRACUNIT
    floorpic := ByteArray.empty, ceilingpic := ByteArray.empty
    lightlevel := 0, special := 0, tag := 0
    lines := #[], blockbox := #[0, 0, 0, 0]
    soundorgX := 0, soundorgY := 0
  }]
  sides := #[]
  lines := #[]
  segs := #[]
  subsectors := #[{ numsegs := 0, firstseg := 0, sector := 0 }]
  nodes := #[]
  things := #[]
  blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
  reject := ByteArray.empty
}

private def crushScene (health : Int32) (flags : UInt32) (height : Int32)
    (leveltime : UInt32 := 0) : GameState :=
  let gs0 := initFromLevel crushLevel 2 #[true, false, false, false] 0
  {
    gs0 with
    leveltime
    mobjs := #[{
      Mobj.empty with
      typeId := 1
      state := 202
      tics := -1
      sprite := 1
      frame := 7
      health
      flags
      height
      radius := 20 * FRACUNIT
      x := -7018182
      y := 19958665
      z := 0
      floorz := 0
    }]
  }

private def runPit (gs : GameState) (nofit crunch : Bool) :
    Except String (GameState × Bool × Bool) :=
  match gs.mobjs[0]? with
  | none => Except.error "runPit: missing mobj"
  | some mo => pitChangeSector gs nofit crunch 0 mo

def checkP2cXxxvUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  ok := (← assert "S_GIBS=895" (S_GIBS == (895 : UInt32))) && ok
  match states[895]? with
  | none =>
    ok := (← assert "states[895] present" false) && ok
  | some st =>
    ok := (← assert "S_GIBS sprite=SPR_POL5=98" (st.sprite == (98 : UInt32))) && ok
    ok := (← assert "S_GIBS frame=0" (st.frame == (0 : UInt32))) && ok
    ok := (← assert "S_GIBS tics=-1" (st.tics == (-1 : Int32))) && ok
    ok := (← assert "S_GIBS action null" (st.action == actionNull)) && ok

  -- Height-clip hit: fit, return immediately even if health<=0. --
  match runPit (crushScene (-60) fixtureCorpseFlags (4 * FRACUNIT)) false false with
  | Except.error e =>
    ok := (← assert s!"height-clip hit ({e})" false) && ok
  | Except.ok (gs, nofit, keep) =>
    ok := (← assert "height-clip hit keep-checking" keep) && ok
    ok := (← assert "height-clip hit nofit unchanged" (!nofit)) && ok
    match gs.mobjs[0]? with
    | none => ok := (← assert "height-clip hit mobj" false) && ok
    | some mo =>
      ok := (← assert "height-clip hit state stays 202" (mo.state == 202)) && ok
      ok := (← assert "height-clip hit tics stay -1" (mo.tics == (-1 : Int32))) && ok
      ok := (← assert "height-clip hit height stays" (mo.height == 4 * FRACUNIT)) && ok
      ok := (← assert "height-clip hit radius stays" (mo.radius == 20 * FRACUNIT)) && ok
      ok := (← assert "height-clip hit flags stay"
        (mo.flags == fixtureCorpseFlags)) && ok
      ok := (← assert "height-clip hit hp stay -60" (mo.health == (-60 : Int32))) && ok

  -- Miss + corpse: S_GIBS overlay, flatten, keep-checking, no nofit. --
  let corpse0 := crushScene (-60) fixtureCorpseFlags (56 * FRACUNIT)
  match runPit corpse0 false false with
  | Except.error e =>
    ok := (← assert s!"corpse gibs ({e})" false) && ok
  | Except.ok (gs, nofit, keep) =>
    ok := (← assert "corpse gibs keep-checking" keep) && ok
    ok := (← assert "corpse gibs does not set nofit" (!nofit)) && ok
    match gs.mobjs[0]?, corpse0.mobjs[0]? with
    | some mo, some mo0 =>
      ok := (← assert "corpse gibs state 202→895" (mo.state == S_GIBS)) && ok
      ok := (← assert "corpse gibs tics stay -1" (mo.tics == (-1 : Int32))) && ok
      ok := (← assert "corpse gibs sprite POL5"
        (mo.sprite == (98 : UInt32))) && ok
      ok := (← assert "corpse gibs frame 0" (mo.frame == (0 : UInt32))) && ok
      ok := (← assert "corpse gibs flags stay 0x500420"
        (mo.flags == fixtureCorpseFlags)) && ok
      ok := (← assert "corpse gibs hp stay -60" (mo.health == (-60 : Int32))) && ok
      ok := (← assert "corpse gibs height=0" (mo.height == 0)) && ok
      ok := (← assert "corpse gibs radius=0" (mo.radius == 0)) && ok
      ok := (← assert "corpse gibs xyz unchanged"
        (mo.x == mo0.x && mo.y == mo0.y && mo.z == mo0.z)) && ok
      ok := (← assert "corpse gibs type stays 1" (mo.typeId == 1)) && ok
    | _, _ =>
      ok := (← assert "corpse gibs mobj" false) && ok

  -- nofit0=true is preserved (not cleared, not newly set). --
  match runPit (crushScene (-60) fixtureCorpseFlags (56 * FRACUNIT)) true false with
  | Except.error e =>
    ok := (← assert s!"corpse gibs nofit0 ({e})" false) && ok
  | Except.ok (_, nofit, keep) =>
    ok := (← assert "corpse gibs preserves nofit0=true" (nofit && keep)) && ok

  -- health==0 is still a corpse (`<= 0`), not the live dropped/crush path. --
  match runPit (crushScene 0 fixtureCorpseFlags (56 * FRACUNIT)) false false with
  | Except.error e =>
    ok := (← assert s!"health=0 gibs ({e})" false) && ok
  | Except.ok (gs, nofit, keep) =>
    ok := (← assert "health=0 keep-checking no nofit" (keep && !nofit)) && ok
    match gs.mobjs[0]? with
    | some mo =>
      ok := (← assert "health=0 applies S_GIBS" (mo.state == S_GIBS)) && ok
      ok := (← assert "health=0 hp stays 0" (mo.health == 0)) && ok
    | none =>
      ok := (← assert "health=0 mobj" false) && ok

  -- Always clear MF_SOLID; never clear MF_SHOOTABLE. --
  let solidShoot := MF_SOLID ||| MF_SHOOTABLE ||| MF_CORPSE
  match runPit (crushScene (-1) solidShoot (56 * FRACUNIT)) false false with
  | Except.error e =>
    ok := (← assert s!"SOLID+SHOOTABLE gibs ({e})" false) && ok
  | Except.ok (gs, nofit, keep) =>
    ok := (← assert "SOLID+SHOOTABLE keep-checking no nofit" (keep && !nofit)) && ok
    match gs.mobjs[0]? with
    | none => ok := (← assert "SOLID+SHOOTABLE mobj" false) && ok
    | some mo =>
      ok := (← assert "gibs clears MF_SOLID"
        ((mo.flags &&& MF_SOLID) == 0)) && ok
      ok := (← assert "gibs keeps MF_SHOOTABLE"
        ((mo.flags &&& MF_SHOOTABLE) != 0)) && ok
      ok := (← assert "gibs state S_GIBS" (mo.state == S_GIBS)) && ok

  -- Dead + MF_DROPPED still gibs (health check before dropped remove). --
  match runPit (crushScene (-60) (fixtureCorpseFlags ||| MF_DROPPED)
      (56 * FRACUNIT)) false false with
  | Except.error e =>
    ok := (← assert s!"dead dropped should gib not remove ({e})" false) && ok
  | Except.ok (gs, nofit, keep) =>
    ok := (← assert "dead dropped gibs keep-checking" (keep && !nofit)) && ok
    match gs.mobjs[0]? with
    | some mo =>
      ok := (← assert "dead dropped applies S_GIBS" (mo.state == S_GIBS)) && ok
    | none =>
      ok := (← assert "dead dropped mobj" false) && ok

  -- Live dropped item still loud-errors. --
  match runPit (crushScene 1 MF_DROPPED (56 * FRACUNIT)) false false with
  | Except.error e =>
    ok := (← assert "dropped item remove loud-error"
      (e.contains "PIT_ChangeSector" && e.contains "dropped item")) && ok
  | Except.ok _ =>
    ok := (← assert "dropped item should throw" false) && ok

  -- Crush spray still loud-errors. --
  match runPit (crushScene 1 MF_SHOOTABLE (56 * FRACUNIT) 0) false true with
  | Except.error e =>
    ok := (← assert "crush spray loud-error"
      (e.contains "PIT_ChangeSector" && e.contains "crush spray")) && ok
  | Except.ok _ =>
    ok := (← assert "crush spray should throw" false) && ok

  -- Non-shootable live miss: keep-checking, no nofit. --
  match runPit (crushScene 1 (0 : UInt32) (56 * FRACUNIT)) false false with
  | Except.error e =>
    ok := (← assert s!"non-shootable miss ({e})" false) && ok
  | Except.ok (gs, nofit, keep) =>
    ok := (← assert "non-shootable keep-checking no nofit" (keep && !nofit)) && ok
    match gs.mobjs[0]? with
    | some mo =>
      ok := (← assert "non-shootable state unchanged" (mo.state == 202)) && ok
    | none =>
      ok := (← assert "non-shootable mobj" false) && ok

  -- Shootable live miss, no crunch: set nofit, keep-checking. --
  match runPit (crushScene 1 MF_SHOOTABLE (56 * FRACUNIT) 1) false false with
  | Except.error e =>
    ok := (← assert s!"shootable no-crunch ({e})" false) && ok
  | Except.ok (_, nofit, keep) =>
    ok := (← assert "shootable no-crunch sets nofit" (nofit && keep)) && ok

  -- Shootable + crunch but leveltime&3 != 0: set nofit, no spray. --
  match runPit (crushScene 1 MF_SHOOTABLE (56 * FRACUNIT) 1) false true with
  | Except.error e =>
    ok := (← assert s!"shootable crunch gated ({e})" false) && ok
  | Except.ok (_, nofit, keep) =>
    ok := (← assert "shootable crunch gated sets nofit" (nofit && keep)) && ok

  pure ok

end Doom.Playsim.MapXxxvTest
