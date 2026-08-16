import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.PlayerThink
import Doom.Playsim.Weapons

/-!
P2c-xxiv unit checks: `P_PlayerInSpecialSector` nukage (`special=7`) via
`P_DamageMobj(..., 5)`. Hellslime (`special=5`) still loud-errors on the
same ironfeet+beat gate. Kept out of `EnemyTest.lean` so that file stays
under 1k lines.
-/

open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.PlayerThink
open Doom.Playsim.Weapons

namespace Doom.Playsim.PlayerThinkXxivTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def floorLevel : LevelData := {
  vertexes := #[]
  sectors := #[{
    floorheight := 0, ceilingheight := 10 * FRACUNIT
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

/-- Player on/off floor with optional ironfeet and `leveltime`. -/
private def playerScene (leveltime : UInt32) (ironfeet z : Int32) : GameState :=
  let gs0 := initFromLevel floorLevel 2 #[true, false, false, false] 0
  let mo := {
    Mobj.empty with
    typeId := 0, player := 0, health := 90, z
    flags := MF_SOLID ||| MF_SHOOTABLE
    height := 56 * FRACUNIT, radius := 16 * FRACUNIT
    subsector := 0
  }
  let powers := GameState.arrSet (Array.replicate NUMPOWERS (0 : Int32)) 3 ironfeet
  let player := {
    Player.empty with
    mo := 0, playerstate := PST_LIVE
    health := 90, armorpoints := 85, armortype := 1
    pendingweapon := wp_nochange, readyweapon := wp_pistol
    powers
  }
  {
    gs0 with
    leveltime
    mobjs := #[mo]
    players := GameState.arrSet gs0.players 0 player
  }

private def runSpecial (gs : GameState) (special : Int32) : Except String GameState :=
  match gs.sectors[0]? with
  | none => throw "test: missing sector"
  | some sec0 => playerInSpecialSector gs 0 0 { sec0 with special }

private def pws (inv str invs iron all inf : Int32) : Array Int32 :=
  let a := Array.replicate NUMPOWERS (0 : Int32)
  let a := GameState.arrSet a pw_invulnerability inv
  let a := GameState.arrSet a pw_strength str
  let a := GameState.arrSet a pw_invisibility invs
  let a := GameState.arrSet a pw_ironfeet iron
  let a := GameState.arrSet a pw_allmap all
  GameState.arrSet a pw_infrared inf

private def powerAt (p : Player) (idx : Nat) : Int32 :=
  match p.powers[idx]? with | some v => v | none => (0 : Int32)

/-- Live `P_PlayerThink` with preset powers; no special sector. -/
private def liveThinkScene (powers : Array Int32) (moFlags : UInt32) : GameState :=
  let gs0 := playerScene 0 0 0
  match gs0.mobjs[0]?, gs0.players[0]? with
  | some mo, some p =>
    {
      gs0 with
      mobjs := #[{ mo with floorz := 0, ceilingz := 10 * FRACUNIT, flags := moFlags }]
      players := GameState.arrSet gs0.players 0
        { p with viewheight := VIEWHEIGHT, usedown := false, powers }
    }
  | _, _ => gs0

def checkP2cXxivUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  -- special=7 beat, on floor, no ironfeet → 5 dmg (armor class 1). --
  match runSpecial (playerScene 0 0 0) 7 with
  | Except.error e =>
    ok := (← assert s!"nukage beat ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]?, gs.mobjs[0]? with
    | some p, some mo =>
      ok := (← assert "nukage hp 90→86" (p.health == 86)) && ok
      ok := (← assert "nukage armor 85→84" (p.armorpoints == 84)) && ok
      ok := (← assert "nukage keeps armortype 1" (p.armortype == 1)) && ok
      ok := (← assert "nukage mobj hp 86" (mo.health == 86)) && ok
      ok := (← assert "nukage S_PLAY_PAIN" (mo.state == S_PLAY_PAIN)) && ok
      ok := (← assert "nukage does not decrement ironfeet"
        (match p.powers[3]? with | some v => v == 0 | none => false)) && ok
    | _, _ => ok := (← assert "nukage player/mobj" false) && ok

  -- Off-beat: no-op, no P_Random. --
  let gsOff := playerScene 1 0 0
  match runSpecial gsOff 7 with
  | Except.error e =>
    ok := (← assert s!"nukage off-beat ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]?, gs.mobjs[0]? with
    | some p, some mo =>
      ok := (← assert "nukage off-beat hp stays 90" (p.health == 90 && mo.health == 90)) && ok
      ok := (← assert "nukage off-beat armor stays 85" (p.armorpoints == 85)) && ok
      ok := (← assert "nukage off-beat no P_Random"
        (gs.rng.prndindex == 0 && gs.rng.rndindex == 0)) && ok
    | _, _ => ok := (← assert "nukage off-beat player/mobj" false) && ok

  -- Ironfeet nonzero (truthy): no-op, powers not decremented. --
  match runSpecial (playerScene 0 1 0) 7 with
  | Except.error e =>
    ok := (← assert s!"nukage ironfeet ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]? with
    | some p =>
      ok := (← assert "nukage ironfeet no-op hp" (p.health == 90)) && ok
      ok := (← assert "nukage ironfeet not decremented"
        (match p.powers[3]? with | some v => v == 1 | none => false)) && ok
      ok := (← assert "nukage ironfeet no P_Random"
        (gs.rng.prndindex == 0 && gs.rng.rndindex == 0)) && ok
    | none => ok := (← assert "nukage ironfeet player" false) && ok

  -- Negative ironfeet is truthy in C (`!powers` is false). --
  match runSpecial (playerScene 0 (-1) 0) 7 with
  | Except.error e =>
    ok := (← assert s!"nukage ironfeet neg ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]? with
    | some p =>
      ok := (← assert "nukage ironfeet neg no-op" (p.health == 90)) && ok
      ok := (← assert "nukage ironfeet neg unchanged"
        (match p.powers[3]? with | some v => v == (-1 : Int32) | none => false)) && ok
    | none => ok := (← assert "nukage ironfeet neg player" false) && ok

  -- z != floorheight: early-out even on a damage beat. --
  match runSpecial (playerScene 0 0 FRACUNIT) 7 with
  | Except.error e =>
    ok := (← assert s!"nukage airborne ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]? with
    | some p =>
      ok := (← assert "nukage airborne no-op" (p.health == 90)) && ok
      ok := (← assert "nukage airborne no P_Random"
        (gs.rng.prndindex == 0 && gs.rng.rndindex == 0)) && ok
    | none => ok := (← assert "nukage airborne player" false) && ok

  -- special=5 still throws on the same ironfeet+beat gate. --
  match runSpecial (playerScene 0 0 0) 5 with
  | Except.error e =>
    ok := (← assert "hellslime beat loud-error" (e.contains "damage special=5")) && ok
  | Except.ok _ =>
    ok := (← assert "hellslime beat should loud-error" false) && ok
  match runSpecial (playerScene 1 0 0) 5 with
  | Except.error e =>
    ok := (← assert s!"hellslime off-beat ({e})" false) && ok
  | Except.ok gs =>
    ok := (← assert "hellslime off-beat no-op" (
      match gs.players[0]? with | some p => p.health == 90 | none => false)) && ok
  match runSpecial (playerScene 0 1 0) 5 with
  | Except.error e =>
    ok := (← assert s!"hellslime ironfeet beat ({e})" false) && ok
  | Except.ok gs =>
    ok := (← assert "hellslime ironfeet beat no-op" (
      match gs.players[0]? with | some p => p.health == 90 | none => false)) && ok

  -- 4/16/11 stay loud-errors (even off-beat); special=9 succeeds. --
  match runSpecial (playerScene 1 0 0) 4 with
  | Except.error e =>
    ok := (← assert "strobe special=4 loud-error" (e.contains "special=4")) && ok
  | Except.ok _ =>
    ok := (← assert "strobe special=4 should loud-error" false) && ok
  match runSpecial (playerScene 1 0 0) 16 with
  | Except.error e =>
    ok := (← assert "super slime special=16 loud-error" (e.contains "special=16")) && ok
  | Except.ok _ =>
    ok := (← assert "super slime special=16 should loud-error" false) && ok
  match runSpecial (playerScene 0 0 0) 9 with
  | Except.error e =>
    ok := (← assert s!"secret special=9 ({e})" false) && ok
  | Except.ok gs =>
    ok := (← assert "secret special=9 success"
      (match gs.players[0]? with | some p => p.secretcount == 1 | none => false)) && ok
  match runSpecial (playerScene 0 0 0) 11 with
  | Except.error e =>
    ok := (← assert "exit special=11 loud-error" (e.contains "exit")) && ok
  | Except.ok _ =>
    ok := (← assert "exit special=11 should loud-error" false) && ok

  -- playerThink usedown-false must not restore a pre-damage snapshot. --
  let gsThink0 := playerScene 0 0 0
  let gsThink :=
    match gsThink0.sectors[0]?, gsThink0.mobjs[0]?, gsThink0.players[0]? with
    | some sec, some mo, some p =>
      {
        gsThink0 with
        sectors := GameState.arrSet gsThink0.sectors 0 { sec with special := 7 }
        mobjs := #[
          {
            mo with
            floorz := 0, ceilingz := 10 * FRACUNIT, z := 0
          }
        ]
        players := GameState.arrSet gsThink0.players 0
          { p with viewheight := VIEWHEIGHT, usedown := false, bonuscount := 6 }
      }
    | _, _, _ => gsThink0
  match playerThink gsThink 0 with
  | Except.error e =>
    ok := (← assert s!"playerThink nukage ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]?, gs.mobjs[0]? with
    | some p, some mo =>
      ok := (← assert "playerThink nukage hp 90→86" (p.health == 86)) && ok
      ok := (← assert "playerThink nukage armor 85→84" (p.armorpoints == 84)) && ok
      ok := (← assert "playerThink nukage mobj hp 86" (mo.health == 86)) && ok
      ok := (← assert "playerThink usedown stays false" (!p.usedown)) && ok
      -- C decrements after special-sector damage: +=4 then --.
      ok := (← assert "playerThink live damagecount += then --"
        (p.damagecount == (3 : Int32))) && ok
      ok := (← assert "playerThink live bonuscount--"
        (p.bonuscount == (5 : Int32))) && ok
    | _, _ => ok := (← assert "playerThink nukage player/mobj" false) && ok

  -- Live power counters (`p_user.c`): strength++, timed powers--, allmap stays. --
  match playerThink (liveThinkScene (pws 0 0 0 0 0 0) (MF_SOLID ||| MF_SHOOTABLE)) 0 with
  | Except.error e =>
    ok := (← assert s!"playerThink zero powers ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]? with
    | some p =>
      ok := (← assert "playerThink zeros stay 0"
        (powerAt p pw_invulnerability == 0 && powerAt p pw_strength == 0
          && powerAt p pw_invisibility == 0 && powerAt p pw_ironfeet == 0
          && powerAt p pw_allmap == 0 && powerAt p pw_infrared == 0)) && ok
    | none => ok := (← assert "playerThink zero powers player" false) && ok

  match playerThink (liveThinkScene (pws 3 10 2 4 7 5) (MF_SOLID ||| MF_SHOOTABLE ||| MF_SHADOW)) 0 with
  | Except.error e =>
    ok := (← assert s!"playerThink power ticks ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]?, gs.mobjs[0]? with
    | some p, some mo =>
      ok := (← assert "playerThink strength++" (powerAt p pw_strength == 11)) && ok
      ok := (← assert "playerThink invuln--" (powerAt p pw_invulnerability == 2)) && ok
      ok := (← assert "playerThink invis--" (powerAt p pw_invisibility == 1)) && ok
      ok := (← assert "playerThink invis>0 keeps MF_SHADOW"
        ((mo.flags &&& MF_SHADOW) != 0)) && ok
      ok := (← assert "playerThink ironfeet--" (powerAt p pw_ironfeet == 3)) && ok
      ok := (← assert "playerThink allmap not decremented"
        (powerAt p pw_allmap == 7)) && ok
      ok := (← assert "playerThink infrared--" (powerAt p pw_infrared == 4)) && ok
    | _, _ => ok := (← assert "playerThink power ticks player/mobj" false) && ok

  match playerThink (liveThinkScene (pws 0 0 0 0 0 0) (MF_SOLID ||| MF_SHOOTABLE ||| MF_SHADOW)) 0 with
  | Except.error e =>
    ok := (← assert s!"playerThink invis 0 keeps shadow ({e})" false) && ok
  | Except.ok gs =>
    match gs.mobjs[0]? with
    | some mo =>
      ok := (← assert "playerThink invis 0 does not clear MF_SHADOW"
        ((mo.flags &&& MF_SHADOW) != 0)) && ok
    | none => ok := (← assert "playerThink invis 0 shadow mobj" false) && ok

  match playerThink (liveThinkScene (pws 0 0 1 0 0 0) (MF_SOLID ||| MF_SHOOTABLE ||| MF_SHADOW)) 0 with
  | Except.error e =>
    ok := (← assert s!"playerThink invis expire ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]?, gs.mobjs[0]? with
    | some p, some mo =>
      ok := (← assert "playerThink invis 1→0" (powerAt p pw_invisibility == 0)) && ok
      ok := (← assert "playerThink invis 0 clears MF_SHADOW"
        ((mo.flags &&& MF_SHADOW) == 0)) && ok
    | _, _ => ok := (← assert "playerThink invis expire player/mobj" false) && ok

  -- Sector function itself must not call P_Random. --
  let root ←
    (do
      let cwd ← IO.currentDir
      match cwd.components.getLast? with
      | some "lean" => pure (cwd.parent.getD cwd)
      | _ => pure cwd)
  let src ← IO.FS.readFile (root / "lean" / "Doom" / "Playsim" / "PlayerThink.lean")
  ok := (← assert "PlayerThink has no pRandom"
    (!src.contains "pRandom" && !src.contains "mRandom")) && ok

  pure ok

end Doom.Playsim.PlayerThinkXxivTest
