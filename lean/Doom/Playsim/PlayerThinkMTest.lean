import Doom.Playsim.Angle
import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.PlayerThink
import Doom.Playsim.Weapons

/-!
P2c-m unit checks: `P_DeathThink` + `P_PlayerThink` `PST_DEAD` routing.
Kept out of `EnemyTest.lean` so that file stays under 1k lines.
-/

open Doom.Playsim.Angle
open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.PlayerThink
open Doom.Playsim.Weapons

namespace Doom.Playsim.PlayerThinkMTest

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

private def deathScene (viewheight damagecount attacker : Int32) (buttons : UInt32) : GameState :=
  let gs0 := initFromLevel floorLevel 2 #[true, false, false, false] 0
  let mo := {
    Mobj.empty with
    typeId := 0, player := 0, health := -1, z := 0
    flags := MF_SHOOTABLE
    height := 56 * FRACUNIT, radius := 16 * FRACUNIT
    subsector := 0, floorz := 0, ceilingz := 10 * FRACUNIT
    state := 158
  }
  let player := {
    Player.empty with
    mo := 0, playerstate := PST_DEAD
    health := 0, damagecount, viewheight, attacker
    cmd := { TicCmd.zero with buttons }
    pendingweapon := wp_nochange, readyweapon := wp_pistol
  }
  {
    gs0 with
    mobjs := #[mo]
    players := GameState.arrSet gs0.players 0 player
  }

def checkP2cMUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  match deathThink (deathScene VIEWHEIGHT 40 (-1 : Int32) 0) 0 with
  | Except.error e =>
    ok := (← assert s!"deathThink viewheight fall ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]? with
    | some p =>
      ok := (← assert "deathThink viewheight -= FRACUNIT"
        (p.viewheight == VIEWHEIGHT - FRACUNIT)) && ok
      ok := (← assert "deathThink deltaviewheight=0" (p.deltaviewheight == 0)) && ok
      ok := (← assert "deathThink damagecount-- no attacker"
        (p.damagecount == 39)) && ok
      ok := (← assert "deathThink no P_Random"
        (gs.rng.prndindex == 0 && gs.rng.rndindex == 0)) && ok
    | none => ok := (← assert "deathThink viewheight player" false) && ok

  match deathThink (deathScene (DEATH_VIEWHEIGHT - 1) 0 (-1 : Int32) 0) 0 with
  | Except.error e =>
    ok := (← assert s!"deathThink clamp ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]? with
    | some p =>
      ok := (← assert "deathThink clamps viewheight to 6*FRACUNIT"
        (p.viewheight == DEATH_VIEWHEIGHT)) && ok
    | none => ok := (← assert "deathThink clamp player" false) && ok

  match deathThink (deathScene VIEWHEIGHT 0 (-1 : Int32) BT_USE) 0 with
  | Except.error e =>
    ok := (← assert s!"deathThink BT_USE ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]? with
    | some p =>
      ok := (← assert "deathThink BT_USE -> PST_REBORN"
        (p.playerstate == PST_REBORN)) && ok
    | none => ok := (← assert "deathThink BT_USE player" false) && ok

  let gsAtt0 :=
    let gs := deathScene VIEWHEIGHT 50 (1 : Int32) 0
    match gs.mobjs[0]? with
    | none => gs
    | some mo0 =>
      let att := {
        Mobj.empty with
        typeId := 3001, x := 10 * FRACUNIT, y := 0, z := 0
        subsector := 0
      }
      let mo := { mo0 with angle := ANG90 }
      { gs with mobjs := #[mo, att] }
  match deathThink gsAtt0 0 with
  | Except.error e =>
    ok := (← assert s!"deathThink attacker rotate ({e})" false) && ok
  | Except.ok gs =>
    match gs.mobjs[0]?, gs.players[0]? with
    | some mo, some p =>
      ok := (← assert "deathThink attacker rotates angle"
        (mo.angle == ANG90 - ANG5)) && ok
      ok := (← assert "deathThink attacker keeps damagecount while rotating"
        (p.damagecount == 50)) && ok
    | _, _ => ok := (← assert "deathThink attacker mo/player" false) && ok

  let gsFace0 :=
    let gs := deathScene VIEWHEIGHT 50 (1 : Int32) 0
    match gs.mobjs[0]? with
    | none => gs
    | some mo0 =>
      let att := {
        Mobj.empty with
        typeId := 3001, x := 10 * FRACUNIT, y := 0, z := 0
        subsector := 0
      }
      let mo := { mo0 with angle := pointToAngle2 0 0 att.x att.y }
      { gs with mobjs := #[mo, att] }
  match deathThink gsFace0 0 with
  | Except.error e =>
    ok := (← assert s!"deathThink face killer ({e})" false) && ok
  | Except.ok gs =>
    match gs.mobjs[0]?, gs.players[0]? with
    | some mo, some p =>
      ok := (← assert "deathThink faces killer"
        (mo.angle == pointToAngle2 0 0 (10 * FRACUNIT) 0)) && ok
      ok := (← assert "deathThink damagecount-- facing killer"
        (p.damagecount == 49)) && ok
    | _, _ => ok := (← assert "deathThink face killer mo/player" false) && ok

  match playerThink (deathScene VIEWHEIGHT 10 (-1 : Int32) 0) 0 with
  | Except.error e =>
    ok := (← assert s!"playerThink PST_DEAD ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]? with
    | some p =>
      ok := (← assert "playerThink PST_DEAD routes deathThink"
        (p.playerstate == PST_DEAD && p.damagecount == 9)) && ok
    | none => ok := (← assert "playerThink PST_DEAD player" false) && ok

  pure ok

end Doom.Playsim.PlayerThinkMTest
