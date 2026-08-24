import Doom.Playsim.Combat
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Player
import Doom.Playsim.Psprite
import Doom.Playsim.Weapons

/-!
P2c-l unit checks owned by Combat: `A_Lower` `PST_DEAD` snap.
Kept out of `EnemyTest.lean` so that file stays under 1k lines.
-/

open Doom.Playsim.Combat
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Player
open Doom.Playsim.Psprite
open Doom.Playsim.Weapons

namespace Doom.Playsim.CombatLTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

/-- `A_Lower` with `PST_DEAD` snaps `sy` to `WEAPONBOTTOM` and does not raise. -/
def checkP2cLUnits (gsA0 : GameState) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  let pspHigh : Psprite := {
    state := S_PISTOLDOWN, tics := 1, sx := FRACUNIT, sy := WEAPONBOTTOM + LOWERSPEED
  }
  let pDead : Player := {
    Doom.Playsim.Player.empty with
    playerstate := PST_DEAD
    health := 0
    readyweapon := wp_pistol
    pendingweapon := wp_nochange
    psprites := setPsp (Array.replicate NUMPSPRITES Psprite.inactive) ps_weapon pspHigh
  }
  let gsDead := {
    gsA0 with
    players := Doom.Playsim.GameState.arrSet gsA0.players 0 pDead
  }
  match aLower gsDead 0 ps_weapon with
  | Except.error e =>
    ok := (← assert s!"A_Lower PST_DEAD ({e})" false) && ok
  | Except.ok gs1 =>
    match gs1.players[0]? with
    | none => ok := (← assert "A_Lower PST_DEAD player" false) && ok
    | some p =>
      let psp := getPsp p.psprites ps_weapon
      ok := (← assert "A_Lower PST_DEAD sy snap" (psp.sy == WEAPONBOTTOM)) && ok
      ok := (← assert "A_Lower PST_DEAD keeps pistol"
        (p.readyweapon == wp_pistol)) && ok
      ok := (← assert "A_Lower PST_DEAD keeps downstate"
        (psp.state == S_PISTOLDOWN)) && ok
  match dropWeapon pDead with
  | Except.error e =>
    ok := (← assert s!"dropWeapon PST_DEAD ({e})" false) && ok
  | Except.ok pDrop =>
    let psp := getPsp pDrop.psprites ps_weapon
    ok := (← assert "dropWeapon PST_DEAD sy snap" (psp.sy == WEAPONBOTTOM)) && ok
    ok := (← assert "dropWeapon PST_DEAD downstate"
      (psp.state == S_PISTOLDOWN)) && ok
  pure ok

end Doom.Playsim.CombatLTest
