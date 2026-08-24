import Doom.Harness.Real
import Doom.Playsim.Combat
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Player
import Doom.Playsim.Psprite
import Doom.Playsim.Spawn
import Doom.Playsim.Weapons
import Doom.Wad

/-!
P2c-xi unit checks owned by Combat: A_Light2 + A_FireShotgun.
Kept out of `EnemyTest.lean` so that file stays under 1k lines.
-/

open Doom.Playsim.Combat
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Player
open Doom.Playsim.Psprite
open Doom.Playsim.Spawn
open Doom.Playsim.Weapons
open Doom.Wad

namespace Doom.Playsim.CombatXiTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

/-- A_Light2 dispatch + A_FireShotgun 7 inaccurate pellets and ammo. -/
def checkP2cXiUnits (wad : WadDirectory) (gsA0 : GameState) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  let pspFlash : Psprite := { state := 0, tics := 0, sx := FRACUNIT, sy := WEAPONTOP }
  let pLight : Player := {
    Doom.Playsim.Player.empty with
    playerstate := PST_LIVE
    health := 100
    readyweapon := wp_shotgun
    pendingweapon := wp_nochange
    extralight := 1
    psprites := setPsp (Array.replicate NUMPSPRITES Psprite.inactive) ps_flash pspFlash
  }
  let gsLight := {
    gsA0 with
    players := Doom.Playsim.GameState.arrSet gsA0.players 0 pLight
  }
  match Doom.Playsim.Combat.setPsprite gsLight 0 ps_flash S_SGUNFLASH2 with
  | Except.error e =>
    ok := (← assert s!"A_Light2 ({e})" false) && ok
  | Except.ok gsL2 =>
    match gsL2.players[0]? with
    | none => ok := (← assert "A_Light2 player present" false) && ok
    | some p =>
      ok := (← assert "A_Light2 extralight=2" (p.extralight == 2)) && ok
      let psp := getPsp p.psprites ps_flash
      ok := (← assert "A_Light2 flash stays S_SGUNFLASH2"
        (psp.state == S_SGUNFLASH2 && psp.tics == 3)) && ok
  match Doom.Harness.Real.loadMap wad "E1M1" with
  | Except.error e =>
    ok := (← assert s!"E1M1 load shotgun ({e})" false) && ok
  | Except.ok levelSg =>
    let gsSg0 := Doom.Playsim.GameState.initFromLevel levelSg 3 #[true, false, false, false] 0
    let px := (-16 : Int32) <<< 16
    let py := (-496 : Int32) <<< 16
    match spawnMobj gsSg0 px py ONFLOORZ 0 with
    | Except.error e =>
      ok := (← assert s!"spawn shotgun player ({e})" false) && ok
    | Except.ok (gsSg1, pIdx) =>
      match gsSg1.mobjs[pIdx]? with
      | none => ok := (← assert "shotgun player mobj" false) && ok
      | some pmo =>
        let plSg : Player := {
          Doom.Playsim.Player.empty with
          mo := pIdx.toInt32
          playerstate := PST_LIVE
          health := 100
          readyweapon := wp_shotgun
          pendingweapon := wp_nochange
          weaponowned := #[1, 1, 1, 0, 0, 0, 0, 0, 0]
          ammo := #[50, 4, 0, 0]
          maxammo := defaultMaxAmmo
          psprites := Array.replicate NUMPSPRITES Psprite.inactive
        }
        let gsSg2 := {
          gsSg1 with
          mobjs := Doom.Playsim.GameState.arrSet gsSg1.mobjs pIdx
            { pmo with player := 0, typeId := 0, state := S_PLAY }
          players := Doom.Playsim.GameState.arrSet gsSg1.players 0 plSg
        }
        let prndBefore := gsSg2.rng.prndindex
        let rndBefore := gsSg2.rng.rndindex
        match fireShotgun gsSg2 0 with
        | Except.error e =>
          ok := (← assert s!"A_FireShotgun ({e})" false) && ok
        | Except.ok gsSg3 =>
          match gsSg3.players[0]?, gsSg3.mobjs[pIdx]? with
          | some p, some mo =>
            ok := (← assert "A_FireShotgun shells 4→3"
              (match p.ammo[1]? with | some v => v == 3 | none => false)) && ok
            ok := (← assert "A_FireShotgun S_PLAY_ATK2" (mo.state == S_PLAY_ATK2)) && ok
            ok := (← assert "A_FireShotgun flash S_SGUNFLASH1"
              ((getPsp p.psprites ps_flash).state == S_SGUNFLASH1)) && ok
            ok := (← assert "A_FireShotgun flash A_Light1 extralight=1"
              (p.extralight == 1)) && ok
            let drew :=
              (gsSg3.rng.prndindex.toNat + 256 - prndBefore.toNat) % 256
            ok := (← assert "A_FireShotgun 7 inaccurate pellets drew >= 21 P_Random"
              (drew >= 21)) && ok
            ok := (← assert "A_FireShotgun sfx_shotgn pitch M_Random"
              (gsSg3.rng.rndindex == rndBefore + 1)) && ok
          | _, _ =>
            ok := (← assert "A_FireShotgun player/mobj" false) && ok
  pure ok

end Doom.Playsim.CombatXiTest
