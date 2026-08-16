import Doom.Harness.Real
import Doom.Playsim.Combat
import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Hitscan
import Doom.Playsim.Info
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Psprite
import Doom.Playsim.Spawn
import Doom.Playsim.Weapons
import Doom.Wad

/-!
P2c-xxxi unit checks: `A_GunFlash` / `A_FireMissile` / `P_SpawnPlayerMissile`.
Kept out of `EnemyTest.lean` so that file stays under 1k lines.
-/

open Doom.Playsim.Combat
open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Hitscan
open Doom.Playsim.Info
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Psprite
open Doom.Playsim.Spawn
open Doom.Playsim.Weapons
open Doom.Wad

namespace Doom.Playsim.CombatXxxiTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

/-- Subtract health only; no pain/kill side effects. -/
private def stubDamage (gs : GameState) (targetIdx : Nat) (_inf _src : Option Nat)
    (damage : Int32) : Except String GameState :=
  match gs.mobjs[targetIdx]? with
  | none => throw "stubDamage: bad target"
  | some mo =>
    pure { gs with mobjs := arrSet gs.mobjs targetIdx { mo with health := mo.health - damage } }

private def stubExplode (gs : GameState) (idx : Nat) : Except String GameState :=
  match gs.mobjs[idx]? with
  | none => throw "stubExplode: bad mobj"
  | some mo =>
    pure {
      gs with
      mobjs := arrSet gs.mobjs idx
        { mo with momx := 0, momy := 0, momz := 0, flags := mo.flags &&& (~~~MF_MISSILE) }
    }

private def rocketPlayer (pIdx : Nat) : Player := {
  Doom.Playsim.Player.empty with
  mo := pIdx.toInt32
  playerstate := PST_LIVE
  health := 100
  readyweapon := wp_missile
  pendingweapon := wp_nochange
  weaponowned := #[1, 1, 1, 0, 1, 0, 0, 0, 0]
  ammo := #[50, 4, 0, 7]
  maxammo := defaultMaxAmmo
  psprites := Array.replicate NUMPSPRITES Psprite.inactive
}

def checkP2cXxxiUnits (wad : WadDirectory) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  match Doom.Harness.Real.loadMap wad "E1M1" with
  | Except.error e =>
    ok := (← assert s!"E1M1 load rocket ({e})" false) && ok
  | Except.ok level =>
    let gs0 := initFromLevel level 3 #[true, false, false, false] 0
    let px := (-16 : Int32) <<< 16
    let py := (-496 : Int32) <<< 16
    match spawnMobj gs0 px py ONFLOORZ 0 with
    | Except.error e =>
      ok := (← assert s!"spawn rocket player ({e})" false) && ok
    | Except.ok (gs1, pIdx) =>
      match gs1.mobjs[pIdx]? with
      | none => ok := (← assert "rocket player mobj" false) && ok
      | some pmo =>
        let gs2 := {
          gs1 with
          mobjs := arrSet gs1.mobjs pIdx
            { pmo with player := 0, typeId := 0, state := S_PLAY }
          players := arrSet gs1.players 0 (rocketPlayer pIdx)
        }
        match fireWeapon gs2 0 with
        | Except.error e =>
          ok := (← assert s!"P_FireWeapon rocket ({e})" false) && ok
        | Except.ok gs3 =>
          match gs3.players[0]?, gs3.mobjs[pIdx]? with
          | some p, some mo =>
            ok := (← assert "P_FireWeapon rocket ammo unchanged"
              (match p.ammo[3]? with | some v => v == 7 | none => false)) && ok
            ok := (← assert "A_GunFlash S_PLAY_ATK2" (mo.state == S_PLAY_ATK2)) && ok
            let w := getPsp p.psprites ps_weapon
            ok := (← assert "P_FireWeapon rocket S_MISSILE1"
              (w.state == S_MISSILE1 && w.tics == 8)) && ok
            let f := getPsp p.psprites ps_flash
            ok := (← assert "A_GunFlash flash S_MISSILEFLASH1"
              (f.state == S_MISSILEFLASH1 && f.tics == 3)) && ok
            ok := (← assert "A_GunFlash A_Light1 extralight=1"
              (p.extralight == 1)) && ok
          | _, _ =>
            ok := (← assert "P_FireWeapon rocket player/mobj" false) && ok
        match fireMissile gs2 0 with
        | Except.error e =>
          ok := (← assert s!"A_FireMissile ({e})" false) && ok
        | Except.ok gsF =>
          match gsF.players[0]? with
          | none => ok := (← assert "A_FireMissile player" false) && ok
          | some p =>
            ok := (← assert "A_FireMissile ammo 7→6"
              (match p.ammo[3]? with | some v => v == 6 | none => false)) && ok
          let mut foundRocket := false
          let mut mi : Nat := 0
          while mi < gsF.mobjs.size do
            match gsF.mobjs[mi]? with
            | some mo =>
              if mo.typeId == MT_ROCKET then
                foundRocket := true
                ok := (← assert "A_FireMissile MT_ROCKET type 33"
                  (mo.typeId == 33)) && ok
                ok := (← assert "A_FireMissile rocket target=source"
                  (mo.target == pIdx.toInt32)) && ok
                ok := (← assert "A_FireMissile rocket spawnstate 114"
                  (mo.state == 114)) && ok
                ok := (← assert "A_FireMissile rocket MF_MISSILE"
                  ((mo.flags &&& MF_MISSILE) != 0)) && ok
            | none => pure ()
            mi := mi + 1
          ok := (← assert "A_FireMissile spawned MT_ROCKET" foundRocket) && ok
        match setPsprite gs2 0 ps_weapon S_MISSILE2 with
        | Except.error e =>
          ok := (← assert s!"setPsprite A_FireMissile ({e})" false) && ok
        | Except.ok gsPsp =>
          match gsPsp.players[0]? with
          | none => ok := (← assert "setPsprite A_FireMissile player" false) && ok
          | some p =>
            ok := (← assert "setPsprite A_FireMissile ammo 7→6"
              (match p.ammo[3]? with | some v => v == 6 | none => false)) && ok
          let mut foundPsp := false
          let mut pj : Nat := 0
          while pj < gsPsp.mobjs.size do
            match gsPsp.mobjs[pj]? with
            | some mo =>
              if mo.typeId == MT_ROCKET then foundPsp := true
            | none => pure ()
            pj := pj + 1
          ok := (← assert "setPsprite A_FireMissile spawned rocket" foundPsp) && ok
        let missAim (gs : GameState) (_idx : Nat) (_an : UInt32) (_range : Int32) :
            Except String (GameState × Int32 × Int32) :=
          pure (gs, (123 : Int32), (-1 : Int32))
        match spawnPlayerMissile missAim stubDamage stubExplode gs2 pIdx MT_ROCKET with
        | Except.error e =>
          ok := (← assert s!"P_SpawnPlayerMissile miss ({e})" false) && ok
        | Except.ok (gsMiss, missIdx) =>
          match gsMiss.mobjs[missIdx]?, gs2.mobjs[pIdx]? with
          | some shot, some src =>
            ok := (← assert "P_SpawnPlayerMissile all-miss restores angle"
              (shot.angle == src.angle)) && ok
            ok := (← assert "P_SpawnPlayerMissile all-miss slope=0 momz"
              (shot.momz == 0)) && ok
          | _, _ =>
            ok := (← assert "P_SpawnPlayerMissile miss mobj" false) && ok
        match spawnPlayerMissile aimLineAttack stubDamage stubExplode gs2 pIdx
            MT_ROCKET with
        | Except.error e =>
          ok := (← assert s!"P_SpawnPlayerMissile ({e})" false) && ok
        | Except.ok (gsM, shotIdx) =>
          match gsM.mobjs[shotIdx]?, gs2.mobjs[pIdx]? with
          | some shot, some src =>
            ok := (← assert "P_SpawnPlayerMissile type=MT_ROCKET"
              (shot.typeId == MT_ROCKET && shot.typeId == 33)) && ok
            ok := (← assert "P_SpawnPlayerMissile target=source"
              (shot.target == pIdx.toInt32)) && ok
            ok := (← assert "P_SpawnPlayerMissile MF_MISSILE"
              ((shot.flags &&& MF_MISSILE) != 0)) && ok
            ok := (← assert "P_SpawnPlayerMissile tics>=1" (shot.tics >= 1)) && ok
            ok := (← assert "P_SpawnPlayerMissile spawnstate 114"
              (shot.state == 114)) && ok
            -- After P_CheckMissileSpawn, z has already stepped momz>>>1; spawn
            -- height is source.z+32fu before that half-step.
            let zSpawn := src.z + 4 * 8 * FRACUNIT
            ok := (← assert "P_SpawnPlayerMissile z near source+32fu"
              (shot.z == zSpawn + (shot.momz >>> 1))) && ok
          | _, _ =>
            ok := (← assert "P_SpawnPlayerMissile mobj" false) && ok
  pure ok

end Doom.Playsim.CombatXxxiTest
