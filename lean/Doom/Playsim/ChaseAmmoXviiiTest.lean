import Doom.Harness.Real
import Doom.Playsim.Combat
import Doom.Playsim.Enemy
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Psprite
import Doom.Playsim.Spawn
import Doom.Playsim.Weapons
import Doom.Wad

/-!
P2c-xviii unit checks: `A_Chase` meleestate early-return and `P_CheckAmmo`
empty-shotgun→pistol (preference + downstate). Kept out of `EnemyTest.lean`
so that file stays under 1k lines.
-/

open Doom.Playsim.Combat
open Doom.Playsim.Enemy
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Psprite
open Doom.Playsim.Spawn
open Doom.Playsim.Weapons
open Doom.Wad

namespace Doom.Playsim.ChaseAmmoXviiiTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

/-- `MT_SERGEANT` ordinal in `mobjinfo`. -/
def MT_SERGEANT : Int32 := 12
/-- `S_SARG_ATK1`. -/
def S_SARG_ATK1 : UInt32 := 485

private def emptyLevel : LevelData := {
  vertexes := #[]
  sectors := #[]
  sides := #[]
  lines := #[]
  segs := #[]
  subsectors := #[]
  nodes := #[]
  things := #[]
  blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
  reject := ByteArray.empty
}

private def dummyGs : GameState :=
  initFromLevel emptyLevel 2 #[true, false, false, false] 0

private def shotgunPlayer (shells clip : Int32) (owned : Array Int32) : Player :=
  let psp : Psprite := { state := S_SGUN, tics := 1, sx := FRACUNIT, sy := WEAPONTOP }
  {
    Player.empty with
    playerstate := PST_LIVE
    health := 100
    readyweapon := wp_shotgun
    pendingweapon := wp_nochange
    weaponowned := owned
    ammo := #[clip, shells, 0, 0]
    maxammo := defaultMaxAmmo
    psprites := setPsp (Array.replicate NUMPSPRITES Psprite.inactive) ps_weapon psp
  }

private def gsWithPlayer (p : Player) : GameState :=
  { dummyGs with players := GameState.arrSet dummyGs.players 0 p }

def checkP2cXviiiUnits (wad : WadDirectory) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  let ownedFps : Array Int32 := #[1, 1, 1, 0, 0, 0, 0, 0, 0]

  -- P_CheckAmmo true-path: shells remain, pending unchanged ----------------
  let gsTrue := gsWithPlayer (shotgunPlayer 4 50 ownedFps)
  match checkAmmo gsTrue 0 with
  | Except.error e =>
    ok := (← assert s!"P_CheckAmmo true ({e})" false) && ok
  | Except.ok (gsT, has) =>
    ok := (← assert "P_CheckAmmo true returns true" has) && ok
    match gsT.players[0]? with
    | none => ok := (← assert "P_CheckAmmo true player" false) && ok
    | some p =>
      ok := (← assert "P_CheckAmmo true pending unchanged"
        (p.pendingweapon == wp_nochange)) && ok
      ok := (← assert "P_CheckAmmo true ready stays shotgun"
        (p.readyweapon == wp_shotgun)) && ok

  -- empty shotgun + clip → pistol, S_SGUNDOWN ------------------------------
  let gsEmpty := gsWithPlayer (shotgunPlayer 0 46 ownedFps)
  match checkAmmo gsEmpty 0 with
  | Except.error e =>
    ok := (← assert s!"P_CheckAmmo empty shotgun ({e})" false) && ok
  | Except.ok (gsE, has) =>
    ok := (← assert "P_CheckAmmo empty shotgun returns false" (!has)) && ok
    match gsE.players[0]? with
    | none => ok := (← assert "P_CheckAmmo empty player" false) && ok
    | some p =>
      ok := (← assert "P_CheckAmmo empty pending=pistol"
        (p.pendingweapon == wp_pistol)) && ok
      ok := (← assert "P_CheckAmmo empty ready stays shotgun"
        (p.readyweapon == wp_shotgun)) && ok
      let psp := getPsp p.psprites ps_weapon
      ok := (← assert "P_CheckAmmo empty psprite S_SGUNDOWN"
        (psp.state == S_SGUNDOWN)) && ok
      ok := (← assert "P_CheckAmmo empty A_Lower sy += LOWERSPEED"
        (psp.sy == WEAPONTOP + LOWERSPEED)) && ok

  -- FireWeapon false: no ATK1 ----------------------------------------------
  let moPlay := { Mobj.empty with state := S_PLAY, tics := -1, player := 0 }
  let pFire := { shotgunPlayer 0 46 ownedFps with mo := 0 }
  let gsFire := {
    dummyGs with
    players := GameState.arrSet dummyGs.players 0 pFire
    mobjs := #[moPlay]
  }
  match fireWeapon gsFire 0 with
  | Except.error e =>
    ok := (← assert s!"P_FireWeapon empty ({e})" false) && ok
  | Except.ok gsF =>
    match gsF.mobjs[0]?, gsF.players[0]? with
    | some mo, some p =>
      ok := (← assert "P_FireWeapon empty does not enter ATK1"
        (mo.state == S_PLAY && mo.state != S_PLAY_ATK1)) && ok
      ok := (← assert "P_FireWeapon empty pending=pistol"
        (p.pendingweapon == wp_pistol)) && ok
    | _, _ => ok := (← assert "P_FireWeapon empty player/mobj" false) && ok

  -- ReFire else: clear refire then CheckAmmo -------------------------------
  let pRf := {
    shotgunPlayer 0 46 ownedFps with
    refire := 3
    cmd := TicCmd.zero
  }
  let gsRf := gsWithPlayer pRf
  match reFire gsRf 0 with
  | Except.error e =>
    ok := (← assert s!"A_ReFire else empty ({e})" false) && ok
  | Except.ok gsRf1 =>
    match gsRf1.players[0]? with
    | none => ok := (← assert "A_ReFire else empty player" false) && ok
    | some p =>
      ok := (← assert "A_ReFire else empty refire=0" (p.refire == 0)) && ok
      ok := (← assert "A_ReFire else empty pending=pistol"
        (p.pendingweapon == wp_pistol)) && ok

  -- plasma owned+cells loud-error ------------------------------------------
  let ownedPl : Array Int32 := #[1, 1, 1, 0, 0, 1, 0, 0, 0]
  let pPl := {
    shotgunPlayer 0 0 ownedPl with
    ammo := #[0, 0, 10, 0]
  }
  match checkAmmo (gsWithPlayer pPl) 0 with
  | Except.error e =>
    ok := (← assert "P_CheckAmmo plasma loud-error"
      (e == "P_CheckAmmo: plasma switch not implemented")) && ok
  | Except.ok _ =>
    ok := (← assert "P_CheckAmmo plasma should loud-error" false) && ok

  -- BFG owned+cells>40 after earlier prefs miss ----------------------------
  let ownedBfg : Array Int32 := #[1, 1, 1, 0, 0, 0, 1, 0, 0]
  let pBfg := {
    shotgunPlayer 0 0 ownedBfg with
    ammo := #[0, 0, 41, 0]
  }
  match checkAmmo (gsWithPlayer pBfg) 0 with
  | Except.error e =>
    ok := (← assert "P_CheckAmmo BFG loud-error"
      (e == "P_CheckAmmo: BFG switch not implemented")) && ok
  | Except.ok _ =>
    ok := (← assert "P_CheckAmmo BFG should loud-error" false) && ok

  -- else fist --------------------------------------------------------------
  match checkAmmo (gsWithPlayer (shotgunPlayer 0 0 ownedFps)) 0 with
  | Except.error e =>
    ok := (← assert s!"P_CheckAmmo fist ({e})" false) && ok
  | Except.ok (gsFist, has) =>
    ok := (← assert "P_CheckAmmo fist returns false" (!has)) && ok
    match gsFist.players[0]? with
    | none => ok := (← assert "P_CheckAmmo fist player" false) && ok
    | some p =>
      ok := (← assert "P_CheckAmmo pending=fist" (p.pendingweapon == wp_fist)) && ok

  -- SSG when commercial && owned && shells>2 --------------------------------
  let ownedSsg : Array Int32 := #[1, 1, 0, 1, 0, 0, 0, 0, 1]
  let pSsg := {
    Player.empty with
    playerstate := PST_LIVE
    health := 100
    readyweapon := wp_chaingun
    pendingweapon := wp_nochange
    weaponowned := ownedSsg
    ammo := #[0, 3, 0, 0]
    maxammo := defaultMaxAmmo
    psprites := setPsp (Array.replicate NUMPSPRITES Psprite.inactive) ps_weapon
      { state := S_CHAIN, tics := 1, sx := FRACUNIT, sy := WEAPONTOP }
  }
  let gsSsg := { gsWithPlayer pSsg with commercial := true }
  match checkAmmo gsSsg 0 with
  | Except.error e =>
    ok := (← assert s!"P_CheckAmmo SSG ({e})" false) && ok
  | Except.ok (gsS, has) =>
    ok := (← assert "P_CheckAmmo SSG returns false" (!has)) && ok
    match gsS.players[0]? with
    | none => ok := (← assert "P_CheckAmmo SSG player" false) && ok
    | some p =>
      ok := (← assert "P_CheckAmmo pending=SSG"
        (p.pendingweapon == wp_supershotgun)) && ok

  -- chaingun+clip beats shotgun+shells -------------------------------------
  let ownedCg : Array Int32 := #[1, 1, 1, 1, 0, 0, 0, 0, 0]
  let pCg := {
    shotgunPlayer 4 10 ownedCg with
    readyweapon := wp_missile
    ammo := #[10, 4, 0, 0]
    psprites := setPsp (Array.replicate NUMPSPRITES Psprite.inactive) ps_weapon
      { state := S_MISSILE, tics := 1, sx := FRACUNIT, sy := WEAPONTOP }
  }
  match checkAmmo (gsWithPlayer pCg) 0 with
  | Except.error e =>
    ok := (← assert s!"P_CheckAmmo chaingun pref ({e})" false) && ok
  | Except.ok (gsCg, has) =>
    ok := (← assert "P_CheckAmmo chaingun pref returns false" (!has)) && ok
    match gsCg.players[0]? with
    | none => ok := (← assert "P_CheckAmmo chaingun pref player" false) && ok
    | some p =>
      ok := (← assert "P_CheckAmmo pending=chaingun not shotgun"
        (p.pendingweapon == wp_chaingun && p.pendingweapon != wp_shotgun)) && ok

  -- A_Chase meleestate: sergeant in range → S_SARG_ATK1 --------------------
  match Doom.Harness.Real.loadMap wad "E1M1" with
  | Except.error e =>
    ok := (← assert s!"E1M1 load chase melee ({e})" false) && ok
  | Except.ok level =>
    let gs0 := initFromLevel level 3 #[true, false, false, false] 0
    let cx := gs0.level.blockmap.originX + (64 : Int32) * FRACUNIT
    let cy := gs0.level.blockmap.originY + (64 : Int32) * FRACUNIT
    match spawnMobj gs0 cx cy ONFLOORZ 0 with
    | Except.error e =>
      ok := (← assert s!"spawn melee listener ({e})" false) && ok
    | Except.ok (gs1, pIdx) =>
      match gs1.mobjs[pIdx]? with
      | none => ok := (← assert "melee listener present" false) && ok
      | some pmo =>
        let pListen : Player := {
          Player.empty with
          mo := pIdx.toInt32
          playerstate := PST_LIVE
          health := 100
        }
        let gs1 := {
          gs1 with
          mobjs := GameState.arrSet gs1.mobjs pIdx
            { pmo with flags := pmo.flags ||| MF_SHOOTABLE, player := 0 }
          players := GameState.arrSet gs1.players 0 pListen
        }
        match spawnMobj gs1 cx cy ONFLOORZ MT_SERGEANT with
        | Except.error e =>
          ok := (← assert s!"spawn sergeant ({e})" false) && ok
        | Except.ok (gs2, sIdx) =>
          match gs2.mobjs[sIdx]? with
          | none => ok := (← assert "sergeant present" false) && ok
          | some sg0 =>
            let gs3 := {
              gs2 with
              mobjs := GameState.arrSet gs2.mobjs sIdx {
                sg0 with
                target := pIdx.toInt32
                movedir := DI_NODIR
                movecount := 5
                flags := sg0.flags &&& (~~~MF_JUSTATTACKED)
              }
            }
            let rnd0 := gs3.rng.rndindex
            match aChase gs3 sIdx with
            | Except.error e =>
              ok := (← assert s!"A_Chase melee ({e})" false) && ok
            | Except.ok gs4 =>
              match gs4.mobjs[sIdx]? with
              | none => ok := (← assert "A_Chase melee mobj" false) && ok
              | some sg1 =>
                ok := (← assert "A_Chase melee state=S_SARG_ATK1"
                  (sg1.state == S_SARG_ATK1)) && ok
                ok := (← assert "A_Chase melee tics=8" (sg1.tics == 8)) && ok
                ok := (← assert "A_Chase melee early-return movecount"
                  (sg1.movecount == 5)) && ok
                ok := (← assert "A_Chase melee attacksound M_Random"
                  (gs4.rng.rndindex == rnd0 + 1)) && ok
  pure ok

end Doom.Playsim.ChaseAmmoXviiiTest
