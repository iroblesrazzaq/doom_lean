import Doom.Playsim.Angle
import Doom.Playsim.Enemy
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Hitscan
import Doom.Playsim.Info
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Psprite
import Doom.Playsim.Random
import Doom.Playsim.Sound
import Doom.Playsim.Spawn
import Doom.Playsim.Tables
import Doom.Playsim.Weapons

/-!
# Doom.Playsim.Combat

Pistol fire / psprite (`p_pspr.c`). Hitscan lives in `Hitscan`; `P_DamageMobj`
lives in `Enemy`. Psprite must not import Map; GameState is threaded from
`PlayerThink`.
-/

namespace Doom.Playsim.Combat

open Doom.Playsim.Angle
open Doom.Playsim.Enemy
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Hitscan
open Doom.Playsim.Info
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Psprite
open Doom.Playsim.Random
open Doom.Playsim.Sound
open Doom.Playsim.Tables
open Doom.Playsim.Weapons

/-- `P_BulletSlope` / `P_AimLineAttack` range. -/
def AIMRANGE : Int32 := 16 * 64 * FRACUNIT

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

private def setPlayer (gs : GameState) (i : Nat) (p : Player) : GameState :=
  { gs with players := setArr gs.players i p }

private def ammoAt (p : Player) (i : Int32) : Int32 :=
  match p.ammo[i.toNatClampNeg]? with | some v => v | none => 0

private def ownedAt (p : Player) (w : Int32) : Int32 :=
  match p.weaponowned[w.toNatClampNeg]? with | some v => v | none => 0

private def setAmmo (p : Player) (i : Int32) (v : Int32) : Player :=
  { p with ammo := setArr p.ammo i.toNatClampNeg v }

/-- `P_BulletSlope`. Returns `(gs, bulletslope)`. -/
def bulletSlope (gs0 : GameState) (mobjIdx : Nat) : Except String (GameState × Int32) := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_BulletSlope: bad mobj"
  | some mo =>
    let mut an := mo.angle
    let (gs1, slope1, lt1) ← aimLineAttack gs0 mobjIdx an AIMRANGE
    if lt1 >= 0 then
      return (gs1, slope1)
    an := an + ((1 : UInt32) <<< 26)
    let (gs2, slope2, lt2) ← aimLineAttack gs1 mobjIdx an AIMRANGE
    if lt2 >= 0 then
      return (gs2, slope2)
    an := an - ((2 : UInt32) <<< 26)
    let (gs3, slope3, _) ← aimLineAttack gs2 mobjIdx an AIMRANGE
    pure (gs3, slope3)

/-- `P_GunShot`. -/
def gunShot (gs0 : GameState) (mobjIdx : Nat) (accurate : Bool) (bulletslope : Int32) :
    Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_GunShot: bad mobj"
  | some mo =>
    let (r, rng) := pRandom gs0.rng
    let damage := 5 * (r % 3 + 1)
    let mut gs := { gs0 with rng }
    let mut angle := mo.angle
    if !accurate then
      let (d, rng2) := pSubRandom gs.rng
      gs := { gs with rng := rng2 }
      angle := angle + (d.toUInt32 <<< 18)
    Hitscan.lineAttack Enemy.damageMobj gs mobjIdx angle Hitscan.MISSILERANGE bulletslope damage

/-- `A_Lower` body (shared by `P_SetPsprite` and `P_CheckAmmo` downstate). -/
def aLower (gs0 : GameState) (playerIdx : Nat) (position : Nat) :
    Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "A_Lower: bad player"
  | some pR =>
    let psp1 := getPsp pR.psprites position
    let sy := psp1.sy + LOWERSPEED
    if sy < WEAPONBOTTOM then
      pure (setPlayer gs0 playerIdx
        { pR with psprites := setPsp pR.psprites position { psp1 with sy } })
    else
      if pR.playerstate == PST_DEAD then
        pure (setPlayer gs0 playerIdx
          { pR with psprites := setPsp pR.psprites position { psp1 with sy := WEAPONBOTTOM } })
      else if pR.health == 0 then
        throw "A_Lower: health==0 not implemented"
      else
        let pReady := { pR with readyweapon := pR.pendingweapon }
        let pUp ← bringUpWeapon pReady
        pure (setPlayer gs0 playerIdx pUp)

/-- Enter `readyweapon.downstate` and run `A_Lower` (C `P_CheckAmmo` overlay). -/
def enterWeaponDown (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "P_CheckAmmo: player lost on downstate"
  | some p0 =>
    let wi ← weaponAt p0.readyweapon
    match states[wi.downstate.toNat]? with
    | none => throw s!"P_CheckAmmo: bad downstate {wi.downstate}"
    | some st =>
      if st.action != action_A_Lower then
        throw s!"P_CheckAmmo: unexpected downstate action {st.action}"
      if st.tics == 0 then
        throw "P_CheckAmmo: downstate tics=0 not implemented"
      let psp0 := getPsp p0.psprites ps_weapon
      let mut psp := { psp0 with state := wi.downstate, tics := st.tics }
      if st.misc1 != 0 then
        psp := { psp with sx := st.misc1 * FRACUNIT, sy := st.misc2 * FRACUNIT }
      let p1 := { p0 with psprites := setPsp p0.psprites ps_weapon psp }
      aLower (setPlayer gs0 playerIdx p1) playerIdx ps_weapon

/-- `P_CheckAmmo`. True-path unchanged; false-path picks pending once then lowers. -/
def checkAmmo (gs0 : GameState) (playerIdx : Nat) : Except String (GameState × Bool) := do
  match gs0.players[playerIdx]? with
  | none => throw "P_CheckAmmo: bad player"
  | some player =>
    let wi ← weaponAt player.readyweapon
    let ammo := wi.ammo
    let count : Int32 :=
      if player.readyweapon == wp_bfg then (40 : Int32)
      else if player.readyweapon == wp_supershotgun then 2
      else 1
    if ammo == am_noammo then
      return (gs0, true)
    if ammoAt player ammo >= count then
      return (gs0, true)
    let cells := ammoAt player am_cell
    let shells := ammoAt player am_shell
    let clip := ammoAt player am_clip
    let misl := ammoAt player am_misl
    let pending ←
      if ownedAt player wp_plasma != 0 && cells != 0 then
        throw "P_CheckAmmo: plasma switch not implemented"
      else if gs0.commercial && ownedAt player wp_supershotgun != 0 && shells > 2 then
        pure wp_supershotgun
      else if ownedAt player wp_chaingun != 0 && clip != 0 then
        pure wp_chaingun
      else if ownedAt player wp_shotgun != 0 && shells != 0 then
        pure wp_shotgun
      else if clip != 0 then
        pure wp_pistol
      else if ownedAt player wp_chainsaw != 0 then
        pure wp_chainsaw
      else if ownedAt player wp_missile != 0 && misl != 0 then
        pure wp_missile
      else if ownedAt player wp_bfg != 0 && cells > 40 then
        throw "P_CheckAmmo: BFG switch not implemented"
      else
        pure wp_fist
    let p1 := { player with pendingweapon := pending }
    let gs1 := setPlayer gs0 playerIdx p1
    let gs2 ← enterWeaponDown gs1 playerIdx
    pure (gs2, false)

/-- `DecreaseAmmo` (in-range clip only for this open subset). -/
def decreaseAmmo (player : Player) (ammonum amount : Int32) : Except String Player :=
  if ammonum < NUMAMMO.toInt32 then
    pure (setAmmo player ammonum (ammoAt player ammonum - amount))
  else
    throw "DecreaseAmmo: maxammo overflow path not implemented"

/-- `A_FirePistol`. -/
def firePistol (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "A_FirePistol: bad player"
  | some player0 =>
    let mut gs := { gs0 with rng := startSoundPitchRng gs0.rng sfx_pistol }
    let (gs1, _) ← Enemy.setMobjState gs player0.mo.toNatClampNeg S_PLAY_ATK2
    gs := gs1
    match gs.players[playerIdx]? with
    | none => throw "A_FirePistol: player lost"
    | some p1 =>
      let wi ← weaponAt p1.readyweapon
      let p2 ← decreaseAmmo p1 wi.ammo 1
      match states[wi.flashstate.toNat]? with
      | none => throw "A_FirePistol: bad flashstate"
      | some stFlash =>
        let pspF := getPsp p2.psprites ps_flash
        let mut p3 := {
          p2 with
          psprites := setPsp p2.psprites ps_flash
            { pspF with state := wi.flashstate, tics := stFlash.tics }
          extralight := 1
        }
        if stFlash.action == action_A_Light1 then
          p3 := { p3 with extralight := 1 }
        else if stFlash.action != actionNull then
          throw s!"A_FirePistol: unexpected flash action {stFlash.action}"
        gs := setPlayer gs playerIdx p3
        let (gsB, slope) ← bulletSlope gs p3.mo.toNatClampNeg
        gs := gsB
        match gs.players[playerIdx]? with
        | none => throw "A_FirePistol: player lost before shot"
        | some p4 =>
          gunShot gs p4.mo.toNatClampNeg (p4.refire == 0) slope

/-- `A_FireShotgun`. Seven inaccurate pellets; flash inlined like `A_FirePistol`. -/
def fireShotgun (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "A_FireShotgun: bad player"
  | some player0 =>
    let mut gs := { gs0 with rng := startSoundPitchRng gs0.rng sfx_shotgn }
    let (gs1, _) ← Enemy.setMobjState gs player0.mo.toNatClampNeg S_PLAY_ATK2
    gs := gs1
    match gs.players[playerIdx]? with
    | none => throw "A_FireShotgun: player lost"
    | some p1 =>
      let wi ← weaponAt p1.readyweapon
      let p2 ← decreaseAmmo p1 wi.ammo 1
      match states[wi.flashstate.toNat]? with
      | none => throw "A_FireShotgun: bad flashstate"
      | some stFlash =>
        let pspF := getPsp p2.psprites ps_flash
        let mut p3 := {
          p2 with
          psprites := setPsp p2.psprites ps_flash
            { pspF with state := wi.flashstate, tics := stFlash.tics }
          extralight := 1
        }
        if stFlash.action == action_A_Light1 then
          p3 := { p3 with extralight := 1 }
        else if stFlash.action != actionNull then
          throw s!"A_FireShotgun: unexpected flash action {stFlash.action}"
        gs := setPlayer gs playerIdx p3
        let (gsB, slope) ← bulletSlope gs p3.mo.toNatClampNeg
        gs := gsB
        match gs.players[playerIdx]? with
        | none => throw "A_FireShotgun: player lost before shot"
        | some p4 =>
          let mut i : Nat := 0
          while i < 7 do
            gs ← gunShot gs p4.mo.toNatClampNeg false slope
            i := i + 1
          pure gs

/-- `A_FireMissile`. -/
def fireMissile (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "A_FireMissile: bad player"
  | some p1 =>
    let wi ← weaponAt p1.readyweapon
    let p2 ← decreaseAmmo p1 wi.ammo 1
    let gs := setPlayer gs0 playerIdx p2
    let (gs, _) ← Spawn.spawnPlayerMissile
      Hitscan.aimLineAttack
      Enemy.damageMobj
      Enemy.explodeMissile
      gs p2.mo.toNatClampNeg Spawn.MT_ROCKET
    pure gs

-- Fuel-bounded fireWeapon ↔ setPsprite (no partial); while-guard keeps tics=0 chains.
mutual
/-- `P_FireWeapon`. -/
def fireWeaponFuel (gs0 : GameState) (playerIdx : Nat) (fuel : Nat) :
    Except String GameState :=
  match fuel with
  | 0 => throw "P_FireWeapon: fuel exhausted"
  | fuel' + 1 => do
    match gs0.players[playerIdx]? with
    | none => throw "P_FireWeapon: bad player"
    | some _ =>
      let (gs, hasAmmo) ← checkAmmo gs0 playerIdx
      if !hasAmmo then
        return gs
      match gs.players[playerIdx]? with
      | none => throw "P_FireWeapon: player lost"
      | some player =>
        match gs.mobjs[player.mo.toNatClampNeg]? with
        | none => throw "P_FireWeapon: bad mo"
        | some _ =>
          let (gs1, _) ← Enemy.setMobjState gs player.mo.toNatClampNeg S_PLAY_ATK1
          match gs1.players[playerIdx]? with
          | none => throw "P_FireWeapon: player lost"
          | some p1 =>
            let wi ← weaponAt p1.readyweapon
            let gs2 ← setPspriteFuel gs1 playerIdx ps_weapon wi.atkstate fuel'
            match gs2.players[playerIdx]? with
            | none => throw "P_FireWeapon: player lost"
            | some p2 =>
              noiseAlert gs2 p2.mo.toNatClampNeg p2.mo.toNatClampNeg

/-- `A_WeaponReady`. `true` = continue into `downstate` (pending / no health). -/
def weaponReadyFuel (gs0 : GameState) (playerIdx : Nat) (position : Nat) (fuel : Nat) :
    Except String (GameState × Bool) :=
  match fuel with
  | 0 => throw "A_WeaponReady: fuel exhausted"
  | fuel' + 1 => do
    match gs0.players[playerIdx]? with
    | none => throw "A_WeaponReady: bad player"
    | some player0 =>
      let mut gs := gs0
      let mut player := player0
      match gs.mobjs[player.mo.toNatClampNeg]? with
      | none => throw "A_WeaponReady: bad mo"
      | some mo =>
        if mo.state == S_PLAY_ATK1 || mo.state == S_PLAY_ATK2 then
          let (gs1, _) ← Enemy.setMobjState gs player.mo.toNatClampNeg S_PLAY
          gs := gs1
          match gs.players[playerIdx]? with
          | some p => player := p
          | none => throw "A_WeaponReady: player lost after ATK reset"
        if player.readyweapon == wp_chainsaw then
          throw "A_WeaponReady: chainsaw idle sound not implemented"
        if player.pendingweapon != wp_nochange || player.health == 0 then
          return (gs, true)
        if (player.cmd.buttons &&& BT_ATTACK) != 0 then
          if !player.attackdown ||
              (player.readyweapon != wp_missile && player.readyweapon != wp_bfg) then
            player := { player with attackdown := true }
            gs := setPlayer gs playerIdx player
            gs ← fireWeaponFuel gs playerIdx fuel'
            return (gs, false)
        else
          player := { player with attackdown := false }
        let psp0 := getPsp player.psprites position
        let ang0 := (128 * gs.leveltime) &&& FINEMASK
        let sx :=
          match finecosine[ang0.toNat]? with
          | some c => FRACUNIT + fixedMul player.bob c
          | none => psp0.sx
        let ang1 := ang0 &&& ((FINEANGLES.toUInt32 / 2) - 1)
        let sy :=
          match finesine[ang1.toNat]? with
          | some s => WEAPONTOP + fixedMul player.bob s
          | none => psp0.sy
        let player' := {
          player with
          psprites := setPsp player.psprites position { psp0 with sx, sy }
        }
        pure (setPlayer gs playerIdx player', false)

/-- `A_ReFire`. -/
def reFireFuel (gs0 : GameState) (playerIdx : Nat) (fuel : Nat) :
    Except String GameState :=
  match fuel with
  | 0 => throw "A_ReFire: fuel exhausted"
  | fuel' + 1 => do
    match gs0.players[playerIdx]? with
    | none => throw "A_ReFire: bad player"
    | some player =>
      if (player.cmd.buttons &&& BT_ATTACK) != 0 &&
          player.pendingweapon == wp_nochange &&
          player.health != 0 then
        let gs := setPlayer gs0 playerIdx { player with refire := player.refire + 1 }
        fireWeaponFuel gs playerIdx fuel'
      else
        let gs := setPlayer gs0 playerIdx { player with refire := 0 }
        let (gs1, _) ← checkAmmo gs playerIdx
        pure gs1

/-- GameState-threaded `P_SetPsprite` (weapon fire + raise). -/
def setPspriteFuel (gs0 : GameState) (playerIdx : Nat) (position : Nat) (stnum0 : UInt32)
    (fuel : Nat) : Except String GameState :=
  match fuel with
  | 0 => throw "P_SetPsprite: fuel exhausted"
  | fuel' + 1 => do
    let mut gs := gs0
    let mut stnum := stnum0
    let mut guard : Nat := 0
    while guard < 100000 do
      guard := guard + 1
      match gs.players[playerIdx]? with
      | none => throw "P_SetPsprite: bad player"
      | some p0 =>
        if stnum == 0 then
          return setPlayer gs playerIdx
            { p0 with psprites := setPsp p0.psprites position Psprite.inactive }
        match states[stnum.toNat]? with
        | none => throw s!"P_SetPsprite: bad state {stnum}"
        | some st =>
          let psp0 := getPsp p0.psprites position
          let mut psp := { psp0 with state := stnum, tics := st.tics }
          if st.misc1 != 0 then
            psp := { psp with sx := st.misc1 * FRACUNIT, sy := st.misc2 * FRACUNIT }
          let p1 := { p0 with psprites := setPsp p0.psprites position psp }
          gs := setPlayer gs playerIdx p1
          let mut contRaise := false
          if st.action == action_A_Raise then
            match gs.players[playerIdx]? with
            | none => throw "P_SetPsprite: player lost on raise"
            | some pR =>
              let psp1 := getPsp pR.psprites position
              let sy := psp1.sy - RAISESPEED
              if sy > WEAPONTOP then
                gs := setPlayer gs playerIdx
                  { pR with psprites := setPsp pR.psprites position { psp1 with sy } }
              else
                let pTop := {
                  pR with
                  psprites := setPsp pR.psprites position { psp1 with sy := WEAPONTOP }
                }
                gs := setPlayer gs playerIdx pTop
                let wi ← weaponAt pTop.readyweapon
                stnum := wi.readystate
                contRaise := true
          else if st.action == action_A_Lower then
            gs ← aLower gs playerIdx position
          else if st.action == action_A_WeaponReady then
            let (gs1, toDown) ← weaponReadyFuel gs playerIdx position fuel'
            gs := gs1
            if toDown then
              match gs.players[playerIdx]? with
              | none => throw "P_SetPsprite: player lost on weapon down"
              | some pD =>
                let wi ← weaponAt pD.readyweapon
                stnum := wi.downstate
                contRaise := true
          else if st.action == action_A_FirePistol then
            gs ← firePistol gs playerIdx
          else if st.action == action_A_FireShotgun then
            gs ← fireShotgun gs playerIdx
          else if st.action == action_A_Light1 then
            match gs.players[playerIdx]? with
            | none => throw "A_Light1: bad player"
            | some pL =>
              gs := setPlayer gs playerIdx { pL with extralight := 1 }
          else if st.action == action_A_Light2 then
            match gs.players[playerIdx]? with
            | none => throw "A_Light2: bad player"
            | some pL =>
              gs := setPlayer gs playerIdx { pL with extralight := 2 }
          else if st.action == action_A_Light0 then
            match gs.players[playerIdx]? with
            | none => throw "A_Light0: bad player"
            | some pL =>
              gs := setPlayer gs playerIdx { pL with extralight := 0 }
          else if st.action == action_A_ReFire then
            gs ← reFireFuel gs playerIdx fuel'
          else if st.action == action_A_GunFlash then
            match gs.players[playerIdx]? with
            | none => throw "A_GunFlash: bad player"
            | some pG =>
              let (gs1, _) ← Enemy.setMobjState gs pG.mo.toNatClampNeg S_PLAY_ATK2
              gs := gs1
              match gs.players[playerIdx]? with
              | none => throw "A_GunFlash: player lost"
              | some pG2 =>
                let wi ← weaponAt pG2.readyweapon
                gs ← setPspriteFuel gs playerIdx ps_flash wi.flashstate fuel'
          else if st.action == action_A_FireMissile then
            gs ← fireMissile gs playerIdx
          else if st.action != actionNull then
            throw s!"P_SetPsprite: unimplemented psprite action {st.action} in state {stnum}"
          if !contRaise then
            match gs.players[playerIdx]? with
            | none => throw "P_SetPsprite: player lost after action"
            | some p2 =>
              let psp2 := getPsp p2.psprites position
              if psp2.tics != 0 then
                return gs
              match states[psp2.state.toNat]? with
              | none => throw "P_SetPsprite: state lost"
              | some st2 => stnum := st2.nextstate
    throw "P_SetPsprite: cycle limit"
end

def fireWeapon (gs0 : GameState) (playerIdx : Nat) : Except String GameState :=
  fireWeaponFuel gs0 playerIdx 100000

def weaponReady (gs0 : GameState) (playerIdx : Nat) (position : Nat) :
    Except String (GameState × Bool) :=
  weaponReadyFuel gs0 playerIdx position 100000

def reFire (gs0 : GameState) (playerIdx : Nat) : Except String GameState :=
  reFireFuel gs0 playerIdx 100000

def setPsprite (gs0 : GameState) (playerIdx : Nat) (position : Nat) (stnum0 : UInt32) :
    Except String GameState :=
  setPspriteFuel gs0 playerIdx position stnum0 100000

/-- GameState-threaded `P_MovePsprites`. -/
def movePsprites (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  let mut gs := gs0
  let mut i : Nat := 0
  while i < NUMPSPRITES do
    match gs.players[playerIdx]? with
    | none => throw "P_MovePsprites: bad player"
    | some p =>
      let psp := getPsp p.psprites i
      if psp.state != 0 then
        if psp.tics != -1 then
          let tics := psp.tics - 1
          gs := setPlayer gs playerIdx
            { p with psprites := setPsp p.psprites i { psp with tics } }
          if tics == 0 then
            match gs.players[playerIdx]? with
            | none => throw "P_MovePsprites: player lost"
            | some _ =>
              match states[psp.state.toNat]? with
              | none => throw "P_MovePsprites: bad state"
              | some st =>
                gs ← setPsprite gs playerIdx i st.nextstate
    i := i + 1
  match gs.players[playerIdx]? with
  | none => throw "P_MovePsprites: player lost at copy"
  | some p =>
    let w := getPsp p.psprites ps_weapon
    let f := getPsp p.psprites ps_flash
    pure (setPlayer gs playerIdx
      { p with psprites := setPsp p.psprites ps_flash { f with sx := w.sx, sy := w.sy } })

end Doom.Playsim.Combat
