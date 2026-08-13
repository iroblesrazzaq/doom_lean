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

/-- `P_CheckAmmo` true-path; loud-error if a weapon switch would occur. -/
def checkAmmo (player : Player) : Except String Unit := do
  let wi ← weaponAt player.readyweapon
  let ammo := wi.ammo
  let count : Int32 :=
    if player.readyweapon == wp_bfg then (40 : Int32)
    else if player.readyweapon == wp_supershotgun then 2
    else 1
  if ammo == am_noammo then
    return
  if ammoAt player ammo >= count then
    return
  throw "P_CheckAmmo: weapon switch not implemented"

/-- `DecreaseAmmo` (in-range clip only for this open subset). -/
def decreaseAmmo (player : Player) (ammonum amount : Int32) : Except String Player :=
  if ammonum < NUMAMMO.toInt32 then
    pure (setAmmo player ammonum (ammoAt player ammonum - amount))
  else
    throw "DecreaseAmmo: maxammo overflow path not implemented"

/-- `P_FireWeapon`. -/
def fireWeapon (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "P_FireWeapon: bad player"
  | some player =>
    checkAmmo player
    match gs0.mobjs[player.mo.toNatClampNeg]? with
    | none => throw "P_FireWeapon: bad mo"
    | some _ =>
      let (gs1, _) ← Enemy.setMobjState gs0 player.mo.toNatClampNeg S_PLAY_ATK1
      match gs1.players[playerIdx]? with
      | none => throw "P_FireWeapon: player lost"
      | some p1 =>
        let wi ← weaponAt p1.readyweapon
        match states[wi.atkstate.toNat]? with
        | none => throw "P_FireWeapon: bad atkstate"
        | some st =>
          if st.action != actionNull then
            throw s!"P_FireWeapon: unexpected atk action {st.action}"
          let psp := getPsp p1.psprites ps_weapon
          let p2 := {
            p1 with
            psprites := setPsp p1.psprites ps_weapon
              { psp with state := wi.atkstate, tics := st.tics }
          }
          let gs2 := setPlayer gs1 playerIdx p2
          noiseAlert gs2 p2.mo.toNatClampNeg p2.mo.toNatClampNeg

/-- `A_WeaponReady`. -/
def weaponReady (gs0 : GameState) (playerIdx : Nat) (position : Nat) :
    Except String GameState := do
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
        throw "A_WeaponReady: weapon down/change not implemented"
      if (player.cmd.buttons &&& BT_ATTACK) != 0 then
        if !player.attackdown ||
            (player.readyweapon != wp_missile && player.readyweapon != wp_bfg) then
          player := { player with attackdown := true }
          gs := setPlayer gs playerIdx player
          gs ← fireWeapon gs playerIdx
          return gs
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
      pure (setPlayer gs playerIdx player')

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

/-- `A_ReFire`. -/
def reFire (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "A_ReFire: bad player"
  | some player =>
    if (player.cmd.buttons &&& BT_ATTACK) != 0 &&
        player.pendingweapon == wp_nochange &&
        player.health != 0 then
      let gs := setPlayer gs0 playerIdx { player with refire := player.refire + 1 }
      fireWeapon gs playerIdx
    else
      let gs := setPlayer gs0 playerIdx { player with refire := 0 }
      match gs.players[playerIdx]? with
      | none => throw "A_ReFire: player lost"
      | some p =>
        checkAmmo p
        pure gs

/-- GameState-threaded `P_SetPsprite` (weapon fire + raise). -/
def setPsprite (gs0 : GameState) (playerIdx : Nat) (position : Nat) (stnum0 : UInt32) :
    Except String GameState := do
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
        else if st.action == action_A_WeaponReady then
          gs ← weaponReady gs playerIdx position
        else if st.action == action_A_FirePistol then
          gs ← firePistol gs playerIdx
        else if st.action == action_A_Light1 then
          match gs.players[playerIdx]? with
          | none => throw "A_Light1: bad player"
          | some pL =>
            gs := setPlayer gs playerIdx { pL with extralight := 1 }
        else if st.action == action_A_Light0 then
          match gs.players[playerIdx]? with
          | none => throw "A_Light0: bad player"
          | some pL =>
            gs := setPlayer gs playerIdx { pL with extralight := 0 }
        else if st.action == action_A_ReFire then
          gs ← reFire gs playerIdx
        else if st.action == action_A_GunFlash then
          throw "A_GunFlash: not implemented"
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
