import Doom.Playsim.Fixed
import Doom.Playsim.Info
import Doom.Playsim.Player
import Doom.Playsim.Weapons

/-!
# Doom.Playsim.Psprite

Weapon psprite setup / raise (`p_pspr.c`) for first-tic parity.
-/

namespace Doom.Playsim.Psprite

open Doom.Playsim.Fixed
open Doom.Playsim.Info
open Doom.Playsim.Player
open Doom.Playsim.Weapons

def WEAPONBOTTOM : Int32 := 128 * FRACUNIT
def WEAPONTOP : Int32 := 32 * FRACUNIT
def RAISESPEED : Int32 := 6 * FRACUNIT
def LOWERSPEED : Int32 := RAISESPEED

def setPsp (ps : Array Psprite) (pos : Nat) (v : Psprite) : Array Psprite :=
  if h : pos < ps.size then ps.set pos v else ps

def getPsp (ps : Array Psprite) (pos : Nat) : Psprite :=
  match ps[pos]? with
  | some p => p
  | none => Psprite.inactive

def weaponAt (w : Int32) : Except String WeaponInfo :=
  match weaponinfo[w.toNatClampNeg]? with
  | some info => pure info
  | none => throw s!"weaponinfo: bad weapon {w}"

/--
`P_SetPsprite` with `A_Raise` inlined (no `partial` / mutual recursion).
-/
def setPsprite (p0 : Player) (position : Nat) (stnum0 : UInt32) : Except String Player := do
  let mut p := p0
  let mut stnum := stnum0
  let mut guard : Nat := 0
  while guard < 100000 do
    guard := guard + 1
    if stnum == 0 then
      return { p with psprites := setPsp p.psprites position Psprite.inactive }
    match states[stnum.toNat]? with
    | none => throw s!"P_SetPsprite: bad state {stnum}"
    | some st =>
      let psp0 := getPsp p.psprites position
      let mut psp := { psp0 with state := stnum, tics := st.tics }
      if st.misc1 != 0 then
        psp := {
          psp with
          sx := st.misc1 * FRACUNIT
          sy := st.misc2 * FRACUNIT
        }
      p := { p with psprites := setPsp p.psprites position psp }
      let mut contRaise := false
      if st.action == action_A_Raise then
        let psp1 := getPsp p.psprites position
        let sy := psp1.sy - RAISESPEED
        if sy > WEAPONTOP then
          p := { p with psprites := setPsp p.psprites position { psp1 with sy } }
        else
          p := {
            p with
            psprites := setPsp p.psprites position { psp1 with sy := WEAPONTOP }
          }
          let wi ← weaponAt p.readyweapon
          stnum := wi.readystate
          contRaise := true
      else if st.action == action_A_Lower then
        let pspL := getPsp p.psprites position
        let syL := pspL.sy + LOWERSPEED
        if syL < WEAPONBOTTOM then
          p := { p with psprites := setPsp p.psprites position { pspL with sy := syL } }
        else if p.playerstate == PST_DEAD then
          p := { p with psprites := setPsp p.psprites position { pspL with sy := WEAPONBOTTOM } }
        else if p.health == 0 then
          throw "A_Lower: health==0 not implemented"
        else
          let pending :=
            if p.pendingweapon == wp_nochange then p.readyweapon else p.pendingweapon
          p := {
            p with
            readyweapon := pending
            pendingweapon := wp_nochange
            psprites := setPsp p.psprites position { pspL with sy := WEAPONBOTTOM }
          }
          let wi ← weaponAt pending
          stnum := wi.upstate
          contRaise := true
      else if st.action == action_A_WeaponReady then
        pure ()
      else if st.action != actionNull then
        throw s!"P_SetPsprite: unimplemented psprite action {st.action} in state {stnum}"
      if !contRaise then
        let psp2 := getPsp p.psprites position
        if psp2.tics != 0 then
          return p
        match states[psp2.state.toNat]? with
        | none => throw "P_SetPsprite: state lost"
        | some st2 => stnum := st2.nextstate
  throw "P_SetPsprite: cycle limit"

/-- `P_BringUpWeapon`. -/
def bringUpWeapon (p0 : Player) : Except String Player := do
  let mut pending := p0.pendingweapon
  if pending == wp_nochange then
    pending := p0.readyweapon
  let wi ← weaponAt pending
  let psp := getPsp p0.psprites ps_weapon
  let p := {
    p0 with
    pendingweapon := wp_nochange
    psprites := setPsp p0.psprites ps_weapon { psp with sy := WEAPONBOTTOM }
  }
  setPsprite p ps_weapon wi.upstate

/-- `P_DropWeapon` — enter `readyweapon.downstate` on `ps_weapon`. -/
def dropWeapon (p0 : Player) : Except String Player := do
  let wi ← weaponAt p0.readyweapon
  setPsprite p0 ps_weapon wi.downstate

/-- `P_SetupPsprites`. -/
def setupPsprites (p0 : Player) : Except String Player := do
  let mut p := p0
  let mut i : Nat := 0
  while i < NUMPSPRITES do
    p := { p with psprites := setPsp p.psprites i Psprite.inactive }
    i := i + 1
  p := { p with pendingweapon := p.readyweapon }
  bringUpWeapon p

end Doom.Playsim.Psprite
