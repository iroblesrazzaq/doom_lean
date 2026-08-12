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

private def setPsp (ps : Array Psprite) (pos : Nat) (v : Psprite) : Array Psprite :=
  if h : pos < ps.size then ps.set pos v else ps

private def getPsp (ps : Array Psprite) (pos : Nat) : Psprite :=
  match ps[pos]? with
  | some p => p
  | none => Psprite.inactive

private def weaponAt (w : Int32) : Except String WeaponInfo :=
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

/-- `P_SetupPsprites`. -/
def setupPsprites (p0 : Player) : Except String Player := do
  let mut p := p0
  let mut i : Nat := 0
  while i < NUMPSPRITES do
    p := { p with psprites := setPsp p.psprites i Psprite.inactive }
    i := i + 1
  p := { p with pendingweapon := p.readyweapon }
  bringUpWeapon p

/-- `P_MovePsprites`. -/
def movePsprites (p0 : Player) : Except String Player := do
  let mut p := p0
  let mut i : Nat := 0
  while i < NUMPSPRITES do
    let psp := getPsp p.psprites i
    if psp.state != 0 then
      if psp.tics != -1 then
        let tics := psp.tics - 1
        p := { p with psprites := setPsp p.psprites i { psp with tics } }
        if tics == 0 then
          match states[psp.state.toNat]? with
          | none => throw "P_MovePsprites: bad state"
          | some st =>
            p ← setPsprite p i st.nextstate
    i := i + 1
  let w := getPsp p.psprites ps_weapon
  let f := getPsp p.psprites ps_flash
  pure { p with psprites := setPsp p.psprites ps_flash { f with sx := w.sx, sy := w.sy } }

end Doom.Playsim.Psprite
