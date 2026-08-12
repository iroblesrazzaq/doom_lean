import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Sound
import Doom.Playsim.Thinker

/-!
# Doom.Playsim.Inter

`p_inter.c` open subset: `P_GiveArmor` / `P_TouchSpecialThing` (`SPR_ARM1` only)
and `P_RemoveMobj` (`p_mobj.c`).
-/

namespace Doom.Playsim.Inter

open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.MapUtil
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Sound
open Doom.Playsim.Thinker

/-- `spritenum_t` `SPR_ARM1`. -/
def SPR_ARM1 : UInt32 := 55

/-- `BONUSADD` (`p_inter.c`). -/
def BONUSADD : Int32 := 6

/-- Green armor class (`deh_green_armor_class` default). -/
def greenArmorClass : Int32 := 1

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

/-- Mark the `THF_MOBJ` thinker for `mobjIdx` as `THF_REMOVED` (lazy free). -/
def markThinkerRemoved (gs : GameState) (mobjIdx : Nat) : Except String GameState := do
  let mut found := false
  let mut thinkers := gs.thinkers
  let mut i : Nat := 0
  while i < thinkers.size do
    match thinkers[i]? with
    | none => throw "P_RemoveMobj: bad thinker"
    | some th =>
      if th.func == THF_MOBJ && th.payload.toNat == mobjIdx then
        thinkers := setArr thinkers i { th with func := THF_REMOVED }
        found := true
      pure ()
    i := i + 1
  if !found then
    throw s!"P_RemoveMobj: no thinker for mobj {mobjIdx}"
  pure { gs with thinkers }

/--
`P_RemoveMobj` — unlink + mark thinker removed. Item-respawn queue is not
modeled (untraced; deathmatch respawn unused for DEMO1).
-/
def removeMobj (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  let gs ← unsetThingPosition gs0 mobjIdx
  markThinkerRemoved gs mobjIdx

/-- `P_GiveArmor` — returns `(player, gave?)`. -/
def giveArmor (player : Player) (armortype : Int32) : Player × Bool :=
  let hits := armortype * 100
  if player.armorpoints >= hits then
    (player, false)
  else
    ({ player with armortype := armortype, armorpoints := hits }, true)

/--
`P_TouchSpecialThing` — DEMO1 open subset: reachability + `SPR_ARM1` green
armor only. All other sprites loud-error.
-/
def touchSpecialThing (gs0 : GameState) (specialIdx toucherIdx : Nat) :
    Except String GameState := do
  match gs0.mobjs[specialIdx]?, gs0.mobjs[toucherIdx]? with
  | none, _ => throw "P_TouchSpecialThing: bad special"
  | _, none => throw "P_TouchSpecialThing: bad toucher"
  | some special, some toucher =>
    let delta := special.z - toucher.z
    if delta > toucher.height || delta < (-8) * FRACUNIT then
      return gs0
    if toucher.health <= 0 then
      return gs0
    if toucher.player < 0 then
      throw "P_TouchSpecialThing: toucher has no player"
    let pi := toucher.player.toNatClampNeg
    match gs0.players[pi]? with
    | none => throw "P_TouchSpecialThing: bad player"
    | some player0 =>
      if special.sprite != SPR_ARM1 then
        throw s!"P_TouchSpecialThing: unsupported sprite {special.sprite}"
      let (player1, gave) := giveArmor player0 greenArmorClass
      if !gave then
        return gs0
      let mut gs := { gs0 with players := setArr gs0.players pi player1 }
      if (special.flags &&& MF_COUNTITEM) != 0 then
        match gs.players[pi]? with
        | none => throw "P_TouchSpecialThing: player lost"
        | some p =>
          gs := {
            gs with
            players := setArr gs.players pi { p with itemcount := p.itemcount + 1 }
          }
      gs ← removeMobj gs specialIdx
      match gs.players[pi]? with
      | none => throw "P_TouchSpecialThing: player lost after remove"
      | some p =>
        let p := { p with bonuscount := p.bonuscount + BONUSADD }
        gs := { gs with players := setArr gs.players pi p }
        if pi == gs.consoleplayer then
          gs := { gs with rng := startSoundPitchRng gs.rng sfx_itemup }
        pure gs

end Doom.Playsim.Inter
