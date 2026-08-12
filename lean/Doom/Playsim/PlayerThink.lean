import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Psprite
import Doom.Playsim.Tables

/-!
# Doom.Playsim.PlayerThink

`P_CalcHeight`, `P_MovePlayer`, `P_PlayerThink` (tic-0 subset).
-/

namespace Doom.Playsim.PlayerThink

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Psprite
open Doom.Playsim.Tables

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

def MAXBOB : Int32 := 0x100000
def CF_NOMOMENTUM : Int32 := 8  -- unused at tic 0 but matches calcHeight branch

/-- `P_CalcHeight`. `onground` threaded explicitly (C uses a file-scope bool). -/
def calcHeight (gs : GameState) (playerIdx : Nat) (onground : Bool) :
    Except String GameState := do
  match gs.players[playerIdx]? with
  | none => throw "P_CalcHeight: bad player"
  | some player0 =>
    match gs.mobjs[player0.mo.toNatClampNeg]? with
    | none => throw "P_CalcHeight: bad mo"
    | some mo =>
      let bob0 :=
        (fixedMul mo.momx mo.momx + fixedMul mo.momy mo.momy) >>> 2
      let bob1 := if bob0 > MAXBOB then MAXBOB else bob0
      if (player0.cheats &&& CF_NOMOMENTUM) != 0 || !onground then
        throw "P_CalcHeight: nomomentum / airborne path unexpected at tic 0"
      let angle :=
        ((FINEANGLES.toUInt32 / 20 * gs.leveltime) &&& FINEMASK)
      let fine ←
        match finesine[angle.toNat]? with
        | some v => pure v
        | none => throw "P_CalcHeight: finesine OOB"
      let bob := fixedMul (bob1 / 2) fine
      let mut player := { player0 with bob := bob1 }
      if player.playerstate == PST_LIVE then
        player := { player with viewheight := player.viewheight + player.deltaviewheight }
        if player.viewheight > VIEWHEIGHT then
          player := { player with viewheight := VIEWHEIGHT, deltaviewheight := 0 }
        if player.viewheight < VIEWHEIGHT / 2 then
          let d :=
            if player.deltaviewheight <= 0 then (1 : Int32) else player.deltaviewheight
          player := { player with viewheight := VIEWHEIGHT / 2, deltaviewheight := d }
        if player.deltaviewheight != 0 then
          let d0 := player.deltaviewheight + FRACUNIT / 4
          let d := if d0 == 0 then (1 : Int32) else d0
          player := { player with deltaviewheight := d }
      let mut viewz := mo.z + player.viewheight + bob
      if viewz > mo.ceilingz - 4 * FRACUNIT then
        viewz := mo.ceilingz - 4 * FRACUNIT
      player := { player with viewz }
      pure { gs with players := setArr gs.players playerIdx player }

/-- `P_MovePlayer` — zero cmd at tic 0: only sets onground. -/
def movePlayer (gs : GameState) (playerIdx : Nat) : Except String (GameState × Bool) := do
  match gs.players[playerIdx]? with
  | none => throw "P_MovePlayer: bad player"
  | some player =>
    match gs.mobjs[player.mo.toNatClampNeg]? with
    | none => throw "P_MovePlayer: bad mo"
    | some mo =>
      let cmd := player.cmd
      -- angleturn << FRACBITS; zero at tic 0
      let mo :=
        if cmd.angleturn != 0 then
          { mo with angle := mo.angle + (cmd.angleturn.toUInt32 <<< 16) }
        else mo
      let gs := { gs with mobjs := setArr gs.mobjs player.mo.toNatClampNeg mo }
      let onground := mo.z <= mo.floorz
      if cmd.forwardmove != 0 || cmd.sidemove != 0 then
        throw "P_MovePlayer: thrust not implemented (non-zero cmd)"
      pure (gs, onground)

/-- `P_PlayerThink` (live, zero powers/counters, pendingweapon == wp_nochange). -/
def playerThink (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "P_PlayerThink: bad player"
  | some player0 =>
    if player0.playerstate == PST_DEAD then
      throw "P_PlayerThink: death path not implemented"
    match gs0.mobjs[player0.mo.toNatClampNeg]? with
    | none => throw "P_PlayerThink: bad mo"
    | some mo0 =>
      let mut gs := gs0
      let mut onground := true
      if mo0.reactiontime != 0 then
        let mo := { mo0 with reactiontime := mo0.reactiontime - 1 }
        gs := { gs with mobjs := setArr gs.mobjs player0.mo.toNatClampNeg mo }
      else
        let (gs1, og) ← movePlayer gs playerIdx
        gs := gs1
        onground := og
      gs ← calcHeight gs playerIdx onground
      -- special sector
      match gs.players[playerIdx]? with
      | none => throw "P_PlayerThink: lost player"
      | some player =>
        match gs.mobjs[player.mo.toNatClampNeg]? with
        | none => throw "P_PlayerThink: lost mo"
        | some mo =>
          match gs.level.subsectors[mo.subsector.toNat]? with
          | none => throw "P_PlayerThink: bad subsector"
          | some ss =>
            match gs.sectors[ss.sector.toNat]? with
            | none => throw "P_PlayerThink: bad sector"
            | some sec =>
              if sec.special != 0 then
                throw s!"P_PlayerThink: P_PlayerInSpecialSector special={sec.special}"
          let cmd := player.cmd
          if (cmd.buttons &&& 128) != 0 then  -- BT_SPECIAL approx — unused at tic 0
            pure ()
          if (cmd.buttons &&& 4) != 0 then  -- BT_CHANGE
            throw "P_PlayerThink: weapon change not implemented"
          -- pendingweapon stays wp_nochange → no bring-up
          if player.pendingweapon != wp_nochange then
            throw "P_PlayerThink: pendingweapon != wp_nochange unexpected"
          if (cmd.buttons &&& 2) != 0 then  -- BT_USE
            throw "P_PlayerThink: use not implemented"
          gs ←
            match movePsprites player with
            | Except.error e => throw e
            | Except.ok p2 => pure { gs with players := setArr gs.players playerIdx p2 }
          -- power / damage / bonus counters: all zero → no-ops
          pure gs

end Doom.Playsim.PlayerThink
