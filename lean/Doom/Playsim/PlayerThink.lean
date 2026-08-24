import Doom.Playsim.Angle
import Doom.Playsim.Combat
import Doom.Playsim.Enemy
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Spec
import Doom.Playsim.Tables
import Doom.Playsim.Think

/-!
# Doom.Playsim.PlayerThink

`P_Thrust`, `P_CalcHeight`, `P_MovePlayer`, `P_PlayerThink`.
-/

namespace Doom.Playsim.PlayerThink

open Doom.Playsim.Angle
open Doom.Playsim.Combat
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Spec
open Doom.Playsim.Tables
open Doom.Playsim.Think

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

/-- C `if (powers[idx]) powers[idx] += delta`; zero stays zero. -/
private def adjPower (p : Player) (idx : Nat) (delta : Int32) : Player :=
  match p.powers[idx]? with
  | some v =>
    if v != 0 then { p with powers := setArr p.powers idx (v + delta) } else p
  | none => p

def MAXBOB : Int32 := 0x100000
def CF_NOMOMENTUM : Int32 := 8
def DEATH_VIEWHEIGHT : Int32 := 6 * FRACUNIT

/-- `P_Thrust` — `finecosine = finesine+2048` indexing via `Tables.finecosine`. -/
def thrust (gs : GameState) (playerIdx : Nat) (angle : UInt32) (move : Int32) :
    Except String GameState := do
  match gs.players[playerIdx]? with
  | none => throw "P_Thrust: bad player"
  | some player =>
    match gs.mobjs[player.mo.toNatClampNeg]? with
    | none => throw "P_Thrust: bad mo"
    | some mo =>
      let fineIdx := (angle >>> ANGLETOFINESHIFT.toUInt32) &&& FINEMASK
      match finecosine[fineIdx.toNat]?, finesine[fineIdx.toNat]? with
      | some cosv, some sinv =>
        let mo := {
          mo with
          momx := mo.momx + fixedMul move cosv
          momy := mo.momy + fixedMul move sinv
        }
        pure { gs with mobjs := setArr gs.mobjs player.mo.toNatClampNeg mo }
      | _, _ => throw "P_Thrust: fine table OOB"

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
      -- Always compute bob (gun swing); CF_NOMOMENTUM / airborne early-out.
      if (player0.cheats &&& CF_NOMOMENTUM) != 0 || !onground then
        let mut viewz := mo.z + VIEWHEIGHT
        if viewz > mo.ceilingz - 4 * FRACUNIT then
          viewz := mo.ceilingz - 4 * FRACUNIT
        -- C overwrites with z+viewheight after the ceiling clamp (quirky).
        viewz := mo.z + player0.viewheight
        let player := { player0 with bob := bob1, viewz }
        pure { gs with players := setArr gs.players playerIdx player }
      else
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

/-- `P_DeathThink` — no `P_Random`. -/
def deathThink (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "P_DeathThink: bad player"
  | some player0 =>
    let mut gs := gs0
    let mut pDeath := player0
    gs ← Combat.movePsprites gs playerIdx
    match gs.players[playerIdx]? with
    | none => throw "P_DeathThink: lost player after psprites"
    | some p1 => pDeath := p1
    if pDeath.viewheight > DEATH_VIEWHEIGHT then
      pDeath := { pDeath with viewheight := pDeath.viewheight - FRACUNIT }
    if pDeath.viewheight < DEATH_VIEWHEIGHT then
      pDeath := { pDeath with viewheight := DEATH_VIEWHEIGHT }
    pDeath := { pDeath with deltaviewheight := 0 }
    gs := { gs with players := setArr gs.players playerIdx pDeath }
    match gs.mobjs[pDeath.mo.toNatClampNeg]? with
    | none => throw "P_DeathThink: bad mo"
    | some mo0 =>
      let onground := mo0.z <= mo0.floorz
      gs ← calcHeight gs playerIdx onground
      match gs.players[playerIdx]? with
      | none => throw "P_DeathThink: lost player after calcHeight"
      | some player1 =>
        match gs.mobjs[player1.mo.toNatClampNeg]? with
        | none => throw "P_DeathThink: lost mo after calcHeight"
        | some mo1 =>
          let mut pOut := player1
          let mut moOut := mo1
          if player1.attacker >= 0 && player1.attacker != player1.mo then
            match gs.mobjs[player1.attacker.toNatClampNeg]? with
            | none => throw "P_DeathThink: bad attacker"
            | some att =>
              let angle := pointToAngle2 mo1.x mo1.y att.x att.y
              let delta := angle - mo1.angle
              if delta < ANG5 || delta > (0 : UInt32) - ANG5 then
                moOut := { mo1 with angle := angle }
                if player1.damagecount > 0 then
                  pOut := { player1 with damagecount := player1.damagecount - 1 }
              else if delta < ANG180 then
                moOut := { mo1 with angle := mo1.angle + ANG5 }
              else
                moOut := { mo1 with angle := mo1.angle - ANG5 }
          else if player1.damagecount > 0 then
            pOut := { player1 with damagecount := player1.damagecount - 1 }
          gs := { gs with
            mobjs := setArr gs.mobjs player1.mo.toNatClampNeg moOut
            players := setArr gs.players playerIdx pOut
          }
          if (pOut.cmd.buttons &&& BT_USE) != 0 then
            pure { gs with
              players := setArr gs.players playerIdx { pOut with playerstate := PST_REBORN }
            }
          else
            pure gs

/-- `P_MovePlayer`. -/
def movePlayer (gs0 : GameState) (playerIdx : Nat) : Except String (GameState × Bool) := do
  match gs0.players[playerIdx]? with
  | none => throw "P_MovePlayer: bad player"
  | some player =>
    match gs0.mobjs[player.mo.toNatClampNeg]? with
    | none => throw "P_MovePlayer: bad mo"
    | some mo0 =>
      let cmd := player.cmd
      let mo :=
        { mo0 with angle := mo0.angle + (cmd.angleturn.toUInt32 <<< 16) }
      let mut gs := { gs0 with mobjs := setArr gs0.mobjs player.mo.toNatClampNeg mo }
      let onground := mo.z <= mo.floorz
      if cmd.forwardmove != 0 && onground then
        gs ← thrust gs playerIdx mo.angle (cmd.forwardmove * 2048)
      if cmd.sidemove != 0 && onground then
        match gs.mobjs[player.mo.toNatClampNeg]? with
        | none => throw "P_MovePlayer: lost mo before side thrust"
        | some mo1 =>
          gs ← thrust gs playerIdx (mo1.angle - ANG90) (cmd.sidemove * 2048)
      if (cmd.forwardmove != 0 || cmd.sidemove != 0) then
        match gs.mobjs[player.mo.toNatClampNeg]? with
        | none => throw "P_MovePlayer: lost mo before run state"
        | some mo2 =>
          if mo2.state == S_PLAY then
            let (gs1, _) ← setMobjState gs player.mo.toNatClampNeg S_PLAY_RUN1
            gs := gs1
      pure (gs, onground)

/--
`P_PlayerInSpecialSector` open subset: early-out when not on sector floor;
nukage (`special=7`) calls `P_DamageMobj(..., 5)` on the ironfeet+beat gate;
hellslime (`special=5`) still loud-errors on that same gate; secret
(`special=9`) wrapping-increments `secretcount` and clears
`sectors[secIdx].special`. Other specials are loud-errors. No `P_Random`
here; ironfeet is truthiness only.
-/
def playerInSpecialSector (gs : GameState) (playerIdx : Nat)
    (secIdx : Nat) (sec : SectorRuntime) : Except String GameState := do
  match gs.players[playerIdx]? with
  | none => throw "P_PlayerInSpecialSector: bad player"
  | some player =>
    match gs.mobjs[player.mo.toNatClampNeg]? with
    | none => throw "P_PlayerInSpecialSector: bad mo"
    | some mo =>
      -- Falling, not all the way down yet?
      if mo.z != sec.floorheight then
        return gs
      let ironfeet := match player.powers[pw_ironfeet]? with | some v => v | none => (0 : Int32)
      if sec.special == 5 || sec.special == 7 then
        if ironfeet == 0 && (gs.leveltime &&& (0x1f : UInt32)) == 0 then
          if sec.special == 7 then
            Enemy.damageMobj gs player.mo.toNatClampNeg none none (5 : Int32)
          else
            throw s!"P_PlayerInSpecialSector: damage special={sec.special}"
        else
          pure gs
      else if sec.special == 4 || sec.special == 16 then
        throw s!"P_PlayerInSpecialSector: strobe/super slime special={sec.special}"
      else if sec.special == 9 then
        pure {
          gs with
          players := setArr gs.players playerIdx
            { player with secretcount := player.secretcount + 1 }
          sectors := setArr gs.sectors secIdx { sec with special := 0 }
        }
      else if sec.special == 11 then
        throw "P_PlayerInSpecialSector: exit super damage not implemented"
      else
        throw s!"P_PlayerInSpecialSector: unknown special {sec.special}"

/-- `P_PlayerThink` (live). Pending weapon change is left to `A_WeaponReady`. -/
def playerThink (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "P_PlayerThink: bad player"
  | some player0 =>
    if player0.playerstate == PST_DEAD then
      deathThink gs0 playerIdx
    else
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
      -- special sector (may update player health/armor via P_DamageMobj)
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
                gs ← playerInSpecialSector gs playerIdx ss.sector.toNat sec
      -- Re-fetch: usedown latch must not restore a pre-damage player snapshot.
      match gs.players[playerIdx]? with
      | none => throw "P_PlayerThink: lost player after special"
      | some player =>
        let cmd := player.cmd
        if (cmd.buttons &&& 128) != 0 then  -- BT_SPECIAL approx — unused early
          pure ()
        if (cmd.buttons &&& 4) != 0 then  -- BT_CHANGE
          throw "P_PlayerThink: weapon change not implemented"
        if (cmd.buttons &&& BT_USE) != 0 then
          if !player.usedown then
            gs ← useLines gs playerIdx
            match gs.players[playerIdx]? with
            | none => throw "P_PlayerThink: lost player after use"
            | some pUse =>
              gs := { gs with players := setArr gs.players playerIdx { pUse with usedown := true } }
        else
          gs := { gs with players := setArr gs.players playerIdx { player with usedown := false } }
        gs ← Combat.movePsprites gs playerIdx
        -- C `P_PlayerThink` live counters (`p_user.c`); untraced.
        match gs.players[playerIdx]? with
        | none => throw "P_PlayerThink: lost player after psprites"
        | some pEnd =>
          let mut p := pEnd
          p := adjPower p pw_strength 1
          p := adjPower p pw_invulnerability (-1)
          match p.powers[pw_invisibility]? with
          | some v =>
            if v != 0 then
              let v' := v - 1
              p := { p with powers := setArr p.powers pw_invisibility v' }
              if v' == 0 then
                match gs.mobjs[p.mo.toNatClampNeg]? with
                | none => throw "P_PlayerThink: lost mo clearing MF_SHADOW"
                | some mo =>
                  gs := { gs with
                    mobjs := setArr gs.mobjs p.mo.toNatClampNeg
                      { mo with flags := mo.flags &&& (~~~MF_SHADOW) } }
          | none => pure ()
          p := adjPower p pw_infrared (-1)
          p := adjPower p pw_ironfeet (-1)
          if p.damagecount != 0 then
            p := { p with damagecount := p.damagecount - 1 }
          if p.bonuscount != 0 then
            p := { p with bonuscount := p.bonuscount - 1 }
          pure { gs with players := setArr gs.players playerIdx p }

end Doom.Playsim.PlayerThink
