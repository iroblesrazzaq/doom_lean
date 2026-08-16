import Doom.Playsim.Enemy
import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Inter
import Doom.Playsim.Map
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Random
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
# Doom.Playsim.Think

`P_MobjThinker`, `P_ZMovement`, light thinkers (`p_lights.c`).
Enemy actions / `P_SetMobjState` live in `Enemy` (P2c-iii). Missile XY/Z
explode lives here and calls `Enemy.explodeMissile`.
-/

namespace Doom.Playsim.Think

open Doom.Playsim.Enemy
open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Inter
open Doom.Playsim.Map
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Random
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

/-- Re-export for `PlayerThink` / callers that open `Think`. -/
def setMobjState := Enemy.setMobjState

/-- `statenum_t` player idle / run. -/
def S_PLAY : UInt32 := 149
def S_PLAY_RUN1 : UInt32 := 150

def STOPSPEED : Int32 := 0x1000
def FRICTION : Int32 := 0xe800
/-- `GRAVITY` (`p_local.h`) — `FRACUNIT`. -/
def GRAVITY : Int32 := FRACUNIT
/-- `CF_NOMOMENTUM` cheat bit (`p_user.c` / `p_local.h`). -/
def CF_NOMOMENTUM : Int32 := 8

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

private def setMo (gs : GameState) (i : Nat) (mo : Mobj) : GameState :=
  { gs with mobjs := setArr gs.mobjs i mo }

/-- True while this mobj still has a live `THF_MOBJ` thinker (`P_RemoveMobj` clears it). -/
private def mobjHasMobjThinker (gs : GameState) (mobjIdx : Nat) : Bool :=
  Id.run do
    let mut i : Nat := 0
    let mut found := false
    while i < gs.thinkers.size do
      match gs.thinkers[i]? with
      | some th =>
        if th.func == THF_MOBJ && th.payload.toNat == mobjIdx then
          found := true
      | none => pure ()
      i := i + 1
    pure found

/-- `P_XYMovement` (`p_mobj.c`). -/
def xyMovement (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_XYMovement: bad mobj"
  | some mo0 =>
    if mo0.momx == 0 && mo0.momy == 0 then
      if (mo0.flags &&& MF_SKULLFLY) != 0 then
        throw "P_XYMovement: skullfly zero-mom path not implemented"
      return gs0
    let mut gs := gs0
    let mut mo := mo0
    let momx :=
      if mo.momx > MAXMOVE then MAXMOVE
      else if mo.momx < -MAXMOVE then -MAXMOVE
      else mo.momx
    let momy :=
      if mo.momy > MAXMOVE then MAXMOVE
      else if mo.momy < -MAXMOVE then -MAXMOVE
      else mo.momy
    mo := { mo with momx, momy }
    gs := setMo gs mobjIdx mo
    let mut xmove := momx
    let mut ymove := momy
    let mut guard : Nat := 8
    while (xmove != 0 || ymove != 0) && guard > 0 do
      guard := guard - 1
      match gs.mobjs[mobjIdx]? with
      | none => throw "P_XYMovement: lost mobj"
      | some moCur =>
        let (ptryx, ptryy, xmove', ymove') :=
          if xmove > MAXMOVE / 2 || ymove > MAXMOVE / 2 then
            (moCur.x + xmove / 2, moCur.y + ymove / 2, xmove >>> 1, ymove >>> 1)
          else
            (moCur.x + xmove, moCur.y + ymove, (0 : Int32), (0 : Int32))
        xmove := xmove'
        ymove := ymove'
        let (gs1, scr, ok) ← tryMove gs mobjIdx ptryx ptryy (some damageMobj)
          (some Spec.crossSpecialLine)
        gs := gs1
        if !ok then
          match gs.mobjs[mobjIdx]? with
          | none => throw "P_XYMovement: lost after blocked"
          | some moB =>
            if moB.player >= 0 then
              -- P_SlideMove (may also pick up MF_SPECIAL via TryMove/CheckPosition)
              gs ← slideMove gs mobjIdx (some Spec.crossSpecialLine)
            else if (moB.flags &&& MF_MISSILE) != 0 then
              if ceilinglineIsSky gs scr then
                return (← removeMobj gs mobjIdx)
              gs ← explodeMissile gs mobjIdx
            else
              gs := setMo gs mobjIdx { moB with momx := 0, momy := 0 }
              xmove := 0
              ymove := 0
    match gs.mobjs[mobjIdx]? with
    | none => throw "P_XYMovement: lost before friction"
    | some moF =>
      let player? : Option Player :=
        if moF.player >= 0 then gs.players[moF.player.toNatClampNeg]? else none
      match player? with
      | some player =>
        if (player.cheats &&& CF_NOMOMENTUM) != 0 then
          return setMo gs mobjIdx { moF with momx := 0, momy := 0 }
      | none => pure ()
      if (moF.flags &&& (MF_MISSILE ||| MF_SKULLFLY)) != 0 then
        return gs
      if moF.z > moF.floorz then
        return gs
      if (moF.flags &&& MF_CORPSE) != 0 then
        if moF.momx > FRACUNIT / 4 || moF.momx < -(FRACUNIT / 4) ||
            moF.momy > FRACUNIT / 4 || moF.momy < -(FRACUNIT / 4) then
          match gs.level.subsectors[moF.subsector.toNat]? with
          | none => throw "P_XYMovement: corpse bad subsector"
          | some ss =>
            match gs.sectors[ss.sector.toNat]? with
            | none => throw "P_XYMovement: corpse bad sector"
            | some sec =>
              if moF.floorz != sec.floorheight then
                return gs
      let cmdIdle :=
        match player? with
        | none => true
        | some player => player.cmd.forwardmove == 0 && player.cmd.sidemove == 0
      if moF.momx > -STOPSPEED && moF.momx < STOPSPEED &&
          moF.momy > -STOPSPEED && moF.momy < STOPSPEED && cmdIdle then
        let mut gs2 := gs
        match player? with
        | some _player =>
          -- walking frame stop: (state - S_PLAY_RUN1) < 4
          if moF.state >= S_PLAY_RUN1 && moF.state < S_PLAY_RUN1 + 4 then
            let (gs3, _) ← setMobjState gs2 mobjIdx S_PLAY
            gs2 := gs3
        | none => pure ()
        match gs2.mobjs[mobjIdx]? with
        | none => throw "P_XYMovement: lost on stop"
        | some moS =>
          pure (setMo gs2 mobjIdx { moS with momx := 0, momy := 0 })
      else
        pure (setMo gs mobjIdx {
          moF with
          momx := fixedMul moF.momx FRICTION
          momy := fixedMul moF.momy FRICTION
        })

/-- `P_ZMovement` (`p_mobj.c`) — DEMO1 open subset (no float/skull/missile). -/
def zMovement (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_ZMovement: bad mobj"
  | some mo0 =>
    let mut gs := gs0
    let mut mo := mo0
    -- Smooth step-up when XY raised floorz under a player still below it.
    if mo.player >= 0 && mo.z < mo.floorz then
      match gs.players[mo.player.toNatClampNeg]? with
      | none => throw "P_ZMovement: bad player for step-up"
      | some pl =>
        let vh := pl.viewheight - (mo.floorz - mo.z)
        let dv := (VIEWHEIGHT - vh) >>> 3
        gs := {
          gs with
          players := setArr gs.players mo.player.toNatClampNeg
            { pl with viewheight := vh, deltaviewheight := dv }
        }
    mo := { mo with z := mo.z + mo.momz }
    gs := setMo gs mobjIdx mo
    if (mo.flags &&& MF_FLOAT) != 0 && mo.target >= 0 then
      throw "P_ZMovement: MF_FLOAT+target float path not implemented"
    if (mo.flags &&& MF_SKULLFLY) != 0 then
      throw "P_ZMovement: MF_SKULLFLY path not implemented"
    -- Floor clip
    if mo.z <= mo.floorz then
      if mo.momz < 0 then
        if mo.player >= 0 && mo.momz < -GRAVITY * 8 then
          match gs.players[mo.player.toNatClampNeg]? with
          | none => throw "P_ZMovement: bad player for hard land"
          | some pl =>
            let pl := { pl with deltaviewheight := mo.momz >>> 3 }
            gs := {
              gs with
              players := setArr gs.players mo.player.toNatClampNeg pl
              rng := startSoundPitchRng gs.rng sfx_oof
            }
        mo := { mo with momz := 0 }
      mo := { mo with z := mo.floorz }
      gs := setMo gs mobjIdx mo
      if (mo.flags &&& MF_MISSILE) != 0 && (mo.flags &&& MF_NOCLIP) == 0 then
        return (← explodeMissile gs mobjIdx)
    else if (mo.flags &&& MF_NOGRAVITY) == 0 then
      let momz :=
        if mo.momz == 0 then -GRAVITY * 2
        else mo.momz - GRAVITY
      mo := { mo with momz }
      gs := setMo gs mobjIdx mo
    -- Ceiling clip
    match gs.mobjs[mobjIdx]? with
    | none => throw "P_ZMovement: lost before ceiling"
    | some moC =>
      if moC.z + moC.height > moC.ceilingz then
        let mut mo2 := moC
        if mo2.momz > 0 then
          mo2 := { mo2 with momz := 0 }
        mo2 := { mo2 with z := mo2.ceilingz - mo2.height }
        gs := setMo gs mobjIdx mo2
        if (mo2.flags &&& MF_SKULLFLY) != 0 then
          throw "P_ZMovement: MF_SKULLFLY ceiling bounce not implemented"
        if (mo2.flags &&& MF_MISSILE) != 0 && (mo2.flags &&& MF_NOCLIP) == 0 then
          return (← explodeMissile gs mobjIdx)
        pure gs
      else
        pure gs

/-- `P_MobjThinker` — XY + Z movement, then state tic countdown. -/
def mobjThinker (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_MobjThinker: bad mobj"
  | some mo =>
    let mut gs := gs0
    if mo.momx != 0 || mo.momy != 0 || (mo.flags &&& MF_SKULLFLY) != 0 then
      gs ← xyMovement gs mobjIdx
      if !mobjHasMobjThinker gs mobjIdx then
        return gs
    match gs.mobjs[mobjIdx]? with
    | none => return gs
    | some mo2 =>
      if mo2.z != mo2.floorz || mo2.momz != 0 then
        gs ← zMovement gs mobjIdx
        if !mobjHasMobjThinker gs mobjIdx then
          return gs
      -- Re-fetch after Z — no stale writes into the tic countdown.
      match gs.mobjs[mobjIdx]? with
      | none => return gs
      | some moZ =>
        if moZ.tics != -1 then
          let tics := moZ.tics - 1
          gs := setMo gs mobjIdx { moZ with tics }
          if tics == 0 then
            match gs.mobjs[mobjIdx]? with
            | none => throw "P_MobjThinker: lost"
            | some mo3 =>
              match states[mo3.state.toNat]? with
              | none => throw "P_MobjThinker: bad state"
              | some st =>
                let (gs2, stillPresent) ← setMobjState gs mobjIdx st.nextstate
                if !stillPresent then
                  return gs2
                pure gs2
          else
            pure gs
        else
          -- nightmare respawn path unused
          pure gs

/-- `T_LightFlash`. -/
def lightFlashThinker (gs0 : GameState) (payload : Nat) : Except String GameState := do
  match gs0.lightFlashes[payload]? with
  | none => throw "T_LightFlash: bad payload"
  | some flash0 =>
    let count := flash0.count - 1
    if count != 0 then
      pure { gs0 with lightFlashes := setArr gs0.lightFlashes payload { flash0 with count } }
    else
      match gs0.sectors[flash0.sector.toNat]? with
      | none => throw "T_LightFlash: bad sector"
      | some sec =>
        let (flash, sec', rng) :=
          if sec.lightlevel == flash0.maxlight then
            let (r, rng) := pRandom gs0.rng
            let flash := { flash0 with count := (r &&& flash0.mintime) + 1 }
            let sec' := { sec with lightlevel := flash0.minlight }
            (flash, sec', rng)
          else
            let (r, rng) := pRandom gs0.rng
            let flash := { flash0 with count := (r &&& flash0.maxtime) + 1 }
            let sec' := { sec with lightlevel := flash0.maxlight }
            (flash, sec', rng)
        pure {
          gs0 with
          lightFlashes := setArr gs0.lightFlashes payload flash
          sectors := setArr gs0.sectors flash0.sector.toNat sec'
          rng
        }

/-- `T_StrobeFlash`. -/
def strobeFlashThinker (gs0 : GameState) (payload : Nat) : Except String GameState := do
  match gs0.strobes[payload]? with
  | none => throw "T_StrobeFlash: bad payload"
  | some flash0 =>
    let count := flash0.count - 1
    if count != 0 then
      pure { gs0 with strobes := setArr gs0.strobes payload { flash0 with count } }
    else
      match gs0.sectors[flash0.sector.toNat]? with
      | none => throw "T_StrobeFlash: bad sector"
      | some sec =>
        let (flash, sec') :=
          if sec.lightlevel == flash0.minlight then
            ({ flash0 with count := flash0.brighttime },
             { sec with lightlevel := flash0.maxlight })
          else
            ({ flash0 with count := flash0.darktime },
             { sec with lightlevel := flash0.minlight })
        pure {
          gs0 with
          strobes := setArr gs0.strobes payload flash
          sectors := setArr gs0.sectors flash0.sector.toNat sec'
        }

/-- `T_Glow`. -/
def glowThinker (gs0 : GameState) (payload : Nat) : Except String GameState := do
  let GLOWSPEED : Int32 := 8
  match gs0.glows[payload]? with
  | none => throw "T_Glow: bad payload"
  | some g0 =>
    match gs0.sectors[g0.sector.toNat]? with
    | none => throw "T_Glow: bad sector"
    | some sec0 =>
      let mut g := g0
      let mut sec := sec0
      if g.direction == -1 then
        sec := { sec with lightlevel := sec.lightlevel - GLOWSPEED }
        if sec.lightlevel <= g.minlight then
          sec := { sec with lightlevel := sec.lightlevel + GLOWSPEED }
          g := { g with direction := 1 }
      else if g.direction == 1 then
        sec := { sec with lightlevel := sec.lightlevel + GLOWSPEED }
        if sec.lightlevel >= g.maxlight then
          sec := { sec with lightlevel := sec.lightlevel - GLOWSPEED }
          g := { g with direction := -1 }
      else
        throw s!"T_Glow: bad direction {g.direction}"
      pure {
        gs0 with
        glows := setArr gs0.glows payload g
        sectors := setArr gs0.sectors g0.sector.toNat sec
      }

/-- Dispatch one thinker by `THF_*`. -/
def runOneThinker (gs : GameState) (th : Thinker) : Except String GameState := do
  if th.func == THF_MOBJ then
    mobjThinker gs th.payload.toNat
  else if th.func == THF_LIGHTFLASH then
    lightFlashThinker gs th.payload.toNat
  else if th.func == THF_STROBEFLASH then
    strobeFlashThinker gs th.payload.toNat
  else if th.func == THF_GLOW then
    glowThinker gs th.payload.toNat
  else if th.func == THF_VERTICALDOOR then
    verticalDoorThinker gs th.payload.toNat
  else if th.func == THF_PLATRAISE then
    platRaiseThinker gs th.payload.toNat
  else if th.func == THF_MOVEFLOOR then
    floorMoveThinker gs th.payload.toNat
  else
    throw s!"P_RunThinkers: unimplemented THF {th.func}"

/--
`P_RunThinkers` — index walk; thinkers appended during the walk are visited
(C linked-list semantics with tail append). `THF_REMOVED` entries are erased
in place (C lazy free on encounter).
-/
def runThinkers (gs0 : GameState) : Except String GameState := do
  let mut gs := gs0
  let mut i : Nat := 0
  while i < gs.thinkers.size do
    match gs.thinkers[i]? with
    | none => throw "P_RunThinkers: missing thinker"
    | some th =>
      if th.func == THF_REMOVED then
        let thinkers :=
          gs.thinkers.extract 0 i ++ gs.thinkers.extract (i + 1) gs.thinkers.size
        gs := { gs with thinkers }
        -- do not advance i — next thinker shifted into place
      else
        gs ← runOneThinker gs th
        i := i + 1
  pure gs

end Doom.Playsim.Think
