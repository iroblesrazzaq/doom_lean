import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Random
import Doom.Playsim.Sight
import Doom.Playsim.Thinker

/-!
# Doom.Playsim.Think

`P_SetMobjState`, `P_MobjThinker`, `A_Look`, light thinkers (`p_lights.c`).
-/

namespace Doom.Playsim.Think

open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Random
open Doom.Playsim.Sight
open Doom.Playsim.Thinker

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

private def setMo (gs : GameState) (i : Nat) (mo : Mobj) : GameState :=
  { gs with mobjs := setArr gs.mobjs i mo }

/-- `P_LookForPlayers` — allaround = false path; draws no `P_Random`. -/
def lookForPlayers (gs0 : GameState) (mobjIdx : Nat) (_allaround : Bool) :
    Except String (GameState × Bool) := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_LookForPlayers: bad mobj"
  | some actor0 =>
    let mut gs := gs0
    let mut actor := actor0
    let mut c : Int32 := 0
    let stop : Int32 := (actor.lastlook - 1) &&& 3
    let mut done := false
    let mut found := false
    let mut guard : Nat := 0
    while !done && guard < 8 do
      guard := guard + 1
      let look := actor.lastlook
      let lookNat := look.toNatClampNeg
      let inGame := match gs.playeringame[lookNat]? with | some true => true | _ => false
      if !inGame then
        actor := { actor with lastlook := (look + 1) &&& 3 }
        gs := setMo gs mobjIdx actor
      else
        c := c + 1
        if c == 2 || look == stop then
          done := true
          found := false
        else
          match gs.players[lookNat]? with
          | none =>
            actor := { actor with lastlook := (look + 1) &&& 3 }
            gs := setMo gs mobjIdx actor
          | some player =>
            if player.health <= 0 then
              actor := { actor with lastlook := (look + 1) &&& 3 }
              gs := setMo gs mobjIdx actor
            else
              match gs.mobjs[player.mo.toNatClampNeg]? with
              | none => throw "P_LookForPlayers: player mo missing"
              | some pmo =>
                let visible ← checkSightRejectOnly gs actor pmo
                if !visible then
                  actor := { actor with lastlook := (look + 1) &&& 3 }
                  gs := setMo gs mobjIdx actor
                else
                  -- REJECT-only never returns true (BSP soft-split errors first).
                  actor := { actor with target := player.mo }
                  gs := setMo gs mobjIdx actor
                  found := true
                  done := true
    pure (gs, found)

/-- `A_Look`. -/
def aLook (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_Look: bad mobj"
  | some actor0 =>
    let mut gs := setMo gs0 mobjIdx { actor0 with threshold := 0 }
    let actor ←
      match gs.mobjs[mobjIdx]? with
      | some a => pure a
      | none => throw "A_Look: lost mobj"
    let secIdx ←
      match gs.level.subsectors[actor.subsector.toNat]? with
      | some ss => pure ss.sector.toNat
      | none => throw "A_Look: bad subsector"
    let soundtarget ←
      match gs.sectors[secIdx]? with
      | some sec => pure sec.soundtarget
      | none => throw "A_Look: bad sector"
    let mut seeyou := false
    if soundtarget >= 0 then
      match gs.mobjs[soundtarget.toNatClampNeg]? with
      | none => pure ()
      | some targ =>
        if (targ.flags &&& MF_SHOOTABLE) != 0 then
          gs := setMo gs mobjIdx { actor with target := soundtarget }
          let actor2 ← match gs.mobjs[mobjIdx]? with | some a => pure a | none => throw "A_Look"
          if (actor2.flags &&& MF_AMBUSH) != 0 then
            let visible ← checkSightRejectOnly gs actor2 targ
            if visible then seeyou := true
          else
            seeyou := true
    if !seeyou then
      let (gs1, found) ← lookForPlayers gs mobjIdx false
      gs := gs1
      if !found then
        return gs
      seeyou := true
    -- seeyou: seesound + seestate — waking at tic 0 would need sound RNG; loud stop
    throw "A_Look: player sighted (wake path) — unexpected at DEMO1 tic 0"

def runMobjAction (gs0 : GameState) (mobjIdx : Nat) (action : ActionId) :
    Except String GameState := do
  if action == actionNull then
    pure gs0
  else if action == action_A_Look then
    aLook gs0 mobjIdx
  else
    throw s!"P_MobjThinker/SetMobjState: unimplemented action {action}"

/-- `P_SetMobjState`. Returns `false` if removed (not expected at tic 0). -/
def setMobjState (gs0 : GameState) (mobjIdx : Nat) (state0 : UInt32) :
    Except String (GameState × Bool) := do
  let mut gs := gs0
  let mut state := state0
  let mut guard : Nat := 0
  while guard < 1000000 do
    guard := guard + 1
    if state == 0 then
      throw "P_SetMobjState: S_NULL remove not implemented"
    match states[state.toNat]? with
    | none => throw s!"P_SetMobjState: bad state {state}"
    | some st =>
      match gs.mobjs[mobjIdx]? with
      | none => throw "P_SetMobjState: bad mobj"
      | some mo =>
        let mo := {
          mo with
          state := state
          tics := st.tics
          sprite := st.sprite
          frame := st.frame
        }
        gs := setMo gs mobjIdx mo
        gs ← runMobjAction gs mobjIdx st.action
        match gs.mobjs[mobjIdx]? with
        | none => throw "P_SetMobjState: mobj lost after action"
        | some mo2 =>
          if mo2.tics != 0 then
            return (gs, true)
          state := st.nextstate
  throw "P_SetMobjState: cycle limit"

/-- `P_MobjThinker` — zero-momentum / on-floor skip; cycle states. -/
def mobjThinker (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_MobjThinker: bad mobj"
  | some mo =>
    if mo.momx != 0 || mo.momy != 0 then
      throw "P_MobjThinker: XY movement not implemented"
    if mo.z != mo.floorz || mo.momz != 0 then
      throw "P_MobjThinker: Z movement not implemented"
    if mo.tics != -1 then
      let tics := mo.tics - 1
      let gs := setMo gs0 mobjIdx { mo with tics }
      if tics == 0 then
        match gs.mobjs[mobjIdx]? with
        | none => throw "P_MobjThinker: lost"
        | some mo2 =>
          match states[mo2.state.toNat]? with
          | none => throw "P_MobjThinker: bad state"
          | some st =>
            let (gs2, _) ← setMobjState gs mobjIdx st.nextstate
            pure gs2
      else
        pure gs
    else
      -- nightmare respawn path unused
      pure gs0

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
  else if th.func == THF_REMOVED then
    pure gs
  else
    throw s!"P_RunThinkers: unimplemented THF {th.func}"

/--
`P_RunThinkers` — index walk; thinkers appended during the walk are visited
(C linked-list semantics with tail append).
-/
def runThinkers (gs0 : GameState) : Except String GameState := do
  let mut gs := gs0
  let mut i : Nat := 0
  while i < gs.thinkers.size do
    match gs.thinkers[i]? with
    | none => throw "P_RunThinkers: missing thinker"
    | some th =>
      gs ← runOneThinker gs th
    i := i + 1
  pure gs

end Doom.Playsim.Think
