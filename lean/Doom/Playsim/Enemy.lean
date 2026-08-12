import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Map
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Random
import Doom.Playsim.Sight
import Doom.Playsim.Sound
import Doom.Playsim.Tables

/-!
# Doom.Playsim.Enemy

`p_enemy.c` open subset for DEMO1 wake + chase (P2c-iii): look/chase helpers,
`A_Look` seeyou, `A_Chase`, plus `P_SetMobjState` / action dispatch (avoids a
Think↔Enemy import cycle).
-/

namespace Doom.Playsim.Enemy

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Map
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Random
open Doom.Playsim.Sight
open Doom.Playsim.Sound
open Doom.Playsim.Tables

/-- `p_local.h` `MELEERANGE`. -/
def MELEERANGE : Int32 := 64 * FRACUNIT

/-- `dirtype_t`. -/
def DI_EAST : Int32 := 0
def DI_NORTHEAST : Int32 := 1
def DI_NORTH : Int32 := 2
def DI_NORTHWEST : Int32 := 3
def DI_WEST : Int32 := 4
def DI_SOUTHWEST : Int32 := 5
def DI_SOUTH : Int32 := 6
def DI_SOUTHEAST : Int32 := 7
def DI_NODIR : Int32 := 8

/-- `mobjtype_t` ordinals used by missile-range early-outs. -/
def MT_VILE : Int32 := 3
def MT_UNDEAD : Int32 := 5
def MT_SKULL : Int32 := 18
def MT_SPIDER : Int32 := 19
def MT_CYBORG : Int32 := 21

/-- `opposite[]` (`p_enemy.c`). -/
def opposite : Array Int32 := #[
  DI_WEST, DI_SOUTHWEST, DI_SOUTH, DI_SOUTHEAST,
  DI_EAST, DI_NORTHEAST, DI_NORTH, DI_NORTHWEST, DI_NODIR
]

/-- `diags[]` (`p_enemy.c`). -/
def diags : Array Int32 := #[
  DI_NORTHWEST, DI_NORTHEAST, DI_SOUTHWEST, DI_SOUTHEAST
]

/-- `xspeed[]` / `yspeed[]` (`p_enemy.c`). -/
def xspeed : Array Int32 := #[
  FRACUNIT, 47000, 0, -47000, -FRACUNIT, -47000, 0, 47000
]
def yspeed : Array Int32 := #[
  0, 47000, FRACUNIT, 47000, 0, -47000, -FRACUNIT, -47000
]

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

private def setMo (gs : GameState) (i : Nat) (mo : Mobj) : GameState :=
  { gs with mobjs := setArr gs.mobjs i mo }

/-- `P_AproxDistance`. -/
def aproxDistance (dx0 dy0 : Int32) : Int32 :=
  let dx := wabs dx0
  let dy := wabs dy0
  if dx < dy then dx + dy - (dx >>> 1)
  else dx + dy - (dy >>> 1)

/-- Resolve `mobjinfo` for a live mobj. -/
private def infoOf (mo : Mobj) : Except String MobjInfo := do
  if mo.typeId < 0 then throw "enemy: bad type"
  match mobjinfo[mo.typeId.toNatClampNeg]? with
  | none => throw "enemy: missing mobjinfo"
  | some info => pure info

/-- `P_Move` — tryMove + floorz snap; loud-error on spechit/door/MF_FLOAT. -/
def pMove (gs0 : GameState) (mobjIdx : Nat) : Except String (GameState × Bool) := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_Move: bad mobj"
  | some actor0 =>
    if actor0.movedir == DI_NODIR then
      return (gs0, false)
    if actor0.movedir < 0 || actor0.movedir >= 8 then
      throw "P_Move: Weird actor->movedir!"
    let info ← infoOf actor0
    let dir := actor0.movedir.toNatClampNeg
    let xs := xspeed.getD dir 0
    let ys := yspeed.getD dir 0
    let tryx := actor0.x + info.speed * xs
    let tryy := actor0.y + info.speed * ys
    let (gs1, ok) ← tryMove gs0 mobjIdx tryx tryy
    if ok then
      match gs1.mobjs[mobjIdx]? with
      | none => throw "P_Move: lost after tryMove"
      | some actor =>
        let mut mo := { actor with flags := actor.flags &&& (~~~MF_INFLOAT) }
        if (mo.flags &&& MF_FLOAT) == 0 then
          mo := { mo with z := mo.floorz }
        pure (setMo gs1 mobjIdx mo, true)
    else
      -- Re-check position to inspect spechit / floatok (same globals C keeps).
      let (gs2, scr, posOk) ← checkPosition gs1 mobjIdx tryx tryy
      match gs2.mobjs[mobjIdx]? with
      | none => throw "P_Move: lost after blocked"
      | some actor =>
        let floatok :=
          if !posOk then false
          else if (actor.flags &&& MF_NOCLIP) != 0 then false
          else if scr.tmceilingz - scr.tmfloorz < actor.height then false
          else true
        if (actor.flags &&& MF_FLOAT) != 0 && floatok then
          throw "P_Move: MF_FLOAT height adjust not implemented"
        if scr.spechit.size != 0 then
          throw "P_Move: spechit/door special path not implemented"
        pure (gs2, false)

/-- `P_TryWalk`. -/
def tryWalk (gs0 : GameState) (mobjIdx : Nat) : Except String (GameState × Bool) := do
  let (gs1, ok) ← pMove gs0 mobjIdx
  if !ok then
    return (gs1, false)
  match gs1.mobjs[mobjIdx]? with
  | none => throw "P_TryWalk: lost mobj"
  | some actor =>
    let (r, rng) := pRandom gs1.rng
    let mo := { actor with movecount := r &&& 15 }
    pure ({ setMo gs1 mobjIdx mo with rng }, true)

/-- Set `movedir` and `P_TryWalk`; returns `(gs, true)` on success. -/
private def tryChaseDir (gs0 : GameState) (mobjIdx : Nat) (dir : Int32) :
    Except String (GameState × Bool) := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_NewChaseDir: lost before try"
  | some actor =>
    let gs := setMo gs0 mobjIdx { actor with movedir := dir }
    tryWalk gs mobjIdx

/-- `P_NewChaseDir`. -/
def newChaseDir (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_NewChaseDir: bad mobj"
  | some actor0 =>
    if actor0.target < 0 then
      throw "P_NewChaseDir: called with no target"
    match gs0.mobjs[actor0.target.toNatClampNeg]? with
    | none => throw "P_NewChaseDir: bad target"
    | some targ =>
      let olddir := actor0.movedir
      let turnaround := opposite.getD olddir.toNatClampNeg DI_NODIR
      let deltax := targ.x - actor0.x
      let deltay := targ.y - actor0.y
      let d1 : Int32 :=
        if deltax > 10 * FRACUNIT then DI_EAST
        else if deltax < -10 * FRACUNIT then DI_WEST
        else DI_NODIR
      let d2 : Int32 :=
        if deltay < -10 * FRACUNIT then DI_SOUTH
        else if deltay > 10 * FRACUNIT then DI_NORTH
        else DI_NODIR
      let mut gs := gs0
      -- try direct route
      if d1 != DI_NODIR && d2 != DI_NODIR then
        let diagIdx :=
          (if deltay < 0 then (2 : Nat) else 0) + (if deltax > 0 then 1 else 0)
        let md := diags.getD diagIdx DI_NODIR
        if md != turnaround then
          let (gs1, ok) ← tryChaseDir gs mobjIdx md
          gs := gs1
          if ok then return gs
      let mut dd1 := d1
      let mut dd2 := d2
      let (rSwap, rng) := pRandom gs.rng
      gs := { gs with rng }
      if rSwap > 200 || wabs deltay > wabs deltax then
        let t := dd1
        dd1 := dd2
        dd2 := t
      if dd1 == turnaround then dd1 := DI_NODIR
      if dd2 == turnaround then dd2 := DI_NODIR
      if dd1 != DI_NODIR then
        let (gs1, ok) ← tryChaseDir gs mobjIdx dd1
        gs := gs1
        if ok then return gs
      if dd2 != DI_NODIR then
        let (gs1, ok) ← tryChaseDir gs mobjIdx dd2
        gs := gs1
        if ok then return gs
      if olddir != DI_NODIR then
        let (gs1, ok) ← tryChaseDir gs mobjIdx olddir
        gs := gs1
        if ok then return gs
      let (rDir, rng) := pRandom gs.rng
      gs := { gs with rng }
      if (rDir &&& 1) != 0 then
        let mut tdir := DI_EAST
        while tdir <= DI_SOUTHEAST do
          if tdir != turnaround then
            let (gs1, ok) ← tryChaseDir gs mobjIdx tdir
            gs := gs1
            if ok then return gs
          tdir := tdir + 1
      else
        let mut tdir := DI_SOUTHEAST
        while tdir != (-1 : Int32) do
          if tdir != turnaround then
            let (gs1, ok) ← tryChaseDir gs mobjIdx tdir
            gs := gs1
            if ok then return gs
          tdir := tdir - 1
      if turnaround != DI_NODIR then
        let (gs1, ok) ← tryChaseDir gs mobjIdx turnaround
        gs := gs1
        if ok then return gs
      match gs.mobjs[mobjIdx]? with
      | none => throw "P_NewChaseDir: lost at NODIR"
      | some actor =>
        pure (setMo gs mobjIdx { actor with movedir := DI_NODIR })

/-- `P_CheckMeleeRange` — DEMO1 uses post-1.2 radius formula. -/
def checkMeleeRange (gs0 : GameState) (mobjIdx : Nat) :
    Except String (GameState × Bool) := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_CheckMeleeRange: bad mobj"
  | some actor =>
    if actor.target < 0 then
      return (gs0, false)
    match gs0.mobjs[actor.target.toNatClampNeg]? with
    | none => throw "P_CheckMeleeRange: bad target"
    | some pl =>
      let dist := aproxDistance (pl.x - actor.x) (pl.y - actor.y)
      let info ← infoOf pl
      let range := MELEERANGE - 20 * FRACUNIT + info.radius
      if dist >= range then
        return (gs0, false)
      let (gs1, visible) ← checkSight gs0 actor pl
      pure (gs1, visible)

/-- `P_CheckMissileRange` with C early-outs; draws `P_Random` when reached. -/
def checkMissileRange (gs0 : GameState) (mobjIdx : Nat) :
    Except String (GameState × Bool) := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_CheckMissileRange: bad mobj"
  | some actor0 =>
    if actor0.target < 0 then
      return (gs0, false)
    match gs0.mobjs[actor0.target.toNatClampNeg]? with
    | none => throw "P_CheckMissileRange: bad target"
    | some targ =>
      let (gs1, visible) ← checkSight gs0 actor0 targ
      if !visible then
        return (gs1, false)
      match gs1.mobjs[mobjIdx]? with
      | none => throw "P_CheckMissileRange: lost actor"
      | some actor =>
        let mut gs := gs1
        let mut mo := actor
        if (mo.flags &&& MF_JUSTHIT) != 0 then
          mo := { mo with flags := mo.flags &&& (~~~MF_JUSTHIT) }
          gs := setMo gs mobjIdx mo
          return (gs, true)
        if mo.reactiontime != 0 then
          return (gs, false)
        let mut dist :=
          aproxDistance (mo.x - targ.x) (mo.y - targ.y) - 64 * FRACUNIT
        let info ← infoOf mo
        if info.meleestate == 0 then
          dist := dist - 128 * FRACUNIT
        dist := dist >>> 16  -- FRACBITS
        if mo.typeId == MT_VILE then
          if dist > 14 * 64 then return (gs, false)
        if mo.typeId == MT_UNDEAD then
          if dist < 196 then return (gs, false)
          dist := dist >>> 1
        if mo.typeId == MT_CYBORG || mo.typeId == MT_SPIDER || mo.typeId == MT_SKULL then
          dist := dist >>> 1
        if dist > 200 then dist := 200
        if mo.typeId == MT_CYBORG && dist > 160 then dist := 160
        let (r, rng) := pRandom gs.rng
        gs := { gs with rng }
        if r < dist then
          pure (gs, false)
        else
          pure (gs, true)

/-- `P_LookForPlayers`. -/
def lookForPlayers (gs0 : GameState) (mobjIdx : Nat) (allaround : Bool) :
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
        -- C: `if (c++ == 2 || …)` — compare then increment; allows two examines.
        let prevC := c
        c := c + 1
        if prevC == 2 || look == stop then
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
                let (gs1, visible) ← checkSight gs actor pmo
                gs := gs1
                match gs.mobjs[mobjIdx]? with
                | none => throw "P_LookForPlayers: actor lost after sight"
                | some a =>
                  actor := a
                  if !visible then
                    actor := { actor with lastlook := (look + 1) &&& 3 }
                    gs := setMo gs mobjIdx actor
                  else
                    let mut acquire := true
                    if !allaround then
                      let an :=
                        pointToAngle2 actor.x actor.y pmo.x pmo.y - actor.angle
                      if an > ANG90 && an < ANG270 then
                        let dist :=
                          aproxDistance (pmo.x - actor.x) (pmo.y - actor.y)
                        if dist > MELEERANGE then
                          acquire := false
                    if !acquire then
                      actor := { actor with lastlook := (look + 1) &&& 3 }
                      gs := setMo gs mobjIdx actor
                    else
                      actor := { actor with target := player.mo }
                      gs := setMo gs mobjIdx actor
                      found := true
                      done := true
    pure (gs, found)

-- Fuel-bounded set-state / action / look / chase cycle (no partial).
mutual
def setMobjStateFuel (gs0 : GameState) (mobjIdx : Nat) (state0 : UInt32) (fuel : Nat) :
    Except String (GameState × Bool) :=
  match fuel with
  | 0 => throw "P_SetMobjState: fuel exhausted"
  | fuel' + 1 => do
    if state0 == 0 then
      throw "P_SetMobjState: S_NULL remove not implemented"
    match states[state0.toNat]? with
    | none => throw s!"P_SetMobjState: bad state {state0}"
    | some st =>
      match gs0.mobjs[mobjIdx]? with
      | none => throw "P_SetMobjState: bad mobj"
      | some mo =>
        let mo := {
          mo with
          state := state0
          tics := st.tics
          sprite := st.sprite
          frame := st.frame
        }
        let gs := setMo gs0 mobjIdx mo
        let gs ← runMobjActionFuel gs mobjIdx st.action fuel'
        match gs.mobjs[mobjIdx]? with
        | none => throw "P_SetMobjState: mobj lost after action"
        | some mo2 =>
          if mo2.tics != 0 then
            pure (gs, true)
          else
            setMobjStateFuel gs mobjIdx st.nextstate fuel'

def runMobjActionFuel (gs0 : GameState) (mobjIdx : Nat) (action : ActionId) (fuel : Nat) :
    Except String GameState :=
  match fuel with
  | 0 => throw "runMobjAction: fuel exhausted"
  | fuel' + 1 =>
    if action == actionNull then
      pure gs0
    else if action == action_A_Look then
      aLookFuel gs0 mobjIdx fuel'
    else if action == action_A_Chase then
      aChaseFuel gs0 mobjIdx fuel'
    else
      throw s!"P_MobjThinker/SetMobjState: unimplemented action {action}"

def aLookFuel (gs0 : GameState) (mobjIdx : Nat) (fuel : Nat) : Except String GameState :=
  match fuel with
  | 0 => throw "A_Look: fuel exhausted"
  | fuel' + 1 => do
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
              let (gs1, visible) ← checkSight gs actor2 targ
              gs := gs1
              if visible then seeyou := true
            else
              seeyou := true
      if !seeyou then
        let (gs1, found) ← lookForPlayers gs mobjIdx false
        gs := gs1
        if !found then
          return gs
        seeyou := true
      let _ := seeyou
      match gs.mobjs[mobjIdx]? with
      | none => throw "A_Look: lost before seeyou"
      | some actorS =>
        let info ← infoOf actorS
        if info.seesound != 0 then
          let seesound := info.seesound
          let posit1 : Int32 := Int32.ofNat sfx_posit1
          let posit2 : Int32 := Int32.ofNat sfx_posit2
          let posit3 : Int32 := Int32.ofNat sfx_posit3
          let bgsit1 : Int32 := Int32.ofNat sfx_bgsit1
          let bgsit2 : Int32 := Int32.ofNat sfx_bgsit2
          let (sound, rng) :=
            if seesound == posit1 || seesound == posit2 || seesound == posit3 then
              let (r, rng) := pRandom gs.rng
              (posit1 + r % 3, rng)
            else if seesound == bgsit1 || seesound == bgsit2 then
              let (r, rng) := pRandom gs.rng
              (bgsit1 + r % 2, rng)
            else
              (seesound, gs.rng)
          gs := { gs with rng }
          gs := { gs with rng := startSoundPitchRng gs.rng sound.toNatClampNeg }
        let (gs2, _) ← setMobjStateFuel gs mobjIdx info.seestate fuel'
        pure gs2

def aChaseFuel (gs0 : GameState) (mobjIdx : Nat) (fuel : Nat) : Except String GameState :=
  match fuel with
  | 0 => throw "A_Chase: fuel exhausted"
  | fuel' + 1 => do
    match gs0.mobjs[mobjIdx]? with
    | none => throw "A_Chase: bad mobj"
    | some actor0 =>
      let mut gs := gs0
      let mut actor := actor0
      if actor.reactiontime != 0 then
        actor := { actor with reactiontime := actor.reactiontime - 1 }
        gs := setMo gs mobjIdx actor
      if actor.threshold != 0 then
        if actor.target < 0 then
          actor := { actor with threshold := 0 }
        else
          match gs.mobjs[actor.target.toNatClampNeg]? with
          | none => actor := { actor with threshold := 0 }
          | some t =>
            if t.health <= 0 then
              actor := { actor with threshold := 0 }
            else
              actor := { actor with threshold := actor.threshold - 1 }
        gs := setMo gs mobjIdx actor
      if actor.movedir < 8 then
        let ang := actor.angle &&& ((7 : UInt32) <<< 29)
        let delta := (ang - (actor.movedir.toUInt32 <<< 29)).toInt32
        let ang :=
          if delta > 0 then ang - ANG90 / 2
          else if delta < 0 then ang + ANG90 / 2
          else ang
        actor := { actor with angle := ang }
        gs := setMo gs mobjIdx actor
      let needNewTarget :=
        match
          if actor.target < 0 then none
          else gs.mobjs[actor.target.toNatClampNeg]?
        with
        | none => true
        | some t => (t.flags &&& MF_SHOOTABLE) == 0
      if needNewTarget then
        let (gs1, found) ← lookForPlayers gs mobjIdx true
        gs := gs1
        if found then
          return gs
        match gs.mobjs[mobjIdx]? with
        | none => throw "A_Chase: lost on spawnstate"
        | some a =>
          let info ← infoOf a
          let (gs2, _) ← setMobjStateFuel gs mobjIdx info.spawnstate fuel'
          return gs2
      match gs.mobjs[mobjIdx]? with
      | none => throw "A_Chase: lost"
      | some a => actor := a
      if (actor.flags &&& MF_JUSTATTACKED) != 0 then
        actor := { actor with flags := actor.flags &&& (~~~MF_JUSTATTACKED) }
        gs := setMo gs mobjIdx actor
        if gs.gameskill != sk_nightmare then
          gs ← newChaseDir gs mobjIdx
        return gs
      let info ← infoOf actor
      if info.meleestate != 0 then
        let (gs1, inMelee) ← checkMeleeRange gs mobjIdx
        gs := gs1
        if inMelee then
          throw "A_Chase: meleestate not implemented"
      if info.missilestate != 0 then
        let skipMissile :=
          gs.gameskill < sk_nightmare && actor.movecount != 0
        if !skipMissile then
          let (gs1, canFire) ← checkMissileRange gs mobjIdx
          gs := gs1
          if canFire then
            throw "A_Chase: missilestate not implemented"
      match gs.mobjs[mobjIdx]? with
      | none => throw "A_Chase: lost before move"
      | some a => actor := a
      let movecount := actor.movecount - 1
      actor := { actor with movecount }
      gs := setMo gs mobjIdx actor
      if movecount < 0 then
        gs ← newChaseDir gs mobjIdx
      else
        let (gs1, moved) ← pMove gs mobjIdx
        gs := gs1
        if !moved then
          gs ← newChaseDir gs mobjIdx
      match gs.mobjs[mobjIdx]? with
      | none => throw "A_Chase: lost before activesound"
      | some a =>
        let info ← infoOf a
        if info.activesound != 0 then
          let (r, rng) := pRandom gs.rng
          let mut gs2 := { gs with rng }
          if r < 3 then
            gs2 := {
              gs2 with
              rng := startSoundPitchRng gs2.rng info.activesound.toNatClampNeg
            }
          pure gs2
        else
          pure gs
end

/-- Public `P_SetMobjState` wrapper. -/
def setMobjState (gs0 : GameState) (mobjIdx : Nat) (state0 : UInt32) :
    Except String (GameState × Bool) :=
  setMobjStateFuel gs0 mobjIdx state0 1000000

def runMobjAction (gs0 : GameState) (mobjIdx : Nat) (action : ActionId) :
    Except String GameState :=
  runMobjActionFuel gs0 mobjIdx action 1000000

def aLook (gs0 : GameState) (mobjIdx : Nat) : Except String GameState :=
  aLookFuel gs0 mobjIdx 1000000

def aChase (gs0 : GameState) (mobjIdx : Nat) : Except String GameState :=
  aChaseFuel gs0 mobjIdx 1000000

end Doom.Playsim.Enemy
