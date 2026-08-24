import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Hitscan
import Doom.Playsim.Info
import Doom.Playsim.Inter
import Doom.Playsim.Level
import Doom.Playsim.Map
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Psprite
import Doom.Playsim.Random
import Doom.Playsim.SargAttack
import Doom.Playsim.Sight
import Doom.Playsim.Sound
import Doom.Playsim.Spawn
import Doom.Playsim.Spec
import Doom.Playsim.Tables

/-!
# Doom.Playsim.Enemy

`p_enemy.c` / `p_inter.c` open subset for DEMO1 wake + chase through first
demon melee (P2c-xix): look/chase helpers, `A_PosAttack` / `A_SPosAttack`, `A_Scream` / `A_XScream` /
`A_Fall`, `A_Explode` → `P_RadiusAttack`, `A_TroopAttack` / `P_ExplodeMissile`,
`A_SargAttack`, `P_DamageMobj` / `P_KillMobj`, plus `P_SetMobjState`
(S_NULL → `Inter.removeMobj`) / action dispatch (avoids a Think↔Enemy cycle).
Hitscan must not import this module.
-/

namespace Doom.Playsim.Enemy

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Inter
open Doom.Playsim.Level
open Doom.Playsim.Map
open Doom.Playsim.MapUtil
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Psprite
open Doom.Playsim.Random
open Doom.Playsim.Sight
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Tables

/-- Re-export `P_AproxDistance` from MapUtil (canonical `p_maputl` home). -/
def aproxDistance := MapUtil.aproxDistance

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

/-- `mobjtype_t` ordinals used by missile-range early-outs and kill drops. -/
def MT_POSSESSED : Int32 := 1
def MT_SHOTGUY : Int32 := 2
def MT_VILE : Int32 := 3
def MT_UNDEAD : Int32 := 5
def MT_CHAINGUY : Int32 := 10
def MT_SKULL : Int32 := 18
def MT_SPIDER : Int32 := 19
def MT_CYBORG : Int32 := 21
def MT_WOLFSS : Int32 := 23
def MT_CLIP : Int32 := 63
def MT_CHAINGUN : Int32 := 73
def MT_SHOTGUN : Int32 := 77

/-- `p_local.h` `BASETHRESHOLD`. -/
def BASETHRESHOLD : Int32 := 100
/-- `CF_GODMODE` (`doomdef.h`). -/
def CF_GODMODE : Int32 := 2

/-- `P_SetMobjState` callback so damage/kill can live outside the mutual block. -/
abbrev SetMobjStateFn :=
  GameState → Nat → UInt32 → Except String (GameState × Bool)

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

private def setPlayer (gs : GameState) (i : Nat) (p : Player) : GameState :=
  { gs with players := setArr gs.players i p }

/-- Resolve `mobjinfo` for a live mobj. -/
private def infoOf (mo : Mobj) : Except String MobjInfo := do
  if mo.typeId < 0 then throw "enemy: bad type"
  match mobjinfo[mo.typeId.toNatClampNeg]? with
  | none => throw "enemy: missing mobjinfo"
  | some info => pure info

/-- `P_Move` — tryMove + floorz snap; blocked spechit walks `P_UseSpecialLine`. -/
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
    let (gs1, scr, ok) ← tryMove gs0 mobjIdx tryx tryy
      (crossSpecial := some Spec.crossSpecialLine)
    if ok then
      match gs1.mobjs[mobjIdx]? with
      | none => throw "P_Move: lost after tryMove"
      | some actor =>
        let mut mo := { actor with flags := actor.flags &&& (~~~MF_INFLOAT) }
        if (mo.flags &&& MF_FLOAT) == 0 then
          mo := { mo with z := mo.floorz }
        pure (setMo gs1 mobjIdx mo, true)
    else
      match gs1.mobjs[mobjIdx]? with
      | none => throw "P_Move: lost after blocked"
      | some actor =>
        if (actor.flags &&& MF_FLOAT) != 0 && scr.floatok then
          throw "P_Move: MF_FLOAT height adjust not implemented"
        if scr.spechit.size == 0 then
          return (gs1, false)
        let mut gs := setMo gs1 mobjIdx { actor with movedir := DI_NODIR }
        let mut good := false
        let mut si := scr.spechit.size
        while si > 0 do
          si := si - 1
          match scr.spechit[si]? with
          | none => throw "P_Move: bad spechit"
          | some lineIdx =>
            let (gs2, used) ← useSpecialLine gs mobjIdx lineIdx 0
            gs := gs2
            if used then good := true
        pure (gs, good)

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

/-- `A_FaceTarget`. `MF_SHADOW` jitter is implemented but unused on DEMO1. -/
def aFaceTarget (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_FaceTarget: bad mobj"
  | some actor =>
    if actor.target < 0 then
      return gs0
    match gs0.mobjs[actor.target.toNatClampNeg]? with
    | none => throw "A_FaceTarget: bad target"
    | some targ =>
      let mo0 := { actor with flags := actor.flags &&& (~~~MF_AMBUSH) }
      let ang := pointToAngle2 mo0.x mo0.y targ.x targ.y
      let mut gs := setMo gs0 mobjIdx { mo0 with angle := ang }
      if (targ.flags &&& MF_SHADOW) != 0 then
        let (d, rng) := pSubRandom gs.rng
        match gs.mobjs[mobjIdx]? with
        | none => throw "A_FaceTarget: lost on shadow"
        | some mo =>
          pure { setMo gs mobjIdx { mo with angle := mo.angle + (d.toUInt32 <<< 21) } with rng }
      else
        pure gs

/-- `A_Pain`. -/
def aPain (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_Pain: bad mobj"
  | some actor =>
    let info ← infoOf actor
    if info.painsound != 0 then
      let rng := startSoundPitchRngMaybe gs0.rng info.painsound.toNatClampNeg
        (originAudible gs0 mobjIdx)
      pure { gs0 with rng }
    else
      pure gs0

/-- `A_Scream`. -/
def aScream (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_Scream: bad mobj"
  | some actor =>
    let info ← infoOf actor
    if info.deathsound == 0 then
      return gs0
    let podth1 : Int32 := Int32.ofNat sfx_podth1
    let podth2 : Int32 := Int32.ofNat sfx_podth2
    let podth3 : Int32 := Int32.ofNat sfx_podth3
    let bgdth1 : Int32 := Int32.ofNat sfx_bgdth1
    let bgdth2 : Int32 := Int32.ofNat sfx_bgdth2
    let deathsound := info.deathsound
    let (sound, rng) :=
      if deathsound == podth1 || deathsound == podth2 || deathsound == podth3 then
        let (r, rng) := pRandom gs0.rng
        (podth1 + r % 3, rng)
      else if deathsound == bgdth1 || deathsound == bgdth2 then
        let (r, rng) := pRandom gs0.rng
        (bgdth1 + r % 2, rng)
      else
        (deathsound, gs0.rng)
    let gs := { gs0 with rng }
    let fullVol := actor.typeId == MT_SPIDER || actor.typeId == MT_CYBORG
    pure {
      gs with
      rng := startSoundPitchRngMaybe gs.rng sound.toNatClampNeg
        (fullVol || originAudible gs mobjIdx)
    }

/-- `A_XScream` — `S_StartSound(actor, sfx_slop)` only. Never NULL origin. -/
def aXScream (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_XScream: bad mobj"
  | some _actor =>
    pure {
      gs0 with
      rng := startSoundPitchRngMaybe gs0.rng sfx_slop (originAudible gs0 mobjIdx)
    }

/-- `A_PlayerScream` — `S_StartSound(actor, sfx_pldeth|sfx_pdiehi)` only. -/
def aPlayerScream (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_PlayerScream: bad mobj"
  | some actor =>
    let sound :=
      if gs0.commercial && actor.health < -50 then
        sfx_pdiehi
      else
        sfx_pldeth
    pure {
      gs0 with
      rng := startSoundPitchRngMaybe gs0.rng sound (originAudible gs0 mobjIdx)
    }

/-- `A_Fall` — clear `MF_SOLID` only. -/
def aFall (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_Fall: bad mobj"
  | some actor =>
    pure (setMo gs0 mobjIdx { actor with flags := actor.flags &&& (~~~MF_SOLID) })

/-- `P_RecursiveSound` — fuel-bounded DFS matching C call order. -/
def recursiveSoundFuel (gs0 : GameState) (secIdx : Nat) (soundblocks : Int32)
    (targetIdx : Int32) (fuel : Nat) : Except String GameState :=
  match fuel with
  | 0 => throw "P_RecursiveSound: fuel exhausted"
  | fuel' + 1 => do
    match gs0.sectors[secIdx]? with
    | none => throw "P_RecursiveSound: bad sector"
    | some sec0 =>
      if sec0.validcount == gs0.validcount && sec0.soundtraversed <= soundblocks + 1 then
        return gs0
      let sec := {
        sec0 with
        validcount := gs0.validcount
        soundtraversed := soundblocks + 1
        soundtarget := targetIdx
      }
      let mut gs := { gs0 with sectors := setArr gs0.sectors secIdx sec }
      let mut i : Nat := 0
      while i < sec.lines.size do
        match sec.lines[i]? with
        | none => throw "P_RecursiveSound: bad line idx"
        | some li =>
          match gs.level.lines[li.toNat]? with
          | none => throw "P_RecursiveSound: bad linedef"
          | some ld =>
            if (ld.flags &&& ML_TWOSIDED) != 0 then
              let op ← lineOpening gs ld
              if op.openrange > 0 then
                let other : Int32 :=
                  if ld.frontsector == secIdx.toInt32 then ld.backsector else ld.frontsector
                if other >= 0 then
                  if (ld.flags &&& ML_SOUNDBLOCK) != 0 then
                    if soundblocks == 0 then
                      gs ← recursiveSoundFuel gs other.toNatClampNeg 1 targetIdx fuel'
                  else
                    gs ← recursiveSoundFuel gs other.toNatClampNeg soundblocks targetIdx fuel'
        i := i + 1
      pure gs

/-- `P_NoiseAlert`. -/
def noiseAlert (gs0 : GameState) (targetIdx emitterIdx : Nat) : Except String GameState := do
  match gs0.mobjs[emitterIdx]? with
  | none => throw "P_NoiseAlert: bad emitter"
  | some em =>
    match gs0.level.subsectors[em.subsector.toNat]? with
    | none => throw "P_NoiseAlert: bad subsector"
    | some ss =>
      let gs := { gs0 with validcount := gs0.validcount + 1 }
      recursiveSoundFuel gs ss.sector.toNat 0 targetIdx.toInt32 1000000

/-- `P_KillMobj` (player-target: clear solid, `PST_DEAD`, `P_DropWeapon`). -/
def killMobjWith (setSt : SetMobjStateFn) (gs0 : GameState) (sourceIdx : Option Nat)
    (targetIdx : Nat) : Except String GameState := do
  match gs0.mobjs[targetIdx]? with
  | none => throw "P_KillMobj: bad target"
  | some target0 =>
    let mut flags := target0.flags &&& (~~~(MF_SHOOTABLE ||| MF_FLOAT ||| MF_SKULLFLY))
    if target0.typeId != MT_SKULL then
      flags := flags &&& (~~~MF_NOGRAVITY)
    flags := flags ||| MF_CORPSE ||| MF_DROPOFF
    let target := { target0 with flags, height := target0.height >>> 2 }
    let mut gs := setMo gs0 targetIdx target
    if target.player >= 0 then
      let pi := target.player.toNatClampNeg
      match gs.mobjs[targetIdx]? with
      | none => throw "P_KillMobj: lost player mobj"
      | some tMo =>
        gs := setMo gs targetIdx { tMo with flags := tMo.flags &&& (~~~MF_SOLID) }
      match gs.players[pi]? with
      | none => throw "P_KillMobj: bad target player"
      | some pl =>
        let plDead := { pl with playerstate := PST_DEAD }
        let plDrop ← dropWeapon plDead
        gs := setPlayer gs pi plDrop
    let countKill := (target.flags &&& MF_COUNTKILL) != 0
    match sourceIdx with
    | some si =>
      match gs.mobjs[si]? with
      | some src =>
        if src.player >= 0 then
          if countKill then
            match gs.players[src.player.toNatClampNeg]? with
            | none => throw "P_KillMobj: bad source player"
            | some pl =>
              gs := setPlayer gs src.player.toNatClampNeg { pl with killcount := pl.killcount + 1 }
        else if !gs.netgame && countKill then
          match gs.players[0]? with
          | none => throw "P_KillMobj: missing player 0"
          | some pl =>
            gs := setPlayer gs 0 { pl with killcount := pl.killcount + 1 }
      | none =>
        if !gs.netgame && countKill then
          match gs.players[0]? with
          | none => throw "P_KillMobj: missing player 0"
          | some pl =>
            gs := setPlayer gs 0 { pl with killcount := pl.killcount + 1 }
    | none =>
      if !gs.netgame && countKill then
        match gs.players[0]? with
        | none => throw "P_KillMobj: missing player 0"
        | some pl =>
          gs := setPlayer gs 0 { pl with killcount := pl.killcount + 1 }
    match gs.mobjs[targetIdx]? with
    | none => throw "P_KillMobj: lost before deathstate"
    | some t1 =>
      let info ←
        match mobjinfo[t1.typeId.toNatClampNeg]? with
        | some i => pure i
        | none => throw "P_KillMobj: missing mobjinfo"
      let deathSt :=
        if t1.health < -info.spawnhealth && info.xdeathstate != 0 then
          info.xdeathstate
        else
          info.deathstate
      let (gs1, _) ← setSt gs targetIdx deathSt
      gs := gs1
      match gs.mobjs[targetIdx]? with
      | none => throw "P_KillMobj: lost after deathstate"
      | some t2 =>
        let (r, rng) := pRandom gs.rng
        let mut tics := t2.tics - (r &&& 3)
        if tics < 1 then tics := 1
        gs := { setMo gs targetIdx { t2 with tics } with rng }
        let item? : Option Int32 :=
          if t2.typeId == MT_WOLFSS || t2.typeId == MT_POSSESSED then some MT_CLIP
          else if t2.typeId == MT_SHOTGUY then some MT_SHOTGUN
          else if t2.typeId == MT_CHAINGUY then some MT_CHAINGUN
          else none
        match item? with
        | none => pure gs
        | some item =>
          match gs.mobjs[targetIdx]? with
          | none => throw "P_KillMobj: lost before drop"
          | some t3 =>
            let (gsD, dropIdx) ← Spawn.spawnMobj gs t3.x t3.y Spawn.ONFLOORZ item
            match gsD.mobjs[dropIdx]? with
            | none => throw "P_KillMobj: lost drop"
            | some drop =>
              pure (setMo gsD dropIdx { drop with flags := drop.flags ||| MF_DROPPED })

/-- `P_DamageMobj` (player victim + kill). -/
def damageMobjWith (setSt : SetMobjStateFn) (gs0 : GameState) (targetIdx : Nat)
    (inflictorIdx sourceIdx : Option Nat) (damage0 : Int32) : Except String GameState := do
  match gs0.mobjs[targetIdx]? with
  | none => throw "P_DamageMobj: bad target"
  | some target0 =>
    if (target0.flags &&& MF_SHOOTABLE) == 0 then
      return gs0
    if target0.health <= 0 then
      return gs0
    let mut gs := gs0
    let mut target := target0
    let mut damage := damage0
    if (target.flags &&& MF_SKULLFLY) != 0 then
      target := { target with momx := 0, momy := 0, momz := 0 }
      gs := setMo gs targetIdx target
    if target.player >= 0 && gs.gameskill == sk_baby then
      damage := damage >>> 1
    let skipThrust :=
      match inflictorIdx with
      | none => true
      | some _ =>
        if (target.flags &&& MF_NOCLIP) != 0 then true
        else
          match sourceIdx with
          | none => false
          | some si =>
            match gs.mobjs[si]? with
            | none => false
            | some src =>
              if src.player < 0 then false
              else
                match gs.players[src.player.toNatClampNeg]? with
                | some pl => pl.readyweapon == wp_chainsaw
                | none => false
    if !skipThrust then
      match inflictorIdx, gs.mobjs[targetIdx]? with
      | some ii, some tgt =>
        match gs.mobjs[ii]? with
        | none => throw "P_DamageMobj: bad inflictor"
        | some inf =>
          let mut ang := pointToAngle2 inf.x inf.y tgt.x tgt.y
          let info ←
            match mobjinfo[tgt.typeId.toNatClampNeg]? with
            | some i => pure i
            | none => throw "P_DamageMobj: missing mobjinfo"
          let mut thrust := damage * (FRACUNIT >>> 3) * 100 / info.mass
          if damage < 40 && damage > tgt.health && tgt.z - inf.z > 64 * FRACUNIT then
            let (r, rng) := pRandom gs.rng
            gs := { gs with rng }
            if (r &&& 1) != 0 then
              ang := ang + ANG180
              thrust := thrust * 4
          let fine := (ang >>> ANGLETOFINESHIFT.toUInt32) &&& FINEMASK
          match finecosine[fine.toNat]?, finesine[fine.toNat]? with
          | some cosv, some sinv =>
            match gs.mobjs[targetIdx]? with
            | none => throw "P_DamageMobj: lost before thrust"
            | some t2 =>
              gs := setMo gs targetIdx {
                t2 with
                momx := t2.momx + fixedMul thrust cosv
                momy := t2.momy + fixedMul thrust sinv
              }
          | _, _ => throw "P_DamageMobj: fine table OOB"
      | _, _ => throw "P_DamageMobj: thrust missing mobj"
    match gs.mobjs[targetIdx]? with
    | none => throw "P_DamageMobj: lost before player"
    | some tP =>
      if tP.player >= 0 then
        match gs.level.subsectors[tP.subsector.toNat]? with
        | none => throw "P_DamageMobj: player subsector missing"
        | some ss =>
          match gs.sectors[ss.sector.toNat]? with
          | none => throw "P_DamageMobj: player sector missing"
          | some sec =>
            if sec.special == 11 && damage >= tP.health then
              damage := tP.health - 1
        match gs.players[tP.player.toNatClampNeg]? with
        | none => throw "P_DamageMobj: bad player"
        | some pl0 =>
          if damage < 1000 &&
              ((pl0.cheats &&& CF_GODMODE) != 0 ||
                (match pl0.powers[pw_invulnerability]? with
                 | some v => v != 0
                 | none => false)) then
            return gs
          let mut pl := pl0
          if pl.armortype != 0 then
            let mut saved :=
              if pl.armortype == 1 then damage / 3 else damage / 2
            if pl.armorpoints <= saved then
              saved := pl.armorpoints
              pl := { pl with armortype := 0 }
            pl := { pl with armorpoints := pl.armorpoints - saved }
            damage := damage - saved
          let mut php := pl.health - damage
          if php < 0 then php := 0
          let mut dmgc := pl.damagecount + damage
          if dmgc > 100 then dmgc := 100
          let attacker :=
            match sourceIdx with
            | none => (-1 : Int32)
            | some si => si.toInt32
          pl := { pl with health := php, damagecount := dmgc, attacker }
          gs := setPlayer gs tP.player.toNatClampNeg pl
      match gs.mobjs[targetIdx]? with
      | none => throw "P_DamageMobj: lost before health"
      | some t3 =>
        let health := t3.health - damage
        gs := setMo gs targetIdx { t3 with health }
        if health <= 0 then
          return (← killMobjWith setSt gs sourceIdx targetIdx)
        let info ←
          match mobjinfo[t3.typeId.toNatClampNeg]? with
          | some i => pure i
          | none => throw "P_DamageMobj: missing mobjinfo"
        match gs.mobjs[targetIdx]? with
        | none => throw "P_DamageMobj: lost before pain"
        | some t4 =>
          let (rPain, rng) := pRandom gs.rng
          gs := { gs with rng }
          if rPain < info.painchance && (t4.flags &&& MF_SKULLFLY) == 0 then
            let t4 := { t4 with flags := t4.flags ||| MF_JUSTHIT }
            gs := setMo gs targetIdx t4
            let (gs1, _) ← setSt gs targetIdx info.painstate
            gs := gs1
          match gs.mobjs[targetIdx]? with
          | none => throw "P_DamageMobj: lost before retarget"
          | some t5 =>
            let t5 := { t5 with reactiontime := 0 }
            gs := setMo gs targetIdx t5
            let srcOk :=
              match sourceIdx with
              | none => false
              | some si =>
                if si == targetIdx then false
                else
                  match gs.mobjs[si]? with
                  | some src => src.typeId != MT_VILE
                  | none => false
            if (t5.threshold == 0 || t5.typeId == MT_VILE) && srcOk then
              match sourceIdx with
              | none => pure gs
              | some si =>
                match gs.mobjs[targetIdx]? with
                | none => throw "P_DamageMobj: lost on retarget"
                | some t6 =>
                  let t6 := { t6 with target := si.toInt32, threshold := BASETHRESHOLD }
                  gs := setMo gs targetIdx t6
                  if t6.state == info.spawnstate && info.seestate != 0 then
                    let (gs1, _) ← setSt gs targetIdx info.seestate
                    pure gs1
                  else
                    pure gs
            else
              pure gs

/-- Nested `P_SetMobjState` from `A_SPosAttack` hits: null-action only (player
pain / death overlays). Non-null actions loud-error instead of re-entering
the set-state mutual. -/
private def applyNullActionState (gs : GameState) (idx : Nat) (stnum : UInt32) :
    Except String (GameState × Bool) := do
  match states[stnum.toNat]? with
  | none => throw s!"P_SetMobjState: bad state {stnum}"
  | some st =>
    if st.action != actionNull then
      throw s!"A_SPosAttack: nested action {st.action} not implemented"
    if st.tics == 0 then
      throw "A_SPosAttack: tics=0 setState not implemented"
    match gs.mobjs[idx]? with
    | none => throw "P_SetMobjState: bad mobj"
    | some mo =>
      pure (setMo gs idx {
        mo with
        state := stnum
        tics := st.tics
        sprite := st.sprite
        frame := st.frame
      }, true)

/-- `A_PosAttack`. C order: face → aim → `sfx_pistol` → spread → one pellet.
Not the `A_SPosAttack` sound-first / 3-pellet sequence. -/
def aPosAttack (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_PosAttack: bad mobj"
  | some actor0 =>
    if actor0.target < 0 then
      return gs0
    let gs ← aFaceTarget gs0 mobjIdx
    match gs.mobjs[mobjIdx]? with
    | none => throw "A_PosAttack: lost after face"
    | some actor =>
      let angle0 := actor.angle
      let (gs1, slope, _) ← Hitscan.aimLineAttack gs mobjIdx angle0 Hitscan.MISSILERANGE
      let mut gs := {
        gs1 with
        rng := startSoundPitchRngMaybe gs1.rng sfx_pistol (originAudible gs1 mobjIdx)
      }
      let (d, rng) := pSubRandom gs.rng
      gs := { gs with rng }
      let angle := angle0 + (d.toUInt32 <<< 20)
      let (r, rng) := pRandom gs.rng
      gs := { gs with rng }
      let damage := ((r % 5) + 1) * 3
      Hitscan.lineAttack (damageMobjWith applyNullActionState)
        gs mobjIdx angle Hitscan.MISSILERANGE slope damage

/-- `A_SPosAttack`. -/
def aSPosAttack (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_SPosAttack: bad mobj"
  | some actor0 =>
    if actor0.target < 0 then
      return gs0
    let mut gs := {
      gs0 with
      rng := startSoundPitchRngMaybe gs0.rng sfx_shotgn (originAudible gs0 mobjIdx)
    }
    gs ← aFaceTarget gs mobjIdx
    match gs.mobjs[mobjIdx]? with
    | none => throw "A_SPosAttack: lost after face"
    | some actor =>
      let bangle := actor.angle
      let (gs1, slope, _) ← Hitscan.aimLineAttack gs mobjIdx bangle Hitscan.MISSILERANGE
      gs := gs1
      let mut i : Nat := 0
      while i < 3 do
        let (d, rng) := pSubRandom gs.rng
        gs := { gs with rng }
        let angle := bangle + (d.toUInt32 <<< 20)
        let (r, rng) := pRandom gs.rng
        gs := { gs with rng }
        let damage := ((r % 5) + 1) * 3
        gs ← Hitscan.lineAttack (damageMobjWith applyNullActionState)
          gs mobjIdx angle Hitscan.MISSILERANGE slope damage
        i := i + 1
      pure gs

/-- `A_Explode`: source is `actor.target` (none if `< 0`). -/
def aExplodeWith (damageMobj : Hitscan.DamageMobjFn) (gs0 : GameState) (mobjIdx : Nat) :
    Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_Explode: bad mobj"
  | some actor =>
    let source :=
      if actor.target < 0 then none else some actor.target.toNatClampNeg
    Hitscan.radiusAttack damageMobj gs0 mobjIdx source 128

/-- `P_ExplodeMissile`. -/
def explodeMissileWith (setSt : SetMobjStateFn) (gs0 : GameState) (mobjIdx : Nat) :
    Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_ExplodeMissile: bad mobj"
  | some mo0 =>
    let info ← infoOf mo0
    let gs := setMo gs0 mobjIdx { mo0 with momx := 0, momy := 0, momz := 0 }
    let (gs1, still) ← setSt gs mobjIdx info.deathstate
    if !still then
      return gs1
    match gs1.mobjs[mobjIdx]? with
    | none => throw "P_ExplodeMissile: lost after deathstate"
    | some mo =>
      let (r, rng) := pRandom gs1.rng
      let mut tics := mo.tics - (r &&& 3)
      if tics < 1 then tics := 1
      let gs2 := {
        setMo gs1 mobjIdx { mo with tics, flags := mo.flags &&& (~~~MF_MISSILE) } with rng
      }
      if info.deathsound == 0 then
        return gs2
      pure {
        gs2 with
        rng := startSoundPitchRngMaybe gs2.rng info.deathsound.toNatClampNeg
          (originAudible gs2 mobjIdx)
      }

/-- `A_TroopAttack`. -/
def aTroopAttack (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_TroopAttack: bad mobj"
  | some actor0 =>
    if actor0.target < 0 then
      return gs0
    let gs ← aFaceTarget gs0 mobjIdx
    let (gs, inMelee) ← checkMeleeRange gs mobjIdx
    if inMelee then
      let gs := {
        gs with
        rng := startSoundPitchRngMaybe gs.rng sfx_claw (originAudible gs mobjIdx)
      }
      let (r, rng) := pRandom gs.rng
      let gs := { gs with rng }
      let damage := (r % 8 + 1) * 3
      match gs.mobjs[mobjIdx]? with
      | none => throw "A_TroopAttack: lost before melee damage"
      | some actor =>
        if actor.target < 0 then
          return gs
        damageMobjWith applyNullActionState gs actor.target.toNatClampNeg
          (some mobjIdx) (some mobjIdx) damage
    else
      match gs.mobjs[mobjIdx]? with
      | none => throw "A_TroopAttack: lost before missile"
      | some actor =>
        if actor.target < 0 then
          return gs
        let (gs, _) ← Spawn.spawnMissile
          (damageMobjWith applyNullActionState)
          (explodeMissileWith applyNullActionState)
          gs mobjIdx actor.target.toNatClampNeg Spawn.MT_TROOPSHOT
        pure gs

/-- `A_SargAttack`. Closes over face / melee / null-action damage. -/
def aSargAttack (gs0 : GameState) (mobjIdx : Nat) : Except String GameState :=
  SargAttack.aSargAttackWith aFaceTarget checkMeleeRange
    (damageMobjWith applyNullActionState) gs0 mobjIdx

-- Fuel-bounded set-state / action / look / chase cycle (no partial).
mutual
def setMobjStateFuel (gs0 : GameState) (mobjIdx : Nat) (state0 : UInt32) (fuel : Nat) :
    Except String (GameState × Bool) :=
  match fuel with
  | 0 => throw "P_SetMobjState: fuel exhausted"
  | fuel' + 1 => do
    if state0 == 0 then
      let mo ←
        match gs0.mobjs[mobjIdx]? with
        | none => throw "P_SetMobjState: bad mobj"
        | some mo => pure mo
      let gs := setMo gs0 mobjIdx { mo with state := 0 }
      let gs ← removeMobj gs mobjIdx
      return (gs, false)
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
    else if action == action_A_FaceTarget then
      aFaceTarget gs0 mobjIdx
    else if action == action_A_Pain then
      aPain gs0 mobjIdx
    else if action == action_A_PosAttack then
      aPosAttack gs0 mobjIdx
    else if action == action_A_SPosAttack then
      aSPosAttack gs0 mobjIdx
    else if action == action_A_Scream then
      aScream gs0 mobjIdx
    else if action == action_A_XScream then
      aXScream gs0 mobjIdx
    else if action == action_A_PlayerScream then
      aPlayerScream gs0 mobjIdx
    else if action == action_A_Fall then
      aFall gs0 mobjIdx
    else if action == action_A_Explode then
      aExplodeWith (damageMobjWith applyNullActionState) gs0 mobjIdx
    else if action == action_A_TroopAttack then
      aTroopAttack gs0 mobjIdx
    else if action == action_A_SargAttack then
      aSargAttack gs0 mobjIdx
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
          let fullVol := actorS.typeId == MT_SPIDER || actorS.typeId == MT_CYBORG
          gs := {
            gs with
            rng := startSoundPitchRngMaybe gs.rng sound.toNatClampNeg
              (fullVol || originAudible gs mobjIdx)
          }
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
          match gs.mobjs[mobjIdx]? with
          | none => throw "A_Chase: lost before meleestate"
          | some a =>
            let info ← infoOf a
            let gsS :=
              if info.attacksound != 0 then
                { gs with
                  rng := startSoundPitchRngMaybe gs.rng info.attacksound.toNatClampNeg
                    (originAudible gs mobjIdx) }
              else gs
            let (gs2, _) ← setMobjStateFuel gsS mobjIdx info.meleestate fuel'
            return gs2
      if info.missilestate != 0 then
        let skipMissile :=
          gs.gameskill < sk_nightmare && actor.movecount != 0
        if !skipMissile then
          let (gs1, canFire) ← checkMissileRange gs mobjIdx
          gs := gs1
          if canFire then
            match gs.mobjs[mobjIdx]? with
            | none => throw "A_Chase: lost before missilestate"
            | some a =>
              let info ← infoOf a
              let (gs2, _) ← setMobjStateFuel gs mobjIdx info.missilestate fuel'
              match gs2.mobjs[mobjIdx]? with
              | none => throw "A_Chase: lost after missilestate"
              | some a2 =>
                return setMo gs2 mobjIdx { a2 with flags := a2.flags ||| MF_JUSTATTACKED }
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
              rng := startSoundPitchRngMaybe gs2.rng info.activesound.toNatClampNeg
                (originAudible gs2 mobjIdx)
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

def damageMobj (gs0 : GameState) (targetIdx : Nat) (inflictorIdx sourceIdx : Option Nat)
    (damage0 : Int32) : Except String GameState :=
  damageMobjWith setMobjState gs0 targetIdx inflictorIdx sourceIdx damage0

def aExplode (gs0 : GameState) (mobjIdx : Nat) : Except String GameState :=
  aExplodeWith damageMobj gs0 mobjIdx

def explodeMissile (gs0 : GameState) (mobjIdx : Nat) : Except String GameState :=
  explodeMissileWith setMobjState gs0 mobjIdx

def killMobj (gs0 : GameState) (sourceIdx : Option Nat) (targetIdx : Nat) :
    Except String GameState :=
  killMobjWith setMobjState gs0 sourceIdx targetIdx

end Doom.Playsim.Enemy
