import Doom.Harness.Fnv
import Doom.Harness.TraceFormat
import Doom.Playsim.GameState
import Doom.Playsim.Thinker

/-!
# Doom.Playsim.TraceEmit

Build a `TicRecord` from `GameState` at the post-`G_Ticker` dump point.
-/

namespace Doom.Playsim.TraceEmit

open Doom.Harness.Fnv
open Doom.Harness.TraceFormat
open Doom.Playsim.GameState
open Doom.Playsim.Thinker

private def i32bits (x : Int32) : UInt32 := x.toUInt32

/-- FNV of all sectors' floor/ceiling (§5.4). -/
def sectorsDigest (gs : GameState) : UInt64 :=
  Id.run do
    let mut acc := ByteArray.emptyWithCapacity (gs.sectors.size * 8)
    for sec in gs.sectors do
      acc := pushU32LE acc (i32bits sec.floorheight)
      acc := pushU32LE acc (i32bits sec.ceilingheight)
    pure (fnv1a64 acc)

def emitPlayer (gs : GameState) (playerIndex : Nat) : Except String PlayerRec := do
  match gs.players[playerIndex]? with
  | none => throw "emitPlayer: missing"
  | some p =>
    let (moTraceId, x, y, z, momx, momy, momz, angle) ←
      if p.mo < 0 then
        pure ((0xffffffff : UInt32), (0 : Int32), 0, 0, 0, 0, 0, (0 : UInt32))
      else
        match gs.mobjs[p.mo.toNatClampNeg]? with
        | none => throw "emitPlayer: bad mo"
        | some mo =>
          pure (mo.traceId, mo.x, mo.y, mo.z, mo.momx, mo.momy, mo.momz, mo.angle)
    let ammo (i : Nat) : Int32 := match p.ammo[i]? with | some v => v | none => 0
    let wo (i : Nat) : Int32 := match p.weaponowned[i]? with | some v => v | none => 0
    pure {
      playerIndex := playerIndex.toUInt32
      moTraceId := moTraceId
      x := i32bits x
      y := i32bits y
      z := i32bits z
      momx := i32bits momx
      momy := i32bits momy
      momz := i32bits momz
      angle := angle
      viewz := i32bits p.viewz
      health := i32bits p.health
      armorpoints := i32bits p.armorpoints
      readyweapon := i32bits p.readyweapon
      pendingweapon := i32bits p.pendingweapon
      ammo0 := i32bits (ammo 0)
      ammo1 := i32bits (ammo 1)
      ammo2 := i32bits (ammo 2)
      ammo3 := i32bits (ammo 3)
      weaponowned0 := i32bits (wo 0)
      weaponowned1 := i32bits (wo 1)
      weaponowned2 := i32bits (wo 2)
      weaponowned3 := i32bits (wo 3)
      weaponowned4 := i32bits (wo 4)
      weaponowned5 := i32bits (wo 5)
      weaponowned6 := i32bits (wo 6)
      weaponowned7 := i32bits (wo 7)
      weaponowned8 := i32bits (wo 8)
      cmdForwardmove := i32bits p.cmd.forwardmove
      cmdSidemove := i32bits p.cmd.sidemove
      cmdAngleturn := i32bits p.cmd.angleturn
      cmdButtons := p.cmd.buttons
    }

def emitThinker (gs : GameState) (th : Thinker) : Except String ThinkerRec := do
  if th.func == THF_MOBJ then
    match gs.mobjs[th.payload.toNat]? with
    | none => throw s!"emitThinker: bad mobj payload {th.payload}"
    | some mo =>
      pure {
        traceId := th.traceId
        func := th.func
        mobj := some {
          x := i32bits mo.x
          y := i32bits mo.y
          z := i32bits mo.z
          momx := i32bits mo.momx
          momy := i32bits mo.momy
          momz := i32bits mo.momz
          angle := mo.angle
          state := mo.state
          tics := i32bits mo.tics
          health := i32bits mo.health
          flags := mo.flags
          type_ := i32bits mo.typeId
        }
      }
  else
    pure { traceId := th.traceId, func := th.func, mobj := none }

/-- Emit one in-level tic record after `G_Ticker`. -/
def emitTicRecord (gs : GameState) : Except String TicRecord := do
  let mut players : Array PlayerRec := #[]
  let mut i : Nat := 0
  while i < 4 do
    match gs.playeringame[i]? with
    | some true =>
      let pr ← emitPlayer gs i
      players := players.push pr
    | _ => pure ()
    i := i + 1
  let mut thinkers : Array ThinkerRec := #[]
  for th in gs.thinkers do
    thinkers := thinkers.push (← emitThinker gs th)
  let mut sectors : Array SectorRec := #[]
  let mut s : Nat := 0
  while s < gs.sectors.size do
    match gs.sectors[s]? with
    | some sec =>
      if sec.specialdata != -1 then
        sectors := sectors.push {
          sectorIndex := s.toUInt32
          floorheight := i32bits sec.floorheight
          ceilingheight := i32bits sec.ceilingheight
        }
    | none => pure ()
    s := s + 1
  pure {
    gametic := gs.gametic
    inLevel := 1
    leveltime := gs.leveltime
    rndindex := gs.rng.rndindex
    prndindex := gs.rng.prndindex
    players
    thinkers
    sectors
    sectorsDigest := sectorsDigest gs
  }

end Doom.Playsim.TraceEmit
