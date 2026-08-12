import Doom.Playsim.Bsp
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj

/-!
# Doom.Playsim.Map

`p_map.c` open subset: `P_CheckPosition` / `P_TryMove` with PIT line/thing
checks. `P_SlideMove` and specials beyond spechit collect are loud-errors.
-/

namespace Doom.Playsim.Map

open Doom.Playsim.Bsp
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.MapUtil
open Doom.Playsim.Mobj

/-- `doomdata.h` linedef blocking flags. -/
def ML_BLOCKING : Int32 := 1
def ML_BLOCKMONSTERS : Int32 := 2

/-- `p_local.h` `MAXMOVE` = 30*FRACUNIT. -/
def MAXMOVE : Int32 := 30 * FRACUNIT

/-- `p_local.h` `MAXSPECIALCROSS`. -/
def MAXSPECIALCROSS : Nat := 20

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

private def idx (x : Int32) : Nat := x.toNatClampNeg

/-- Scratch globals for check/try move (`tmbbox`, floors, spechit, …). -/
structure MapScratch where
  tmthingIdx : Nat
  tmflags : UInt32
  tmx : Int32
  tmy : Int32
  tmbbox : Array Int32
  tmfloorz : Int32
  tmceilingz : Int32
  tmdropoffz : Int32
  floatok : Bool
  /-- Linedef index of `ceilingline`, or `-1`. -/
  ceilingline : Int32
  spechit : Array Nat
  deriving Repr

def emptyScratch : MapScratch := {
  tmthingIdx := 0
  tmflags := 0
  tmx := 0
  tmy := 0
  tmbbox := #[0, 0, 0, 0]
  tmfloorz := 0
  tmceilingz := 0
  tmdropoffz := 0
  floatok := false
  ceilingline := -1
  spechit := #[]
}

/-- `PIT_CheckLine`. -/
def pitCheckLine (gs : GameState) (scr : MapScratch) (lineIdx : Nat) (ld : Line) :
    Except String (MapScratch × Bool) := do
  let boxtop := match scr.tmbbox[BOXTOP]? with | some v => v | none => (0 : Int32)
  let boxbottom := match scr.tmbbox[BOXBOTTOM]? with | some v => v | none => (0 : Int32)
  let boxleft := match scr.tmbbox[BOXLEFT]? with | some v => v | none => (0 : Int32)
  let boxright := match scr.tmbbox[BOXRIGHT]? with | some v => v | none => (0 : Int32)
  let ldLeft := match ld.bbox[BOXLEFT]? with | some v => v | none => (0 : Int32)
  let ldRight := match ld.bbox[BOXRIGHT]? with | some v => v | none => (0 : Int32)
  let ldTop := match ld.bbox[BOXTOP]? with | some v => v | none => (0 : Int32)
  let ldBottom := match ld.bbox[BOXBOTTOM]? with | some v => v | none => (0 : Int32)
  if boxright <= ldLeft || boxleft >= ldRight || boxtop <= ldBottom || boxbottom >= ldTop then
    return (scr, true)
  match gs.level.vertexes[ld.v1.toNat]? with
  | none => throw "PIT_CheckLine: bad v1"
  | some v1 =>
    if boxOnLineSide scr.tmbbox ld v1 != (-1 : Int32) then
      return (scr, true)
    if ld.backsector < 0 then
      return (scr, false)
    match gs.mobjs[scr.tmthingIdx]? with
    | none => throw "PIT_CheckLine: bad tmthing"
    | some tmthing =>
      if (tmthing.flags &&& MF_MISSILE) == 0 then
        if (ld.flags &&& ML_BLOCKING) != 0 then
          return (scr, false)
        if tmthing.player < 0 && (ld.flags &&& ML_BLOCKMONSTERS) != 0 then
          return (scr, false)
      let op ← lineOpening gs ld
      let mut scr := scr
      if op.opentop < scr.tmceilingz then
        scr := { scr with tmceilingz := op.opentop, ceilingline := lineIdx.toInt32 }
      if op.openbottom > scr.tmfloorz then
        scr := { scr with tmfloorz := op.openbottom }
      if op.lowfloor < scr.tmdropoffz then
        scr := { scr with tmdropoffz := op.lowfloor }
      if ld.special != 0 then
        if scr.spechit.size >= MAXSPECIALCROSS then
          throw "PIT_CheckLine: spechit overrun"
        scr := { scr with spechit := scr.spechit.push lineIdx }
      pure (scr, true)

/-- `PIT_CheckThing` — solid branch only; other branches loud-error. -/
def pitCheckThing (gs : GameState) (scr : MapScratch) (thingIdx : Nat) (thing : Mobj) :
    Except String (MapScratch × Bool) := do
  if (thing.flags &&& (MF_SOLID ||| MF_SPECIAL ||| MF_SHOOTABLE)) == 0 then
    return (scr, true)
  match gs.mobjs[scr.tmthingIdx]? with
  | none => throw "PIT_CheckThing: bad tmthing"
  | some tmthing =>
    let blockdist := thing.radius + tmthing.radius
    if wabs (thing.x - scr.tmx) >= blockdist || wabs (thing.y - scr.tmy) >= blockdist then
      return (scr, true)
    if thingIdx == scr.tmthingIdx then
      return (scr, true)
    if (tmthing.flags &&& MF_SKULLFLY) != 0 then
      throw "PIT_CheckThing: skullfly not implemented"
    if (tmthing.flags &&& MF_MISSILE) != 0 then
      throw "PIT_CheckThing: missile not implemented"
    if (thing.flags &&& MF_SPECIAL) != 0 then
      if (scr.tmflags &&& MF_PICKUP) != 0 then
        throw "PIT_CheckThing: pickup not implemented"
      -- solid flag decides continue (C: return !solid)
      pure (scr, (thing.flags &&& MF_SOLID) == 0)
    else
      pure (scr, (thing.flags &&& MF_SOLID) == 0)

/-- `P_CheckPosition`. -/
def checkPosition (gs0 : GameState) (mobjIdx : Nat) (x y : Int32) :
    Except String (GameState × MapScratch × Bool) := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_CheckPosition: bad mobj"
  | some tmthing =>
    let tmbbox := #[
      y + tmthing.radius,  -- BOXTOP
      y - tmthing.radius,  -- BOXBOTTOM
      x - tmthing.radius,  -- BOXLEFT
      x + tmthing.radius   -- BOXRIGHT
    ]
    let ssIdx ← pointInSubsector gs0.level x y
    match gs0.level.subsectors[ssIdx.toNat]? with
    | none => throw "P_CheckPosition: bad subsector"
    | some ss =>
      match gs0.sectors[ss.sector.toNat]? with
      | none => throw "P_CheckPosition: bad sector"
      | some sec =>
        let mut gs := { gs0 with validcount := gs0.validcount + 1 }
        let mut scr : MapScratch := {
          emptyScratch with
          tmthingIdx := mobjIdx
          tmflags := tmthing.flags
          tmx := x
          tmy := y
          tmbbox
          tmfloorz := sec.floorheight
          tmdropoffz := sec.floorheight
          tmceilingz := sec.ceilingheight
          ceilingline := -1
          spechit := #[]
        }
        if (scr.tmflags &&& MF_NOCLIP) != 0 then
          return (gs, scr, true)
        let bmap := gs.level.blockmap
        let boxtop := match scr.tmbbox[BOXTOP]? with | some v => v | none => (0 : Int32)
        let boxbottom := match scr.tmbbox[BOXBOTTOM]? with | some v => v | none => (0 : Int32)
        let boxleft := match scr.tmbbox[BOXLEFT]? with | some v => v | none => (0 : Int32)
        let boxright := match scr.tmbbox[BOXRIGHT]? with | some v => v | none => (0 : Int32)
        -- things first (bbox extended by MAXRADIUS)
        let xl := ashrMapBlock (boxleft - bmap.originX - MAXRADIUS)
        let xh := ashrMapBlock (boxright - bmap.originX + MAXRADIUS)
        let yl := ashrMapBlock (boxbottom - bmap.originY - MAXRADIUS)
        let yh := ashrMapBlock (boxtop - bmap.originY + MAXRADIUS)
        let mut bx := xl
        while bx <= xh do
          let mut byCoord := yl
          while byCoord <= yh do
            let (gs1, scr1, ok) ← blockThingsIterator gs scr bx byCoord
              fun gs st mi mo => do
                let (st1, ok) ← pitCheckThing gs st mi mo
                pure (gs, st1, ok)
            gs := gs1
            scr := scr1
            if !ok then
              return (gs, scr, false)
            byCoord := byCoord + 1
          bx := bx + 1
        -- lines
        let xl := ashrMapBlock (boxleft - bmap.originX)
        let xh := ashrMapBlock (boxright - bmap.originX)
        let yl := ashrMapBlock (boxbottom - bmap.originY)
        let yh := ashrMapBlock (boxtop - bmap.originY)
        bx := xl
        while bx <= xh do
          let mut byCoord := yl
          while byCoord <= yh do
            let (gs1, scr1, ok) ← blockLinesIterator gs scr bx byCoord
              fun gs st li ld => do
                let (st1, ok) ← pitCheckLine gs st li ld
                pure (gs, st1, ok)
            gs := gs1
            scr := scr1
            if !ok then
              return (gs, scr, false)
            byCoord := byCoord + 1
          bx := bx + 1
        pure (gs, scr, true)

/-- `P_CrossSpecialLine` — none expected before DEMO1 tic 27. -/
def crossSpecialLine (_gs : GameState) (lineIdx : Nat) (_side : Nat) (_thingIdx : Nat) :
    Except String Unit :=
  throw s!"P_CrossSpecialLine: special crossed on line {lineIdx} (unexpected)"

/-- `P_TryMove`. -/
def tryMove (gs0 : GameState) (mobjIdx : Nat) (x y : Int32) :
    Except String (GameState × Bool) := do
  let (gs1, scr0, ok) ← checkPosition gs0 mobjIdx x y
  let mut gs := gs1
  let mut scr := { scr0 with floatok := false }
  if !ok then
    return (gs, false)
  match gs.mobjs[mobjIdx]? with
  | none => throw "P_TryMove: bad mobj"
  | some thing =>
    if (thing.flags &&& MF_NOCLIP) == 0 then
      if scr.tmceilingz - scr.tmfloorz < thing.height then
        return (gs, false)
      scr := { scr with floatok := true }
      if (thing.flags &&& MF_TELEPORT) == 0 && scr.tmceilingz - thing.z < thing.height then
        return (gs, false)
      if (thing.flags &&& MF_TELEPORT) == 0 && scr.tmfloorz - thing.z > 24 * FRACUNIT then
        return (gs, false)
      if (thing.flags &&& (MF_DROPOFF ||| MF_FLOAT)) == 0 &&
          scr.tmfloorz - scr.tmdropoffz > 24 * FRACUNIT then
        return (gs, false)
    gs ← unsetThingPosition gs mobjIdx
    match gs.mobjs[mobjIdx]? with
    | none => throw "P_TryMove: lost after unset"
    | some thing2 =>
      let oldx := thing2.x
      let oldy := thing2.y
      let thing3 := {
        thing2 with
        floorz := scr.tmfloorz
        ceilingz := scr.tmceilingz
        x := x
        y := y
      }
      gs := { gs with mobjs := setArr gs.mobjs mobjIdx thing3 }
      gs ← setThingPosition gs mobjIdx
      if (thing3.flags &&& (MF_TELEPORT ||| MF_NOCLIP)) == 0 then
        let mut si := scr.spechit.size
        while si > 0 do
          si := si - 1
          match scr.spechit[si]? with
          | none => throw "P_TryMove: bad spechit"
          | some lineIdx =>
            match gs.level.lines[lineIdx]? with
            | none => throw "P_TryMove: bad spechit line"
            | some ld =>
              match gs.level.vertexes[ld.v1.toNat]?, gs.mobjs[mobjIdx]? with
              | some v1, some mo =>
                let side := pointOnLineSide mo.x mo.y ld v1
                let oldside := pointOnLineSide oldx oldy ld v1
                if side != oldside && ld.special != 0 then
                  crossSpecialLine gs lineIdx oldside mobjIdx
              | _, _ => throw "P_TryMove: spechit geometry missing"
      pure (gs, true)

end Doom.Playsim.Map
