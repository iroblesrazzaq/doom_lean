import Doom.Playsim.Angle
import Doom.Playsim.Bsp
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Inter
import Doom.Playsim.Level
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj
import Doom.Playsim.Tables

/-!
# Doom.Playsim.Map

`p_map.c` open subset: `P_CheckPosition` / `P_TryMove` / `P_SlideMove` with
PIT line/thing checks and `SPR_ARM1` pickup via `P_TouchSpecialThing`.
-/

namespace Doom.Playsim.Map

open Doom.Playsim.Angle
open Doom.Playsim.Bsp
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Inter
open Doom.Playsim.Level
open Doom.Playsim.MapUtil
open Doom.Playsim.Mobj
open Doom.Playsim.Tables

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

/-- `PIT_CheckThing` — solid + `MF_SPECIAL`/`MF_PICKUP` → `P_TouchSpecialThing`. -/
def pitCheckThing (gs0 : GameState) (scr : MapScratch) (thingIdx : Nat) (thing : Mobj) :
    Except String (GameState × MapScratch × Bool) := do
  if (thing.flags &&& (MF_SOLID ||| MF_SPECIAL ||| MF_SHOOTABLE)) == 0 then
    return (gs0, scr, true)
  match gs0.mobjs[scr.tmthingIdx]? with
  | none => throw "PIT_CheckThing: bad tmthing"
  | some tmthing =>
    let blockdist := thing.radius + tmthing.radius
    if wabs (thing.x - scr.tmx) >= blockdist || wabs (thing.y - scr.tmy) >= blockdist then
      return (gs0, scr, true)
    if thingIdx == scr.tmthingIdx then
      return (gs0, scr, true)
    if (tmthing.flags &&& MF_SKULLFLY) != 0 then
      throw "PIT_CheckThing: skullfly not implemented"
    if (tmthing.flags &&& MF_MISSILE) != 0 then
      throw "PIT_CheckThing: missile not implemented"
    if (thing.flags &&& MF_SPECIAL) != 0 then
      let mut gs := gs0
      if (scr.tmflags &&& MF_PICKUP) != 0 then
        gs ← touchSpecialThing gs thingIdx scr.tmthingIdx
      -- solid flag decides continue (C: return !solid)
      pure (gs, scr, (thing.flags &&& MF_SOLID) == 0)
    else
      pure (gs0, scr, (thing.flags &&& MF_SOLID) == 0)

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
                pitCheckThing gs st mi mo
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

/--
`P_CrossSpecialLine` — monster early-out from `p_spec.c`.
Non-player things only activate specials 4/10/39/88/97/125/126; anything
else is a no-op. Player (and those monster-ok specials) stay loud-errors
until the corresponding EV_* is implemented.
-/
def crossSpecialLine (gs : GameState) (lineIdx : Nat) (_side : Nat) (thingIdx : Nat) :
    Except String Unit := do
  match gs.mobjs[thingIdx]?, gs.level.lines[lineIdx]? with
  | none, _ => throw "P_CrossSpecialLine: bad thing"
  | _, none => throw "P_CrossSpecialLine: bad line"
  | some thing, some ld =>
    if thing.player < 0 then
      let spec := ld.special
      let monsterOk :=
        spec == 4 || spec == 10 || spec == 39 || spec == 88
        || spec == 97 || spec == 125 || spec == 126
      if !monsterOk then
        return ()
    throw s!"P_CrossSpecialLine: special crossed on line {lineIdx} (unexpected)"

/-- `P_TryMove`. Returns the CheckPosition scratch (`floatok` / `spechit`) as C globals. -/
def tryMove (gs0 : GameState) (mobjIdx : Nat) (x y : Int32) :
    Except String (GameState × MapScratch × Bool) := do
  let (gs1, scr0, ok) ← checkPosition gs0 mobjIdx x y
  let mut gs := gs1
  let mut scr := { scr0 with floatok := false }
  if !ok then
    return (gs, scr, false)
  match gs.mobjs[mobjIdx]? with
  | none => throw "P_TryMove: bad mobj"
  | some thing =>
    if (thing.flags &&& MF_NOCLIP) == 0 then
      if scr.tmceilingz - scr.tmfloorz < thing.height then
        return (gs, scr, false)
      scr := { scr with floatok := true }
      if (thing.flags &&& MF_TELEPORT) == 0 && scr.tmceilingz - thing.z < thing.height then
        return (gs, scr, false)
      if (thing.flags &&& MF_TELEPORT) == 0 && scr.tmfloorz - thing.z > 24 * FRACUNIT then
        return (gs, scr, false)
      if (thing.flags &&& (MF_DROPOFF ||| MF_FLOAT)) == 0 &&
          scr.tmfloorz - scr.tmdropoffz > 24 * FRACUNIT then
        return (gs, scr, false)
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
      pure (gs, scr, true)

/-- Slide-move scratch (`bestslidefrac` / `tmxmove` / …). -/
structure SlideState where
  slidemoIdx : Nat
  bestslidefrac : Int32
  bestslideline : Int32
  secondslidefrac : Int32
  secondslideline : Int32
  tmxmove : Int32
  tmymove : Int32

/-- `P_HitSlideLine` — clip `tmxmove`/`tmymove` along wall. -/
def hitSlideLine (gs : GameState) (sl : SlideState) (ld : Line) :
    Except String SlideState := do
  if ld.slopetype == ST_HORIZONTAL then
    return { sl with tmymove := 0 }
  if ld.slopetype == ST_VERTICAL then
    return { sl with tmxmove := 0 }
  match gs.mobjs[sl.slidemoIdx]?, gs.level.vertexes[ld.v1.toNat]? with
  | none, _ => throw "P_HitSlideLine: bad slidemo"
  | _, none => throw "P_HitSlideLine: bad v1"
  | some slidemo, some v1 =>
    let side := pointOnLineSide slidemo.x slidemo.y ld v1
    let mut lineangle := pointToAngle2 0 0 ld.dx ld.dy
    if side == 1 then
      lineangle := lineangle + ANG180
    let moveangle := pointToAngle2 0 0 sl.tmxmove sl.tmymove
    let mut deltaangle := moveangle - lineangle
    if deltaangle > ANG180 then
      deltaangle := deltaangle + ANG180
    let lineFine := lineangle >>> ANGLETOFINESHIFT.toUInt32
    let deltaFine := deltaangle >>> ANGLETOFINESHIFT.toUInt32
    let movelen := aproxDistance sl.tmxmove sl.tmymove
    match finecosine[deltaFine.toNat]?, finecosine[lineFine.toNat]?,
          finesine[lineFine.toNat]? with
    | some cosD, some cosL, some sinL =>
      let newlen := fixedMul movelen cosD
      pure {
        sl with
        tmxmove := fixedMul newlen cosL
        tmymove := fixedMul newlen sinL
      }
    | _, _, _ => throw "P_HitSlideLine: fine table OOB"

/-- `PTR_SlideTraverse`. Returns `false` to stop. -/
def ptrSlideTraverse (gs : GameState) (sl0 : SlideState) (inn : Intercept) :
    Except String (SlideState × Bool) := do
  if !inn.isaline then
    throw "PTR_SlideTraverse: not a line?"
  match gs.level.lines[inn.lineIdx]?, gs.mobjs[sl0.slidemoIdx]? with
  | none, _ => throw "PTR_SlideTraverse: bad line"
  | _, none => throw "PTR_SlideTraverse: bad slidemo"
  | some li, some slidemo =>
    let mut blocking := false
    if (li.flags &&& ML_TWOSIDED) == 0 then
      match gs.level.vertexes[li.v1.toNat]? with
      | none => throw "PTR_SlideTraverse: bad v1"
      | some v1 =>
        if pointOnLineSide slidemo.x slidemo.y li v1 != 0 then
          -- don't hit the back side
          return (sl0, true)
        blocking := true
    else
      let op ← lineOpening gs li
      if op.openrange < slidemo.height then
        blocking := true
      else if op.opentop - slidemo.z < slidemo.height then
        blocking := true
      else if op.openbottom - slidemo.z > 24 * FRACUNIT then
        blocking := true
    if !blocking then
      return (sl0, true)
    -- isblocking: closer than best so far?
    if inn.frac < sl0.bestslidefrac then
      pure ({
        sl0 with
        secondslidefrac := sl0.bestslidefrac
        secondslideline := sl0.bestslideline
        bestslidefrac := inn.frac
        bestslideline := inn.lineIdx.toInt32
      }, false)
    else
      pure (sl0, false)

/-- Local stair-step fallback used by `P_SlideMove` (C `stairstep:`). -/
def stairStep (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_SlideMove: bad mobj for stairstep"
  | some mo =>
    let (gs1, _, okY) ← tryMove gs0 mobjIdx mo.x (mo.y + mo.momy)
    if okY then
      pure gs1
    else
      let (gs2, _, _) ← tryMove gs1 mobjIdx (mo.x + mo.momx) mo.y
      pure gs2

/-- One of the three leading-corner `P_PathTraverse` calls in `P_SlideMove`. -/
def traceSlideCorner (gs0 : GameState) (sl0 : SlideState)
    (x1 y1 x2 y2 : Int32) : Except String (GameState × SlideState) := do
  let (gs1, sl1, _) ← pathTraverse gs0 sl0 x1 y1 x2 y2 PT_ADDLINES fun gs st inn => do
    let (st1, ok) ← ptrSlideTraverse gs st inn
    pure (gs, st1, ok)
  pure (gs1, sl1)

/-- `P_SlideMove`. -/
def slideMove (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  let mut gs := gs0
  let mut hitcount : Nat := 0
  let mut done := false
  while !done do
    hitcount := hitcount + 1
    match gs.mobjs[mobjIdx]? with
    | none => throw "P_SlideMove: bad mobj"
    | some mo =>
      if hitcount == 3 then
        gs ← stairStep gs mobjIdx
        done := true
      else
        let (leadx, trailx) :=
          if mo.momx > 0 then (mo.x + mo.radius, mo.x - mo.radius)
          else (mo.x - mo.radius, mo.x + mo.radius)
        let (leady, traily) :=
          if mo.momy > 0 then (mo.y + mo.radius, mo.y - mo.radius)
          else (mo.y - mo.radius, mo.y + mo.radius)
        let mut sl : SlideState := {
          slidemoIdx := mobjIdx
          bestslidefrac := FRACUNIT + 1
          bestslideline := -1
          secondslidefrac := 0
          secondslideline := -1
          tmxmove := 0
          tmymove := 0
        }
        let (gsA, slA) ← traceSlideCorner gs sl leadx leady (leadx + mo.momx) (leady + mo.momy)
        gs := gsA
        sl := slA
        let (gsB, slB) ← traceSlideCorner gs sl trailx leady (trailx + mo.momx) (leady + mo.momy)
        gs := gsB
        sl := slB
        let (gsC, slC) ← traceSlideCorner gs sl leadx traily (leadx + mo.momx) (traily + mo.momy)
        gs := gsC
        sl := slC
        if sl.bestslidefrac == FRACUNIT + 1 then
          gs ← stairStep gs mobjIdx
          done := true
        else
          let bestFudge := sl.bestslidefrac - (0x800 : Int32)
          let mut stair := false
          if bestFudge > 0 then
            let newx := fixedMul mo.momx bestFudge
            let newy := fixedMul mo.momy bestFudge
            let (gs1, _, ok) ← tryMove gs mobjIdx (mo.x + newx) (mo.y + newy)
            gs := gs1
            if !ok then
              stair := true
          if stair then
            gs ← stairStep gs mobjIdx
            done := true
          else
            let mut rem := FRACUNIT - sl.bestslidefrac
            if rem > FRACUNIT then
              rem := FRACUNIT
            if rem <= 0 then
              done := true
            else
              match gs.mobjs[mobjIdx]? with
              | none => throw "P_SlideMove: lost before remainder"
              | some mo3 =>
                sl := {
                  sl with
                  tmxmove := fixedMul mo3.momx rem
                  tmymove := fixedMul mo3.momy rem
                }
                match gs.level.lines[sl.bestslideline.toNatClampNeg]? with
                | none => throw "P_SlideMove: bad bestslideline"
                | some ld =>
                  sl ← hitSlideLine gs sl ld
                  let mo4 := { mo3 with momx := sl.tmxmove, momy := sl.tmymove }
                  gs := { gs with mobjs := setArr gs.mobjs mobjIdx mo4 }
                  let (gs1, _, ok) ← tryMove gs mobjIdx (mo4.x + sl.tmxmove) (mo4.y + sl.tmymove)
                  gs := gs1
                  if ok then
                    done := true
  pure gs

/-- `P_ThingHeightClip`. -/
def thingHeightClip (gs0 : GameState) (mobjIdx : Nat) :
    Except String (GameState × Bool) := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_ThingHeightClip: bad mobj"
  | some thing0 =>
    let onfloor := thing0.z == thing0.floorz
    let (gs1, scr, _) ← checkPosition gs0 mobjIdx thing0.x thing0.y
    match gs1.mobjs[mobjIdx]? with
    | none => throw "P_ThingHeightClip: lost after check"
    | some thing =>
      let mut mo := { thing with floorz := scr.tmfloorz, ceilingz := scr.tmceilingz }
      if onfloor then
        mo := { mo with z := mo.floorz }
      else if mo.z + mo.height > mo.ceilingz then
        mo := { mo with z := mo.ceilingz - mo.height }
      let gs := { gs1 with mobjs := setArr gs1.mobjs mobjIdx mo }
      pure (gs, mo.ceilingz - mo.floorz >= mo.height)

/-- `PIT_ChangeSector` — height-clip miss on corpses / dropped / crush spray loud-errors. -/
def pitChangeSector (gs0 : GameState) (nofit0 : Bool) (crunch : Bool)
    (thingIdx : Nat) (_thing : Mobj) : Except String (GameState × Bool × Bool) := do
  let (gs1, fit) ← thingHeightClip gs0 thingIdx
  if fit then
    return (gs1, nofit0, true)
  match gs1.mobjs[thingIdx]? with
  | none => throw "PIT_ChangeSector: lost"
  | some th =>
    if th.health <= 0 then
      throw "PIT_ChangeSector: corpse gibs not implemented"
    if (th.flags &&& MF_DROPPED) != 0 then
      throw "PIT_ChangeSector: dropped item remove not implemented"
    if (th.flags &&& MF_SHOOTABLE) == 0 then
      return (gs1, nofit0, true)
    if crunch && (gs1.leveltime &&& 3) == 0 then
      throw "PIT_ChangeSector: crush spray not implemented"
    pure (gs1, true, true)

/-- `P_ChangeSector`. -/
def changeSector (gs0 : GameState) (secIdx : Nat) (crunch : Bool) :
    Except String (GameState × Bool) := do
  match gs0.level.sectors[secIdx]? with
  | none => throw "P_ChangeSector: bad sector"
  | some geo =>
    let boxTop := match geo.blockbox[BOXTOP]? with | some v => v | none => (0 : Int32)
    let boxBottom := match geo.blockbox[BOXBOTTOM]? with | some v => v | none => (0 : Int32)
    let boxLeft := match geo.blockbox[BOXLEFT]? with | some v => v | none => (0 : Int32)
    let boxRight := match geo.blockbox[BOXRIGHT]? with | some v => v | none => (0 : Int32)
    let mut gs := gs0
    let mut nofit := false
    let mut x := boxLeft
    while x <= boxRight do
      let mut y := boxBottom
      while y <= boxTop do
        let (gs1, nofit1, _) ← blockThingsIterator gs nofit x y
          fun gs st mi mo => pitChangeSector gs st crunch mi mo
        gs := gs1
        nofit := nofit1
        y := y + 1
      x := x + 1
    pure (gs, nofit)

end Doom.Playsim.Map
