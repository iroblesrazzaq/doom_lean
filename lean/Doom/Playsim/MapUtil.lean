import Doom.Playsim.Bsp
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj

/-!
# Doom.Playsim.MapUtil

`p_maputl.c` open subset: point/box line side, line opening, thing position
link/unlink (sector + blockmap), blockmap iterators.
-/

namespace Doom.Playsim.MapUtil

open Doom.Playsim.Bsp
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

private def idx (x : Int32) : Nat := x.toNatClampNeg

/-- Result of `P_LineOpening`. -/
structure LineOpening where
  opentop : Int32
  openbottom : Int32
  openrange : Int32
  lowfloor : Int32
  deriving Repr

/-- `P_PointOnLineSide` — 0 front, 1 back. -/
def pointOnLineSide (x y : Int32) (ld : Line) (v1 : Vertex) : Nat :=
  if ld.dx == 0 then
    if x <= v1.x then
      if ld.dy > 0 then 1 else 0
    else
      if ld.dy < 0 then 1 else 0
  else if ld.dy == 0 then
    if y <= v1.y then
      if ld.dx < 0 then 1 else 0
    else
      if ld.dx > 0 then 1 else 0
  else
    let dx := x - v1.x
    let dy := y - v1.y
    let left := fixedMul (ld.dy >>> 16) dx
    let right := fixedMul dy (ld.dx >>> 16)
    if right < left then 0 else 1

/--
`P_BoxOnLineSide` — 0 or 1 if box is entirely on one side, `-1` if it crosses.
-/
def boxOnLineSide (tmbbox : Array Int32) (ld : Line) (v1 : Vertex) : Int32 :=
  let boxtop := match tmbbox[BOXTOP]? with | some v => v | none => (0 : Int32)
  let boxbottom := match tmbbox[BOXBOTTOM]? with | some v => v | none => (0 : Int32)
  let boxleft := match tmbbox[BOXLEFT]? with | some v => v | none => (0 : Int32)
  let boxright := match tmbbox[BOXRIGHT]? with | some v => v | none => (0 : Int32)
  let (p1, p2) : Nat × Nat :=
    if ld.slopetype == ST_HORIZONTAL then
      let p1 := if boxtop > v1.y then (1 : Nat) else 0
      let p2 := if boxbottom > v1.y then (1 : Nat) else 0
      if ld.dx < 0 then (p1 ^^^ 1, p2 ^^^ 1) else (p1, p2)
    else if ld.slopetype == ST_VERTICAL then
      let p1 := if boxright < v1.x then (1 : Nat) else 0
      let p2 := if boxleft < v1.x then (1 : Nat) else 0
      if ld.dy < 0 then (p1 ^^^ 1, p2 ^^^ 1) else (p1, p2)
    else if ld.slopetype == ST_POSITIVE then
      (pointOnLineSide boxleft boxtop ld v1, pointOnLineSide boxright boxbottom ld v1)
    else
      -- ST_NEGATIVE
      (pointOnLineSide boxright boxtop ld v1, pointOnLineSide boxleft boxbottom ld v1)
  if p1 == p2 then p1.toInt32 else (-1 : Int32)

/-- `P_LineOpening` using sector runtime heights. -/
def lineOpening (gs : GameState) (ld : Line) : Except String LineOpening := do
  if ld.sidenum1 == (-1 : Int32) then
    pure { opentop := 0, openbottom := 0, openrange := 0, lowfloor := 0 }
  else
    if ld.frontsector < 0 || ld.backsector < 0 then
      throw "P_LineOpening: missing sector"
    match gs.sectors[idx ld.frontsector]?, gs.sectors[idx ld.backsector]? with
    | some front, some back =>
      let opentop :=
        if front.ceilingheight < back.ceilingheight then front.ceilingheight
        else back.ceilingheight
      let (openbottom, lowfloor) :=
        if front.floorheight > back.floorheight then
          (front.floorheight, back.floorheight)
        else
          (back.floorheight, front.floorheight)
      pure {
        opentop
        openbottom
        openrange := opentop - openbottom
        lowfloor
      }
    | _, _ => throw "P_LineOpening: bad sector index"

/-- `P_UnsetThingPosition` — unlink from sector thinglist + blockmap. -/
def unsetThingPosition (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_UnsetThingPosition: bad mobj"
  | some mo0 =>
    let mut gs := gs0
    let mut mo := mo0
    if (mo.flags &&& MF_NOSECTOR) == 0 then
      if mo.snext >= 0 then
        match gs.mobjs[idx mo.snext]? with
        | none => throw "P_UnsetThingPosition: bad snext"
        | some sn =>
          gs := { gs with mobjs := setArr gs.mobjs (idx mo.snext) { sn with sprev := mo.sprev } }
      if mo.sprev >= 0 then
        match gs.mobjs[idx mo.sprev]? with
        | none => throw "P_UnsetThingPosition: bad sprev"
        | some sp =>
          gs := { gs with mobjs := setArr gs.mobjs (idx mo.sprev) { sp with snext := mo.snext } }
      else
        match gs.level.subsectors[mo.subsector.toNat]? with
        | none => throw "P_UnsetThingPosition: bad subsector"
        | some ss =>
          match gs.sectors[ss.sector.toNat]? with
          | none => throw "P_UnsetThingPosition: bad sector"
          | some sec =>
            gs := {
              gs with
              sectors := setArr gs.sectors ss.sector.toNat { sec with thinglist := mo.snext }
            }
    if (mo.flags &&& MF_NOBLOCKMAP) == 0 then
      if mo.bnext >= 0 then
        match gs.mobjs[idx mo.bnext]? with
        | none => throw "P_UnsetThingPosition: bad bnext"
        | some bn =>
          gs := { gs with mobjs := setArr gs.mobjs (idx mo.bnext) { bn with bprev := mo.bprev } }
      if mo.bprev >= 0 then
        match gs.mobjs[idx mo.bprev]? with
        | none => throw "P_UnsetThingPosition: bad bprev"
        | some bp =>
          gs := { gs with mobjs := setArr gs.mobjs (idx mo.bprev) { bp with bnext := mo.bnext } }
      else
        let bmap := gs.level.blockmap
        let blockx := ashrMapBlock (mo.x - bmap.originX)
        let blocky := ashrMapBlock (mo.y - bmap.originY)
        if blockx >= 0 && blockx < bmap.width && blocky >= 0 && blocky < bmap.height then
          let bi := (blocky * bmap.width + blockx).toNatClampNeg
          gs := { gs with blocklinks := setArr gs.blocklinks bi mo.bnext }
    match gs.mobjs[mobjIdx]? with
    | none => throw "P_UnsetThingPosition: mobj lost"
    | some moCur =>
      -- keep link fields as-is until SetThingPosition rewrites them (C leaves stale)
      pure { gs with mobjs := setArr gs.mobjs mobjIdx moCur }

/-- `P_SetThingPosition` — link into subsector, sector thinglist, and blockmap. -/
def setThingPosition (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_SetThingPosition: bad mobj"
  | some mo0 =>
    let ssIdx ← pointInSubsector gs0.level mo0.x mo0.y
    match gs0.level.subsectors[ssIdx.toNat]? with
    | none => throw "P_SetThingPosition: bad subsector"
    | some ss =>
      let mut mo := { mo0 with subsector := ssIdx }
      let mut sectors := gs0.sectors
      let mut mobjs := gs0.mobjs
      let mut blocklinks := gs0.blocklinks
      if (mo.flags &&& MF_NOSECTOR) == 0 then
        let secIdx := ss.sector.toNat
        match sectors[secIdx]? with
        | none => throw "P_SetThingPosition: bad sector"
        | some sec =>
          let head := sec.thinglist
          mo := { mo with sprev := -1, snext := head }
          if head >= 0 then
            match mobjs[idx head]? with
            | none => throw "P_SetThingPosition: bad thinglist head"
            | some headMo =>
              mobjs := setArr mobjs (idx head) { headMo with sprev := mobjIdx.toInt32 }
          sectors := setArr sectors secIdx { sec with thinglist := mobjIdx.toInt32 }
      if (mo.flags &&& MF_NOBLOCKMAP) == 0 then
        let bmap := gs0.level.blockmap
        let blockx := ashrMapBlock (mo.x - bmap.originX)
        let blocky := ashrMapBlock (mo.y - bmap.originY)
        if blockx >= 0 && blockx < bmap.width && blocky >= 0 && blocky < bmap.height then
          let bi := (blocky * bmap.width + blockx).toNatClampNeg
          let head := match blocklinks[bi]? with | some v => v | none => (-1 : Int32)
          mo := { mo with bprev := -1, bnext := head }
          if head >= 0 then
            match mobjs[idx head]? with
            | none => throw "P_SetThingPosition: bad block head"
            | some headMo =>
              mobjs := setArr mobjs (idx head) { headMo with bprev := mobjIdx.toInt32 }
          blocklinks := setArr blocklinks bi mobjIdx.toInt32
        else
          mo := { mo with bnext := -1, bprev := -1 }
      mobjs := setArr mobjs mobjIdx mo
      pure { gs0 with mobjs, sectors, blocklinks }

/--
Walk linedefs in one blockmap cell; `validcount` dedups. Callback returns
`false` to abort early (C PIT false). Extra state `σ` is threaded through.
-/
def blockLinesIterator {σ : Type} (gs0 : GameState) (st0 : σ) (bx byCoord : Int32)
    (f : GameState → σ → Nat → Line → Except String (GameState × σ × Bool)) :
    Except String (GameState × σ × Bool) := do
  let bmap := gs0.level.blockmap
  if bx < 0 || byCoord < 0 || bx >= bmap.width || byCoord >= bmap.height then
    pure (gs0, st0, true)
  else
    let tableIdx := (4 + byCoord * bmap.width + bx).toNatClampNeg
    match bmap.lump[tableIdx]? with
    | none => throw "P_BlockLinesIterator: bad offset table"
    | some offset0 =>
      let mut gs := gs0
      let mut st := st0
      let mut listOff := offset0.toNatClampNeg
      let mut cont := true
      let mut guard : Nat := bmap.lump.size + 1
      while cont && guard > 0 do
        guard := guard - 1
        match bmap.lump[listOff]? with
        | none => throw "P_BlockLinesIterator: list OOB"
        | some lineIdx =>
          if lineIdx == (-1 : Int32) then
            cont := false
          else
            let li := lineIdx.toNatClampNeg
            match gs.level.lines[li]?, gs.lineValidcount[li]? with
            | some ld, some lvc =>
              if lvc == gs.validcount then
                listOff := listOff + 1
              else
                gs := { gs with lineValidcount := setArr gs.lineValidcount li gs.validcount }
                let (gs1, st1, ok) ← f gs st li ld
                gs := gs1
                st := st1
                if !ok then
                  return (gs, st, false)
                listOff := listOff + 1
            | _, _ => throw s!"P_BlockLinesIterator: bad line {lineIdx}"
      pure (gs, st, true)

/-- Walk things in one blockmap cell via `blocklinks`. -/
def blockThingsIterator {σ : Type} (gs0 : GameState) (st0 : σ) (bx byCoord : Int32)
    (f : GameState → σ → Nat → Mobj → Except String (GameState × σ × Bool)) :
    Except String (GameState × σ × Bool) := do
  let bmap := gs0.level.blockmap
  if bx < 0 || byCoord < 0 || bx >= bmap.width || byCoord >= bmap.height then
    pure (gs0, st0, true)
  else
    let bi := (byCoord * bmap.width + bx).toNatClampNeg
    let head := match gs0.blocklinks[bi]? with | some v => v | none => (-1 : Int32)
    let mut gs := gs0
    let mut st := st0
    let mut cur := head
    let mut guard : Nat := gs0.mobjs.size + 1
    while cur >= 0 && guard > 0 do
      guard := guard - 1
      let mi := idx cur
      match gs.mobjs[mi]? with
      | none => throw "P_BlockThingsIterator: bad mobj"
      | some mo =>
        let next := mo.bnext
        let (gs1, st1, ok) ← f gs st mi mo
        gs := gs1
        st := st1
        if !ok then
          return (gs, st, false)
        cur := next
    pure (gs, st, true)

end Doom.Playsim.MapUtil
