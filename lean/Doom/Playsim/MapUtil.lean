import Doom.Playsim.Bsp
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj

/-!
# Doom.Playsim.MapUtil

`p_maputl.c` open subset: point/box line side, line opening, thing position
link/unlink (sector + blockmap), blockmap iterators, and `P_PathTraverse`
(`PT_ADDLINES` / `PT_ADDTHINGS`).
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

/-- `P_AproxDistance` (`p_maputl.c`) — shared by slide/hit and enemy chase. -/
def aproxDistance (dx0 dy0 : Int32) : Int32 :=
  let dx := wabs dx0
  let dy := wabs dy0
  if dx < dy then dx + dy - (dx >>> 1) else dx + dy - (dy >>> 1)

/-- `p_local.h` path-traverse flags. -/
def PT_ADDLINES : Nat := 1
def PT_ADDTHINGS : Nat := 2
def PT_EARLYOUT : Nat := 4

/-- `MAXINTERCEPTS_ORIGINAL` — overflow is a loud-error (no overrun emu). -/
def MAXINTERCEPTS : Nat := 128

/-- `MAPBLOCKSIZE` = 128*FRACUNIT; `MAPBMASK`; `MAPBTOFRAC` = 7. -/
def MAPBLOCKSIZE : Int32 := 128 * FRACUNIT
def MAPBMASK : Int32 := MAPBLOCKSIZE - 1
def MAPBTOFRAC : Nat := 7

/-- `divline_t` — canonical playsim divline (also used by Sight). -/
structure Divline where
  x : Int32
  y : Int32
  dx : Int32
  dy : Int32

/-- One intercept entry (`intercept_t`). -/
structure Intercept where
  frac : Int32
  isaline : Bool
  /-- Linedef index when `isaline`. -/
  lineIdx : Nat
  /-- Mobj index when `PT_ADDTHINGS` and `!isaline`. -/
  thingIdx : Nat

/-- Path-traverse scratch (C globals `trace` / `earlyout` / `intercepts`). -/
structure PathTraverseState where
  trace : Divline
  earlyout : Bool
  intercepts : Array Intercept

/--
`P_InterceptVector` / `P_InterceptVector2` — fractional intercept along `v2`
(`den == 0 → 0`, FixedDiv overflow clamps).
-/
def interceptVector (v2 v1 : Divline) : Int32 :=
  let den := fixedMul (v1.dy >>> 8) v2.dx - fixedMul (v1.dx >>> 8) v2.dy
  if den == 0 then
    0
  else
    let num :=
      fixedMul ((v1.x - v2.x) >>> 8) v1.dy + fixedMul ((v2.y - v1.y) >>> 8) v1.dx
    fixedDiv num den

/-- Alias used by Sight (`P_InterceptVector2`). -/
def interceptVector2 (v2 v1 : Divline) : Int32 := interceptVector v2 v1

/-- `P_PointOnDivlineSide` — 0 front, 1 back (no on-line ternary). -/
def pointOnDivlineSide (x y : Int32) (line : Divline) : Nat :=
  if line.dx == 0 then
    if x <= line.x then
      if line.dy > 0 then 1 else 0
    else
      if line.dy < 0 then 1 else 0
  else if line.dy == 0 then
    if y <= line.y then
      if line.dx < 0 then 1 else 0
    else
      if line.dx > 0 then 1 else 0
  else
    let dx := x - line.x
    let dy := y - line.y
    -- quick sign-bit reject
    if ((line.dy ^^^ line.dx ^^^ dx ^^^ dy) &&& (0x80000000 : Int32)) != 0 then
      if ((line.dy ^^^ dx) &&& (0x80000000 : Int32)) != 0 then 1 else 0
    else
      let left := fixedMul (line.dy >>> 8) (dx >>> 8)
      let right := fixedMul (dy >>> 8) (line.dx >>> 8)
      if right < left then 0 else 1

/-- `P_MakeDivline`. -/
def makeDivline (ld : Line) (v1 : Vertex) : Divline :=
  { x := v1.x, y := v1.y, dx := ld.dx, dy := ld.dy }

/-- `PIT_AddLineIntercepts`. Returns `false` to early-out. -/
def pitAddLineIntercepts (gs : GameState) (pts0 : PathTraverseState) (lineIdx : Nat)
    (ld : Line) : Except String (PathTraverseState × Bool) := do
  match gs.level.vertexes[ld.v1.toNat]?, gs.level.vertexes[ld.v2.toNat]? with
  | none, _ => throw "PIT_AddLineIntercepts: bad v1"
  | _, none => throw "PIT_AddLineIntercepts: bad v2"
  | some v1, some v2 =>
    let trace := pts0.trace
    let (s1, s2) :=
      if trace.dx > 16 * FRACUNIT || trace.dy > 16 * FRACUNIT ||
          trace.dx < (-16) * FRACUNIT || trace.dy < (-16) * FRACUNIT then
        (pointOnDivlineSide v1.x v1.y trace, pointOnDivlineSide v2.x v2.y trace)
      else
        (pointOnLineSide trace.x trace.y ld v1,
         pointOnLineSide (trace.x + trace.dx) (trace.y + trace.dy) ld v1)
    if s1 == s2 then
      return (pts0, true)
    let dl := makeDivline ld v1
    let frac := interceptVector trace dl
    if frac < 0 then
      return (pts0, true)
    if pts0.earlyout && frac < FRACUNIT && ld.backsector < 0 then
      return (pts0, false)
    if pts0.intercepts.size >= MAXINTERCEPTS then
      throw "PIT_AddLineIntercepts: intercepts overrun"
    let pts := {
      pts0 with
      intercepts := pts0.intercepts.push { frac, isaline := true, lineIdx, thingIdx := 0 }
    }
    pure (pts, true)

/-- `PIT_AddThingIntercepts`. Returns `false` to early-out (never in vanilla). -/
def pitAddThingIntercepts (pts0 : PathTraverseState) (thingIdx : Nat) (thing : Mobj) :
    Except String (PathTraverseState × Bool) := do
  let trace := pts0.trace
  let tracepositive := (trace.dx ^^^ trace.dy) > 0
  let (x1, y1, x2, y2) :=
    if tracepositive then
      (thing.x - thing.radius, thing.y + thing.radius,
       thing.x + thing.radius, thing.y - thing.radius)
    else
      (thing.x - thing.radius, thing.y - thing.radius,
       thing.x + thing.radius, thing.y + thing.radius)
  let s1 := pointOnDivlineSide x1 y1 trace
  let s2 := pointOnDivlineSide x2 y2 trace
  if s1 == s2 then
    return (pts0, true)
  let dl : Divline := { x := x1, y := y1, dx := x2 - x1, dy := y2 - y1 }
  let frac := interceptVector trace dl
  if frac < 0 then
    return (pts0, true)
  if pts0.intercepts.size >= MAXINTERCEPTS then
    throw "PIT_AddThingIntercepts: intercepts overrun"
  let pts := {
    pts0 with
    intercepts := pts0.intercepts.push { frac, isaline := false, lineIdx := 0, thingIdx }
  }
  pure (pts, true)

/--
`P_TraverseIntercepts` — sort by ascending `frac`, call `trav` until false or
`frac > maxfrac`. Mutates intercept fracs to `Int32.maxValue` after visit.
-/
def traverseIntercepts {σ : Type} (gs0 : GameState) (pts0 : PathTraverseState) (st0 : σ)
    (maxfrac : Int32)
    (trav : GameState → σ → Intercept → Except String (GameState × σ × Bool)) :
    Except String (GameState × PathTraverseState × σ × Bool) := do
  let mut gs := gs0
  let mut pts := pts0
  let mut st := st0
  let mut count := pts.intercepts.size
  let mut guard : Nat := count + 1
  while count > 0 && guard > 0 do
    guard := guard - 1
    count := count - 1
    let mut dist : Int32 := Int32.maxValue
    let mut bestIdx : Nat := 0
    let mut i : Nat := 0
    while i < pts.intercepts.size do
      match pts.intercepts[i]? with
      | none => throw "P_TraverseIntercepts: bad intercept"
      | some scan =>
        if scan.frac < dist then
          dist := scan.frac
          bestIdx := i
      i := i + 1
    if dist > maxfrac then
      return (gs, pts, st, true)
    match pts.intercepts[bestIdx]? with
    | none => throw "P_TraverseIntercepts: lost best"
    | some inn =>
      let (gs1, st1, ok) ← trav gs st inn
      gs := gs1
      st := st1
      pts := {
        pts with
        intercepts := setArr pts.intercepts bestIdx { inn with frac := Int32.maxValue }
      }
      if !ok then
        return (gs, pts, st, false)
  pure (gs, pts, st, true)

/--
`P_PathTraverse` — blockmap DDA + line/thing intercepts.
-/
def pathTraverse {σ : Type} (gs0 : GameState) (st0 : σ)
    (x1_0 y1_0 x2 y2 : Int32) (flags : Nat)
    (trav : GameState → σ → Intercept → Except String (GameState × σ × Bool)) :
    Except String (GameState × σ × Bool) := do
  let earlyout := (flags &&& PT_EARLYOUT) != 0
  let mut gs := { gs0 with validcount := gs0.validcount + 1 }
  let bmap := gs.level.blockmap
  let mut x1 := x1_0
  let mut y1 := y1_0
  if ((x1 - bmap.originX) &&& MAPBMASK) == 0 then
    x1 := x1 + FRACUNIT
  if ((y1 - bmap.originY) &&& MAPBMASK) == 0 then
    y1 := y1 + FRACUNIT
  let mut pts : PathTraverseState := {
    trace := { x := x1, y := y1, dx := x2 - x1, dy := y2 - y1 }
    earlyout
    intercepts := #[]
  }
  let x1b := x1 - bmap.originX
  let y1b := y1 - bmap.originY
  let x2b := x2 - bmap.originX
  let y2b := y2 - bmap.originY
  let xt1 := ashrMapBlock x1b
  let yt1 := ashrMapBlock y1b
  let xt2 := ashrMapBlock x2b
  let yt2 := ashrMapBlock y2b
  let (mapxstep, partialX, ystep) : Int32 × Int32 × Int32 :=
    if xt2 > xt1 then
      (1, FRACUNIT - ((x1b >>> 7) &&& (FRACUNIT - 1)),
       fixedDiv (y2b - y1b) (wabs (x2b - x1b)))
    else if xt2 < xt1 then
      ((-1 : Int32), (x1b >>> 7) &&& (FRACUNIT - 1),
       fixedDiv (y2b - y1b) (wabs (x2b - x1b)))
    else
      (0, FRACUNIT, 256 * FRACUNIT)
  let mut yintercept := (y1b >>> 7) + fixedMul partialX ystep
  let (mapystep, partialY, xstep) : Int32 × Int32 × Int32 :=
    if yt2 > yt1 then
      (1, FRACUNIT - ((y1b >>> 7) &&& (FRACUNIT - 1)),
       fixedDiv (x2b - x1b) (wabs (y2b - y1b)))
    else if yt2 < yt1 then
      ((-1 : Int32), (y1b >>> 7) &&& (FRACUNIT - 1),
       fixedDiv (x2b - x1b) (wabs (y2b - y1b)))
    else
      (0, FRACUNIT, 256 * FRACUNIT)
  let mut xintercept := (x1b >>> 7) + fixedMul partialY xstep
  let mut mapx := xt1
  let mut mapy := yt1
  let mut count : Nat := 0
  let mut cont := true
  while cont && count < 64 do
    count := count + 1
    if (flags &&& PT_ADDLINES) != 0 then
      let (gs1, pts1, ok) ← blockLinesIterator gs pts mapx mapy
        fun gs st li ld => do
          let (st1, ok) ← pitAddLineIntercepts gs st li ld
          pure (gs, st1, ok)
      gs := gs1
      pts := pts1
      if !ok then
        return (gs, st0, false)
    if (flags &&& PT_ADDTHINGS) != 0 then
      let (gs1, pts1, ok) ← blockThingsIterator gs pts mapx mapy
        fun gs st mi mo => do
          let (st1, ok) ← pitAddThingIntercepts st mi mo
          pure (gs, st1, ok)
      gs := gs1
      pts := pts1
      if !ok then
        return (gs, st0, false)
    if mapx == xt2 && mapy == yt2 then
      cont := false
    else if (yintercept >>> 16) == mapy then
      yintercept := yintercept + ystep
      mapx := mapx + mapxstep
    else if (xintercept >>> 16) == mapx then
      xintercept := xintercept + xstep
      mapy := mapy + mapystep
  let (gs2, _pts2, st2, ok) ← traverseIntercepts gs pts st0 FRACUNIT trav
  pure (gs2, st2, ok)

end Doom.Playsim.MapUtil
