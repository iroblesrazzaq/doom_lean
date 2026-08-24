import Doom.Playsim.Angle
import Doom.Playsim.Bsp
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Render.Clip
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Gfx.Texture
import Doom.Render.Plane
import Doom.Render.Seg
import Doom.Render.Things.Add
import Doom.Render.Types
import Doom.Render.Util
import Doom.Render.View
import Doom.Wad

/-!
# Doom.Render.Bsp

BSP traversal (`r_bsp.c` `R_RenderBSPNode`, `R_AddLine`).
-/

namespace Doom.Render.Bsp

open Doom.Playsim.Angle
open Doom.Playsim.Bsp
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Render.Clip
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Gfx.Texture
open Doom.Render.Plane
open Doom.Render.Seg
open Doom.Render.Things
open Doom.Render.Types
open Doom.Render.Util
open Doom.Render.View
open Doom.Wad

/-- Replay metadata for `renderMaskedSegRange` (`r_segs.c` drawseg fields). -/
structure DrawSegReplayMeta where
  texnum : Nat
  segStart : Int32
  x1 : Int32
  x2 : Int32
  viewz : Int32
  frontFloor : Int32
  frontCeil : Int32
  backFloor : Int32
  backCeil : Int32
  midtextureHeight : Nat
  rowoffset : Int32 := 0
  linedefFlags : Int32 := 0
  lightlevel : Int32 := 128
  v1x : Int32 := 0
  v1y : Int32 := 0
  v2x : Int32 := 0
  v2y : Int32 := 0
  centery : Int32 := defaultCentery
  viewwindowx : Int32 := 0
  viewwindowy : Int32 := 0
  deriving Repr

/-- Accumulated drawseg from `R_StoreWallRange` for post-BSP masked replay. -/
structure DrawSegRecord where
  storeRes : StoreWallResult
  replayMeta : DrawSegReplayMeta
  deriving Repr

structure BspDrawState where
  data : RenderData
  wad : WadDirectory
  fb : Framebuffer
  ceilingclip : Array Int32
  floorclip : Array Int32
  drawSegIdx : Nat
  drawSegs : Array DrawSegRecord := #[]
  planes : VisPlaneState := initVisPlaneState
  floorplane : Option Nat := none
  ceilingplane : Option Nat := none
  spriteCollect : Option SpriteCollectState := none
  /-- Live `GameState.sectors` overlay for `R_Subsector` / `R_StoreWallRange`. -/
  runtimeSectors : Array SectorRuntime := #[]
  /-- C `frontsector` for the current subsector (`r_bsp.c` `frontsector = sub->sector`). -/
  frontsectorIdx : Int32 := 0
  viewSize : ViewSize := {}
  /-- C `extralight` from the console player (`r_main.c`). -/
  extralight : Int32 := 0

def initBspDrawState (data : RenderData) (wad : WadDirectory) (fb : Framebuffer)
    (vs : ViewSize := {}) : BspDrawState :=
  let ceilClip := Array.ofFn (n := 320) (fun _ => (-1 : Int32))
  let floorClip := Array.ofFn (n := 320) (fun _ => vs.viewheight)
  { data, wad, fb, ceilingclip := ceilClip, floorclip := floorClip, drawSegIdx := 0, drawSegs := #[],
    planes := initVisPlaneState, floorplane := none, ceilingplane := none, viewSize := vs }

def buildDrawSegReplayMeta (inp : StoreWallInput) (res : StoreWallResult) (viewz : Int32) :
    DrawSegReplayMeta :=
  {
    texnum := res.midtexture
    segStart := inp.start
    x1 := inp.start
    x2 := inp.stop
    viewz := viewz
    frontFloor := inp.floorheight
    frontCeil := inp.ceilingheight
    backFloor := inp.backFloorheight
    backCeil := inp.backCeilingheight
    midtextureHeight := inp.midtextureHeight
    rowoffset := inp.rowoffset
    linedefFlags := inp.linedefFlags
    lightlevel := inp.lightlevel
    v1x := inp.v1x
    v1y := inp.v1y
    v2x := inp.v2x
    v2y := inp.v2y
    centery := inp.centery
    viewwindowx := inp.viewwindowx
    viewwindowy := inp.viewwindowy
  }

/-- Append a drawseg record; silent no-op when store did not advance or at `maxDrawSegs`. -/
def pushDrawSegRecord (ds : BspDrawState) (inp : StoreWallInput) (res : StoreWallResult)
    (viewz : Int32) : BspDrawState :=
  if res.drawSegIdx <= inp.drawSegIdx then
    ds
  else if ds.drawSegs.size >= maxDrawSegs then
    ds
  else
    { ds with drawSegs := ds.drawSegs.push { storeRes := res, replayMeta := buildDrawSegReplayMeta inp res viewz } }

private def runStoreWallHook (viewz : Int32)
    (buildInp : BspDrawState → Int32 → Int32 → Except String StoreWallInput)
    (store : RenderData → WadDirectory → StoreWallInput → Framebuffer →
      Except String (StoreWallResult × Framebuffer))
    (ds : BspDrawState) (start stop : Int32) : Except String BspDrawState := do
  let inp ← buildInp ds start stop
  let (res, fb') ← store ds.data ds.wad inp ds.fb
  pure (pushDrawSegRecord { ds with
    fb := fb'
    ceilingclip := res.ceilingclip
    floorclip := res.floorclip
    drawSegIdx := res.drawSegIdx
    planes := res.visplanes
    floorplane := res.floorplane
    ceilingplane := res.ceilingplane } inp res viewz)

structure BspAuxState where
  sscount : Nat := 0
  deriving Repr, Inhabited, DecidableEq

private def checkCoordTable : Array (Array Nat) := #[
  #[3, 0, 2, 1], #[3, 0, 2, 0], #[3, 1, 2, 0], #[0, 0, 0, 0],
  #[2, 0, 2, 1], #[0, 0, 0, 0], #[3, 1, 3, 0], #[0, 0, 0, 0],
  #[2, 0, 3, 1], #[2, 1, 3, 1], #[2, 1, 3, 0]
]

private def bspcoordAt (bspcoord : Array Int32) (i : Nat) : Int32 :=
  bspcoord.getD i 0

private def solidSegAt (st : ClipState) (i : Nat) : ClipRange :=
  match st.solidsegs[i]? with
  | some r => r
  | none => ⟨0, 0⟩

/-- Vanilla `0xffff` signed-short `-1` child → subsector 0 (`P_CrossBSPNode` parity). -/
private def resolveSubsectorNum (bspnum : UInt32) : Nat :=
  if bspnum == 0xffff then 0
  else (bspnum &&& (~~~NF_SUBSECTOR)).toNat

private def resolveFlatPicnum (tt : TextureTables) (wad : WadDirectory) (pic : ByteArray) : Nat :=
  match flatNumFromBytes tt wad pic with
  | Except.ok n => n
  | Except.error _ => 0

private def setupSubsectorPlanes (viewz : Int32) (ds : BspDrawState) (sector : Sector) :
    Except String BspDrawState := do
  let tt := ds.data.textureTables
  let floorPicnum := resolveFlatPicnum tt ds.wad sector.floorpic
  let ceilPicnum := resolveFlatPicnum tt ds.wad sector.ceilingpic
  let (planes0, floorIdx) ←
    if sector.floorheight < viewz then do
      let (ps, idx) ← findPlane ds.planes sector.floorheight floorPicnum sector.lightlevel tt.skyFlatNum
      pure (ps, some idx)
    else
      pure (ds.planes, none)
  let (planes1, ceilIdx) ←
    if sector.ceilingheight > viewz || isSkyPic sector.ceilingpic then do
      let (ps, idx) ← findPlane planes0 sector.ceilingheight ceilPicnum sector.lightlevel tt.skyFlatNum
      pure (ps, some idx)
    else
      pure (planes0, none)
  pure { ds with planes := planes1, floorplane := floorIdx, ceilingplane := ceilIdx }

/-- No-op when overlay is absent or has empty sectors. -/
private def collectSprites (ds : BspDrawState) (secIdx : UInt32) :
    Except String BspDrawState :=
  match ds.spriteCollect with
  | none => pure ds
  | some st => do
    let st' ← addSprites ds.data st secIdx.toNat
    pure { ds with spriteCollect := some st' }

structure AddLineMeta where
  rw_angle1 : UInt32
  linedef : UInt32
  frontsector : Int32
  backsector : Int32
  contributed : Bool
  deriving Repr, Inhabited, DecidableEq

private def noContrib (m : AddLineMeta) : AddLineMeta :=
  { m with contributed := false }

private def clipLeft (angle1 _angle2 span clipangle : UInt32) : Option UInt32 :=
  let tspan := angle1 + clipangle
  if tspan > clipangle * 2 then
    let tspan' := tspan - clipangle * 2
    if tspan' >= span then none else some clipangle
  else
    some angle1

private def clipRight (_angle1 angle2 span clipangle : UInt32) : Option UInt32 :=
  let tspan := clipangle - angle2
  if tspan > clipangle * 2 then
    let tspan' := tspan - clipangle * 2
    if tspan' >= span then none else some (0 - clipangle)
  else
    some angle2

private def getVertex (level : LevelData) (idx : UInt32) : Except String Vertex :=
  match level.vertexes[i32Idx idx.toInt32]? with
  | some v => pure v
  | none => throw s!"R_AddLine: vertex {idx} out of range"

private def overlayRuntimeSector (rt : Array SectorRuntime) (idx : Nat) (geo : Sector) : Sector :=
  match rt[idx]? with
  | none => geo
  | some r =>
    { geo with
      floorheight := r.floorheight
      ceilingheight := r.ceilingheight
      floorpic := r.floorpic
      lightlevel := r.lightlevel }

private def getSector (level : LevelData) (idx : Int32) (rt : Array SectorRuntime := #[]) :
    Except String Sector :=
  match level.sectors[i32Idx idx]? with
  | some s => pure (overlayRuntimeSector rt (i32Idx idx) s)
  | none => throw s!"R_AddLine: sector {idx} out of range"

private def getSide (level : LevelData) (seg : Seg) : Except String Side := do
  let ld ← match level.lines[i32Idx seg.linedef.toInt32]? with
    | some l => pure l
    | none => throw s!"R_AddLine: linedef {seg.linedef} out of range"
  let sidenum : Int32 := if seg.side == 0 then ld.sidenum0 else ld.sidenum1
  match level.sides[i32Idx sidenum]? with
  | some sd => pure sd
  | none => throw s!"R_AddLine: side {sidenum} out of range"

private def getLine (level : LevelData) (idx : UInt32) : Except String Line :=
  match level.lines[i32Idx idx.toInt32]? with
  | some l => pure l
  | none => throw s!"R_AddLine: linedef {idx} out of range"

/-- Build `StoreWallInput` for a single-sided solid seg (`r_segs.c` `R_StoreWallRange`). -/
def buildSolidStoreWallInput (ctx : RenderViewCtx) (viewz : Int32) (seg : Seg)
    (level : LevelData) (lineMeta : AddLineMeta) (start stop : Int32) (ds : BspDrawState) :
    Except String StoreWallInput := do
  let v1 ← getVertex level seg.v1
  let v2 ← getVertex level seg.v2
  let sector ← getSector level ds.frontsectorIdx ds.runtimeSectors
  let sd ← getSide level seg
  let ld ← getLine level seg.linedef
  let midtexture ← textureNumForName ds.data.textureTables sd.midtexture
  let midtextureHeight := ds.data.textureTables.height[midtexture]!
  pure {
    viewx := ctx.viewx
    viewy := ctx.viewy
    viewz := viewz
    viewangle := ctx.viewangle
    viewwidth := ds.viewSize.viewwidth
    viewheight := ds.viewSize.viewheight
    centery := ds.viewSize.centery
    projection := ds.viewSize.projection
    viewwindowx := ds.viewSize.viewwindowx
    viewwindowy := ds.viewSize.viewwindowy
    extralight := ds.extralight
    rwAngle1 := lineMeta.rw_angle1
    segAngle := seg.angle
    segOffset := seg.offset
    v1x := v1.x
    v1y := v1.y
    v2x := v2.x
    v2y := v2.y
    linedefFlags := ld.flags
    textureoffset := sd.textureoffset
    rowoffset := sd.rowoffset
    midtexture := midtexture
    midtextureHeight := midtextureHeight
    ceilingheight := sector.ceilingheight
    floorheight := sector.floorheight
    ceilingpic := sector.ceilingpic
    floorpic := sector.floorpic
    lightlevel := sector.lightlevel
    start := start
    stop := stop
    xtoviewangle := ctx.mapping.xtoviewangle
    drawSegIdx := ds.drawSegIdx
    ceilingclip := ds.ceilingclip
    floorclip := ds.floorclip
    visplanes := ds.planes
    floorplane := ds.floorplane
    ceilingplane := ds.ceilingplane
  }

private def resolveTextureNum (tt : TextureTables) (name : ByteArray) : Except String Nat :=
  if isNoTexture name || name.isEmpty then
    pure 0
  else
    textureNumForName tt name

/-- Build `StoreWallInput` for a two-sided pass seg (`r_segs.c` two-sided `R_StoreWallRange`). -/
def buildTwoSidedStoreWallInput (ctx : RenderViewCtx) (viewz : Int32) (seg : Seg)
    (level : LevelData) (lineMeta : AddLineMeta) (start stop : Int32) (ds : BspDrawState) :
    Except String StoreWallInput := do
  let v1 ← getVertex level seg.v1
  let v2 ← getVertex level seg.v2
  let frontSector ← getSector level ds.frontsectorIdx ds.runtimeSectors
  let backSector ← getSector level seg.backsector ds.runtimeSectors
  let sd ← getSide level seg
  let ld ← getLine level seg.linedef
  let midtexture ← resolveTextureNum ds.data.textureTables sd.midtexture
  let midtextureHeight := ds.data.textureTables.height[midtexture]!
  let toptexture ← resolveTextureNum ds.data.textureTables sd.toptexture
  let toptextureHeight := ds.data.textureTables.height[toptexture]!
  let bottomtexture ← resolveTextureNum ds.data.textureTables sd.bottomtexture
  let bottomtextureHeight := ds.data.textureTables.height[bottomtexture]!
  pure {
    viewx := ctx.viewx
    viewy := ctx.viewy
    viewz := viewz
    viewangle := ctx.viewangle
    viewwidth := ds.viewSize.viewwidth
    viewheight := ds.viewSize.viewheight
    centery := ds.viewSize.centery
    projection := ds.viewSize.projection
    viewwindowx := ds.viewSize.viewwindowx
    viewwindowy := ds.viewSize.viewwindowy
    extralight := ds.extralight
    rwAngle1 := lineMeta.rw_angle1
    segAngle := seg.angle
    segOffset := seg.offset
    v1x := v1.x
    v1y := v1.y
    v2x := v2.x
    v2y := v2.y
    linedefFlags := ld.flags
    textureoffset := sd.textureoffset
    rowoffset := sd.rowoffset
    midtexture := midtexture
    midtextureHeight := midtextureHeight
    ceilingheight := frontSector.ceilingheight
    floorheight := frontSector.floorheight
    ceilingpic := frontSector.ceilingpic
    floorpic := frontSector.floorpic
    lightlevel := frontSector.lightlevel
    backFloorheight := backSector.floorheight
    backCeilingheight := backSector.ceilingheight
    backFloorpic := backSector.floorpic
    backCeilingpic := backSector.ceilingpic
    backLightlevel := backSector.lightlevel
    toptexture := toptexture
    bottomtexture := bottomtexture
    toptextureHeight := toptextureHeight
    bottomtextureHeight := bottomtextureHeight
    start := start
    stop := stop
    xtoviewangle := ctx.mapping.xtoviewangle
    drawSegIdx := ds.drawSegIdx
    ceilingclip := ds.ceilingclip
    floorclip := ds.floorclip
    visplanes := ds.planes
    floorplane := ds.floorplane
    ceilingplane := ds.ceilingplane
  }

private def twoSidedDrawHook (ctx : RenderViewCtx) (viewz : Int32) (seg : Seg)
    (level : LevelData) (lineMeta : AddLineMeta) : StoreWallHook BspDrawState :=
  fun ds start stop =>
    runStoreWallHook viewz
      (fun ds' s e => buildTwoSidedStoreWallInput ctx viewz seg level lineMeta s e ds')
      storeWallTwoSided ds start stop

private def clipPassWithDraw (ctx : RenderViewCtx) (viewz : Int32) (st : ClipState) (seg : Seg)
    (level : LevelData) (lineMeta : AddLineMeta) (x1 x2 : Int32) (draw : Option BspDrawState) :
    Except String (ClipState × AddLineMeta × Option BspDrawState) := do
  match draw with
  | none =>
    let st' ← clipPassWallSegment st x1 x2
    pure (st', lineMeta, none)
  | some ds0 =>
    let hook := twoSidedDrawHook ctx viewz seg level lineMeta
    let (st', ds') ← clipPassWallSegmentWith st x1 x2 ds0 (some hook)
    pure (st', lineMeta, some ds')

/-- Closed doors are clip-solid but still `R_StoreWallRange` two-sided (`r_bsp.c` `clipsolid`). -/
private def clipSolidWithTwoSidedDraw (ctx : RenderViewCtx) (viewz : Int32) (st : ClipState)
    (seg : Seg) (level : LevelData) (lineMeta : AddLineMeta) (x1 x2 : Int32)
    (draw : Option BspDrawState) :
    Except String (ClipState × AddLineMeta × Option BspDrawState) := do
  match draw with
  | none =>
    let st' ← clipSolidWallSegment st x1 x2
    pure (st', lineMeta, none)
  | some ds0 =>
    let hook := twoSidedDrawHook ctx viewz seg level lineMeta
    let (st', ds') ← clipSolidWallSegmentWith st x1 x2 ds0 (some hook)
    pure (st', lineMeta, some ds')

private def isClosedDoor (front back : Sector) : Bool :=
  back.ceilingheight <= front.floorheight || back.floorheight >= front.ceilingheight

private def isWindow (front back : Sector) : Bool :=
  back.ceilingheight != front.ceilingheight || back.floorheight != front.floorheight

private def isEmptyTrigger (front back : Sector) (sd : Side) : Bool :=
  front.ceilingpic == back.ceilingpic &&
    front.floorpic == back.floorpic &&
    front.lightlevel == back.lightlevel &&
    isNoTexture sd.midtexture

/-- `R_AddLine` — clip classifier; draws single-sided solids when `draw` is `some`. -/
def addLine (ctx : RenderViewCtx) (viewz : Int32) (st : ClipState) (seg : Seg)
    (level : LevelData) (draw : Option BspDrawState := none) :
    Except String (ClipState × AddLineMeta × Option BspDrawState) := do
  let v1 ← getVertex level seg.v1
  let v2 ← getVertex level seg.v2
  let angle1 := pointToAngle2 ctx.viewx ctx.viewy v1.x v1.y
  let angle2 := pointToAngle2 ctx.viewx ctx.viewy v2.x v2.y
  let span := angle1 - angle2
  let frontIdx :=
    match draw with
    | some ds => ds.frontsectorIdx
    | none => seg.frontsector
  let baseMeta : AddLineMeta := {
    rw_angle1 := angle1
    linedef := seg.linedef
    frontsector := frontIdx
    backsector := seg.backsector
    contributed := false
  }
  if span >= ANG180 then
    return (st, noContrib baseMeta, draw)

  let clipangle := ctx.mapping.clipangle
  let mut ang1 := angle1 - ctx.viewangle
  let mut ang2 := angle2 - ctx.viewangle

  match clipLeft ang1 ang2 span clipangle with
  | none => return (st, noContrib baseMeta, draw)
  | some a1 => ang1 := a1

  match clipRight ang1 ang2 span clipangle with
  | none => return (st, noContrib baseMeta, draw)
  | some a2 => ang2 := a2

  let idx1 := ((ang1 + ANG90) >>> ANGLETOFINESHIFT.toUInt32).toNat
  let idx2 := ((ang2 + ANG90) >>> ANGLETOFINESHIFT.toUInt32).toNat
  let x1 := ctx.mapping.viewangletox.getD idx1 0
  let x2 := ctx.mapping.viewangletox.getD idx2 0
  if x1 == x2 then
    return (st, noContrib baseMeta, draw)

  if seg.backsector < 0 then
    match draw with
    | none =>
      let st' ← clipSolidWallSegment st x1 (x2 - 1)
      return (st', { baseMeta with contributed := true }, none)
    | some ds0 =>
      let lineMeta := { baseMeta with contributed := true }
      let drawHook : StoreWallHook BspDrawState := fun ds start stop =>
        runStoreWallHook viewz
          (fun ds' s e => buildSolidStoreWallInput ctx viewz seg level lineMeta s e ds')
          storeWallSolid ds start stop
      let (st', ds') ← clipSolidWallSegmentWith st x1 (x2 - 1) ds0 (some drawHook)
      return (st', lineMeta, some ds')
  else
    let rt := match draw with | some d => d.runtimeSectors | none => #[]
    let front ← getSector level frontIdx rt
    let back ← getSector level seg.backsector rt
    if isClosedDoor front back then
      let lineMeta := { baseMeta with contributed := true }
      clipSolidWithTwoSidedDraw ctx viewz st seg level lineMeta x1 (x2 - 1) draw
    else if isWindow front back then
      let lineMeta := { baseMeta with contributed := true }
      clipPassWithDraw ctx viewz st seg level lineMeta x1 (x2 - 1) draw
    else do
      let sd ← getSide level seg
      if isEmptyTrigger front back sd then
        return (st, noContrib baseMeta, draw)
      else
        let lineMeta := { baseMeta with contributed := true }
        clipPassWithDraw ctx viewz st seg level lineMeta x1 (x2 - 1) draw

private def finishCheckBBox (ctx : RenderViewCtx) (st : ClipState) (ang1 ang2 : UInt32) : Bool :=
  Id.run do
    let idx1 := ((ang1 + ANG90) >>> ANGLETOFINESHIFT.toUInt32).toNat
    let idx2 := ((ang2 + ANG90) >>> ANGLETOFINESHIFT.toUInt32).toNat
    let sx1 := ctx.mapping.viewangletox.getD idx1 0
    let sx2raw := ctx.mapping.viewangletox.getD idx2 0
    if sx1 == sx2raw then
      return false
    let sx2 := sx2raw - 1
    let mut segIdx := 0
    while segIdx < st.newend && (solidSegAt st segIdx).last < sx2 do
      segIdx := segIdx + 1
    if segIdx < st.newend then
      let seg := solidSegAt st segIdx
      return !(sx1 >= seg.first && sx2 <= seg.last)
    return true

private def checkBBoxClipAngles (ctx : RenderViewCtx) (st : ClipState)
    (angle1 angle2 span : UInt32) : Bool := Id.run do
  if span >= ANG180 then
    return true
  let clipangle := ctx.mapping.clipangle
  let mut ang1 := angle1
  let mut ang2 := angle2
  let mut tspan := ang1 + clipangle
  if tspan > clipangle * 2 then
    tspan := tspan - clipangle * 2
    if tspan >= span then
      return false
    ang1 := clipangle
  tspan := clipangle - ang2
  if tspan > clipangle * 2 then
    tspan := tspan - clipangle * 2
    if tspan >= span then
      return false
    ang2 := 0 - clipangle
  finishCheckBBox ctx st ang1 ang2

/-- `R_CheckBBox` — returns true when some part of the bbox might be visible. -/
def checkBBox (ctx : RenderViewCtx) (st : ClipState) (bspcoord : Array Int32) : Bool :=
  let boxx :=
    if ctx.viewx <= bspcoordAt bspcoord BOXLEFT then 0
    else if ctx.viewx < bspcoordAt bspcoord BOXRIGHT then 1
    else 2
  let boxy :=
    if ctx.viewy >= bspcoordAt bspcoord BOXTOP then 0
    else if ctx.viewy > bspcoordAt bspcoord BOXBOTTOM then 1
    else 2
  let boxpos := (boxy <<< 2) + boxx
  if boxpos == 5 then
    true
  else
    match checkCoordTable[boxpos]? with
    | none => true
    | some cc =>
      let x1 := bspcoordAt bspcoord (cc.getD 0 0)
      let y1 := bspcoordAt bspcoord (cc.getD 1 0)
      let x2 := bspcoordAt bspcoord (cc.getD 2 0)
      let y2 := bspcoordAt bspcoord (cc.getD 3 0)
      let angle1 := pointToAngle2 ctx.viewx ctx.viewy x1 y1 - ctx.viewangle
      let angle2 := pointToAngle2 ctx.viewx ctx.viewy x2 y2 - ctx.viewangle
      checkBBoxClipAngles ctx st angle1 angle2 (angle1 - angle2)

/-- `R_Subsector` — classify segs via `addLine`; set floor/ceiling visplanes per sector. -/
def subsector (viewz : Int32) (ctx : RenderViewCtx) (st : ClipState) (aux : BspAuxState)
    (num : Nat) (level : LevelData) (draw : Option BspDrawState := none) :
    Except String (ClipState × BspAuxState × Option BspDrawState) := do
  if num >= level.subsectors.size then
    throw s!"R_Subsector: ss {num} with numss = {level.subsectors.size}"
  let sub ← match level.subsectors[num]? with
    | some s => pure s
    | none => throw s!"R_Subsector: ss {num} with numss = {level.subsectors.size}"
  let rt := match draw with | some d => d.runtimeSectors | none => #[]
  let sector ← getSector level sub.sector.toInt32 rt
  let aux' := { aux with sscount := aux.sscount + 1 }
  let mut drawSt := draw
  match drawSt with
  | none => pure ()
  | some ds0 =>
    let ds1 ← setupSubsectorPlanes viewz ds0 sector
    let ds2 ← collectSprites ds1 sub.sector
    drawSt := some { ds2 with frontsectorIdx := sub.sector.toInt32 }
  let mut clip := st
  let mut count := sub.numsegs.toNat
  let mut segIdx := sub.firstseg.toNat
  while count > 0 do
    let seg ← match level.segs[segIdx]? with
      | some s => pure s
      | none => throw s!"R_Subsector: seg {segIdx} out of range"
    let (clip', _, draw') ← addLine ctx viewz clip seg level drawSt
    clip := clip'
    drawSt := draw'
    segIdx := segIdx + 1
    count := count - 1
  if clip.newend > 33 then
    throw "solidsegs overflow"
  pure (clip, aux', drawSt)

private def renderBspNodeFuel (viewz : Int32) (ctx : RenderViewCtx) (st : ClipState)
    (aux : BspAuxState) (level : LevelData) (bspnum : UInt32) (fuel : Nat)
    (draw : Option BspDrawState) :
    Except String (ClipState × BspAuxState × Option BspDrawState) :=
  match fuel with
  | 0 => throw "R_RenderBSPNode: BSP walk overflow"
  | fuel' + 1 =>
    if (bspnum &&& NF_SUBSECTOR) != 0 then
      subsector viewz ctx st aux (resolveSubsectorNum bspnum) level draw
    else
      match level.nodes[bspnum.toNat]? with
      | none => throw s!"R_RenderBSPNode: bad node {bspnum}"
      | some node =>
        let side := pointOnSide ctx.viewx ctx.viewy node
        let nearChild := if side == 0 then node.child0 else node.child1
        let farChild := if side == 0 then node.child1 else node.child0
        let farBBox := if side == 0 then node.bbox1 else node.bbox0
        do
          let (st1, aux1, draw1) ← renderBspNodeFuel viewz ctx st aux level nearChild fuel' draw
          if checkBBox ctx st1 farBBox then
            renderBspNodeFuel viewz ctx st1 aux1 level farChild fuel' draw1
          else
            pure (st1, aux1, draw1)

/-- Build `R_RenderMaskedSegRange` input from a recorded drawseg over `[x1, x2]`. -/
def maskedSegReplayInput (rec : DrawSegRecord) (x1 x2 : Int32) : MaskedSegReplayInput :=
  let m := rec.replayMeta
  {
    res := rec.storeRes
    texnum := m.texnum
    segStart := m.segStart
    x1 := x1
    x2 := x2
    viewz := m.viewz
    frontFloor := m.frontFloor
    frontCeil := m.frontCeil
    backFloor := m.backFloor
    backCeil := m.backCeil
    midtextureHeight := m.midtextureHeight
    rowoffset := m.rowoffset
    linedefFlags := m.linedefFlags
    lightlevel := m.lightlevel
    v1x := m.v1x
    v1y := m.v1y
    v2x := m.v2x
    v2y := m.v2y
    centery := m.centery
    viewwindowx := m.viewwindowx
    viewwindowy := m.viewwindowy
  }

private def replayMaskedDrawSegAt (data : RenderData) (wad : WadDirectory)
    (ds : BspDrawState) (i : Nat) : Except String BspDrawState := do
  match ds.drawSegs[i]? with
  | none => pure ds
  | some rec =>
    if !rec.storeRes.maskedtexture then
      pure ds
    else
      let inp := maskedSegReplayInput rec rec.replayMeta.x1 rec.replayMeta.x2
      let (res', fb') ← renderMaskedSegRange data wad inp ds.fb
      pure { ds with
        fb := fb'
        drawSegs := Doom.Render.Util.arrSet ds.drawSegs i { rec with storeRes := res' }
      }

/-- Post-BSP masked midtexture replay (`r_segs.c` reverse drawseg walk). -/
def replayMaskedDrawSegs (data : RenderData) (wad : WadDirectory)
    (ds : BspDrawState) : Except String BspDrawState := do
  let mut state := ds
  let mut i := ds.drawSegs.size
  while i > 0 do
    i := i - 1
    state ← replayMaskedDrawSegAt data wad state i
  pure state

/-- `R_RenderBSPNode` — recursive BSP walk from a child reference or root index. -/
def renderBspNode (viewz : Int32) (ctx : RenderViewCtx) (st : ClipState) (aux : BspAuxState)
    (level : LevelData) (bspnum : UInt32) (draw : Option BspDrawState := none) :
    Except String (ClipState × BspAuxState × Option BspDrawState) :=
  renderBspNodeFuel viewz ctx st aux level bspnum (level.nodes.size + level.subsectors.size + 2) draw

/-- Post-BSP frame payload for `R_DrawPlanes` / `R_DrawMasked` (`r_main.c` `R_RenderPlayerView`). -/
structure BspFrameResult where
  fb : Framebuffer
  planes : VisPlaneState
  view : RenderViewCtx
  viewz : Int32
  drawSegIdx : Nat
  drawSegs : Array DrawSegRecord := #[]
  visSprites : Array VisSprite := #[]

/-- `R_RenderPlayerView` BSP entry: build view context from the console player. -/
def renderBspFromGame (data : RenderData) (wad : WadDirectory) (gs : GameState)
    (fb : Framebuffer) (vs : ViewSize := {}) : Except String BspFrameResult := do
  let pl ← match gs.players[gs.consoleplayer]? with
    | some p => pure p
    | none => throw "R_RenderBSPNode: no player"
  let mo ← match gs.mobjs[pl.mo.toNatClampNeg]? with
    | some m => pure m
    | none => throw "R_RenderBSPNode: no view mobj"
  let mapping :=
    if vs.mapping.xtoviewangle.size == 0 then
      initViewMapping vs.viewwidth vs.centerx
    else
      vs.mapping
  let ctx : RenderViewCtx := {
    viewx := mo.x
    viewy := mo.y
    viewangle := mo.angle
    mapping := mapping
  }
  let st := clearClipSegs (clearDrawSegs { emptyClipState with viewwidth := vs.viewwidth })
  let root := (gs.level.nodes.size - 1).toUInt32
  let collect :=
    initSpriteCollect gs.sectors gs.mobjs mo.x mo.y pl.viewz mo.angle pl.extralight
      none vs.viewwidth vs.centerxfrac vs.projection
  let ds0 := { initBspDrawState data wad fb vs with
    spriteCollect := some collect
    runtimeSectors := gs.sectors
    extralight := pl.extralight }
  let (_, _, draw) ← renderBspNode pl.viewz ctx st {} gs.level root (some ds0)
  match draw with
  | none =>
    pure {
      fb := fb
      planes := initVisPlaneState
      view := ctx
      viewz := pl.viewz
      drawSegIdx := 0
      drawSegs := #[]
      visSprites := #[]
    }
  | some ds =>
    let visSprites :=
      match ds.spriteCollect with
      | none => #[]
      | some sc => sc.visSprites
    pure {
      fb := ds.fb
      planes := ds.planes
      view := ctx
      viewz := pl.viewz
      drawSegIdx := ds.drawSegIdx
      drawSegs := ds.drawSegs
      visSprites
    }

end Doom.Render.Bsp
