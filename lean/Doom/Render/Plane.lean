import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Level
import Doom.Playsim.Tables
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Draw
import Doom.Render.Gfx.Texture
import Doom.Render.Tables
import Doom.Render.Types
import Doom.Render.Util
import Doom.Render.View
import Doom.Wad

/-!
# Doom.Render.Plane

Floor/ceiling visplane accumulation (`r_plane.c` `R_FindPlane`, `R_CheckPlane`)
and `R_DrawPlanes` sky-column / flat raster.
-/

namespace Doom.Render.Plane

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Level
open Doom.Playsim.Tables
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Draw
open Doom.Render.Gfx.Texture
open Doom.Render.Tables hiding lightLevels maxLightZ
open Doom.Render.Types
open Doom.Render.Util
open Doom.Render.View
open Doom.Wad

def screenWidth : Nat := 320

private def visplaneSentinel : UInt8 := 255

/-- `visplane_t` column span storage (`r_defs.h`). -/
structure VisPlane where
  height : Int32
  picnum : Nat
  lightlevel : Int32
  minx : Int32
  maxx : Int32
  top : Array UInt8
  bottom : Array UInt8
  deriving Repr, Inhabited

/-- Visplane pool (`r_plane.c` `visplanes` / `lastvisplane`). -/
structure VisPlaneState where
  visplanes : Array VisPlane
  lastvisplane : Nat
  deriving Repr, Inhabited

def emptyVisPlaneColumn : Array UInt8 :=
  Array.ofFn (n := screenWidth) (fun _ => visplaneSentinel)

/-- `visplane_t.bottom` is BSS 0; only `top` is `memset` 0xff (`r_plane.c` `R_FindPlane`). -/
def emptyVisPlaneBottom : Array UInt8 :=
  Array.ofFn (n := screenWidth) (fun _ => (0 : UInt8))

def freshVisPlane (height : Int32) (picnum : Nat) (lightlevel : Int32) : VisPlane :=
  {
    height := height
    picnum := picnum
    lightlevel := lightlevel
    minx := Int32.ofNat screenWidth
    maxx := (-1 : Int32)
    top := emptyVisPlaneColumn
    bottom := emptyVisPlaneBottom
  }

def initVisPlaneState : VisPlaneState :=
  { visplanes := #[], lastvisplane := 0 }

private def normalizePlaneParams (height lightlevel : Int32) (picnum skyFlatNum : Nat) :
    Int32 × Nat × Int32 :=
  if picnum == skyFlatNum then
    (0, picnum, 0)
  else
    (height, picnum, lightlevel)

private def visPlaneAt (st : VisPlaneState) (idx : Nat) : Option VisPlane :=
  st.visplanes[idx]?

private def setVisPlaneAt (st : VisPlaneState) (idx : Nat) (vp : VisPlane) : VisPlaneState :=
  { st with visplanes := Doom.Render.Util.arrSet st.visplanes idx vp }

private def pushVisPlane (st : VisPlaneState) (vp : VisPlane) : VisPlaneState :=
  { visplanes := st.visplanes.push vp, lastvisplane := st.lastvisplane + 1 }

private def i32ToU8 (v : Int32) : UInt8 :=
  UInt8.ofNat (i32ToNat v &&& 0xff)

def markCeilingColumn (st : VisPlaneState) (plIdx : Nat) (x top bottom : Int32) : VisPlaneState :=
  match visPlaneAt st plIdx with
  | none => st
  | some vp =>
    let xn := i32ToNat x
    let vp' := {
      vp with
        top := arrSetU8 vp.top xn (i32ToU8 top)
        bottom := arrSetU8 vp.bottom xn (i32ToU8 bottom)
    }
    setVisPlaneAt st plIdx vp'

def markFloorColumn (st : VisPlaneState) (plIdx : Nat) (x top bottom : Int32) : VisPlaneState :=
  markCeilingColumn st plIdx x top bottom

/--
`R_FindPlane` — reuse or allocate a visplane for `(height, picnum, lightlevel)`.
Sky flats normalize height/light to zero (`r_plane.c`).
-/
def findPlane (st : VisPlaneState) (height : Int32) (picnum : Nat) (lightlevel : Int32)
    (skyFlatNum : Nat) : Except String (VisPlaneState × Nat) := do
  let (height', picnum', lightlevel') := normalizePlaneParams height lightlevel picnum skyFlatNum
  let mut i : Nat := 0
  while i < st.lastvisplane do
    match st.visplanes[i]? with
    | none => pure ()
    | some check =>
      if height' == check.height && picnum' == check.picnum && lightlevel' == check.lightlevel then
        return (st, i)
    i := i + 1
  if st.lastvisplane >= maxVisPlanes then
    throw "R_FindPlane: no more visplanes"
  let vp := freshVisPlane height' picnum' lightlevel'
  let st' := pushVisPlane st vp
  pure (st', st.lastvisplane)

/--
`R_CheckPlane` — split when the span overlaps an existing column mark (`r_plane.c`).
-/
def checkPlane (st : VisPlaneState) (plIdx : Nat) (start stop : Int32) : Except String (VisPlaneState × Nat) := do
  let pl ← match visPlaneAt st plIdx with
    | none => throw "R_CheckPlane: bad visplane index"
    | some p => pure p
  let (intrl, unionl) :=
    if start < pl.minx then (pl.minx, start) else (start, pl.minx)
  let (intrh, unionh) :=
    if stop > pl.maxx then (pl.maxx, stop) else (stop, pl.maxx)
  let mut x := intrl
  let mut overlapped := false
  while x <= intrh do
    if pl.top.getD (i32ToNat x) visplaneSentinel != visplaneSentinel then
      overlapped := true
      x := intrh + 1
    else
      x := x + 1
  if !overlapped then
    let pl' := { pl with minx := unionl, maxx := unionh }
    pure (setVisPlaneAt st plIdx pl', plIdx)
  else
    if st.lastvisplane >= maxVisPlanes then
      throw "R_CheckPlane: no more visplanes"
    let vp := {
      (freshVisPlane pl.height pl.picnum pl.lightlevel) with
        minx := start
        maxx := stop
    }
    let st' := pushVisPlane st vp
    pure (st', st.lastvisplane)

/-- Plane span raster globals (`r_plane.c` / `r_main.c`). -/
structure PlaneDrawCtx where
  yslope : Array Int32
  distscale : Array Int32
  basexscale : Int32
  baseyscale : Int32
  spanstart : Array Int32
  cachedheight : Array Int32
  cacheddistance : Array Int32
  cachedxstep : Array Int32
  cachedystep : Array Int32
  planeheight : Int32
  planezlight : Array Nat
  drawViewWidth : Nat := screenWidth
  drawViewHeight : Nat := 168
  viewwindowx : Int32 := 0
  viewwindowy : Int32 := 0
  centery : Int32 := 84
  deriving Repr, Inhabited

def initPlaneDrawCtx (planezlight : Array Nat := #[]) : PlaneDrawCtx :=
  {
    yslope := Array.replicate screenHeight 0
    distscale := Array.replicate screenWidth 0
    basexscale := 0
    baseyscale := 0
    spanstart := Array.replicate screenHeight 0
    cachedheight := Array.replicate screenHeight 0
    cacheddistance := Array.replicate screenHeight 0
    cachedxstep := Array.replicate screenHeight 0
    cachedystep := Array.replicate screenHeight 0
    planeheight := 0
    planezlight := planezlight
  }

/--
`r_main.c` slope tables (`yslope`, `distscale`) for the active view size.
-/
def initPlaneSlopeTables (ctx : PlaneDrawCtx) (viewwidth viewheight : Int32)
    (xtoviewangle : Array UInt32) (detailshift : Nat := detailShift) : PlaneDrawCtx :=
  let half := viewheight / 2
  let yslope :=
    Array.ofFn (n := i32ToNat viewheight) fun i =>
      let dy := ((Int32.ofNat i - half) <<< 16) + FRACUNIT / 2
      let dy' := cabs dy
      let vwScaled := (i32ToNat viewwidth) <<< detailshift
      fixedDiv (Int32.ofNat (vwScaled / 2 * i32ToNat FRACUNIT)) dy'
  let distscale :=
    Array.ofFn (n := i32ToNat viewwidth) fun i =>
      let ang := xtoviewangle.getD i 0 >>> ANGLETOFINESHIFT.toUInt32
      let cosadj := cabs (finecosine.getD ang.toNat 0)
      fixedDiv FRACUNIT cosadj
  { ctx with yslope, distscale, drawViewWidth := i32ToNat viewwidth, drawViewHeight := i32ToNat viewheight }

/--
`R_ClearPlanes` texture-calculation tail (`r_plane.c`): zero `cachedheight`,
recompute `basexscale` / `baseyscale`.
-/
def clearPlaneDraw (ctx : PlaneDrawCtx) (viewangle : UInt32) (centerxfrac : Int32) : PlaneDrawCtx :=
  let angleIdx := ((viewangle - ANG90) >>> ANGLETOFINESHIFT.toUInt32).toNat
  let basexscale := fixedDiv (finecosine.getD angleIdx 0) centerxfrac
  let baseyscale := -fixedDiv (finesine.getD angleIdx 0) centerxfrac
  {
    ctx with
      cachedheight := Array.replicate screenHeight 0
      basexscale := basexscale
      baseyscale := baseyscale
  }

private def mapPlaneRangeError (x1 x2 y : Int32) : String :=
  s!"R_MapPlane: {x1}, {x2} at {y}"

private def planeColormapLevel (ctx : PlaneDrawCtx) (distance : Int32) : Nat :=
  let raw := (distance.toUInt32 >>> lightZShift.toUInt32).toNat
  let idx := if raw >= maxLightZ then maxLightZ - 1 else raw
  ctx.planezlight.getD idx 0

/--
`R_MapPlane` — texture-map one horizontal span and blit via `drawSpan`.
-/
def mapPlane (data : RenderData) (ctx : PlaneDrawCtx) (fb : Framebuffer)
    (viewx viewy : Int32) (viewangle : UInt32) (xtoviewangle : Array UInt32)
    (x1 x2 y : Int32) (source : ByteArray)
    (viewwidth : Nat := ctx.drawViewWidth)
    (viewheight : Nat := ctx.drawViewHeight) : Except String (PlaneDrawCtx × Framebuffer) := do
  if x2 < x1 || x1 < 0 || x2 >= Int32.ofNat viewwidth || i32AsU32 y > viewheight.toUInt32 then
    throw (mapPlaneRangeError x1 x2 y)
  else do
    let yNat := i32ToNat y
    let (ctx', distance, xstep, ystep) :=
      if ctx.planeheight != ctx.cachedheight.getD yNat 0 then
        let distance' := fixedMul ctx.planeheight (ctx.yslope.getD yNat 0)
        let xstep' := fixedMul distance' ctx.basexscale
        let ystep' := fixedMul distance' ctx.baseyscale
        let ctx'' := {
          ctx with
            cachedheight := arrSetI32 ctx.cachedheight yNat ctx.planeheight
            cacheddistance := arrSetI32 ctx.cacheddistance yNat distance'
            cachedxstep := arrSetI32 ctx.cachedxstep yNat xstep'
            cachedystep := arrSetI32 ctx.cachedystep yNat ystep'
        }
        (ctx'', distance', xstep', ystep')
      else
        (ctx, ctx.cacheddistance.getD yNat 0, ctx.cachedxstep.getD yNat 0, ctx.cachedystep.getD yNat 0)
    let length := fixedMul distance (ctx'.distscale.getD (i32ToNat x1) 0)
    let ang := (viewangle + xtoviewangle.getD (i32ToNat x1) 0) >>> ANGLETOFINESHIFT.toUInt32
    let angleIdx := ang.toNat
    let xfrac := viewx + fixedMul (finecosine.getD angleIdx 0) length
    let yfrac := -viewy - fixedMul (finesine.getD angleIdx 0) length
    let colormapLevel := planeColormapLevel ctx' distance
    let spanParams : SpanDrawParams := {
      x1 := x1
      x2 := x2
      y := y
      xfrac := xfrac
      yfrac := yfrac
      xstep := xstep
      ystep := ystep
      source := source
      colormapLevel := colormapLevel
      viewwindowx := ctx.viewwindowx
      viewwindowy := ctx.viewwindowy
    }
    let fb' ← drawSpan data fb spanParams
    pure (ctx', fb')

/--
`R_MakeSpans` — emit completed spans and update `spanstart` (`r_plane.c`).
-/
def makeSpans (data : RenderData) (ctx : PlaneDrawCtx) (fb : Framebuffer)
    (viewx viewy : Int32) (viewangle : UInt32) (xtoviewangle : Array UInt32)
    (source : ByteArray) (x t1 b1 t2 b2 : Int32) : Except String (PlaneDrawCtx × Framebuffer) := do
  let mut ctx := ctx
  let mut fb := fb
  let mut t1 := t1
  let mut b1 := b1
  let mut t2 := t2
  let mut b2 := b2
  while t1 < t2 && t1 <= b1 do
    let spanX1 := ctx.spanstart.getD (i32ToNat t1) 0
    let (ctx', fb') ← mapPlane data ctx fb viewx viewy viewangle xtoviewangle spanX1 (x - 1) t1 source
    ctx := ctx'
    fb := fb'
    t1 := t1 + 1
  while b1 > b2 && b1 >= t1 do
    let spanX1 := ctx.spanstart.getD (i32ToNat b1) 0
    let (ctx', fb') ← mapPlane data ctx fb viewx viewy viewangle xtoviewangle spanX1 (x - 1) b1 source
    ctx := ctx'
    fb := fb'
    b1 := b1 - 1
  while t2 < t1 && t2 <= b2 do
    ctx := { ctx with spanstart := arrSetI32 ctx.spanstart (i32ToNat t2) x }
    t2 := t2 + 1
  while b2 > b1 && b2 >= t2 do
    ctx := { ctx with spanstart := arrSetI32 ctx.spanstart (i32ToNat b2) x }
    b2 := b2 - 1
  pure (ctx, fb)

/--
C `visplane_t` pad addressing (`r_defs.h`): `top[-1]` / `top[SCREENWIDTH]` are
written `0xff` in `R_DrawPlanes`; `bottom[-1]` / `bottom[SCREENWIDTH]` stay BSS 0.
In-range columns stay the 320-wide arrays; do not `arrSetU8` via `i32ToNat(-1)`.
-/
private def visplanePadByte (arr : Array UInt8) (col : Int32) (pad : UInt8) : Int32 :=
  if col < 0 || col >= Int32.ofNat screenWidth then
    Int32.ofNat pad.toNat
  else
    Int32.ofNat (arr.getD (i32ToNat col) pad).toNat

private def visplaneTopByte (arr : Array UInt8) (col : Int32) : Int32 :=
  visplanePadByte arr col visplaneSentinel

private def visplaneBottomByte (arr : Array UInt8) (col : Int32) : Int32 :=
  visplanePadByte arr col 0

private def visplaneByte (arr : Array UInt8) (col : Int32) : Int32 :=
  visplaneTopByte arr col

private def planeLightIndex (lightlevel extralight : Int32) : Nat :=
  let shifted := (lightlevel.toUInt32 >>> lightSegShift.toUInt32).toInt32
  let raw := shifted + extralight
  let light :=
    if raw < 0 then 0 else i32ToNat raw
  if light >= lightLevels then lightLevels - 1 else light

/--
Test-facing visplane column sweep (`R_DrawPlanes` inner `R_MakeSpans` loop).
-/
def drawVisPlaneMarks (data : RenderData) (ctx : PlaneDrawCtx) (fb : Framebuffer)
    (viewx viewy viewz : Int32) (viewangle : UInt32) (xtoviewangle : Array UInt32)
    (pl : VisPlane) (source : ByteArray) (extralight : Int32 := 0)
    : Except String (PlaneDrawCtx × Framebuffer) := do
  let planeheight' := cabs (pl.height - viewz)
  let lightIdx := planeLightIndex pl.lightlevel extralight
  let planezlight' := zlight.getD lightIdx #[]
  let mut ctx := { ctx with planeheight := planeheight', planezlight := planezlight' }
  let mut fb := fb
  let mut x := pl.minx
  while x <= pl.maxx + 1 do
    let t1 := visplaneTopByte pl.top (x - 1)
    let b1 := visplaneBottomByte pl.bottom (x - 1)
    let t2 := visplaneTopByte pl.top x
    let b2 := visplaneBottomByte pl.bottom x
    let (ctx', fb') ← makeSpans data ctx fb viewx viewy viewangle xtoviewangle source x t1 b1 t2 b2
    ctx := ctx'
    fb := fb'
    x := x + 1
  pure (ctx, fb)

/--
`R_DrawPlanes` — raster accumulated visplanes (`r_plane.c`). Sky flats paint
wall-texture columns (`ANGLETOSKYSHIFT`); regular flats load
`firstFlat + flattranslation[picnum]` (empty table is identity). Default
`extralight` is 0.
-/
def drawPlanes (data : RenderData) (wad : WadDirectory) (fb : Framebuffer)
    (planes : VisPlaneState) (view : RenderViewCtx) (viewz : Int32)
    (extralight : Int32 := 0) (vs : ViewSize := {})
    (flattranslation : Array Int32 := #[]) : Except String Framebuffer := do
  let ctx0 := initPlaneDrawCtx
  let ctxSlope := initPlaneSlopeTables ctx0 vs.viewwidth vs.viewheight view.mapping.xtoviewangle
    vs.detailshift
  let ctxSlope := { ctxSlope with
    viewwindowx := vs.viewwindowx, viewwindowy := vs.viewwindowy, centery := vs.centery }
  let mut ctx := clearPlaneDraw ctxSlope view.viewangle vs.centerxfrac
  let mut fb := fb
  let mut tt := data.textureTables
  let mut i : Nat := 0
  while i < planes.lastvisplane do
    match visPlaneAt planes i with
    | none => pure ()
    | some pl =>
      if pl.minx > pl.maxx then
        pure ()
      else if pl.picnum == tt.skyFlatNum then do
        let mut x := pl.minx
        while x <= pl.maxx do
          let yl := visplaneByte pl.top x
          let yh := visplaneByte pl.bottom x
          if yl <= yh then do
            let angle :=
              (view.viewangle + view.mapping.xtoviewangle.getD (i32ToNat x) 0)
                >>> angletoskyShift.toUInt32
            let (tt', source) ← getColumn tt wad tt.skytexture angle.toNat
            let colParams : ColumnDrawParams := {
              x := x
              yl := yl
              yh := yh
              iscale := FRACUNIT
              texturemid := 100 * FRACUNIT
              source := source
              colormapLevel := 0
              centery := vs.centery
              viewwindowx := vs.viewwindowx
              viewwindowy := vs.viewwindowy
            }
            fb ← drawColumn data fb colParams
            tt := tt'
          else
            pure ()
          x := x + 1
      else do
        let translated :=
          match flattranslation[pl.picnum]? with
          | some t => i32ToNat t
          | none => pl.picnum
        let source ← lumpData wad (tt.firstFlat + translated)
        let (ctx', fb') ←
          drawVisPlaneMarks data ctx fb view.viewx view.viewy viewz view.viewangle
            view.mapping.xtoviewangle pl source extralight
        ctx := ctx'
        fb := fb'
    i := i + 1
  pure fb

end Doom.Render.Plane
