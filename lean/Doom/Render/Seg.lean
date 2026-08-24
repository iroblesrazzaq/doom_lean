import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Level
import Doom.Playsim.Tables
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Draw
import Doom.Render.Gfx.Texture
import Doom.Render.Plane
import Doom.Render.Tables
import Doom.Render.Types
import Doom.Render.Util
import Doom.Render.View
import Doom.Wad

/-!
# Doom.Render.Seg

Wall columns (`r_segs.c` `R_RenderSegLoop`, midtexture and two-sided tiers).
-/

namespace Doom.Render.Seg

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Level
open Doom.Playsim.Tables
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Draw
open Doom.Render.Gfx.Texture
open Doom.Render.Plane
open Doom.Render.Tables
open Doom.Render.Types
open Doom.Render.Util
open Doom.Render.View
open Doom.Wad

def defaultViewheight : Int32 := 168
def defaultCentery : Int32 := 84
def defaultProjection : Int32 := 160 * FRACUNIT

structure StoreWallInput where
  viewx : Int32
  viewy : Int32
  viewz : Int32
  viewangle : UInt32
  viewwidth : Int32 := 320
  viewheight : Int32 := defaultViewheight
  centery : Int32 := defaultCentery
  projection : Int32 := defaultProjection
  detailshift : Nat := detailShift
  viewwindowx : Int32 := 0
  viewwindowy : Int32 := 0
  extralight : Int32 := 0
  fixedcolormap : Bool := false
  rwAngle1 : UInt32
  segAngle : UInt32
  segOffset : Int32
  v1x : Int32
  v1y : Int32
  v2x : Int32
  v2y : Int32
  linedefFlags : Int32
  textureoffset : Int32
  rowoffset : Int32
  midtexture : Nat
  midtextureHeight : Nat
  ceilingheight : Int32
  floorheight : Int32
  ceilingpic : ByteArray
  floorpic : ByteArray := ByteArray.empty
  lightlevel : Int32
  backFloorheight : Int32 := 0
  backCeilingheight : Int32 := 0
  backFloorpic : ByteArray := ByteArray.empty
  backCeilingpic : ByteArray := ByteArray.empty
  backLightlevel : Int32 := 0
  toptexture : Nat := 0
  bottomtexture : Nat := 0
  toptextureHeight : Nat := 0
  bottomtextureHeight : Nat := 0
  start : Int32
  stop : Int32
  xtoviewangle : Array UInt32
  drawSegIdx : Nat := 0
  ceilingclip : Array Int32
  floorclip : Array Int32
  visplanes : VisPlaneState := initVisPlaneState
  floorplane : Option Nat := none
  ceilingplane : Option Nat := none

structure StoreWallResult where
  drawSegIdx : Nat
  rwScale : Int32
  rwScaleStep : Int32
  rwDistance : Int32
  rwOffset : Int32
  rwMidTextureMid : Int32
  rwCenterAngle : UInt32
  walllights : Array Nat
  topfrac : Int32
  topstep : Int32
  bottomfrac : Int32
  bottomstep : Int32
  markceiling : Bool
  markfloor : Bool
  midtexture : Nat
  silhouette : Nat := 0
  bsilheight : Int32 := 0
  tsilheight : Int32 := 0
  toptexture : Nat := 0
  bottomtexture : Nat := 0
  maskedtexture : Bool := false
  maskedtexturecol : Array Int32 := #[]
  rwTopTextureMid : Int32 := 0
  rwBottomTextureMid : Int32 := 0
  worldtop : Int32 := 0
  worldhigh : Int32 := 0
  worldbottom : Int32 := 0
  worldlow : Int32 := 0
  pixhigh : Int32 := 0
  pixlow : Int32 := 0
  pixhighstep : Int32 := 0
  pixlowstep : Int32 := 0
  ceilingclip : Array Int32
  floorclip : Array Int32
  visplanes : VisPlaneState := initVisPlaneState
  floorplane : Option Nat := none
  ceilingplane : Option Nat := none
  /-- C `ds->sprtopclip`; `none` is NULL. Indexed at screen x. -/
  sprtopclip : Option (Array Int32) := none
  /-- C `ds->sprbottomclip`; `none` is NULL. Indexed at screen x. -/
  sprbottomclip : Option (Array Int32) := none
  deriving Repr

structure SegLoopState where
  rwX : Int32
  rwStopX : Int32
  rwScale : Int32
  rwScaleStep : Int32
  rwMidTextureMid : Int32
  rwCenterAngle : UInt32
  rwDistance : Int32
  rwOffset : Int32
  midtexture : Nat
  walllights : Array Nat
  ceilingclip : Array Int32
  floorclip : Array Int32
  markceiling : Bool
  markfloor : Bool
  segtextured : Bool
  topfrac : Int32
  topstep : Int32
  bottomfrac : Int32
  bottomstep : Int32
  xtoviewangle : Array UInt32
  viewheight : Int32 := defaultViewheight
  centery : Int32 := defaultCentery
  viewwindowx : Int32 := 0
  viewwindowy : Int32 := 0
  textureTables : TextureTables
  toptexture : Nat := 0
  bottomtexture : Nat := 0
  rwTopTextureMid : Int32 := 0
  rwBottomTextureMid : Int32 := 0
  pixhigh : Int32 := 0
  pixlow : Int32 := 0
  pixhighstep : Int32 := 0
  pixlowstep : Int32 := 0
  maskedtexture : Bool := false
  maskedtexturecol : Array Int32 := #[]
  maskedColBase : Int32 := 0
  visplanes : VisPlaneState := initVisPlaneState
  floorplane : Option Nat := none
  ceilingplane : Option Nat := none

private def clipAt (arr : Array Int32) (x : Int32) : Int32 :=
  arr.getD (i32ToNat x) 0

private def setClipAt (arr : Array Int32) (x : Int32) (v : Int32) : Array Int32 :=
  arrSetI32 arr (i32ToNat x) v

private def lightIndex (scale : Int32) : Nat :=
  let raw := i32ToNat (scale >>> 12)
  if raw >= Constants.maxLightScale then Constants.maxLightScale - 1 else raw

private def iscaleFor (scale : Int32) : Int32 :=
  (0xffffffff / scale.toUInt32).toInt32

private def textureColumnAt (centerAngle xto : UInt32) (offset distance : Int32) : Int32 :=
  let angleIdx :=
    (((centerAngle + xto) >>> ANGLETOFINESHIFT.toUInt32) &&& FINEMASK).toNat
  let ft := finetangent.getD angleIdx 0
  (offset - fixedMul ft distance) >>> 16

private def rwOffsetAngle (rwNormalAngle rwAngle1 : UInt32) : UInt32 :=
  let raw := rwNormalAngle - rwAngle1
  let folded := if raw > ANG180 then (-raw.toInt32).toUInt32 else raw
  if folded > ANG90 then ANG90 else folded

/-- C `r_segs.c`: `if (rw_normalangle-rw_angle1 < ANG180) rw_offset = -rw_offset`. -/
private def negRwOffset (rwNormalAngle rwAngle1 : UInt32) : Bool :=
  rwNormalAngle - rwAngle1 < ANG180

private def wallLightTable (inp : StoreWallInput) : Array Nat :=
  if inp.fixedcolormap then
    #[]
  else
    let lightnum0 := (inp.lightlevel >>> 4) + inp.extralight
    let lightnum1 := if inp.v1y == inp.v2y then lightnum0 - 1 else lightnum0
    let lightnum := if inp.v1x == inp.v2x then lightnum1 + 1 else lightnum1
    if lightnum < 0 then
      scalelight.getD 0 #[]
    else if lightnum >= Int32.ofNat Tables.lightLevels then
      scalelight.getD (Tables.lightLevels - 1) #[]
    else
      scalelight.getD (i32ToNat lightnum) #[]

private def segTextureParams (hyp : Int32) (rwNormalAngle : UInt32) (inp : StoreWallInput) :
    Int32 × UInt32 × Array Nat :=
  let offAng := rwOffsetAngle rwNormalAngle inp.rwAngle1
  let sineOff := finesine.getD ((offAng >>> ANGLETOFINESHIFT.toUInt32).toNat) 0
  let rwOffset0 := fixedMul hyp sineOff
  let rwOffset1 := if negRwOffset rwNormalAngle inp.rwAngle1 then -rwOffset0 else rwOffset0
  let rwOffset := rwOffset1 + inp.textureoffset + inp.segOffset
  let rwCenterAngle := ANG90 + inp.viewangle - rwNormalAngle
  (rwOffset, rwCenterAngle, wallLightTable inp)

private def rangecheckError (start stop _viewwidth : Int32) : String :=
  s!"Bad R_RenderWallRange: {start} to {stop}"

private def silHeightMax : Int32 := 2147483647
private def silHeightMin : Int32 := -2147483648

/-- Copy `src[start:stopx)` into a screen-x-indexed array (`lastopening - start`). -/
private def snapshotClipAtScreenX (src : Array Int32) (start stopx : Int32) : Array Int32 :=
  Id.run do
    let mut out := Array.replicate Types.screenWidth (0 : Int32)
    let mut x := start
    while x < stopx do
      let i := i32ToNat x
      out := arrSetI32 out i (src.getD i 0)
      x := x + 1
    pure out

/-- Post-`R_RenderSegLoop` sprite clip pointers and masked SIL bump (`r_segs.c` L715–740). -/
private def fillSpriteClips (silhouette : Nat) (bsilheight tsilheight : Int32)
    (maskedtexture : Bool) (preTop preBot : Option (Array Int32))
    (ceilingclip floorclip : Array Int32) (start stopx : Int32) :
    Nat × Int32 × Int32 × Option (Array Int32) × Option (Array Int32) :=
  Id.run do
    let mut sprtop := preTop
    let mut sprbot := preBot
    let mut sil := silhouette
    let mut ts := tsilheight
    let mut bs := bsilheight
    if ((sil &&& silTop) != 0 || maskedtexture) && sprtop.isNone then
      sprtop := some (snapshotClipAtScreenX ceilingclip start stopx)
    if ((sil &&& silBottom) != 0 || maskedtexture) && sprbot.isNone then
      sprbot := some (snapshotClipAtScreenX floorclip start stopx)
    if maskedtexture && (sil &&& silTop) == 0 then
      sil := sil ||| silTop
      ts := silHeightMin
    if maskedtexture && (sil &&& silBottom) == 0 then
      sil := sil ||| silBottom
      bs := silHeightMax
    pure (sil, bs, ts, sprtop, sprbot)

private def storeWallDistanceScale (inp : StoreWallInput) :
    Int32 × Int32 × Int32 × Int32 × UInt32 :=
  let rwNormalAngle := inp.segAngle + ANG90
  let offsetangle0 := cabs (rwNormalAngle.toInt32 - inp.rwAngle1.toInt32)
  let offsetangle := if offsetangle0 > ANG90.toInt32 then ANG90.toInt32 else offsetangle0
  let distangle := ANG90 - offsetangle.toUInt32
  let hyp := pointToDist inp.viewx inp.viewy inp.v1x inp.v1y
  let sineval := finesine.getD ((distangle >>> ANGLETOFINESHIFT.toUInt32).toNat) 0
  let rwDistance := fixedMul hyp sineval
  let visStart := inp.viewangle + inp.xtoviewangle.getD (i32ToNat inp.start) 0
  let rwScale :=
    scaleFromGlobalAngle visStart inp.viewangle rwNormalAngle rwDistance inp.projection
      inp.detailshift
  let rwScaleStep :=
    if inp.stop > inp.start then
      let visStop := inp.viewangle + inp.xtoviewangle.getD (i32ToNat inp.stop) 0
      let scale2 :=
        scaleFromGlobalAngle visStop inp.viewangle rwNormalAngle rwDistance inp.projection
          inp.detailshift
      (scale2 - rwScale) / (inp.stop - inp.start)
    else
      (0 : Int32)
  (rwDistance, rwScale, rwScaleStep, hyp, rwNormalAngle)

private def emptyStoreWallResult (inp : StoreWallInput) : StoreWallResult :=
  {
    drawSegIdx := inp.drawSegIdx
    rwScale := 0
    rwScaleStep := 0
    rwDistance := 0
    rwOffset := 0
    rwMidTextureMid := 0
    rwCenterAngle := 0
    walllights := #[]
    topfrac := 0
    topstep := 0
    bottomfrac := 0
    bottomstep := 0
    markceiling := false
    markfloor := false
    midtexture := 0
    ceilingclip := inp.ceilingclip
    floorclip := inp.floorclip
    visplanes := inp.visplanes
    floorplane := inp.floorplane
    ceilingplane := inp.ceilingplane
  }

private def computeTwoSidedSilhouette (inp : StoreWallInput) : Nat × Int32 × Int32 :=
  let (sil, bs, ts) := (0, (0 : Int32), (0 : Int32))
  let (sil, bs) :=
    if inp.floorheight > inp.backFloorheight then (silBottom, inp.floorheight)
    else if inp.backFloorheight > inp.viewz then (silBottom, silHeightMax)
    else (sil, bs)
  let (sil, ts) :=
    if inp.ceilingheight < inp.backCeilingheight then (sil ||| silTop, inp.ceilingheight)
    else if inp.backCeilingheight < inp.viewz then (sil ||| silTop, silHeightMin)
    else (sil, ts)
  let (sil, bs) :=
    if inp.backCeilingheight <= inp.floorheight then (sil ||| silBottom, silHeightMax)
    else (sil, bs)
  let (sil, ts) :=
    if inp.backFloorheight >= inp.ceilingheight then (sil ||| silTop, silHeightMin)
    else (sil, ts)
  (sil, bs, ts)

private def byteAt (pic : ByteArray) (i : Nat) : UInt8 :=
  match pic[i]? with | some b => b | none => 0

private def sameFlatPic (a b : ByteArray) : Bool :=
  a.size == b.size &&
    (List.range (min a.size b.size)).all fun i => byteAt a i == byteAt b i

/-- `R_RenderSegLoop` — midtexture (single-sided) and top/bottom (two-sided) tiers. -/
def renderSegLoop (data : RenderData) (wad : WadDirectory) (st : SegLoopState) (fb : Framebuffer) :
    Except String (SegLoopState × Framebuffer) := do
  let mut state := st
  let mut out := fb
  while state.rwX < state.rwStopX do
    let x := state.rwX
    let xNat := i32ToNat x
    let ceilClip := clipAt state.ceilingclip x
    let floorClip := clipAt state.floorclip x

    let mut yl : Int32 := (state.topfrac + Int32.ofNat (heightUnit - 1)) >>> 12
    if yl < ceilClip + 1 then
      yl := ceilClip + 1

    if state.markceiling then
      let top := ceilClip + 1
      let mut bottom : Int32 := yl - 1
      if bottom >= floorClip then
        bottom := floorClip - 1
      if top <= bottom then
        match state.ceilingplane with
        | none => pure ()
        | some plIdx =>
          state := { state with
            visplanes := markCeilingColumn state.visplanes plIdx x top bottom
          }

    let mut yh : Int32 := state.bottomfrac >>> 12
    if yh >= floorClip then
      yh := floorClip - 1

    if state.markfloor then
      let top := yh + 1
      let bottom := floorClip - 1
      let mut adjTop := top
      if adjTop <= ceilClip then
        adjTop := ceilClip + 1
      if adjTop <= bottom then
        match state.floorplane with
        | none => pure ()
        | some plIdx =>
          state := { state with
            visplanes := markFloorColumn state.visplanes plIdx x adjTop bottom
          }

    let mut texturecolumn : Int32 := 0
    let mut colormapLevel : Nat := 0
    if state.segtextured then
      let xto := state.xtoviewangle.getD xNat 0
      texturecolumn := textureColumnAt state.rwCenterAngle xto state.rwOffset state.rwDistance
      let idx := lightIndex state.rwScale
      colormapLevel := state.walllights.getD idx 0

    if state.midtexture != 0 then
      let colIdx := (i32AsU32 texturecolumn).toNat
      let (tt, source) ← getColumn state.textureTables wad state.midtexture colIdx
      let colParams : ColumnDrawParams := {
        x := x
        yl := yl
        yh := yh
        iscale := iscaleFor state.rwScale
        texturemid := state.rwMidTextureMid
        source := source
        colormapLevel := colormapLevel
        centery := state.centery
        viewwindowx := state.viewwindowx
        viewwindowy := state.viewwindowy
      }
      out ← drawColumn data out colParams
      state := { state with
        textureTables := tt
        ceilingclip := setClipAt state.ceilingclip x state.viewheight
        floorclip := setClipAt state.floorclip x (-1 : Int32)
      }
    else
      if state.toptexture != 0 then
        let mut mid := state.pixhigh >>> 12
        let pixhighNext := state.pixhigh + state.pixhighstep
        if mid >= floorClip then
          mid := floorClip - 1
        if mid >= yl then
          let colIdx := (i32AsU32 texturecolumn).toNat
          let (tt, source) ← getColumn state.textureTables wad state.toptexture colIdx
          let colParams : ColumnDrawParams := {
            x := x
            yl := yl
            yh := mid
            iscale := iscaleFor state.rwScale
            texturemid := state.rwTopTextureMid
            source := source
            colormapLevel := colormapLevel
            centery := state.centery
            viewwindowx := state.viewwindowx
            viewwindowy := state.viewwindowy
          }
          out ← drawColumn data out colParams
          state := { state with
            textureTables := tt
            ceilingclip := setClipAt state.ceilingclip x mid
          }
        else
          state := { state with ceilingclip := setClipAt state.ceilingclip x (yl - 1) }
        state := { state with pixhigh := pixhighNext }
      else if state.markceiling then
        state := { state with ceilingclip := setClipAt state.ceilingclip x (yl - 1) }

      let ceilClipAfterTop := clipAt state.ceilingclip x

      if state.bottomtexture != 0 then
        let mut mid := (state.pixlow + Int32.ofNat (heightUnit - 1)) >>> 12
        let pixlowNext := state.pixlow + state.pixlowstep
        if mid <= ceilClipAfterTop then
          mid := ceilClipAfterTop + 1
        if mid <= yh then
          let colIdx := (i32AsU32 texturecolumn).toNat
          let (tt, source) ← getColumn state.textureTables wad state.bottomtexture colIdx
          let colParams : ColumnDrawParams := {
            x := x
            yl := mid
            yh := yh
            iscale := iscaleFor state.rwScale
            texturemid := state.rwBottomTextureMid
            source := source
            colormapLevel := colormapLevel
            centery := state.centery
            viewwindowx := state.viewwindowx
            viewwindowy := state.viewwindowy
          }
          out ← drawColumn data out colParams
          state := { state with
            textureTables := tt
            floorclip := setClipAt state.floorclip x mid
          }
        else
          state := { state with floorclip := setClipAt state.floorclip x (yh + 1) }
        state := { state with pixlow := pixlowNext }
      else if state.markfloor then
        state := { state with floorclip := setClipAt state.floorclip x (yh + 1) }

      if state.maskedtexture then
        let idx := i32ToNat (x - state.maskedColBase)
        if idx < state.maskedtexturecol.size then
          state := { state with
            maskedtexturecol := arrSetI32 state.maskedtexturecol idx texturecolumn
          }

    state := { state with
      rwX := state.rwX + 1
      rwScale := state.rwScale + state.rwScaleStep
      topfrac := state.topfrac + state.topstep
      bottomfrac := state.bottomfrac + state.bottomstep
    }
  pure (state, out)

/--
`R_StoreWallRange` single-sided path (`r_segs.c`): distance/scale/lighting,
midtexture peg, top/bottom frac, then `renderSegLoop`. Silent no-op at
`maxDrawSegs`.
-/
def storeWallSolid (data : RenderData) (wad : WadDirectory) (inp : StoreWallInput)
    (fb : Framebuffer) : Except String (StoreWallResult × Framebuffer) := do
  if inp.start >= inp.viewwidth || inp.start > inp.stop then
    throw (rangecheckError inp.start inp.stop inp.viewwidth)
  if inp.drawSegIdx >= maxDrawSegs then
    return (emptyStoreWallResult inp, fb)
  let (rwDistance, rwScale, rwScaleStep, hyp, rwNormalAngle) := storeWallDistanceScale inp
  let worldtop := inp.ceilingheight - inp.viewz
  let worldbottom := inp.floorheight - inp.viewz
  let midtexture := inp.midtexture
  let rwMidTextureMid :=
    if (inp.linedefFlags &&& mlDontpegBottom) != 0 then
      let vtop := inp.floorheight + Int32.ofNat inp.midtextureHeight * FRACUNIT
      vtop - inp.viewz
    else
      worldtop
  let rwMidTextureMid' := rwMidTextureMid + inp.rowoffset
  let segtextured := midtexture != 0
  let (rwOffset, rwCenterAngle, walllights) :=
    if segtextured then
      segTextureParams hyp rwNormalAngle inp
    else
      (0, 0, #[])
  let markfloor := inp.floorheight < inp.viewz
  let markceiling := inp.ceilingheight > inp.viewz || isSkyPic inp.ceilingpic
  let centeryfrac := inp.centery <<< 16
  let worldtop4 := worldtop >>> 4
  let worldbottom4 := worldbottom >>> 4
  let topstep := -fixedMul rwScaleStep worldtop4
  let topfrac := (centeryfrac >>> 4) - fixedMul worldtop4 rwScale
  let bottomstep := -fixedMul rwScaleStep worldbottom4
  let bottomfrac := (centeryfrac >>> 4) - fixedMul worldbottom4 rwScale
  let mut vpSt := inp.visplanes
  let mut ceilPl := inp.ceilingplane
  let mut floorPl := inp.floorplane
  if markceiling then
    match ceilPl with
    | none => pure ()
    | some plIdx =>
      let (st', idx') ← checkPlane vpSt plIdx inp.start inp.stop
      vpSt := st'
      ceilPl := some idx'
  if markfloor then
    match floorPl with
    | none => pure ()
    | some plIdx =>
      let (st', idx') ← checkPlane vpSt plIdx inp.start inp.stop
      vpSt := st'
      floorPl := some idx'
  let loopSt : SegLoopState := {
    rwX := inp.start
    rwStopX := inp.stop + 1
    rwScale := rwScale
    rwScaleStep := rwScaleStep
    rwMidTextureMid := rwMidTextureMid'
    rwCenterAngle := rwCenterAngle
    rwDistance := rwDistance
    rwOffset := rwOffset
    midtexture := midtexture
    walllights := walllights
    ceilingclip := inp.ceilingclip
    floorclip := inp.floorclip
    markceiling := markceiling
    markfloor := markfloor
    segtextured := segtextured
    topfrac := topfrac
    topstep := topstep
    bottomfrac := bottomfrac
    bottomstep := bottomstep
    xtoviewangle := inp.xtoviewangle
    viewheight := inp.viewheight
    centery := inp.centery
    viewwindowx := inp.viewwindowx
    viewwindowy := inp.viewwindowy
    textureTables := data.textureTables
    visplanes := vpSt
    floorplane := floorPl
    ceilingplane := ceilPl
  }
  let (loopOut, fbOut) ← renderSegLoop data wad loopSt fb
  let (sil, bs, ts, sprtop, sprbot) :=
    fillSpriteClips (silBottom + silTop) silHeightMax silHeightMin false
      (some data.screenheightarray) (some data.negonearray)
      loopOut.ceilingclip loopOut.floorclip inp.start (inp.stop + 1)
  pure ({
    drawSegIdx := inp.drawSegIdx + 1
    rwScale := rwScale
    rwScaleStep := rwScaleStep
    rwDistance := rwDistance
    rwOffset := rwOffset
    rwMidTextureMid := rwMidTextureMid'
    rwCenterAngle := rwCenterAngle
    walllights := walllights
    topfrac := topfrac
    topstep := topstep
    bottomfrac := bottomfrac
    bottomstep := bottomstep
    markceiling := markceiling
    markfloor := markfloor
    midtexture := midtexture
    silhouette := sil
    bsilheight := bs
    tsilheight := ts
    worldtop := worldtop
    worldhigh := worldtop
    worldbottom := worldbottom
    worldlow := worldbottom
    ceilingclip := loopOut.ceilingclip
    floorclip := loopOut.floorclip
    visplanes := loopOut.visplanes
    floorplane := floorPl
    ceilingplane := ceilPl
    sprtopclip := sprtop
    sprbottomclip := sprbot
  }, fbOut)

/--
`R_StoreWallRange` two-sided path (`r_segs.c` L479–702): silhouette, plane marks,
sky hack, top/bottom texture selection/pegging, masked flag, pix steps; then
`renderSegLoop` with `midtexture := 0`. Masked midtexture deferred.
-/
def storeWallTwoSided (data : RenderData) (wad : WadDirectory) (inp : StoreWallInput)
    (fb : Framebuffer) : Except String (StoreWallResult × Framebuffer) := do
  if inp.start >= inp.viewwidth || inp.start > inp.stop then
    throw (rangecheckError inp.start inp.stop inp.viewwidth)
  if inp.drawSegIdx >= maxDrawSegs then
    return (emptyStoreWallResult inp, fb)
  let (rwDistance, rwScale, rwScaleStep, hyp, rwNormalAngle) := storeWallDistanceScale inp
  let (silhouette, bsilheight, tsilheight) := computeTwoSidedSilhouette inp
  let preBot : Option (Array Int32) :=
    if inp.backCeilingheight <= inp.floorheight then some data.negonearray else none
  let preTop : Option (Array Int32) :=
    if inp.backFloorheight >= inp.ceilingheight then some data.screenheightarray else none
  let mut worldtop := inp.ceilingheight - inp.viewz
  let worldbottom := inp.floorheight - inp.viewz
  let worldhigh := inp.backCeilingheight - inp.viewz
  let worldlow := inp.backFloorheight - inp.viewz
  if isSkyPic inp.ceilingpic && isSkyPic inp.backCeilingpic then
    worldtop := worldhigh
  let mut markfloor :=
    worldlow != worldbottom ||
      !sameFlatPic inp.backFloorpic inp.floorpic ||
      inp.backLightlevel != inp.lightlevel
  let mut markceiling :=
    worldhigh != worldtop ||
      !sameFlatPic inp.backCeilingpic inp.ceilingpic ||
      inp.backLightlevel != inp.lightlevel
  if inp.backCeilingheight <= inp.floorheight ||
      inp.backFloorheight >= inp.ceilingheight then
    markceiling := true
    markfloor := true
  let mut toptextureSel : Nat := 0
  let mut bottomtextureSel : Nat := 0
  let mut rwTopTextureMid : Int32 := 0
  let mut rwBottomTextureMid : Int32 := 0
  if worldhigh < worldtop then
    toptextureSel := inp.toptexture
    rwTopTextureMid :=
      if (inp.linedefFlags &&& mlDontpegTop) != 0 then
        worldtop
      else
        let vtop := inp.backCeilingheight + Int32.ofNat inp.toptextureHeight * FRACUNIT
        vtop - inp.viewz
  if worldlow > worldbottom then
    bottomtextureSel := inp.bottomtexture
    rwBottomTextureMid :=
      if (inp.linedefFlags &&& mlDontpegBottom) != 0 then
        worldtop
      else
        worldlow
  rwTopTextureMid := rwTopTextureMid + inp.rowoffset
  rwBottomTextureMid := rwBottomTextureMid + inp.rowoffset
  let maskedtexture := inp.midtexture != 0
  let segtextured :=
    toptextureSel != 0 || bottomtextureSel != 0 || maskedtexture
  let (rwOffset, rwCenterAngle, walllights) :=
    if segtextured then
      segTextureParams hyp rwNormalAngle inp
    else
      (0, 0, #[])
  if inp.floorheight >= inp.viewz then
    markfloor := false
  if inp.ceilingheight <= inp.viewz && !isSkyPic inp.ceilingpic then
    markceiling := false
  let centeryfrac := inp.centery <<< 16
  let worldtop4 := worldtop >>> 4
  let worldbottom4 := worldbottom >>> 4
  let topstep := -fixedMul rwScaleStep worldtop4
  let topfrac := (centeryfrac >>> 4) - fixedMul worldtop4 rwScale
  let bottomstep := -fixedMul rwScaleStep worldbottom4
  let bottomfrac := (centeryfrac >>> 4) - fixedMul worldbottom4 rwScale
  let worldhigh4 := worldhigh >>> 4
  let worldlow4 := worldlow >>> 4
  let mut pixhigh : Int32 := 0
  let mut pixhighstep : Int32 := 0
  let mut pixlow : Int32 := 0
  let mut pixlowstep : Int32 := 0
  if worldhigh4 < worldtop4 then
    pixhigh := (centeryfrac >>> 4) - fixedMul worldhigh4 rwScale
    pixhighstep := -fixedMul rwScaleStep worldhigh4
  if worldlow4 > worldbottom4 then
    pixlow := (centeryfrac >>> 4) - fixedMul worldlow4 rwScale
    pixlowstep := -fixedMul rwScaleStep worldlow4
  let maskedColCount :=
    if maskedtexture then i32ToNat (inp.stop - inp.start + 1) else 0
  let maskedtexturecol :=
    Array.replicate maskedColCount (Int32.ofNat Constants.shrtMax)
  let mut vpSt := inp.visplanes
  let mut ceilPl := inp.ceilingplane
  let mut floorPl := inp.floorplane
  if markceiling then
    match ceilPl with
    | none => pure ()
    | some plIdx =>
      let (st', idx') ← checkPlane vpSt plIdx inp.start inp.stop
      vpSt := st'
      ceilPl := some idx'
  if markfloor then
    match floorPl with
    | none => pure ()
    | some plIdx =>
      let (st', idx') ← checkPlane vpSt plIdx inp.start inp.stop
      vpSt := st'
      floorPl := some idx'
  let loopSt : SegLoopState := {
    rwX := inp.start
    rwStopX := inp.stop + 1
    rwScale := rwScale
    rwScaleStep := rwScaleStep
    rwMidTextureMid := 0
    rwCenterAngle := rwCenterAngle
    rwDistance := rwDistance
    rwOffset := rwOffset
    midtexture := 0
    walllights := walllights
    ceilingclip := inp.ceilingclip
    floorclip := inp.floorclip
    markceiling := markceiling
    markfloor := markfloor
    segtextured := segtextured
    topfrac := topfrac
    topstep := topstep
    bottomfrac := bottomfrac
    bottomstep := bottomstep
    xtoviewangle := inp.xtoviewangle
    viewheight := inp.viewheight
    centery := inp.centery
    viewwindowx := inp.viewwindowx
    viewwindowy := inp.viewwindowy
    textureTables := data.textureTables
    toptexture := toptextureSel
    bottomtexture := bottomtextureSel
    rwTopTextureMid := rwTopTextureMid
    rwBottomTextureMid := rwBottomTextureMid
    pixhigh := pixhigh
    pixlow := pixlow
    pixhighstep := pixhighstep
    pixlowstep := pixlowstep
    maskedtexture := maskedtexture
    maskedtexturecol := maskedtexturecol
    maskedColBase := inp.start
    visplanes := vpSt
    floorplane := floorPl
    ceilingplane := ceilPl
  }
  let (loopOut, fbOut) ← renderSegLoop data wad loopSt fb
  let (sil, bs, ts, sprtop, sprbot) :=
    fillSpriteClips silhouette bsilheight tsilheight maskedtexture preTop preBot
      loopOut.ceilingclip loopOut.floorclip inp.start (inp.stop + 1)
  pure ({
    drawSegIdx := inp.drawSegIdx + 1
    rwScale := rwScale
    rwScaleStep := rwScaleStep
    rwDistance := rwDistance
    rwOffset := rwOffset
    rwMidTextureMid := 0
    rwCenterAngle := rwCenterAngle
    walllights := walllights
    topfrac := topfrac
    topstep := topstep
    bottomfrac := bottomfrac
    bottomstep := bottomstep
    markceiling := markceiling
    markfloor := markfloor
    midtexture := if maskedtexture then inp.midtexture else 0
    silhouette := sil
    bsilheight := bs
    tsilheight := ts
    toptexture := toptextureSel
    bottomtexture := bottomtextureSel
    maskedtexture := maskedtexture
    maskedtexturecol := loopOut.maskedtexturecol
    rwTopTextureMid := rwTopTextureMid
    rwBottomTextureMid := rwBottomTextureMid
    worldtop := worldtop
    worldhigh := worldhigh
    worldbottom := worldbottom
    worldlow := worldlow
    pixhigh := pixhigh
    pixlow := pixlow
    pixhighstep := pixhighstep
    pixlowstep := pixlowstep
    ceilingclip := loopOut.ceilingclip
    floorclip := loopOut.floorclip
    visplanes := loopOut.visplanes
    floorplane := floorPl
    ceilingplane := ceilPl
    sprtopclip := sprtop
    sprbottomclip := sprbot
  }, fbOut)

structure MaskedSegReplayInput where
  res : StoreWallResult
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
  fixedcolormap : Bool := false
  centery : Int32 := defaultCentery
  viewwindowx : Int32 := 0
  viewwindowy : Int32 := 0
  extralight : Int32 := 0
  lightlevel : Int32 := 128
  v1y : Int32 := 0
  v2y : Int32 := 0
  v1x : Int32 := 0
  v2x : Int32 := 64 * FRACUNIT

private def maskedTextureMid (inp : MaskedSegReplayInput) : Int32 :=
  if (inp.linedefFlags &&& mlDontpegBottom) != 0 then
    let floor :=
      if inp.frontFloor > inp.backFloor then inp.frontFloor else inp.backFloor
    floor + Int32.ofNat inp.midtextureHeight * FRACUNIT - inp.viewz + inp.rowoffset
  else
    let ceil :=
      if inp.frontCeil < inp.backCeil then inp.frontCeil else inp.backCeil
    ceil - inp.viewz + inp.rowoffset

private def replayWallLightTable (inp : MaskedSegReplayInput) : Array Nat :=
  if inp.fixedcolormap then
    #[]
  else
    let lightnum0 := (inp.lightlevel >>> 4) + inp.extralight
    let lightnum1 := if inp.v1y == inp.v2y then lightnum0 - 1 else lightnum0
    let lightnum := if inp.v1x == inp.v2x then lightnum1 + 1 else lightnum1
    if lightnum < 0 then
      scalelight.getD 0 #[]
    else if lightnum >= Int32.ofNat Tables.lightLevels then
      scalelight.getD (Tables.lightLevels - 1) #[]
    else
      scalelight.getD (i32ToNat lightnum) #[]

/-- `R_RenderMaskedSegRange` — replay recorded masked midtexture columns (`r_segs.c`). -/
def renderMaskedSegRange (data : RenderData) (wad : WadDirectory) (inp : MaskedSegReplayInput)
    (fb : Framebuffer) : Except String (StoreWallResult × Framebuffer) := do
  let res := inp.res
  if !res.maskedtexture then
    return (res, fb)
  let walllights := replayWallLightTable inp
  let texturemid := maskedTextureMid inp
  let centeryfrac := inp.centery <<< 16
  let mceilingclip :=
    match res.sprtopclip with
    | some a => a
    | none => res.ceilingclip
  let mfloorclip :=
    match res.sprbottomclip with
    | some a => a
    | none => res.floorclip
  let mut spryscale := res.rwScale + (inp.x1 - inp.segStart) * res.rwScaleStep
  let mut maskedCols := res.maskedtexturecol
  let mut out := fb
  let mut x := inp.x1
  while x <= inp.x2 do
    let arrIdx := i32ToNat (x - inp.segStart)
    let colIdx := maskedCols.getD arrIdx (Int32.ofNat Constants.shrtMax)
    if colIdx != Int32.ofNat Constants.shrtMax then
      let lightIdx :=
        if inp.fixedcolormap then 0
        else
          let raw := i32ToNat (spryscale >>> 12)
          if raw >= Constants.maxLightScale then Constants.maxLightScale - 1 else raw
      let colormapLevel :=
        if inp.fixedcolormap then 0 else walllights.getD lightIdx 0
      let sprtopscreen := centeryfrac - fixedMul texturemid spryscale
      let (tt, posts) ← getMaskColumnPosts data.textureTables wad inp.texnum
        ((i32AsU32 colIdx).toNat)
      let colParams : MaskedColumnDrawParams := {
        x := x
        sprtopscreen := sprtopscreen
        spryscale := spryscale
        texturemid := texturemid
        posts := posts
        mceilingclip := mceilingclip
        mfloorclip := mfloorclip
        colormapLevel := colormapLevel
        centery := inp.centery
        viewwindowx := inp.viewwindowx
        viewwindowy := inp.viewwindowy
      }
      out ← drawMaskedColumn { data with textureTables := tt } out colParams
      maskedCols := arrSetI32 maskedCols arrIdx (Int32.ofNat Constants.shrtMax)
    spryscale := spryscale + res.rwScaleStep
    x := x + 1
  pure ({ res with maskedtexturecol := maskedCols }, out)

end Doom.Render.Seg
