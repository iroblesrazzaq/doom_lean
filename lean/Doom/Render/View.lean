import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Tables
import Doom.Render.Constants
import Doom.Render.Types
import Doom.Render.Util

open Doom.Playsim.Tables

/-!
# Doom.Render.View

View-angle ↔ screen-column mapping (`r_main.c` `R_InitTextureMapping`).
-/

namespace Doom.Render.View

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Render.Constants
open Doom.Render.Types hiding sbarHeight
open Doom.Render.Util

structure ViewMapping where
  viewangletox : Array Int32
  xtoviewangle : Array UInt32
  clipangle : UInt32
  deriving Repr

/-- View parameters for BSP line classification (`r_bsp.c` globals). -/
structure RenderViewCtx where
  viewx : Int32
  viewy : Int32
  viewangle : UInt32
  mapping : ViewMapping
  deriving Repr

/-- Pending `R_SetViewSize` request (`r_main.c` `setsizeneeded` / `setblocks` / `setdetail`). -/
structure ViewSizePending where
  setsizeneeded : Bool := false
  setblocks : Int32 := 0
  setdetail : Int32 := 0
  deriving Repr

/-- Result of `R_ExecuteSetViewSize` (`r_main.c`). Defaults match blocks=10 unit tests. -/
structure ViewSize where
  scaledviewwidth : Int32 := 320
  viewwidth : Int32 := 320
  viewheight : Int32 := 168
  centerx : Int32 := 160
  centery : Int32 := 84
  centerxfrac : Int32 := (160 : Int32) <<< 16
  centeryfrac : Int32 := (84 : Int32) <<< 16
  projection : Int32 := (160 : Int32) <<< 16
  detailshift : Nat := 0
  viewwindowx : Int32 := 0
  viewwindowy : Int32 := 0
  mapping : ViewMapping := { viewangletox := #[], xtoviewangle := #[], clipangle := 0 }
  screenheightarray : Array Int32 := #[]
  yslope : Array Int32 := #[]
  distscale : Array Int32 := #[]
  pspritescale : Int32 := FRACUNIT
  pspriteiscale : Int32 := FRACUNIT
  deriving Repr

/--
`R_InitBuffer` view-window origin only. `columnofs` / `ylookup` are unused
under Lean `(x, y)` addressing.
-/
def initBuffer (width height : Int32) : Int32 × Int32 :=
  let viewwindowx := (Int32.ofNat screenWidth - width) >>> 1
  let viewwindowy :=
    if width == Int32.ofNat screenWidth then
      (0 : Int32)
    else
      (Int32.ofNat screenHeight - Int32.ofNat sbarHeight - height) >>> 1
  (viewwindowx, viewwindowy)

/-- `R_SetViewSize`: record pending size; applied on the next `R_ExecuteSetViewSize`. -/
def setViewSize (blocks detail : Int32) : ViewSizePending :=
  { setsizeneeded := true, setblocks := blocks, setdetail := detail }

private def twoFracUnit : Int32 := FRACUNIT * 2

private def halfFineAngles : Nat := FINEANGLES / 2

private def focallengthTanIdx : Nat := FINEANGLES / 4 + fieldOfView / 2

private def calcViewAngleToX (i : Nat) (centerxfrac focallength viewwidth : Int32) : Int32 :=
  let ft := finetangent.getD i 0
  if ft > twoFracUnit then
    (-1 : Int32)
  else if ft < -twoFracUnit then
    viewwidth + 1
  else
    let mul := fixedMul ft focallength
    let raw := (centerxfrac - mul + FRACUNIT - 1) >>> 16
    if raw < -1 then
      (-1 : Int32)
    else if raw > viewwidth + 1 then
      viewwidth + 1
    else
      raw

private def buildViewAngleToX (centerxfrac focallength viewwidth : Int32) : Array Int32 :=
  let rec go (i : Nat) (acc : Array Int32) : Array Int32 :=
    if i >= halfFineAngles then
      acc
    else
      go (i + 1) (acc.push (calcViewAngleToX i centerxfrac focallength viewwidth))
  go 0 #[]

private def findXtoviewIdx (viewangletox : Array Int32) (x : Int32) : Nat :=
  Id.run do
    let mut i := 0
    while viewangletox.getD i 0 > x && i < halfFineAngles do
      i := i + 1
    return i

private def buildXtoViewAngle (viewwidth : Int32) (viewangletox : Array Int32) : Array UInt32 :=
  let n := i32ToNat viewwidth + 1
  let rec go (x : Nat) (acc : Array UInt32) : Array UInt32 :=
    if x >= n then
      acc
    else
      let idx := findXtoviewIdx viewangletox (Int32.ofNat x)
      let ang := (idx.toUInt32 <<< ANGLETOFINESHIFT.toUInt32) - ANG90
      go (x + 1) (acc.push ang)
  go 0 #[]

private def applyFenceposts (viewangletox : Array Int32) (viewwidth : Int32) : Array Int32 :=
  viewangletox.map fun v =>
    if v == -1 then
      (0 : Int32)
    else if v == viewwidth + 1 then
      viewwidth
    else
      v

/-- `R_InitTextureMapping` for the given `viewwidth` and screen center `centerx`. -/
def initViewMapping (viewwidth centerx : Int32) : ViewMapping :=
  let centerxfrac := centerx <<< 16
  let focallength := fixedDiv centerxfrac (finetangent.getD focallengthTanIdx 0)
  let viewangletoxRaw := buildViewAngleToX centerxfrac focallength viewwidth
  let xtoviewangle := buildXtoViewAngle viewwidth viewangletoxRaw
  let viewangletox := applyFenceposts viewangletoxRaw viewwidth
  let clipangle := xtoviewangle.getD 0 0
  { viewangletox, xtoviewangle, clipangle }

/-- `R_PointOnSegSide` (`r_main.c`). `true` is the back side. -/
def pointOnSegSide (x y lx ly v2x v2y : Int32) : Bool :=
  let ldx := v2x - lx
  let ldy := v2y - ly
  if ldx == 0 then
    if x <= lx then ldy > 0 else ldy < 0
  else if ldy == 0 then
    if y <= ly then ldx < 0 else ldx > 0
  else
    let dx := x - lx
    let dy := y - ly
    let signBit : Int32 := Int32.minValue
    if ((ldy ^^^ ldx ^^^ dx ^^^ dy) &&& signBit) != 0 then
      ((ldy ^^^ dx) &&& signBit) != 0
    else
      let left := fixedMul (ldy >>> 16) dx
      let right := fixedMul dy (ldx >>> 16)
      right >= left

/-- `R_PointToDist` (`r_main.c`) — distance from `(viewx, viewy)` to `(x, y)`. -/
def pointToDist (viewx viewy x y : Int32) : Int32 :=
  let dx0 := cabs (x - viewx)
  let dy0 := cabs (y - viewy)
  let (dx, dy) := if dy0 > dx0 then (dy0, dx0) else (dx0, dy0)
  let frac : Int32 := if dx != 0 then fixedDiv dy dx else 0
  let ang := (tantoangle.getD (i32ToNat (frac >>> 5)) 0) + ANG90
  let angleIdx := (ang >>> ANGLETOFINESHIFT.toUInt32).toNat
  fixedDiv dx (finesine.getD angleIdx 0)

/--
`R_ScaleFromGlobalAngle` (`r_main.c`) — horizontal texture scale at `visangle`
given precomputed `rwDistance`.
-/
def scaleFromGlobalAngle (visangle viewangle rwNormalAngle : UInt32)
    (rwDistance projection : Int32) (_detailshift : Nat := detailShift) : Int32 :=
  let anglea := ANG90 + (visangle - viewangle)
  let angleb := ANG90 + (visangle - rwNormalAngle)
  let sinea := finesine.getD ((anglea >>> ANGLETOFINESHIFT.toUInt32).toNat) 0
  let sineb := finesine.getD ((angleb >>> ANGLETOFINESHIFT.toUInt32).toNat) 0
  let num : Int32 := fixedMul projection sineb
  let den := fixedMul rwDistance sinea
  if den > (num >>> 16) then
    let scale0 := fixedDiv num den
    let scale1 := if scale0 > 64 * FRACUNIT then 64 * FRACUNIT else scale0
    if scale1 < 256 then 256 else scale1
  else
    64 * FRACUNIT

end Doom.Render.View
