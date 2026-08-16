import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Level
import Doom.Playsim.Tables
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Draw
import Doom.Render.Gfx.Texture
import Doom.Render.Seg
import Doom.Render.Tables
import Doom.Render.Types
import Doom.Render.View
import Doom.Wad

/-!
# StorewallTwosidedTest

R1i-storewall-twosided unit tests: `storeWallTwoSided` two-sided branch fields.
-/

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Level
open Doom.Playsim.Tables
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Draw
open Doom.Render.Gfx.Texture
open Doom.Render.Seg
open Doom.Render.Tables
open Doom.Render.Types
open Doom.Render.View
open Doom.Wad

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def identityColormaps : ByteArray :=
  ByteArray.mk (Array.ofFn (n := 8192) (fun i => UInt8.ofNat (i % 256)))

private def synthColumnSource : ByteArray :=
  ByteArray.mk (Array.ofFn (n := 128) (fun i => UInt8.ofNat i))

private def synthWad : WadDirectory :=
  let size := UInt32.ofNat synthColumnSource.size
  {
    identification := ByteArray.mk #[73, 87, 65, 68], numlumps := 2, infotableofs := 0,
    entries := #[
      { filepos := 0, size := 0, name := nameTo8 "DUMMY" },
      { filepos := 0, size := size, name := nameTo8 "COLUMN0" }
    ],
    data := synthColumnSource
  }

private def synthTextureTables : TextureTables :=
  {
    firstFlat := 0, numFlats := 0, skyFlatNum := 0, numTextures := 2,
    textures := #[
      { name := nameTo8 "-", width := 1, height := 1, patches := #[] },
      {
        name := nameTo8 "WALL", width := 1, height := 128,
        patches := #[{ originx := 0, originy := 0, patchLump := 0 }]
      }
    ],
    columnLump := #[#[(0 : Int32)], #[(1 : Int32)]],
    columnOfs := #[#[0], #[0]],
    widthMask := #[0, 0],
    height := #[1, 128],
    compositeSize := #[0, 0],
    composite := #[none, none],
    hashHead := #[none, some 1],
    hashNext := #[none, none]
  }

private def synthRenderData : RenderData :=
  {
    playpal := ByteArray.empty
    colormaps := identityColormaps
    textureTables := synthTextureTables
    firstSprite := 0
    spriteMetrics := #[]
  }

private def mkClipArrays : Array Int32 × Array Int32 :=
  let ceil := Array.ofFn (n := 320) (fun _ => (-1 : Int32))
  let floor := Array.ofFn (n := 320) (fun _ => defaultViewheight)
  (ceil, floor)

private def flatCeiling : ByteArray := ByteArray.mk (Array.replicate 8 (UInt8.ofNat 65))
private def flatFloor : ByteArray := ByteArray.mk (Array.replicate 8 (UInt8.ofNat 66))

private def skyCeiling : ByteArray :=
  ByteArray.mk #[70, 95, 83, 75, 89, 49, 0, 0]

private def viewz : Int32 := 41 * FRACUNIT

private def baseTwoSidedInput
    (frontFloor frontCeil backFloor backCeil : Int32)
    (start stop : Int32) (xto : Array UInt32) : StoreWallInput :=
  let (ceilClip, floorClip) := mkClipArrays
  {
    viewx := 0, viewy := 0, viewz := viewz, viewangle := 0,
    rwAngle1 := 0, segAngle := 0, segOffset := 0,
    v1x := 0, v1y := 0, v2x := 64 * FRACUNIT, v2y := 0,
    linedefFlags := 0, textureoffset := 0, rowoffset := 0,
    midtexture := 0, midtextureHeight := 0,
    floorheight := frontFloor, ceilingheight := frontCeil,
    floorpic := flatFloor, ceilingpic := flatCeiling, lightlevel := 128,
    backFloorheight := backFloor, backCeilingheight := backCeil,
    backFloorpic := flatFloor, backCeilingpic := flatCeiling, backLightlevel := 128,
    toptexture := 1, bottomtexture := 1,
    toptextureHeight := 64, bottomtextureHeight := 64,
    start := start, stop := stop,
    xtoviewangle := xto,
    ceilingclip := ceilClip, floorclip := floorClip
  }

private def expectedPix (centery rwScale rwScaleStep world : Int32) : Int32 × Int32 :=
  let centeryfrac := centery <<< 16
  let world4 := world >>> 4
  let pix := (centeryfrac >>> 4) - fixedMul world4 rwScale
  let pixstep := -fixedMul rwScaleStep world4
  (pix, pixstep)

private def testCentery : Int32 := 84

def main (_args : List String) : IO UInt32 := do
  let data := synthRenderData
  let fb0 := Framebuffer.initBlack
  let mapping := initViewMapping 320 160
  let mut ok := true

  -- 1. Window: back ceiling lower → upper gap, toptexture + markceiling
  let windowInp :=
    { baseTwoSidedInput 0 (128 * FRACUNIT) 0 (96 * FRACUNIT) 10 10 mapping.xtoviewangle with
      midtexture := 1 }
  match storeWallTwoSided data synthWad windowInp fb0 with
  | Except.error e =>
    IO.eprintln s!"window case failed: {e}"
    ok := false
  | Except.ok (resW, _) =>
    ok := (← assert "window markceiling" resW.markceiling) && ok
    ok := (← assert "window toptexture" (resW.toptexture == 1)) && ok
    ok := (← assert "window bottomtexture absent" (resW.bottomtexture == 0)) && ok
    ok := (← assert "window drawSegIdx" (resW.drawSegIdx == 1)) && ok
    ok := (← assert "window maskedtexture" resW.maskedtexture) && ok
    ok := (← assert "window midtexture persisted" (resW.midtexture == 1)) && ok
    ok := (← assert "window maskedtexturecol size" (resW.maskedtexturecol.size == 1)) && ok
    ok := (← assert "window SIL_TOP bump" ((resW.silhouette &&& silTop) != 0)) && ok
    ok := (← assert "window SIL_BOTTOM bump" ((resW.silhouette &&& silBottom) != 0)) && ok
    ok := (← assert "window tsilheight INT_MIN" (resW.tsilheight == Int32.minValue)) && ok
    ok := (← assert "window bsilheight INT_MAX" (resW.bsilheight == Int32.maxValue)) && ok
    ok := (← assert "window sprtopclip snapshot"
      (match resW.sprtopclip with
       | some a => a.getD 10 999 == resW.ceilingclip.getD 10 998
       | none => false)) && ok
    ok := (← assert "window sprbottomclip snapshot"
      (match resW.sprbottomclip with
       | some a => a.getD 10 999 == resW.floorclip.getD 10 998
       | none => false)) && ok

  -- 2. Step: front floor higher than back → SIL_BOTTOM + markfloor
  let stepInp :=
    baseTwoSidedInput (32 * FRACUNIT) (128 * FRACUNIT) 0 (128 * FRACUNIT) 10 10
      mapping.xtoviewangle
  match storeWallTwoSided data synthWad stepInp fb0 with
  | Except.error e =>
    IO.eprintln s!"step case failed: {e}"
    ok := false
  | Except.ok (resS, _) =>
    ok := (← assert "step markfloor" resS.markfloor) && ok
    ok := (← assert "step SIL_BOTTOM"
      ((resS.silhouette &&& silBottom) != 0)) && ok
    ok := (← assert "step bsilheight" (resS.bsilheight == 32 * FRACUNIT)) && ok
    ok := (← assert "step sprbottomclip snapshot"
      (match resS.sprbottomclip with
       | some a => a.getD 10 999 == resS.floorclip.getD 10 998
       | none => false)) && ok
    ok := (← assert "step sprtopclip null" resS.sprtopclip.isNone) && ok

  -- 3. Sky + sky outdoor hack: worldtop == worldhigh after hack
  let skyInp :=
    { (baseTwoSidedInput 0 (128 * FRACUNIT) 0 (256 * FRACUNIT) 10 10 mapping.xtoviewangle) with
      ceilingpic := skyCeiling, backCeilingpic := skyCeiling }
  match storeWallTwoSided data synthWad skyInp fb0 with
  | Except.error e =>
    IO.eprintln s!"sky hack failed: {e}"
    ok := false
  | Except.ok (resSk, _) =>
    ok := (← assert "sky worldtop hack" (resSk.worldtop == resSk.worldhigh)) && ok
    ok := (← assert "sky SIL_TOP" ((resSk.silhouette &&& silTop) != 0)) && ok
    ok := (← assert "sky tsilheight" (resSk.tsilheight == 128 * FRACUNIT)) && ok

  -- 4. Closed door: both marks true
  let closedInp :=
    baseTwoSidedInput 0 (128 * FRACUNIT) (128 * FRACUNIT) (256 * FRACUNIT) 10 10
      mapping.xtoviewangle
  match storeWallTwoSided data synthWad closedInp fb0 with
  | Except.error e =>
    IO.eprintln s!"closed door failed: {e}"
    ok := false
  | Except.ok (resC, _) =>
    ok := (← assert "closed markfloor" resC.markfloor) && ok
    ok := (← assert "closed markceiling" resC.markceiling) && ok
    ok := (← assert "closed sprtopclip screenheightarray"
      (match resC.sprtopclip with
       | some a => a == data.screenheightarray
       | none => false)) && ok

  let closedBotInp :=
    baseTwoSidedInput 0 (128 * FRACUNIT) 0 0 10 10 mapping.xtoviewangle
  match storeWallTwoSided data synthWad closedBotInp fb0 with
  | Except.error e =>
    IO.eprintln s!"closed door bottom failed: {e}"
    ok := false
  | Except.ok (resCb, _) =>
    ok := (← assert "closed-bot sprbottomclip negonearray"
      (match resCb.sprbottomclip with
       | some a => a == data.negonearray
       | none => false)) && ok

  -- 5. Same plane both sides → marks false
  let sameInp :=
    baseTwoSidedInput 0 (128 * FRACUNIT) 0 (128 * FRACUNIT) 10 10 mapping.xtoviewangle
  match storeWallTwoSided data synthWad sameInp fb0 with
  | Except.error e =>
    IO.eprintln s!"same plane failed: {e}"
    ok := false
  | Except.ok (resP, _) =>
    ok := (← assert "same plane markfloor false" (!resP.markfloor)) && ok
    ok := (← assert "same plane markceiling false" (!resP.markceiling)) && ok

  -- 6. Peg flags golden mids
  let pegBase :=
    baseTwoSidedInput 0 (128 * FRACUNIT) (32 * FRACUNIT) (96 * FRACUNIT) 10 10
      mapping.xtoviewangle
  let pegTop :=
    { pegBase with linedefFlags := mlDontpegTop, rowoffset := 4 * FRACUNIT }
  let pegBot :=
    { pegBase with linedefFlags := mlDontpegBottom, rowoffset := 8 * FRACUNIT }
  match storeWallTwoSided data synthWad pegBase fb0,
    storeWallTwoSided data synthWad pegTop fb0,
    storeWallTwoSided data synthWad pegBot fb0 with
  | Except.error e, _, _ =>
    IO.eprintln s!"peg default failed: {e}"
    ok := false
  | _, Except.error e, _ =>
    IO.eprintln s!"peg top failed: {e}"
    ok := false
  | _, _, Except.error e =>
    IO.eprintln s!"peg bottom failed: {e}"
    ok := false
  | Except.ok (resDef, _), Except.ok (resTop, _), Except.ok (resBot, _) =>
    let worldtop := (128 * FRACUNIT) - viewz
    let worldlow := (32 * FRACUNIT) - viewz
    let defaultTopMid :=
      (96 * FRACUNIT) + Int32.ofNat 64 * FRACUNIT - viewz
    ok := (← assert "peg default top mid" (resDef.rwTopTextureMid == defaultTopMid)) && ok
    ok := (← assert "peg top mid" (resTop.rwTopTextureMid == worldtop + 4 * FRACUNIT)) && ok
    ok := (← assert "peg bottom mid" (resBot.rwBottomTextureMid == worldtop + 8 * FRACUNIT)) && ok
    ok := (← assert "peg default bottom mid" (resDef.rwBottomTextureMid == worldlow)) && ok

  -- 7. Pixhigh/pixlow step golden values
  let pixInp :=
    baseTwoSidedInput 0 (128 * FRACUNIT) (32 * FRACUNIT) (96 * FRACUNIT) 50 50
      mapping.xtoviewangle
  match storeWallTwoSided data synthWad pixInp fb0 with
  | Except.error e =>
    IO.eprintln s!"pix steps failed: {e}"
    ok := false
  | Except.ok (resPix, _) =>
    let (expHigh, expHighStep) :=
      expectedPix testCentery resPix.rwScale resPix.rwScaleStep ((96 * FRACUNIT) - viewz)
    let (expLow, expLowStep) :=
      expectedPix testCentery resPix.rwScale resPix.rwScaleStep ((32 * FRACUNIT) - viewz)
    ok := (← assert "pixhigh" (resPix.pixhigh == expHigh)) && ok
    ok := (← assert "pixhighstep" (resPix.pixhighstep == expHighStep)) && ok
    ok := (← assert "pixlow" (resPix.pixlow == expLow)) && ok
    ok := (← assert "pixlowstep" (resPix.pixlowstep == expLowStep)) && ok

  -- 8b. Riser with wall in front of view: lower strip, floorclip = mid
  let riserInp :=
    { baseTwoSidedInput 0 (128 * FRACUNIT) (32 * FRACUNIT) (128 * FRACUNIT) 160 160
        mapping.xtoviewangle with
      v1x := 128 * FRACUNIT, v1y := 64 * FRACUNIT
      v2x := 128 * FRACUNIT, v2y := (-64) * FRACUNIT }
  match storeWallTwoSided data synthWad riserInp fb0 with
  | Except.error e =>
    IO.eprintln s!"riser store failed: {e}"
    ok := false
  | Except.ok (resR, _) =>
    let midR := (resR.pixlow + Int32.ofNat (heightUnit - 1)) >>> 12
    let yhR0 := resR.bottomfrac >>> 12
    let yhR := if yhR0 >= defaultViewheight then defaultViewheight - 1 else yhR0
    let midR' := if midR <= (-1 : Int32) then (0 : Int32) else midR
    ok := (← assert "riser bottomtexture selected" (resR.bottomtexture == 1)) && ok
    ok := (← assert "riser worldlow above worldbottom"
      (resR.worldlow > resR.worldbottom)) && ok
    ok := (← assert "riser lower span mid <= yh" (midR' <= yhR)) && ok
    ok := (← assert "riser floorclip is mid"
      (resR.floorclip.getD 160 200 == midR')) && ok

  -- 8. Rangecheck + maxDrawSegs guards
  let badStart := { windowInp with start := 320, stop := 325 }
  match storeWallTwoSided data synthWad badStart fb0 with
  | Except.error e =>
    ok := (← assert "rangecheck bad start"
      (e == "Bad R_RenderWallRange: 320 to 325")) && ok
  | Except.ok _ =>
    ok := (← assert "rangecheck bad start should error" false) && ok
  let fullInp := { windowInp with drawSegIdx := maxDrawSegs }
  match storeWallTwoSided data synthWad fullInp fb0 with
  | Except.error e =>
    IO.eprintln s!"maxDrawSegs no-op failed: {e}"
    ok := false
  | Except.ok (resF, fbF) =>
    ok := (← assert "maxDrawSegs drawSegIdx unchanged" (resF.drawSegIdx == maxDrawSegs)) && ok
    ok := (← assert "maxDrawSegs fb unchanged" (fbF.pixels == fb0.pixels)) && ok
    ok := (← assert "maxDrawSegs rwScale zero" (resF.rwScale == 0)) && ok

  if ok then
    IO.println "storewall-twosided-test: all passed"
    pure 0
  else
    IO.eprintln "storewall-twosided-test: failures"
    pure 1
