import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Level
import Doom.Playsim.Tables
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Draw
import Doom.Render.Gfx.Texture
import Doom.Render.Plane
import Doom.Render.Seg
import Doom.Render.Tables
import Doom.Render.Types
import Doom.Render.View
import Doom.Wad

/-!
# StorewallTest

R1h-storewall-solid unit tests: `storeWallSolid`, `pointToDist`, `scaleFromGlobalAngle`.
-/

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Level
open Doom.Playsim.Tables
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Draw
open Doom.Render.Gfx.Texture
open Doom.Render.Plane
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

private def baseStoreInput (ceil floor : Int32) (flags : Int32) (v1x v1y v2x v2y : Int32)
    (start stop : Int32) (xto : Array UInt32) : StoreWallInput :=
  let (ceilClip, floorClip) := mkClipArrays
  {
    viewx := 0, viewy := 0, viewz := 41 * FRACUNIT, viewangle := 0,
    rwAngle1 := 0, segAngle := 0, segOffset := 0,
    v1x := v1x, v1y := v1y, v2x := v2x, v2y := v2y,
    linedefFlags := flags, textureoffset := 0, rowoffset := 0,
    midtexture := 1, midtextureHeight := 128,
    ceilingheight := ceil, floorheight := floor,
    ceilingpic := flatCeiling, lightlevel := 128,
    start := start, stop := stop,
    xtoviewangle := xto,
    ceilingclip := ceilClip, floorclip := floorClip
  }

private def fbPixel (fb : Framebuffer) (x y : Nat) : UInt8 :=
  fb.get x y

private def columnStrip (fb : Framebuffer) (x y0 y1 : Nat) : List UInt8 :=
  (List.range (y1 - y0 + 1)).map (fun d => fbPixel fb x (y0 + d))

private def goldenColumnStrip : List UInt8 :=
  [87, 87, 87, 87, 87].map UInt8.ofNat

def main (_args : List String) : IO UInt32 := do
  let data := synthRenderData
  let fb0 := Framebuffer.initBlack
  let mapping := initViewMapping 320 160
  let mut ok := true

  -- Golden single-sided wall (matches segloop-test column strip)
  let (vpSt0, ceilIdx) ←
    match findPlane initVisPlaneState (128 * FRACUNIT) 1 128 0 with
    | Except.ok p => pure p
    | Except.error e => IO.eprintln s!"findPlane ceiling failed: {e}"; ok := false; pure (initVisPlaneState, 0)
  let (vpSt1, floorIdx) ←
    match findPlane vpSt0 0 2 128 0 with
    | Except.ok p => pure p
    | Except.error e => IO.eprintln s!"findPlane floor failed: {e}"; ok := false; pure (vpSt0, 0)
  let goldenInp :=
    { (baseStoreInput (128 * FRACUNIT) 0 0 0 (-256 * FRACUNIT) (64 * FRACUNIT) (-256 * FRACUNIT) 10 10
        mapping.xtoviewangle) with
      viewz := 20 * FRACUNIT,
      midtexture := 0,
      visplanes := vpSt1, floorplane := some floorIdx, ceilingplane := some ceilIdx }
  match storeWallSolid data synthWad goldenInp fb0 with
  | Except.error e =>
    IO.eprintln s!"golden storeWallSolid failed: {e}"
    ok := false
  | Except.ok (res, _) =>
    ok := (← assert "visplane golden drawSegIdx" (res.drawSegIdx == 1)) && ok
    match res.ceilingplane, res.floorplane with
    | some cIdx, some fIdx =>
      ok := (← assert "visplane golden markceiling set" res.markceiling) && ok
      ok := (← assert "visplane golden markfloor set" res.markfloor) && ok
      match res.visplanes.visplanes[cIdx]?, res.visplanes.visplanes[fIdx]? with
      | some cVp, some fVp =>
        ok := (← assert "visplane golden ceiling checkPlane minx" (cVp.minx == 10)) && ok
        ok := (← assert "visplane golden floor checkPlane minx" (fVp.minx == 10)) && ok
      | _, _ => ok := (← assert "visplane golden lookup" false) && ok
    | _, _ => ok := (← assert "visplane golden refs" false) && ok

  -- Original golden column strip (viewz=41*FRACUNIT; no visplane marks — wall fills column)
  let goldenPaintInp :=
    baseStoreInput (128 * FRACUNIT) 0 0 0 (-256 * FRACUNIT) (64 * FRACUNIT) (-256 * FRACUNIT) 10 10
      mapping.xtoviewangle
  match storeWallSolid data synthWad goldenPaintInp fb0 with
  | Except.error e =>
    IO.eprintln s!"golden paint storeWallSolid failed: {e}"
    ok := false
  | Except.ok (res, fbCol) =>
    let strip := columnStrip fbCol 10 84 88
    ok := (← assert "golden column strip" (strip == goldenColumnStrip)) && ok
    ok := (← assert "golden rwMidTextureMid"
      (res.rwMidTextureMid == (128 * FRACUNIT) - (41 * FRACUNIT))) && ok
    ok := (← assert "golden drawSegIdx" (res.drawSegIdx == 1)) && ok
    ok := (← assert "golden ceilingclip" (res.ceilingclip.getD 10 0 == defaultViewheight)) && ok
    ok := (← assert "solid sprtopclip screenheightarray"
      (match res.sprtopclip with
       | some a => a == data.screenheightarray && a != res.ceilingclip
       | none => false)) && ok
    ok := (← assert "solid sprbottomclip negonearray"
      (match res.sprbottomclip with
       | some a => a == data.negonearray && a != res.floorclip
       | none => false)) && ok

  -- Peg modes
  let pegDefault :=
    baseStoreInput (128 * FRACUNIT) 0 0 0 64 0 0 10 10 mapping.xtoviewangle
  let pegBottom :=
    { pegDefault with
      linedefFlags := mlDontpegBottom, midtextureHeight := 64,
      v1x := 0, v1y := 256 * FRACUNIT, v2x := 64 * FRACUNIT, v2y := 256 * FRACUNIT }
  match storeWallSolid data synthWad pegDefault fb0, storeWallSolid data synthWad pegBottom fb0 with
  | Except.error e, _ =>
    IO.eprintln s!"peg default failed: {e}"
    ok := false
  | _, Except.error e =>
    IO.eprintln s!"peg bottom failed: {e}"
    ok := false
  | Except.ok (resDef, _), Except.ok (resBot, _) =>
    let worldtop := (128 * FRACUNIT) - (41 * FRACUNIT)
    ok := (← assert "peg default mid" (resDef.rwMidTextureMid == worldtop)) && ok
    let vtop := (64 * FRACUNIT) - (41 * FRACUNIT)
    ok := (← assert "peg bottom mid" (resBot.rwMidTextureMid == vtop)) && ok

  -- Scale at span ends
  let scaleInp :=
    { (baseStoreInput (128 * FRACUNIT) 0 0 (256 * FRACUNIT) (256 * FRACUNIT)
        (320 * FRACUNIT) (256 * FRACUNIT) 50 52 mapping.xtoviewangle) with
      segAngle := ANG90 }
  match storeWallSolid data synthWad scaleInp fb0 with
  | Except.error e =>
    IO.eprintln s!"scale span failed: {e}"
    ok := false
  | Except.ok (resS, _) =>
    let visStop := scaleInp.viewangle + mapping.xtoviewangle.getD 52 0
    let rwNormal := scaleInp.segAngle + ANG90
    let scaleStop :=
      scaleFromGlobalAngle visStop scaleInp.viewangle rwNormal resS.rwDistance defaultProjection
    ok := (← assert "scale start positive" (resS.rwScale > 0)) && ok
    ok := (← assert "scale step"
      (resS.rwScale + 2 * resS.rwScaleStep == scaleStop)) && ok

  -- Light table horizontal vs vertical
  let horizInp :=
    { (baseStoreInput (128 * FRACUNIT) 0 0 0 (256 * FRACUNIT) (64 * FRACUNIT) (256 * FRACUNIT) 10 10
        mapping.xtoviewangle) with lightlevel := 128 }
  let vertInp :=
    { horizInp with
      v1x := 256 * FRACUNIT, v1y := 0, v2x := 256 * FRACUNIT, v2y := 64 * FRACUNIT }
  match storeWallSolid data synthWad horizInp fb0, storeWallSolid data synthWad vertInp fb0 with
  | Except.error e, _ =>
    IO.eprintln s!"horiz light failed: {e}"
    ok := false
  | _, Except.error e =>
    IO.eprintln s!"vert light failed: {e}"
    ok := false
  | Except.ok (resH, _), Except.ok (resV, _) =>
    ok := (← assert "light horizontal row"
      (resH.walllights == scalelight.getD 7 #[])) && ok
    ok := (← assert "light vertical row"
      (resV.walllights == scalelight.getD 9 #[])) && ok
    let extraInp := { horizInp with extralight := 1 }
    match storeWallSolid data synthWad extraInp fb0 with
    | Except.error e =>
      IO.eprintln s!"extralight walllights failed: {e}"
      ok := false
    | Except.ok (resE, _) =>
      ok := (← assert "extralight shifts walllights row"
        (resE.walllights == scalelight.getD 8 #[] && resE.walllights != resH.walllights)) && ok

  -- C `r_segs.c`: `if (rw_normalangle-rw_angle1 < ANG180) rw_offset = -rw_offset`
  -- with textureoffset=segOffset=0 so the stored offset carries that sign.
  let offsetNegInp :=
    baseStoreInput (128 * FRACUNIT) 0 0 0 (-256 * FRACUNIT) (64 * FRACUNIT) (-256 * FRACUNIT) 10 10
      mapping.xtoviewangle
  let offsetPosInp := { offsetNegInp with rwAngle1 := ANG90 + 1 }
  match storeWallSolid data synthWad offsetNegInp fb0, storeWallSolid data synthWad offsetPosInp fb0 with
  | Except.error e, _ =>
    IO.eprintln s!"offset sign <ANG180 failed: {e}"
    ok := false
  | _, Except.error e =>
    IO.eprintln s!"offset sign >=ANG180 failed: {e}"
    ok := false
  | Except.ok (resNeg, _), Except.ok (resPos, _) =>
    ok := (← assert "rw_offset sign <ANG180 is negative" (resNeg.rwOffset < 0)) && ok
    ok := (← assert "rw_offset sign >=ANG180 is positive" (resPos.rwOffset > 0)) && ok

  -- Rangecheck
  let badStart := { goldenInp with start := 320, stop := 325 }
  match storeWallSolid data synthWad badStart fb0 with
  | Except.error e =>
    ok := (← assert "rangecheck bad start"
      (e == "Bad R_RenderWallRange: 320 to 325")) && ok
  | Except.ok _ =>
    ok := (← assert "rangecheck bad start should error" false) && ok
  let badOrder := { goldenInp with start := 12, stop := 10 }
  match storeWallSolid data synthWad badOrder fb0 with
  | Except.error e =>
    ok := (← assert "rangecheck start>stop"
      (e == "Bad R_RenderWallRange: 12 to 10")) && ok
  | Except.ok _ =>
    ok := (← assert "rangecheck start>stop should error" false) && ok

  -- maxDrawSegs overflow silent no-op
  let fullInp := { goldenInp with drawSegIdx := maxDrawSegs }
  match storeWallSolid data synthWad fullInp fb0 with
  | Except.error e =>
    IO.eprintln s!"maxDrawSegs no-op failed: {e}"
    ok := false
  | Except.ok (resF, fbF) =>
    ok := (← assert "maxDrawSegs drawSegIdx unchanged" (resF.drawSegIdx == maxDrawSegs)) && ok
    ok := (← assert "maxDrawSegs fb unchanged" (fbF.pixels == fb0.pixels)) && ok
    ok := (← assert "maxDrawSegs rwScale zero" (resF.rwScale == 0)) && ok

  if ok then
    IO.println "storewall-test: all passed"
    pure 0
  else
    IO.eprintln "storewall-test: failures"
    pure 1
