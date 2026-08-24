import Doom.Playsim.Fixed
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Draw
import Doom.Render.Gfx.Texture
import Doom.Render.Plane
import Doom.Render.Seg
import Doom.Render.Types
import Doom.Wad

/-!
# SegloopTest

R1g-segs-loop unit tests: `R_RenderSegLoop` midtexture and two-sided tier goldens.
-/

open Doom.Playsim.Fixed
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Draw
open Doom.Render.Gfx.Texture
open Doom.Render.Plane
open Doom.Render.Seg
open Doom.Render.Types
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

private def synthMaskedPostColumn : ByteArray :=
  ByteArray.mk #[0, 5, 0, 84, 85, 86, 87, 88, 0xff]

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

private def synthMaskedWad : WadDirectory :=
  let size := UInt32.ofNat synthMaskedPostColumn.size
  {
    identification := ByteArray.mk #[73, 87, 65, 68], numlumps := 2, infotableofs := 0,
    entries := #[
      { filepos := 0, size := 0, name := nameTo8 "DUMMY" },
      { filepos := 0, size := size, name := nameTo8 "MASKCOL" }
    ],
    data := synthMaskedPostColumn
  }

private def synthTextureTables : TextureTables :=
  {
    firstFlat := 0, numFlats := 0, skyFlatNum := 0, numTextures := 3,
    textures := #[
      { name := nameTo8 "-", width := 1, height := 1, patches := #[] },
      {
        name := nameTo8 "WALL", width := 1, height := 128,
        patches := #[{ originx := 0, originy := 0, patchLump := 0 }]
      },
      {
        name := nameTo8 "MASK", width := 1, height := 128,
        patches := #[{ originx := 0, originy := 0, patchLump := 1 }]
      }
    ],
    columnLump := #[#[(0 : Int32)], #[(1 : Int32)], #[(1 : Int32)]],
    columnOfs := #[#[0], #[0], #[3]],
    widthMask := #[0, 0, 0],
    height := #[1, 128, 128],
    compositeSize := #[0, 0, 0],
    composite := #[none, none, none],
    hashHead := #[none, some 1, some 2],
    hashNext := #[none, none, none]
  }

private def synthRenderData : RenderData :=
  {
    playpal := ByteArray.empty
    colormaps := identityColormaps
    textureTables := synthTextureTables
    firstSprite := 0
    spriteMetrics := #[]
  }

private def walllightsIdentity : Array Nat :=
  Array.ofFn (n := 48) (fun i => i)

private def fbPixel (fb : Framebuffer) (x y : Nat) : UInt8 :=
  fb.get x y

private def columnStrip (fb : Framebuffer) (x y0 y1 : Nat) : List UInt8 :=
  (List.range (y1 - y0 + 1)).map (fun d => fbPixel fb x (y0 + d))

private def goldenColumnStrip : List UInt8 :=
  [84, 84, 85, 86, 87].map UInt8.ofNat

private def mkClipArrays : Array Int32 × Array Int32 :=
  let ceil := Array.ofFn (n := 320) (fun _ => (-1 : Int32))
  let floor := Array.ofFn (n := 320) (fun _ => defaultViewheight)
  (ceil, floor)

private def goldenSegState : SegLoopState :=
  let (ceil, floor) := mkClipArrays
  {
    rwX := 10, rwStopX := 11,
    rwScale := FRACUNIT, rwScaleStep := 0,
    rwMidTextureMid := Int32.ofNat (84 * 65536),
    rwCenterAngle := 0, rwDistance := 0, rwOffset := 0,
    midtexture := 1,
    walllights := walllightsIdentity,
    ceilingclip := ceil, floorclip := floor,
    markceiling := true, markfloor := true, segtextured := true,
    topfrac := Int32.ofNat (84 * 4096 - 4095),
    topstep := 0,
    bottomfrac := Int32.ofNat (88 * 4096),
    bottomstep := 0,
    xtoviewangle := Array.replicate 320 0,
    textureTables := synthTextureTables
  }

def main (_args : List String) : IO UInt32 := do
  let data := synthRenderData
  let fb0 := Framebuffer.initBlack
  let mut ok := true

  match renderSegLoop data synthWad goldenSegState fb0 with
  | Except.error e =>
    IO.eprintln s!"renderSegLoop golden failed: {e}"
    ok := false
  | Except.ok (stOut, fbCol) =>
    let strip := columnStrip fbCol 10 84 88
    ok := (← assert "golden column strip" (strip == goldenColumnStrip)) && ok
    ok := (← assert "golden advances rwX" (stOut.rwX == 11)) && ok
    ok := (← assert "golden ceilingclip" (stOut.ceilingclip.getD 10 0 == defaultViewheight)) && ok
    ok := (← assert "golden floorclip" (stOut.floorclip.getD 10 0 == (-1 : Int32))) && ok

  let (ceil0, floor0) := mkClipArrays
  let zeroIter := { goldenSegState with rwX := 15, rwStopX := 15, ceilingclip := ceil0, floorclip := floor0 }
  match renderSegLoop data synthWad zeroIter fb0 with
  | Except.error e =>
    IO.eprintln s!"zero-iteration failed: {e}"
    ok := false
  | Except.ok (stZ, fbZ) =>
    ok := (← assert "zero-iteration fb unchanged" (fbZ.pixels == fb0.pixels)) && ok
    ok := (← assert "zero-iteration state unchanged"
      (stZ.rwX == 15 && stZ.rwScale == zeroIter.rwScale &&
        stZ.topfrac == zeroIter.topfrac && stZ.bottomfrac == zeroIter.bottomfrac)) && ok

  let (ceil1, floor1) := mkClipArrays
  let ceilWide := ceil1.push ((-1 : Int32))
  let floorWide := floor1.push defaultViewheight
  let badX := { goldenSegState with rwX := 320, rwStopX := 321, ceilingclip := ceilWide, floorclip := floorWide }
  match renderSegLoop data synthWad badX fb0 with
  | Except.error e =>
    ok := (← assert "rangecheck x OOB" (e == "R_DrawColumn: 84 to 88 at 320")) && ok
  | Except.ok _ =>
    ok := (← assert "rangecheck x OOB should error" false) && ok

  let (ceil2, floor2) := mkClipArrays
  let noMid := { goldenSegState with midtexture := 0, ceilingclip := ceil2, floorclip := floor2 }
  match renderSegLoop data synthWad noMid fb0 with
  | Except.error e =>
    IO.eprintln s!"no midtexture failed: {e}"
    ok := false
  | Except.ok (stN, fbN) =>
    ok := (← assert "no midtexture no paint" (fbN.pixels == fb0.pixels)) && ok
    ok := (← assert "no midtexture mark clips"
      (stN.ceilingclip.getD 10 0 == 83 && stN.floorclip.getD 10 0 == 89)) && ok

  let (ceil3, floor3) := mkClipArrays
  let topOnly := {
    goldenSegState with
      midtexture := 0, toptexture := 1, segtextured := true,
      markfloor := false,
      ceilingclip := ceil3, floorclip := floor3,
      pixhigh := Int32.ofNat (86 * 4096), pixhighstep := 0,
      rwTopTextureMid := Int32.ofNat (84 * 65536)
  }
  match renderSegLoop data synthWad topOnly fb0 with
  | Except.error e =>
    IO.eprintln s!"top texture failed: {e}"
    ok := false
  | Except.ok (stTop, fbTop) =>
    let strip := columnStrip fbTop 10 84 86
    ok := (← assert "top texture column strip" (strip == goldenColumnStrip.take 3)) && ok
    ok := (← assert "top texture ceilingclip" (stTop.ceilingclip.getD 10 0 == 86)) && ok
    ok := (← assert "top texture floorclip unchanged" (stTop.floorclip.getD 10 0 == defaultViewheight)) && ok

  let (ceil4, floor4) := mkClipArrays
  let bottomOnly := {
    goldenSegState with
      midtexture := 0, bottomtexture := 1, segtextured := true,
      markceiling := false,
      ceilingclip := ceil4, floorclip := floor4,
      pixlow := Int32.ofNat (87 * 4096), pixlowstep := 0,
      rwBottomTextureMid := Int32.ofNat (84 * 65536)
  }
  match renderSegLoop data synthWad bottomOnly fb0 with
  | Except.error e =>
    IO.eprintln s!"bottom texture failed: {e}"
    ok := false
  | Except.ok (stBot, fbBot) =>
    let strip := columnStrip fbBot 10 87 88
    ok := (← assert "bottom texture column strip" (strip == goldenColumnStrip.drop 3)) && ok
    ok := (← assert "bottom texture floorclip" (stBot.floorclip.getD 10 0 == 87)) && ok
    ok := (← assert "bottom texture ceilingclip unchanged" (stBot.ceilingclip.getD 10 0 == (-1 : Int32))) && ok

  let (ceil5, floor5) := mkClipArrays
  let combined := {
    goldenSegState with
      midtexture := 0, toptexture := 1, bottomtexture := 1, segtextured := true,
      ceilingclip := ceil5, floorclip := floor5,
      pixhigh := Int32.ofNat (86 * 4096), pixhighstep := 0,
      pixlow := Int32.ofNat (87 * 4096), pixlowstep := 0,
      rwTopTextureMid := Int32.ofNat (84 * 65536),
      rwBottomTextureMid := Int32.ofNat (84 * 65536)
  }
  match renderSegLoop data synthWad combined fb0 with
  | Except.error e =>
    IO.eprintln s!"combined tiers failed: {e}"
    ok := false
  | Except.ok (stComb, fbComb) =>
    let strip := columnStrip fbComb 10 84 88
    ok := (← assert "combined tiers column strip" (strip == goldenColumnStrip)) && ok
    ok := (← assert "combined tiers ceilingclip" (stComb.ceilingclip.getD 10 0 == 86)) && ok
    ok := (← assert "combined tiers floorclip" (stComb.floorclip.getD 10 0 == 87)) && ok

  let (ceil6, floor6) := mkClipArrays
  let (vpCeil, ceilIdx) ←
    match findPlane initVisPlaneState (128 * FRACUNIT) 1 128 0 with
    | Except.ok p => pure p
    | Except.error e => IO.eprintln s!"mark-only findPlane ceiling: {e}"; ok := false; pure (initVisPlaneState, 0)
  let (vpBoth, floorIdx) ←
    match findPlane vpCeil 0 2 128 0 with
    | Except.ok p => pure p
    | Except.error e => IO.eprintln s!"mark-only findPlane floor: {e}"; ok := false; pure (vpCeil, 0)
  let markOnly := {
    goldenSegState with
      midtexture := 0, segtextured := false,
      markceiling := true, markfloor := true,
      ceilingclip := ceil6, floorclip := floor6,
      visplanes := vpBoth, floorplane := some floorIdx, ceilingplane := some ceilIdx
  }
  match renderSegLoop data synthWad markOnly fb0 with
  | Except.error e =>
    IO.eprintln s!"mark-only failed: {e}"
    ok := false
  | Except.ok (stMark, fbMark) =>
    ok := (← assert "mark-only no paint" (fbMark.pixels == fb0.pixels)) && ok
    ok := (← assert "mark-only ceilingclip" (stMark.ceilingclip.getD 10 0 == 83)) && ok
    ok := (← assert "mark-only floorclip" (stMark.floorclip.getD 10 0 == 89)) && ok
    match stMark.visplanes.visplanes[ceilIdx]?, stMark.visplanes.visplanes[floorIdx]? with
    | some cVp, some fVp =>
      ok := (← assert "mark-only ceiling visplane"
        (cVp.top.getD 10 (UInt8.ofNat 0xff) == UInt8.ofNat 0)) && ok
      ok := (← assert "mark-only floor visplane"
        (fVp.top.getD 10 (UInt8.ofNat 0xff) == UInt8.ofNat 89)) && ok
    | _, _ => ok := (← assert "mark-only visplane lookup" false) && ok

  let (ceil7, floor7) := mkClipArrays
  let maskedRec := {
    goldenSegState with
      midtexture := 0
      maskedtexture := true
      maskedtexturecol := #[Int32.ofNat shrtMax]
      maskedColBase := 10
      ceilingclip := ceil7
      floorclip := floor7
  }
  match renderSegLoop data synthWad maskedRec fb0 with
  | Except.error e =>
    IO.eprintln s!"masked-record failed: {e}"
    ok := false
  | Except.ok (stMask, fbMask) =>
    ok := (← assert "masked-record no paint" (fbMask.pixels == fb0.pixels)) && ok
    ok := (← assert "masked-record column saved" (stMask.maskedtexturecol.getD 0 0 == 0)) && ok
    ok := (← assert "masked-record ceilingclip unchanged" (stMask.ceilingclip.getD 10 0 == 83)) && ok
    ok := (← assert "masked-record floorclip unchanged" (stMask.floorclip.getD 10 0 == 89)) && ok

  let (ceil8, floor8) := mkClipArrays
  let replayRes : StoreWallResult := {
    drawSegIdx := 0
    rwScale := FRACUNIT
    rwScaleStep := 0
    rwDistance := 0
    rwOffset := 0
    rwMidTextureMid := 0
    rwCenterAngle := 0
    walllights := walllightsIdentity
    topfrac := 0
    topstep := 0
    bottomfrac := 0
    bottomstep := 0
    markceiling := true
    markfloor := true
    midtexture := 1
    maskedtexture := true
    maskedtexturecol := #[0]
    ceilingclip := ceil8
    floorclip := floor8
  }
  let replayInp : MaskedSegReplayInput := {
    res := replayRes
    texnum := 2
    segStart := 10
    x1 := 10
    x2 := 10
    viewz := 84 * FRACUNIT
    frontFloor := 0
    frontCeil := 84 * FRACUNIT
    backFloor := 0
    backCeil := 84 * FRACUNIT
    midtextureHeight := 128
  }
  match renderMaskedSegRange data synthMaskedWad replayInp fb0 with
  | Except.error e =>
    IO.eprintln s!"masked-replay failed: {e}"
    ok := false
  | Except.ok (resReplay, fbReplay) =>
    let strip := columnStrip fbReplay 10 84 88
    ok := (← assert "masked-replay golden strip" (strip == goldenColumnStrip)) && ok
    ok := (← assert "masked-replay column reset"
      (resReplay.maskedtexturecol.getD 0 0 == Int32.ofNat shrtMax)) && ok

  if ok then
    IO.println "segloop-test: all passed"
    pure 0
  else
    IO.eprintln "segloop-test: failures"
    pure 1
