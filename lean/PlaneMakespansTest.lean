import Doom.Playsim.Fixed
import Doom.Render.Data
import Doom.Render.Gfx.Texture
import Doom.Render.Plane
import Doom.Render.Types
import Doom.Render.View
import Doom.Render.Util
import Doom.Wad

/-!
# PlaneMakespansTest

R1p-plane-makespans-mapplane: slope tables, `clearPlaneDraw`, `mapPlane`,
`makeSpans`, `drawVisPlaneMarks`.
-/

open Doom.Playsim.Fixed
open Doom.Render.Data
open Doom.Render.Gfx.Texture
open Doom.Render.Plane
open Doom.Render.Types
open Doom.Render.View
open Doom.Render.Util
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

private def levelColormaps : ByteArray :=
  ByteArray.mk (Array.ofFn (n := 8192) (fun i => UInt8.ofNat (40 + i / 256)))

private def synthFlatSource : ByteArray :=
  ByteArray.mk (Array.ofFn (n := 4096) (fun i => UInt8.ofNat (i % 256)))

private def synthFlatWad : WadDirectory :=
  let size := UInt32.ofNat synthFlatSource.size
  {
    identification := ByteArray.mk #[73, 87, 65, 68], numlumps := 2, infotableofs := 0,
    entries := #[
      { filepos := 0, size := 0, name := nameTo8 "DUMMY" },
      { filepos := 0, size := size, name := nameTo8 "FLAT1" }
    ],
    data := synthFlatSource
  }

private def synthRenderData (colormaps : ByteArray) : RenderData :=
  {
    playpal := ByteArray.empty
    colormaps := colormaps
    textureTables := {
      firstFlat := 0, numFlats := 0, skyFlatNum := 0, numTextures := 0,
      textures := #[], columnLump := #[], columnOfs := #[], widthMask := #[], height := #[]
      compositeSize := #[], composite := #[], hashHead := #[], hashNext := #[]
    }
    firstSprite := 0
    spriteMetrics := #[]
  }

private def rowStrip (fb : Framebuffer) (y x0 x1 : Nat) : List UInt8 :=
  (List.range (x1 - x0 + 1)).map (fun d => fb.get (x0 + d) y)

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let mapping := initViewMapping 320 160
  let ctx0 := initPlaneDrawCtx

  let ctxSlope := initPlaneSlopeTables ctx0 320 168 mapping.xtoviewangle
  ok := (← assert "yslope[84]" (ctxSlope.yslope.getD 84 0 == 20971520)) && ok
  ok := (← assert "yslope[99]" (ctxSlope.yslope.getD 99 0 == 676500)) && ok
  ok := (← assert "yslope[100]" (ctxSlope.yslope.getD 100 0 == 635500)) && ok
  ok := (← assert "distscale[160]" (ctxSlope.distscale.getD 160 0 == 65537)) && ok
  ok := (← assert "distscale[0]" (ctxSlope.distscale.getD 0 0 == 92789)) && ok
  ok := (← assert "distscale[319]" (ctxSlope.distscale.getD 319 0 == 92364)) && ok

  let ctxClear := clearPlaneDraw ctxSlope 0 ((160 : Int32) <<< 16)
  ok := (← assert "clear cachedheight zero"
    (ctxClear.cachedheight.all (· == 0))) && ok
  ok := (← assert "clear basexscale" (ctxClear.basexscale == 0)) && ok
  ok := (← assert "clear baseyscale" (ctxClear.baseyscale == 409)) && ok

  let data := synthRenderData identityColormaps
  let mut ctx := {
    ctxClear with
      planeheight := 64 * FRACUNIT
      planezlight := #[0, 1, 2, 3]
  }
  match mapPlane data ctx Framebuffer.initBlack 0 0 0 mapping.xtoviewangle 20 25 100 synthFlatSource with
  | Except.error e =>
    IO.eprintln s!"mapPlane golden failed: {e}"
    ok := false
  | Except.ok (ctxMap, fbMap) =>
    let row := rowStrip fbMap 100 20 25
    ok := (← assert "mapPlane golden row" (row == [236, 236, 236, 236, 236, 236].map UInt8.ofNat)) && ok
    ok := (← assert "mapPlane cache height"
      (ctxMap.cachedheight.getD 100 0 == 64 * FRACUNIT)) && ok
    ok := (← assert "mapPlane cache distance"
      (ctxMap.cacheddistance.getD 100 0 == 40672000)) && ok
    ctx := ctxMap

  match mapPlane data ctx Framebuffer.initBlack 0 0 0 mapping.xtoviewangle 30 35 100 synthFlatSource with
  | Except.error e =>
    IO.eprintln s!"mapPlane cache reuse failed: {e}"
    ok := false
  | Except.ok (ctxReuse, _) =>
    ok := (← assert "mapPlane cache reuse distance"
      (ctxReuse.cacheddistance.getD 100 0 == 40672000)) && ok

  let dataLevel := synthRenderData levelColormaps
  let ctxLevel := {
    ctxSlope with
      planeheight := FRACUNIT
      basexscale := 409
      baseyscale := 0
      planezlight := Array.replicate 128 5
  }
  match mapPlane dataLevel ctxLevel Framebuffer.initBlack 0 0 0 mapping.xtoviewangle 40 40 100 synthFlatSource with
  | Except.error e =>
    IO.eprintln s!"mapPlane colormap failed: {e}"
    ok := false
  | Except.ok (_, fbLevel) =>
    ok := (← assert "mapPlane colormap level 5" (fbLevel.get 40 100 == 45)) && ok

  for (label, x1, x2, y, expect) in [
    ("mapPlane x2<x1", (30 : Int32), (20 : Int32), (100 : Int32), "R_MapPlane: 30, 20 at 100"),
    ("mapPlane x1<0", (-1 : Int32), (5 : Int32), (100 : Int32), "R_MapPlane: -1, 5 at 100"),
    ("mapPlane x2>=width", (20 : Int32), (320 : Int32), (100 : Int32), "R_MapPlane: 20, 320 at 100"),
    ("mapPlane y>height", (20 : Int32), (25 : Int32), (201 : Int32), "R_MapPlane: 20, 25 at 201"),
    ("mapPlane y>168", (20 : Int32), (25 : Int32), (169 : Int32), "R_MapPlane: 20, 25 at 169")
  ] do
    match mapPlane data ctxSlope Framebuffer.initBlack 0 0 0 mapping.xtoviewangle x1 x2 y synthFlatSource with
    | Except.error e =>
      ok := (← assert label (e == expect)) && ok
    | Except.ok _ =>
      ok := (← assert s!"{label} should error" false) && ok

  match makeSpans data ctx0 Framebuffer.initBlack 0 0 0 mapping.xtoviewangle synthFlatSource
      8 60 70 60 70 with
  | Except.error e =>
    IO.eprintln s!"makeSpans spanstart failed: {e}"
    ok := false
  | Except.ok (ctxSpan, _) =>
    ok := (← assert "makeSpans spanstart unchanged when no emit"
      (ctxSpan.spanstart.getD 60 0 == 0)) && ok

  let ctxDraw := clearPlaneDraw ctxSlope 0 ((160 : Int32) <<< 16)
  let ctxDraw' := { ctxDraw with planeheight := 64 * FRACUNIT, planezlight := #[0] }

  let ctxSpan0 := { ctxDraw' with spanstart := arrSetI32 ctxDraw'.spanstart 70 3 }
  match makeSpans data ctxSpan0 Framebuffer.initBlack 0 0 0 mapping.xtoviewangle synthFlatSource
      8 70 80 71 80 with
  | Except.error e =>
    IO.eprintln s!"makeSpans sweep failed: {e}"
    ok := false
  | Except.ok (_, fbSpan) =>
    ok := (← assert "makeSpans paints row" (fbSpan.get 3 70 != 0)) && ok

  let ctxSpan2 := { ctxDraw' with spanstart := arrSetI32 ctxDraw'.spanstart 60 3 }
  match makeSpans data ctxSpan2 Framebuffer.initBlack 0 0 0 mapping.xtoviewangle synthFlatSource
      8 90 100 60 70 with
  | Except.error e =>
    IO.eprintln s!"makeSpans spanstart2 failed: {e}"
    ok := false
  | Except.ok (ctxSpan3, _) =>
    ok := (← assert "makeSpans spanstart new row" (ctxSpan3.spanstart.getD 60 0 == 8)) && ok

  let bottom := arrSetU8 (arrSetU8 emptyVisPlaneColumn 0 (UInt8.ofNat 55)) 1 (UInt8.ofNat 55)
  let top := arrSetU8 emptyVisPlaneColumn 1 (UInt8.ofNat 50)
  let vp : VisPlane := {
    height := 64 * FRACUNIT, picnum := 1, lightlevel := 128,
    minx := 1, maxx := 1,
    top := top, bottom := bottom
  }
  match drawVisPlaneMarks data ctxSlope Framebuffer.initBlack 0 0 0 0 mapping.xtoviewangle vp synthFlatSource with
  | Except.error e =>
    IO.eprintln s!"drawVisPlaneMarks failed: {e}"
    ok := false
  | Except.ok (_, fbVis) =>
    ok := (← assert "drawVisPlaneMarks ceiling pixel" (fbVis.get 1 50 == 241)) && ok
    ok := (← assert "drawVisPlaneMarks floor pixel" (fbVis.get 1 55 == 167)) && ok

  let view : RenderViewCtx := {
    viewx := 0, viewy := 0, viewangle := 0, mapping := mapping
  }
  let planesMarked : VisPlaneState := { visplanes := #[vp], lastvisplane := 1 }
  match drawPlanes data synthFlatWad Framebuffer.initBlack planesMarked view 0 with
  | Except.error e =>
    IO.eprintln s!"drawPlanes golden failed: {e}"
    ok := false
  | Except.ok fbPlanes =>
    ok := (← assert "drawPlanes ceiling pixel" (fbPlanes.get 1 50 == 241)) && ok
    ok := (← assert "drawPlanes floor pixel" (fbPlanes.get 1 55 == 167)) && ok

  let black := Framebuffer.initBlack
  match drawPlanes data synthFlatWad black initVisPlaneState view 0 with
  | Except.error e =>
    IO.eprintln s!"drawPlanes empty failed: {e}"
    ok := false
  | Except.ok fbEmpty =>
    ok := (← assert "drawPlanes empty identity" (fbEmpty.pixels == black.pixels)) && ok

  let dataSky := synthRenderData identityColormaps
  let dataSky := {
    dataSky with
      textureTables := {
        dataSky.textureTables with
          skyFlatNum := 1
          skytexture := 0
          numTextures := 1
          columnLump := #[#[(1 : Int32)]]
          columnOfs := #[#[0]]
          widthMask := #[0]
          height := #[128]
          compositeSize := #[0]
          composite := #[none]
      }
  }
  let planesSky : VisPlaneState := { visplanes := #[vp], lastvisplane := 1 }
  match drawPlanes dataSky synthFlatWad black planesSky view 0 with
  | Except.error e =>
    IO.eprintln s!"drawPlanes sky failed: {e}"
    ok := false
  | Except.ok fbSky =>
    ok := (← assert "drawPlanes sky y50" (fbSky.get 1 50 == 66)) && ok
    ok := (← assert "drawPlanes sky y51" (fbSky.get 1 51 == 67)) && ok
    ok := (← assert "drawPlanes sky y52" (fbSky.get 1 52 == 68)) && ok
    ok := (← assert "drawPlanes sky y53" (fbSky.get 1 53 == 69)) && ok
    ok := (← assert "drawPlanes sky y54" (fbSky.get 1 54 == 70)) && ok
    ok := (← assert "drawPlanes sky y55" (fbSky.get 1 55 == 71)) && ok

  let dataSkyAng := {
    dataSky with
      textureTables := {
        dataSky.textureTables with
          columnLump := #[#[(1 : Int32), (1 : Int32)]]
          columnOfs := #[#[0, 64]]
          widthMask := #[1]
      }
  }
  let mappingAng := {
    mapping with
      xtoviewangle := arrSet mapping.xtoviewangle 1 ((1 : UInt32) <<< (22 : UInt32))
  }
  let viewAng : RenderViewCtx := { view with mapping := mappingAng }
  match drawPlanes dataSkyAng synthFlatWad black planesSky viewAng 0 with
  | Except.error e =>
    IO.eprintln s!"drawPlanes sky angle failed: {e}"
    ok := false
  | Except.ok fbSkyAng =>
    ok := (← assert "drawPlanes sky angle column" (fbSkyAng.get 1 50 == 130)) && ok

  let topSkip := arrSetU8 emptyVisPlaneColumn 1 (UInt8.ofNat 55)
  let bottomSkip := arrSetU8 emptyVisPlaneColumn 1 (UInt8.ofNat 50)
  let vpSkySkip : VisPlane := { vp with top := topSkip, bottom := bottomSkip }
  let planesSkySkip : VisPlaneState := { visplanes := #[vpSkySkip], lastvisplane := 1 }
  match drawPlanes dataSky synthFlatWad black planesSkySkip view 0 with
  | Except.error e =>
    IO.eprintln s!"drawPlanes sky top>bottom failed: {e}"
    ok := false
  | Except.ok fbSkySkip =>
    ok := (← assert "drawPlanes sky top>bottom black" (fbSkySkip.pixels == black.pixels)) && ok

  let vpEmptySpan := { vp with minx := 10, maxx := 5 }
  let planesEmptySpan : VisPlaneState := { visplanes := #[vpEmptySpan], lastvisplane := 1 }
  match drawPlanes data synthFlatWad black planesEmptySpan view 0 with
  | Except.error e =>
    IO.eprintln s!"drawPlanes minx>maxx failed: {e}"
    ok := false
  | Except.ok fbSkip =>
    ok := (← assert "drawPlanes minx>maxx identity" (fbSkip.pixels == black.pixels)) && ok

  let planesStale : VisPlaneState := { visplanes := #[vp], lastvisplane := 0 }
  match drawPlanes data synthFlatWad black planesStale view 0 with
  | Except.error e =>
    IO.eprintln s!"drawPlanes lastvisplane bound failed: {e}"
    ok := false
  | Except.ok fbStale =>
    ok := (← assert "drawPlanes lastvisplane bound" (fbStale.pixels == black.pixels)) && ok

  let dataOff := {
    data with
      textureTables := { data.textureTables with firstFlat := 2 }
  }
  let wadOff : WadDirectory :=
    let size := UInt32.ofNat synthFlatSource.size
    {
      identification := ByteArray.mk #[73, 87, 65, 68], numlumps := 4, infotableofs := 0,
      entries := #[
        { filepos := 0, size := 0, name := nameTo8 "D0" },
        { filepos := 0, size := 0, name := nameTo8 "D1" },
        { filepos := 0, size := 0, name := nameTo8 "D2" },
        { filepos := 0, size := size, name := nameTo8 "FLAT1" }
      ],
      data := synthFlatSource
    }
  match drawPlanes dataOff wadOff Framebuffer.initBlack planesMarked view 0 with
  | Except.error e =>
    IO.eprintln s!"drawPlanes firstFlat offset failed: {e}"
    ok := false
  | Except.ok fbOff =>
    ok := (← assert "drawPlanes firstFlat ceiling" (fbOff.get 1 50 == 241)) && ok
    ok := (← assert "drawPlanes firstFlat floor" (fbOff.get 1 55 == 167)) && ok

  let vpSkipOob := { vp with minx := 10, maxx := 5, picnum := 5 }
  let planesSkipOob : VisPlaneState := { visplanes := #[vpSkipOob], lastvisplane := 1 }
  match drawPlanes data synthFlatWad black planesSkipOob view 0 with
  | Except.error e =>
    ok := (← assert "drawPlanes minx>maxx skips OOB" false) && ok
    IO.eprintln s!"drawPlanes minx>maxx OOB: {e}"
  | Except.ok fbSkipOob =>
    ok := (← assert "drawPlanes minx>maxx skips OOB" (fbSkipOob.pixels == black.pixels)) && ok

  let vpOob := { vp with picnum := 5 }
  let planesOob : VisPlaneState := { visplanes := #[vpOob], lastvisplane := 1 }
  match drawPlanes data synthFlatWad black planesOob view 0 with
  | Except.error e =>
    ok := (← assert "drawPlanes OOB lumpData"
      (e == "lump index 5 out of range (2)")) && ok
  | Except.ok _ =>
    ok := (← assert "drawPlanes OOB should error" false) && ok

  -- R1aa: minx=0 must not clobber top[0]; pads open spans at x=0.
  let vpFresh0 := freshVisPlane (64 * FRACUNIT) 1 128
  let vpMinx0 : VisPlane := {
    vpFresh0 with
      minx := 0, maxx := 0,
      top := arrSetU8 vpFresh0.top 0 (UInt8.ofNat 50),
      bottom := arrSetU8 vpFresh0.bottom 0 (UInt8.ofNat 55)
  }
  ok := (← assert "minx=0 leaves top[0] marked"
    (vpMinx0.top.getD 0 (UInt8.ofNat 0) == UInt8.ofNat 50)) && ok
  match drawVisPlaneMarks data ctxSlope Framebuffer.initBlack 0 0 0 0 mapping.xtoviewangle
      vpMinx0 synthFlatSource with
  | Except.error e =>
    IO.eprintln s!"minx=0 drawVisPlaneMarks failed: {e}"
    ok := false
  | Except.ok (_, fbMinx0) =>
    ok := (← assert "minx=0 does not clobber top[0]"
      (vpMinx0.top.getD 0 (UInt8.ofNat 0) == UInt8.ofNat 50)) && ok
    ok := (← assert "minx=0 paints column 0 ceiling" (fbMinx0.get 0 50 != 0)) && ok
    ok := (← assert "minx=0 paints column 0 floor" (fbMinx0.get 0 55 != 0)) && ok
    ok := (← assert "minx=0 does not paint column 1" (fbMinx0.get 1 50 == 0)) && ok

  let planesMinx0 : VisPlaneState := { visplanes := #[vpMinx0], lastvisplane := 1 }
  match drawPlanes data synthFlatWad Framebuffer.initBlack planesMinx0 view 0 with
  | Except.error e =>
    IO.eprintln s!"drawPlanes minx=0 failed: {e}"
    ok := false
  | Except.ok fbDraw0 =>
    ok := (← assert "drawPlanes minx=0 paints column 0" (fbDraw0.get 0 50 != 0)) && ok

  -- maxx+1 pad (top 0xff / bottom 0) must close the last column without RANGECHECK.
  -- Unmarked in-range neighbors are BSS 0 (`freshVisPlane`), not sentinel 0xff.
  let vpFresh := freshVisPlane (64 * FRACUNIT) 1 128
  ok := (← assert "freshVisPlane top pad sentinel"
    (vpFresh.top.getD 0 (UInt8.ofNat 0) == UInt8.ofNat 0xff)) && ok
  ok := (← assert "freshVisPlane bottom BSS 0"
    (vpFresh.bottom.getD 0 (UInt8.ofNat 0xff) == (0 : UInt8))) && ok
  let vpMaxx319 : VisPlane := {
    vpFresh with
      minx := 319, maxx := 319,
      top := arrSetU8 vpFresh.top 319 (UInt8.ofNat 50),
      bottom := arrSetU8 vpFresh.bottom 319 (UInt8.ofNat 55)
  }
  match drawVisPlaneMarks data ctxSlope Framebuffer.initBlack 0 0 0 0 mapping.xtoviewangle
      vpMaxx319 synthFlatSource with
  | Except.error e =>
    IO.eprintln s!"maxx=319 drawVisPlaneMarks failed: {e}"
    ok := false
  | Except.ok (_, fb319) =>
    ok := (← assert "maxx=319 paints column 319" (fb319.get 319 50 != 0)) && ok

  let vpMid : VisPlane := {
    vpFresh with
      minx := 10, maxx := 10,
      top := arrSetU8 vpFresh.top 10 (UInt8.ofNat 50),
      bottom := arrSetU8 vpFresh.bottom 10 (UInt8.ofNat 55)
  }
  match drawVisPlaneMarks data ctxSlope Framebuffer.initBlack 0 0 0 0 mapping.xtoviewangle
      vpMid synthFlatSource with
  | Except.error e =>
    IO.eprintln s!"minx=10 drawVisPlaneMarks failed: {e}"
    ok := false
  | Except.ok (_, fbMid) =>
    ok := (← assert "minx=10 paints column 10" (fbMid.get 10 50 != 0)) && ok
    ok := (← assert "minx=10 does not paint column 9" (fbMid.get 9 50 == 0)) && ok

  match drawVisPlaneMarks dataLevel ctxSlope Framebuffer.initBlack 0 0 0 0 mapping.xtoviewangle
      vpMinx0 synthFlatSource 0,
        drawVisPlaneMarks dataLevel ctxSlope Framebuffer.initBlack 0 0 0 0 mapping.xtoviewangle
          vpMinx0 synthFlatSource 1 with
  | Except.error e, _ =>
    IO.eprintln s!"extralight 0 failed: {e}"
    ok := false
  | _, Except.error e =>
    IO.eprintln s!"extralight 1 failed: {e}"
    ok := false
  | Except.ok (ctxL0, fbL0), Except.ok (ctxL1, fbL1) =>
    ok := (← assert "extralight shifts planezlight" (ctxL0.planezlight != ctxL1.planezlight)) && ok
    ok := (← assert "extralight 0 planezlight nonempty" (ctxL0.planezlight.size == 128)) && ok
    ok := (← assert "extralight 1 planezlight nonempty" (ctxL1.planezlight.size == 128)) && ok
    ok := (← assert "extralight changes span pixels" (fbL0.pixels != fbL1.pixels)) && ok

  -- Empty `flattranslation` is identity; remap `picnum` 1 → 0 loads lump 0.
  let src0 := ByteArray.mk (Array.replicate 4096 (UInt8.ofNat 7))
  let wadRemap : WadDirectory :=
    let size0 := UInt32.ofNat src0.size
    let size1 := UInt32.ofNat synthFlatSource.size
    {
      identification := ByteArray.mk #[73, 87, 65, 68], numlumps := 2, infotableofs := 0,
      entries := #[
        { filepos := 0, size := size0, name := nameTo8 "FLAT0" },
        { filepos := size0, size := size1, name := nameTo8 "FLAT1" }
      ],
      data := src0 ++ synthFlatSource
    }
  match drawPlanes data wadRemap Framebuffer.initBlack planesMarked view 0 with
  | Except.error e =>
    IO.eprintln s!"drawPlanes identity default failed: {e}"
    ok := false
  | Except.ok fbId =>
    match drawPlanes data wadRemap Framebuffer.initBlack planesMarked view 0
        (flattranslation := #[(0 : Int32), 0]) with
    | Except.error e =>
      IO.eprintln s!"drawPlanes remap failed: {e}"
      ok := false
    | Except.ok fbRemap =>
      ok := (← assert "drawPlanes identity default still paints"
        (fbId.get 1 50 != 0)) && ok
      ok := (← assert "drawPlanes remap 1→0 differs from identity"
        (fbRemap.get 1 50 != fbId.get 1 50)) && ok
      ok := (← assert "drawPlanes remap loads lump 0"
        (fbRemap.get 1 50 == (7 : UInt8))) && ok

  if ok then
    IO.println "plane-makespans-test: all passed"
    pure 0
  else
    IO.eprintln "plane-makespans-test: failures"
    pure 1
