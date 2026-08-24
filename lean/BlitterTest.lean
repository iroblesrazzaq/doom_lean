import Doom.Playsim.Fixed
import Doom.Render.Data
import Doom.Render.Draw
import Doom.Render.Palette
import Doom.Render.Types

/-!
# BlitterTest

R1b-blitter unit tests: `R_DrawColumn` / `R_DrawSpan` goldens and rangechecks.
-/

open Doom.Playsim.Fixed
open Doom.Render.Data
open Doom.Render.Draw
open Doom.Render.Palette
open Doom.Render.Types

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def identityColormaps : ByteArray :=
  ByteArray.mk (Array.ofFn (n := 8192) (fun i => UInt8.ofNat (i % 256)))

private def synthRenderData : RenderData :=
  {
    playpal := ByteArray.empty
    colormaps := identityColormaps
    textureTables := {
      firstFlat := 0, numFlats := 0, skyFlatNum := 0, numTextures := 0,
      textures := #[], columnLump := #[], columnOfs := #[], widthMask := #[], height := #[]
      compositeSize := #[], composite := #[], hashHead := #[], hashNext := #[]
    }
    firstSprite := 0
    spriteMetrics := #[]
  }

private def synthColumnSource : ByteArray :=
  ByteArray.mk (Array.ofFn (n := 128) (fun i => UInt8.ofNat i))

private def synthFlatSource : ByteArray :=
  ByteArray.mk (Array.ofFn (n := 4096) (fun i => UInt8.ofNat (i % 256)))

private def fbPixel (fb : Framebuffer) (x y : Nat) : UInt8 :=
  fb.get x y

private def columnStrip (fb : Framebuffer) (x y0 y1 : Nat) : List UInt8 :=
  (List.range (y1 - y0 + 1)).map (fun d => fbPixel fb x (y0 + d))

private def rowStrip (fb : Framebuffer) (y x0 x1 : Nat) : List UInt8 :=
  (List.range (x1 - x0 + 1)).map (fun d => fbPixel fb (x0 + d) y)

private def goldenColumnStrip : List UInt8 :=
  [84, 85, 86, 87, 88].map UInt8.ofNat

private def goldenSpanRow : List UInt8 :=
  [0, 1, 2, 3, 4, 5].map UInt8.ofNat

def main (_args : List String) : IO UInt32 := do
  let data := synthRenderData
  let fb0 := Framebuffer.initBlack
  let mut ok := true

  let colParams : ColumnDrawParams := {
    x := 10, yl := 84, yh := 88,
    iscale := FRACUNIT,
    texturemid := Int32.ofNat (84 * 65536),
    source := synthColumnSource,
    colormapLevel := 0
  }
  match drawColumn data fb0 colParams with
  | Except.error e =>
    IO.eprintln s!"drawColumn golden failed: {e}"
    ok := false
  | Except.ok fbCol =>
    let strip := columnStrip fbCol 10 84 88
    ok := (← assert "column golden strip" (strip == goldenColumnStrip)) && ok

  let colZero : ColumnDrawParams := { colParams with yh := 84 }
  match drawColumn data fb0 colZero with
  | Except.error e =>
    IO.eprintln s!"drawColumn count=0 failed: {e}"
    ok := false
  | Except.ok fbOne =>
    ok := (← assert "count=0 paints one pixel" (fbPixel fbOne 10 84 == 84)) && ok
    ok := (← assert "count=0 leaves other pixels black"
      (fbPixel fbOne 10 85 == 0)) && ok

  let colNeg : ColumnDrawParams := { colParams with yl := 90, yh := 80 }
  match drawColumn data fb0 colNeg with
  | Except.error e =>
    IO.eprintln s!"drawColumn count<0 failed: {e}"
    ok := false
  | Except.ok fbNeg =>
    ok := (← assert "count<0 no-op" (fbNeg.pixels == fb0.pixels)) && ok

  for (label, params, expect) in [
    ("column x OOB", { colParams with x := 320 }, "R_DrawColumn: 84 to 88 at 320"),
    ("column yl<0", { colParams with yl := -1, yh := 5 }, "R_DrawColumn: -1 to 5 at 10"),
    ("column yh>=SCREENHEIGHT", { colParams with yh := 200 }, "R_DrawColumn: 84 to 200 at 10")
  ] do
    match drawColumn data fb0 params with
    | Except.error e =>
      ok := (← assert label (e == expect)) && ok
    | Except.ok _ =>
      ok := (← assert s!"{label} should error" false) && ok

  let spanParams : SpanDrawParams := {
    x1 := 20, x2 := 25, y := 100,
    xfrac := 0, yfrac := 0,
    xstep := FRACUNIT, ystep := 0,
    source := synthFlatSource,
    colormapLevel := 0
  }
  match drawSpan data fb0 spanParams with
  | Except.error e =>
    IO.eprintln s!"drawSpan golden failed: {e}"
    ok := false
  | Except.ok fbSpan =>
    let row := rowStrip fbSpan 100 20 25
    ok := (← assert "span golden row" (row == goldenSpanRow)) && ok

  let colOff : ColumnDrawParams := { colParams with viewwindowx := 16, viewwindowy := 12 }
  match drawColumn data fb0 colOff with
  | Except.error e =>
    IO.eprintln s!"drawColumn viewwindow failed: {e}"
    ok := false
  | Except.ok fbOff =>
    ok := (← assert "viewwindow stores at (x+16,y+12)"
      (fbPixel fbOff 26 96 == 84)) && ok
    ok := (← assert "viewwindow leaves view-local origin black"
      (fbPixel fbOff 10 84 == 0)) && ok

  for (label, params, expect) in [
    ("span x2<x1", { spanParams with x1 := 30, x2 := 20 }, "R_DrawSpan: 30 to 20 at 100"),
    ("span x1<0", { spanParams with x1 := -1, x2 := 5 }, "R_DrawSpan: -1 to 5 at 100"),
    ("span x2>=SCREENWIDTH", { spanParams with x2 := 320 }, "R_DrawSpan: 20 to 320 at 100"),
    ("span y>SCREENHEIGHT", { spanParams with y := 201 }, "R_DrawSpan: 20 to 25 at 201")
  ] do
    match drawSpan data fb0 params with
    | Except.error e =>
      ok := (← assert label (e == expect)) && ok
    | Except.ok _ =>
      ok := (← assert s!"{label} should error" false) && ok

  match colormapSlice identityColormaps 32 with
  | Except.error _ => ok := (← assert "colormapSlice rejects level>=32" true) && ok
  | Except.ok _ => ok := (← assert "colormapSlice rejects level>=32" false) && ok

  match colormapSlice identityColormaps 0 with
  | Except.error e =>
    IO.eprintln s!"colormapSlice level 0 failed: {e}"
    ok := false
  | Except.ok slice =>
    ok := (← assert "colormapSlice level 0 size" (slice.size == 256)) && ok

  -- R_DrawFuzzColumn
  let mut fbFuzz := Framebuffer.initBlack
  fbFuzz := fbFuzz.set 10 83 40
  fbFuzz := fbFuzz.set 10 84 40
  fbFuzz := fbFuzz.set 10 85 40
  let fuzzParams : FuzzColumnDrawParams := {
    x := 10, yl := 84, yh := 84, viewheight := 168
  }
  match drawFuzzColumn data fbFuzz fuzzParams 0 with
  | Except.error e =>
    IO.eprintln s!"drawFuzzColumn golden failed: {e}"
    ok := false
  | Except.ok (fbF, fp1) =>
    ok := (← assert "fuzz colormap 6 identity samples y+1" (fbPixel fbF 10 84 == 40)) && ok
    ok := (← assert "fuzzpos advances" (fp1 == 1)) && ok
  match drawFuzzColumn data fbFuzz fuzzParams 1 with
  | Except.error e =>
    IO.eprintln s!"drawFuzzColumn offset-1 failed: {e}"
    ok := false
  | Except.ok (fbF1, fp2) =>
    ok := (← assert "fuzzoffset[1] samples y-1" (fbPixel fbF1 10 84 == 40)) && ok
    ok := (← assert "fuzzpos 1→2" (fp2 == 2)) && ok
  match drawFuzzColumn data fbFuzz fuzzParams 49 with
  | Except.error e =>
    IO.eprintln s!"drawFuzzColumn wrap failed: {e}"
    ok := false
  | Except.ok (_, fp0) =>
    ok := (← assert "fuzzpos wraps at 50" (fp0 == 0)) && ok
  let fuzzEmpty : FuzzColumnDrawParams := { fuzzParams with yl := 0, yh := 0 }
  match drawFuzzColumn data fb0 fuzzEmpty 7 with
  | Except.error e =>
    IO.eprintln s!"drawFuzzColumn empty failed: {e}"
    ok := false
  | Except.ok (fbE, fpE) =>
    ok := (← assert "yl=yh=0 border clamp is no-op" (fbE.pixels == fb0.pixels)) && ok
    ok := (← assert "empty fuzz leaves fuzzpos" (fpE == 7)) && ok
  let fuzzOob : FuzzColumnDrawParams := { fuzzParams with x := 320 }
  match drawFuzzColumn data fb0 fuzzOob 0 with
  | Except.error e =>
    ok := (← assert "fuzz x OOB" (e == "R_DrawFuzzColumn: 84 to 84 at 320")) && ok
  | Except.ok _ =>
    ok := (← assert "fuzz x OOB should error" false) && ok
  ok := (← assert "fuzzoffset table size" (fuzzoffset.size == fuzzTableSize)) && ok
  ok := (← assert "fuzzoffset[0] is +SCREENWIDTH" (fuzzoffset.getD 0 0 == fuzzOff)) && ok
  ok := (← assert "fuzzoffset[1] is -SCREENWIDTH" (fuzzoffset.getD 1 0 == -fuzzOff)) && ok

  let cmap6 : ByteArray :=
    ByteArray.mk (Array.ofFn (n := 8192) fun i =>
      if i / 256 == 6 && i % 256 == 40 then (99 : UInt8) else UInt8.ofNat (i % 256))
  let data6 := { data with colormaps := cmap6 }
  match drawFuzzColumn data6 fbFuzz fuzzParams 0 with
  | Except.error e =>
    IO.eprintln s!"drawFuzzColumn cmap6 failed: {e}"
    ok := false
  | Except.ok (fbC6, _) =>
    ok := (← assert "fuzz uses colormap 6" (fbPixel fbC6 10 84 == 99)) && ok

  if ok then
    IO.println "blitter-test: all passed"
    pure 0
  else
    IO.eprintln "blitter-test: failures"
    pure 1
