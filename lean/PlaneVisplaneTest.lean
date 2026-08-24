import Doom.Playsim.Fixed
import Doom.Playsim.Level
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Draw
import Doom.Render.Gfx.Texture
import Doom.Render.Plane
import Doom.Render.Seg
import Doom.Render.Types
import Doom.Render.Util
import Doom.Render.View
import Doom.Wad

/-!
# PlaneVisplaneTest

R1o-plane-visplane-accum: `R_FindPlane`, `R_CheckPlane`, segloop marks, storeWallSolid visplanes.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.Level
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Draw
open Doom.Render.Gfx.Texture
open Doom.Render.Plane
open Doom.Render.Seg
open Doom.Render.Types
open Doom.Render.Util
open Doom.Render.View
open Doom.Wad

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def skyFlat : Nat := 5

private def fillVisPlanes (n : Nat) : Except String VisPlaneState := do
  let mut st := initVisPlaneState
  let mut i : Nat := 0
  while i < n do
    let (st', _) ← findPlane st (Int32.ofNat i) i (Int32.ofNat i) skyFlat
    st := st'
    i := i + 1
  pure st

private def mkClipArrays : Array Int32 × Array Int32 :=
  let ceil := Array.ofFn (n := 320) (fun _ => (-1 : Int32))
  let floor := Array.ofFn (n := 320) (fun _ => defaultViewheight)
  (ceil, floor)

private def markOnlySegState (vpSt : VisPlaneState) (ceilIdx floorIdx : Nat) : SegLoopState :=
  let (ceil, floor) := mkClipArrays
  {
    rwX := 10, rwStopX := 11,
    rwScale := FRACUNIT, rwScaleStep := 0,
    rwMidTextureMid := 0, rwCenterAngle := 0, rwDistance := 0, rwOffset := 0,
    midtexture := 0,
    walllights := #[], ceilingclip := ceil, floorclip := floor,
    markceiling := true, markfloor := true, segtextured := false,
    topfrac := Int32.ofNat (84 * 4096 - 4095),
    topstep := 0,
    bottomfrac := Int32.ofNat (88 * 4096),
    bottomstep := 0,
    xtoviewangle := Array.replicate 320 0,
    textureTables := {
      firstFlat := 0, numFlats := 0, skyFlatNum := skyFlat, numTextures := 0,
      textures := #[], columnLump := #[], columnOfs := #[], widthMask := #[], height := #[],
      compositeSize := #[], composite := #[], hashHead := #[], hashNext := #[]
    },
    visplanes := vpSt,
    floorplane := some floorIdx,
    ceilingplane := some ceilIdx
  }

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let st0 := initVisPlaneState

  -- findPlane: allocate first plane
  match findPlane st0 (64 * FRACUNIT) 3 128 skyFlat with
  | Except.error e =>
    IO.eprintln s!"findPlane alloc failed: {e}"
    ok := false
  | Except.ok (st1, idx0) =>
    ok := (← assert "findPlane first index" (idx0 == 0)) && ok
    ok := (← assert "findPlane count" (st1.lastvisplane == 1)) && ok
  -- findPlane: reuse matching plane
  match findPlane st0 (64 * FRACUNIT) 3 128 skyFlat, findPlane st0 (64 * FRACUNIT) 3 128 skyFlat with
  | Except.error e, _ =>
    IO.eprintln s!"findPlane reuse failed: {e}"
    ok := false
  | _, Except.error e =>
    IO.eprintln s!"findPlane reuse2 failed: {e}"
    ok := false
  | Except.ok (stA, idxA), Except.ok (stB, idxB) =>
    ok := (← assert "findPlane reuse same index" (idxA == idxB)) && ok
    ok := (← assert "findPlane reuse no growth" (stA.lastvisplane == stB.lastvisplane)) && ok
  -- findPlane: sky normalization merges distinct heights
  match findPlane st0 (100 * FRACUNIT) skyFlat 200 skyFlat,
        findPlane st0 (200 * FRACUNIT) skyFlat 50 skyFlat with
  | Except.error e, _ =>
    IO.eprintln s!"findPlane sky failed: {e}"
    ok := false
  | _, Except.error e =>
    IO.eprintln s!"findPlane sky2 failed: {e}"
    ok := false
  | Except.ok (_, idxS1), Except.ok (stS2, idxS2) =>
    ok := (← assert "findPlane sky reuse" (idxS1 == idxS2)) && ok
    match stS2.visplanes[idxS2]? with
    | none =>
      ok := (← assert "findPlane sky plane exists" false) && ok
    | some vp =>
      ok := (← assert "findPlane sky height zero" (vp.height == 0)) && ok
      ok := (← assert "findPlane sky light zero" (vp.lightlevel == 0)) && ok
  -- findPlane overflow
  match fillVisPlanes maxVisPlanes with
  | Except.error e =>
    IO.eprintln s!"fillVisPlanes failed: {e}"
    ok := false
  | Except.ok full =>
    match findPlane full (0 : Int32) 99 0 skyFlat with
    | Except.error e =>
      ok := (← assert "findPlane overflow" (e == "R_FindPlane: no more visplanes")) && ok
    | Except.ok _ =>
      ok := (← assert "findPlane overflow should error" false) && ok

  -- checkPlane: no overlap expands bounds
  match findPlane st0 (32 * FRACUNIT) 1 128 skyFlat with
  | Except.error e =>
    IO.eprintln s!"checkPlane setup failed: {e}"
    ok := false
  | Except.ok (stP, plIdx) =>
    match checkPlane stP plIdx 10 20 with
    | Except.error e =>
      IO.eprintln s!"checkPlane no-overlap failed: {e}"
      ok := false
    | Except.ok (stC, idxC) =>
      ok := (← assert "checkPlane no-overlap same idx" (idxC == plIdx)) && ok
      match stC.visplanes[plIdx]? with
      | none => ok := false && ok
      | some vp =>
        ok := (← assert "checkPlane minx" (vp.minx == 10)) && ok
        ok := (← assert "checkPlane maxx" (vp.maxx == 20)) && ok

  -- checkPlane: overlap splits to new plane
  match findPlane st0 (48 * FRACUNIT) 2 100 skyFlat with
  | Except.error e =>
    IO.eprintln s!"checkPlane overlap setup failed: {e}"
    ok := false
  | Except.ok (stQ, pl0) =>
    let vp0 := stQ.visplanes[pl0]!
    let vpMarked := {
      vp0 with
        top := arrSetU8 vp0.top 15 (UInt8.ofNat 10)
        bottom := arrSetU8 vp0.bottom 15 (UInt8.ofNat 20)
        minx := 0
        maxx := 30
    }
    let stMarked := {
      stQ with
        visplanes := Doom.Render.Util.arrSet stQ.visplanes pl0 vpMarked
    }
    match checkPlane stMarked pl0 10 20 with
    | Except.error e =>
      IO.eprintln s!"checkPlane overlap failed: {e}"
      ok := false
    | Except.ok (stSplit, idxSplit) =>
      ok := (← assert "checkPlane overlap new idx" (idxSplit == stMarked.lastvisplane)) && ok
      match stSplit.visplanes[idxSplit]? with
      | none => ok := false && ok
      | some vpNew =>
        ok := (← assert "checkPlane split minx" (vpNew.minx == 10)) && ok
        ok := (← assert "checkPlane split maxx" (vpNew.maxx == 20)) && ok
        ok := (← assert "checkPlane split empty col"
          (vpNew.top.getD 10 (UInt8.ofNat 0xff) == UInt8.ofNat 0xff)) && ok

  -- checkPlane overflow
  match fillVisPlanes maxVisPlanes with
  | Except.error e =>
    IO.eprintln s!"fillVisPlanes2 failed: {e}"
    ok := false
  | Except.ok full2 =>
    let vp0 := full2.visplanes[0]!
    let vpMarked := {
      vp0 with
        top := arrSetU8 vp0.top 15 (UInt8.ofNat 10)
        minx := 0
        maxx := 30
    }
    let stFull := {
      full2 with
        visplanes := Doom.Render.Util.arrSet full2.visplanes 0 vpMarked
    }
    match checkPlane stFull 0 10 20 with
    | Except.error e =>
      ok := (← assert "checkPlane overflow" (e == "R_CheckPlane: no more visplanes")) && ok
    | Except.ok _ =>
      ok := (← assert "checkPlane overflow should error" false) && ok

  -- segloop mark-only column marks
  match findPlane st0 (128 * FRACUNIT) 10 128 skyFlat with
  | Except.error e =>
    IO.eprintln s!"segloop setup failed: {e}"
    ok := false
  | Except.ok (stCeil, ceilIdx) =>
    match findPlane stCeil 0 11 128 skyFlat with
    | Except.error e =>
      IO.eprintln s!"segloop setup2 failed: {e}"
      ok := false
    | Except.ok (stFloor, floorIdx) =>
      let segSt := markOnlySegState stFloor ceilIdx floorIdx
      let data : RenderData := {
        playpal := ByteArray.empty, colormaps := ByteArray.empty,
        textureTables := segSt.textureTables, firstSprite := 0, spriteMetrics := #[]
      }
      let wad : WadDirectory := {
        identification := ByteArray.empty, numlumps := 0, infotableofs := 0, entries := #[], data := ByteArray.empty
      }
      match renderSegLoop data wad segSt Framebuffer.initBlack with
      | Except.error e =>
        IO.eprintln s!"segloop mark-only failed: {e}"
        ok := false
      | Except.ok (stOut, _) =>
        match stOut.visplanes.visplanes[ceilIdx]?, stOut.visplanes.visplanes[floorIdx]? with
        | none, _ | _, none =>
          ok := (← assert "segloop planes exist" false) && ok
        | some cVp, some fVp =>
          ok := (← assert "segloop ceiling top"
            (cVp.top.getD 10 (UInt8.ofNat 0xff) == UInt8.ofNat 0)) && ok
          ok := (← assert "segloop ceiling bottom"
            (cVp.bottom.getD 10 (UInt8.ofNat 0xff) == UInt8.ofNat 83)) && ok
          ok := (← assert "segloop floor top"
            (fVp.top.getD 10 (UInt8.ofNat 0xff) == UInt8.ofNat 89)) && ok
          ok := (← assert "segloop floor bottom"
            (fVp.bottom.getD 10 (UInt8.ofNat 0xff) == UInt8.ofNat 167)) && ok

  -- storeWallSolid threads visplanes through checkPlane before renderSegLoop
  let mapping := initViewMapping 320 160
  let (ceilClip, floorClip) := mkClipArrays
  match findPlane st0 (128 * FRACUNIT) 10 128 skyFlat with
  | Except.error e =>
    IO.eprintln s!"storewall visplane setup failed: {e}"
    ok := false
  | Except.ok (stC, ceilIdx) =>
    match findPlane stC 0 11 128 skyFlat with
    | Except.error e =>
      IO.eprintln s!"storewall visplane setup2 failed: {e}"
      ok := false
    | Except.ok (stF, floorIdx) =>
      let flatCeiling := ByteArray.mk (Array.replicate 8 (UInt8.ofNat 65))
      let inp : StoreWallInput := {
        viewx := 0, viewy := 0, viewz := 20 * FRACUNIT, viewangle := 0,
        rwAngle1 := 0, segAngle := 0, segOffset := 0,
        v1x := 0, v1y := 0, v2x := 64 * FRACUNIT, v2y := (-256 * FRACUNIT),
        linedefFlags := 0, textureoffset := 0, rowoffset := 0,
        midtexture := 0, midtextureHeight := 128,
        ceilingheight := 128 * FRACUNIT, floorheight := 0,
        ceilingpic := flatCeiling, lightlevel := 128,
        start := 10, stop := 10, xtoviewangle := mapping.xtoviewangle,
        ceilingclip := ceilClip, floorclip := floorClip,
        visplanes := stF, floorplane := some floorIdx, ceilingplane := some ceilIdx
      }
      let data : RenderData := {
        playpal := ByteArray.empty, colormaps := ByteArray.empty,
        textureTables := {
          firstFlat := 0, numFlats := 0, skyFlatNum := skyFlat, numTextures := 0,
          textures := #[], columnLump := #[], columnOfs := #[], widthMask := #[], height := #[],
          compositeSize := #[], composite := #[], hashHead := #[], hashNext := #[]
        },
        firstSprite := 0, spriteMetrics := #[]
      }
      let wad : WadDirectory := {
        identification := ByteArray.empty, numlumps := 0, infotableofs := 0, entries := #[], data := ByteArray.empty
      }
      match storeWallSolid data wad inp Framebuffer.initBlack with
      | Except.error e =>
        IO.eprintln s!"storeWallSolid visplane failed: {e}"
        ok := false
      | Except.ok (res, _) =>
        match res.ceilingplane, res.floorplane with
        | some cIdx, some fIdx =>
          match res.visplanes.visplanes[cIdx]?, res.visplanes.visplanes[fIdx]? with
          | some cVp, some fVp =>
            ok := (← assert "storeWallSolid ceiling checkPlane minx" (cVp.minx == 10)) && ok
            ok := (← assert "storeWallSolid floor checkPlane minx" (fVp.minx == 10)) && ok
          | _, _ => ok := (← assert "storeWallSolid visplanes exist" false) && ok
        | _, _ => ok := (← assert "storeWallSolid plane refs set" false) && ok

  -- R1ab: two-sided drop window (front floor higher, different floorpic, no bottom
  -- texture). Near floor visplane owns [yh+1, floorclip-1]; floorclip becomes yh+1;
  -- a later far visplane on the same columns must not mark that band.
  let (ceilDrop, floorDrop) := mkClipArrays
  match findPlane st0 (32 * FRACUNIT) 10 128 skyFlat with
  | Except.error e =>
    IO.eprintln s!"drop-window near findPlane failed: {e}"
    ok := false
  | Except.ok (stNear, nearIdx) =>
    match findPlane stNear 0 20 128 skyFlat with
    | Except.error e =>
      IO.eprintln s!"drop-window far findPlane failed: {e}"
      ok := false
    | Except.ok (stBoth, farIdx) =>
      ok := (← assert "drop-window distinct visplanes" (nearIdx != farIdx)) && ok
      let nearLoop :=
        { markOnlySegState stBoth 0 nearIdx with
          ceilingclip := ceilDrop, floorclip := floorDrop
          markceiling := false, markfloor := true, segtextured := false
          midtexture := 0, toptexture := 0, bottomtexture := 0
          visplanes := stBoth, floorplane := some nearIdx, ceilingplane := none }
      let data : RenderData := {
        playpal := ByteArray.empty, colormaps := ByteArray.empty,
        textureTables := nearLoop.textureTables, firstSprite := 0, spriteMetrics := #[]
      }
      let wad : WadDirectory := {
        identification := ByteArray.empty, numlumps := 0, infotableofs := 0, entries := #[],
        data := ByteArray.empty
      }
      match renderSegLoop data wad nearLoop Framebuffer.initBlack with
      | Except.error e =>
        IO.eprintln s!"drop-window near segloop failed: {e}"
        ok := false
      | Except.ok (stNearOut, _) =>
        let yh : Int32 := 88
        let bandTop := yh + 1
        let bandBot : Int32 := 167
        ok := (← assert "drop-window floorclip becomes yh+1"
          (stNearOut.floorclip.getD 10 999 == bandTop)) && ok
        match stNearOut.visplanes.visplanes[nearIdx]? with
        | none =>
          ok := (← assert "drop-window near visplane exists" false) && ok
        | some nVp =>
          ok := (← assert "drop-window near owns band top"
            (nVp.top.getD 10 (UInt8.ofNat 0xff) == UInt8.ofNat 89)) && ok
          ok := (← assert "drop-window near owns band bottom"
            (nVp.bottom.getD 10 (UInt8.ofNat 0xff) == UInt8.ofNat 167)) && ok
        let farLoop :=
          { nearLoop with
            visplanes := stNearOut.visplanes
            floorplane := some farIdx
            floorclip := stNearOut.floorclip
            -- Distant courtyard floor: yh above the closed near band.
            bottomfrac := Int32.ofNat (50 * 4096)
            topfrac := Int32.ofNat (20 * 4096 - 4095) }
        match renderSegLoop data wad farLoop Framebuffer.initBlack with
        | Except.error e =>
          IO.eprintln s!"drop-window far segloop failed: {e}"
          ok := false
        | Except.ok (stFarOut, _) =>
          match stFarOut.visplanes.visplanes[farIdx]?,
                stFarOut.visplanes.visplanes[nearIdx]? with
          | none, _ | _, none =>
            ok := (← assert "drop-window far visplanes exist" false) && ok
          | some fVp, some nVp =>
            let farTop := fVp.top.getD 10 (UInt8.ofNat 0xff)
            let farBot := fVp.bottom.getD 10 (UInt8.ofNat 0)
            let farMissesBand :=
              farTop == UInt8.ofNat 0xff ||
                Int32.ofNat farBot.toNat < bandTop ||
                Int32.ofNat farTop.toNat > bandBot
            ok := (← assert "drop-window far does not own near band" farMissesBand) && ok
            ok := (← assert "drop-window near band preserved"
              (nVp.top.getD 10 (UInt8.ofNat 0xff) == UInt8.ofNat 89 &&
                nVp.bottom.getD 10 (UInt8.ofNat 0xff) == UInt8.ofNat 167)) && ok
            ok := (← assert "drop-window far may mark above closed clip"
              (farTop == UInt8.ofNat 51 && farBot == UInt8.ofNat 88)) && ok

      let mapping := initViewMapping 320 160
      let (ceilSw, floorSw) := mkClipArrays
      match findPlane initVisPlaneState (32 * FRACUNIT) 10 128 skyFlat with
      | Except.error e =>
        IO.eprintln s!"drop-window store findPlane failed: {e}"
        ok := false
      | Except.ok (stSw, nearSw) =>
        let dropInp : StoreWallInput := {
          viewx := 0, viewy := 0, viewz := 41 * FRACUNIT, viewangle := 0,
          rwAngle1 := 0, segAngle := 0, segOffset := 0,
          v1x := 128 * FRACUNIT, v1y := 64 * FRACUNIT,
          v2x := 128 * FRACUNIT, v2y := (-64 * FRACUNIT),
          linedefFlags := 0, textureoffset := 0, rowoffset := 0,
          midtexture := 0, midtextureHeight := 0,
          ceilingheight := 128 * FRACUNIT, floorheight := 32 * FRACUNIT,
          ceilingpic := ByteArray.mk (Array.replicate 8 (UInt8.ofNat 65)),
          floorpic := ByteArray.mk (Array.replicate 8 (UInt8.ofNat 10)),
          lightlevel := 128,
          backFloorheight := 0, backCeilingheight := 128 * FRACUNIT,
          backFloorpic := ByteArray.mk (Array.replicate 8 (UInt8.ofNat 20)),
          backCeilingpic := ByteArray.mk (Array.replicate 8 (UInt8.ofNat 65)),
          backLightlevel := 128,
          toptexture := 0, bottomtexture := 0,
          toptextureHeight := 0, bottomtextureHeight := 0,
          start := 160, stop := 160, xtoviewangle := mapping.xtoviewangle,
          ceilingclip := ceilSw, floorclip := floorSw,
          visplanes := stSw, floorplane := some nearSw, ceilingplane := none
        }
        match storeWallTwoSided data wad dropInp Framebuffer.initBlack with
        | Except.error e =>
          IO.eprintln s!"drop-window storeWallTwoSided failed: {e}"
          ok := false
        | Except.ok (resDrop, _) =>
          ok := (← assert "drop-window store markfloor" resDrop.markfloor) && ok
          ok := (← assert "drop-window store no bottom texture"
            (resDrop.bottomtexture == 0)) && ok
          let yhSw := resDrop.bottomfrac >>> 12
          let yhClamped :=
            if yhSw >= defaultViewheight then defaultViewheight - 1 else yhSw
          ok := (← assert "drop-window store floorclip yh+1"
            (resDrop.floorclip.getD 160 999 == yhClamped + 1)) && ok
          ok := (← assert "drop-window store band nonempty"
            (yhClamped + 1 <= (167 : Int32))) && ok
          match resDrop.floorplane with
          | none =>
            ok := (← assert "drop-window store floorplane set" false) && ok
          | some plIdx =>
            match resDrop.visplanes.visplanes[plIdx]? with
            | none =>
              ok := (← assert "drop-window store visplane exists" false) && ok
            | some vp =>
              let topM := vp.top.getD 160 (UInt8.ofNat 0xff)
              let botM := vp.bottom.getD 160 (UInt8.ofNat 0)
              ok := (← assert "drop-window store near owns [yh+1, floorclip-1]"
                (topM == UInt8.ofNat (i32ToNat (yhClamped + 1)) &&
                  botM == UInt8.ofNat 167)) && ok

  if ok then
    IO.println "plane-visplane-test: all passed"
    pure 0
  else
    IO.eprintln "plane-visplane-test: failures"
    pure 1
