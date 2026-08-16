import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Level
import Doom.Render.Bsp
import Doom.Render.Clip
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Gfx.Texture
import Doom.Render.Plane
import Doom.Render.Seg
import Doom.Render.Types
import Doom.Render.View
import Doom.Wad

/-!
# BspStorewallTwosidedTest

R1m-bsp-storewall-twosided: BSP two-sided pass walls draw via `storeWallTwoSided`.
-/

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Level
open Doom.Render.Bsp
open Doom.Render.Clip
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Gfx.Texture
open Doom.Render.Plane
open Doom.Render.Seg
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

private def F : Int32 := FRACUNIT

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

private def flatCeiling : ByteArray := ByteArray.mk (Array.replicate 8 (UInt8.ofNat 65))

private def wallTex : ByteArray := nameTo8 "WALL"
private def noTex : ByteArray := nameTo8 "-"

private def pic (b : UInt8) : ByteArray := ByteArray.mk #[b, 0, 0, 0, 0, 0, 0, 0]

private def mkSector (floor ceiling light : Int32) (fp cp : ByteArray) : Sector := {
  floorheight := floor, ceilingheight := ceiling
  floorpic := fp, ceilingpic := cp, lightlevel := light
  special := 0, tag := 0, lines := #[], blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def mkSide (sec : UInt32) (top bot mid : ByteArray) : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := top, bottomtexture := bot, midtexture := mid, sector := sec
}

private def mkLine (v1 v2 sidenum1 flags front back : Int32) : Line := {
  v1 := v1.toUInt32, v2 := v2.toUInt32, flags, special := 0, tag := 0
  sidenum0 := 0, sidenum1, dx := 0, dy := F
  slopetype := ST_VERTICAL, bbox := #[F, 0, 0, 0]
  frontsector := front, backsector := back
}

private def mkSeg (v1 v2 linedef side : Nat) (front back : Int32) : Seg := {
  v1 := v1.toUInt32, v2 := v2.toUInt32, angle := 0, offset := 0
  linedef := linedef.toUInt32, side := side.toUInt32
  frontsector := front, backsector := back
}

private def mkLevel (verts : Array Vertex) (sectors : Array Sector) (sides : Array Side)
    (lines : Array Line) (segs : Array Seg) (subsectors : Array Subsector)
    (nodes : Array Node) : LevelData := {
  vertexes := verts, sectors, sides, lines, segs, subsectors, nodes, things := #[]
  blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
  reject := ByteArray.empty
}

private def viewCtx : RenderViewCtx := {
  viewx := 0, viewy := 0, viewangle := 0
  mapping := initViewMapping 320 160
}

private def fresh : ClipState := clearClipSegs emptyClipState

private def viewz : Int32 := 41 * FRACUNIT

private def wideVerts : Array Vertex :=
  #[{ x := 0, y := 0 }, { x := -10 * F, y := F }, { x := 10 * F, y := F }]

private def wideSeg : Seg := mkSeg 1 2 0 0 0 1

/-- Window: back ceiling lower; upper gap + masked midtexture. -/
private def windowLevel : LevelData :=
  let sFront := mkSector 0 (128 * F) 128 (pic 1) flatCeiling
  let sBack := mkSector 0 (96 * F) 128 (pic 1) flatCeiling
  let sd := mkSide 0 wallTex noTex wallTex
  let l := mkLine 1 2 1 ML_TWOSIDED 0 1
  mkLevel wideVerts #[sFront, sBack] #[sd, mkSide 1 noTex noTex noTex] #[l]
    #[wideSeg]
    #[{ numsegs := 1, firstseg := 0, sector := 0 }] #[]

/-- Step: front floor higher than back; bottom tier + markfloor. -/
private def stepLevel : LevelData :=
  let sFront := mkSector (32 * F) (128 * F) 128 (pic 1) flatCeiling
  let sBack := mkSector 0 (128 * F) 128 (pic 1) flatCeiling
  let sd := mkSide 0 noTex wallTex noTex
  let l := mkLine 1 2 1 ML_TWOSIDED 0 1
  mkLevel wideVerts #[sFront, sBack] #[sd, mkSide 1 noTex noTex noTex] #[l]
    #[wideSeg]
    #[{ numsegs := 1, firstseg := 0, sector := 0 }] #[]

/-- Wall in front of the view (east, x=128) so yh is above the screen bottom. -/
private def dropVerts : Array Vertex :=
  #[{ x := 0, y := 0 }, { x := 128 * F, y := 64 * F }, { x := 128 * F, y := -64 * F }]

private def dropSeg : Seg := mkSeg 1 2 0 0 0 1

/-- Drop window: front floor higher, different floorpic, no bottom texture. -/
private def dropLevel : LevelData :=
  let sFront := mkSector (32 * F) (128 * F) 128 (pic 1) flatCeiling
  let sBack := mkSector 0 (128 * F) 128 (pic 2) flatCeiling
  let sd := mkSide 0 noTex noTex noTex
  let l := mkLine 1 2 1 ML_TWOSIDED 0 1
  mkLevel dropVerts #[sFront, sBack] #[sd, mkSide 1 noTex noTex noTex] #[l]
    #[dropSeg]
    #[{ numsegs := 1, firstseg := 0, sector := 0 }] #[]

/-- Riser: back floor higher than front; lower texture paints and floorclip = mid. -/
private def riserLevel : LevelData :=
  let sFront := mkSector 0 (128 * F) 128 (pic 1) flatCeiling
  let sBack := mkSector (32 * F) (128 * F) 128 (pic 2) flatCeiling
  let sd := mkSide 0 noTex wallTex noTex
  let l := mkLine 1 2 1 ML_TWOSIDED 0 1
  mkLevel dropVerts #[sFront, sBack] #[sd, mkSide 1 noTex noTex noTex] #[l]
    #[dropSeg]
    #[{ numsegs := 1, firstseg := 0, sector := 0 }] #[]

private def lineMeta (rw : UInt32) : AddLineMeta := {
  rw_angle1 := rw, linedef := 0, frontsector := 0, backsector := 1, contributed := true
}

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let ctx := viewCtx
  let st0 := fresh
  let data := synthRenderData
  let wad := synthWad
  let fb0 := Framebuffer.initBlack

  -- window golden storeWall fields via buildTwoSidedStoreWallInput (column 10)
  match addLine ctx viewz st0 wideSeg windowLevel none with
  | Except.error e =>
    IO.eprintln s!"window meta setup: {e}"; ok := false
  | Except.ok (_, m, _) =>
    let ds0 := { initBspDrawState data wad fb0 with frontsectorIdx := wideSeg.frontsector }
    match buildTwoSidedStoreWallInput ctx viewz wideSeg windowLevel (lineMeta m.rw_angle1) 10 10 ds0 with
    | Except.error e =>
      IO.eprintln s!"window inp setup: {e}"; ok := false
    | Except.ok inp =>
      match storeWallTwoSided data wad inp fb0 with
      | Except.error e =>
        IO.eprintln s!"window store setup: {e}"; ok := false
      | Except.ok (resW, _) =>
        ok := (← assert "window markceiling" resW.markceiling) && ok
        ok := (← assert "window toptexture" (resW.toptexture == 1)) && ok
        ok := (← assert "window maskedtexture" resW.maskedtexture) && ok
        ok := (← assert "window maskedtexturecol size" (resW.maskedtexturecol.size == 1)) && ok
        ok := (← assert "window drawSegIdx" (resW.drawSegIdx == 1)) && ok

  -- step golden storeWall fields via buildTwoSidedStoreWallInput (column 10)
  match addLine ctx viewz st0 wideSeg stepLevel none with
  | Except.error e =>
    IO.eprintln s!"step meta setup: {e}"; ok := false
  | Except.ok (_, m, _) =>
    let ds0 := { initBspDrawState data wad fb0 with frontsectorIdx := wideSeg.frontsector }
    match buildTwoSidedStoreWallInput ctx viewz wideSeg stepLevel (lineMeta m.rw_angle1) 10 10 ds0 with
    | Except.error e =>
      IO.eprintln s!"step inp setup: {e}"; ok := false
    | Except.ok inp =>
      match storeWallTwoSided data wad inp fb0 with
      | Except.error e =>
        IO.eprintln s!"step store setup: {e}"; ok := false
      | Except.ok (resS, _) =>
        ok := (← assert "step markfloor" resS.markfloor) && ok
        ok := (← assert "step SIL_BOTTOM" ((resS.silhouette &&& silBottom) != 0)) && ok
        ok := (← assert "step bsilheight" (resS.bsilheight == 32 * F)) && ok
        ok := (← assert "step drawSegIdx" (resS.drawSegIdx == 1)) && ok

  -- window subsector integration
  let dsWin := initBspDrawState data wad fb0
  match subsector viewz ctx st0 {} 0 windowLevel (some dsWin) with
  | Except.error e =>
    IO.eprintln s!"window draw setup: {e}"; ok := false
  | Except.ok (st, _, draw) =>
    match draw with
    | none =>
      IO.eprintln "window draw: missing draw state"; ok := false
    | some ds =>
      ok := (← assert "window subsector drawSegIdx" (ds.drawSegIdx == 1)) && ok
      ok := (← assert "window record-only walls empty" (st.recordedWalls.isEmpty)) && ok
      ok := (← assert "window solidsegs untouched"
        ((st.solidsegs.getD 0 ⟨0, 0⟩).last == (-1 : Int32))) && ok
      ok := (← assert "window floorclip unchanged"
        (ds.floorclip.getD 10 200 == defaultViewheight)) && ok

  -- step subsector integration
  let dsStep := initBspDrawState data wad fb0
  match subsector viewz ctx st0 {} 0 stepLevel (some dsStep) with
  | Except.error e =>
    IO.eprintln s!"step draw setup: {e}"; ok := false
  | Except.ok (st, _, draw) =>
    match draw with
    | none =>
      IO.eprintln "step draw: missing draw state"; ok := false
    | some ds =>
      ok := (← assert "step subsector drawSegIdx" (ds.drawSegIdx == 1)) && ok
      ok := (← assert "step record-only walls empty" (st.recordedWalls.isEmpty)) && ok
      ok := (← assert "step floorclip default at column 10"
        (ds.floorclip.getD 10 200 == defaultViewheight)) && ok

  -- pass clip with solid occluder: two fragments, no new records
  let stSolid :=
    match clipSolidWallSegment fresh 50 100 with
    | Except.ok s => s
    | Except.error _ => fresh
  let dsClip := initBspDrawState data wad fb0
  match addLine ctx viewz stSolid wideSeg windowLevel (some dsClip) with
  | Except.error e =>
    IO.eprintln s!"clip pass setup: {e}"; ok := false
  | Except.ok (stClip, m, draw) =>
    ok := (← assert "clip pass contributed" m.contributed) && ok
    ok := (← assert "clip pass no new records"
      (stClip.recordedWalls == stSolid.recordedWalls)) && ok
    match draw with
    | none => ok := false
    | some ds =>
      ok := (← assert "clip pass drawSegIdx" (ds.drawSegIdx == 2)) && ok
      ok := (← assert "clip pass floorclip unchanged at occluded"
        (ds.floorclip.getD 60 200 == defaultViewheight)) && ok

  -- drop window: clippass threads floorclip; far visplane must not own the near band
  let dsDrop := initBspDrawState data wad fb0
  match subsector viewz ctx st0 {} 0 dropLevel (some dsDrop) with
  | Except.error e =>
    IO.eprintln s!"drop draw setup: {e}"; ok := false
  | Except.ok (_, _, draw) =>
    match draw with
    | none =>
      IO.eprintln "drop draw: missing draw state"; ok := false
    | some ds =>
      match ds.drawSegs[0]? with
      | none =>
        ok := (← assert "drop drawseg recorded" false) && ok
      | some rec =>
        ok := (← assert "drop markfloor" rec.storeRes.markfloor) && ok
        ok := (← assert "drop no bottom texture" (rec.storeRes.bottomtexture == 0)) && ok
        let x1 := rec.replayMeta.x1
        let xNat := (x1.toInt).toNat
        let yh0 := rec.storeRes.bottomfrac >>> 12
        let yhClamped :=
          if yh0 >= defaultViewheight then defaultViewheight - 1 else yh0
        ok := (← assert "drop floorclip threaded yh+1"
          (ds.floorclip.getD xNat 200 == yhClamped + 1)) && ok
        match ds.floorplane with
        | none =>
          ok := (← assert "drop near floorplane set" false) && ok
        | some nearIdx =>
          match findPlane ds.planes 0 20 128 0 with
          | Except.error e =>
            IO.eprintln s!"drop far findPlane: {e}"; ok := false
          | Except.ok (stFar, farIdx) =>
            ok := (← assert "drop far visplane distinct" (farIdx != nearIdx)) && ok
            let farInp : StoreWallInput := {
              viewx := ctx.viewx, viewy := ctx.viewy, viewz := viewz, viewangle := ctx.viewangle,
              rwAngle1 := 0, segAngle := 0, segOffset := 0,
              v1x := rec.replayMeta.v1x, v1y := rec.replayMeta.v1y,
              v2x := rec.replayMeta.v2x, v2y := rec.replayMeta.v2y,
              linedefFlags := 0, textureoffset := 0, rowoffset := 0,
              midtexture := 0, midtextureHeight := 0,
              ceilingheight := 128 * F, floorheight := 0,
              ceilingpic := flatCeiling, floorpic := pic 2, lightlevel := 128,
              backFloorheight := 32 * F, backCeilingheight := 128 * F,
              backFloorpic := pic 1, backCeilingpic := flatCeiling, backLightlevel := 128,
              toptexture := 0, bottomtexture := 0,
              start := rec.replayMeta.x1, stop := rec.replayMeta.x1,
              xtoviewangle := ctx.mapping.xtoviewangle,
              ceilingclip := ds.ceilingclip, floorclip := ds.floorclip,
              visplanes := stFar, floorplane := some farIdx, ceilingplane := none
            }
            match storeWallTwoSided data wad farInp fb0 with
            | Except.error e =>
              IO.eprintln s!"drop far store: {e}"; ok := false
            | Except.ok (resFar, _) =>
              match resFar.visplanes.visplanes[farIdx]?,
                    ds.planes.visplanes[nearIdx]? with
              | some fVp, some nVp =>
                let farTop := fVp.top.getD xNat (UInt8.ofNat 0xff)
                let nearTop := nVp.top.getD xNat (UInt8.ofNat 0xff)
                let bandTop := yhClamped + 1
                let farBot := fVp.bottom.getD xNat (UInt8.ofNat 0)
                let farMisses :=
                  farTop == UInt8.ofNat 0xff ||
                    Int32.ofNat farBot.toNat < bandTop ||
                    Int32.ofNat farTop.toNat > (167 : Int32)
                ok := (← assert "drop far visplane misses near band" farMisses) && ok
                ok := (← assert "drop near visplane still marked"
                  (nearTop != UInt8.ofNat 0xff)) && ok
              | _, _ =>
                ok := (← assert "drop visplanes exist after far store" false) && ok

  -- riser: clippass paints lower strip; floorclip = mid; later floor visplane misses it
  let dsRiser := initBspDrawState data wad fb0
  match subsector viewz ctx st0 {} 0 riserLevel (some dsRiser) with
  | Except.error e =>
    IO.eprintln s!"riser draw setup: {e}"; ok := false
  | Except.ok (_, _, draw) =>
    match draw with
    | none =>
      IO.eprintln "riser draw: missing draw state"; ok := false
    | some ds =>
      match ds.drawSegs[0]? with
      | none =>
        ok := (← assert "riser drawseg recorded" false) && ok
      | some rec =>
        ok := (← assert "riser markfloor" rec.storeRes.markfloor) && ok
        ok := (← assert "riser bottom texture" (rec.storeRes.bottomtexture != 0)) && ok
        ok := (← assert "riser worldlow above worldbottom"
          (rec.storeRes.worldlow > rec.storeRes.worldbottom)) && ok
        let x1 := rec.replayMeta.x1
        let xNat := (x1.toInt).toNat
        let yh0 := rec.storeRes.bottomfrac >>> 12
        let yhClamped :=
          if yh0 >= defaultViewheight then defaultViewheight - 1 else yh0
        let mid0 :=
          (rec.storeRes.pixlow + Int32.ofNat (heightUnit - 1)) >>> 12
        let expFloor := if mid0 <= yhClamped then mid0 else yhClamped + 1
        ok := (← assert "riser floorclip threaded mid"
          (ds.floorclip.getD xNat 200 == expFloor)) && ok
        ok := (← assert "riser floorclip is not drop yh+1"
          (ds.floorclip.getD xNat 200 != yhClamped + 1 || mid0 > yhClamped)) && ok
        match ds.floorplane with
        | none =>
          ok := (← assert "riser near floorplane set" false) && ok
        | some nearIdx =>
          match findPlane ds.planes (32 * F) 20 128 0 with
          | Except.error e =>
            IO.eprintln s!"riser far findPlane: {e}"; ok := false
          | Except.ok (stFar, farIdx) =>
            ok := (← assert "riser far visplane distinct" (farIdx != nearIdx)) && ok
            let farInp : StoreWallInput := {
              viewx := ctx.viewx, viewy := ctx.viewy, viewz := viewz, viewangle := ctx.viewangle,
              rwAngle1 := 0, segAngle := 0, segOffset := 0,
              v1x := rec.replayMeta.v1x, v1y := rec.replayMeta.v1y,
              v2x := rec.replayMeta.v2x, v2y := rec.replayMeta.v2y,
              linedefFlags := 0, textureoffset := 0, rowoffset := 0,
              midtexture := 0, midtextureHeight := 0,
              ceilingheight := 128 * F, floorheight := 32 * F,
              ceilingpic := flatCeiling, floorpic := pic 2, lightlevel := 128,
              backFloorheight := 0, backCeilingheight := 128 * F,
              backFloorpic := pic 1, backCeilingpic := flatCeiling, backLightlevel := 128,
              toptexture := 0, bottomtexture := 0,
              start := rec.replayMeta.x1, stop := rec.replayMeta.x1,
              xtoviewangle := ctx.mapping.xtoviewangle,
              ceilingclip := ds.ceilingclip, floorclip := ds.floorclip,
              visplanes := stFar, floorplane := some farIdx, ceilingplane := none
            }
            match storeWallTwoSided data wad farInp fb0 with
            | Except.error e =>
              IO.eprintln s!"riser far store: {e}"; ok := false
            | Except.ok (resFar, _) =>
              match resFar.visplanes.visplanes[farIdx]? with
              | some fVp =>
                let farTop := fVp.top.getD xNat (UInt8.ofNat 0xff)
                let farBot := fVp.bottom.getD xNat (UInt8.ofNat 0)
                let stripLo := expFloor
                let stripHi := yhClamped
                let farMisses :=
                  farTop == UInt8.ofNat 0xff ||
                    Int32.ofNat farBot.toNat < stripLo ||
                    Int32.ofNat farTop.toNat > stripHi
                ok := (← assert "riser far visplane misses wall strip" farMisses) && ok
              | none =>
                ok := (← assert "riser far visplane exists after far store" false) && ok

  -- closed door is clip-solid but still stored two-sided (`r_bsp.c` `clipsolid`)
  let sClosed := mkSector 0 0 128 (pic 1) flatCeiling
  let doorLevel :=
    let l := mkLine 1 2 1 ML_TWOSIDED 0 1
    mkLevel wideVerts
      #[mkSector 0 (128 * F) 128 (pic 1) flatCeiling, sClosed]
      #[mkSide 0 wallTex noTex wallTex, mkSide 1 noTex noTex noTex] #[l]
      #[wideSeg]
      #[{ numsegs := 1, firstseg := 0, sector := 0 }] #[]
  let dsDoor := initBspDrawState data wad fb0
  match subsector viewz ctx st0 {} 0 doorLevel (some dsDoor) with
  | Except.error e =>
    IO.eprintln s!"door draw setup: {e}"; ok := false
  | Except.ok (st, _, draw) =>
    ok := (← assert "door record-only walls empty" (st.recordedWalls.isEmpty)) && ok
    ok := (← assert "door solidsegs closed"
      ((st.solidsegs.getD 0 ⟨0, 0⟩).last == 144)) && ok
    match draw with
    | none => ok := (← assert "door draw state" false) && ok
    | some ds =>
      ok := (← assert "door drawSegIdx" (ds.drawSegIdx == 1)) && ok
      match ds.drawSegs[0]? with
      | none => ok := (← assert "door drawseg recorded" false) && ok
      | some rec =>
        ok := (← assert "door markceiling" rec.storeRes.markceiling) && ok
        ok := (← assert "door markfloor" rec.storeRes.markfloor) && ok

  if ok then
    IO.println "bsp-storewall-twosided-test: ALL PASS"
    pure 0
  else
    IO.eprintln "bsp-storewall-twosided-test: FAILURES"
    pure 1
