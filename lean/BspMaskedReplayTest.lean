import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Render.Bsp
import Doom.Render.Clip
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Gfx.Texture
import Doom.Render.Seg
import Doom.Render.Things
import Doom.Render.Types
import Doom.Render.View
import Doom.Wad

/-!
# BspMaskedReplayTest

R1n-bsp-drawseg-masked-replay: accumulate drawsegs during BSP, replay masked
midtextures post-BSP in reverse order.
-/

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Render.Bsp
open Doom.Render.Clip
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Gfx.Texture
open Doom.Render.Seg
open Doom.Render.Things
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

private def synthMaskedPostColumn : ByteArray :=
  ByteArray.mk #[0, 5, 0, 84, 85, 86, 87, 88, 0xff]

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
    firstFlat := 0, numFlats := 0, skyFlatNum := 0, numTextures := 2,
    textures := #[
      { name := nameTo8 "-", width := 1, height := 1, patches := #[] },
      {
        name := nameTo8 "WALL", width := 1, height := 128,
        patches := #[{ originx := 0, originy := 0, patchLump := 1 }]
      }
    ],
    columnLump := #[#[(0 : Int32)], #[(1 : Int32)]],
    columnOfs := #[#[0], #[3]],
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

private def viewz : Int32 := 84 * FRACUNIT

private def wideVerts : Array Vertex :=
  #[{ x := 0, y := 0 }, { x := -10 * F, y := F }, { x := 10 * F, y := F }]

private def wideSeg : Seg := mkSeg 1 2 0 0 0 1

/-- Window: floor step + masked midtexture only (no top/bottom tier paint). -/
private def windowLevel : LevelData :=
  let sFront := mkSector (32 * F) (84 * F) 128 (pic 1) flatCeiling
  let sBack := mkSector 0 (84 * F) 128 (pic 1) flatCeiling
  let sd := mkSide 0 noTex noTex wallTex
  let l := mkLine 1 2 1 ML_TWOSIDED 0 1
  mkLevel wideVerts #[sFront, sBack] #[sd, mkSide 1 noTex noTex noTex] #[l]
    #[wideSeg]
    #[{ numsegs := 1, firstseg := 0, sector := 0 }] #[]

/-- Step: front floor higher; bottom tier, no masked midtexture. -/
private def stepLevel : LevelData :=
  let sFront := mkSector (32 * F) (128 * F) 128 (pic 1) flatCeiling
  let sBack := mkSector 0 (128 * F) 128 (pic 1) flatCeiling
  let sd := mkSide 0 noTex wallTex noTex
  let l := mkLine 1 2 1 ML_TWOSIDED 0 1
  mkLevel wideVerts #[sFront, sBack] #[sd, mkSide 1 noTex noTex noTex] #[l]
    #[wideSeg]
    #[{ numsegs := 1, firstseg := 0, sector := 0 }] #[]

private def windowNodeLevel : LevelData :=
  let base := windowLevel
  { base with
    subsectors := #[
      { numsegs := 1, firstseg := 0, sector := 0 },
      { numsegs := 0, firstseg := 0, sector := 0 }
    ]
    nodes := #[{
      x := 0, y := 0, dx := F, dy := 0
      bbox0 := #[F, (-F), (-10 * F), 0]
      bbox1 := #[F, (-F), (-20 * F), (-10 * F)]
      child0 := NF_SUBSECTOR ||| 0, child1 := NF_SUBSECTOR ||| 1
    }]
  }

private def fbPixel (fb : Framebuffer) (x y : Nat) : UInt8 :=
  fb.get x y

private def columnStrip (fb : Framebuffer) (x y0 y1 : Nat) : List UInt8 :=
  (List.range (y1 - y0 + 1)).map (fun d => fbPixel fb x (y0 + d))

private def goldenColumnStrip : List UInt8 :=
  [84, 84, 84, 84, 84].map UInt8.ofNat

private def blackStrip : List UInt8 :=
  [0, 0, 0, 0, 0].map UInt8.ofNat

private def lineMeta (rw : UInt32) : AddLineMeta := {
  rw_angle1 := rw, linedef := 0, frontsector := 0, backsector := 1, contributed := true
}

private def mkTestGameState (level : LevelData) : GameState :=
  let gs0 := initFromLevel level 2 #[true, false, false, false] 0
  let mo : Mobj := { Doom.Playsim.Mobj.empty with x := 0, y := 0, angle := 0 }
  let pl : Player := { Doom.Playsim.Player.empty with mo := 0, viewz := viewz, viewheight := VIEWHEIGHT }
  { gs0 with
    mobjs := #[mo]
    players := gs0.players.set! 0 pl
  }

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let ctx := viewCtx
  let st0 := fresh
  let data := synthRenderData
  let wad := synthMaskedWad
  let fb0 := Framebuffer.initBlack

  -- record on store (window subsector accumulates one masked drawseg)
  let dsWin := initBspDrawState data wad fb0
  match subsector viewz ctx st0 {} 0 windowLevel (some dsWin) with
  | Except.error e =>
    IO.eprintln s!"record setup: {e}"; ok := false
  | Except.ok (_, _, draw) =>
    match draw with
    | none =>
      IO.eprintln "record: missing draw state"; ok := false
    | some ds =>
      ok := (← assert "record drawSegs size" (ds.drawSegs.size == 1)) && ok
      ok := (← assert "record maskedtexture"
        (ds.drawSegs[0]?.map (·.storeRes.maskedtexture) == some true)) && ok
      ok := (← assert "record drawSegIdx" (ds.drawSegIdx == 1)) && ok

  -- deferred paint: masked midtexture not painted until replay
  let dsDef := initBspDrawState data wad fb0
  match subsector viewz ctx st0 {} 0 windowLevel (some dsDef) with
  | Except.error e =>
    IO.eprintln s!"deferred setup: {e}"; ok := false
  | Except.ok (_, _, draw) =>
    match draw with
    | none => ok := false
    | some ds =>
      let stripBefore := columnStrip ds.fb 10 84 88
      ok := (← assert "deferred no paint before replay" (stripBefore == blackStrip)) && ok
      match replayMaskedDrawSegs data wad ds with
      | Except.error e =>
        IO.eprintln s!"deferred replay: {e}"; ok := false
      | Except.ok ds' =>
        let stripAfter := columnStrip ds'.fb 10 84 88
        ok := (← assert "deferred paint after replay" (stripAfter == goldenColumnStrip)) && ok

  -- direct replay golden strip via recorded drawseg
  match addLine ctx viewz st0 wideSeg windowLevel none with
  | Except.error e =>
    IO.eprintln s!"direct setup: {e}"; ok := false
  | Except.ok (_, m, _) =>
    let ds0 := initBspDrawState data wad fb0
    match buildTwoSidedStoreWallInput ctx viewz wideSeg windowLevel (lineMeta m.rw_angle1) 10 10 ds0 with
    | Except.error e =>
      IO.eprintln s!"direct inp: {e}"; ok := false
    | Except.ok inp =>
      match storeWallTwoSided data wad inp fb0 with
      | Except.error e =>
        IO.eprintln s!"direct store: {e}"; ok := false
      | Except.ok (res, fbStore) =>
        let dseg : DrawSegRecord := { storeRes := res, replayMeta := buildDrawSegReplayMeta inp res viewz }
        let dsRec := { ds0 with fb := fbStore, drawSegs := #[dseg] }
        match replayMaskedDrawSegs data wad dsRec with
        | Except.error e =>
          IO.eprintln s!"direct replay: {e}"; ok := false
        | Except.ok ds' =>
          let strip := columnStrip ds'.fb 10 84 88
          ok := (← assert "direct replay golden strip" (strip == goldenColumnStrip)) && ok

  -- skip non-masked (step level records but replay does not paint masked)
  let dsStep := initBspDrawState data wad fb0
  match subsector viewz ctx st0 {} 0 stepLevel (some dsStep) with
  | Except.error e =>
    IO.eprintln s!"skip setup: {e}"; ok := false
  | Except.ok (_, _, draw) =>
    match draw with
    | none => ok := false
    | some ds =>
      ok := (← assert "skip non-masked record" (ds.drawSegs.size == 1)) && ok
      ok := (← assert "skip non-masked flag"
        (ds.drawSegs[0]?.map (·.storeRes.maskedtexture) == some false)) && ok
      match replayMaskedDrawSegs data wad ds with
      | Except.error e =>
        IO.eprintln s!"skip replay: {e}"; ok := false
      | Except.ok ds' =>
        let strip := columnStrip ds'.fb 10 84 88
        ok := (← assert "skip non-masked no paint" (strip == blackStrip)) && ok

  -- maxDrawSegs silent no-op on push
  let dsBase := initBspDrawState data wad fb0
  let dummyRec : DrawSegRecord := {
    storeRes := {
      drawSegIdx := 0
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
      ceilingclip := dsBase.ceilingclip
      floorclip := dsBase.floorclip
    }
    replayMeta := {
      texnum := 0
      segStart := 0
      x1 := 0
      x2 := 0
      viewz := viewz
      frontFloor := 0
      frontCeil := 0
      backFloor := 0
      backCeil := 0
      midtextureHeight := 0
    }
  }
  let fullSegs := Array.replicate maxDrawSegs dummyRec
  let dsFull := { initBspDrawState data wad fb0 with drawSegs := fullSegs }
  match subsector viewz ctx st0 {} 0 windowLevel (some dsFull) with
  | Except.error e =>
    IO.eprintln s!"maxDrawSegs setup: {e}"; ok := false
  | Except.ok (_, _, draw) =>
    match draw with
    | none => ok := false
    | some ds =>
      ok := (← assert "maxDrawSegs size unchanged" (ds.drawSegs.size == maxDrawSegs)) && ok

  -- renderBspFromGame records drawsegs only; masked mid paint is deferred to drawMasked
  let gs := mkTestGameState windowNodeLevel
  match renderBspFromGame data wad gs fb0 with
  | Except.error e =>
    IO.eprintln s!"e2e setup: {e}"; ok := false
  | Except.ok frame =>
    let strip := columnStrip frame.fb 10 84 88
    ok := (← assert "e2e bsp mid strip black" (strip == blackStrip)) && ok
    ok := (← assert "e2e drawSegs size" (frame.drawSegs.size == 1)) && ok
    ok := (← assert "e2e drawSegIdx" (frame.drawSegIdx == 1)) && ok
    let fbMarked := Framebuffer.set frame.fb 0 0 77
    match drawMasked data wad gs fbMarked frame.drawSegs #[] with
    | Except.error e =>
      IO.eprintln s!"e2e drawMasked: {e}"; ok := false
    | Except.ok fbPainted =>
      let stripPainted := columnStrip fbPainted 10 84 88
      ok := (← assert "e2e drawMasked golden strip" (stripPainted == goldenColumnStrip)) && ok
      ok := (← assert "e2e drawMasked preserves unrelated pixels" (fbPixel fbPainted 0 0 == 77)) && ok

  -- empty drawSegs: identity, no throw
  let fbMark := Framebuffer.set fb0 10 84 99
  match drawMasked data wad gs fbMark #[] #[] with
  | Except.error e =>
    IO.eprintln s!"empty drawSegs: {e}"; ok := false
  | Except.ok fbId =>
    ok := (← assert "empty drawSegs identity" (fbId.pixels == fbMark.pixels)) && ok

  -- reverse order: replay processes higher index before lower
  match addLine ctx viewz st0 wideSeg windowLevel none with
  | Except.error e =>
    IO.eprintln s!"reverse meta: {e}"; ok := false
  | Except.ok (_, m, _) =>
    let ds0 := initBspDrawState data wad fb0
    match buildTwoSidedStoreWallInput ctx viewz wideSeg windowLevel (lineMeta m.rw_angle1) 10 10 ds0 with
    | Except.error e =>
      IO.eprintln s!"reverse inp0: {e}"; ok := false
    | Except.ok inp0 =>
      match storeWallTwoSided data wad inp0 fb0 with
      | Except.error e =>
        IO.eprintln s!"reverse store0: {e}"; ok := false
      | Except.ok (res0, fb1) =>
        let dseg0 : DrawSegRecord := { storeRes := res0, replayMeta := buildDrawSegReplayMeta inp0 res0 viewz }
        match buildTwoSidedStoreWallInput ctx viewz wideSeg windowLevel (lineMeta m.rw_angle1) 11 11 ds0 with
        | Except.error e =>
          IO.eprintln s!"reverse inp1: {e}"; ok := false
        | Except.ok inp1 =>
          match storeWallTwoSided data wad inp1 fb1 with
          | Except.error e =>
            IO.eprintln s!"reverse store1: {e}"; ok := false
          | Except.ok (res1, fb2) =>
            let dseg1 : DrawSegRecord := { storeRes := res1, replayMeta := buildDrawSegReplayMeta inp1 res1 viewz }
            let dsTwo := { ds0 with fb := fb2, drawSegs := #[dseg0, dseg1] }
            match replayMaskedDrawSegs data wad dsTwo with
            | Except.error e =>
              IO.eprintln s!"reverse replay: {e}"; ok := false
            | Except.ok ds' =>
              let strip10 := columnStrip ds'.fb 10 84 88
              let strip11 := columnStrip ds'.fb 11 84 88
              ok := (← assert "reverse col10 painted" (strip10 == goldenColumnStrip)) && ok
              ok := (← assert "reverse col11 painted" (strip11 == goldenColumnStrip)) && ok
              ok := (← assert "reverse drawSegs order preserved" (ds'.drawSegs.size == 2)) && ok

  if ok then
    IO.println "bsp-masked-replay-test: ALL PASS"
    pure 0
  else
    IO.eprintln "bsp-masked-replay-test: FAILURES"
    pure 1
