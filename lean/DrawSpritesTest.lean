import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
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
import Doom.Render.Things.Add
import Doom.Render.Types
import Doom.Render.View
import Doom.Wad

/-!
# DrawSpritesTest

R1v-drawsprites: `R_DrawMasked` / `R_SortVisSprites` / `R_DrawSprite` / `R_DrawVisSprite`.
-/

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
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

private def mkSpritePatch (topdelta : UInt8) (pixels : Array UInt8) : ByteArray :=
  let len := UInt8.ofNat pixels.size
  ByteArray.mk (
    #[1, 0, 128, 0, 0, 0, 0, 0, 12, 0, 0, 0, topdelta, len, 0] ++ pixels ++ #[0, 0xff]
  )

private def spritePatchA : ByteArray :=
  mkSpritePatch 0 #[201, 202, 203, 204, 205]

private def spritePatchB : ByteArray :=
  mkSpritePatch 0 #[211, 212, 213, 214, 215]

private def spritePatchTiny : ByteArray :=
  mkSpritePatch 0 #[201]

private def combinedWad : WadDirectory :=
  let mask := synthMaskedPostColumn
  let a := spritePatchA
  let b := spritePatchB
  let tiny := spritePatchTiny
  let data := a ++ mask ++ b ++ tiny
  {
    identification := ByteArray.mk #[73, 87, 65, 68], numlumps := 4, infotableofs := 0
    entries := #[
      { filepos := 0, size := UInt32.ofNat a.size, name := nameTo8 "SPRA" },
      { filepos := UInt32.ofNat a.size, size := UInt32.ofNat mask.size, name := nameTo8 "MASKCOL" },
      { filepos := UInt32.ofNat (a.size + mask.size), size := UInt32.ofNat b.size, name := nameTo8 "SPRB" },
      { filepos := UInt32.ofNat (a.size + mask.size + b.size), size := UInt32.ofNat tiny.size, name := nameTo8 "SPRT" }
    ]
    data
  }

private def synthTextureTables : TextureTables :=
  {
    firstFlat := 0, numFlats := 0, skyFlatNum := 0, numTextures := 2
    textures := #[
      { name := nameTo8 "-", width := 1, height := 1, patches := #[] },
      {
        name := nameTo8 "WALL", width := 1, height := 128
        patches := #[{ originx := 0, originy := 0, patchLump := 1 }]
      }
    ]
    columnLump := #[#[(0 : Int32)], #[(1 : Int32)]]
    columnOfs := #[#[0], #[3]]
    widthMask := #[0, 0]
    height := #[1, 128]
    compositeSize := #[0, 0]
    composite := #[none, none]
    hashHead := #[none, some 1]
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

private def windowLevel : LevelData :=
  let sFront := mkSector (32 * F) (84 * F) 128 (pic 1) flatCeiling
  let sBack := mkSector 0 (84 * F) 128 (pic 1) flatCeiling
  let sd := mkSide 0 noTex noTex wallTex
  let l := mkLine 1 2 1 ML_TWOSIDED 0 1
  mkLevel wideVerts #[sFront, sBack] #[sd, mkSide 1 noTex noTex noTex] #[l]
    #[wideSeg]
    #[{ numsegs := 1, firstseg := 0, sector := 0 }] #[]

private def dummyLevel : LevelData :=
  mkLevel #[] #[] #[] #[] #[] #[] #[]

private def dummyGs : GameState :=
  initFromLevel dummyLevel 2 #[true, false, false, false] 0

private def fbPixel (fb : Framebuffer) (x y : Nat) : UInt8 :=
  fb.get x y

private def columnStrip (fb : Framebuffer) (x y0 y1 : Nat) : List UInt8 :=
  (List.range (y1 - y0 + 1)).map (fun d => fbPixel fb x (y0 + d))

private def goldenMidStrip : List UInt8 :=
  [84, 84, 84, 84, 84].map UInt8.ofNat

private def spriteStripA : List UInt8 :=
  [201, 202, 203, 204, 205].map UInt8.ofNat

private def spriteStripB : List UInt8 :=
  [211, 212, 213, 214, 215].map UInt8.ofNat

private def blackStrip : List UInt8 :=
  [0, 0, 0, 0, 0].map UInt8.ofNat

private def lineMeta (rw : UInt32) : AddLineMeta := {
  rw_angle1 := rw, linedef := 0, frontsector := 0, backsector := 1, contributed := true
}

private def mkVis (x1 x2 scale patch : Int32) (cmap : Option Nat) : VisSprite :=
  {
    mobjflags := 0
    scale := scale
    gx := 0, gy := F, gz := 0, gzt := 64 * F
    texturemid := 0
    x1 := x1, x2 := x2
    startfrac := 0
    xiscale := F
    patch := patch
    colormap := cmap
  }

private def emptyMeta : DrawSegReplayMeta :=
  {
    texnum := 0, segStart := 0, x1 := 0, x2 := 0, viewz := 0
    frontFloor := 0, frontCeil := 0, backFloor := 0, backCeil := 0
    midtextureHeight := 0
  }

private def solidClipSeg (x1 x2 rwScale : Int32) : DrawSegRecord :=
  {
    storeRes := {
      drawSegIdx := 1
      rwScale := rwScale
      rwScaleStep := 0
      rwDistance := 0
      rwOffset := 0
      rwMidTextureMid := 0
      rwCenterAngle := 0
      walllights := #[]
      topfrac := 0, topstep := 0, bottomfrac := 0, bottomstep := 0
      markceiling := false, markfloor := false, midtexture := 0
      silhouette := silBottom + silTop
      bsilheight := Int32.maxValue
      tsilheight := Int32.minValue
      ceilingclip := Array.replicate 320 (-1 : Int32)
      floorclip := Array.replicate 320 defaultViewheight
      sprtopclip := some (Array.replicate 320 defaultViewheight)
      sprbottomclip := some (Array.replicate 320 (-1 : Int32))
    }
    replayMeta := { emptyMeta with x1 := x1, x2 := x2, segStart := x1 }
  }

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let data := synthRenderData
  let wad := combinedWad
  let gs := dummyGs
  let fb0 := Framebuffer.initBlack
  let ctx := viewCtx
  let st0 := fresh

  -- (1) empty visSprites → existing mid golden unchanged
  match addLine ctx viewz st0 wideSeg windowLevel none with
  | Except.error e =>
    IO.eprintln s!"t1 setup: {e}"; ok := false
  | Except.ok (_, m, _) =>
    let ds0 := initBspDrawState data wad fb0
    match buildTwoSidedStoreWallInput ctx viewz wideSeg windowLevel (lineMeta m.rw_angle1) 10 10 ds0 with
    | Except.error e =>
      IO.eprintln s!"t1 inp: {e}"; ok := false
    | Except.ok inp =>
      match storeWallTwoSided data wad inp fb0 with
      | Except.error e =>
        IO.eprintln s!"t1 store: {e}"; ok := false
      | Except.ok (res, fbStore) =>
        let dseg : DrawSegRecord :=
          { storeRes := res, replayMeta := buildDrawSegReplayMeta inp res viewz }
        let fbMarked := Framebuffer.set fbStore 0 0 77
        match drawMasked data wad gs fbMarked #[dseg] #[] with
        | Except.error e =>
          IO.eprintln s!"t1 drawMasked: {e}"; ok := false
        | Except.ok fbPainted =>
          ok := (← assert "empty visSprites mid golden"
            (columnStrip fbPainted 10 84 88 == goldenMidStrip)) && ok
          ok := (← assert "empty visSprites preserves unrelated"
            (fbPixel fbPainted 0 0 == 77)) && ok

  -- (2) visSprite, no drawsegs → posts paint; unrelated pixels preserved
  let vis2 := mkVis 5 5 F 0 (some 0)
  let fbMark2 := Framebuffer.set fb0 0 0 77
  match drawMasked data wad gs fbMark2 #[] #[vis2] with
  | Except.error e =>
    IO.eprintln s!"t2: {e}"; ok := false
  | Except.ok fb2 =>
    ok := (← assert "sprite no drawsegs paints"
      (columnStrip fb2 5 84 88 == spriteStripA)) && ok
    ok := (← assert "sprite no drawsegs preserves unrelated"
      (fbPixel fb2 0 0 == 77)) && ok

  -- (3) visSprite behind solid drawseg → fully clipped
  let vis3 := mkVis 10 10 1 0 (some 0)
  let solid := solidClipSeg 10 10 (100 * F)
  match drawMasked data wad gs fb0 #[solid] #[vis3] with
  | Except.error e =>
    IO.eprintln s!"t3: {e}"; ok := false
  | Except.ok fb3 =>
    ok := (← assert "sprite behind solid fully clipped"
      (columnStrip fb3 10 84 88 == blackStrip)) && ok

  -- (4) visSprite in front of masked mid, overlapping x
  match addLine ctx viewz st0 wideSeg windowLevel none with
  | Except.error e =>
    IO.eprintln s!"t4 setup: {e}"; ok := false
  | Except.ok (_, m, _) =>
    let ds0 := initBspDrawState data wad fb0
    match buildTwoSidedStoreWallInput ctx viewz wideSeg windowLevel (lineMeta m.rw_angle1) 10 11 ds0 with
    | Except.error e =>
      IO.eprintln s!"t4 inp: {e}"; ok := false
    | Except.ok inp =>
      match storeWallTwoSided data wad inp fb0 with
      | Except.error e =>
        IO.eprintln s!"t4 store: {e}"; ok := false
      | Except.ok (res, fbStore) =>
        let dseg : DrawSegRecord :=
          { storeRes := res, replayMeta := buildDrawSegReplayMeta inp res viewz }
        let vis4 := mkVis 10 10 (65 * F) 3 (some 0)
        match drawSprite data wad vis4 #[dseg] fbStore with
        | Except.error e =>
          IO.eprintln s!"t4 drawSprite: {e}"; ok := false
        | Except.ok (segsAfter, fbSpr) =>
          let consumed :=
            match segsAfter[0]? with
            | some rec => rec.storeRes.maskedtexturecol.getD 0 0 == Int32.ofNat shrtMax
            | none => false
          let unconsumed :=
            match segsAfter[0]? with
            | some rec => rec.storeRes.maskedtexturecol.getD 1 0 != Int32.ofNat shrtMax
            | none => false
          ok := (← assert "overlap mid consumed SHRT_MAX" consumed) && ok
          ok := (← assert "unconsumed mid still live" unconsumed) && ok
          ok := (← assert "sprite painted on overlap" (fbPixel fbSpr 10 84 == 201)) && ok
          match drawMasked data wad gs fbStore #[dseg] #[vis4] with
          | Except.error e =>
            IO.eprintln s!"t4 drawMasked: {e}"; ok := false
          | Except.ok fb4 =>
            ok := (← assert "remaining pass paints unconsumed mid"
              (columnStrip fb4 11 84 88 == goldenMidStrip)) && ok
            ok := (← assert "remaining pass keeps overlap sprite"
              (fbPixel fb4 10 84 == 201)) && ok

  -- (5) farther sprite (smaller scale) paints before nearer
  let far := mkVis 5 5 F 0 (some 0)
  let near := mkVis 5 5 (2 * F) 2 (some 0)
  match drawMasked data wad gs fb0 #[] #[near, far] with
  | Except.error e =>
    IO.eprintln s!"t5: {e}"; ok := false
  | Except.ok fb5 =>
    ok := (← assert "nearer sprite wins overlapping pixels"
      (columnStrip fb5 5 84 88 == spriteStripB)) && ok

  -- sort permutation: strict <, equal scales keep earlier unsorted entry
  let s0 := mkVis 0 0 300 0 (some 0)
  let s1 := { (mkVis 0 0 100 0 (some 0)) with gx := 1 }
  let s2 := { (mkVis 0 0 100 0 (some 0)) with gx := 2 }
  let sorted := sortVisSprites #[s0, s1, s2]
  ok := (← assert "sort back-to-front permutation"
    (sorted.size == 3 &&
      (sorted.getD 0 {}).gx == 1 &&
      (sorted.getD 1 {}).gx == 2 &&
      (sorted.getD 2 {}).scale == 300)) && ok

  -- R_PointOnSegSide axis / sign-bit cases
  ok := (← assert "pointOnSegSide vertical left-up back"
    (pointOnSegSide 0 F  F 0  F (2 * F))) && ok
  ok := (← assert "pointOnSegSide vertical right-up front"
    (!pointOnSegSide (2 * F) F  F 0  F (2 * F))) && ok
  ok := (← assert "pointOnSegSide horizontal below-left back"
    (pointOnSegSide F 0  0 F  (-F) F)) && ok
  ok := (← assert "pointOnSegSide horizontal below-right front"
    (!pointOnSegSide F 0  0 F  F F)) && ok
  ok := (← assert "pointOnSegSide sign-bit back"
    (pointOnSegSide (-10 * F) (10 * F)  0 0  (100 * F) (100 * F))) && ok
  ok := (← assert "pointOnSegSide sign-bit front"
    (!pointOnSegSide (10 * F) (-10 * F)  0 0  (100 * F) (100 * F))) && ok
  ok := (← assert "pointOnSegSide FixedMul front"
    (!pointOnSegSide (50 * F) (10 * F)  0 0  (100 * F) (100 * F))) && ok
  ok := (← assert "pointOnSegSide FixedMul back"
    (pointOnSegSide (10 * F) (50 * F)  0 0  (100 * F) (100 * F))) && ok

  -- negative xiscale uses cabs for dc_iscale (same 5-pixel strip)
  let visFlip := { vis2 with xiscale := -F }
  match drawMasked data wad gs fb0 #[] #[visFlip] with
  | Except.error e =>
    IO.eprintln s!"flip iscale: {e}"; ok := false
  | Except.ok fbFlip =>
    ok := (← assert "negative xiscale cabs iscale"
      (columnStrip fbFlip 5 84 88 == spriteStripA)) && ok

  -- SIL_BOTTOM dropped when gz >= bsilheight
  let visDrop := mkVis 8 8 F 0 (some 0)
  let dropSeg : DrawSegRecord :=
    { storeRes := {
        drawSegIdx := 1
        rwScale := 100 * F
        rwScaleStep := 0
        rwDistance := 0
        rwOffset := 0
        rwMidTextureMid := 0
        rwCenterAngle := 0
        walllights := #[]
        topfrac := 0, topstep := 0, bottomfrac := 0, bottomstep := 0
        markceiling := false, markfloor := false, midtexture := 0
        silhouette := silBottom
        bsilheight := 0
        tsilheight := 0
        ceilingclip := Array.replicate 320 (-1 : Int32)
        floorclip := Array.replicate 320 defaultViewheight
        sprbottomclip := some (Array.replicate 320 (50 : Int32))
      }
      replayMeta := { emptyMeta with x1 := 8, x2 := 8, segStart := 8 }
    }
  match drawMasked data wad gs fb0 #[dropSeg] #[visDrop] with
  | Except.error e =>
    IO.eprintln s!"sil drop: {e}"; ok := false
  | Except.ok fbDrop =>
    ok := (← assert "SIL_BOTTOM dropped when gz >= bsilheight"
      (columnStrip fbDrop 8 84 88 == spriteStripA)) && ok
  let visBad := { (mkVis 0 0 F 0 (some 0)) with startfrac := 2 * F }
  match drawVisSprite data wad visBad (Array.replicate 320 defaultViewheight)
      (Array.replicate 320 (-1)) fb0 with
  | Except.error e =>
    ok := (← assert "RANGECHECK bad texturecolumn"
      (e == "R_DrawSpriteRange: bad texturecolumn")) && ok
  | Except.ok _ =>
    ok := (← assert "RANGECHECK should throw" false) && ok

  -- colormap=none uses R_DrawFuzzColumn
  let visShadow := mkVis 0 0 F 0 none
  match drawVisSprite data wad visShadow (Array.replicate 320 defaultViewheight)
      (Array.replicate 320 (-1)) fb0 with
  | Except.error e =>
    ok := (← assert s!"colormap none fuzz: {e}" false) && ok
  | Except.ok _ =>
    ok := (← assert "colormap none is fuzz draw" true) && ok

  -- MF_TRANSLATION throws
  let visTr := { (mkVis 0 0 F 0 (some 0)) with mobjflags := MF_TRANSLATION }
  match drawVisSprite data wad visTr (Array.replicate 320 defaultViewheight)
      (Array.replicate 320 (-1)) fb0 with
  | Except.error e =>
    ok := (← assert "MF_TRANSLATION throws"
      (e == "R_DrawVisSprite: MF_TRANSLATION not implemented")) && ok
  | Except.ok _ =>
    ok := (← assert "MF_TRANSLATION should throw" false) && ok

  if ok then
    IO.println "draw-sprites-test: ALL PASS"
    pure 0
  else
    IO.eprintln "draw-sprites-test: FAILURES"
    pure 1
