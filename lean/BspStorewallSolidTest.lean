import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Render.Bsp
import Doom.Render.Clip
import Doom.Render.Data
import Doom.Render.Gfx.Texture
import Doom.Render.Seg
import Doom.Render.Types
import Doom.Render.View
import Doom.Wad

/-!
# BspStorewallSolidTest

R1l-bsp-storewall-solid: BSP single-sided solid path draws via `storeWallSolid`.
-/

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Render.Bsp
open Doom.Render.Clip
open Doom.Render.Data
open Doom.Render.Gfx.Texture
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

private def pic (b : UInt8) : ByteArray := ByteArray.mk #[b, 0, 0, 0, 0, 0, 0, 0]

private def mkSector (floor ceiling light : Int32) (fp cp : ByteArray) : Sector := {
  floorheight := floor, ceilingheight := ceiling
  floorpic := fp, ceilingpic := cp, lightlevel := light
  special := 0, tag := 0, lines := #[], blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def mkSide (sec : UInt32) (mid : ByteArray) : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := ByteArray.empty, bottomtexture := ByteArray.empty
  midtexture := mid, sector := sec
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

private def goldenVerts : Array Vertex :=
  #[{ x := 0, y := 0 }, { x := -10 * F, y := F }, { x := 10 * F, y := F }]

private def goldenSeg : Seg := mkSeg 1 2 0 0 0 ((-1 : Int32))

private def goldenLevel : LevelData :=
  let s0 := mkSector 0 (128 * F) 128 (pic 1) flatCeiling
  let sd0 := mkSide 0 wallTex
  let l := mkLine 1 2 ((-1 : Int32)) 0 0 ((-1 : Int32))
  mkLevel goldenVerts #[s0] #[sd0] #[l] #[goldenSeg]
    #[{ numsegs := 1, firstseg := 0, sector := 0 }] #[]

private def goldenNodeLevel : LevelData :=
  let base := goldenLevel
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
  [87, 87, 87, 87, 87].map UInt8.ofNat

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
  let wad := synthWad
  let fb0 := Framebuffer.initBlack
  let ds0 := initBspDrawState data wad fb0

  -- subsector draw: golden column strip + clip + drawSegIdx
  match subsector viewz ctx st0 {} 0 goldenLevel (some ds0) with
  | Except.error e =>
    IO.eprintln s!"subsector draw setup: {e}"; ok := false
  | Except.ok (st, _, draw) =>
    match draw with
    | none =>
      IO.eprintln "subsector draw: missing draw state"; ok := false
    | some ds =>
      let strip := columnStrip ds.fb 10 84 88
      ok := (← assert "golden column strip" (strip == goldenColumnStrip)) && ok
      ok := (← assert "drawSegIdx" (ds.drawSegIdx == 1)) && ok
      ok := (← assert "ceilingclip" (ds.ceilingclip.getD 10 0 == defaultViewheight)) && ok
      ok := (← assert "record-only walls empty" (st.recordedWalls.isEmpty)) && ok
      ok := (← assert "solid seg last" ((st.solidsegs.getD 0 ⟨0, 0⟩).last == 144)) && ok

  -- closed door is clip-solid but still stored two-sided (`r_bsp.c` `clipsolid`)
  let sClosed := mkSector 0 0 128 (pic 1) flatCeiling
  let doorLevel :=
    let wideVerts := #[{ x := 0, y := 0 }, { x := -10 * F, y := F }, { x := 10 * F, y := F }]
    let l := mkLine 1 2 1 ML_TWOSIDED 0 1
    mkLevel wideVerts #[mkSector 0 (128 * F) 128 (pic 1) flatCeiling, sClosed] #[mkSide 0 wallTex, mkSide 1 wallTex]
      #[l] #[mkSeg 1 2 0 0 0 1]
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
    | some ds => ok := (← assert "door drawSegIdx" (ds.drawSegIdx == 1)) && ok

  -- renderBspFromGame integration
  let gs := mkTestGameState goldenNodeLevel
  match renderBspFromGame data wad gs fb0 with
  | Except.error e =>
    IO.eprintln s!"renderBspFromGame setup: {e}"; ok := false
  | Except.ok frame =>
    let strip := columnStrip frame.fb 10 84 88
    ok := (← assert "render column strip" (strip == goldenColumnStrip)) && ok
    ok := (← assert "render drawSegIdx" (frame.drawSegIdx == 1)) && ok

  if ok then
    IO.println "bsp-storewall-solid-test: ALL PASS"
    pure 0
  else
    IO.eprintln "bsp-storewall-solid-test: FAILURES"
    pure 1
