import Doom.Playsim.Fixed
import Doom.Playsim.Level
import Doom.Render.Bsp
import Doom.Render.Clip
import Doom.Render.View

/-!
# BspnodeTest

R1f-bspnode unit tests: `checkBBox`, `subsector`, `renderBspNode`.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.Level
open Doom.Render.Bsp
open Doom.Render.Clip
open Doom.Render.View

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def F : Int32 := FRACUNIT

private def noTex : ByteArray := ByteArray.mk #[45, 0, 0, 0, 0, 0, 0, 0]

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

private def s0 : Sector := mkSector 0 (10 * F) 160 (pic 1) (pic 2)
private def sd0 : Side := mkSide 0 noTex

private def wideVerts : Array Vertex :=
  #[{ x := 0, y := 0 }, { x := -10 * F, y := F }, { x := 10 * F, y := F }]

private def wideSolidSeg : Seg := mkSeg 1 2 0 0 0 ((-1 : Int32))

private def solidLevel : LevelData :=
  let l := mkLine 1 2 ((-1 : Int32)) 0 0 ((-1 : Int32))
  mkLevel wideVerts #[s0] #[sd0] #[l] #[wideSolidSeg]
    #[{ numsegs := 1, firstseg := 0, sector := 0 }] #[]

private def insideBBox : Array Int32 := #[F, (-F), (-F), F]

private def frontBBox : Array Int32 := #[F, (-F), F, (10 * F)]

private def leftBBox : Array Int32 := #[F, (-F), (-20 * F), (-10 * F)]

private def mkSplitNode (childNear childFar : UInt32) : Node := {
  x := 0, y := 0, dx := F, dy := 0
  bbox0 := #[F, (-F), (-10 * F), 0]
  bbox1 := #[F, (-F), (-20 * F), (-10 * F)]
  child0 := childNear, child1 := childFar
}

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let ctx := viewCtx
  let st0 := fresh

  -- checkBBox: view inside bbox (boxpos == 5)
  ok := (← assert "checkBBox inside" (checkBBox ctx st0 insideBBox)) && ok

  -- checkBBox: bbox fully left of view frustum
  ok := (← assert "checkBBox off-left" (!(checkBBox ctx st0 leftBBox))) && ok

  -- checkBBox: open gap (no solidsegs yet, bbox in front)
  ok := (← assert "checkBBox open gap" (checkBBox ctx st0 frontBBox)) && ok

  -- checkBBox: occluded after solid wall covers the projected span
  match clipSolidWallSegment st0 0 319 with
  | Except.error e =>
    IO.eprintln s!"occluded setup: {e}"; ok := false
  | Except.ok stOcc =>
    ok := (← assert "checkBBox occluded" (!(checkBBox ctx stOcc frontBBox))) && ok

  -- subsector: one solid seg golden
  match subsector 0 ctx st0 {} 0 solidLevel with
  | Except.error e =>
    IO.eprintln s!"subsector setup: {e}"; ok := false
  | Except.ok (st, aux, _) =>
    ok := (← assert "subsector sscount" (aux.sscount == 1)) && ok
    ok := (← assert "subsector walls" (st.recordedWalls == #[(0, 144)])) && ok
    ok := (← assert "subsector newend" (st.newend == 2)) && ok
    ok := (← assert "subsector solid last" ((st.solidsegs.getD 0 ⟨0, 0⟩).last == 144)) && ok

  -- renderBspNode: direct leaf subsector 0
  match renderBspNode 0 ctx st0 {} solidLevel (NF_SUBSECTOR ||| 0) with
  | Except.error e =>
    IO.eprintln s!"leaf render: {e}"; ok := false
  | Except.ok (st, aux, _) =>
    ok := (← assert "leaf sscount" (aux.sscount == 1)) && ok
    ok := (← assert "leaf walls" (st.recordedWalls == #[(0, 144)])) && ok

  -- renderBspNode: mini tree — near subsector only (far bbox culled off-left)
  let miniLevel :=
    let l := mkLine 1 2 ((-1 : Int32)) 0 0 ((-1 : Int32))
    mkLevel wideVerts #[s0] #[sd0] #[l] #[wideSolidSeg]
      #[{ numsegs := 1, firstseg := 0, sector := 0 }, { numsegs := 0, firstseg := 0, sector := 0 }]
      #[mkSplitNode (NF_SUBSECTOR ||| 0) (NF_SUBSECTOR ||| 1)]
  match renderBspNode 0 ctx st0 {} miniLevel 0 with
  | Except.error e =>
    IO.eprintln s!"mini-bsp render: {e}"; ok := false
  | Except.ok (st, aux, _) =>
    ok := (← assert "mini-bsp visits near ss" (aux.sscount == 1)) && ok
    ok := (← assert "mini-bsp near walls" (st.recordedWalls == #[(0, 144)])) && ok

  -- loud-error: subsector OOB
  match subsector 0 ctx st0 {} 99 solidLevel with
  | Except.error e =>
    ok := (← assert "subsector OOB loud" (e == "R_Subsector: ss 99 with numss = 1")) && ok
  | Except.ok _ =>
    ok := (← assert "subsector OOB loud" false) && ok

  -- loud-error: bad node index
  match renderBspNode 0 ctx st0 {} solidLevel 99 with
  | Except.error e =>
    ok := (← assert "node OOB loud" (e == "R_RenderBSPNode: bad node 99")) && ok
  | Except.ok _ =>
    ok := (← assert "node OOB loud" false) && ok

  if ok then
    IO.println "bspnode-test: ALL PASS"
    pure 0
  else
    IO.eprintln "bspnode-test: FAILURES"
    pure 1
