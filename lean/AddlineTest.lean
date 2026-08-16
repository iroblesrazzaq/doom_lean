import Doom.Playsim.Fixed
import Doom.Playsim.Level
import Doom.Render.Bsp
import Doom.Render.Clip
import Doom.Render.View

/-!
# AddlineTest

R1e-addline unit tests: `addLine` clip classifier (`r_bsp.c` `R_AddLine`).
View at origin, angle 0, `initViewMapping 320 160`.
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
    (lines : Array Line) (segs : Array Seg) : LevelData := {
  vertexes := verts, sectors, sides, lines, segs
  subsectors := #[{ numsegs := 0, firstseg := 0, sector := 0 }]
  nodes := #[], things := #[]
  blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
  reject := ByteArray.empty
}

private def viewCtx : RenderViewCtx := {
  viewx := 0, viewy := 0, viewangle := 0
  mapping := initViewMapping 320 160
}

private def fresh : ClipState := clearClipSegs emptyClipState

private def s0 : Sector := mkSector 0 (10 * F) 160 (pic 1) (pic 2)
private def s1 : Sector := mkSector F (10 * F) 160 (pic 1) (pic 2)
private def sClosed : Sector := mkSector 0 0 160 (pic 1) (pic 2)
private def sd0 : Side := mkSide 0 noTex
private def sd1 : Side := mkSide 1 noTex

private def wideVerts : Array Vertex :=
  #[{ x := 0, y := 0 }, { x := -10 * F, y := F }, { x := 10 * F, y := F }]

private def wideSolidLevel : LevelData :=
  let l := mkLine 1 2 ((-1 : Int32)) 0 0 ((-1 : Int32))
  mkLevel wideVerts #[s0] #[sd0] #[l] #[mkSeg 1 2 0 0 0 ((-1 : Int32))]

private def wideWindowLevel : LevelData :=
  let l := mkLine 1 2 1 ML_TWOSIDED 0 1
  mkLevel wideVerts #[s0, s1] #[sd0, sd1] #[l] #[mkSeg 1 2 0 0 0 1]

private def wideDoorLevel : LevelData :=
  let l := mkLine 1 2 1 ML_TWOSIDED 0 1
  mkLevel wideVerts #[s0, sClosed] #[sd0, mkSide 1 noTex] #[l] #[mkSeg 1 2 0 0 0 1]

private def wideSeg : Seg := mkSeg 1 2 0 0 0 1
private def wideSolidSeg : Seg := mkSeg 1 2 0 0 0 ((-1 : Int32))

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let ctx := viewCtx
  let st0 := fresh

  -- 1. Backface
  let vBack := #[{ x := 0, y := 0 }, { x := F, y := -F }, { x := F, y := F }]
  let levBack :=
    mkLevel vBack #[s0, s1] #[sd0, sd1]
      #[mkLine 1 2 1 ML_TWOSIDED 0 1]
      #[mkSeg 1 2 0 0 0 1]
  match addLine ctx 0 st0 (mkSeg 1 2 0 0 0 1) levBack with
  | Except.error e =>
    IO.eprintln s!"backface setup: {e}"; ok := false
  | Except.ok (st, m, _) =>
    ok := (← assert "backface no contrib" (!m.contributed)) && ok
    ok := (← assert "backface unchanged walls" (st.recordedWalls.isEmpty)) && ok
    ok := (← assert "backface unchanged newend" (st.newend == st0.newend)) && ok
    ok := (← assert "backface rw_angle1" (m.rw_angle1 == 3758096384)) && ok

  -- 2. Off-left
  let vLeft := #[{ x := 0, y := 0 }, { x := -20 * F, y := F }, { x := -20 * F, y := 2 * F }]
  let levLeft :=
    mkLevel vLeft #[s0, s1] #[sd0, sd1]
      #[mkLine 1 2 1 ML_TWOSIDED 0 1]
      #[mkSeg 1 2 0 0 0 1]
  match addLine ctx 0 st0 (mkSeg 1 2 0 0 0 1) levLeft with
  | Except.error e =>
    IO.eprintln s!"off-left setup: {e}"; ok := false
  | Except.ok (st, m, _) =>
    ok := (← assert "off-left no contrib" (!m.contributed)) && ok
    ok := (← assert "off-left unchanged walls" (st.recordedWalls.isEmpty)) && ok
    ok := (← assert "off-left unchanged newend" (st.newend == st0.newend)) && ok
    ok := (← assert "off-left rw_angle1" (m.rw_angle1 == 2113466999)) && ok

  -- 3. Off-right
  let vRight := #[{ x := 0, y := 0 }, { x := 20 * F, y := F }, { x := 20 * F, y := 2 * F }]
  let levRight :=
    mkLevel vRight #[s0, s1] #[sd0, sd1]
      #[mkLine 1 2 1 ML_TWOSIDED 0 1]
      #[mkSeg 1 2 0 0 0 1]
  match addLine ctx 0 st0 (mkSeg 1 2 0 0 0 1) levRight with
  | Except.error e =>
    IO.eprintln s!"off-right setup: {e}"; ok := false
  | Except.ok (st, m, _) =>
    ok := (← assert "off-right no contrib" (!m.contributed)) && ok
    ok := (← assert "off-right unchanged walls" (st.recordedWalls.isEmpty)) && ok
    ok := (← assert "off-right unchanged newend" (st.newend == st0.newend)) && ok
    ok := (← assert "off-right rw_angle1" (m.rw_angle1 == 34016648)) && ok

  -- 4. Single-sided solid golden
  match addLine ctx 0 st0 wideSolidSeg wideSolidLevel with
  | Except.error e =>
    IO.eprintln s!"solid setup: {e}"; ok := false
  | Except.ok (st, m, _) =>
    ok := (← assert "solid contributed" m.contributed) && ok
    ok := (← assert "solid rw_angle1" (m.rw_angle1 == 2079617999)) && ok
    ok := (← assert "solid walls" (st.recordedWalls == #[(0, 144)])) && ok
    ok := (← assert "solid seg0 last" ((st.solidsegs.getD 0 ⟨0, 0⟩).last == 144)) && ok
    ok := (← assert "solid backsector -1" (m.backsector == (-1 : Int32))) && ok

  -- 5. Two-sided window pass
  match addLine ctx 0 st0 wideSeg wideWindowLevel with
  | Except.error e =>
    IO.eprintln s!"window setup: {e}"; ok := false
  | Except.ok (st, m, _) =>
    ok := (← assert "window contributed" m.contributed) && ok
    ok := (← assert "window walls" (st.recordedWalls == #[(0, 144)])) && ok
    ok := (← assert "window solidsegs untouched"
      ((st.solidsegs.getD 0 ⟨0, 0⟩).last == (-1 : Int32))) && ok
    ok := (← assert "window backsector 1" (m.backsector == 1)) && ok

  -- 6. Closed door solid
  match addLine ctx 0 st0 wideSeg wideDoorLevel with
  | Except.error e =>
    IO.eprintln s!"door setup: {e}"; ok := false
  | Except.ok (st, m, _) =>
    ok := (← assert "door contributed" m.contributed) && ok
    ok := (← assert "door walls" (st.recordedWalls == #[(0, 144)])) && ok
    ok := (← assert "door solid seg0 last" ((st.solidsegs.getD 0 ⟨0, 0⟩).last == 144)) && ok

  -- 7. Empty trigger no-op
  let vTrig := #[{ x := 0, y := 0 }, { x := -F, y := F }, { x := F, y := F }]
  let levTrig :=
    mkLevel vTrig #[s0, s0] #[sd0, mkSide 1 noTex]
      #[mkLine 1 2 1 ML_TWOSIDED 0 1]
      #[mkSeg 1 2 0 0 0 1]
  match addLine ctx 0 st0 (mkSeg 1 2 0 0 0 1) levTrig with
  | Except.error e =>
    IO.eprintln s!"trigger setup: {e}"; ok := false
  | Except.ok (st, m, _) =>
    ok := (← assert "trigger no contrib" (!m.contributed)) && ok
    ok := (← assert "trigger unchanged walls" (st.recordedWalls.isEmpty)) && ok
    ok := (← assert "trigger unchanged newend" (st.newend == st0.newend)) && ok
    ok := (← assert "trigger rw_angle1" (m.rw_angle1 == 1610612736)) && ok

  -- 8. Degenerate x1 == x2
  let vDeg := #[{ x := 0, y := 0 }, { x := F, y := 0 }, { x := F, y := 1 }]
  let levDeg :=
    mkLevel vDeg #[s0] #[sd0]
      #[mkLine 1 2 ((-1 : Int32)) 0 0 ((-1 : Int32))]
      #[mkSeg 1 2 0 0 0 ((-1 : Int32))]
  match addLine ctx 0 st0 (mkSeg 1 2 0 0 0 ((-1 : Int32))) levDeg with
  | Except.error e =>
    IO.eprintln s!"degen setup: {e}"; ok := false
  | Except.ok (st, m, _) =>
    ok := (← assert "degen no contrib" (!m.contributed)) && ok
    ok := (← assert "degen unchanged walls" (st.recordedWalls.isEmpty)) && ok
    ok := (← assert "degen unchanged newend" (st.newend == st0.newend)) && ok
    ok := (← assert "degen rw_angle1 zero" (m.rw_angle1 == 0)) && ok

  if ok then
    IO.println "addline-test: ALL PASS"
    pure 0
  else
    IO.eprintln "addline-test: FAILURES"
    pure 1
