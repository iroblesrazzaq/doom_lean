import Doom.Wad
import Doom.Playsim.Level

/-!
E1M1 map-load acceptance tests. Observed values extracted from
`fixtures/wads/doom1.wad` via throwaway python3 (see chunk P1b plan).

Observed ground truth (doom1.wad):
- numlumps = 1264
- PLAYPAL last-match index = 0; DEMO1 = 3; E1M1 = 6
- THINGS last-match index = 95 (duplicates across maps; last wins)
- SW18_7 last-match index = 1121
- E1M1 counts: vertexes=467 lines=475 sides=648 sectors=85
  segs=732 subsectors=237 nodes=236 things=138 reject=904 bytes
- vertex[0] map (1088,-3680) → fixed (71303168, -241172480)
- vertex[233] map (3160,-4160) → fixed (207093760, -272629760)
- sector[0] floor=0 ceil_fixed=4718592 light=160
- linedef[0] v1=0 v2=1 flags=1 sidenum=(0,-1)
- linedef[0] dy=0 → ST_HORIZONTAL
- thing[0] (1056,-3616) angle=90 type=1 options=7
- blockmap origin_fixed=(-50855936,-319291392) width=36 height=23
-/

open Doom.Wad
open Doom.Playsim.Level

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  let parts := cwd.components
  match parts.getLast? with
  | some "lean" =>
    match cwd.parent with
    | some p => pure p
    | none => pure cwd
  | _ => pure cwd

private def loadIwad : IO WadDirectory := do
  let root ← defaultRoot
  loadFile (root / "fixtures" / "wads" / "doom1.wad")

private def loadE1M1 (wad : WadDirectory) : Except String LevelData := do
  match checkNumForName wad "E1M1" with
  | none => throw "E1M1 not found"
  | some label =>
    let things ← mapLumpData wad label ML_THINGS
    let linedefs ← mapLumpData wad label ML_LINEDEFS
    let sidedefs ← mapLumpData wad label ML_SIDEDEFS
    let vertexes ← mapLumpData wad label ML_VERTEXES
    let segs ← mapLumpData wad label ML_SEGS
    let ssectors ← mapLumpData wad label ML_SSECTORS
    let nodes ← mapLumpData wad label ML_NODES
    let sectors ← mapLumpData wad label ML_SECTORS
    let reject ← mapLumpData wad label ML_REJECT
    let blockmap ← mapLumpData wad label ML_BLOCKMAP
    buildLevel things linedefs sidedefs vertexes segs ssectors nodes sectors reject blockmap

/-- Structural invariant sweep: indices in bounds; line sectors match sides. -/
private def checkInvariants (level : LevelData) : Except String Unit := do
  let nV := level.vertexes.size
  let nS := level.sectors.size
  let nSd := level.sides.size
  let nL := level.lines.size
  let nSg := level.segs.size
  let nSs := level.subsectors.size
  let nN := level.nodes.size

  let mut i : Nat := 0
  while i < nSd do
    match level.sides[i]? with
    | none => throw "side missing"
    | some sd =>
      if sd.sector.toNat >= nS then
        throw s!"side {i} sector {sd.sector} out of range"
    i := i + 1

  i := 0
  while i < nL do
    match level.lines[i]? with
    | none => throw "line missing"
    | some ld =>
      if ld.v1.toNat >= nV || ld.v2.toNat >= nV then
        throw s!"line {i} vertex out of range"
      if ld.bbox.size != 4 then
        throw s!"line {i} bbox size {ld.bbox.size}"
      if ld.sidenum0 != (-1 : Int32) then
        if ld.sidenum0 < 0 || i32Idx ld.sidenum0 >= nSd then
          throw s!"line {i} sidenum0 out of range"
        match level.sides[i32Idx ld.sidenum0]? with
        | none => throw "side missing"
        | some sd =>
          if ld.frontsector != sd.sector.toInt32 then
            throw s!"line {i} frontsector mismatch"
      else if ld.frontsector != (-1 : Int32) then
        throw s!"line {i} expected frontsector -1"
      if ld.sidenum1 != (-1 : Int32) then
        if ld.sidenum1 < 0 || i32Idx ld.sidenum1 >= nSd then
          throw s!"line {i} sidenum1 out of range"
        match level.sides[i32Idx ld.sidenum1]? with
        | none => throw "side missing"
        | some sd =>
          if ld.backsector != sd.sector.toInt32 then
            throw s!"line {i} backsector mismatch"
      else if ld.backsector != (-1 : Int32) then
        throw s!"line {i} expected backsector -1"
    i := i + 1

  i := 0
  while i < nSg do
    match level.segs[i]? with
    | none => throw "seg missing"
    | some sg =>
      if sg.v1.toNat >= nV || sg.v2.toNat >= nV then
        throw s!"seg {i} vertex out of range"
      if sg.linedef.toNat >= nL then
        throw s!"seg {i} linedef out of range"
      if sg.frontsector < 0 || i32Idx sg.frontsector >= nS then
        throw s!"seg {i} frontsector out of range"
      if sg.backsector != (-1 : Int32) && i32Idx sg.backsector >= nS then
        throw s!"seg {i} backsector out of range"
    i := i + 1

  i := 0
  while i < nSs do
    match level.subsectors[i]? with
    | none => throw "subsector missing"
    | some ss =>
      if ss.firstseg.toNat + ss.numsegs.toNat > nSg then
        throw s!"subsector {i} seg range out of bounds"
      if ss.sector.toNat >= nS then
        throw s!"subsector {i} sector out of range"
    i := i + 1

  i := 0
  while i < nN do
    match level.nodes[i]? with
    | none => throw "node missing"
    | some nd =>
      if nd.bbox0.size != 4 || nd.bbox1.size != 4 then
        throw s!"node {i} bbox size"
      let checkChild (c : UInt32) : Except String Unit := do
        if (c &&& NF_SUBSECTOR) != 0 then
          let idx := (c &&& (0x7fff : UInt32)).toNat
          if idx >= nSs then
            throw s!"node {i} child subsector {idx} out of range"
        else if c.toNat >= nN then
          throw s!"node {i} child node {c} out of range"
      checkChild nd.child0
      checkChild nd.child1
    i := i + 1

  i := 0
  while i < nS do
    match level.sectors[i]? with
    | none => throw "sector missing"
    | some sec =>
      if sec.blockbox.size != 4 then
        throw s!"sector {i} blockbox size"
      let mut j : Nat := 0
      while j < sec.lines.size do
        match sec.lines[j]? with
        | none => throw "sector line missing"
        | some li =>
          if li.toNat >= nL then
            throw s!"sector {i} line {li} out of range"
        j := j + 1
    i := i + 1

  pure ()

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let wad ← loadIwad

  -- WAD header / lookup -------------------------------------------------
  ok := (← assert "numlumps" (wad.numlumps == 1264)) && ok
  ok := (← assert "PLAYPAL index" (checkNumForName wad "PLAYPAL" == some 0)) && ok
  ok := (← assert "DEMO1 index" (checkNumForName wad "DEMO1" == some 3)) && ok
  ok := (← assert "E1M1 index" (checkNumForName wad "E1M1" == some 6)) && ok
  ok := (← assert "last-match-wins THINGS" (checkNumForName wad "THINGS" == some 95)) && ok
  ok := (← assert "last-match-wins SW18_7" (checkNumForName wad "SW18_7" == some 1121)) && ok
  ok := (← assert "case-insensitive lookup" (checkNumForName wad "e1m1" == some 6)) && ok

  match loadE1M1 wad with
  | Except.error e =>
    IO.eprintln s!"FAIL: load E1M1: {e}"
    pure 1
  | Except.ok level =>
    ok := (← assert "count vertexes" (level.vertexes.size == 467)) && ok
    ok := (← assert "count lines" (level.lines.size == 475)) && ok
    ok := (← assert "count sides" (level.sides.size == 648)) && ok
    ok := (← assert "count sectors" (level.sectors.size == 85)) && ok
    ok := (← assert "count segs" (level.segs.size == 732)) && ok
    ok := (← assert "count subsectors" (level.subsectors.size == 237)) && ok
    ok := (← assert "count nodes" (level.nodes.size == 236)) && ok
    ok := (← assert "count things" (level.things.size == 138)) && ok
    ok := (← assert "reject size" (level.reject.size == 904)) && ok

    match level.vertexes[0]? with
    | some v =>
      ok := (← assert "vertex[0].x" (v.x == (71303168 : Int32))) && ok
      ok := (← assert "vertex[0].y" (v.y == (-241172480 : Int32))) && ok
    | none =>
      ok := (← assert "vertex[0] present" false) && ok

    match level.vertexes[233]? with
    | some v =>
      ok := (← assert "vertex[233].x" (v.x == (207093760 : Int32))) && ok
      ok := (← assert "vertex[233].y" (v.y == (-272629760 : Int32))) && ok
    | none =>
      ok := (← assert "vertex[233] present" false) && ok

    match level.sectors[0]? with
    | some s =>
      ok := (← assert "sector[0].floorheight" (s.floorheight == 0)) && ok
      ok := (← assert "sector[0].ceilingheight" (s.ceilingheight == (4718592 : Int32))) && ok
      ok := (← assert "sector[0].lightlevel" (s.lightlevel == 160)) && ok
    | none =>
      ok := (← assert "sector[0] present" false) && ok

    match level.lines[0]? with
    | some ld =>
      ok := (← assert "linedef[0].v1" (ld.v1 == 0)) && ok
      ok := (← assert "linedef[0].v2" (ld.v2 == 1)) && ok
      ok := (← assert "linedef[0].flags" (ld.flags == 1)) && ok
      ok := (← assert "linedef[0].sidenum0" (ld.sidenum0 == 0)) && ok
      ok := (← assert "linedef[0].sidenum1" (ld.sidenum1 == (-1 : Int32))) && ok
      ok := (← assert "linedef[0].backsector" (ld.backsector == (-1 : Int32))) && ok
      ok := (← assert "linedef[0].dy" (ld.dy == 0)) && ok
      ok := (← assert "linedef[0].slopetype" (ld.slopetype == ST_HORIZONTAL)) && ok
    | none =>
      ok := (← assert "linedef[0] present" false) && ok

    match level.things[0]? with
    | some t =>
      ok := (← assert "thing[0].x" (t.x == (1056 : Int32))) && ok
      ok := (← assert "thing[0].y" (t.y == (-3616 : Int32))) && ok
      ok := (← assert "thing[0].angle" (t.angle == 90)) && ok
      ok := (← assert "thing[0].typeId" (t.typeId == 1)) && ok
      ok := (← assert "thing[0].options" (t.options == 7)) && ok
    | none =>
      ok := (← assert "thing[0] present" false) && ok

    ok := (← assert "blockmap.originX" (level.blockmap.originX == (-50855936 : Int32))) && ok
    ok := (← assert "blockmap.originY" (level.blockmap.originY == (-319291392 : Int32))) && ok
    ok := (← assert "blockmap.width" (level.blockmap.width == 36)) && ok
    ok := (← assert "blockmap.height" (level.blockmap.height == 23)) && ok

    match checkInvariants level with
    | Except.error e =>
      ok := (← assert s!"invariants ({e})" false) && ok
    | Except.ok () =>
      ok := (← assert "structural invariants" true) && ok

    if ok then
      IO.println "ALL MAP TESTS PASSED"
      pure 0
    else
      IO.eprintln "SOME MAP TESTS FAILED"
      pure 1
