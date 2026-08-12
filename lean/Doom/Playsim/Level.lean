import Doom.Playsim.Fixed

/-!
# Doom.Playsim.Level

Level geometry loaded from map lumps (`p_setup.c` / `doomdata.h` / `r_defs.h`).
Pointers become indices. Texture/flat name resolution, thing→mobj spawning,
soundorg, and reject matrix usage are deferred.
-/

namespace Doom.Playsim.Level

open Doom.Playsim.Fixed

/-- `doomdata.h` `NF_SUBSECTOR`. -/
def NF_SUBSECTOR : UInt32 := 0x8000

/-- `doomdata.h` `ML_TWOSIDED`. -/
def ML_TWOSIDED : Int32 := 4

/-- `m_bbox.h` box indices. -/
def BOXTOP : Nat := 0
def BOXBOTTOM : Nat := 1
def BOXLEFT : Nat := 2
def BOXRIGHT : Nat := 3

/-- `r_defs.h` `slopetype_t`. -/
def ST_HORIZONTAL : UInt32 := 0
def ST_VERTICAL : UInt32 := 1
def ST_POSITIVE : UInt32 := 2
def ST_NEGATIVE : UInt32 := 3

/-- `p_local.h` `MAPBLOCKSHIFT` = FRACBITS+7. -/
def MAPBLOCKSHIFT : Nat := 23

/-- `p_local.h` `MAXRADIUS` = 32*FRACUNIT. -/
def MAXRADIUS : Int32 := 2097152

structure Vertex where
  x : Int32
  y : Int32

structure Sector where
  floorheight : Int32
  ceilingheight : Int32
  /-- Raw 8-byte flat name; resolution deferred. -/
  floorpic : ByteArray
  /-- Raw 8-byte flat name; resolution deferred. -/
  ceilingpic : ByteArray
  lightlevel : Int32
  special : Int32
  tag : Int32
  /-- Line indices into `LevelData.lines` (filled by `groupLines`). -/
  lines : Array UInt32
  /-- Map-block bbox `[BOXTOP,BOXBOTTOM,BOXLEFT,BOXRIGHT]` (filled by `groupLines`). -/
  blockbox : Array Int32

structure Side where
  textureoffset : Int32
  rowoffset : Int32
  /-- Raw 8-byte texture name; resolution deferred. -/
  toptexture : ByteArray
  /-- Raw 8-byte texture name; resolution deferred. -/
  bottomtexture : ByteArray
  /-- Raw 8-byte texture name; resolution deferred. -/
  midtexture : ByteArray
  sector : UInt32

structure Line where
  v1 : UInt32
  v2 : UInt32
  flags : Int32
  special : Int32
  tag : Int32
  sidenum0 : Int32
  sidenum1 : Int32
  dx : Int32
  dy : Int32
  slopetype : UInt32
  bbox : Array Int32
  frontsector : Int32
  backsector : Int32

structure Seg where
  v1 : UInt32
  v2 : UInt32
  angle : UInt32
  offset : Int32
  linedef : UInt32
  side : UInt32
  frontsector : Int32
  backsector : Int32

structure Subsector where
  numsegs : UInt32
  firstseg : UInt32
  /-- Sector index; filled by `groupLines`. -/
  sector : UInt32

structure Node where
  x : Int32
  y : Int32
  dx : Int32
  dy : Int32
  /-- `bbox[0]` then `bbox[1]`, each length 4. -/
  bbox0 : Array Int32
  bbox1 : Array Int32
  child0 : UInt32
  child1 : UInt32

structure Thing where
  /-- Raw map units (not fixed); sign-extended from disk. -/
  x : Int32
  y : Int32
  angle : Int32
  typeId : Int32
  options : Int32

/--
Blockmap lump as in `P_LoadBlockMap`: header shorts become origin (fixed) and
dimensions; full lump kept as sign-extended Int32 entries (vanilla `short`
array, including offset table and linedef lists terminated by `-1`).
-/
structure BlockMap where
  originX : Int32
  originY : Int32
  width : Int32
  height : Int32
  lump : Array Int32

structure LevelData where
  vertexes : Array Vertex
  sectors : Array Sector
  sides : Array Side
  lines : Array Line
  segs : Array Seg
  subsectors : Array Subsector
  nodes : Array Node
  things : Array Thing
  blockmap : BlockMap
  reject : ByteArray

private def getByte? (data : ByteArray) (i : Nat) : Option UInt8 :=
  if h : i < data.size then some (data.get i h) else none

/-- Little-endian signed 16-bit → sign-extended Int32 (`SHORT`). -/
def readI16LE (data : ByteArray) (off : Nat) : Except String Int32 := do
  match getByte? data off, getByte? data (off + 1) with
  | some b0, some b1 =>
    let u : UInt32 := b0.toUInt32 ||| (b1.toUInt32 <<< 8)
    pure (u.toUInt16.toInt16.toInt32)
  | _, _ => throw s!"unexpected EOF reading i16 at {off}"

/-- Little-endian unsigned 16-bit → zero-extended UInt32. -/
def readU16LE (data : ByteArray) (off : Nat) : Except String UInt32 := do
  match getByte? data off, getByte? data (off + 1) with
  | some b0, some b1 =>
    pure (b0.toUInt32 ||| (b1.toUInt32 <<< 8))
  | _, _ => throw s!"unexpected EOF reading u16 at {off}"

/-- `SHORT(x) << FRACBITS`. -/
def shortFixed (data : ByteArray) (off : Nat) : Except String Int32 := do
  let s ← readI16LE data off
  pure (s <<< 16)

def loadVertexes (data : ByteArray) : Except String (Array Vertex) := do
  if data.size % 4 != 0 then
    throw s!"VERTEXES size {data.size} not multiple of 4"
  let n := data.size / 4
  let mut out : Array Vertex := Array.emptyWithCapacity n
  let mut i : Nat := 0
  while i < n do
    let off := i * 4
    let x ← shortFixed data off
    let y ← shortFixed data (off + 2)
    out := out.push { x, y }
    i := i + 1
  pure out

def loadSectors (data : ByteArray) : Except String (Array Sector) := do
  if data.size % 26 != 0 then
    throw s!"SECTORS size {data.size} not multiple of 26"
  let n := data.size / 26
  let mut out : Array Sector := Array.emptyWithCapacity n
  let mut i : Nat := 0
  while i < n do
    let off := i * 26
    let floorheight ← shortFixed data off
    let ceilingheight ← shortFixed data (off + 2)
    let floorpic := data.extract (off + 4) (off + 12)
    let ceilingpic := data.extract (off + 12) (off + 20)
    let lightlevel ← readI16LE data (off + 20)
    let special ← readI16LE data (off + 22)
    let tag ← readI16LE data (off + 24)
    if floorpic.size != 8 || ceilingpic.size != 8 then
      throw s!"sector {i} flat name length"
    out := out.push {
      floorheight, ceilingheight, floorpic, ceilingpic
      lightlevel, special, tag
      lines := #[]
      blockbox := #[0, 0, 0, 0]
    }
    i := i + 1
  pure out

def loadSideDefs (data : ByteArray) : Except String (Array Side) := do
  if data.size % 30 != 0 then
    throw s!"SIDEDEFS size {data.size} not multiple of 30"
  let n := data.size / 30
  let mut out : Array Side := Array.emptyWithCapacity n
  let mut i : Nat := 0
  while i < n do
    let off := i * 30
    let textureoffset ← shortFixed data off
    let rowoffset ← shortFixed data (off + 2)
    let toptexture := data.extract (off + 4) (off + 12)
    let bottomtexture := data.extract (off + 12) (off + 20)
    let midtexture := data.extract (off + 20) (off + 28)
    let sectorI16 ← readI16LE data (off + 28)
    if toptexture.size != 8 || bottomtexture.size != 8 || midtexture.size != 8 then
      throw s!"side {i} texture name length"
    if sectorI16 < 0 then
      throw s!"side {i} negative sector index {sectorI16}"
    out := out.push {
      textureoffset, rowoffset, toptexture, bottomtexture, midtexture
      sector := sectorI16.toUInt32
    }
    i := i + 1
  pure out

private def setBBox4 (left right bottom top : Int32) : Array Int32 :=
  -- indices: BOXTOP=0, BOXBOTTOM=1, BOXLEFT=2, BOXRIGHT=3
  #[top, bottom, left, right]

/-- Non-negative Int32 → Nat index (`toNatClampNeg`). -/
def i32Idx (x : Int32) : Nat := x.toNatClampNeg

/-- Proof-free `Array.set` that no-ops on OOB (callers check first). -/
def arrSet {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

/-- Arithmetic `>> MAPBLOCKSHIFT` (23). -/
def ashrMapBlock (x : Int32) : Int32 := x >>> 23

def loadLineDefs (data : ByteArray) (vertexes : Array Vertex) (sides : Array Side) :
    Except String (Array Line) := do
  if data.size % 14 != 0 then
    throw s!"LINEDEFS size {data.size} not multiple of 14"
  let n := data.size / 14
  let mut out : Array Line := Array.emptyWithCapacity n
  let mut i : Nat := 0
  while i < n do
    let off := i * 14
    let v1i ← readI16LE data off
    let v2i ← readI16LE data (off + 2)
    let flags ← readI16LE data (off + 4)
    let special ← readI16LE data (off + 6)
    let tag ← readI16LE data (off + 8)
    let sidenum0 ← readI16LE data (off + 10)
    let sidenum1 ← readI16LE data (off + 12)
    if v1i < 0 || v2i < 0 then
      throw s!"line {i} negative vertex"
    let v1u := v1i.toUInt32
    let v2u := v2i.toUInt32
    match vertexes[v1u.toNat]?, vertexes[v2u.toNat]? with
    | some (v1 : Vertex), some (v2 : Vertex) =>
      let dx := v2.x - v1.x
      let dy := v2.y - v1.y
      let slopetype :=
        if dx == 0 then ST_VERTICAL
        else if dy == 0 then ST_HORIZONTAL
        else if fixedDiv dy dx > 0 then ST_POSITIVE
        else ST_NEGATIVE
      let left := if v1.x < v2.x then v1.x else v2.x
      let right := if v1.x < v2.x then v2.x else v1.x
      let bottom := if v1.y < v2.y then v1.y else v2.y
      let top := if v1.y < v2.y then v2.y else v1.y
      let bbox := setBBox4 left right bottom top
      let frontsector : Int32 ←
        if sidenum0 != (-1 : Int32) then
          if sidenum0 < 0 then
            throw s!"line {i} bad sidenum0 {sidenum0}"
          match sides[i32Idx sidenum0]? with
          | some (sd : Side) => pure sd.sector.toInt32
          | none => throw s!"line {i} sidenum0 {sidenum0} out of range"
        else
          pure (-1)
      let backsector : Int32 ←
        if sidenum1 != (-1 : Int32) then
          if sidenum1 < 0 then
            throw s!"line {i} bad sidenum1 {sidenum1}"
          match sides[i32Idx sidenum1]? with
          | some (sd : Side) => pure sd.sector.toInt32
          | none => throw s!"line {i} sidenum1 {sidenum1} out of range"
        else
          pure (-1)
      out := out.push {
        v1 := v1u, v2 := v2u, flags, special, tag
        sidenum0, sidenum1, dx, dy, slopetype, bbox
        frontsector, backsector
      }
    | _, _ => throw s!"line {i} vertex out of range ({v1i},{v2i})"
    i := i + 1
  pure out

def loadSubsectors (data : ByteArray) : Except String (Array Subsector) := do
  if data.size % 4 != 0 then
    throw s!"SSECTORS size {data.size} not multiple of 4"
  let n := data.size / 4
  let mut out : Array Subsector := Array.emptyWithCapacity n
  let mut i : Nat := 0
  while i < n do
    let off := i * 4
    let numsegs ← readU16LE data off
    let firstseg ← readU16LE data (off + 2)
    out := out.push { numsegs, firstseg, sector := 0 }
    i := i + 1
  pure out

def loadNodes (data : ByteArray) : Except String (Array Node) := do
  if data.size % 28 != 0 then
    throw s!"NODES size {data.size} not multiple of 28"
  let n := data.size / 28
  let mut out : Array Node := Array.emptyWithCapacity n
  let mut i : Nat := 0
  while i < n do
    let off := i * 28
    let x ← shortFixed data off
    let y ← shortFixed data (off + 2)
    let dx ← shortFixed data (off + 4)
    let dy ← shortFixed data (off + 6)
    let mut bbox0 : Array Int32 := Array.emptyWithCapacity 4
    let mut bbox1 : Array Int32 := Array.emptyWithCapacity 4
    let mut k : Nat := 0
    while k < 4 do
      let v ← shortFixed data (off + 8 + k * 2)
      bbox0 := bbox0.push v
      k := k + 1
    k := 0
    while k < 4 do
      let v ← shortFixed data (off + 16 + k * 2)
      bbox1 := bbox1.push v
      k := k + 1
    let child0 ← readU16LE data (off + 24)
    let child1 ← readU16LE data (off + 26)
    out := out.push { x, y, dx, dy, bbox0, bbox1, child0, child1 }
    i := i + 1
  pure out

def loadSegs (data : ByteArray) (vertexes : Array Vertex) (lines : Array Line)
    (sides : Array Side) : Except String (Array Seg) := do
  if data.size % 12 != 0 then
    throw s!"SEGS size {data.size} not multiple of 12"
  let n := data.size / 12
  let mut out : Array Seg := Array.emptyWithCapacity n
  let mut i : Nat := 0
  while i < n do
    let off := i * 12
    let v1i ← readI16LE data off
    let v2i ← readI16LE data (off + 2)
    let angleI ← readI16LE data (off + 4)
    let linedefI ← readI16LE data (off + 6)
    let sideI ← readI16LE data (off + 8)
    let offset ← shortFixed data (off + 10)
    if v1i < 0 || v2i < 0 || linedefI < 0 || sideI < 0 then
      throw s!"seg {i} negative index field"
    if vertexes[i32Idx v1i]?.isNone || vertexes[i32Idx v2i]?.isNone then
      throw s!"seg {i} vertex out of range"
    match lines[i32Idx linedefI]? with
    | none => throw s!"seg {i} linedef {linedefI} out of range"
    | some (ldef : Line) =>
      let sideU := sideI.toUInt32
      let sidenum : Int32 :=
        if sideU == 0 then ldef.sidenum0 else ldef.sidenum1
      if sidenum < 0 then
        throw s!"seg {i} linedef {linedefI} side {sideI} has sidenum {sidenum}"
      match sides[i32Idx sidenum]? with
      | none => throw s!"seg {i} sidenum {sidenum} out of range"
      | some (sd : Side) =>
        let frontsector := sd.sector.toInt32
        let backsector : Int32 :=
          if (ldef.flags &&& ML_TWOSIDED) != 0 then
            let otherSide : Int32 :=
              if sideU == 0 then ldef.sidenum1 else ldef.sidenum0
            if otherSide < 0 || i32Idx otherSide >= sides.size then
              (-1)
            else
              match sides[i32Idx otherSide]? with
              | some (osd : Side) => osd.sector.toInt32
              | none => (-1)
          else
            (-1)
        let angle : UInt32 := (angleI <<< 16).toUInt32
        out := out.push {
          v1 := v1i.toUInt32, v2 := v2i.toUInt32
          angle, offset
          linedef := linedefI.toUInt32
          side := sideU
          frontsector, backsector
        }
    i := i + 1
  pure out

def loadThings (data : ByteArray) : Except String (Array Thing) := do
  if data.size % 10 != 0 then
    throw s!"THINGS size {data.size} not multiple of 10"
  let n := data.size / 10
  let mut out : Array Thing := Array.emptyWithCapacity n
  let mut i : Nat := 0
  while i < n do
    let off := i * 10
    let x ← readI16LE data off
    let y ← readI16LE data (off + 2)
    let angle ← readI16LE data (off + 4)
    let typeId ← readI16LE data (off + 6)
    let options ← readI16LE data (off + 8)
    out := out.push { x, y, angle, typeId, options }
    i := i + 1
  pure out

def loadBlockMap (data : ByteArray) : Except String BlockMap := do
  if data.size < 8 then
    throw "BLOCKMAP too small for header"
  if data.size % 2 != 0 then
    throw s!"BLOCKMAP size {data.size} not even"
  let count := data.size / 2
  let mut lump : Array Int32 := Array.emptyWithCapacity count
  let mut i : Nat := 0
  while i < count do
    let s ← readI16LE data (i * 2)
    lump := lump.push s
    i := i + 1
  let ox? : Option Int32 := lump[0]?
  let oy? : Option Int32 := lump[1]?
  let w? : Option Int32 := lump[2]?
  let h? : Option Int32 := lump[3]?
  match ox?, oy?, w?, h? with
  | some ox, some oy, some w, some h =>
    pure {
      originX := ox <<< 16
      originY := oy <<< 16
      width := w
      height := h
      lump
    }
  | _, _, _, _ => throw "BLOCKMAP header missing"

private def clearBox : Array Int32 :=
  #[Int32.minValue, Int32.maxValue, Int32.maxValue, Int32.minValue]

private def addToBox (box : Array Int32) (x y : Int32) : Array Int32 :=
  let top := match box[BOXTOP]? with | some v => v | none => Int32.minValue
  let bottom := match box[BOXBOTTOM]? with | some v => v | none => Int32.maxValue
  let left := match box[BOXLEFT]? with | some v => v | none => Int32.maxValue
  let right := match box[BOXRIGHT]? with | some v => v | none => Int32.minValue
  let top := if y > top then y else top
  let bottom := if y < bottom then y else bottom
  let left := if x < left then x else left
  let right := if x > right then x else right
  #[top, bottom, left, right]

/--
`P_GroupLines`: assign subsector sectors, per-sector line lists, and sector
blockboxes. Sound origin (`soundorg`) is deferred.
-/
def groupLines (level : LevelData) : Except String LevelData := do
  let mut subsectors := level.subsectors
  let mut si : Nat := 0
  while si < subsectors.size do
    match subsectors[si]? with
    | none => throw "subsector missing"
    | some (ss : Subsector) =>
      match level.segs[ss.firstseg.toNat]? with
      | none => throw s!"subsector {si} firstseg {ss.firstseg} out of range"
      | some (seg : Seg) =>
        if seg.frontsector < 0 then
          throw s!"subsector {si} seg has no frontsector"
        let ss' : Subsector := { ss with sector := seg.frontsector.toUInt32 }
        subsectors := arrSet subsectors si ss'
    si := si + 1

  let nsec := level.sectors.size
  let mut linecounts : Array Nat := Array.replicate nsec 0
  let mut li : Nat := 0
  while li < level.lines.size do
    match level.lines[li]? with
    | none => throw "line missing"
    | some (ld : Line) =>
      if ld.frontsector < 0 then
        throw s!"line {li} missing frontsector"
      let fs := i32Idx ld.frontsector
      match linecounts[fs]? with
      | none => throw s!"line {li} frontsector {fs} out of range"
      | some c => linecounts := arrSet linecounts fs (c + 1)
      if ld.backsector >= 0 && ld.backsector != ld.frontsector then
        let bs := i32Idx ld.backsector
        match linecounts[bs]? with
        | none => throw s!"line {li} backsector {bs} out of range"
        | some c => linecounts := arrSet linecounts bs (c + 1)
    li := li + 1

  let mut sectorLines : Array (Array UInt32) := Array.emptyWithCapacity nsec
  let mut s : Nat := 0
  while s < nsec do
    let cap := match linecounts[s]? with | some c => c | none => 0
    sectorLines := sectorLines.push (Array.emptyWithCapacity cap)
    s := s + 1

  linecounts := Array.replicate nsec 0
  li := 0
  while li < level.lines.size do
    match level.lines[li]? with
    | none => throw "line missing"
    | some (ld : Line) =>
      if ld.frontsector >= 0 then
        let secIdx := i32Idx ld.frontsector
        match sectorLines[secIdx]?, linecounts[secIdx]? with
        | some arr, some c =>
          sectorLines := arrSet sectorLines secIdx (arr.push li.toUInt32)
          linecounts := arrSet linecounts secIdx (c + 1)
        | _, _ => throw s!"sector {secIdx} line list missing"
      if ld.backsector >= 0 && ld.frontsector != ld.backsector then
        let secIdx := i32Idx ld.backsector
        match sectorLines[secIdx]?, linecounts[secIdx]? with
        | some arr, some c =>
          sectorLines := arrSet sectorLines secIdx (arr.push li.toUInt32)
          linecounts := arrSet linecounts secIdx (c + 1)
        | _, _ => throw s!"sector {secIdx} line list missing"
    li := li + 1

  let bmap := level.blockmap
  let mut sectors := level.sectors
  s := 0
  while s < nsec do
    match sectors[s]?, sectorLines[s]? with
    | some (sec : Sector), some linesArr =>
      let mut box := clearBox
      let mut j : Nat := 0
      while j < linesArr.size do
        match linesArr[j]? with
        | none => pure ()
        | some lineIdx =>
          match level.lines[lineIdx.toNat]? with
          | none => throw s!"sector {s} line {lineIdx} missing"
          | some (ld : Line) =>
            match level.vertexes[ld.v1.toNat]?, level.vertexes[ld.v2.toNat]? with
            | some (v1 : Vertex), some (v2 : Vertex) =>
              box := addToBox box v1.x v1.y
              box := addToBox box v2.x v2.y
            | _, _ => throw s!"sector {s} line vertex missing"
        j := j + 1
      -- soundorg deferred
      let top := match box[BOXTOP]? with | some v => v | none => (0 : Int32)
      let bottom := match box[BOXBOTTOM]? with | some v => v | none => (0 : Int32)
      let left := match box[BOXLEFT]? with | some v => v | none => (0 : Int32)
      let right := match box[BOXRIGHT]? with | some v => v | none => (0 : Int32)
      let clampHi (block limit : Int32) : Int32 :=
        if block >= limit then limit - 1 else block
      let clampLo (block : Int32) : Int32 :=
        if block < 0 then 0 else block
      let blockTop :=
        clampHi (ashrMapBlock ((top - bmap.originY) + MAXRADIUS)) bmap.height
      let blockBottom :=
        clampLo (ashrMapBlock ((bottom - bmap.originY) - MAXRADIUS))
      let blockRight :=
        clampHi (ashrMapBlock ((right - bmap.originX) + MAXRADIUS)) bmap.width
      let blockLeft :=
        clampLo (ashrMapBlock ((left - bmap.originX) - MAXRADIUS))
      let blockbox := #[blockTop, blockBottom, blockLeft, blockRight]
      sectors := arrSet sectors s { sec with lines := linesArr, blockbox }
    | _, _ => throw s!"sector {s} missing during groupLines"
    s := s + 1

  pure { level with sectors, subsectors }

/--
Build `LevelData` from map lump bytes (order matches `P_SetupLevel` dependencies).
Reject is stored raw; usage deferred.
-/
def buildLevel
    (thingsData linedefsData sidedefsData vertexesData segsData
      ssectorsData nodesData sectorsData rejectData blockmapData : ByteArray) :
    Except String LevelData := do
  let blockmap ← loadBlockMap blockmapData
  let vertexes ← loadVertexes vertexesData
  let sectors ← loadSectors sectorsData
  let sides ← loadSideDefs sidedefsData
  let lines ← loadLineDefs linedefsData vertexes sides
  let subsectors ← loadSubsectors ssectorsData
  let nodes ← loadNodes nodesData
  let segs ← loadSegs segsData vertexes lines sides
  let things ← loadThings thingsData
  let level : LevelData := {
    vertexes, sectors, sides, lines, segs, subsectors, nodes, things
    blockmap, reject := rejectData
  }
  groupLines level

end Doom.Playsim.Level
