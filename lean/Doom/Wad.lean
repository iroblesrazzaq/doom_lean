/-!
# Doom.Wad

WAD file reader (header, directory, lump lookup, lump bytes). Lives outside
`Doom.Playsim` so ordinary Lean types and `IO` are allowed.

Vanilla lookup semantics (`W_CheckNumForName`): scan the directory **backwards**
so the last matching lump wins; name compare is case-insensitive over exactly
8 bytes (space/null-padded on disk).
-/

namespace Doom.Wad

structure LumpEntry where
  filepos : UInt32
  size : UInt32
  /-- Exactly 8 bytes, as stored in the WAD directory. -/
  name : ByteArray

structure WadDirectory where
  /-- `"IWAD"` or `"PWAD"` as 4 bytes. -/
  identification : ByteArray
  numlumps : Nat
  infotableofs : Nat
  entries : Array LumpEntry
  /-- Full file bytes (for lump extraction). -/
  data : ByteArray

def getByte? (data : ByteArray) (i : Nat) : Option UInt8 :=
  if h : i < data.size then some (data.get i h) else none

def readU32LE (data : ByteArray) (off : Nat) : Except String UInt32 := do
  match getByte? data off, getByte? data (off + 1),
        getByte? data (off + 2), getByte? data (off + 3) with
  | some b0, some b1, some b2, some b3 =>
    pure (
      b0.toUInt32
        ||| (b1.toUInt32 <<< 8)
        ||| (b2.toUInt32 <<< 16)
        ||| (b3.toUInt32 <<< 24))
  | _, _, _, _ => throw "unexpected EOF reading u32"

def toUpperAscii (b : UInt8) : UInt8 :=
  if b >= 97 && b <= 122 then b - 32 else b

/-- Pad/truncate an ASCII name to exactly 8 bytes with trailing NULs. -/
def nameTo8 (name : String) : ByteArray :=
  Id.run do
    let raw := name.toUTF8
    let mut out := ByteArray.emptyWithCapacity 8
    let mut i : Nat := 0
    while i < 8 do
      if i < raw.size then
        match getByte? raw i with
        | some b => out := out.push b
        | none => out := out.push 0
      else
        out := out.push 0
      i := i + 1
    pure out

def namesEqualCI8 (a b : ByteArray) : Bool :=
  if a.size != 8 || b.size != 8 then
    false
  else
    Id.run do
      let mut ok := true
      let mut i : Nat := 0
      while i < 8 do
        match getByte? a i, getByte? b i with
        | some x, some y =>
          if toUpperAscii x != toUpperAscii y then
            ok := false
        | _, _ => ok := false
        i := i + 1
      pure ok

/-- Parse a WAD from bytes. Magic must be `IWAD` or `PWAD`. -/
def parseWad (data : ByteArray) : Except String WadDirectory := do
  if data.size < 12 then
    throw "WAD too small for header"
  let id := data.extract 0 4
  let isIwad :=
    match getByte? id 0, getByte? id 1, getByte? id 2, getByte? id 3 with
    | some 73, some 87, some 65, some 68 => true  -- IWAD
    | _, _, _, _ => false
  let isPwad :=
    match getByte? id 0, getByte? id 1, getByte? id 2, getByte? id 3 with
    | some 80, some 87, some 65, some 68 => true  -- PWAD
    | _, _, _, _ => false
  if !(isIwad || isPwad) then
    throw "WAD magic must be IWAD or PWAD"
  let numlumpsU ← readU32LE data 4
  let infotableofsU ← readU32LE data 8
  let numlumps := numlumpsU.toNat
  let infotableofs := infotableofsU.toNat
  let dirBytes := numlumps * 16
  if infotableofs + dirBytes > data.size then
    throw s!"WAD directory out of range: ofs={infotableofs} lumps={numlumps}"
  let mut entries : Array LumpEntry := Array.emptyWithCapacity numlumps
  let mut i : Nat := 0
  while i < numlumps do
    let off := infotableofs + i * 16
    let filepos ← readU32LE data off
    let size ← readU32LE data (off + 4)
    let name := data.extract (off + 8) (off + 16)
    if name.size != 8 then
      throw s!"directory name length {name.size} at lump {i}"
    entries := entries.push { filepos, size, name }
    i := i + 1
  pure {
    identification := id
    numlumps
    infotableofs
    entries
    data
  }

/--
`W_CheckNumForName`: scan backwards; return `none` if not found.
Index is into `entries` (0-based).
-/
def checkNumForName (wad : WadDirectory) (name : String) : Option Nat :=
  let target := nameTo8 name
  Id.run do
    let mut i := wad.entries.size
    let mut found : Option Nat := none
    while i > 0 && found.isNone do
      i := i - 1
      match wad.entries[i]? with
      | some e =>
        if namesEqualCI8 e.name target then
          found := some i
      | none => pure ()
    pure found

/-- Extract lump bytes by directory index. -/
def lumpData (wad : WadDirectory) (idx : Nat) : Except String ByteArray := do
  match wad.entries[idx]? with
  | none => throw s!"lump index {idx} out of range ({wad.entries.size})"
  | some e =>
    let start := e.filepos.toNat
    let stop := start + e.size.toNat
    if stop > wad.data.size then
      throw s!"lump {idx} data out of range: pos={start} size={e.size.toNat}"
    pure (wad.data.extract start stop)

/-- Load and parse a WAD file from disk. -/
def loadFile (path : System.FilePath) : IO WadDirectory := do
  let data ← IO.FS.readBinFile path
  match parseWad data with
  | Except.error e => throw (IO.userError e)
  | Except.ok wad => pure wad

/-- Map lump order after a level label (`doomdata.h` `ML_*`). -/
def ML_LABEL : Nat := 0
def ML_THINGS : Nat := 1
def ML_LINEDEFS : Nat := 2
def ML_SIDEDEFS : Nat := 3
def ML_VERTEXES : Nat := 4
def ML_SEGS : Nat := 5
def ML_SSECTORS : Nat := 6
def ML_NODES : Nat := 7
def ML_SECTORS : Nat := 8
def ML_REJECT : Nat := 9
def ML_BLOCKMAP : Nat := 10

/-- Bytes for the lump at `labelIndex + offset` (e.g. E1M1 + ML_VERTEXES). -/
def mapLumpData (wad : WadDirectory) (labelIndex offset : Nat) : Except String ByteArray :=
  lumpData wad (labelIndex + offset)

end Doom.Wad
