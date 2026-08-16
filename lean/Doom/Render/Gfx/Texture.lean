import Doom.Render.Gfx.Flat
import Doom.Render.Gfx.Patch
import Doom.Render.Util
import Doom.Wad

/-!
# Doom.Render.Gfx.Texture

Wall texture tables (`r_data.c`: `R_InitTextures`, `R_GenerateLookup`,
`R_GenerateComposite`, `R_GetColumn`, texture name lookup).
-/

namespace Doom.Render.Gfx.Texture

open Doom.Render.Gfx.Flat
open Doom.Render.Util
open Doom.Wad

structure TexPatch where
  originx : Int32
  originy : Int32
  patchLump : Nat

structure TextureDef where
  name : ByteArray
  width : Nat
  height : Nat
  patches : Array TexPatch

/-- Per-texture column routing and lazily-built composite caches (`r_data.c`). -/
structure TextureTables where
  firstFlat : Nat
  numFlats : Nat
  skyFlatNum : Nat
  skytexture : Nat := 0
  numTextures : Nat
  textures : Array TextureDef
  columnLump : Array (Array Int32)
  columnOfs : Array (Array Nat)
  widthMask : Array Nat
  height : Array Nat
  compositeSize : Array Nat
  composite : Array (Option ByteArray)
  hashHead : Array (Option Nat)
  hashNext : Array (Option Nat)

def byteArrayToName (ba : ByteArray) : String :=
  Id.run do
    let mut chars : Array Char := #[]
    let mut i : Nat := 0
    while i < ba.size && i < 8 do
      match ba[i]? with
      | some b =>
        if b == 0 then
          i := 8
        else
          chars := chars.push (Char.ofNat b.toNat)
          i := i + 1
      | none => i := i + 1
    String.ofList chars.toList

def widthMask (w : Nat) : Nat :=
  Id.run do
    let mut j : Nat := 1
    while j * 2 <= w do
      j := j * 2
    pure (j - 1)

private def readTextureName (data : ByteArray) (off : Nat) : ByteArray :=
  data.extract off (off + 8)

private def parseMapPatches (data : ByteArray) (off : Nat) (count : Nat) (patchlookup : Array (Option Nat)) (texName : ByteArray) :
    Except String (Array TexPatch) := do
  let mut patches : Array TexPatch := #[]
  let mut j : Nat := 0
  while j < count do
    let base := off + j * 10
    let originx ← readI16LE data base
    let originy ← readI16LE data (base + 2)
    let patchIdx ← readI16LE data (base + 4)
    let pi := usize16 patchIdx
    if pi >= patchlookup.size then
      throw s!"patch index {pi} out of range ({patchlookup.size})"
    match patchlookup[pi]! with
    | none =>
      throw s!"R_InitTextures: Missing patch in texture {byteArrayToName texName}"
    | some lumpIdx =>
      patches := patches.push { originx, originy, patchLump := lumpIdx }
    j := j + 1
  pure patches

private def parseMapTexture (data : ByteArray) (off : Nat) (patchlookup : Array (Option Nat)) :
    Except String TextureDef := do
  if off + 22 > data.size then
    throw "maptexture truncated"
  let name := readTextureName data off
  let widthS ← readI16LE data (off + 12)
  let heightS ← readI16LE data (off + 14)
  let patchcountS ← readI16LE data (off + 20)
  let width := usize16 widthS
  let height := usize16 heightS
  let patchcount := usize16 patchcountS
  let patches ← parseMapPatches data (off + 22) patchcount patchlookup name
  pure { name, width, height, patches }

private def loadPatchlookup (wad : WadDirectory) : Except String (Array (Option Nat)) := do
  match checkNumForName wad "PNAMES" with
  | none => throw "missing PNAMES"
  | some pnamesIdx =>
    let pnames ← lumpData wad pnamesIdx
    if pnames.size < 4 then throw "PNAMES too small"
    let n ← readI32LE pnames 0
    let count := i32ToNat n
    let need := 4 + count * 8
    if pnames.size < need then
      throw s!"PNAMES size {pnames.size} < expected {need}"
    let mut lookup : Array (Option Nat) := #[]
    let mut i : Nat := 0
    while i < count do
      let nameBa := pnames.extract (4 + i * 8) (4 + i * 8 + 8)
      let name := byteArrayToName nameBa
      lookup := lookup.push (checkNumForName wad name)
      i := i + 1
    pure lookup

private def loadTextureDirectory (wad : WadDirectory) (patchlookup : Array (Option Nat)) :
    Except String (Array TextureDef) := do
  match checkNumForName wad "TEXTURE1" with
  | none => throw "missing TEXTURE1"
  | some tex1Idx =>
    let maptex1 ← lumpData wad tex1Idx
    if maptex1.size < 4 then throw "TEXTURE1 too small"
    let num1 ← readI32LE maptex1 0
    let count1 := i32ToNat num1
    let dir1Need := 4 + count1 * 4
    if maptex1.size < dir1Need then throw "TEXTURE1 directory truncated"
    let (extraTex, extraCount) ←
      match checkNumForName wad "TEXTURE2" with
      | none => pure (ByteArray.empty, 0)
      | some tex2Idx => do
        let maptex2 ← lumpData wad tex2Idx
        let num2 ← readI32LE maptex2 0
        let count2 := i32ToNat num2
        let dir2Need := 4 + count2 * 4
        if maptex2.size < dir2Need then throw "TEXTURE2 directory truncated"
        pure (maptex2, count2)
    let total := count1 + extraCount
    let mut textures : Array TextureDef := #[]
    let mut i : Nat := 0
    while i < total do
      let (maptex, maxoff, dirOff) :=
        if i < count1 then
          (maptex1, maptex1.size, 4 + i * 4)
        else
          let j := i - count1
          (extraTex, extraTex.size, 4 + j * 4)
      let offset ← readI32LE maptex dirOff
      let off := i32ToNat offset
      if off > maxoff then
        throw "R_InitTextures: bad texture directory"
      let tex ← parseMapTexture maptex off patchlookup
      textures := textures.push tex
      i := i + 1
    pure textures

/-- Clip and draw one patch column into a composite cache column (`R_DrawColumnInCache`). -/
private def drawColumnInCache (patchcol : ByteArray) (block : ByteArray) (blockOff originy : Nat)
    (cacheheight : Nat) : Except String ByteArray := do
  let mut pos : Nat := 0
  let mut out := block
  let mut guard : Nat := cacheheight + 8
  while guard > 0 do
    guard := guard - 1
    match patchcol[pos]? with
    | none =>
      throw s!"truncated patch column at composite draw pos={pos}"
    | some topdelta =>
      if topdelta == 0xff then
        guard := 0
      else
        match patchcol[pos + 1]? with
        | none =>
          throw s!"truncated patch column at composite draw pos={pos}"
        | some length =>
          let len := length.toNat
          let mut position := Int32.ofNat originy + Int32.ofNat topdelta.toNat
          let mut count := len
          if position < 0 then
            count := usize16 (Int32.ofNat count + position)
            position := 0
          let posNat := usize16 position
          if posNat + count > cacheheight then
            count := cacheheight - posNat
          if count > 0 then
            let mut row : Nat := 0
            while row < count do
              match patchcol[pos + 3 + row]? with
              | some pix =>
                let dst := blockOff + posNat + row
                if dst < out.size then
                  out := setByte out dst pix
              | none => pure ()
              row := row + 1
          pos := pos + 4 + len
  pure out

private def patchColumnBytes (patchData : ByteArray) (col : Nat) : Except String ByteArray := do
  let hdr ← Patch.parseHeader patchData
  let colOff ← Patch.columnOffset patchData hdr col
  pure (patchData.extract colOff patchData.size)

private def generateLookupFor (tex : TextureDef) (wad : WadDirectory) :
    Except String (Array Int32 × Array Nat × Nat) := do
  let w := tex.width
  let h := tex.height
  let mut collump : Array Int32 := Array.replicate w 0
  let mut colofs : Array Nat := Array.replicate w 0
  let mut patchcount : Array Nat := Array.replicate w 0
  let mut i : Nat := 0
  while i < tex.patches.size do
    match tex.patches[i]? with
    | none => pure ()
    | some patch =>
      let patchData ← lumpData wad patch.patchLump
      let hdr ← Patch.parseHeader patchData
      let pw := Int32.ofNat hdr.width.toNat
      let x1 := patch.originx
      let x2 := x1 + pw
      let xStart : Int32 := if x1 < 0 then 0 else x1
      let xStop : Int32 := if x2 > Int32.ofNat w then Int32.ofNat w else x2
      let mut x : Int32 := xStart
      while x < xStop do
        let xi := usize16 x
        let relCol := usize16 (x - x1)
        let relOff ← readI32LE patchData (8 + relCol * 4)
        let absOff := relOff.toUInt32.toNat + 3
        patchcount := arrSetNat patchcount xi (patchcount[xi]! + 1)
        collump := arrSetI32 collump xi (Int32.ofNat patch.patchLump)
        colofs := arrSetNat colofs xi absOff
        x := x + 1
    i := i + 1
  let mut compositesize : Nat := 0
  let mut x : Nat := 0
  while x < w do
    if patchcount[x]! == 0 then
      pure ()
    else if patchcount[x]! > 1 then
      collump := arrSetI32 collump x (-1 : Int32)
      colofs := arrSetNat colofs x compositesize
      if compositesize > 0x10000 - h then
        throw s!"R_GenerateLookup: texture is >64k"
      compositesize := compositesize + h
    x := x + 1
  pure (collump, colofs, compositesize)

private def generateCompositeFor (tt : TextureTables) (wad : WadDirectory) (texnum : Nat) :
    Except String ByteArray := do
  match tt.textures[texnum]? with
  | none => throw s!"texture {texnum} missing"
  | some tex =>
    let w := tex.width
    let h := tex.height
    let size := tt.compositeSize[texnum]!
    let mut block := ByteArray.mk (Array.replicate size 0)
    let collump := tt.columnLump[texnum]!
    let colofs := tt.columnOfs[texnum]!
    let mut i : Nat := 0
    while i < tex.patches.size do
      match tex.patches[i]? with
      | none => pure ()
      | some patch =>
        let patchData ← lumpData wad patch.patchLump
        let hdr ← Patch.parseHeader patchData
        let pw := Int32.ofNat hdr.width.toNat
        let x1 := patch.originx
        let x2 := x1 + pw
        let xStart : Int32 := if x1 < 0 then 0 else x1
        let xStop : Int32 := if x2 > Int32.ofNat w then Int32.ofNat w else x2
        let mut x : Int32 := xStart
        while x < xStop do
          let xi := usize16 x
          let lumpEntry := collump[xi]!
          if lumpEntry >= 0 then
            pure ()
          else
            let relCol := usize16 (x - x1)
            let patchcol ← patchColumnBytes patchData relCol
            let originy := usize16 patch.originy
            block ← drawColumnInCache patchcol block (colofs[xi]!) originy h
          x := x + 1
      i := i + 1
    pure block

private def buildHashTable (textures : Array TextureDef) : (Array (Option Nat) × Array (Option Nat)) :=
  Id.run do
    let n := textures.size
    let mut heads : Array (Option Nat) := Array.replicate n none
    let mut next : Array (Option Nat) := Array.replicate n none
    let mut i : Nat := 0
    while i < n do
      match textures[i]? with
      | none => pure ()
      | some tex =>
        let key := lumpNameHash tex.name % n
        match heads[key]! with
        | none => heads := arrSet heads key (some i)
        | some head =>
          let mut rover := head
          let mut done := false
          while !done do
            match next[rover]! with
            | none =>
              next := arrSet next rover (some i)
              done := true
            | some nxt => rover := nxt
      i := i + 1
    pure (heads, next)

private def loadTextureTables (wad : WadDirectory) : Except String TextureTables := do
  let fStart ←
    match checkNumForName wad "F_START" with
    | none => throw "missing F_START"
    | some idx => pure (idx + 1)
  let fEnd ←
    match checkNumForName wad "F_END" with
    | none => throw "missing F_END"
    | some idx => pure idx
  if fEnd <= fStart then
    throw "invalid flat range F_START..F_END"
  let patchlookup ← loadPatchlookup wad
  let textures ← loadTextureDirectory wad patchlookup
  let numTextures := textures.size
  let mut columnLump : Array (Array Int32) := #[]
  let mut columnOfs : Array (Array Nat) := #[]
  let mut widthMasks : Array Nat := #[]
  let mut heights : Array Nat := #[]
  let mut compositeSize : Array Nat := #[]
  let mut composite : Array (Option ByteArray) := #[]
  let mut ti : Nat := 0
  while ti < numTextures do
    match textures[ti]? with
    | none => throw s!"texture {ti} missing after load"
    | some tex =>
      let (cl, co, cs) ← generateLookupFor tex wad
      columnLump := columnLump.push cl
      columnOfs := columnOfs.push co
      widthMasks := widthMasks.push (widthMask tex.width)
      heights := heights.push tex.height
      compositeSize := compositeSize.push cs
      composite := composite.push none
    ti := ti + 1
  let (hashHead, hashNext) := buildHashTable textures
  pure {
    firstFlat := fStart
    numFlats := fEnd - fStart
    skyFlatNum := 0
    skytexture := 0
    numTextures
    textures
    columnLump
    columnOfs
    widthMask := widthMasks
    height := heights
    compositeSize
    composite
    hashHead
    hashNext
  }

def checkTextureNumForName (tt : TextureTables) (name : ByteArray) : Option Nat :=
  if isNoTexture name then
    some 0
  else if tt.numTextures == 0 then
    none
  else
    let key := lumpNameHash name % tt.numTextures
    Id.run do
      let mut idx : Option Nat := tt.hashHead[key]!
      while idx.isSome do
        match idx with
        | none => return none
        | some i =>
          match tt.textures[i]? with
          | none => return none
          | some tex =>
            if namesEqual8 tex.name name then
              return some i
            idx := tt.hashNext[i]!
      return none

def textureNumForName (tt : TextureTables) (name : ByteArray) : Except String Nat := do
  match checkTextureNumForName tt name with
  | some n => pure n
  | none => throw s!"R_TextureNumForName: {byteArrayToName name} not found"

def flatNumFromBytes (tt : TextureTables) (wad : WadDirectory) (name : ByteArray) : Except String Nat :=
  flatNumForName wad tt.firstFlat (byteArrayToName name)

/-- `R_InitTextures` plus shareware sky map (`F_SKY1` / `SKY1`). -/
def initTextures (wad : WadDirectory) : Except String TextureTables := do
  let tt ← loadTextureTables wad
  let skyFlatNum ← flatNumFromBytes tt wad (nameTo8 "F_SKY1")
  let skytexture ← textureNumForName tt (nameTo8 "SKY1")
  pure { tt with skyFlatNum, skytexture }

def getColumn (tt : TextureTables) (wad : WadDirectory) (tex : Nat) (col : Nat) :
    Except String (TextureTables × ByteArray) := do
  if tex >= tt.numTextures then
    throw s!"R_GetColumn: texture {tex} out of range"
  if tex >= tt.composite.size then
    throw s!"R_GetColumn: texture {tex} composite table out of range"
  let mask := tt.widthMask[tex]!
  let colMasked := col &&& mask
  let lump := tt.columnLump[tex]![colMasked]!
  let ofs := tt.columnOfs[tex]![colMasked]!
  if lump > 0 then
    let patchData ← lumpData wad (usize16 lump)
    if ofs > patchData.size then
      throw s!"R_GetColumn: offset {ofs} out of patch range {patchData.size}"
    pure (tt, patchData.extract ofs patchData.size)
  else
    let tt2 ←
      match tt.composite[tex]! with
      | some _ => pure tt
      | none => do
        let block ← generateCompositeFor tt wad tex
        pure { tt with composite := arrSet tt.composite tex (some block) }
    match tt2.composite[tex]! with
    | none => throw "R_GetColumn: composite missing after generation"
    | some compData =>
      let h := tt2.height[tex]!
      if ofs + h > compData.size then
        throw s!"R_GetColumn: composite offset {ofs}+height {h} > {compData.size}"
      pure (tt2, compData.extract ofs (ofs + h))

/-- `R_GetColumn` offset −3 for masked post headers (`r_things.c` / `r_segs.c`). -/
def getMaskColumnPosts (tt : TextureTables) (wad : WadDirectory) (tex : Nat) (col : Nat) :
    Except String (TextureTables × ByteArray) := do
  if tex >= tt.numTextures then
    throw s!"R_GetColumn: texture {tex} out of range"
  if tex >= tt.composite.size then
    throw s!"R_GetColumn: texture {tex} composite table out of range"
  let mask := tt.widthMask[tex]!
  let colMasked := col &&& mask
  let lump := tt.columnLump[tex]![colMasked]!
  let ofs := tt.columnOfs[tex]![colMasked]!
  if lump <= 0 then
    throw s!"R_GetColumn: lump {lump} <= 0 for masked column"
  if ofs < 3 then
    throw s!"R_GetColumn: offset {ofs} < 3 for masked column"
  let patchData ← lumpData wad (usize16 lump)
  if ofs > patchData.size then
    throw s!"R_GetColumn: offset {ofs} out of patch range {patchData.size}"
  pure (tt, patchData.extract (ofs - 3) patchData.size)

end Doom.Render.Gfx.Texture
