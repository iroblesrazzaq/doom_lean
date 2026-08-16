import Doom.Wad

/-!
# Doom.Render.Gfx.Patch

Doom patch (column/post) decode (`v_patch.h`, `v_video.c`).
-/

namespace Doom.Render.Gfx.Patch

open Doom.Wad

structure PatchHeader where
  width : UInt16
  height : UInt16
  leftOffset : UInt16
  topOffset : UInt16
  deriving Repr

def readU16LE (data : ByteArray) (off : Nat) : Except String UInt16 := do
  match data[off]?, data[off + 1]? with
  | some b0, some b1 =>
    pure (b0.toUInt16 ||| (b1.toUInt16 <<< 8))
  | _, _ => throw s!"EOF reading u16 at {off}"

def readI32LE (data : ByteArray) (off : Nat) : Except String Int32 := do
  match data[off]?, data[off + 1]?, data[off + 2]?, data[off + 3]? with
  | some b0, some b1, some b2, some b3 =>
    pure (
      b0.toUInt32
        ||| (b1.toUInt32 <<< 8)
        ||| (b2.toUInt32 <<< 16)
        ||| (b3.toUInt32 <<< 24)).toInt32
  | _, _, _, _ => throw s!"EOF reading i32 at {off}"

def parseHeader (data : ByteArray) : Except String PatchHeader := do
  if data.size < 8 then throw "patch too small for header"
  let width ← readU16LE data 0
  let height ← readU16LE data 2
  let leftOffset ← readU16LE data 4
  let topOffset ← readU16LE data 6
  pure { width, height, leftOffset, topOffset }

/-- Column offset table starts at byte 8; `width` entries of i32. -/
def columnOffset (data : ByteArray) (hdr : PatchHeader) (col : Nat) : Except String Nat := do
  if col >= hdr.width.toNat then
    throw s!"patch column {col} out of range width={hdr.width}"
  let off := 8 + col * 4
  let rel ← readI32LE data off
  let abs := rel.toUInt32.toNat
  if abs >= data.size then
    throw s!"patch column offset {abs} out of range size={data.size}"
  pure abs

/--
Draw one patch column's posts into a temporary column buffer (height bytes).
Returns list of (y, color) pixels relative to patch top.
-/
def decodeColumnPixels (data : ByteArray) (colOff : Nat) (patchHeight : Nat) :
    Except String (Array (Nat × UInt8)) := do
  let mut out : Array (Nat × UInt8) := #[]
  let mut pos := colOff
  let mut guard : Nat := patchHeight + 4
  while guard > 0 do
    guard := guard - 1
    match data[pos]? with
    | none =>
      throw s!"truncated patch column at offset {colOff}"
    | some topdelta =>
      if topdelta == 0xff then
        guard := 0
      else
        match data[pos + 1]? with
        | none =>
          throw s!"truncated patch column at offset {colOff}"
        | some length =>
          let y0 := topdelta.toNat
          let mut row : Nat := 0
          while row < length.toNat do
            match data[pos + 3 + row]? with
            | some pix =>
              let y := y0 + row
              if y < patchHeight then
                out := out.push (y, pix)
            | none => pure ()
            row := row + 1
          pos := pos + 4 + length.toNat
  pure out

def loadPatch (wad : WadDirectory) (lumpIdx : Nat) : Except String (PatchHeader × ByteArray) := do
  let data ← lumpData wad lumpIdx
  let hdr ← parseHeader data
  pure (hdr, data)

def loadPatchByName (wad : WadDirectory) (name : String) : Except String (PatchHeader × ByteArray) := do
  match checkNumForName wad name with
  | none => throw s!"missing patch lump {name}"
  | some idx => loadPatch wad idx

end Doom.Render.Gfx.Patch
