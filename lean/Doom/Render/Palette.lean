import Doom.Wad

/-!
# Doom.Render.Palette

Load raw 768-byte `PLAYPAL` and `COLORMAP` lumps.
-/

namespace Doom.Render.Palette

open Doom.Wad

def playpalName : String := "PLAYPAL"
def playpalSize : Nat := 768
/-- Vanilla PLAYPAL slices 0–13 (`ST_doPaletteStuff` / `RADIATIONPAL`). -/
def playpalSliceCount : Nat := 14
def playpalLumpSize : Nat := playpalSliceCount * playpalSize
def colormapSize : Nat := 256 * 32
def colormapLevels : Nat := 32
def colormapLevelSize : Nat := 256

/-- Full PLAYPAL lump: 14 × 768 bytes (not just slice 0). -/
def loadPlaypal (wad : WadDirectory) : Except String ByteArray := do
  match checkNumForName wad playpalName with
  | none => throw s!"missing lump {playpalName}"
  | some idx =>
    let bytes ← lumpData wad idx
    if bytes.size < playpalLumpSize then
      throw s!"{playpalName} size {bytes.size}, expected {playpalLumpSize}"
    pure (bytes.extract 0 playpalLumpSize)

/-- One 768-byte PLAYPAL slice (`palette * 768`). -/
def playpalSlice (lump : ByteArray) (idx : Nat) : Except String ByteArray := do
  if idx >= playpalSliceCount then
    throw s!"PLAYPAL slice {idx} out of range (max {playpalSliceCount - 1})"
  else
    let off := idx * playpalSize
    if lump.size < off + playpalSize then
      throw s!"PLAYPAL truncated at slice {idx}"
    pure (lump.extract off (off + playpalSize))

def loadColormap (wad : WadDirectory) : Except String ByteArray := do
  match checkNumForName wad "COLORMAP" with
  | none => throw "missing lump COLORMAP"
  | some idx =>
    let bytes ← lumpData wad idx
    if bytes.size < colormapSize then
      throw s!"COLORMAP size {bytes.size}, expected {colormapSize}"
    pure bytes

/-- One 256-byte lighting slice from `COLORMAP` (`level * 256`, `level < 32`). -/
def colormapSlice (colormaps : ByteArray) (level : Nat) : Except String ByteArray := do
  if level >= colormapLevels then
    throw s!"colormap level {level} out of range (max {colormapLevels - 1})"
  else
    let off := level * colormapLevelSize
    if colormaps.size < off + colormapLevelSize then
      throw s!"COLORMAP truncated at level {level}"
    pure (colormaps.extract off (off + colormapLevelSize))

def indexToRgb (pal : ByteArray) (idx : UInt8) : (UInt8 × UInt8 × UInt8) :=
  let i := idx.toNat * 3
  match pal[i]?, pal[i + 1]?, pal[i + 2]? with
  | some r, some g, some b => (r, g, b)
  | _, _, _ => (0, 0, 0)

end Doom.Render.Palette
