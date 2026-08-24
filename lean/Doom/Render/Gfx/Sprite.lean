import Doom.Render.Gfx.Patch
import Doom.Wad

/-!
# Doom.Render.Gfx.Sprite

Sprite lump metrics (`r_data.c` `R_InitSpriteLumps`).
-/

namespace Doom.Render.Gfx.Sprite

open Doom.Render.Gfx.Patch
open Doom.Wad

structure SpriteMetrics where
  width : Int32
  offset : Int32
  topOffset : Int32
  deriving Repr

def initSprites (wad : WadDirectory) : Except String (Nat × Array SpriteMetrics) := do
  match checkNumForName wad "S_START", checkNumForName wad "S_END" with
  | some s, some e =>
    let first := s + 1
    let count := e - first
    let mut metrics : Array SpriteMetrics := #[]
    let mut i : Nat := 0
    while i < count do
      let (hdr, _) ← loadPatch wad (first + i)
      metrics := metrics.push {
        width := (hdr.width.toUInt32 <<< 16).toInt32
        offset := (hdr.leftOffset.toUInt32 <<< 16).toInt32
        topOffset := (hdr.topOffset.toUInt32 <<< 16).toInt32
      }
      i := i + 1
    pure (first, metrics)
  | _, _ => throw "missing S_START/S_END"

end Doom.Render.Gfx.Sprite
