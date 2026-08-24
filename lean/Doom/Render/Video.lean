import Doom.Render.Gfx.Patch
import Doom.Render.Types

/-!
# Doom.Render.Video

Screen buffer operations (`v_video.c` `V_DrawPatch`).
-/

namespace Doom.Render.Video

open Doom.Render.Gfx.Patch
open Doom.Render.Types
open Doom.Wad

/-- Draw patch at screen (x,y) into framebuffer; leftover/topoffset are signed 16-bit. -/
def drawPatch (wad : WadDirectory) (fb : Framebuffer) (lumpIdx : Nat) (x y : Int32) :
    Except String Framebuffer := do
  let (hdr, data) ← loadPatch wad lumpIdx
  let x0 := x - hdr.leftOffset.toInt16.toInt32
  let y0 := y - hdr.topOffset.toInt16.toInt32
  let mut out := fb
  let mut col : Nat := 0
  while col < hdr.width.toNat do
    let colOff ← columnOffset data hdr col
    let pixels ← decodeColumnPixels data colOff hdr.height.toNat
    for (py, pix) in pixels do
      let sx := x0 + Int32.ofNat col
      let sy := y0 + Int32.ofNat py
      if sx >= 0 && sy >= 0 then
        let ux := sx.toNatClampNeg
        let uy := sy.toNatClampNeg
        if ux < screenWidth && uy < screenHeight then
          out := out.set ux uy pix
    col := col + 1
  pure out

def fillRect (fb : Framebuffer) (x y w h : Nat) (color : UInt8) : Framebuffer :=
  Id.run do
    let mut out := fb
    let mut row : Nat := 0
    while row < h do
      let mut col : Nat := 0
      while col < w do
        let sx := x + col
        let sy := y + row
        if sx < screenWidth && sy < screenHeight then
          out := out.set sx sy color
        col := col + 1
      row := row + 1
    pure out

end Doom.Render.Video
