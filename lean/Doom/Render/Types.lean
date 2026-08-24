/-!
# Doom.Render.Types

Screen dimensions and indexed framebuffer types (`i_video.h`).
-/

namespace Doom.Render.Types

def screenWidth : Nat := 320
def screenHeight : Nat := 200
def screenPixels : Nat := screenWidth * screenHeight
def sbarHeight : Nat := 32

/-- Indexed 8-bit framebuffer (row-major, top-left first). -/
structure Framebuffer where
  pixels : ByteArray

def Framebuffer.empty : Framebuffer :=
  { pixels := ByteArray.emptyWithCapacity screenPixels }

def Framebuffer.initBlack : Framebuffer :=
  { pixels := ByteArray.mk (Array.replicate screenPixels (UInt8.ofNat 0)) }

def Framebuffer.get (fb : Framebuffer) (x y : Nat) : UInt8 :=
  let idx := y * screenWidth + x
  match fb.pixels[idx]? with
  | some b => b
  | none => 0

def Framebuffer.set (fb : Framebuffer) (x y : Nat) (v : UInt8) : Framebuffer :=
  let idx := y * screenWidth + x
  if h : idx < fb.pixels.size then
    { pixels := fb.pixels.set idx v }
  else
    fb

def Framebuffer.setIdx (fb : Framebuffer) (idx : Nat) (v : UInt8) : Framebuffer :=
  if h : idx < fb.pixels.size then
    { pixels := fb.pixels.set idx v }
  else
    fb

end Doom.Render.Types
