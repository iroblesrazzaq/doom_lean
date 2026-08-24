import Doom.Harness.Fnv
import Doom.Render.Palette
import Doom.Render.Types

/-!
# Doom.Render.Dump

PPM + FNV framebuffer output (`otrace.c` `OTrace_DumpFramebuffer`, TRACE.md §9).
-/

namespace Doom.Render.Dump

open Doom.Harness.Fnv
open Doom.Render.Palette
open Doom.Render.Types

def ppmHeader : String := "P6\n320 200\n255\n"

def writePpm (path : System.FilePath) (fb : Framebuffer) (pal : ByteArray) : IO Unit := do
  let h := ppmHeader.toUTF8
  let mut out := ByteArray.emptyWithCapacity (h.size + screenPixels * 3)
  out := out.append h
  let mut i : Nat := 0
  while i < screenPixels do
    let idx := match fb.pixels[i]? with | some b => b | none => 0
    let (r, g, b) := indexToRgb pal idx
    out := out.push r |>.push g |>.push b
    i := i + 1
  IO.FS.writeBinFile path out

private def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)

private def u64Hex16 (h : UInt64) : String :=
  String.ofList <| List.range 16 |>.map fun i =>
    hexDigit (((h >>> (UInt64.ofNat ((15 - i) * 4))) &&& 0xf).toNat)

#guard u64Hex16 0xee13be115dc0314e == "ee13be115dc0314e"
#guard u64Hex16 0 == "0000000000000000"
#guard u64Hex16 0xf == "000000000000000f"

def writeFnv (path : System.FilePath) (fb : Framebuffer) (pal : ByteArray) : IO Unit := do
  let mut data := ByteArray.emptyWithCapacity (screenPixels + playpalSize)
  data := data.append fb.pixels
  data := data.append pal
  let h := fnv1a64 data
  let line := u64Hex16 h ++ "\n"
  IO.FS.writeFile path line

def dumpFrame (dir : System.FilePath) (gametic : Nat) (fb : Framebuffer) (pal : ByteArray) :
    IO Unit := do
  IO.FS.createDirAll dir
  writePpm (dir / s!"fb_{gametic}.ppm") fb pal
  writeFnv (dir / s!"fb_{gametic}.fnv") fb pal

end Doom.Render.Dump
