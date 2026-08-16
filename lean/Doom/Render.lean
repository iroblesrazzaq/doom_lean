import Doom.Playsim.GameState
import Doom.Render.Data
import Doom.Render.Dump
import Doom.Render.Gfx.Flat
import Doom.Render.Gfx.Patch
import Doom.Render.Palette
import Doom.Render.Render
import Doom.Render.Types
import Doom.Render.Video
import Doom.Wad

namespace Doom.Render

open Doom.Playsim.GameState
open Doom.Render.Dump
open Doom.Render.Render
open Doom.Wad

/-- Dump framebuffer when `gametic` is listed in `fbTics` (`otrace.c` timing).
When `fbDir` is set, every display frame is rendered so `fuzzpos` matches C. -/
def dumpIfRequested (wad : WadDirectory) (gs : GameState)
    (fbDir : Option System.FilePath) (fbTics : Array Nat) (fuzzpos : Nat) :
    IO (Except String Nat) :=
  match fbDir with
  | none => pure (Except.ok fuzzpos)
  | some dir =>
    match renderLevelFrameWithFuzz wad gs fuzzpos with
    | Except.error e => pure (Except.error e)
    | Except.ok (fb, pal, fp) => do
      if fbTics.contains gs.gametic.toNat then
        dumpFrame dir gs.gametic.toNat fb pal
      pure (Except.ok fp)

end Doom.Render
