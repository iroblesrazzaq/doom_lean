import Doom.Playsim.GameState
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Draw
import Doom.Render.Dump
import Doom.Render.Hud
import Doom.Render.Main
import Doom.Render.Palette
import Doom.Render.StatusBar
import Doom.Render.Types
import Doom.Render.View
import Doom.Wad

/-!
# Doom.Render.Render

Top-level frame orchestration (`d_main.c` `D_Display` for `GS_LEVEL`).
-/

namespace Doom.Render.Render

open Doom.Playsim.GameState
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Draw
open Doom.Render.Dump
open Doom.Render.Main
open Doom.Render.Palette
open Doom.Render.Types
open Doom.Render.View
open Doom.Wad

/--
Render one level display frame into an indexed buffer.
`R_ExecuteSetViewSize` then `ST_Drawer` → `R_RenderPlayerView`
→ `R_FillBackScreen` / `R_DrawViewBorder` → `HU_Drawer`.
Lean rebuilds the bezel every frame, so HUD text is painted after the bezel
(C only redraws the border some frames).
`fuzzpos` is C `r_draw.c` static state and must persist across displayed frames.
-/
def renderLevelFrameWithFuzz (wad : WadDirectory) (gs : GameState) (fuzzpos : Nat) :
    Except String (Framebuffer × ByteArray × Nat) := do
  let data ← initData wad
  let pal ← playpalSlice data.playpal (Doom.Render.StatusBar.doPaletteStuff gs)
  let fb0 := Framebuffer.initBlack
  let pending := setViewSize (Int32.ofNat screenBlocksDefault) 0
  let (vs, _) := executeSetViewSize pending
  let fullscreen := vs.viewheight == (200 : Int32)
  let fb1 ← Doom.Render.StatusBar.drawer wad fb0 gs fullscreen true
  let (fb2, fp) ← renderPlayerView data wad gs fb1 vs fuzzpos
  let bg? ← fillBackScreen wad vs.scaledviewwidth vs.viewheight vs.viewwindowx vs.viewwindowy
    gs.commercial
  let fb3 := drawViewBorder fb2 bg? vs.scaledviewwidth vs.viewheight
  let fb4 ← Doom.Render.Hud.drawer wad gs fb3
  pure (fb4, pal, fp)

def renderLevelFrame (wad : WadDirectory) (gs : GameState) : Except String (Framebuffer × ByteArray) := do
  let (fb, pal, _) ← renderLevelFrameWithFuzz wad gs 0
  pure (fb, pal)

def renderAndDump (wad : WadDirectory) (gs : GameState) (dir : System.FilePath) (gametic : Nat)
    (fuzzpos : Nat := 0) : IO (Except String Nat) := do
  match renderLevelFrameWithFuzz wad gs fuzzpos with
  | Except.error e => pure (Except.error e)
  | Except.ok (fb, pal, fp) => do
    dumpFrame dir gametic fb pal
    pure (Except.ok fp)

end Doom.Render.Render
