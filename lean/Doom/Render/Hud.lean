import Doom.Playsim.GameState
import Doom.Render.Gfx.Patch
import Doom.Render.Types
import Doom.Render.Video
import Doom.Wad

/-!
# Doom.Render.Hud

Heads-up display (`hu_stuff.c` `HU_Drawer` / `hu_lib.c` `HUlib_drawTextLine`).
Message path only: no chat, title, automap, or `HU_Erase` backing-screen.
-/

namespace Doom.Render.Hud

open Doom.Playsim.GameState
open Doom.Render.Gfx.Patch
open Doom.Render.Types
open Doom.Render.Video
open Doom.Wad

def HU_MSGX : Int32 := 0
def HU_MSGY : Int32 := 0
def HU_FONTSTART : Nat := 33
def HU_FONTEND : Nat := 95
def HU_SPACEWIDTH : Int32 := 4

private def pad3 (n : Nat) : String :=
  if n < 10 then s!"00{n}"
  else if n < 100 then s!"0{n}"
  else toString n

def stcfnName (code : Nat) : String := s!"STCFN{pad3 code}"

private def asciiToUpper (c : Char) : Char :=
  let n := c.toNat
  if n >= 97 && n <= 122 then Char.ofNat (n - 32) else c

/-- `HUlib_drawTextLine` without cursor. Space / `< '!'` / `> '_'` advance 4; clip at 320. -/
def drawTextLine (wad : WadDirectory) (fb0 : Framebuffer) (x0 y : Int32) (msg : String) :
    Except String Framebuffer := do
  let mut out := fb0
  let mut x := x0
  let mut stop := false
  for b in msg.toUTF8 do
    if !stop then
      let c := asciiToUpper (Char.ofNat b.toNat)
      let n := c.toNat
      if c != ' ' && n >= HU_FONTSTART && n <= HU_FONTEND then
        match checkNumForName wad (stcfnName n) with
        | none => throw s!"missing patch lump {stcfnName n}"
        | some idx =>
          let (hdr, _) ← loadPatch wad idx
          let w := hdr.width.toUInt32.toInt32
          if x + w > Int32.ofNat screenWidth then
            stop := true
          else
            out ← drawPatch wad out idx x y
            x := x + w
      else
        x := x + HU_SPACEWIDTH
        if x >= Int32.ofNat screenWidth then
          stop := true
  pure out

/-- At gametic 35 with no chat/message, HUD draw is a no-op. -/
def erase (fb : Framebuffer) : Framebuffer := fb

/-- `HU_Drawer` message widget only. Needs WAD + HU state. -/
def drawer (wad : WadDirectory) (gs : GameState) (fb : Framebuffer) :
    Except String Framebuffer := do
  if !gs.hu.messageOn then
    return fb
  drawTextLine wad fb HU_MSGX HU_MSGY gs.hu.messageText

end Doom.Render.Hud
