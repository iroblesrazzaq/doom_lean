import Doom.Playsim.Info
import Doom.Render.Gfx.Sprite
import Doom.Render.Gfx.Texture
import Doom.Render.Palette
import Doom.Render.Things.Init
import Doom.Wad

/-!
# Doom.Render.Data

Renderer lump tables (`r_data.c` `R_InitData`).
-/

namespace Doom.Render.Data

open Doom.Render.Gfx.Sprite
open Doom.Render.Gfx.Texture
open Doom.Render.Palette
open Doom.Render.Things
open Doom.Wad

structure RenderData where
  playpal : ByteArray
  colormaps : ByteArray
  textureTables : TextureTables
  firstSprite : Nat
  spriteMetrics : Array SpriteMetrics
  sprites : Array SpriteDef := #[]
  negonearray : Array Int32 := mkNegonearray
  screenheightarray : Array Int32 := mkScreenheightarray 168

def loadColormaps (wad : WadDirectory) : Except String ByteArray := do
  match checkNumForName wad "COLORMAP" with
  | none => throw "missing COLORMAP"
  | some idx => lumpData wad idx

def initData (wad : WadDirectory) : Except String RenderData := do
  let playpal ← loadPlaypal wad
  let colormaps ← loadColormaps wad
  let textureTables ← initTextures wad
  let (firstSprite, spriteMetrics) ← Doom.Render.Gfx.Sprite.initSprites wad
  let lastSprite := firstSprite + spriteMetrics.size - 1
  let (sprites, negonearray) ←
    Doom.Render.Things.initSprites wad Doom.Playsim.Info.sprnames firstSprite lastSprite
  pure { playpal, colormaps, textureTables, firstSprite, spriteMetrics, sprites, negonearray }

end Doom.Render.Data
