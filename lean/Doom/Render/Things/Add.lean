import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Mobj
import Doom.Playsim.Tables
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Gfx.Sprite
import Doom.Render.Tables
import Doom.Render.Things.Init
import Doom.Render.Util

/-!
# Doom.Render.Things.Add

Sprite collection (`r_things.c` `R_AddSprites` / `R_ProjectSprite` / `R_NewVisSprite`).
-/

namespace Doom.Render.Things

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Mobj
open Doom.Playsim.Tables
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Gfx.Sprite
open Doom.Render.Tables
open Doom.Render.Util

/-- `vissprite_t`. `colormap = none` is C NULL (shadow). Fullbright is index 0. -/
structure VisSprite where
  mobjflags : UInt32 := 0
  scale : Int32 := 0
  gx : Int32 := 0
  gy : Int32 := 0
  gz : Int32 := 0
  gzt : Int32 := 0
  texturemid : Int32 := 0
  x1 : Int32 := 0
  x2 : Int32 := 0
  startfrac : Int32 := 0
  xiscale : Int32 := 0
  patch : Int32 := 0
  colormap : Option Nat := none
  deriving BEq, Repr

/-- Renderer-local sprite overlay. Playsim `validcount` is never read or written. -/
structure SpriteCollectState where
  visSprites : Array VisSprite := #[]
  overflowSprite : Option VisSprite := none
  validcount : Int32 := 1
  sectorStamp : Array Int32 := #[]
  spritelights : Array Nat := #[]
  sectors : Array SectorRuntime := #[]
  mobjs : Array Mobj := #[]
  extralight : Int32 := 0
  fixedcolormap : Option Nat := none
  viewwidth : Int32 := 320
  centerxfrac : Int32 := 160 * FRACUNIT
  projection : Int32 := 160 * FRACUNIT
  viewcos : Int32 := 0
  viewsin : Int32 := 0
  viewx : Int32 := 0
  viewy : Int32 := 0
  viewz : Int32 := 0

private def emptyMetrics : SpriteMetrics :=
  { width := 0, offset := 0, topOffset := 0 }

/-- `R_ClearSprites`. -/
def clearSprites (st : SpriteCollectState) : SpriteCollectState :=
  { st with visSprites := #[], overflowSprite := none }

/-- Frame overlay: local stamp 1, empty vissprite list, viewcos/viewsin from angle. -/
def initSpriteCollect (sectors : Array SectorRuntime) (mobjs : Array Mobj)
    (viewx viewy viewz : Int32) (viewangle : UInt32) (extralight : Int32)
    (fixedcolormap : Option Nat := none)
    (viewwidth : Int32 := 320) (centerxfrac : Int32 := 160 * FRACUNIT)
    (projection : Int32 := 160 * FRACUNIT) : SpriteCollectState :=
  let angIdx := (viewangle >>> ANGLETOFINESHIFT.toUInt32).toNat
  {
    visSprites := #[]
    overflowSprite := none
    validcount := 1
    sectorStamp := Array.replicate sectors.size (0 : Int32)
    spritelights := #[]
    sectors
    mobjs
    extralight
    fixedcolormap
    viewwidth
    centerxfrac
    projection
    viewcos := finecosine.getD angIdx 0
    viewsin := finesine.getD angIdx 0
    viewx
    viewy
    viewz
  }

/-- `R_NewVisSprite`: 128 slots; the 129th overwrites `overflowsprite` and is not appended. -/
def newVisSprite (st : SpriteCollectState) (vis : VisSprite) : SpriteCollectState :=
  if st.visSprites.size >= maxVisSprites then
    { st with overflowSprite := some vis }
  else
    { st with visSprites := st.visSprites.push vis }

/-- `R_AddSprites` / `R_DrawPlayerSprites` lightnum clamp into `scalelight`. -/
def spriteLightTable (lightlevel extralight : Int32) : Array Nat :=
  let lightnum := (lightlevel >>> 4) + extralight
  if lightnum < 0 then
    scalelight.getD 0 #[]
  else if lightnum >= Int32.ofNat Tables.lightLevels then
    scalelight.getD (Tables.lightLevels - 1) #[]
  else
    scalelight.getD (i32ToNat lightnum) #[]

private def visColormap (st : SpriteCollectState) (thing : Mobj) (xscale : Int32) :
    Option Nat :=
  if (thing.flags &&& MF_SHADOW) != 0 then
    none
  else
    match st.fixedcolormap with
    | some cm => some cm
    | none =>
      if (thing.frame &&& ffFullbright) != 0 then
        some 0
      else
        let raw := i32ToNat (xscale >>> 12)
        let idx := if raw >= Constants.maxLightScale then Constants.maxLightScale - 1 else raw
        some (st.spritelights.getD idx 0)

/-- `R_ProjectSprite` — generate-or-reject one thing. -/
def projectSprite (data : RenderData) (st : SpriteCollectState) (thing : Mobj) :
    Except String SpriteCollectState := do
  let trX := thing.x - st.viewx
  let trY := thing.y - st.viewy
  let gxt0 := fixedMul trX st.viewcos
  let gyt0 := -fixedMul trY st.viewsin
  let tz := gxt0 - gyt0
  if tz < minZ then
    return st
  let xscale := fixedDiv st.projection tz
  let gxt1 := -fixedMul trX st.viewsin
  let gyt1 := fixedMul trY st.viewcos
  let tx0 := -(gyt1 + gxt1)
  if cabs tx0 > (tz <<< 2) then
    return st
  if thing.sprite.toNat >= data.sprites.size then
    throw s!"R_ProjectSprite: invalid sprite number {thing.sprite.toNat} "
  let sprdef := data.sprites.getD thing.sprite.toNat { numframes := 0, spriteframes := #[] }
  let frame := thing.frame &&& ffFrameMask
  if frame.toNat >= sprdef.numframes then
    throw s!"R_ProjectSprite: invalid sprite frame {thing.sprite.toNat} : {thing.frame.toNat} "
  let sprframe :=
    sprdef.spriteframes.getD frame.toNat
      { rotate := false, lump := Array.replicate 8 (0 : Int32), flip := Array.replicate 8 (0 : UInt8) }
  let (lump, flip) :=
    if sprframe.rotate then
      let ang := pointToAngle2 st.viewx st.viewy thing.x thing.y
      let rot := ((ang - thing.angle) + (ANG45 / 2) * 9) >>> 29
      (sprframe.lump.getD rot.toNat 0, sprframe.flip.getD rot.toNat 0 != 0)
    else
      (sprframe.lump.getD 0 0, sprframe.flip.getD 0 0 != 0)
  let metrics := data.spriteMetrics.getD lump.toNatClampNeg emptyMetrics
  let tx1 := tx0 - metrics.offset
  let x1 := (st.centerxfrac + fixedMul tx1 xscale) >>> 16
  if x1 > st.viewwidth then
    return st
  let tx2 := tx1 + metrics.width
  let x2 := ((st.centerxfrac + fixedMul tx2 xscale) >>> 16) - 1
  if x2 < 0 then
    return st
  let x1clip := if x1 < 0 then (0 : Int32) else x1
  let x2clip := if x2 >= st.viewwidth then st.viewwidth - 1 else x2
  let iscale := fixedDiv FRACUNIT xscale
  let (start0, xiscale) :=
    if flip then
      (metrics.width - 1, -iscale)
    else
      ((0 : Int32), iscale)
  let startfrac :=
    if x1clip > x1 then
      start0 + xiscale * (x1clip - x1)
    else
      start0
  let vis : VisSprite := {
    mobjflags := thing.flags
    scale := xscale
    gx := thing.x
    gy := thing.y
    gz := thing.z
    gzt := thing.z + metrics.topOffset
    texturemid := thing.z + metrics.topOffset - st.viewz
    x1 := x1clip
    x2 := x2clip
    startfrac
    xiscale
    patch := lump
    colormap := visColormap st thing xscale
  }
  pure (newVisSprite st vis)

private def projectThingList (data : RenderData) (st : SpriteCollectState) (idx : Int32)
    (fuel : Nat) : Except String SpriteCollectState := do
  match fuel with
  | 0 => throw "R_AddSprites: thinglist cycle"
  | fuel' + 1 =>
    if idx < 0 then
      pure st
    else
      match st.mobjs[idx.toNatClampNeg]? with
      | none => throw "R_AddSprites: thinglist cycle"
      | some thing =>
        let st' ← projectSprite data st thing
        projectThingList data st' thing.snext fuel'

/-- `R_AddSprites`. Empty overlay sectors skip. Renderer-local stamp, not playsim validcount. -/
def addSprites (data : RenderData) (st : SpriteCollectState) (secIdx : Nat) :
    Except String SpriteCollectState := do
  if st.sectors.isEmpty then
    return st
  match st.sectors[secIdx]? with
  | none => pure st
  | some sec =>
    if st.sectorStamp.getD secIdx 0 == st.validcount then
      pure st
    else
      let st1 := {
        st with
          sectorStamp := arrSetI32 st.sectorStamp secIdx st.validcount
          spritelights := spriteLightTable sec.lightlevel st.extralight
      }
      projectThingList data st1 sec.thinglist (st1.mobjs.size + 1)

end Doom.Render.Things
