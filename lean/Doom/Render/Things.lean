import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Render.Bsp
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Draw
import Doom.Render.Gfx.Patch
import Doom.Render.Gfx.Sprite
import Doom.Render.Seg
import Doom.Render.Things.Add
import Doom.Render.Things.Init
import Doom.Render.Types
import Doom.Render.Util
import Doom.Render.View
import Doom.Wad

/-!
# Doom.Render.Things

Sprite defs (`r_things.c` `R_InitSprites`), sprite collection (`R_AddSprites` /
`R_ProjectSprite`), and `R_DrawMasked` / `R_DrawSprite` / `R_DrawVisSprite`.
-/

namespace Doom.Render.Things

open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Render.Bsp
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Draw
open Doom.Render.Gfx.Patch
open Doom.Render.Gfx.Sprite
open Doom.Render.Seg
open Doom.Render.Types
open Doom.Render.Util
open Doom.Render.View
open Doom.Wad

/-- C selection-sort: strict `<` on scale; ties keep the earlier unsorted entry. -/
def sortVisSprites (sprites : Array VisSprite) : Array VisSprite :=
  Id.run do
    let n := sprites.size
    let mut used := Array.replicate n false
    let mut out : Array VisSprite := #[]
    let mut k := 0
    while k < n do
      let mut bestIdx : Option Nat := none
      let mut bestScale : Int32 := 0
      let mut i := 0
      while i < n do
        if used.getD i false then
          pure ()
        else
          let sc := (sprites.getD i {}).scale
          match bestIdx with
          | none =>
            bestIdx := some i
            bestScale := sc
          | some _ =>
            if sc < bestScale then
              bestIdx := some i
              bestScale := sc
        i := i + 1
      match bestIdx with
      | none => k := n
      | some idx =>
        used := Doom.Render.Util.arrSet used idx true
        out := out.push (sprites.getD idx {})
        k := k + 1
    pure out

private def initSpriteClips (x1 x2 : Int32) : Array Int32 × Array Int32 :=
  Id.run do
    let mut bot := Array.replicate Types.screenWidth (0 : Int32)
    let mut top := Array.replicate Types.screenWidth (0 : Int32)
    let mut x := x1
    while x <= x2 do
      let i := i32ToNat x
      bot := arrSetI32 bot i (-2)
      top := arrSetI32 top i (-2)
      x := x + 1
    pure (bot, top)

private def applySilClip (dst src : Array Int32) (r1 r2 : Int32) : Array Int32 :=
  Id.run do
    let mut out := dst
    let mut x := r1
    while x <= r2 do
      let i := i32ToNat x
      if out.getD i 0 == (-2 : Int32) then
        out := arrSetI32 out i (src.getD i 0)
      x := x + 1
    pure out

private def fillUnclipped (bot top : Array Int32) (x1 x2 : Int32)
    (viewheight : Int32 := Seg.defaultViewheight) : Array Int32 × Array Int32 :=
  Id.run do
    let mut clipbot := bot
    let mut cliptop := top
    let mut x := x1
    while x <= x2 do
      let i := i32ToNat x
      if clipbot.getD i 0 == (-2 : Int32) then
        clipbot := arrSetI32 clipbot i viewheight
      if cliptop.getD i 0 == (-2 : Int32) then
        cliptop := arrSetI32 cliptop i (-1)
      x := x + 1
    pure (clipbot, cliptop)

/-- `R_DrawVisSprite` (`r_things.c`). NULL colormap is `fuzzcolfunc`; translation is loud. -/
def drawVisSpriteEx (data : RenderData) (wad : WadDirectory) (vis : VisSprite)
    (mfloorclip mceilingclip : Array Int32) (fb : Framebuffer)
    (centery : Int32 := Seg.defaultCentery)
    (viewwindowx : Int32 := 0) (viewwindowy : Int32 := 0)
    (viewheight : Int32 := Seg.defaultViewheight) (fuzzpos : Nat := 0) :
    Except String (Framebuffer × Nat) := do
  let cmap? := vis.colormap
  if cmap?.isSome && (vis.mobjflags &&& MF_TRANSLATION) != 0 then
    throw "R_DrawVisSprite: MF_TRANSLATION not implemented"
  let lumpIdx := data.firstSprite + i32ToNat vis.patch
  let (hdr, patchData) ← loadPatch wad lumpIdx
  let centeryfrac := centery <<< 16
  let sprtopscreen := centeryfrac - fixedMul vis.texturemid vis.scale
  let dcIscale := cabs vis.xiscale
  let mut frac := vis.startfrac
  let mut x := vis.x1
  let mut out := fb
  let mut fp := fuzzpos
  while x <= vis.x2 do
    let texturecolumn := frac >>> 16
    if texturecolumn < 0 || texturecolumn >= Int32.ofNat hdr.width.toNat then
      throw "R_DrawSpriteRange: bad texturecolumn"
    let colOff ← columnOffset patchData hdr (i32ToNat texturecolumn)
    let posts := patchData.extract colOff patchData.size
    let colParams : MaskedColumnDrawParams := {
      x := x
      sprtopscreen := sprtopscreen
      spryscale := vis.scale
      texturemid := vis.texturemid
      posts := posts
      mceilingclip := mceilingclip
      mfloorclip := mfloorclip
      colormapLevel := cmap?.getD 0
      centery := centery
      iscaleOverride := some dcIscale
      viewwindowx := viewwindowx
      viewwindowy := viewwindowy
    }
    match cmap? with
    | none =>
      let (out', fp') ← drawMaskedFuzzColumn data out colParams viewheight fp
      out := out'
      fp := fp'
    | some _ =>
      out ← drawMaskedColumn data out colParams
    frac := frac + vis.xiscale
    x := x + 1
  pure (out, fp)

def drawVisSprite (data : RenderData) (wad : WadDirectory) (vis : VisSprite)
    (mfloorclip mceilingclip : Array Int32) (fb : Framebuffer)
    (centery : Int32 := Seg.defaultCentery)
    (viewwindowx : Int32 := 0) (viewwindowy : Int32 := 0) :
    Except String Framebuffer := do
  let (fb', _) ←
    drawVisSpriteEx data wad vis mfloorclip mceilingclip fb centery viewwindowx viewwindowy
  pure fb'

/-- `R_DrawSprite` (`r_things.c`): reverse-walk drawsegs, clip, then `R_DrawVisSprite`. -/
def drawSpriteEx (data : RenderData) (wad : WadDirectory) (spr : VisSprite)
    (drawSegs : Array DrawSegRecord) (fb : Framebuffer)
    (vs : ViewSize := {}) (fuzzpos : Nat := 0) :
    Except String (Array DrawSegRecord × Framebuffer × Nat) := do
  let (clipbot0, cliptop0) := initSpriteClips spr.x1 spr.x2
  let mut clipbot := clipbot0
  let mut cliptop := cliptop0
  let mut segs := drawSegs
  let mut out := fb
  let mut i := segs.size
  while i > 0 do
    i := i - 1
    match segs[i]? with
    | none => pure ()
    | some rec =>
      let dsx1 := rec.replayMeta.x1
      let dsx2 := rec.replayMeta.x2
      if dsx1 > spr.x2 || dsx2 < spr.x1 ||
          (rec.storeRes.silhouette == 0 && !rec.storeRes.maskedtexture) then
        pure ()
      else
        let r1 := if dsx1 < spr.x1 then spr.x1 else dsx1
        let r2 := if dsx2 > spr.x2 then spr.x2 else dsx2
        let scale1 := rec.storeRes.rwScale
        let scale2 := rec.storeRes.rwScale + (dsx2 - dsx1) * rec.storeRes.rwScaleStep
        let (lowscale, scale) :=
          if scale1 > scale2 then (scale2, scale1) else (scale1, scale2)
        let behind :=
          scale < spr.scale ||
            (lowscale < spr.scale &&
              !pointOnSegSide spr.gx spr.gy rec.replayMeta.v1x rec.replayMeta.v1y
                rec.replayMeta.v2x rec.replayMeta.v2y)
        if behind then
          if rec.storeRes.maskedtexture then
            let inp := maskedSegReplayInput rec r1 r2
            let (res', fb') ← renderMaskedSegRange data wad inp out
            segs := Doom.Render.Util.arrSet segs i { rec with storeRes := res' }
            out := fb'
          pure ()
        else
          let mut sil := rec.storeRes.silhouette
          if spr.gz >= rec.storeRes.bsilheight then
            sil := sil - (sil &&& silBottom)
          if spr.gzt <= rec.storeRes.tsilheight then
            sil := sil - (sil &&& silTop)
          if sil == silBottom then
            match rec.storeRes.sprbottomclip with
            | some src => clipbot := applySilClip clipbot src r1 r2
            | none => pure ()
          else if sil == silTop then
            match rec.storeRes.sprtopclip with
            | some src => cliptop := applySilClip cliptop src r1 r2
            | none => pure ()
          else if sil == silBottom + silTop then
            match rec.storeRes.sprbottomclip, rec.storeRes.sprtopclip with
            | some srcBot, some srcTop =>
              clipbot := applySilClip clipbot srcBot r1 r2
              cliptop := applySilClip cliptop srcTop r1 r2
            | some srcBot, none =>
              clipbot := applySilClip clipbot srcBot r1 r2
            | none, some srcTop =>
              cliptop := applySilClip cliptop srcTop r1 r2
            | none, none => pure ()
  let (clipbot', cliptop') := fillUnclipped clipbot cliptop spr.x1 spr.x2 vs.viewheight
  let (fb', fp') ←
    drawVisSpriteEx data wad spr clipbot' cliptop' out vs.centery vs.viewwindowx vs.viewwindowy
      vs.viewheight fuzzpos
  pure (segs, fb', fp')

def drawSprite (data : RenderData) (wad : WadDirectory) (spr : VisSprite)
    (drawSegs : Array DrawSegRecord) (fb : Framebuffer)
    (vs : ViewSize := {}) :
    Except String (Array DrawSegRecord × Framebuffer) := do
  let (segs, fb', _) ← drawSpriteEx data wad spr drawSegs fb vs 0
  pure (segs, fb')

/-- `BASEYCENTER` (`r_main.h`): `SCREENHEIGHT/2`. -/
private def baseYCenter : Int32 := Int32.ofNat (screenHeight / 2)

/-- `pw_invisibility` (`powertype_t`); renderer-local, playsim tables unused. -/
private def pwInvisibility : Nat := 2

private def emptyPspMetrics : SpriteMetrics :=
  { width := 0, offset := 0, topOffset := 0 }

/-- `R_DrawPSprite` (`r_things.c`). `psp.state = 0` is C NULL and is skipped by the caller. -/
def drawPSprite (data : RenderData) (wad : WadDirectory) (psp : Psprite)
    (powers : Array Int32) (spritelights : Array Nat)
    (mfloorclip mceilingclip : Array Int32) (fb : Framebuffer)
    (vs : ViewSize := {}) (fuzzpos : Nat := 0) :
    Except String (Framebuffer × Nat) := do
  let st ← match states[psp.state.toNat]? with
    | some s => pure s
    | none => throw s!"R_DrawPSprite: invalid state {psp.state.toNat}"
  if st.sprite.toNat >= data.sprites.size then
    throw s!"R_ProjectSprite: invalid sprite number {st.sprite.toNat} "
  let sprdef := data.sprites.getD st.sprite.toNat { numframes := 0, spriteframes := #[] }
  let frame := st.frame &&& ffFrameMask
  if frame.toNat >= sprdef.numframes then
    throw s!"R_ProjectSprite: invalid sprite frame {st.sprite.toNat} : {st.frame.toNat} "
  let sprframe :=
    sprdef.spriteframes.getD frame.toNat
      { rotate := false, lump := Array.replicate 8 (0 : Int32), flip := Array.replicate 8 (0 : UInt8) }
  let lump := sprframe.lump.getD 0 0
  let flip := sprframe.flip.getD 0 0 != 0
  let metrics := data.spriteMetrics.getD lump.toNatClampNeg emptyPspMetrics
  let viewwidth := vs.viewwidth
  let centerxfrac := vs.centerxfrac
  let pspriteScale := vs.pspritescale
  let pspriteIscale := vs.pspriteiscale
  let tx0 := psp.sx - (Int32.ofNat screenWidth / 2) * FRACUNIT - metrics.offset
  let x1 := (centerxfrac + fixedMul tx0 pspriteScale) >>> 16
  if x1 > viewwidth then
    return (fb, fuzzpos)
  let tx1 := tx0 + metrics.width
  let x2 := ((centerxfrac + fixedMul tx1 pspriteScale) >>> 16) - 1
  if x2 < 0 then
    return (fb, fuzzpos)
  let x1clip := if x1 < 0 then (0 : Int32) else x1
  let x2clip := if x2 >= viewwidth then viewwidth - 1 else x2
  let (start0, xiscale) :=
    if flip then
      (metrics.width - 1, -pspriteIscale)
    else
      ((0 : Int32), pspriteIscale)
  let startfrac :=
    if x1clip > x1 then
      start0 + xiscale * (x1clip - x1)
    else
      start0
  let inv := powers.getD pwInvisibility 0
  let colormap : Option Nat :=
    if inv > 4 * 32 || (inv &&& (8 : Int32)) != 0 then
      none
    else if (st.frame &&& ffFullbright) != 0 then
      some 0
    else
      some (spritelights.getD (maxLightScale - 1) 0)
  let vis : VisSprite := {
    mobjflags := 0
    scale := pspriteScale <<< Int32.ofNat vs.detailshift
    gx := 0, gy := 0, gz := 0, gzt := 0
    texturemid := (baseYCenter <<< 16) + FRACUNIT / 2 - (psp.sy - metrics.topOffset)
    x1 := x1clip
    x2 := x2clip
    startfrac
    xiscale
    patch := lump
    colormap
  }
  drawVisSpriteEx data wad vis mfloorclip mceilingclip fb vs.centery vs.viewwindowx vs.viewwindowy
    vs.viewheight fuzzpos

/-- `R_DrawPlayerSprites` (`r_things.c`). Unspawned `mo < 0` skips the pass. -/
def drawPlayerSprites (data : RenderData) (wad : WadDirectory) (gs : GameState)
    (fb : Framebuffer) (vs : ViewSize := {}) (fuzzpos : Nat := 0) :
    Except String (Framebuffer × Nat) := do
  match gs.players[gs.consoleplayer]? with
  | none =>
    pure (fb, fuzzpos)
  | some player =>
    if player.mo < 0 then
      pure (fb, fuzzpos)
    else do
      let mo ← match gs.mobjs[player.mo.toNatClampNeg]? with
        | some m => pure m
        | none => throw "R_DrawPlayerSprites: no view mobj"
      let ss ← match gs.level.subsectors[mo.subsector.toNat]? with
        | some s => pure s
        | none => throw "R_DrawPlayerSprites: bad subsector"
      let sec ← match gs.sectors[ss.sector.toNat]? with
        | some s => pure s
        | none => throw "R_DrawPlayerSprites: bad sector"
      let lights := spriteLightTable sec.lightlevel player.extralight
      let mut out := fb
      let mut fp := fuzzpos
      let mut i := 0
      while i < NUMPSPRITES do
        let psp := match player.psprites[i]? with
          | some p => p
          | none => Psprite.inactive
        if psp.state != 0 then
          let (out', fp') ← drawPSprite data wad psp player.powers lights
            data.screenheightarray data.negonearray out vs fp
          out := out'
          fp := fp'
        i := i + 1
      pure (out, fp)

/--
`R_DrawMasked` (`r_things.c` L951–978): sort visSprites back-to-front,
`R_DrawSprite` each, remaining masked mids, then console-player psprites.
Lean has no side views (`viewangleoffset` always 0).
-/
def drawMaskedEx (data : RenderData) (wad : WadDirectory) (gs : GameState)
    (fb : Framebuffer) (drawSegs : Array DrawSegRecord)
    (visSprites : Array VisSprite) (vs : ViewSize := {}) (fuzzpos : Nat := 0) :
    Except String (Framebuffer × Nat) := do
  let sorted := sortVisSprites visSprites
  let mut ds := { initBspDrawState data wad fb vs with drawSegs }
  let mut fp := fuzzpos
  let mut si := 0
  while si < sorted.size do
    let vis := sorted.getD si {}
    let (segs', fb', fp') ← drawSpriteEx data wad vis ds.drawSegs ds.fb vs fp
    ds := { ds with drawSegs := segs', fb := fb' }
    fp := fp'
    si := si + 1
  let ds' ← replayMaskedDrawSegs data wad ds
  drawPlayerSprites data wad gs ds'.fb vs fp

def drawMasked (data : RenderData) (wad : WadDirectory) (gs : GameState)
    (fb : Framebuffer) (drawSegs : Array DrawSegRecord)
    (visSprites : Array VisSprite) (vs : ViewSize := {}) : Except String Framebuffer := do
  let (fb', _) ← drawMaskedEx data wad gs fb drawSegs visSprites vs 0
  pure fb'

end Doom.Render.Things
