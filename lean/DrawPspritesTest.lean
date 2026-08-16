import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Gfx.Sprite
import Doom.Render.Gfx.Texture
import Doom.Render.Tables
import Doom.Render.Things
import Doom.Render.Things.Add
import Doom.Render.Things.Init
import Doom.Render.Types
import Doom.Render.Util
import Doom.Wad

/-!
# DrawPspritesTest

R1w-drawpsprites: `R_DrawPSprite` / `R_DrawPlayerSprites` / `R_DrawMasked` tail.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Gfx.Sprite
open Doom.Render.Gfx.Texture
open Doom.Render.Tables
open Doom.Render.Things
open Doom.Render.Types
open Doom.Render.Util
open Doom.Wad

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def expectError (name : String) (got : Except String Framebuffer) (want : String) :
    IO Bool := do
  match got with
  | Except.ok _ =>
    IO.eprintln s!"FAIL: {name} (expected error)"
    pure false
  | Except.error e =>
    if e == want then
      IO.println s!"PASS: {name}"
      pure true
    else
      IO.eprintln s!"FAIL: {name} (got {e})"
      pure false

private def F : Int32 := FRACUNIT

/-- `Info.states[5]`: sprite 2, frame 1 (not fullbright). -/
private def stWeapon : UInt32 := 5
/-- `Info.states[13]`: sprite 3, frame 0 (not fullbright). -/
private def stFlash : UInt32 := 13
/-- `Info.states[17]`: sprite 4, frame 32768 (`FF_FULLBRIGHT`). -/
private def stBright : UInt32 := 17

private def identityColormaps : ByteArray :=
  ByteArray.mk (Array.ofFn (n := 8192) (fun i => UInt8.ofNat (i % 256)))

/-- Each colormap level maps every index to that level number. -/
private def levelColormaps : ByteArray :=
  ByteArray.mk (Array.ofFn (n := 8192) (fun i => UInt8.ofNat (i / 256)))

private def mkSpritePatch (topdelta : UInt8) (pixels : Array UInt8) : ByteArray :=
  let len := UInt8.ofNat pixels.size
  ByteArray.mk (
    #[1, 0, 128, 0, 0, 0, 0, 0, 12, 0, 0, 0, topdelta, len, 0] ++ pixels ++ #[0, 0xff]
  )

private def u8 (n : Nat) : UInt8 := UInt8.ofNat n

private def i32le (n : Nat) : Array UInt8 :=
  #[u8 (n % 256), u8 ((n / 256) % 256), u8 ((n / 65536) % 256), u8 ((n / 16777216) % 256)]

/-- One-pixel-tall patch; column `i` has pixel `pixels[i]`. -/
private def mkWidePatch (pixels : Array UInt8) : ByteArray :=
  let w := pixels.size
  let header : Array UInt8 := #[u8 w, 0, 1, 0, 0, 0, 0, 0]
  let colStart := 8 + 4 * w
  Id.run do
    let mut ofs : Array UInt8 := #[]
    let mut cols : Array UInt8 := #[]
    let mut i := 0
    while i < w do
      ofs := ofs ++ i32le (colStart + 6 * i)
      cols := cols ++ #[0, 1, 0, pixels.getD i 0, 0, 0xff]
      i := i + 1
    pure (ByteArray.mk (header ++ ofs ++ cols))

private def spritePatchA : ByteArray :=
  mkSpritePatch 0 #[201, 202, 203, 204, 205]

private def spritePatchB : ByteArray :=
  mkSpritePatch 0 #[211, 212, 213, 214, 215]

private def widePixels : Array UInt8 :=
  #[200, 201, 202, 203, 204, 205, 206, 207, 208, 209]

private def spritePatchWide : ByteArray := mkWidePatch widePixels

private def combinedWad : WadDirectory :=
  let a := spritePatchA
  let b := spritePatchB
  let w := spritePatchWide
  let data := a ++ b ++ w
  {
    identification := ByteArray.mk #[73, 87, 65, 68], numlumps := 3, infotableofs := 0
    entries := #[
      { filepos := 0, size := UInt32.ofNat a.size, name := nameTo8 "SPRA" },
      { filepos := UInt32.ofNat a.size, size := UInt32.ofNat b.size, name := nameTo8 "SPRB" },
      { filepos := UInt32.ofNat (a.size + b.size), size := UInt32.ofNat w.size, name := nameTo8 "SPRW" }
    ]
    data
  }

private def synthTextureTables : TextureTables :=
  {
    firstFlat := 0, numFlats := 0, skyFlatNum := 0, numTextures := 1
    textures := #[{ name := nameTo8 "-", width := 1, height := 1, patches := #[] }]
    columnLump := #[#[(0 : Int32)]]
    columnOfs := #[#[0]]
    widthMask := #[0]
    height := #[1]
    compositeSize := #[0]
    composite := #[none]
    hashHead := #[none]
    hashNext := #[none]
  }

private def emptyFrame : SpriteFrame :=
  { rotate := false, lump := Array.replicate 8 (0 : Int32), flip := Array.replicate 8 (0 : UInt8) }

private def frameLump (lump : Int32) (flip : UInt8 := 0) : SpriteFrame :=
  { rotate := false, lump := Array.replicate 8 lump, flip := Array.replicate 8 flip }

private def padDef : SpriteDef := { numframes := 0, spriteframes := #[] }

private def paintSprites : Array SpriteDef :=
  #[
    padDef, padDef
    , { numframes := 2, spriteframes := #[emptyFrame, frameLump 0] }
    , { numframes := 1, spriteframes := #[frameLump 1] }
    , { numframes := 1, spriteframes := #[frameLump 0] }
  ]

private def mkMetrics (w off top : Int32) : SpriteMetrics :=
  { width := w, offset := off, topOffset := top }

private def paintMetrics : Array SpriteMetrics :=
  #[mkMetrics F 0 0, mkMetrics F 0 0, mkMetrics (10 * F) 0 0]

private def mkData (sprites : Array SpriteDef) (metrics : Array SpriteMetrics)
    (cmaps : ByteArray := identityColormaps) : RenderData :=
  {
    playpal := ByteArray.empty
    colormaps := cmaps
    textureTables := synthTextureTables
    firstSprite := 0
    spriteMetrics := metrics
    sprites
  }

private def pic (b : UInt8) : ByteArray := ByteArray.mk #[b, 0, 0, 0, 0, 0, 0, 0]
private def flatCeiling : ByteArray := ByteArray.mk (Array.replicate 8 (UInt8.ofNat 65))

private def mkSector (floor ceiling light : Int32) : Sector := {
  floorheight := floor, ceilingheight := ceiling
  floorpic := pic 1, ceilingpic := flatCeiling, lightlevel := light
  special := 0, tag := 0, lines := #[], blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def mkLevel (sectors : Array Sector) (subsectors : Array Subsector) : LevelData := {
  vertexes := #[], sectors, sides := #[], lines := #[], segs := #[], subsectors, nodes := #[]
  things := #[]
  blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
  reject := ByteArray.empty
}

private def dummyLevel : LevelData := mkLevel #[] #[]

private def emptyGs : GameState :=
  initFromLevel dummyLevel 2 #[true, false, false, false] 0

private def pspAt (state : UInt32) (sx sy : Int32) : Psprite :=
  { state, tics := 1, sx, sy }

/-- `texturemid = 0` when `sy = BASEYCENTER<<FRACBITS + FRACUNIT/2` and topoffset 0. -/
private def syCenter : Int32 := (100 : Int32) <<< 16 + F / 2

private def spawnedGs (light : Int32) (weapon flash : Psprite)
    (powers : Array Int32 := Array.replicate NUMPOWERS 0) (extralight : Int32 := 0) :
    GameState :=
  let level := mkLevel #[mkSector 0 (128 * F) light]
    #[{ numsegs := 0, firstseg := 0, sector := 0 }]
  let gs0 := initFromLevel level 2 #[true, false, false, false] 0
  let mo : Mobj := { Doom.Playsim.Mobj.empty with subsector := 0 }
  let pl : Player := {
    Doom.Playsim.Player.empty with
      mo := 0
      extralight
      powers
      psprites := #[weapon, flash]
  }
  { gs0 with mobjs := #[mo], players := Doom.Playsim.GameState.arrSet gs0.players 0 pl }

private def fbPixel (fb : Framebuffer) (x y : Nat) : UInt8 :=
  fb.get x y

private def columnStrip (fb : Framebuffer) (x y0 y1 : Nat) : List UInt8 :=
  (List.range (y1 - y0 + 1)).map (fun d => fbPixel fb x (y0 + d))

private def spriteStripA : List UInt8 :=
  [201, 202, 203, 204, 205].map UInt8.ofNat

private def spriteStripB : List UInt8 :=
  [211, 212, 213, 214, 215].map UInt8.ofNat

private def blackStrip : List UInt8 :=
  [0, 0, 0, 0, 0].map UInt8.ofNat

private def mkVis (x1 x2 scale patch : Int32) (cmap : Option Nat) : VisSprite :=
  {
    mobjflags := 0
    scale := scale
    gx := 0, gy := F, gz := 0, gzt := 64 * F
    texturemid := 0
    x1 := x1, x2 := x2
    startfrac := 0
    xiscale := F
    patch := patch
    colormap := cmap
  }

private def expectedPspVis (lump : Int32) (metrics : SpriteMetrics) (psp : Psprite)
    (flip : Bool) (cmap : Option Nat) : Option VisSprite :=
  let viewwidth : Int32 := 320
  let centerxfrac := 160 * F
  let tx0 := psp.sx - 160 * F - metrics.offset
  let x1 := (centerxfrac + fixedMul tx0 F) >>> 16
  if x1 > viewwidth then
    none
  else
    let tx1 := tx0 + metrics.width
    let x2 := ((centerxfrac + fixedMul tx1 F) >>> 16) - 1
    if x2 < 0 then
      none
    else
      let x1clip := if x1 < 0 then (0 : Int32) else x1
      let x2clip := if x2 >= viewwidth then viewwidth - 1 else x2
      let start0 := if flip then metrics.width - 1 else (0 : Int32)
      let xiscale := if flip then -F else F
      let startfrac := if x1clip > x1 then start0 + xiscale * (x1clip - x1) else start0
      some {
        mobjflags := 0
        scale := F
        gx := 0, gy := 0, gz := 0, gzt := 0
        texturemid := (100 : Int32) <<< 16 + F / 2 - (psp.sy - metrics.topOffset)
        x1 := x1clip, x2 := x2clip, startfrac, xiscale
        patch := lump
        colormap := cmap
      }

private def localCmap (light extralight : Int32) : Nat :=
  let lightnum := (light >>> 4) + extralight
  let row :=
    if lightnum < 0 then
      scalelight.getD 0 #[]
    else if lightnum >= Int32.ofNat Doom.Render.Tables.lightLevels then
      scalelight.getD (Doom.Render.Tables.lightLevels - 1) #[]
    else
      scalelight.getD (i32ToNat lightnum) #[]
  row.getD (Doom.Render.Constants.maxLightScale - 1) 0

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let wad := combinedWad
  let data := mkData paintSprites paintMetrics
  let fb0 := Framebuffer.initBlack
  let fbMark := Framebuffer.set fb0 0 0 77
  let weapon5 := pspAt stWeapon (5 * F) syCenter
  let flash5 := pspAt stFlash (5 * F) syCenter
  let inactive := Psprite.inactive
  let light128 : Int32 := 128
  let cmap128 := localCmap light128 0

  -- (1) empty / unspawned player: drawMasked is a no-op on the framebuffer
  match drawMasked data wad emptyGs fbMark #[] #[] with
  | Except.error e =>
    IO.eprintln s!"empty player: {e}"; ok := false
  | Except.ok fb =>
    ok := (← assert "unspawned player identity" (fb.pixels == fbMark.pixels)) && ok

  -- (2) spawned player, inactive psprites: light lookup then no paint
  let gsInact := spawnedGs light128 inactive inactive
  match drawMasked data wad gsInact fbMark #[] #[] with
  | Except.error e =>
    IO.eprintln s!"inactive psp: {e}"; ok := false
  | Except.ok fb =>
    ok := (← assert "inactive psp identity" (fb.pixels == fbMark.pixels)) && ok

  -- (3) weapon paints via drawVisSprite + screenheightarray / negonearray
  let clipBot := Array.replicate 320 (87 : Int32)
  let clipTop := Array.replicate 320 (84 : Int32)
  let dataClip := { data with screenheightarray := clipBot, negonearray := clipTop }
  let gsW := spawnedGs light128 weapon5 inactive
  match expectedPspVis 0 (mkMetrics F 0 0) weapon5 false (some cmap128) with
  | none =>
    ok := (← assert "weapon expected vis" false) && ok
  | some visW =>
    match drawVisSprite dataClip wad visW clipBot clipTop fbMark with
    | Except.error e =>
      IO.eprintln s!"weapon gold: {e}"; ok := false
    | Except.ok fbGold =>
      match drawMasked dataClip wad gsW fbMark #[] #[] with
      | Except.error e =>
        IO.eprintln s!"weapon drawMasked: {e}"; ok := false
      | Except.ok fbW =>
        ok := (← assert "weapon paints via drawVisSprite clips"
          (fbW.pixels == fbGold.pixels)) && ok
        ok := (← assert "weapon preserves unrelated"
          (fbPixel fbW 0 0 == 77)) && ok

  -- default clips: 5-pixel post at y=84
  match drawMasked data wad gsW fbMark #[] #[] with
  | Except.error e =>
    IO.eprintln s!"weapon default: {e}"; ok := false
  | Except.ok fbW =>
    ok := (← assert "weapon default strip"
      (columnStrip fbW 5 84 88 == spriteStripA)) && ok

  -- (4) flash overwrites weapon
  let gsBoth := spawnedGs light128 weapon5 flash5
  match drawMasked data wad gsBoth fb0 #[] #[] with
  | Except.error e =>
    IO.eprintln s!"flash over: {e}"; ok := false
  | Except.ok fbF =>
    ok := (← assert "flash overwrites weapon"
      (columnStrip fbF 5 84 88 == spriteStripB)) && ok

  -- (5) drawMasked tail paints over a world visSprite
  let visWorld := mkVis 5 5 F 0 (some 0)
  match drawMasked data wad gsW fb0 #[] #[visWorld] with
  | Except.error e =>
    IO.eprintln s!"over visSprite: {e}"; ok := false
  | Except.ok fbOver =>
    ok := (← assert "psprite paints over world visSprite"
      (columnStrip fbOver 5 84 88 == spriteStripA)) && ok

  -- (6) off-screen: no paint
  let gsRight := spawnedGs light128 (pspAt stWeapon (321 * F) syCenter) inactive
  match drawMasked data wad gsRight fbMark #[] #[] with
  | Except.error e =>
    IO.eprintln s!"off-right: {e}"; ok := false
  | Except.ok fbR =>
    ok := (← assert "off-right no paint" (fbR.pixels == fbMark.pixels)) && ok
  let gsLeft := spawnedGs light128 (pspAt stWeapon ((-10) * F) syCenter) inactive
  match drawMasked data wad gsLeft fbMark #[] #[] with
  | Except.error e =>
    IO.eprintln s!"off-left: {e}"; ok := false
  | Except.ok fbL =>
    ok := (← assert "off-left no paint" (fbL.pixels == fbMark.pixels)) && ok

  -- (7) invisibility → R_DrawFuzzColumn (NULL colormap)
  let inv129 := #[(0 : Int32), 0, 129, 0, 0, 0]
  let gsInv := spawnedGs light128 weapon5 inactive inv129
  match drawMasked data wad gsInv fb0 #[] #[] with
  | Except.error e =>
    ok := (← assert s!"invisibility > 4*32 fuzz: {e}" false) && ok
  | Except.ok _ =>
    ok := (← assert "invisibility > 4*32 is fuzz draw" true) && ok
  let inv8 := #[(0 : Int32), 0, 8, 0, 0, 0]
  let gsInv8 := spawnedGs light128 weapon5 inactive inv8
  match drawMasked data wad gsInv8 fb0 #[] #[] with
  | Except.error e =>
    ok := (← assert s!"invisibility & 8 fuzz: {e}" false) && ok
  | Except.ok _ =>
    ok := (← assert "invisibility & 8 is fuzz draw" true) && ok

  -- (8) FF_FULLBRIGHT colormap 0
  let dataLvl := mkData paintSprites paintMetrics levelColormaps
  let gsBright := spawnedGs 0 (pspAt stBright (5 * F) syCenter) inactive
  match drawMasked dataLvl wad gsBright fb0 #[] #[] with
  | Except.error e =>
    IO.eprintln s!"fullbright: {e}"; ok := false
  | Except.ok fbB =>
    ok := (← assert "FF_FULLBRIGHT colormap 0" (fbPixel fbB 5 84 == 0)) && ok

  -- (9) local light scalelight[lightnum][MAXLIGHTSCALE-1]
  let gsDark := spawnedGs 0 weapon5 inactive
  match drawMasked dataLvl wad gsDark fb0 #[] #[] with
  | Except.error e =>
    IO.eprintln s!"local light: {e}"; ok := false
  | Except.ok fbD =>
    let want := UInt8.ofNat (localCmap 0 0)
    ok := (← assert "local light scalelight last" (fbPixel fbD 5 84 == want)) && ok
    ok := (← assert "local light is not fullbright 0" (want != 0)) && ok

  -- (10) flip: 3-wide patch, reversed columns
  let flipSprites : Array SpriteDef :=
    #[
      padDef, padDef
      , { numframes := 2, spriteframes := #[emptyFrame, frameLump 2 1] }
      , padDef, padDef
    ]
  let flipMetrics := #[mkMetrics F 0 0, mkMetrics F 0 0, mkMetrics (3 * F) 0 0]
  let dataFlip := mkData flipSprites flipMetrics
  let gsFlip := spawnedGs light128 (pspAt stWeapon (5 * F) syCenter) inactive
  match drawMasked dataFlip wad gsFlip fb0 #[] #[] with
  | Except.error e =>
    IO.eprintln s!"flip: {e}"; ok := false
  | Except.ok fbFlip =>
    ok := (← assert "flip col x=5 is last"
      (fbPixel fbFlip 5 84 == 202)) && ok
    ok := (← assert "flip col x=6 is mid"
      (fbPixel fbFlip 6 84 == 201)) && ok
    ok := (← assert "flip col x=7 is first"
      (fbPixel fbFlip 7 84 == 200)) && ok

  -- (11) clipped x1: integer startfrac, not FixedMul
  let clipSprites : Array SpriteDef :=
    #[
      padDef, padDef
      , { numframes := 2, spriteframes := #[emptyFrame, frameLump 2] }
      , padDef, padDef
    ]
  let clipMetrics := #[mkMetrics F 0 0, mkMetrics F 0 0, mkMetrics (10 * F) 0 0]
  let dataWide := mkData clipSprites clipMetrics
  let gsClip := spawnedGs light128 (pspAt stWeapon ((-2) * F) syCenter) inactive
  match drawMasked dataWide wad gsClip fb0 #[] #[] with
  | Except.error e =>
    IO.eprintln s!"clipped-x1: {e}"; ok := false
  | Except.ok fbC =>
    -- x1raw=-2, startfrac += FRACUNIT*2 → column 2 at x=0. FixedMul would yield column 0.
    ok := (← assert "clipped-x1 integer startfrac"
      (fbPixel fbC 0 84 == 202)) && ok
    ok := (← assert "clipped-x1 not FixedMul column 0"
      (fbPixel fbC 0 84 != 200)) && ok

  -- (12) RANGECHECK strings match projectSprite
  let badNum := mkData #[padDef, padDef] paintMetrics
  let gsBad := spawnedGs light128 weapon5 inactive
  ok := (← expectError "RANGECHECK invalid sprite number"
    (drawMasked badNum wad gsBad fb0 #[] #[])
    "R_ProjectSprite: invalid sprite number 2 ") && ok
  let oneFrame : Array SpriteDef :=
    #[padDef, padDef, { numframes := 1, spriteframes := #[emptyFrame] }]
  let badFrame := mkData oneFrame paintMetrics
  ok := (← expectError "RANGECHECK invalid sprite frame"
    (drawMasked badFrame wad gsBad fb0 #[] #[])
    "R_ProjectSprite: invalid sprite frame 2 : 1 ") && ok

  -- (13) invisibility formula is not `powers != 0` (visible blink frame)
  let visBlink := #[(0 : Int32), 0, 1, 0, 0, 0]
  let gsBlink := spawnedGs light128 weapon5 inactive visBlink
  match drawMasked data wad gsBlink fb0 #[] #[] with
  | Except.error e =>
    IO.eprintln s!"visible blink: {e}"; ok := false
  | Except.ok fbBlink =>
    ok := (← assert "powers=1 still paints"
      (columnStrip fbBlink 5 84 88 == spriteStripA)) && ok

  -- (14) skipped weapon slot still draws flash
  let gsFlashOnly := spawnedGs light128 inactive flash5
  match drawMasked data wad gsFlashOnly fb0 #[] #[] with
  | Except.error e =>
    IO.eprintln s!"flash only: {e}"; ok := false
  | Except.ok fbFlash =>
    ok := (← assert "inactive weapon still draws flash"
      (columnStrip fbFlash 5 84 88 == spriteStripB)) && ok

  -- (15) extralight participates in lightnum
  let gsEx0 := spawnedGs 0 weapon5 inactive
  let gsEx1 := spawnedGs 0 weapon5 inactive (Array.replicate NUMPOWERS 0) 1
  match drawMasked dataLvl wad gsEx0 fb0 #[] #[], drawMasked dataLvl wad gsEx1 fb0 #[] #[] with
  | Except.ok fbEx0, Except.ok fbEx1 =>
    let c0 := UInt8.ofNat (localCmap 0 0)
    let c1 := UInt8.ofNat (localCmap 0 1)
    ok := (← assert "extralight 0 colormap" (fbPixel fbEx0 5 84 == c0)) && ok
    ok := (← assert "extralight 1 colormap" (fbPixel fbEx1 5 84 == c1)) && ok
    ok := (← assert "extralight shifts lightnum" (c0 != c1)) && ok
  | Except.error e, _ =>
    IO.eprintln s!"extralight 0: {e}"; ok := false
  | _, Except.error e =>
    IO.eprintln s!"extralight 1: {e}"; ok := false

  -- (16) consoleplayer, not hardcoded players[0]
  let gsWrongCp := { gsW with consoleplayer := 1 }
  match drawMasked data wad gsWrongCp fbMark #[] #[] with
  | Except.error e =>
    IO.eprintln s!"consoleplayer: {e}"; ok := false
  | Except.ok fbCp =>
    ok := (← assert "consoleplayer 1 skips player 0 psprites"
      (fbCp.pixels == fbMark.pixels)) && ok

  if ok then
    IO.println "draw-psprites-test: ALL PASS"
    pure 0
  else
    IO.eprintln "draw-psprites-test: FAILURES"
    pure 1
