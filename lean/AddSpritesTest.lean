import Doom.Playsim.Angle
import Doom.Playsim.Bsp
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Render.Bsp
import Doom.Render.Clip
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Gfx.Sprite
import Doom.Render.Gfx.Texture
import Doom.Render.Tables
import Doom.Render.Things.Add
import Doom.Render.Things.Init
import Doom.Render.Types
import Doom.Render.Util
import Doom.Render.View
import Doom.Wad

/-!
# AddSpritesTest

R1u-addsprites: `R_AddSprites` / `R_ProjectSprite` / `R_NewVisSprite` behavior.
-/

open Doom.Playsim.Angle
open Doom.Playsim.Bsp
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Render.Bsp
open Doom.Render.Clip
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Gfx.Sprite
open Doom.Render.Gfx.Texture
open Doom.Render.Tables
open Doom.Render.Things
open Doom.Render.Types
open Doom.Render.Util
open Doom.Render.View
open Doom.Wad

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def F : Int32 := FRACUNIT

private def identityColormaps : ByteArray :=
  ByteArray.mk (Array.ofFn (n := 8192) (fun i => UInt8.ofNat (i % 256)))

private def synthColumnSource : ByteArray :=
  ByteArray.mk (Array.ofFn (n := 128) (fun i => UInt8.ofNat i))

private def synthWad : WadDirectory :=
  let size := UInt32.ofNat synthColumnSource.size
  {
    identification := ByteArray.mk #[73, 87, 65, 68], numlumps := 2, infotableofs := 0,
    entries := #[
      { filepos := 0, size := 0, name := nameTo8 "DUMMY" },
      { filepos := 0, size := size, name := nameTo8 "COLUMN0" }
    ],
    data := synthColumnSource
  }

private def synthTextureTables : TextureTables :=
  {
    firstFlat := 0, numFlats := 0, skyFlatNum := 0, numTextures := 2,
    textures := #[
      { name := nameTo8 "-", width := 1, height := 1, patches := #[] },
      {
        name := nameTo8 "WALL", width := 1, height := 128,
        patches := #[{ originx := 0, originy := 0, patchLump := 0 }]
      }
    ],
    columnLump := #[#[(0 : Int32)], #[(1 : Int32)]],
    columnOfs := #[#[0], #[0]],
    widthMask := #[0, 0],
    height := #[1, 128],
    compositeSize := #[0, 0],
    composite := #[none, none],
    hashHead := #[none, some 1],
    hashNext := #[none, none]
  }

private def mkMetrics (w off top : Int32) : SpriteMetrics :=
  { width := w, offset := off, topOffset := top }

private def rotLumps : Array Int32 := #[0, 1, 2, 3, 4, 5, 6, 7]

private def noFlip : Array UInt8 := Array.replicate 8 (0 : UInt8)

private def allFlip : Array UInt8 := Array.replicate 8 (1 : UInt8)

private def frame0 (rotate : Bool) (flip : Array UInt8 := noFlip) : SpriteFrame :=
  { rotate, lump := rotLumps, flip }

private def oneDef (rotate : Bool) (flip : Array UInt8 := noFlip) : Array SpriteDef :=
  #[{ numframes := 1, spriteframes := #[frame0 rotate flip] }]

private def eightMetrics : Array SpriteMetrics :=
  Array.ofFn (n := 8) (fun _ => mkMetrics F 0 0)

private def wideMetrics : Array SpriteMetrics :=
  Array.ofFn (n := 8) (fun _ => mkMetrics (10 * F) 0 0)

private def mkData (sprites : Array SpriteDef) (metrics : Array SpriteMetrics) : RenderData :=
  {
    playpal := ByteArray.empty
    colormaps := identityColormaps
    textureTables := synthTextureTables
    firstSprite := 0
    spriteMetrics := metrics
    sprites
  }

private def pic (b : UInt8) : ByteArray := ByteArray.mk #[b, 0, 0, 0, 0, 0, 0, 0]

private def flatCeiling : ByteArray := ByteArray.mk (Array.replicate 8 (UInt8.ofNat 65))

private def mkSector (floor ceiling light : Int32) (fp cp : ByteArray) : Sector := {
  floorheight := floor, ceilingheight := ceiling
  floorpic := fp, ceilingpic := cp, lightlevel := light
  special := 0, tag := 0, lines := #[], blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def mkSide (sec : UInt32) (mid : ByteArray) : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := ByteArray.empty, bottomtexture := ByteArray.empty
  midtexture := mid, sector := sec
}

private def mkLine (v1 v2 sidenum1 flags front back : Int32) : Line := {
  v1 := v1.toUInt32, v2 := v2.toUInt32, flags, special := 0, tag := 0
  sidenum0 := 0, sidenum1, dx := 0, dy := F
  slopetype := ST_VERTICAL, bbox := #[F, 0, 0, 0]
  frontsector := front, backsector := back
}

private def mkSeg (v1 v2 linedef side : Nat) (front back : Int32) : Seg := {
  v1 := v1.toUInt32, v2 := v2.toUInt32, angle := 0, offset := 0
  linedef := linedef.toUInt32, side := side.toUInt32
  frontsector := front, backsector := back
}

private def mkLevel (verts : Array Vertex) (sectors : Array Sector) (sides : Array Side)
    (lines : Array Line) (segs : Array Seg) (subsectors : Array Subsector)
    (nodes : Array Node) : LevelData := {
  vertexes := verts, sectors, sides, lines, segs, subsectors, nodes, things := #[]
  blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
  reject := ByteArray.empty
}

private def viewCtx : RenderViewCtx := {
  viewx := 0, viewy := 0, viewangle := 0
  mapping := initViewMapping 320 160
}

private def viewz : Int32 := 41 * FRACUNIT

private def goldenLevel : LevelData :=
  let s0 := mkSector 0 (128 * F) 128 (pic 1) flatCeiling
  let sd0 := mkSide 0 (nameTo8 "WALL")
  let l := mkLine 1 2 ((-1 : Int32)) 0 0 ((-1 : Int32))
  let verts : Array Vertex := #[{ x := 0, y := 0 }, { x := -10 * F, y := F }, { x := 10 * F, y := F }]
  mkLevel verts #[s0] #[sd0] #[l] #[mkSeg 1 2 0 0 0 ((-1 : Int32))]
    #[{ numsegs := 1, firstseg := 0, sector := 0 }] #[]

private def goldenNodeLevel : LevelData :=
  let base := goldenLevel
  { base with
    subsectors := #[
      { numsegs := 1, firstseg := 0, sector := 0 },
      { numsegs := 0, firstseg := 0, sector := 0 }
    ]
    nodes := #[{
      x := 0, y := 0, dx := F, dy := 0
      bbox0 := #[F, (-F), (-10 * F), 0]
      bbox1 := #[F, (-F), (-20 * F), (-10 * F)]
      child0 := NF_SUBSECTOR ||| 0, child1 := NF_SUBSECTOR ||| 1
    }]
  }

private def mkThing (x y : Int32) (snext : Int32 := -1) : Mobj :=
  { Doom.Playsim.Mobj.empty with x, y, snext }

private def mkRuntime (light : Int32) (head : Int32) : SectorRuntime :=
  { mkSectorRuntime (mkSector 0 (128 * F) light (pic 1) flatCeiling) with thinglist := head }

private def collectOf (sectors : Array SectorRuntime) (mobjs : Array Mobj)
    (extralight : Int32 := 0) (viewx : Int32 := 0) (viewy : Int32 := 0) :
    SpriteCollectState :=
  initSpriteCollect sectors mobjs viewx viewy 0 0 extralight

private def runAdd (data : RenderData) (light : Int32) (mobjs : Array Mobj)
    (head : Int32 := 0) (extralight : Int32 := 0) :
    Except String SpriteCollectState :=
  addSprites data (collectOf #[mkRuntime light head] mobjs extralight) 0

private def expectError (name : String) (got : Except String SpriteCollectState)
    (want : String) : IO Bool := do
  match got with
  | Except.ok _ =>
    IO.eprintln s!"FAIL: {name} (expected error)"
    pure false
  | Except.error e =>
    if e == want then
      IO.println s!"PASS: {name}"
      pure true
    else
      IO.eprintln s!"FAIL: {name} got {e}"
      pure false

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let data := mkData (oneDef false) eightMetrics
  let dataWide := mkData (oneDef false) wideMetrics
  let dataFlip := mkData (oneDef false allFlip) wideMetrics
  let dataRot := mkData (oneDef true) eightMetrics
  let front := mkThing (32 * F) 0
  let behind := mkThing (-32 * F) 0
  let offSide := mkThing (32 * F) (200 * F)
  let offRight := mkThing (32 * F) (-48 * F)
  let offLeft := mkThing (32 * F) (48 * F)

  match runAdd data 128 #[behind] with
  | Except.error e =>
    IO.eprintln s!"behind: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "reject behind" st.visSprites.isEmpty) && ok

  match runAdd data 128 #[offSide] with
  | Except.error e =>
    IO.eprintln s!"off-side: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "reject off-side" st.visSprites.isEmpty) && ok

  match runAdd data 128 #[offRight] with
  | Except.error e =>
    IO.eprintln s!"off-right: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "reject off-right" st.visSprites.isEmpty) && ok

  match runAdd data 128 #[offLeft] with
  | Except.error e =>
    IO.eprintln s!"off-left: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "reject off-left" st.visSprites.isEmpty) && ok

  match runAdd dataRot 128 #[front] with
  | Except.error e =>
    IO.eprintln s!"rotate: {e}"; ok := false
  | Except.ok st =>
    let patch := (st.visSprites.getD 0 {}).patch
    ok := (← assert "rotate uses rot 4" (st.visSprites.size == 1 && patch == 4)) && ok

  let angled : Mobj := { front with angle := ANG90 }
  match runAdd dataRot 128 #[angled] with
  | Except.error e =>
    IO.eprintln s!"rotate-ang90: {e}"; ok := false
  | Except.ok st =>
    let patch := (st.visSprites.getD 0 {}).patch
    ok := (← assert "rotate ANG90 uses rot 2" (st.visSprites.size == 1 && patch == 2)) && ok

  match runAdd data 128 #[front] with
  | Except.error e =>
    IO.eprintln s!"rotate-false: {e}"; ok := false
  | Except.ok st =>
    let patch := (st.visSprites.getD 0 {}).patch
    ok := (← assert "rotate=false uses lump 0" (st.visSprites.size == 1 && patch == 0)) && ok

  match runAdd dataFlip 128 #[front] with
  | Except.error e =>
    IO.eprintln s!"flip: {e}"; ok := false
  | Except.ok st =>
    let vis := st.visSprites.getD 0 {}
    let iscale := fixedDiv F (fixedDiv (160 * F) (32 * F))
    ok := (← assert "flip startfrac" (vis.startfrac == 10 * F - 1)) && ok
    ok := (← assert "flip xiscale" (vis.xiscale == -iscale)) && ok

  let clipThing := mkThing (32 * F) (40 * F)
  match runAdd dataWide 128 #[clipThing] with
  | Except.error e =>
    IO.eprintln s!"clip-x1: {e}"; ok := false
  | Except.ok st =>
    let vis := st.visSprites.getD 0 {}
    let trX := clipThing.x - st.viewx
    let trY := clipThing.y - st.viewy
    let tz := fixedMul trX st.viewcos - (-fixedMul trY st.viewsin)
    let xscale := fixedDiv st.projection tz
    let tx0 := -(fixedMul trY st.viewcos + (-fixedMul trX st.viewsin))
    let x1raw := (st.centerxfrac + fixedMul tx0 xscale) >>> 16
    ok := (← assert "clipped x1" (st.visSprites.size == 1 && vis.x1 == 0)) && ok
    ok := (← assert "clipped-x1 integer multiply"
      (vis.startfrac == vis.xiscale * (vis.x1 - x1raw))) && ok
    ok := (← assert "clipped-x1 not FixedMul"
      (vis.startfrac != fixedMul vis.xiscale (vis.x1 - x1raw))) && ok

  let bright : Mobj := { front with frame := ffFullbright }
  match runAdd data 128 #[bright] with
  | Except.error e =>
    IO.eprintln s!"fullbright: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "FF_FULLBRIGHT colormap 0"
      ((st.visSprites.getD 0 {}).colormap == some 0)) && ok

  let shadow : Mobj := { front with flags := MF_SHADOW }
  match runAdd data 128 #[shadow] with
  | Except.error e =>
    IO.eprintln s!"shadow: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "MF_SHADOW colormap none"
      ((st.visSprites.getD 0 {}).colormap == none)) && ok

  match runAdd data 128 #[front] with
  | Except.error e =>
    IO.eprintln s!"light-clamp: {e}"; ok := false
  | Except.ok st =>
    let want := (scalelight.getD 8 #[]).getD (Doom.Render.Constants.maxLightScale - 1) 0
    ok := (← assert "light clamp"
      ((st.visSprites.getD 0 {}).colormap == some want)) && ok

  let far := mkThing (80 * F) 0
  match runAdd data 128 #[far] with
  | Except.error e =>
    IO.eprintln s!"light-far: {e}"; ok := false
  | Except.ok st =>
    let xscale := fixedDiv (160 * F) (80 * F)
    let idx := i32ToNat (xscale >>> 12)
    let want := (scalelight.getD 8 #[]).getD idx 0
    ok := (← assert "light unclamped far"
      (idx < Doom.Render.Constants.maxLightScale && (st.visSprites.getD 0 {}).colormap == some want)) && ok

  match runAdd data 128 #[front] 0 16 with
  | Except.error e =>
    IO.eprintln s!"extralight: {e}"; ok := false
  | Except.ok st =>
    let want := (scalelight.getD (Doom.Render.Tables.lightLevels - 1) #[]).getD
      (Doom.Render.Constants.maxLightScale - 1) 0
    ok := (← assert "extralight clamp"
      ((st.visSprites.getD 0 {}).colormap == some want)) && ok

  match runAdd data 128 #[front] with
  | Except.error e =>
    IO.eprintln s!"twice-1: {e}"; ok := false
  | Except.ok st1 =>
    match addSprites data st1 0 with
    | Except.error e =>
      IO.eprintln s!"twice-2: {e}"; ok := false
    | Except.ok st2 =>
      ok := (← assert "same sector twice one pass"
        (st1.visSprites.size == 1 && st2.visSprites.size == 1)) && ok

  let a := mkThing (32 * F) 0 1
  let b := mkThing (40 * F) 0
  match runAdd data 128 #[a, b] with
  | Except.error e =>
    IO.eprintln s!"order: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "thinglist order"
      (st.visSprites.size == 2 &&
        (st.visSprites.getD 0 {}).gx == 32 * F &&
        (st.visSprites.getD 1 {}).gx == 40 * F)) && ok

  let mut many : Array Mobj := #[]
  let mut i : Nat := 0
  while i < 129 do
    let nxt : Int32 := if i + 1 < 129 then Int32.ofNat (i + 1) else -1
    let x := 32 * F + Int32.ofNat i
    many := many.push (mkThing x 0 nxt)
    i := i + 1
  match runAdd data 128 many with
  | Except.error e =>
    IO.eprintln s!"overflow: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "128 keep" (st.visSprites.size == 128)) && ok
    ok := (← assert "129th overflow not appended" (st.visSprites.size == maxVisSprites)) && ok
    match st.overflowSprite with
    | none =>
      ok := (← assert "overflow slot occupied" false) && ok
    | some vis =>
      ok := (← assert "overflow is 129th"
        (vis.gx == 32 * F + 128)) && ok

  let badSpr : Mobj := { front with sprite := 5 }
  ok := (← expectError "loud invalid sprite"
    (runAdd data 128 #[badSpr])
    "R_ProjectSprite: invalid sprite number 5 ") && ok

  let badFr : Mobj := { front with frame := 1 }
  ok := (← expectError "loud invalid frame"
    (runAdd data 128 #[badFr])
    "R_ProjectSprite: invalid sprite frame 0 : 1 ") && ok

  let atView : Mobj := { mkThing 0 0 with sprite := 99 }
  match runAdd data 128 #[atView] with
  | Except.error e =>
    IO.eprintln s!"view-player RANGECHECK leaked: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "view-player MINZ before RANGECHECK" st.visSprites.isEmpty) && ok

  let looped : Mobj := { front with snext := 0 }
  ok := (← expectError "thinglist cycle"
    (runAdd data 128 #[looped])
    "R_AddSprites: thinglist cycle") && ok

  match addSprites data (collectOf #[] #[front]) 0 with
  | Except.error e =>
    IO.eprintln s!"empty-sectors: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "empty sectors skip" st.visSprites.isEmpty) && ok

  let fb0 := Framebuffer.initBlack
  let dsBare := initBspDrawState data synthWad fb0
  let st0 := clearClipSegs (clearDrawSegs emptyClipState)
  match subsector viewz viewCtx st0 {} 0 goldenLevel (some dsBare) with
  | Except.error e =>
    IO.eprintln s!"no-overlay: {e}"; ok := false
  | Except.ok (_, _, draw) =>
    match draw with
    | none => ok := false
    | some ds =>
      ok := (← assert "initBspDrawState without overlay no-op"
        ds.spriteCollect.isNone) && ok

  let gs0 := initFromLevel goldenNodeLevel 2 #[true, false, false, false] 0
  let playerMo : Mobj := { Doom.Playsim.Mobj.empty with x := 0, y := 0, angle := 0 }
  let visMo : Mobj := mkThing (32 * F) 0
  let pl : Player := { Doom.Playsim.Player.empty with mo := 0, viewz := viewz, viewheight := VIEWHEIGHT }
  let sec0 :=
    match gs0.sectors[0]? with
    | some s => { s with thinglist := 1, validcount := 9 }
    | none => mkRuntime 128 1
  let gs : GameState := {
    gs0 with
      mobjs := #[playerMo, visMo]
      players := Doom.Playsim.GameState.arrSet gs0.players 0 pl
      sectors := Doom.Playsim.GameState.arrSet gs0.sectors 0 sec0
      validcount := 7
  }
  match renderBspFromGame data synthWad gs fb0 with
  | Except.error e =>
    IO.eprintln s!"renderBspFromGame: {e}"; ok := false
  | Except.ok frame =>
    ok := (← assert "renderBspFromGame in-front one vissprite"
      (frame.visSprites.size == 1)) && ok
    ok := (← assert "GameState.validcount unchanged" (gs.validcount == 7)) && ok
    let secAfter :=
      match gs.sectors[0]? with
      | some s => s.validcount == 9
      | none => false
    ok := (← assert "SectorRuntime.validcount unchanged" secAfter) && ok

  let collectShared := collectOf #[mkRuntime 128 0] #[front]
  let dsShared := { initBspDrawState data synthWad fb0 with spriteCollect := some collectShared }
  match subsector viewz viewCtx st0 {} 0 goldenNodeLevel (some dsShared) with
  | Except.error e =>
    IO.eprintln s!"ss0: {e}"; ok := false
  | Except.ok (clip1, _, draw1) =>
    match draw1 with
    | none => ok := false
    | some ds1 =>
      match subsector viewz viewCtx clip1 {} 1 goldenNodeLevel (some ds1) with
      | Except.error e =>
        IO.eprintln s!"ss1: {e}"; ok := false
      | Except.ok (_, _, draw2) =>
        match draw2 with
        | none => ok := false
        | some ds2 =>
          let n :=
            match ds2.spriteCollect with
            | none => 0
            | some sc => sc.visSprites.size
          ok := (← assert "two subsectors sharing sector one collection" (n == 1)) && ok

  match runAdd data 0 #[front] with
  | Except.error e =>
    IO.eprintln s!"runtime-light0: {e}"; ok := false
  | Except.ok st =>
    let want := (scalelight.getD 0 #[]).getD (Doom.Render.Constants.maxLightScale - 1) 0
    ok := (← assert "SectorRuntime lightlevel 0"
      ((st.visSprites.getD 0 {}).colormap == some want)) && ok

  let flagged : Mobj := { front with flags := MF_SOLID }
  match runAdd data 128 #[flagged] with
  | Except.error e =>
    IO.eprintln s!"flags: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "stores mobjflags"
      ((st.visSprites.getD 0 {}).mobjflags == MF_SOLID)) && ok

  let alongY := mkThing 0 (32 * F)
  let stAng := initSpriteCollect #[mkRuntime 128 0] #[alongY] 0 0 0 ANG90 0
  match addSprites data stAng 0 with
  | Except.error e =>
    IO.eprintln s!"viewangle: {e}"; ok := false
  | Except.ok st =>
    ok := (← assert "viewangle ANG90 projects +y" (st.visSprites.size == 1)) && ok

  let collectDark := collectOf #[mkRuntime 0 0] #[front]
  let dsDark := { initBspDrawState data synthWad fb0 with spriteCollect := some collectDark }
  match subsector viewz viewCtx st0 {} 0 goldenLevel (some dsDark) with
  | Except.error e =>
    IO.eprintln s!"runtime-vs-geom: {e}"; ok := false
  | Except.ok (_, _, drawDark) =>
    match drawDark with
    | none => ok := false
    | some ds =>
      let vis :=
        match ds.spriteCollect with
        | none => none
        | some sc => sc.visSprites[0]?
      let want := (scalelight.getD 0 #[]).getD (Doom.Render.Constants.maxLightScale - 1) 0
      ok := (← assert "subsector uses SectorRuntime light not geometry"
        (match vis with
         | some v => v.colormap == some want
         | none => false)) && ok

  let plExtra : Player := { pl with extralight := 16 }
  let gsExtra := { gs with players := Doom.Playsim.GameState.arrSet gs.players 0 plExtra }
  match renderBspFromGame data synthWad gsExtra fb0 with
  | Except.error e =>
    IO.eprintln s!"console-extralight: {e}"; ok := false
  | Except.ok frame =>
    let want := (scalelight.getD (Doom.Render.Tables.lightLevels - 1) #[]).getD
      (Doom.Render.Constants.maxLightScale - 1) 0
    ok := (← assert "renderBspFromGame console extralight"
      (frame.visSprites.size == 1 &&
        (frame.visSprites.getD 0 {}).colormap == some want)) && ok

  if ok then
    IO.println "add-sprites-test: ALL PASS"
    pure 0
  else
    IO.eprintln "add-sprites-test: FAILURES"
    pure 1
