import Doom.Playsim.GameState
import Doom.Playsim.Player
import Doom.Playsim.Weapons
import Doom.Render.Gfx.Patch
import Doom.Render.Types
import Doom.Render.Video
import Doom.Wad

/-!
# Doom.Render.StatusBar

Status bar overlay (`st_stuff.c` `ST_Drawer` / `ST_drawWidgets`).
-/

namespace Doom.Render.StatusBar

open Doom.Playsim.GameState
open Doom.Playsim.Player
open Doom.Playsim.Weapons
open Doom.Render.Gfx.Patch
open Doom.Render.Types
open Doom.Render.Video
open Doom.Wad

def ST_AMMOX : Int32 := 44
def ST_AMMOY : Int32 := 171
def ST_HEALTHX : Int32 := 90
def ST_HEALTHY : Int32 := 171
def ST_ARMORX : Int32 := 221
def ST_ARMORY : Int32 := 171
def ST_ARMSX : Int32 := 111
def ST_ARMSY : Int32 := 172
def ST_ARMSBGX : Int32 := 104
def ST_ARMSBGY : Int32 := 168
def ST_ARMSXSPACE : Int32 := 12
def ST_ARMSYSPACE : Int32 := 10
def ST_FACESX : Int32 := 143
def ST_FACESY : Int32 := 168
def ST_AMMOWIDTH : Nat := 3
def ST_NOAMMO : Int32 := 1994
def ST_FACESTRIDE : Int32 := 8
def ST_GODFACE : Int32 := 40
def ST_DEADFACE : Int32 := 41

/-- `st_stuff.c` palette indices (`ST_doPaletteStuff`). Chex is skipped. -/
def STARTREDPALS : Int32 := 1
def STARTBONUSPALS : Int32 := 9
def NUMREDPALS : Int32 := 8
def NUMBONUSPALS : Int32 := 4
def RADIATIONPAL : Int32 := 13

private def powerAt (plyr : Player) (idx : Nat) : Int32 :=
  match plyr.powers[idx]? with | some v => v | none => (0 : Int32)

/-- Active PLAYPAL slice for the console player (`ST_doPaletteStuff`). -/
def paletteIndex (plyr : Player) : Nat :=
  Id.run do
    let mut cnt := plyr.damagecount
    let strength := powerAt plyr pw_strength
    if strength != 0 then
      let bzc := (12 : Int32) - (strength >>> 6)
      if bzc > cnt then cnt := bzc
    if cnt != 0 then
      let mut pal := (cnt + 7) >>> 3
      if pal >= NUMREDPALS then pal := NUMREDPALS - 1
      pure (pal + STARTREDPALS).toNatClampNeg
    else if plyr.bonuscount != 0 then
      let mut pal := (plyr.bonuscount + 7) >>> 3
      if pal >= NUMBONUSPALS then pal := NUMBONUSPALS - 1
      pure (pal + STARTBONUSPALS).toNatClampNeg
    else
      let ironfeet := powerAt plyr pw_ironfeet
      if ironfeet > (4 * 32 : Int32) || (ironfeet &&& (8 : Int32)) != 0 then
        pure RADIATIONPAL.toNatClampNeg
      else
        pure 0

/-- Recompute the dump palette from live player fields (C `st_palette` is I_SetPalette-only). -/
def doPaletteStuff (gs : GameState) : Nat :=
  match gs.players[gs.consoleplayer]? with
  | none => 0
  | some plyr => paletteIndex plyr

def ammoXY : Array (Int32 × Int32) := #[(288, 173), (288, 179), (288, 191), (288, 185)]
def maxAmmoXY : Array (Int32 × Int32) := #[(314, 173), (314, 179), (314, 191), (314, 185)]

def drawNamed (wad : WadDirectory) (fb : Framebuffer) (name : String) (x y : Int32) :
    Except String Framebuffer := do
  match checkNumForName wad name with
  | none => throw s!"missing patch lump {name}"
  | some idx => drawPatch wad fb idx x y

def digitName (stem : String) (d : Nat) : String := s!"{stem}{d}"

/-- `STlib_drawNum` without backing-screen `V_CopyRect` / `STTMINUS`. -/
def drawNum (wad : WadDirectory) (fb : Framebuffer) (x y : Int32) (numdigits : Nat)
    (num0 : Int32) (stem : String) : Except String Framebuffer := do
  match checkNumForName wad (digitName stem 0) with
  | none => throw s!"missing patch lump {digitName stem 0}"
  | some idx0 =>
    let (hdr0, _) ← loadPatch wad idx0
    let w := hdr0.width.toUInt32.toInt32
    let mut num := num0
    if num < 0 then
      if numdigits == 2 && num < -9 then
        num := -9
      else if numdigits == 3 && num < -99 then
        num := -99
      num := -num
    if num == ST_NOAMMO then
      return fb
    let mut out := fb
    let mut xCur := x
    let mut digits := numdigits
    if num == 0 then
      out ← drawNamed wad out (digitName stem 0) (xCur - w) y
    while num != 0 && digits > 0 do
      digits := digits - 1
      xCur := xCur - w
      let d := (num % (10 : Int32)).toNatClampNeg
      out ← drawNamed wad out (digitName stem d) xCur y
      num := num / (10 : Int32)
    pure out

def drawPercent (wad : WadDirectory) (fb : Framebuffer) (x y : Int32) (num : Int32) :
    Except String Framebuffer := do
  let fb ← drawNamed wad fb "STTPRCNT" x y
  drawNum wad fb x y 3 num "STTNUM"

def readyAmmo (plyr : Player) : Except String Int32 := do
  match weaponinfo[plyr.readyweapon.toNatClampNeg]? with
  | none => throw s!"weaponinfo: bad weapon {plyr.readyweapon}"
  | some wi =>
    if wi.ammo == am_noammo then
      pure ST_NOAMMO
    else
      match plyr.ammo[wi.ammo.toNatClampNeg]? with
      | some n => pure n
      | none => pure 0

def ammoAt (plyr : Player) (i : Nat) : Int32 :=
  match plyr.ammo[i]? with | some n => n | none => 0

def maxAmmoAt (plyr : Player) (i : Nat) : Int32 :=
  match plyr.maxammo[i]? with | some n => n | none => 0

def ownedAt (plyr : Player) (i : Nat) : Int32 :=
  match plyr.weaponowned[i]? with | some n => n | none => 0

def faceLumpName (idx : Int32) : Except String String := do
  if idx == ST_GODFACE then
    return "STFGOD0"
  if idx == ST_DEADFACE then
    return "STFDEAD0"
  if idx < 0 || idx >= ST_GODFACE then
    throw s!"ST face index {idx} out of range"
  let pain := (idx / ST_FACESTRIDE).toNatClampNeg
  let off := (idx % ST_FACESTRIDE).toNatClampNeg
  match off with
  | 0 => pure s!"STFST{pain}0"
  | 1 => pure s!"STFST{pain}1"
  | 2 => pure s!"STFST{pain}2"
  | 3 => pure s!"STFTR{pain}0"
  | 4 => pure s!"STFTL{pain}0"
  | 5 => pure s!"STFOUCH{pain}"
  | 6 => pure s!"STFEVL{pain}"
  | 7 => pure s!"STFKILL{pain}"
  | _ => throw s!"ST face offset {off} out of range"

/-- Copy `src` into `dst` over a rectangle (`STlib` `V_CopyRect` from STBAR backing). -/
private def copyRect (src dst : Framebuffer) (x0 y0 : Int32) (w h : Nat) : Framebuffer :=
  Id.run do
    let mut out := dst
    let mut row : Nat := 0
    while row < h do
      let mut col : Nat := 0
      while col < w do
        let x := x0 + Int32.ofNat col
        let y := y0 + Int32.ofNat row
        if x >= 0 && y >= 0 then
          let ux := x.toNatClampNeg
          let uy := y.toNatClampNeg
          if ux < screenWidth && uy < screenHeight then
            out := out.set ux uy (src.get ux uy)
        col := col + 1
      row := row + 1
    pure out

/-- Restore STBAR under a patch dest rect, then the caller draws the patch. -/
private def restorePatchRect (src dst : Framebuffer) (wad : WadDirectory) (name : String)
    (x y : Int32) : Except String Framebuffer := do
  match checkNumForName wad name with
  | none => throw s!"missing patch lump {name}"
  | some idx =>
    let (hdr, _) ← loadPatch wad idx
    let x0 := x - hdr.leftOffset.toInt16.toInt32
    let y0 := y - hdr.topOffset.toInt16.toInt32
    pure (copyRect src dst x0 y0 hdr.width.toNat hdr.height.toNat)

/-- C `ST_drawWidgets(true)` over the already-painted STBAR. Keys/frags skipped. -/
def drawWidgets (wad : WadDirectory) (fb0 : Framebuffer) (gs : GameState) (plyr : Player) :
    Except String Framebuffer := do
  let mut fb := fb0
  let ready ← readyAmmo plyr
  fb ← drawNum wad fb ST_AMMOX ST_AMMOY ST_AMMOWIDTH ready "STTNUM"
  let mut i : Nat := 0
  while i < NUMAMMO do
    match ammoXY[i]?, maxAmmoXY[i]? with
    | some (ax, ay), some (mx, my) =>
      fb ← drawNum wad fb ax ay 3 (ammoAt plyr i) "STYSNUM"
      fb ← drawNum wad fb mx my 3 (maxAmmoAt plyr i) "STYSNUM"
    | _, _ => throw "ST ammo coord missing"
    i := i + 1
  fb ← drawPercent wad fb ST_HEALTHX ST_HEALTHY plyr.health
  fb ← drawPercent wad fb ST_ARMORX ST_ARMORY plyr.armorpoints
  if !gs.deathmatch then
    let fbBar := fb
    fb ← drawNamed wad fb "STARMS" ST_ARMSBGX ST_ARMSBGY
    let mut a : Nat := 0
    while a < 6 do
      let owned := ownedAt plyr (a + 1)
      let name :=
        if owned != 0 then s!"STYSNUM{a + 2}" else s!"STGNUM{a + 2}"
      let x := ST_ARMSX + (Int32.ofNat (a % 3)) * ST_ARMSXSPACE
      let y := ST_ARMSY + (Int32.ofNat (a / 3)) * ST_ARMSYSPACE
      -- Unchanged since `ST_initData`: STARMS shows through holes (C first draw).
      -- Changed icons: copy STBAR (C `V_CopyRect` when `inum` changes).
      let spawnOwned :=
        match gs.stFace.spawnWeaponsOwned[a + 1]? with
        | some v => v
        | none => (0 : Int32)
      if owned != spawnOwned then
        fb ← restorePatchRect fbBar fb wad name x y
      fb ← drawNamed wad fb name x y
      a := a + 1
  let faceName ← faceLumpName gs.stFace.faceindex
  fb ← drawNamed wad fb faceName ST_FACESX ST_FACESY
  -- keyboxes: inum == -1 while no cards; skip. Frags skipped (not deathmatch).
  pure fb

def drawer (wad : WadDirectory) (fb : Framebuffer) (gs : GameState) (fullscreen : Bool)
    (_refresh : Bool) : Except String Framebuffer := do
  if fullscreen then
    return fb
  match checkNumForName wad "STBAR" with
  | none => throw "missing STBAR"
  | some idx =>
    let fb ← drawPatch wad fb idx 0 168
    match gs.players[gs.consoleplayer]? with
    | none => throw "ST_Drawer: missing console player"
    | some plyr => drawWidgets wad fb gs plyr

end Doom.Render.StatusBar
