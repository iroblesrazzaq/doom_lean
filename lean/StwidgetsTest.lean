import Doom.Harness.DisplaySim
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Inter
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Random
import Doom.Playsim.Tick
import Doom.Render.Palette
import Doom.Render.StatusBar
import Doom.Render.Types
import Doom.Render.Video
import Doom.Wad

/-!
# stwidgets-test

R1z-stwidgets unit/integration: ST_drawWidgets over STBAR, key/am_noammo skips,
missing lumps, and ST_Ticker RNG/face history.
-/

open Doom.Harness.DisplaySim
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Inter
open Doom.Playsim.Level
open Doom.Playsim.Player
open Doom.Playsim.Random
open Doom.Playsim.Tick
open Doom.Render.Palette
open Doom.Render.Types
open Doom.Render.Video
open Doom.Wad

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" => pure (cwd.parent.getD cwd)
  | _ => pure cwd

private def loadIwad : IO WadDirectory := do
  let root ← defaultRoot
  loadFile (root / "fixtures" / "wads" / "doom1.wad")

private def emptyLevel : LevelData := {
  vertexes := #[]
  sectors := #[]
  sides := #[]
  lines := #[]
  segs := #[]
  subsectors := #[]
  nodes := #[]
  things := #[]
  blockmap := {
    originX := 0
    originY := 0
    width := 0
    height := 0
    lump := #[]
  }
  reject := ByteArray.empty
}

private def emptyWad : WadDirectory := {
  identification := "PWAD".toUTF8
  numlumps := 0
  infotableofs := 12
  entries := #[]
  data := ByteArray.empty
}

private def pwadOneLump (name : String) (payload : ByteArray) : WadDirectory :=
  let header := ByteArray.mk (Array.replicate 12 0)
  {
    identification := "PWAD".toUTF8
    numlumps := 1
    infotableofs := 12
    entries := #[{
      filepos := 12
      size := payload.size.toUInt32
      name := nameTo8 name
    }]
    data := header ++ payload
  }

private def regionDiffers (a b : Framebuffer) (x0 y0 x1 y1 : Nat) : Bool :=
  Id.run do
    let mut hit := false
    let mut y := y0
    while y <= y1 && !hit do
      let mut x := x0
      while x <= x1 && !hit do
        if Framebuffer.get a x y != Framebuffer.get b x y then
          hit := true
        x := x + 1
      y := y + 1
    hit

private def regionSame (a b : Framebuffer) (x0 y0 x1 y1 : Nat) : Bool :=
  !regionDiffers a b x0 y0 x1 y1

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def demoPlayer : Player :=
  { empty with
    playerstate := PST_LIVE
    health := 100
    armorpoints := 0
    readyweapon := wp_pistol
    pendingweapon := wp_nochange
    weaponowned :=
      let o := Array.replicate NUMWEAPONS (0 : Int32)
      let o := if h : 0 < o.size then o.set 0 1 else o
      if h : 1 < o.size then o.set 1 1 else o
    ammo :=
      let a := Array.replicate NUMAMMO (0 : Int32)
      if h : 0 < a.size then a.set 0 50 else a
    maxammo := defaultMaxAmmo
  }

private def withPlayer (p : Player) (face : StFaceState := StFaceState.init) : GameState :=
  let gs := initFromLevel emptyLevel 2 (Array.replicate 4 false) 0
  { gs with
    players := Doom.Playsim.GameState.arrSet gs.players 0 p
    stFace := face
  }

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let wad ← loadIwad

  ok := (← assert "palette idle 0" (Doom.Render.StatusBar.paletteIndex demoPlayer == 0)) && ok
  ok := (← assert "palette damage 1 → red 2"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with damagecount := 1 } == 2)) && ok
  ok := (← assert "palette damage 9 → red 3"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with damagecount := 9 } == 3)) && ok
  ok := (← assert "palette damage 100 clamps to red 8"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with damagecount := 100 } == 8)) && ok
  ok := (← assert "palette bonus 1 → gold 10"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with bonuscount := 1 } == 10)) && ok
  ok := (← assert "palette bonus 25 clamps to gold 12"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with bonuscount := 25 } == 12)) && ok
  ok := (← assert "palette damage beats bonus"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with damagecount := 1, bonuscount := 25 } == 2)) && ok
  let berserk : Array Int32 := Doom.Playsim.GameState.arrSet (Array.replicate NUMPOWERS (0 : Int32)) pw_strength 1
  ok := (← assert "palette berserk fade uses max(cnt, 12)"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with powers := berserk } == 3)) && ok
  ok := (← assert "palette berserk max with damagecount"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with damagecount := 20, powers := berserk } == 4)) && ok
  let radOn : Array Int32 := Doom.Playsim.GameState.arrSet (Array.replicate NUMPOWERS (0 : Int32)) pw_ironfeet 129
  ok := (← assert "palette ironfeet > 4*32 → radiation 13"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with powers := radOn } == 13)) && ok
  let radBlink : Array Int32 := Doom.Playsim.GameState.arrSet (Array.replicate NUMPOWERS (0 : Int32)) pw_ironfeet 8
  ok := (← assert "palette ironfeet & 8 → radiation 13"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with powers := radBlink } == 13)) && ok
  let radOff : Array Int32 := Doom.Playsim.GameState.arrSet (Array.replicate NUMPOWERS (0 : Int32)) pw_ironfeet 128
  ok := (← assert "palette ironfeet 128 not blinking"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with powers := radOff } == 0)) && ok
  ok := (← assert "palette bonus beats radiation"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with bonuscount := 1, powers := radOn } == 10)) && ok
  ok := (← assert "palette damage beats radiation"
    (Doom.Render.StatusBar.paletteIndex { demoPlayer with damagecount := 1, powers := radOn } == 2)) && ok
  ok := (← assert "doPaletteStuff console player"
    (Doom.Render.StatusBar.doPaletteStuff (withPlayer { demoPlayer with damagecount := 9 }) == 3)) && ok

  match loadPlaypal wad with
  | Except.error e =>
    ok := (← assert s!"PLAYPAL load ({e})" false) && ok
  | Except.ok lump =>
    ok := (← assert "PLAYPAL 14 slices" (lump.size == playpalLumpSize)) && ok
    match playpalSlice lump 0, playpalSlice lump 13 with
    | Except.ok s0, Except.ok s13 =>
      ok := (← assert "dump slice is 768" (s0.size == playpalSize)) && ok
      ok := (← assert "radiation slice differs from 0" (s0 != s13)) && ok
    | _, _ =>
      ok := (← assert "PLAYPAL slice 0 and 13" false) && ok

  let gs := withPlayer demoPlayer { StFaceState.init with faceindex := 0 }

  match checkNumForName wad "STBAR" with
  | none =>
    ok := (← assert "STBAR lump present" false) && ok
  | some idx =>
    match drawPatch wad Framebuffer.initBlack idx 0 168 with
    | Except.error e =>
      ok := (← assert s!"STBAR-only draw: {e}" false) && ok
    | Except.ok stbarOnly =>
      match Doom.Render.StatusBar.drawer wad Framebuffer.initBlack gs false true with
      | Except.error e =>
        ok := (← assert s!"drawer: {e}" false) && ok
      | Except.ok full =>
        ok := (← assert "ready ammo painted"
          (regionDiffers full stbarOnly 8 171 44 186)) && ok
        ok := (← assert "health painted"
          (regionDiffers full stbarOnly 50 171 90 186)) && ok
        ok := (← assert "armor painted"
          (regionDiffers full stbarOnly 180 171 221 186)) && ok
        ok := (← assert "arms painted"
          (regionDiffers full stbarOnly 104 168 145 191)) && ok
        ok := (← assert "face painted"
          (regionDiffers full stbarOnly 143 168 175 199)) && ok
        ok := (← assert "keys skipped while inum==-1"
          (regionSame full stbarOnly 235 171 250 199)) && ok
        let fistGs := withPlayer { demoPlayer with readyweapon := wp_fist }
          { StFaceState.init with faceindex := 0 }
        match Doom.Render.StatusBar.drawer wad Framebuffer.initBlack fistGs false true with
        | Except.error e =>
          ok := (← assert s!"fist drawer: {e}" false) && ok
        | Except.ok fistFb =>
          ok := (← assert "am_noammo ready ammo blank"
            (regionSame fistFb stbarOnly 8 171 44 186)) && ok
          ok := (← assert "fist still paints health"
            (regionDiffers fistFb stbarOnly 50 171 90 186)) && ok

  match Doom.Render.StatusBar.drawer emptyWad Framebuffer.initBlack gs false true with
  | Except.error e =>
    ok := (← assert "missing STBAR errors" (e == "missing STBAR")) && ok
  | Except.ok _ =>
    ok := (← assert "missing STBAR should error" false) && ok

  match checkNumForName wad "STBAR" with
  | none =>
    ok := (← assert "STBAR for STBAR-only PWAD" false) && ok
  | some idx =>
    match lumpData wad idx with
    | Except.error e =>
      ok := (← assert s!"STBAR lumpData: {e}" false) && ok
    | Except.ok payload =>
      let onlyBar := pwadOneLump "STBAR" payload
      match Doom.Render.StatusBar.drawer onlyBar Framebuffer.initBlack gs false true with
      | Except.error e =>
        ok := (← assert "missing widget lump errors" (e.contains "missing")) && ok
      | Except.ok _ =>
        ok := (← assert "STBAR-only PWAD should miss widget lumps" false) && ok

  let rng2 : RandomState := { prndindex := 0, rndindex := 2 }
  let rng66 := wipeInitMelt rng2
  let gsBase := initFromLevel emptyLevel 2 (Array.replicate 4 false) 0
  let gsAfter := stTicker { gsBase with rng := rng66 }
  ok := (← assert "stTicker after wipe →67" (gsAfter.rng.rndindex == 67)) && ok

  let live := withPlayer demoPlayer
  let gs1 := stTicker live
  ok := (← assert "idle face uses first M_Random % 3"
    (gs1.stFace.faceindex == (2 : Int32) && gs1.stFace.facecount == (16 : Int32))) && ok
  let mut gs5 := live
  let mut n : Nat := 0
  while n < 5 do
    gs5 := stTicker gs5
    n := n + 1
  -- Fifth M_Random is rndtable[5]=241 ≡ 1 (mod 3); persisted look must stay 2.
  ok := (← assert "face idle look is history, not last RNG"
    (gs5.stFace.faceindex == (2 : Int32) && gs5.stFace.facecount == (12 : Int32))) && ok

  let deadGs := stTicker (withPlayer { demoPlayer with health := 0 })
  ok := (← assert "dead face"
    (deadGs.stFace.faceindex == ST_DEADFACE)) && ok

  ok := (← assert "pain offset health 100"
    ((calcPainOffset StFaceState.init 100).2 == 0)) && ok
  ok := (← assert "pain offset health 60"
    ((calcPainOffset StFaceState.init 60).2 == 8)) && ok
  ok := (← assert "pain offset health 0"
    ((calcPainOffset StFaceState.init 0).2 == 32)) && ok
  ok := (← assert "pain offset clamps >100"
    ((calcPainOffset StFaceState.init 150).2 == 0)) && ok

  let armed := { demoPlayer with bonuscount := BONUSADD }
  let grinned := stTicker (withPlayer armed)
  ok := (← assert "BON2 without ST_initData evil-grins"
    (grinned.stFace.faceindex == ST_EVILGRINOFFSET)) && ok
  let spawned := stInitData (withPlayer armed)
  let afterBonus := stTicker spawned
  ok := (← assert "ST_Start+BON2 copies oldweaponsowned, no evil-grin"
    (afterBonus.stFace.faceindex == (2 : Int32)
      && afterBonus.stFace.faceindex != ST_EVILGRINOFFSET)) && ok

  let playerMo := { Doom.Playsim.Mobj.empty with x := 0, y := 0, angle := 0, player := 0 }
  let other := { Doom.Playsim.Mobj.empty with x := 0, y := 100 * FRACUNIT }
  let hurt : Player := {
    demoPlayer with
    mo := 1, attacker := -1, damagecount := 5, health := 100
  }
  let faceReady : StFaceState := { StFaceState.init with oldhealth := 100 }
  let missGs : GameState :=
    let gs := withPlayer hurt faceReady
    { gs with mobjs := #[other, playerMo] }
  let missFace := stTicker missGs
  ok := (← assert "missing attacker is not mobj 0 turn-face"
    (missFace.stFace.faceindex != ST_TURNOFFSET
      && missFace.stFace.faceindex != ST_TURNOFFSET + 1
      && missFace.stFace.faceindex == ST_RAMPAGEOFFSET)) && ok
  let hit : Player := { hurt with attacker := 0 }
  let hitGs : GameState :=
    let gs := withPlayer hit faceReady
    { gs with mobjs := #[other, playerMo] }
  let hitFace := stTicker hitGs
  ok := (← assert "attacker to the north → turn-left faceindex 4"
    (hitFace.stFace.faceindex == ST_TURNOFFSET + 1)) && ok
  let idleGs := stTicker (withPlayer demoPlayer)
  ok := (← assert "idle faceindex STFST00 family (M_Random % 3)"
    (idleGs.stFace.faceindex == (2 : Int32))) && ok

  if ok then
    IO.println "stwidgets-test: all passed"
    pure 0
  else
    IO.eprintln "stwidgets-test: FAILURES"
    pure 1
