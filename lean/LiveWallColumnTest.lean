import Doom.Harness.DisplaySim
import Doom.Harness.Real
import Doom.Playsim.Demo
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Spawn
import Doom.Playsim.Spec
import Doom.Playsim.Tick
import Doom.Render.Dump
import Doom.Render.Gfx.Flat
import Doom.Render.Main
import Doom.Render.Render
import Doom.Render.Types
import Doom.Render.View
import Doom.Wad

/-!
# LiveWallColumnTest

Live DEMO1 tic 35 wall-column blit: screen strip x=160 y=68–83 matches
fixtures/fb/demo1/fb_35.ppm. Pixels are painted walls, not black/bezel/nukage.
Uses `renderLevelFrame` (screenblocks=9 via `executeSetViewSize`).
-/

open Doom.Harness.DisplaySim
open Doom.Harness.Real
open Doom.Playsim.Demo
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Spawn
open Doom.Playsim.Spec
open Doom.Playsim.Tick
open Doom.Render.Dump
open Doom.Render.Gfx.Flat
open Doom.Render.Main
open Doom.Render.Render
open Doom.Render.Types
open Doom.Render.View
open Doom.Wad

private def lastGametic : Nat := 35
private def stripX : Nat := 160
private def stripY0 : Nat := 68
private def stripY1 : Nat := 83
private def ppmHeaderBytes : Nat := 15
private def nukageX : Nat := 40
private def nukageY : Nat := 140
private def bezelX : Nat := 0
private def bezelY : Nat := 0

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" => pure (cwd.parent.getD cwd)
  | _ => pure cwd

private def ppmPixelOff (x y : Nat) : Nat :=
  ppmHeaderBytes + (y * 320 + x) * 3

private def extractColumnStrip (ppm : ByteArray) (x y0 y1 : Nat) : ByteArray :=
  Id.run do
    let n := y1 - y0 + 1
    let mut out := ByteArray.emptyWithCapacity (n * 3)
    let mut y := y0
    while y <= y1 do
      let off := ppmPixelOff x y
      out := out.append (ppm.extract off (off + 3))
      y := y + 1
    out

private def rgbAt (ppm : ByteArray) (x y : Nat) : ByteArray :=
  let off := ppmPixelOff x y
  ppm.extract off (off + 3)

private def stripAllZero (strip : ByteArray) : Bool :=
  Id.run do
    let mut i := 0
    let mut allZ := true
    while i < strip.size && allZ do
      match strip[i]? with
      | some b => if b != 0 then allZ := false
      | none => allZ := false
      i := i + 1
    allZ

private def stripHasRgb (strip sample : ByteArray) : Bool :=
  if sample.size != 3 then
    false
  else
    Id.run do
      let mut i := 0
      let mut found := false
      while i + 3 <= strip.size && !found do
        if strip.extract i (i + 3) == sample then
          found := true
        i := i + 3
      found

private def hexNibble (n : Nat) : Char :=
  if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)

private def hexByte (b : UInt8) : String :=
  let n := b.toNat
  String.ofList [hexNibble (n / 16), hexNibble (n % 16)]

private def dumpStripHex (label : String) (strip : ByteArray) : IO Unit := do
  let mut acc := s!"  {label}:"
  let mut i := 0
  while i < strip.size do
    match strip[i]? with
    | some b => acc := acc ++ " " ++ hexByte b
    | none => pure ()
    i := i + 1
  IO.eprintln acc

private def runDemo1ToGametic (gs0 : GameState) (lastInclusive : Nat) :
    Except String GameState := Id.run do
  let mut gs := gs0
  let mut disp := initDisplay
  let mut err : Option String := none
  let mut g : Nat := 0
  while g <= lastInclusive && err.isNone do
    match gTicker { gs with gametic := g.toUInt32 } with
    | Except.error e => err := some e
    | Except.ok gs1 =>
      gs := gs1
      if g > 0 then
        let (disp', rng') := onFrame disp gs.rng gsLevel
        disp := disp'
        gs := { gs with rng := rng' }
    g := g + 1
  match err with
  | some e => Except.error e
  | none => Except.ok gs

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let root ← defaultRoot
  let wad ← loadFile (root / "fixtures" / "wads" / "doom1.wad")
  let refPpm ← IO.FS.readBinFile (root / "fixtures" / "fb" / "demo1" / "fb_35.ppm")
  let (vs, _) := executeSetViewSize (setViewSize 9 0)
  ok := (← assert "screenblocks=9 viewwidth=288" (vs.viewwidth == 288)) && ok
  ok := (← assert "screenblocks=9 viewheight=144" (vs.viewheight == 144)) && ok
  ok := (← assert "screenblocks=9 viewwindow=(16,12)"
    (vs.viewwindowx == 16 && vs.viewwindowy == 12)) && ok
  ok := (← assert "screenblocks=9 centery=72" (vs.centery == 72)) && ok

  let firstFlat ←
    match initFlats wad with
    | Except.error e =>
      IO.eprintln s!"live-wall-column-test FAIL: initFlats {e}"
      return 1
    | Except.ok (first, _) => pure first
  let resolve (name : String) : Option Nat :=
    match checkNumForName wad name with
    | none => none
    | some idx => some (idx - firstFlat)
  let picAnims ←
    match initPicAnims resolve with
    | Except.error e =>
      IO.eprintln s!"live-wall-column-test FAIL: initPicAnims {e}"
      return 1
    | Except.ok anims => pure anims

  let demoBytes ←
    match checkNumForName wad "DEMO1" with
    | none =>
      IO.eprintln "live-wall-column-test FAIL: DEMO1 lump missing"
      return 1
    | some idx =>
      match lumpData wad idx with
      | Except.error e =>
        IO.eprintln s!"live-wall-column-test FAIL: DEMO1 bytes {e}"
        return 1
      | Except.ok b => pure b
  match parseHeader demoBytes with
  | Except.error e =>
    IO.eprintln s!"live-wall-column-test FAIL: demo header {e}"
    return 1
  | Except.ok hdr =>
    match loadMap wad (mapLabel hdr.episode hdr.map) with
    | Except.error e =>
      IO.eprintln s!"live-wall-column-test FAIL: load map {e}"
      return 1
    | Except.ok level =>
      match setupSpawnedLevel level (hdr.skill.toUInt32.toInt32) hdr.playeringame
          hdr.consoleplayer.toNat with
      | Except.error e =>
        IO.eprintln s!"live-wall-column-test FAIL: spawn {e}"
        return 1
      | Except.ok gsSpawn =>
        let gs0 := {
          gsSpawn with
          picAnims
          demoBytes
          demoCursor := 13
          demoplayback := true
          gametic := 0
        }
        match runDemo1ToGametic gs0 lastGametic with
        | Except.error e =>
          IO.eprintln s!"live-wall-column-test FAIL: ticker {e}"
          return 1
        | Except.ok gs =>
          match renderLevelFrame wad gs with
          | Except.error e =>
            IO.eprintln s!"live-wall-column-test FAIL: renderLevelFrame {e}"
            return 1
          | Except.ok (fb, pal) =>
            let outDir := root / ".agent_tmp" / "live_wall_column_cand"
            dumpFrame outDir lastGametic fb pal
            let cand ← IO.FS.readBinFile (outDir / "fb_35.ppm")
            ok := (← assert "PPM size"
              (cand.size == 192015 && refPpm.size == 192015)) && ok
            let want := extractColumnStrip refPpm stripX stripY0 stripY1
            let got := extractColumnStrip cand stripX stripY0 stripY1
            ok := (← assert "strip length 16×3"
              (want.size == 48 && got.size == 48)) && ok
            let bezel := rgbAt refPpm bezelX bezelY
            let nukage := rgbAt refPpm nukageX nukageY
            ok := (← assert "strip not black" (!stripAllZero got)) && ok
            ok := (← assert "strip not bezel" (!stripHasRgb got bezel)) && ok
            ok := (← assert "strip not nukage" (!stripHasRgb got nukage)) && ok
            if got != want then
              ok := (← assert "strip x=160 y=68–83 vs fixture" false) && ok
              dumpStripHex "got" got
              dumpStripHex "want" want
            else
              ok := (← assert "strip x=160 y=68–83 vs fixture" true) && ok
  if ok then
    IO.println "live-wall-column-test: ALL PASS"
    pure 0
  else
    IO.eprintln "live-wall-column-test: FAILURES"
    pure 1
