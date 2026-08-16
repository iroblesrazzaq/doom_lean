import Doom.Harness.DisplaySim
import Doom.Harness.Real
import Doom.Playsim.Demo
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Spawn
import Doom.Playsim.Spec
import Doom.Playsim.Tick
import Doom.Render.Bsp
import Doom.Render.Data
import Doom.Render.Dump
import Doom.Render.Gfx.Flat
import Doom.Render.Main
import Doom.Render.Render
import Doom.Render.Types
import Doom.Render.View
import Doom.Wad

/-!
# flat-anim-test

Animated flats: `P_InitPicAnims` NUKAGE cycle, `P_UpdateSpecials`
`flattranslation`, DEMO1 tic 35 NUKAGE3→NUKAGE1 and one visplane-band PPM pixel.
-/

open Doom.Harness.DisplaySim
open Doom.Harness.Real
open Doom.Playsim.Demo
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Spawn
open Doom.Playsim.Spec
open Doom.Playsim.Tick
open Doom.Render.Bsp
open Doom.Render.Data
open Doom.Render.Dump
open Doom.Render.Gfx.Flat
open Doom.Render.Main
open Doom.Render.Render
open Doom.Render.Types
open Doom.Render.View
open Doom.Wad

private def lastGametic : Nat := 35
private def sampleX : Nat := 40
private def sampleY : Nat := 140
private def ppmHeaderBytes : Nat := 15

private def emptyLevel : LevelData := {
  vertexes := #[]
  sectors := #[]
  sides := #[]
  lines := #[]
  segs := #[]
  subsectors := #[]
  nodes := #[]
  things := #[]
  blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
  reject := ByteArray.empty
}

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

private def flatResolver (wad : WadDirectory) (firstFlat : Nat) (name : String) : Option Nat :=
  match checkNumForName wad name with
  | none => none
  | some idx => some (idx - firstFlat)

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

private def findNukage (anims : Array PicAnim) (nukage1 nukage3 : Nat) : Option PicAnim :=
  Id.run do
    let mut found : Option PicAnim := none
    let mut i : Nat := 0
    while i < anims.size do
      match anims[i]? with
      | some a =>
        if !a.istexture && a.basepic == Int32.ofNat nukage1 && a.picnum == Int32.ofNat nukage3 then
          found := some a
      | none => pure ()
      i := i + 1
    pure found

private def hasBasepic (anims : Array PicAnim) (pic : Nat) : Bool :=
  Id.run do
    let mut found := false
    let mut i : Nat := 0
    while i < anims.size do
      match anims[i]? with
      | some a =>
        if a.basepic == Int32.ofNat pic then
          found := true
      | none => pure ()
      i := i + 1
    pure found

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let root ← defaultRoot
  let wad ← loadFile (root / "fixtures" / "wads" / "doom1.wad")
  let firstFlat ←
    match initFlats wad with
    | Except.error e =>
      IO.eprintln s!"flat-anim-test FAIL: initFlats {e}"
      return 1
    | Except.ok (first, _) => pure first
  let resolve := flatResolver wad firstFlat
  let nukage1 ←
    match flatNumForName wad firstFlat "NUKAGE1" with
    | Except.error e =>
      IO.eprintln s!"flat-anim-test FAIL: {e}"
      return 1
    | Except.ok n => pure n
  let nukage3 ←
    match flatNumForName wad firstFlat "NUKAGE3" with
    | Except.error e =>
      IO.eprintln s!"flat-anim-test FAIL: {e}"
      return 1
    | Except.ok n => pure n

  match initPicAnims resolve with
  | Except.error e =>
    IO.eprintln s!"flat-anim-test FAIL: initPicAnims {e}"
    return 1
  | Except.ok anims =>
    match findNukage anims nukage1 nukage3 with
    | none =>
      ok := (← assert "initPicAnims NUKAGE cycle" false) && ok
    | some nuk =>
      ok := (← assert "NUKAGE speed 8" (nuk.speed == (8 : Int32))) && ok
      ok := (← assert "NUKAGE numpics=3" (nuk.numpics == (3 : Int32))) && ok
      ok := (← assert "NUKAGE istexture=false" (!nuk.istexture)) && ok
    let mut i : Nat := 0
    while i < anims.size do
      match anims[i]? with
      | some a =>
        ok := (← assert s!"installed anim[{i}] is flat" (!a.istexture)) && ok
      | none => pure ()
      i := i + 1
    match resolve "RROCK05", resolve "SLIME01" with
    | none, none =>
      ok := (← assert "skips missing Doom II starts" true) && ok
    | some rrock, _ =>
      ok := (← assert "skips missing RROCK05" (!hasBasepic anims rrock)) && ok
    | none, some slime =>
      ok := (← assert "skips missing SLIME01" (!hasBasepic anims slime)) && ok

  let badResolve (name : String) : Option Nat :=
    if name == "NUKAGE1" || name == "NUKAGE3" then some 7 else none
  match initPicAnims badResolve with
  | Except.error e =>
    ok := (← assert "numpics<2 loud Except"
      (e == "P_InitPicAnims: bad cycle from NUKAGE1 to NUKAGE3")) && ok
  | Except.ok _ =>
    ok := (← assert "numpics<2 should error" false) && ok

  let endMissing (name : String) : Option Nat :=
    if name == "NUKAGE1" then some 51 else none
  match initPicAnims endMissing with
  | Except.error e =>
    ok := (← assert "missing endname loud Except"
      (e == "R_FlatNumForName: NUKAGE3 not found")) && ok
  | Except.ok _ =>
    ok := (← assert "missing endname should error" false) && ok

  let gsIdle := initFromLevel emptyLevel 2 #[true, false, false, false] 0
  let gsId := updateSpecials { gsIdle with leveltime := 35 }
  ok := (← assert "empty picAnims keeps identity table"
    (gsId.picAnims.isEmpty && gsId.flattranslation.isEmpty)) && ok

  match initPicAnims resolve with
  | Except.error e =>
    IO.eprintln s!"flat-anim-test FAIL: initPicAnims replay {e}"
    return 1
  | Except.ok anims =>
    let gs35 := {
      (initFromLevel emptyLevel 2 #[true, false, false, false] 0) with
      picAnims := anims
      leveltime := 35
    }
    let gsU := updateSpecials gs35
    let expect35 :=
      Int32.ofNat nukage1 +
        (((35 : Int32) / (8 : Int32) + Int32.ofNat nukage3) % (3 : Int32))
    ok := (← assert "updateSpecials leveltime=35 C formula"
      (match gsU.flattranslation[nukage3]? with
       | some v => v == expect35 && v == Int32.ofNat nukage1
       | none => false)) && ok
    let gs31 := updateSpecials { gs35 with leveltime := 31 }
    let expect31 :=
      Int32.ofNat nukage1 +
        (((31 : Int32) / (8 : Int32) + Int32.ofNat nukage3) % (3 : Int32))
    ok := (← assert "updateSpecials leveltime=31 C formula"
      (match gs31.flattranslation[nukage3]? with
       | some v => v == expect31
       | none => false)) && ok
    ok := (← assert "leveltime 31 vs 35 differ"
      (match gs31.flattranslation[nukage3]?, gsU.flattranslation[nukage3]? with
       | some a, some b => a != b
       | _, _ => false)) && ok

  let demoBytes ←
    match checkNumForName wad "DEMO1" with
    | none =>
      IO.eprintln "flat-anim-test FAIL: DEMO1 lump missing"
      return 1
    | some idx =>
      match lumpData wad idx with
      | Except.error e =>
        IO.eprintln s!"flat-anim-test FAIL: DEMO1 bytes {e}"
        return 1
      | Except.ok b => pure b
  match parseHeader demoBytes with
  | Except.error e =>
    IO.eprintln s!"flat-anim-test FAIL: demo header {e}"
    return 1
  | Except.ok hdr =>
    match loadMap wad (mapLabel hdr.episode hdr.map) with
    | Except.error e =>
      IO.eprintln s!"flat-anim-test FAIL: load map {e}"
      return 1
    | Except.ok level =>
      match setupSpawnedLevel level (hdr.skill.toUInt32.toInt32) hdr.playeringame
          hdr.consoleplayer.toNat with
      | Except.error e =>
        IO.eprintln s!"flat-anim-test FAIL: spawn {e}"
        return 1
      | Except.ok gsSpawn =>
        match initPicAnims resolve with
        | Except.error e =>
          IO.eprintln s!"flat-anim-test FAIL: initPicAnims spawn {e}"
          return 1
        | Except.ok anims =>
          let gs0 := {
            gsSpawn with
            picAnims := anims
            demoBytes
            demoCursor := 13
            demoplayback := true
            gametic := 0
          }
          match runDemo1ToGametic gs0 lastGametic with
          | Except.error e =>
            IO.eprintln s!"flat-anim-test FAIL: ticker {e}"
            return 1
          | Except.ok gs =>
            ok := (← assert "DEMO1 tic 35 leveltime=36" (gs.leveltime == 36)) && ok
            ok := (← assert "DEMO1 tic 35 flattranslation[NUKAGE3]==NUKAGE1"
              (match gs.flattranslation[nukage3]? with
               | some v => v == Int32.ofNat nukage1
               | none => false)) && ok
            match initData wad with
            | Except.error e =>
              IO.eprintln s!"flat-anim-test FAIL: initData {e}"
              return 1
            | Except.ok data =>
              let (vs, _) := executeSetViewSize (setViewSize 9 0)
              match renderBspFromGame data wad gs Framebuffer.initBlack vs with
              | Except.error e =>
                IO.eprintln s!"flat-anim-test FAIL: renderBsp {e}"
                return 1
              | Except.ok frame =>
                let mut sawNukage3 := false
                let mut i : Nat := 0
                while i < frame.planes.lastvisplane do
                  match frame.planes.visplanes[i]? with
                  | some vp =>
                    if vp.picnum == nukage3 then
                      sawNukage3 := true
                  | none => pure ()
                  i := i + 1
                ok := (← assert "visplane picnum untranslated NUKAGE3" sawNukage3) && ok
            match renderLevelFrame wad gs with
            | Except.error e =>
              IO.eprintln s!"flat-anim-test FAIL: render {e}"
              return 1
            | Except.ok (fb, pal) =>
              let outDir := root / ".agent_tmp" / "flat_anim_cand"
              dumpFrame outDir lastGametic fb pal
              let cand ← IO.FS.readBinFile (outDir / "fb_35.ppm")
              let ref ← IO.FS.readBinFile (root / "fixtures" / "fb" / "demo1" / "fb_35.ppm")
              let off := ppmHeaderBytes + (sampleY * 320 + sampleX) * 3
              ok := (← assert "PPM size"
                (cand.size == 192015 && ref.size == 192015)) && ok
              ok := (← assert "NUKAGE band PPM pixel (40,140)"
                (off + 3 <= cand.size && off + 3 <= ref.size &&
                  cand.extract off (off + 3) == ref.extract off (off + 3))) && ok
  if ok then
    IO.println "flat-anim-test: ALL PASS"
    pure 0
  else
    IO.eprintln "flat-anim-test: FAILURES"
    pure 1
