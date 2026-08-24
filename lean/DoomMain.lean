import Doom.Harness.Real
import Doom.Playsim.GameState
import Doom.Playsim.Input
import Doom.Playsim.Player
import Doom.Playsim.Spawn
import Doom.Playsim.Spec
import Doom.Playsim.Tick
import Doom.Render.Gfx.Flat
import Doom.Render.Render
import Doom.Wad

/-!
# doom

Live E1M1 window: Lake Lean exe + thin SDL2 FFI. `@[extern]` lives here so
`digest-test` / `fb-test` are not linked against SDL.
-/

open Doom.Harness.Real
open Doom.Playsim.GameState
open Doom.Playsim.Input
open Doom.Playsim.Player
open Doom.Playsim.Spawn
open Doom.Playsim.Spec
open Doom.Playsim.Tick
open Doom.Render.Gfx.Flat
open Doom.Render.Render
open Doom.Wad

@[extern c inline "lean_io_result_mk_ok(lean_box_uint32((uint32_t)doom_i_init_graphics()))"]
opaque iInitGraphics : IO UInt32

@[extern c inline "(doom_i_shutdown(), lean_io_result_mk_ok(lean_box(0)))"]
opaque iShutdown : IO Unit

@[extern c inline "lean_io_result_mk_ok(lean_box_uint32(doom_i_get_time()))"]
opaque iGetTime : IO UInt32

@[extern c inline "(doom_i_sleep(#1), lean_io_result_mk_ok(lean_box(0)))"]
opaque iSleep (ms : UInt32) : IO Unit

@[extern c inline "(doom_i_start_tic(), lean_io_result_mk_ok(lean_box(0)))"]
opaque iStartTic : IO Unit

@[extern c inline "lean_io_result_mk_ok(lean_box_uint32(doom_i_poll_event()))"]
opaque iPollEvent : IO UInt32

@[extern c inline "(doom_i_finish_update(lean_sarray_cptr(#1), lean_sarray_size(#1), lean_sarray_cptr(#2), lean_sarray_size(#2)), lean_io_result_mk_ok(lean_box(0)))"]
opaque iFinishUpdate (fb : @& ByteArray) (pal : @& ByteArray) : IO Unit

private def usage : String :=
  "usage: doom -iwad <doom1.wad>"

structure DoomArgs where
  iwad : System.FilePath
  quitAfterTics : Option Nat

private def parseArgs (args : List String) : Except String DoomArgs :=
  let rec go (rest : List String) (iwad : String) (quit : Option Nat) : Except String DoomArgs :=
    match rest with
    | [] =>
      if iwad == "" then Except.error "missing -iwad"
      else Except.ok { iwad := iwad, quitAfterTics := quit }
    | "-iwad" :: p :: rest => go rest p quit
    | "--quit-after-tics" :: nStr :: rest =>
      match nStr.toNat? with
      | none => Except.error s!"invalid --quit-after-tics value: {nStr}"
      | some n => go rest iwad (some n)
    | other :: _ => Except.error s!"unknown or incomplete argument: {other}"
  go args "" none

private def setPlayerCmd (gs : GameState) (cmd : TicCmd) : GameState :=
  match gs.players[gs.consoleplayer]? with
  | none => gs
  | some p => { gs with players := arrSet gs.players gs.consoleplayer { p with cmd } }

private def spawnLive (iwadPath : System.FilePath) : IO (Except String (WadDirectory × GameState)) := do
  try
    let wad ← loadFile iwadPath
    match loadMap wad "E1M1" with
    | Except.error e => return Except.error e
    | Except.ok level =>
      match setupSpawnedLevel level sk_medium #[true, false, false, false] 0 with
      | Except.error e => return Except.error e
      | Except.ok gsSpawn =>
        let firstFlat ←
          match initFlats wad with
          | Except.error e => return Except.error e
          | Except.ok (first, _) => pure first
        let resolve (name : String) : Option Nat :=
          match checkNumForName wad name with
          | none => none
          | some idx => some (idx - firstFlat)
        match initPicAnims resolve with
        | Except.error e => return Except.error e
        | Except.ok picAnims =>
          let gs := {
            gsSpawn with
            picAnims
            demoplayback := false
            gametic := 0
          }
          pure (Except.ok (wad, gs))
  catch e =>
    pure (Except.error (toString e))

private def pollInput (inp : InputState) : IO (InputState × Bool) := do
  iStartTic
  let mut inp := inp
  let mut quit := false
  while true do
    let ev ← iPollEvent
    if ev == 0 then
      break
    else
      let kind := ev >>> 16
      let key := ev &&& (0xffff : UInt32)
      if kind == 3 then
        quit := true
      else if kind == 1 then
        inp := gResponder inp true key
      else if kind == 2 then
        inp := gResponder inp false key
  pure (inp, quit)

private def runOneTic (gs : GameState) (inp : InputState) (g : Nat) :
    Except String (GameState × InputState) := do
  let (inp, cmd) := buildTiccmd inp
  let gs := setPlayerCmd gs cmd
  let gs := { gs with gametic := g.toUInt32 }
  let gs ← gTicker gs
  pure (gs, inp)

/-- Single-player `TryRunTics` (`ticdup=1`): wait until a tic is due, then catch up. -/
private def tryRunTics (gs : GameState) (inp : InputState)
    (lastTime : UInt32) (ticsRun : Nat) (quitAfter : Option Nat) :
    IO (Except String (GameState × InputState × UInt32 × Nat × Bool)) := do
  let mut gs := gs
  let mut inp := inp
  let mut quit := false
  let mut now ← iGetTime
  while true do
    if now > lastTime || quit then
      break
    else
      iSleep (1 : UInt32)
      let (inp', q) ← pollInput inp
      inp := inp'
      quit := q
      now ← iGetTime
  if quit then
    return Except.ok (gs, inp, lastTime, ticsRun, true)
  let counts := now - lastTime
  let n := if counts < 1 then (1 : UInt32) else counts
  let mut i : UInt32 := 0
  let mut ticsRun := ticsRun
  let mut err : Option String := none
  while true do
    if i >= n || err.isSome || quit then
      break
    else
      match quitAfter with
      | some limit =>
        if ticsRun >= limit then
          break
      | none => pure ()
      let (inp', q) ← pollInput inp
      inp := inp'
      quit := q
      if quit then
        break
      else
        match runOneTic gs inp ticsRun with
        | Except.error e => err := some e
        | Except.ok (gs', inp') =>
          gs := gs'
          inp := inp'
          ticsRun := ticsRun + 1
        i := i + 1
  match err with
  | some e => pure (Except.error e)
  | none =>
    pure (Except.ok (gs, inp, now, ticsRun, quit))

private def frameLoop (wad : WadDirectory) (gs : GameState) (inp : InputState)
    (lastTime : UInt32) (ticsRun : Nat) (fuzzpos : Nat) (quitAfter : Option Nat) :
    IO UInt32 := do
  let mut gs := gs
  let mut inp := inp
  let mut lastTime := lastTime
  let mut ticsRun := ticsRun
  let mut fuzzpos := fuzzpos
  let mut rc : UInt32 := 0
  while true do
    match quitAfter with
    | some limit =>
      if ticsRun >= limit then
        iShutdown
        break
    | none => pure ()
    match ← tryRunTics gs inp lastTime ticsRun quitAfter with
    | Except.error e =>
      IO.eprintln s!"doom: {e}"
      iShutdown
      rc := 1
      break
    | Except.ok (gs', inp', lastTime', ticsRun', quit) =>
      if quit then
        iShutdown
        rc := 0
        break
      else
        match renderLevelFrameWithFuzz wad gs' fuzzpos with
        | Except.error e =>
          IO.eprintln s!"doom: {e}"
          iShutdown
          rc := 1
          break
        | Except.ok (fb, pal, fp) =>
          iFinishUpdate fb.pixels pal
          gs := gs'
          inp := inp'
          lastTime := lastTime'
          ticsRun := ticsRun'
          fuzzpos := fp
  pure rc

def main (args : List String) : IO UInt32 := do
  match parseArgs args with
  | Except.error _ => do
    IO.eprintln usage
    pure 2
  | Except.ok da => do
    match ← spawnLive da.iwad with
    | Except.error e => do
      IO.eprintln s!"doom: {e}"
      pure 1
    | Except.ok (wad, gs0) => do
      let rc ← iInitGraphics
      if rc != 0 then
        IO.eprintln s!"doom: I_InitGraphics failed ({rc})"
        iShutdown
        return 1
      -- Bootstrap one tic with no display (`D_DoomLoop`).
      match ← tryRunTics gs0 InputState.init 0 0 da.quitAfterTics with
      | Except.error e => do
        IO.eprintln s!"doom: {e}"
        iShutdown
        pure 1
      | Except.ok (gs, inp, _last, ticsRun, quit) =>
        if quit then
          iShutdown
          return 0
        let lastTime ← iGetTime
        frameLoop wad gs inp lastTime ticsRun 0 da.quitAfterTics
