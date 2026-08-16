import Doom.Harness.DisplaySim
import Doom.Harness.TraceFormat
import Doom.Render
import Doom.Render.Gfx.Flat
import Doom.Playsim.Demo
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Spawn
import Doom.Playsim.Spec
import Doom.Playsim.Tick
import Doom.Playsim.TraceEmit
import Doom.Wad

/-!
# Doom.Harness.Real

Load IWAD + DEMO1, spawn E1M5, run N tics mirroring `D_DoomLoop`:
bootstrap one `G_Ticker` + emit (tic 0, no display), then for each remaining
tic: `G_Ticker` + emit + `DisplaySim.onFrame` (wipe check / melt RNG).
-/

namespace Doom.Harness.Real

open Doom.Harness.DisplaySim
open Doom.Harness.TraceFormat
open Doom.Render
open Doom.Render.Gfx.Flat
open Doom.Playsim.Demo
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Spawn
open Doom.Playsim.Spec
open Doom.Playsim.Tick
open Doom.Playsim.TraceEmit
open Doom.Wad

def loadMap (wad : WadDirectory) (label : String) : Except String LevelData := do
  match checkNumForName wad label with
  | none => throw s!"missing {label}"
  | some idx =>
    let things ← mapLumpData wad idx ML_THINGS
    let linedefs ← mapLumpData wad idx ML_LINEDEFS
    let sidedefs ← mapLumpData wad idx ML_SIDEDEFS
    let vertexes ← mapLumpData wad idx ML_VERTEXES
    let segs ← mapLumpData wad idx ML_SEGS
    let ssectors ← mapLumpData wad idx ML_SSECTORS
    let nodes ← mapLumpData wad idx ML_NODES
    let sectors ← mapLumpData wad idx ML_SECTORS
    let reject ← mapLumpData wad idx ML_REJECT
    let blockmap ← mapLumpData wad idx ML_BLOCKMAP
    buildLevel things linedefs sidedefs vertexes segs ssectors nodes sectors reject blockmap

def mapLabel (episode map : UInt8) : String :=
  s!"E{episode}M{map}"

private def runOneTic (gs0 : GameState) (g : Nat) :
    Except String (GameState × TicRecord) := do
  let gs := { gs0 with gametic := g.toUInt32 }
  let gs ← gTicker gs
  let rec ← emitTicRecord gs
  pure (gs, rec)

/--
Spawn from IWAD + demo lump, run `tics` iterations mirroring `D_DoomLoop`,
write `pathBase.trc` / `pathBase.dig`.
-/
def runReal (iwadPath : System.FilePath) (demoName : String) (tics : Nat)
    (pathBase : System.FilePath)
    (fbDir : Option System.FilePath := none)
    (fbTics : Array Nat := #[]) : IO (Except String Unit) := do
  try
    let wad ← loadFile iwadPath
    let demoBytes ←
      match checkNumForName wad demoName with
      | none =>
        return Except.error s!"demo lump {demoName} not found"
      | some idx =>
        match lumpData wad idx with
        | Except.error e => return Except.error e
        | Except.ok bytes => pure bytes
    match parseHeader demoBytes with
    | Except.error e => return Except.error e
    | Except.ok hdr =>
      let label := mapLabel hdr.episode hdr.map
      match loadMap wad label with
      | Except.error e => return Except.error e
      | Except.ok level =>
        match setupSpawnedLevel level (hdr.skill.toUInt32.toInt32) hdr.playeringame
            hdr.consoleplayer.toNat with
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
          let picAnims ←
            match initPicAnims resolve with
            | Except.error e => return Except.error e
            | Except.ok anims => pure anims
          let gs0 := {
            gsSpawn with
            picAnims
            demoBytes
            demoCursor := 13
            demoplayback := true
            gametic := 0
          }
          let mut gs := gs0
          let mut disp := initDisplay
          let mut records : Array TicRecord := #[]
          let mut err : Option String := none
          let mut fuzzpos : Nat := 0
          -- Bootstrap: tic 0 via TryRunTics, no display (`D_DoomLoop`).
          if tics > 0 && err.isNone then
            match runOneTic gs 0 with
            | Except.error e => err := some e
            | Except.ok (gs1, rec) =>
              gs := gs1
              records := records.push rec
          -- Main loop: remaining tics each followed by display/wipe step.
          let mut g : Nat := 1
          while g < tics && err.isNone do
            match runOneTic gs g with
            | Except.error e => err := some e
            | Except.ok (gs1, rec) =>
              gs := gs1
              records := records.push rec
              -- In-level DEMO1: `gamestate == GS_LEVEL` (spawn already loaded).
              let (disp', rng') := onFrame disp gs.rng gsLevel
              disp := disp'
              gs := { gs with rng := rng' }
              match ← dumpIfRequested wad gs fbDir fbTics fuzzpos with
              | Except.error e => err := some e
              | Except.ok fp => fuzzpos := fp
            g := g + 1
          match err with
          | some e => pure (Except.error e)
          | none => do
            writeTracePair pathBase records
            pure (Except.ok ())
  catch e =>
    pure (Except.error (toString e))

end Doom.Harness.Real
