import Doom.Harness.TraceFormat
import Doom.Playsim.Demo
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Spawn
import Doom.Playsim.Tick
import Doom.Playsim.TraceEmit
import Doom.Wad

/-!
# Doom.Harness.Real

Load IWAD + DEMO1, spawn E1M5, run N `G_Ticker`s, emit candidate trace.
-/

namespace Doom.Harness.Real

open Doom.Harness.TraceFormat
open Doom.Playsim.Demo
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Spawn
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

/--
Spawn from IWAD + demo lump, run `tics` iterations of `G_Ticker`, write
`pathBase.trc` / `pathBase.dig`.
-/
def runReal (iwadPath : System.FilePath) (demoName : String) (tics : Nat)
    (pathBase : System.FilePath) : IO (Except String Unit) := do
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
        | Except.ok gs0 =>
          let gs0 := {
            gs0 with
            demoBytes
            demoCursor := 13
            demoplayback := true
            gametic := 0
          }
          let mut gs := gs0
          let mut records : Array TicRecord := #[]
          let mut g : Nat := 0
          let mut err : Option String := none
          while g < tics && err.isNone do
            gs := { gs with gametic := g.toUInt32 }
            match gTicker gs with
            | Except.error e => err := some e
            | Except.ok gs1 =>
              gs := gs1
              match emitTicRecord gs with
              | Except.error e => err := some e
              | Except.ok rec =>
                records := records.push rec
            g := g + 1
          match err with
          | some e => pure (Except.error e)
          | none => do
            writeTracePair pathBase records
            pure (Except.ok ())
  catch e =>
    pure (Except.error (toString e))

end Doom.Harness.Real
