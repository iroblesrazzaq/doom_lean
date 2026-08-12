import Doom.Playsim.Demo
import Doom.Playsim.GameState
import Doom.Playsim.Player
import Doom.Playsim.PlayerThink
import Doom.Playsim.Random
import Doom.Playsim.Think

/-!
# Doom.Playsim.Tick

`P_Ticker` / `G_Ticker` (level + demoplayback) and minimal `ST_Ticker`.
-/

namespace Doom.Playsim.Tick

open Doom.Playsim.Demo
open Doom.Playsim.GameState
open Doom.Playsim.Player
open Doom.Playsim.PlayerThink
open Doom.Playsim.Random
open Doom.Playsim.Think

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

/--
`P_UpdateSpecials` — animates flats/textures (not in playsim trace).
No traced effect at DEMO1 tic 0; intentionally empty.
-/
def updateSpecials (gs : GameState) : GameState := gs

/--
`P_RespawnSpecials` — commercial item respawn queue. Empty / early-out when
queue idle (Doom 1 shareware path).
-/
def respawnSpecials (gs : GameState) : GameState := gs

/-- `ST_Ticker`: always draws one `M_Random` into `st_randomnumber` (untraced). -/
def stTicker (gs : GameState) : GameState :=
  let (_, rng) := mRandom gs.rng
  { gs with rng }

/--
`AM_Ticker` — no-op while automap inactive (default). Traced state unaffected.
-/
def amTicker (gs : GameState) : GameState := gs

/--
`HU_Ticker` — message/chat only; no traced fields at tic 0. Stubbed no-op.
-/
def huTicker (gs : GameState) : GameState := gs

/-- `P_Ticker`. -/
def pTicker (gs0 : GameState) : Except String GameState := do
  let mut gs := gs0
  let mut i : Nat := 0
  while i < MAXPLAYERS do
    match gs.playeringame[i]? with
    | some true =>
      gs ← playerThink gs i
    | _ => pure ()
    i := i + 1
  gs ← runThinkers gs
  gs := updateSpecials gs
  gs := respawnSpecials gs
  pure { gs with leveltime := gs.leveltime + 1 }

/-- Read demo ticcmds into each in-game player's `cmd`. -/
def readDemoCmds (gs0 : GameState) : Except String GameState := do
  let mut gs := gs0
  let mut i : Nat := 0
  while i < MAXPLAYERS do
    match gs.playeringame[i]? with
    | some true =>
      let (cursor, cmd) ← readDemoTiccmd gs.demoBytes gs.demoCursor
      match gs.players[i]? with
      | none => throw "G_Ticker: missing player"
      | some p =>
        gs := {
          gs with
          demoCursor := cursor
          players := setArr gs.players i { p with cmd }
        }
    | _ => pure ()
    i := i + 1
  pure gs

/--
`G_Ticker` for `GS_LEVEL` + `demoplayback`:
read ticcmds → `P_Ticker` → `ST_Ticker` → `AM_Ticker` → `HU_Ticker`.
Dump point is immediately after this returns (`docs/TRACE.md` §1).
-/
def gTicker (gs0 : GameState) : Except String GameState := do
  let mut gs := gs0
  if gs.demoplayback then
    gs ← readDemoCmds gs
  gs ← pTicker gs
  gs := stTicker gs
  gs := amTicker gs
  gs := huTicker gs
  pure gs

end Doom.Playsim.Tick
