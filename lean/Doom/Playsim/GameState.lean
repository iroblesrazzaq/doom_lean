import Doom.Playsim.Level
import Doom.Playsim.Random
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Thinker

/-!
# Doom.Playsim.GameState

Level runtime state: players, mobjs, thinker list (insertion order), sector
runtime fields, RNG, trace-id counter, leveltime.
-/


namespace Doom.Playsim.GameState

open Doom.Playsim.Level
open Doom.Playsim.Random
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Thinker

/-- Skill ordinals (`skill_t` in `doomdef` / `d_mode.h`). -/
def sk_baby : Int32 := 0
def sk_easy : Int32 := 1
def sk_medium : Int32 := 2
def sk_hard : Int32 := 3
def sk_nightmare : Int32 := 4

/-- Sector runtime overlay (mutable light / special / thinglist / specialdata). -/
structure SectorRuntime where
  lightlevel : Int32
  special : Int32
  tag : Int32
  floorheight : Int32
  ceilingheight : Int32
  /-- Head of sector thinglist (`mobj` index), or `-1`. -/
  thinglist : Int32
  /-- Attached special thinker payload index, or `-1` (unused at P2a-i spawn beyond clear). -/
  specialdata : Int32
  /-- Sector `soundtarget` mobj index, or `-1`. -/
  soundtarget : Int32
  /-- Line indices (copied from geometry). -/
  lines : Array UInt32
  deriving Repr

structure LightFlash where
  sector : UInt32
  count : Int32
  maxlight : Int32
  minlight : Int32
  maxtime : Int32
  mintime : Int32
  deriving Repr

structure StrobeFlash where
  sector : UInt32
  count : Int32
  minlight : Int32
  maxlight : Int32
  darktime : Int32
  brighttime : Int32
  deriving Repr

structure Glow where
  sector : UInt32
  minlight : Int32
  maxlight : Int32
  direction : Int32
  deriving Repr

structure GameState where
  level : LevelData
  sectors : Array SectorRuntime
  players : Array Player
  playeringame : Array Bool
  mobjs : Array Mobj
  thinkers : Array Thinker
  lightFlashes : Array LightFlash
  strobes : Array StrobeFlash
  glows : Array Glow
  rng : RandomState
  /-- Next trace id to assign (`P_AddThinker`); reset to 1 at level load. -/
  traceIdCounter : UInt32
  leveltime : UInt32
  /-- Current gametic at dump point (not advanced inside `G_Ticker`). -/
  gametic : UInt32
  /-- Demo lump bytes (header + cmds); cursor indexes next cmd byte. -/
  demoBytes : ByteArray
  demoCursor : Nat
  demoplayback : Bool
  gameskill : Int32
  consoleplayer : Nat
  deathmatch : Bool
  nomonsters : Bool
  netgame : Bool
  /-- `true` when `gamemode == commercial` (Doom II); shareware/registered = false. -/
  commercial : Bool
  totalkills : Int32
  totalitems : Int32
  totalsecret : Int32

def mkSectorRuntime (s : Sector) : SectorRuntime := {
  lightlevel := s.lightlevel
  special := s.special
  tag := s.tag
  floorheight := s.floorheight
  ceilingheight := s.ceilingheight
  thinglist := -1
  specialdata := -1
  soundtarget := -1
  lines := s.lines
}

def initFromLevel (level : LevelData) (skill : Int32) (playeringame : Array Bool)
    (consoleplayer : Nat) : GameState :=
  let sectors := level.sectors.map mkSectorRuntime
  let players := Array.replicate MAXPLAYERS Player.empty
  { level
    sectors
    players
    playeringame
    mobjs := #[]
    thinkers := #[]
    lightFlashes := #[]
    strobes := #[]
    glows := #[]
    rng := clearRandom
    traceIdCounter := 1
    leveltime := 0
    gametic := 0
    demoBytes := ByteArray.empty
    demoCursor := 0
    demoplayback := false
    gameskill := skill
    consoleplayer
    deathmatch := false
    nomonsters := false
    netgame := false
    commercial := false
    totalkills := 0
    totalitems := 0
    totalsecret := 0
  }

/-- Proof-free array set. -/
def arrSet {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

/-- Append thinker at list tail (`P_AddThinker`); assigns `trace_id`. -/
def addThinker (gs : GameState) (func : UInt32) (payload : UInt32) : GameState × UInt32 :=
  let tid := gs.traceIdCounter
  let th : Thinker := { traceId := tid, func, payload }
  let gs := { gs with
    thinkers := gs.thinkers.push th
    traceIdCounter := tid + 1
  }
  (gs, tid)

end Doom.Playsim.GameState
