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
  /-- `sector_t::soundtraversed` (`P_RecursiveSound`). -/
  soundtraversed : Int32
  /-- Per-sector `validcount` stamp (`P_RecursiveSound`). -/
  validcount : Int32
  /-- Line indices (copied from geometry). -/
  lines : Array UInt32
  /-- Mutable floor flat (copied from geometry; `EV_DoPlat` may replace). -/
  floorpic : ByteArray

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

/-- `vldoor_t` payload (`p_spec.h`). `specialdata` stores this array index. -/
structure VerticalDoor where
  sector : UInt32
  type_ : Int32
  topheight : Int32
  speed : Int32
  direction : Int32
  topwait : Int32
  topcountdown : Int32
  deriving Repr

/-- `p_spec.h` `MAXPLATS`. -/
def MAXPLATS : Nat := 30

/-- `floormove_t` payload (`p_spec.h`). `specialdata` stores this array index. -/
structure FloorMove where
  sector : UInt32
  type_ : Int32
  crush : Bool
  direction : Int32
  speed : Int32
  floordestheight : Int32
  deriving Repr

/-- `plat_t` payload (`p_spec.h`). `specialdata` / `activePlats` store this array index. -/
structure Plat where
  sector : UInt32
  speed : Int32
  low : Int32
  high : Int32
  wait : Int32
  count : Int32
  status : Int32
  oldstatus : Int32
  crush : Bool
  tag : Int32
  type_ : Int32
  deriving Repr

/-- Untraced `ST_Ticker` face widget state (`st_stuff.c` statics). Not in TraceFormat. -/
structure StFaceState where
  faceindex : Int32
  facecount : Int32
  oldhealth : Int32
  oldweaponsowned : Array Int32
  lastattackdown : Int32
  priority : Int32
  /-- `ST_calcPainOffset` `lastcalc`. -/
  painOffset : Int32
  /-- `ST_calcPainOffset` static `oldhealth`. -/
  painOldhealth : Int32
  /-- `weaponowned` snapshot at `ST_initData`; arms icons use it for backing-copy. -/
  spawnWeaponsOwned : Array Int32

/-- Untraced `anim_t` (`p_spec.c`). Not in TraceFormat. -/
structure PicAnim where
  istexture : Bool
  picnum : Int32
  basepic : Int32
  numpics : Int32
  speed : Int32
  deriving Repr

/-- `ST_initData` / `ST_Start` defaults before copying live `weaponowned`. -/
def StFaceState.init : StFaceState := {
  faceindex := 0
  facecount := 0
  oldhealth := -1
  oldweaponsowned := Array.replicate NUMWEAPONS 0
  lastattackdown := -1
  priority := 0
  painOffset := 0
  painOldhealth := -1
  spawnWeaponsOwned := Array.replicate NUMWEAPONS 0
}

/-- Untraced `HU_Ticker` message widget (`hu_stuff.c` statics). Not in TraceFormat. -/
structure HuState where
  messageOn : Bool
  messageCounter : Int32
  messageText : String

/-- `HU_Start` message widget defaults (no title/chat). -/
def HuState.init : HuState := {
  messageOn := false
  messageCounter := 0
  messageText := ""
}

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
  verticalDoors : Array VerticalDoor
  floors : Array FloorMove
  plats : Array Plat
  /-- `activeplats[MAXPLATS]`; empty slot = `-1`, else `plats` payload index. -/
  activePlats : Array Int32
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
  /-- Global `validcount` (`r_state.h`); bumped by sight/path checks. -/
  validcount : Int32
  /-- Per-linedef `line_t::validcount` stamp. -/
  lineValidcount : Array Int32
  /-- Blockmap thing chains (`blocklinks`); `-1` = empty. Size `width*height`. -/
  blocklinks : Array Int32
  /-- Status-bar face widget statics (`ST_Ticker`); untraced. -/
  stFace : StFaceState
  /-- Heads-up message widget statics (`HU_Ticker`); untraced. -/
  hu : HuState
  /-- Untraced `anims[0 .. lastanim)` from `P_InitPicAnims`. -/
  picAnims : Array PicAnim := #[]
  /-- Untraced `flattranslation`; empty means identity at draw. -/
  flattranslation : Array Int32 := #[]

def mkSectorRuntime (s : Sector) : SectorRuntime := {
  lightlevel := s.lightlevel
  special := s.special
  tag := s.tag
  floorheight := s.floorheight
  ceilingheight := s.ceilingheight
  thinglist := -1
  specialdata := -1
  soundtarget := -1
  soundtraversed := 0
  validcount := 0
  lines := s.lines
  floorpic := s.floorpic
}

def initFromLevel (level : LevelData) (skill : Int32) (playeringame : Array Bool)
    (consoleplayer : Nat) : GameState :=
  let sectors := level.sectors.map mkSectorRuntime
  let players := Array.replicate MAXPLAYERS Player.empty
  let bmapCells :=
    (level.blockmap.width * level.blockmap.height).toNatClampNeg
  { level
    sectors
    players
    playeringame
    mobjs := #[]
    thinkers := #[]
    lightFlashes := #[]
    strobes := #[]
    glows := #[]
    verticalDoors := #[]
    floors := #[]
    plats := #[]
    activePlats := Array.replicate MAXPLATS (-1 : Int32)
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
    validcount := 0
    lineValidcount := Array.replicate level.lines.size (0 : Int32)
    blocklinks := Array.replicate bmapCells (-1 : Int32)
    stFace := StFaceState.init
    hu := HuState.init
    picAnims := #[]
    flattranslation := #[]
  }

/-- `ST_initData` after console `P_SpawnPlayer`: copy live `weaponowned`. -/
def stInitData (gs : GameState) : GameState :=
  match gs.players[gs.consoleplayer]? with
  | none => { gs with stFace := StFaceState.init }
  | some plyr =>
    { gs with stFace := {
        StFaceState.init with
        oldweaponsowned := plyr.weaponowned
        spawnWeaponsOwned := plyr.weaponowned
      } }

/-- `HU_Start` message path only (no title/chat/automap). -/
def huStart (gs : GameState) : GameState :=
  { gs with hu := HuState.init }

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
