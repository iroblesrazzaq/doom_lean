import Doom.Playsim.Angle
import Doom.Playsim.Bsp
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Level
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Psprite
import Doom.Playsim.Random
import Doom.Playsim.Thinker

/-!
# Doom.Playsim.Spawn

Level spawn path: `P_SpawnMobj` / `P_SpawnMapThing` / `P_SpawnPlayer` /
`P_SpawnSpecials`, matching oracle call order and RNG draws.
-/

namespace Doom.Playsim.Spawn

open Doom.Playsim.Angle
open Doom.Playsim.Bsp
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Level
open Doom.Playsim.MapUtil
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Psprite
open Doom.Playsim.Random
open Doom.Playsim.Thinker

/-- `p_local.h` `ONFLOORZ` = `INT_MIN`. -/
def ONFLOORZ : Int32 := Int32.minValue
/-- `p_local.h` `ONCEILINGZ` = `INT_MAX`. -/
def ONCEILINGZ : Int32 := Int32.maxValue

def MTF_EASY : Int32 := 1
def MTF_NORMAL : Int32 := 2
def MTF_HARD : Int32 := 4
def MTF_AMBUSH : Int32 := 8
def MTF_MULTIPLAYER : Int32 := 16

def STROBEBRIGHT : Int32 := 5
def FASTDARK : Int32 := 15
def SLOWDARK : Int32 := 35

/-- `MT_SKULL` ordinal in `mobjinfo`. -/
def MT_SKULL : Int32 := 18

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  GameState.arrSet arr i v

private def idx (x : Int32) : Nat := x.toNatClampNeg

/-- Skill bit for `P_SpawnMapThing` (`MTF_*`). -/
def skillBit (skill : Int32) : UInt32 :=
  if skill == sk_baby then 1
  else if skill == sk_nightmare then 4
  else
    let shift := ((skill - 1).toUInt32) &&& (31 : UInt32)
    (1 : UInt32) <<< shift

/-- True when mapthing options allow spawn at this skill. -/
def skillAllows (options skill : Int32) : Bool :=
  let bit := skillBit skill
  (options.toUInt32 &&& bit) != 0

/-- Mapthing tics randomization: `1 + (P_Random % tics)` when `tics > 0`. -/
def randomizeSpawnTics (tics : Int32) (rng : RandomState) : Int32 × RandomState :=
  if tics > 0 then
    let (r, rng) := pRandom rng
    (1 + (r % tics), rng)
  else
    (tics, rng)

/-- `getNextSector` for min-light search. -/
def getNextSector (ld : Line) (secIdx : Nat) : Option Nat :=
  if (ld.flags &&& ML_TWOSIDED) == 0 then
    none
  else if ld.frontsector >= 0 && idx ld.frontsector == secIdx then
    if ld.backsector >= 0 then some (idx ld.backsector) else none
  else if ld.frontsector >= 0 then
    some (idx ld.frontsector)
  else
    none

/-- `P_FindMinSurroundingLight`. -/
def findMinSurroundingLight (gs : GameState) (secIdx : Nat) (maxLight : Int32) : Int32 :=
  Id.run do
    let mut minL := maxLight
    match gs.sectors[secIdx]? with
    | none => pure minL
    | some sec =>
      let mut i : Nat := 0
      while i < sec.lines.size do
        match sec.lines[i]? with
        | none => pure ()
        | some li =>
          match gs.level.lines[li.toNat]? with
          | none => pure ()
          | some ld =>
            match getNextSector ld secIdx with
            | none => pure ()
            | some otherIdx =>
              match gs.sectors[otherIdx]? with
              | none => pure ()
              | some other =>
                if other.lightlevel < minL then
                  minL := other.lightlevel
        i := i + 1
      pure minL

/--
`P_SetThingPosition` (delegates to `MapUtil` — sector + blockmap links).
-/
def setThingPosition (gs : GameState) (mobjIdx : Nat) : Except String GameState :=
  MapUtil.setThingPosition gs mobjIdx

/-- `P_SpawnMobj` — does **not** invoke state action routines. -/
def spawnMobj (gs0 : GameState) (x y z : Int32) (typeId : Int32) :
    Except String (GameState × Nat) := do
  let ti := idx typeId
  if typeId < 0 || ti >= mobjinfo.size then
    throw s!"spawnMobj: bad type {typeId}"
  match mobjinfo[ti]? with
  | none => throw "spawnMobj: missing mobjinfo"
  | some (info : MobjInfo) =>
    match states[info.spawnstate.toNat]? with
    | none => throw "spawnMobj: missing spawn state"
    | some (st : State) =>
      let (lastlookRaw, rng) := pRandom gs0.rng
      let lastlook := lastlookRaw % (MAXPLAYERS.toInt32)
      let reactiontime : Int32 :=
        if gs0.gameskill != sk_nightmare then info.reactiontime else 0
      let mo : Mobj := {
        Mobj.empty with
        typeId := typeId
        x := x
        y := y
        radius := info.radius
        height := info.height
        flags := info.flags
        health := info.spawnhealth
        reactiontime := reactiontime
        lastlook := lastlook
        state := info.spawnstate
        tics := st.tics
        sprite := st.sprite
        frame := st.frame
      }
      let mobjIdx := gs0.mobjs.size
      let gs1 := { gs0 with mobjs := gs0.mobjs.push mo, rng }
      let gs2 ← setThingPosition gs1 mobjIdx
      match gs2.mobjs[mobjIdx]? with
      | none => throw "spawnMobj: lost mobj"
      | some mo2 =>
        match gs2.level.subsectors[mo2.subsector.toNat]? with
        | none => throw "spawnMobj: subsector missing after link"
        | some ss =>
          match gs2.sectors[ss.sector.toNat]? with
          | none => throw "spawnMobj: sector missing"
          | some sec =>
            let floorz := sec.floorheight
            let ceilingz := sec.ceilingheight
            let z' :=
              if z == ONFLOORZ then floorz
              else if z == ONCEILINGZ then ceilingz - info.height
              else z
            let mo3 := { mo2 with floorz, ceilingz, z := z' }
            let gs3 := { gs2 with mobjs := setArr gs2.mobjs mobjIdx mo3 }
            let (gs4, tid) := addThinker gs3 THF_MOBJ mobjIdx.toUInt32
            match gs4.mobjs[mobjIdx]? with
            | none => throw "spawnMobj: lost mobj after thinker"
            | some mo4 =>
              let mo5 := { mo4 with traceId := tid }
              pure ({ gs4 with mobjs := setArr gs4.mobjs mobjIdx mo5 }, mobjIdx)

/-- `P_SpawnPlayer`. -/
def spawnPlayer (gs0 : GameState) (mthing : Thing) : Except String GameState := do
  if mthing.typeId == 0 then
    pure gs0
  else
    let pnum := idx (mthing.typeId - 1)
    if pnum >= MAXPLAYERS then
      throw s!"spawnPlayer: bad player type {mthing.typeId}"
    match gs0.playeringame[pnum]? with
    | some true =>
      let p0 := match gs0.players[pnum]? with | some x => x | none => Player.empty
      let p1 := if p0.playerstate == PST_REBORN then playerReborn p0 else p0
      let x := mthing.x <<< 16
      let y := mthing.y <<< 16
      let gs1 := { gs0 with players := setArr gs0.players pnum p1 }
      let (gs2, mobjIdx) ← spawnMobj gs1 x y ONFLOORZ 0
      match gs2.mobjs[mobjIdx]? with
      | none => throw "spawnPlayer: mobj missing"
      | some mo =>
        let angle := ANG45 * (mthing.angle / 45).toUInt32
        let flags :=
          if mthing.typeId > 1 then
            mo.flags ||| (((mthing.typeId - 1).toUInt32) <<< MF_TRANSSHIFT)
          else
            mo.flags
        let mo' := { mo with angle, flags, player := pnum.toInt32, health := p1.health }
        -- C: `P_SpawnPlayer` sets `viewheight` only; `viewz` stays 0 from `G_PlayerReborn` memset.
        let p2 ← setupPsprites {
          p1 with
          mo := mobjIdx.toInt32
          playerstate := PST_LIVE
          refire := 0
          viewheight := VIEWHEIGHT
        }
        pure {
          gs2 with
          players := setArr gs2.players pnum p2
          mobjs := setArr gs2.mobjs mobjIdx mo'
        }
    | _ => pure gs0

/-- Resolve `mobjinfo` index by `doomednum`. -/
def findTypeByDoomedNum (doomed : Int32) : Option Int32 :=
  Id.run do
    let mut i : Nat := 0
    let mut found : Option Int32 := none
    while i < mobjinfo.size && found.isNone do
      match mobjinfo[i]? with
      | some info =>
        if info.doomednum == doomed then
          found := some i.toInt32
      | none => pure ()
      i := i + 1
    pure found

/-- `P_SpawnMapThing`. -/
def spawnMapThing (gs0 : GameState) (mthing : Thing) : Except String GameState := do
  if mthing.typeId == 11 then
    -- deathmatch start: record-only stub (array not stored in P2a-i)
    pure gs0
  else if mthing.typeId <= 0 then
    pure gs0
  else if mthing.typeId <= 4 then
    -- player start: always record conceptually; spawn only when !deathmatch
    if gs0.deathmatch then
      pure gs0
    else
      spawnPlayer gs0 mthing
  else if !gs0.netgame && (mthing.options &&& MTF_MULTIPLAYER) != 0 then
    pure gs0
  else if !skillAllows mthing.options gs0.gameskill then
    pure gs0
  else
    match findTypeByDoomedNum mthing.typeId with
    | none => throw s!"spawnMapThing: unknown type {mthing.typeId}"
    | some typeId =>
      match mobjinfo[idx typeId]? with
      | none => throw "spawnMapThing: mobjinfo missing"
      | some (info : MobjInfo) =>
        if gs0.deathmatch && (info.flags &&& MF_NOTDMATCH) != 0 then
          pure gs0
        else if gs0.nomonsters && (typeId == MT_SKULL || (info.flags &&& MF_COUNTKILL) != 0) then
          pure gs0
        else
          let x := mthing.x <<< 16
          let y := mthing.y <<< 16
          let z := if (info.flags &&& MF_SPAWNCEILING) != 0 then ONCEILINGZ else ONFLOORZ
          let (gs1, mobjIdx) ← spawnMobj gs0 x y z typeId
          match gs1.mobjs[mobjIdx]? with
          | none => throw "spawnMapThing: mobj missing"
          | some mo =>
            let (tics, rng) := randomizeSpawnTics mo.tics gs1.rng
            let totalkills :=
              if (mo.flags &&& MF_COUNTKILL) != 0 then gs1.totalkills + 1 else gs1.totalkills
            let totalitems :=
              if (mo.flags &&& MF_COUNTITEM) != 0 then gs1.totalitems + 1 else gs1.totalitems
            let angle := ANG45 * (mthing.angle / 45).toUInt32
            let flags :=
              if (mthing.options &&& MTF_AMBUSH) != 0 then mo.flags ||| MF_AMBUSH else mo.flags
            let mo' := { mo with tics, angle, flags }
            pure {
              gs1 with
              mobjs := setArr gs1.mobjs mobjIdx mo'
              rng
              totalkills
              totalitems
            }

/-- Clear sector special after spawning a light thinker. -/
def clearSectorSpecial (gs : GameState) (secIdx : Nat) : GameState :=
  match gs.sectors[secIdx]? with
  | none => gs
  | some sec => { gs with sectors := setArr gs.sectors secIdx { sec with special := 0 } }

def spawnLightFlash (gs0 : GameState) (secIdx : Nat) : GameState :=
  match gs0.sectors[secIdx]? with
  | none => gs0
  | some sec =>
    let gs1 := clearSectorSpecial gs0 secIdx
    let payload := gs1.lightFlashes.size.toUInt32
    let (gs2, _) := addThinker gs1 THF_LIGHTFLASH payload
    let minlight := findMinSurroundingLight gs2 secIdx sec.lightlevel
    let (r, rng) := pRandom gs2.rng
    let flash : LightFlash := {
      sector := secIdx.toUInt32
      maxlight := sec.lightlevel
      minlight := minlight
      maxtime := 64
      mintime := 7
      count := (r &&& (64 : Int32)) + 1
    }
    { gs2 with lightFlashes := gs2.lightFlashes.push flash, rng }

def spawnStrobeFlash (gs0 : GameState) (secIdx : Nat) (fastOrSlow : Int32) (inSync : Bool) :
    GameState :=
  match gs0.sectors[secIdx]? with
  | none => gs0
  | some sec =>
    let payload := gs0.strobes.size.toUInt32
    let (gs1, _) := addThinker gs0 THF_STROBEFLASH payload
    let min0 := findMinSurroundingLight gs1 secIdx sec.lightlevel
    let minlight := if min0 == sec.lightlevel then (0 : Int32) else min0
    let gs2 := clearSectorSpecial gs1 secIdx
    let (count, rng) :=
      if !inSync then
        let (r, rng) := pRandom gs2.rng
        ((r &&& (7 : Int32)) + 1, rng)
      else
        ((1 : Int32), gs2.rng)
    let flash : StrobeFlash := {
      sector := secIdx.toUInt32
      darktime := fastOrSlow
      brighttime := STROBEBRIGHT
      maxlight := sec.lightlevel
      minlight := minlight
      count := count
    }
    { gs2 with strobes := gs2.strobes.push flash, rng }

def spawnGlowingLight (gs0 : GameState) (secIdx : Nat) : GameState :=
  match gs0.sectors[secIdx]? with
  | none => gs0
  | some sec =>
    let payload := gs0.glows.size.toUInt32
    let (gs1, _) := addThinker gs0 THF_GLOW payload
    let minlight := findMinSurroundingLight gs1 secIdx sec.lightlevel
    let g : Glow := {
      sector := secIdx.toUInt32
      minlight := minlight
      maxlight := sec.lightlevel
      direction := -1
    }
    let gs2 := clearSectorSpecial gs1 secIdx
    { gs2 with glows := gs2.glows.push g }

/-- `P_SpawnSpecials` (sector specials only; line scrollers deferred). -/
def spawnSpecials (gs0 : GameState) : GameState :=
  Id.run do
    let mut gs := gs0
    let mut i : Nat := 0
    while i < gs.sectors.size do
      match gs.sectors[i]? with
      | none => pure ()
      | some sec =>
        if sec.special != 0 then
          let sp := sec.special
          if sp == 1 then
            gs := spawnLightFlash gs i
          else if sp == 2 then
            gs := spawnStrobeFlash gs i FASTDARK false
          else if sp == 3 then
            gs := spawnStrobeFlash gs i SLOWDARK false
          else if sp == 4 then
            gs := spawnStrobeFlash gs i FASTDARK false
            match gs.sectors[i]? with
            | some s => gs := { gs with sectors := setArr gs.sectors i { s with special := 4 } }
            | none => pure ()
          else if sp == 8 then
            gs := spawnGlowingLight gs i
          else if sp == 9 then
            gs := { gs with totalsecret := gs.totalsecret + 1 }
          else if sp == 12 then
            gs := spawnStrobeFlash gs i SLOWDARK true
          else if sp == 13 then
            gs := spawnStrobeFlash gs i FASTDARK true
          else
            pure ()
      i := i + 1
    pure gs

/-- Non-commercial Doom II thing filter from `P_LoadThings` (break on first). -/
def isDoom2OnlyThing (typeId : Int32) : Bool :=
  typeId == 68 || typeId == 64 || typeId == 88 || typeId == 89 || typeId == 69
    || typeId == 67 || typeId == 71 || typeId == 65 || typeId == 66 || typeId == 84

/--
Spawn all map things then specials. Caller must have already cleared RNG
(`M_ClearRandom` / `G_InitNew`) and set `traceIdCounter = 1`.
-/
def spawnLevelThings (gs0 : GameState) : Except String GameState := do
  let mut gs := gs0
  let mut i : Nat := 0
  let mut stop := false
  while i < gs.level.things.size && !stop do
    match gs.level.things[i]? with
    | none => throw "spawnLevelThings: missing thing"
    | some mt =>
      -- Oracle only aborts the remaining THINGS loop in non-commercial.
      if !gs.commercial && isDoom2OnlyThing mt.typeId then
        stop := true
      else
        gs ← spawnMapThing gs mt
    i := i + 1
  pure (spawnSpecials gs)

/--
Full DEMO1-style setup after geometry load:
`M_ClearRandom` → reset thinkers/trace ids → spawn things → spawn specials.
-/
def setupSpawnedLevel (level : LevelData) (skill : Int32) (playeringame : Array Bool)
    (consoleplayer : Nat) : Except String GameState := do
  let mut gs := initFromLevel level skill playeringame consoleplayer
  gs := { gs with rng := clearRandom }
  let mut players := gs.players
  let mut pi : Nat := 0
  while pi < MAXPLAYERS do
    match players[pi]? with
    | some p => players := setArr players pi { p with playerstate := PST_REBORN }
    | none => pure ()
    pi := pi + 1
  gs := { gs with players, traceIdCounter := 1, thinkers := #[], mobjs := #[] }
  spawnLevelThings gs

end Doom.Playsim.Spawn
