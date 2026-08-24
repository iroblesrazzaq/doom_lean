import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Map
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Sound
import Doom.Playsim.Spawn
import Doom.Playsim.Tables
import Doom.Playsim.Thinker

/-!
# Doom.Playsim.Spec

Open subset of `p_spec.c` / `p_switch.c` / `p_doors.c` / `p_floor.c` / `p_plats.c`
for DEMO1: surrounding-ceiling search, `P_UseSpecialLine`, `EV_VerticalDoor` /
`T_VerticalDoor` (UP + wait + wait-expire + DOWN + closing-door reopen),
special 1 and yellow DR special 27, `EV_DoDoor` (`vld_normal` only), ceiling
UP/DOWN `T_MovePlane`, floor UP/DOWN `T_MovePlane`, `P_CrossSpecialLine` 1.9
missile early-out (non-player `MT_BRUISERSHOT`/`MT_TROOPSHOT`/`MT_HEADSHOT`/
`MT_ROCKET`/`MT_PLASMA`/`MT_BFG` identity before monster-ok), spec 1 / spec 27
(walk no-op) / spec 22 (`EV_DoPlat` `raiseToNearestAndChange` + `T_PlatRaise`
UP including pastdest / `P_RemoveActivePlat`) / spec 88 WR (`EV_DoPlat`
`downWaitUpStay` + `T_PlatRaise` DOWN resultOk only) / spec 90 WR
`EV_DoDoor(vld_normal)`. Plat crush-reverse, waiting/down/stasis pastdest,
player reverse-close on busy raise, plat-misuse, other door types stay loud-error.
-/

namespace Doom.Playsim.Spec

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Map hiding crossSpecialLine
open Doom.Playsim.MapUtil
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Sound
open Doom.Playsim.Tables
open Doom.Playsim.Thinker

/-- `DEH_String` — identity (no Dehacked). -/
def dehString (s : String) : String := s

/-- `animdef_t` (`p_spec.c`). `endname` / `startname` match C field order. -/
structure PicAnimDef where
  istexture : Bool
  endname : String
  startname : String
  speed : Int32

/-- Shareware + Doom II flat `animdefs[]`. Texture entries are omitted this chunk. -/
def animdefs : Array PicAnimDef := #[
  { istexture := false, endname := "NUKAGE3", startname := "NUKAGE1", speed := 8 },
  { istexture := false, endname := "FWATER4", startname := "FWATER1", speed := 8 },
  { istexture := false, endname := "SWATER4", startname := "SWATER1", speed := 8 },
  { istexture := false, endname := "LAVA4", startname := "LAVA1", speed := 8 },
  { istexture := false, endname := "BLOOD3", startname := "BLOOD1", speed := 8 },
  { istexture := false, endname := "RROCK08", startname := "RROCK05", speed := 8 },
  { istexture := false, endname := "SLIME04", startname := "SLIME01", speed := 8 },
  { istexture := false, endname := "SLIME08", startname := "SLIME05", speed := 8 },
  { istexture := false, endname := "SLIME12", startname := "SLIME09", speed := 8 }
]

/--
`P_InitPicAnims` for flats (`istexture == false`). `resolve` is Wad-free
`R_FlatNumForName` (`none` ≡ `W_CheckNumForName == -1`).
-/
def initPicAnims (resolve : String → Option Nat) : Except String (Array PicAnim) := do
  let mut out : Array PicAnim := #[]
  let mut i : Nat := 0
  while i < animdefs.size do
    match animdefs[i]? with
    | none => pure ()
    | some adef =>
      if !adef.istexture then do
        let startname := dehString adef.startname
        let endname := dehString adef.endname
        match resolve startname with
        | none => pure ()
        | some baseNat =>
          match resolve endname with
          | none => throw s!"R_FlatNumForName: {endname} not found"
          | some picNat =>
            let basepic := baseNat.toUInt32.toInt32
            let picnum := picNat.toUInt32.toInt32
            let numpics := picnum - basepic + (1 : Int32)
            if numpics < (2 : Int32) then
              throw s!"P_InitPicAnims: bad cycle from {startname} to {endname}"
            else
              out := out.push {
                istexture := false
                picnum
                basepic
                numpics
                speed := adef.speed
              }
    i := i + 1
  pure out

/-- `vldoor_e`. -/
def vld_normal : Int32 := 0
def vld_open : Int32 := 3

/-- `p_spec.h` `FLOORSPEED`. -/
def FLOORSPEED : Int32 := FRACUNIT

/-- `floor_e` `raiseFloor`. -/
def raiseFloor : Int32 := 3

/-- `floor_e` `turboLower`. -/
def turboLower : Int32 := 2

/-- `p_spec.h` `VDOORSPEED` / `VDOORWAIT`. -/
def VDOORSPEED : Int32 := 2 * FRACUNIT
def VDOORWAIT : Int32 := 150

/-- `result_e`. -/
def resultOk : Int32 := 0
def resultCrushed : Int32 := 1
def resultPastdest : Int32 := 2

/-- `P_FindLowestFloorSurrounding` — seed `sec->floorheight`. -/
def findLowestFloorSurrounding (gs : GameState) (secIdx : Nat) : Int32 :=
  Id.run do
    match gs.sectors[secIdx]? with
    | none => pure (0 : Int32)
    | some sec =>
      let mut floor := sec.floorheight
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
                if other.floorheight < floor then
                  floor := other.floorheight
        i := i + 1
      pure floor

/-- `P_FindHighestFloorSurrounding` — seed `-500*FRACUNIT`. -/
def findHighestFloorSurrounding (gs : GameState) (secIdx : Nat) : Int32 :=
  Id.run do
    let mut floor := (-500 : Int32) * FRACUNIT
    match gs.sectors[secIdx]? with
    | none => pure floor
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
                if other.floorheight > floor then
                  floor := other.floorheight
        i := i + 1
      pure floor

/-- `P_FindLowestCeilingSurrounding` — seed `INT_MAX`. -/
def findLowestCeilingSurrounding (gs : GameState) (secIdx : Nat) : Int32 :=
  Id.run do
    let mut height := Int32.maxValue
    match gs.sectors[secIdx]? with
    | none => pure height
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
                if other.ceilingheight < height then
                  height := other.ceilingheight
        i := i + 1
      pure height

/-- `P_RemoveThinker` for a `THF_VERTICALDOOR` payload (lazy `THF_REMOVED`). -/
def removeDoorThinker (gs : GameState) (payload : Nat) : Except String GameState := do
  let mut found := false
  let mut thinkers := gs.thinkers
  let mut i : Nat := 0
  while i < thinkers.size do
    match thinkers[i]? with
    | none => throw "T_VerticalDoor: bad thinker"
    | some th =>
      if th.func == THF_VERTICALDOOR && th.payload.toNat == payload then
        thinkers := GameState.arrSet thinkers i { th with func := THF_REMOVED }
        found := true
      pure ()
    i := i + 1
  if !found then
    throw s!"T_VerticalDoor: no thinker for payload {payload}"
  pure { gs with thinkers }

/-- `P_RemoveThinker` for a `THF_MOVEFLOOR` payload (lazy `THF_REMOVED`). -/
def removeFloorThinker (gs : GameState) (payload : Nat) : Except String GameState := do
  let mut found := false
  let mut thinkers := gs.thinkers
  let mut i : Nat := 0
  while i < thinkers.size do
    match thinkers[i]? with
    | none => throw "T_MoveFloor: bad thinker"
    | some th =>
      if th.func == THF_MOVEFLOOR && th.payload.toNat == payload then
        thinkers := GameState.arrSet thinkers i { th with func := THF_REMOVED }
        found := true
      pure ()
    i := i + 1
  if !found then
    throw s!"T_MoveFloor: no thinker for payload {payload}"
  pure { gs with thinkers }

/-- `T_MovePlane` — ceiling UP/DOWN and floor UP/DOWN. -/
def movePlane (gs0 : GameState) (secIdx : Nat) (speed dest : Int32)
    (crush : Bool) (floorOrCeiling direction : Int32) :
    Except String (GameState × Int32) := do
  if floorOrCeiling == 0 then
    if direction != 1 && direction != (-1 : Int32) then
      throw s!"T_MovePlane: floor direction {direction} not implemented"
    match gs0.sectors[secIdx]? with
    | none => throw "T_MovePlane: bad sector"
    | some sec0 =>
      if direction == (-1 : Int32) then
        if sec0.floorheight - speed < dest then
          let lastpos := sec0.floorheight
          let gs := { gs0 with sectors := GameState.arrSet gs0.sectors secIdx { sec0 with floorheight := dest } }
          let (gs, flag) ← changeSector gs secIdx crush
          if flag then
            match gs.sectors[secIdx]? with
            | none => throw "T_MovePlane: lost sector on pastdest undo"
            | some sec1 =>
              let gs := { gs with sectors := GameState.arrSet gs.sectors secIdx { sec1 with floorheight := lastpos } }
              let (gs, _) ← changeSector gs secIdx crush
              pure (gs, resultPastdest)
          else
            pure (gs, resultPastdest)
        else
          let lastpos := sec0.floorheight
          let gs := {
            gs0 with
            sectors := GameState.arrSet gs0.sectors secIdx { sec0 with floorheight := lastpos - speed }
          }
          let (gs, flag) ← changeSector gs secIdx crush
          if flag then
            if crush then
              pure (gs, resultCrushed)
            else
              match gs.sectors[secIdx]? with
              | none => throw "T_MovePlane: lost sector on crush undo"
              | some sec1 =>
                let gs := { gs with sectors := GameState.arrSet gs.sectors secIdx { sec1 with floorheight := lastpos } }
                let (gs, _) ← changeSector gs secIdx crush
                pure (gs, resultCrushed)
          else
            pure (gs, resultOk)
      else if sec0.floorheight + speed > dest then
        let lastpos := sec0.floorheight
        let gs := { gs0 with sectors := GameState.arrSet gs0.sectors secIdx { sec0 with floorheight := dest } }
        let (gs, flag) ← changeSector gs secIdx crush
        if flag then
          match gs.sectors[secIdx]? with
          | none => throw "T_MovePlane: lost sector on pastdest undo"
          | some sec1 =>
            let gs := { gs with sectors := GameState.arrSet gs.sectors secIdx { sec1 with floorheight := lastpos } }
            let (gs, _) ← changeSector gs secIdx crush
            pure (gs, resultPastdest)
        else
          pure (gs, resultPastdest)
      else
        let lastpos := sec0.floorheight
        let gs := {
          gs0 with
          sectors := GameState.arrSet gs0.sectors secIdx { sec0 with floorheight := lastpos + speed }
        }
        let (gs, flag) ← changeSector gs secIdx crush
        if flag then
          if crush then
            pure (gs, resultCrushed)
          else
            match gs.sectors[secIdx]? with
            | none => throw "T_MovePlane: lost sector on crush undo"
            | some sec1 =>
              let gs := { gs with sectors := GameState.arrSet gs.sectors secIdx { sec1 with floorheight := lastpos } }
              let (gs, _) ← changeSector gs secIdx crush
              pure (gs, resultCrushed)
        else
          pure (gs, resultOk)
  else if floorOrCeiling != 1 then
    throw s!"T_MovePlane: floorOrCeiling {floorOrCeiling} not implemented"
  else if direction != 1 && direction != (-1 : Int32) then
    throw s!"T_MovePlane: direction {direction} not implemented"
  else
  match gs0.sectors[secIdx]? with
  | none => throw "T_MovePlane: bad sector"
  | some sec0 =>
    if direction == 1 then
      if sec0.ceilingheight + speed > dest then
        let lastpos := sec0.ceilingheight
        let gs := { gs0 with sectors := GameState.arrSet gs0.sectors secIdx { sec0 with ceilingheight := dest } }
        let (gs, flag) ← changeSector gs secIdx crush
        if flag then
          match gs.sectors[secIdx]? with
          | none => throw "T_MovePlane: lost sector on pastdest undo"
          | some sec1 =>
            let gs := { gs with sectors := GameState.arrSet gs.sectors secIdx { sec1 with ceilingheight := lastpos } }
            let (gs, _) ← changeSector gs secIdx crush
            pure (gs, resultPastdest)
        else
          pure (gs, resultPastdest)
      else
        match gs0.sectors[secIdx]? with
        | none => throw "T_MovePlane: lost sector"
        | some sec =>
          let gs := {
            gs0 with
            sectors := GameState.arrSet gs0.sectors secIdx { sec with ceilingheight := sec.ceilingheight + speed }
          }
          let (gs, _) ← changeSector gs secIdx crush
          pure (gs, resultOk)
    else
      -- ceiling DOWN (`p_floor.c`)
      if sec0.ceilingheight - speed < dest then
        let lastpos := sec0.ceilingheight
        let gs := { gs0 with sectors := GameState.arrSet gs0.sectors secIdx { sec0 with ceilingheight := dest } }
        let (gs, flag) ← changeSector gs secIdx crush
        if flag then
          match gs.sectors[secIdx]? with
          | none => throw "T_MovePlane: lost sector on pastdest undo"
          | some sec1 =>
            let gs := { gs with sectors := GameState.arrSet gs.sectors secIdx { sec1 with ceilingheight := lastpos } }
            let (gs, _) ← changeSector gs secIdx crush
            pure (gs, resultPastdest)
        else
          pure (gs, resultPastdest)
      else
        match gs0.sectors[secIdx]? with
        | none => throw "T_MovePlane: lost sector"
        | some sec =>
          let lastpos := sec.ceilingheight
          let gs := {
            gs0 with
            sectors := GameState.arrSet gs0.sectors secIdx { sec with ceilingheight := lastpos - speed }
          }
          let (gs, flag) ← changeSector gs secIdx crush
          if flag then
            if crush then
              pure (gs, resultCrushed)
            else
              match gs.sectors[secIdx]? with
              | none => throw "T_MovePlane: lost sector on crush undo"
              | some sec1 =>
                let gs := { gs with sectors := GameState.arrSet gs.sectors secIdx { sec1 with ceilingheight := lastpos } }
                let (gs, _) ← changeSector gs secIdx crush
                pure (gs, resultCrushed)
          else
            pure (gs, resultOk)

/-- Sector `soundorg` audibility vs consoleplayer (degenmobj ≠ player mo). -/
def sectorSoundAudible (gs : GameState) (ox oy : Int32) : Bool :=
  match gs.players[gs.consoleplayer]? with
  | none => true
  | some pl =>
    match gs.mobjs[pl.mo.toNatClampNeg]? with
    | some listener => soundAudible listener.x listener.y ox oy
    | none => true

/-- `EV_VerticalDoor` — special 1 / 26 / 27 / 28 (`vld_normal`), special 31 / 32 / 33 / 34
(`vld_open`, clears `line.special`). Special 26/32 blue-lock, 27/34 yellow-lock, 28/33
red-lock checks run before 1-sided / specialdata. Deny is `S_StartSound(NULL, sfx_oof)`
(always one pitch draw). No `player.message`. `specialdata` reopen only for raise types
{1,26,27,28,117}; 31/32/33/34 fall through. -/
def evVerticalDoor (gs0 : GameState) (lineIdx : Nat) (thingIdx : Nat) :
    Except String GameState := do
  match gs0.level.lines[lineIdx]? with
  | none => throw "EV_VerticalDoor: bad line"
  | some ld =>
    if ld.special != 1 && ld.special != 26 && ld.special != 27 && ld.special != 28
        && ld.special != 31 && ld.special != 32 && ld.special != 33 && ld.special != 34 then
      throw s!"EV_VerticalDoor: special {ld.special} not implemented"
    if ld.special == 26 || ld.special == 32
        || ld.special == 27 || ld.special == 34
        || ld.special == 28 || ld.special == 33 then
      match gs0.mobjs[thingIdx]? with
      | none => throw "EV_VerticalDoor: bad thing"
      | some thing =>
        if thing.player < 0 then
          return gs0
        match gs0.players[thing.player.toNatClampNeg]? with
        | none => throw "EV_VerticalDoor: bad player"
        | some player =>
          let hasKey :=
            if ld.special == 26 || ld.special == 32 then
              match player.cards[it_bluecard]?, player.cards[it_blueskull]? with
              | some true, _ => true
              | _, some true => true
              | _, _ => false
            else if ld.special == 27 || ld.special == 34 then
              match player.cards[it_yellowcard]?, player.cards[it_yellowskull]? with
              | some true, _ => true
              | _, some true => true
              | _, _ => false
            else
              match player.cards[it_redcard]?, player.cards[it_redskull]? with
              | some true, _ => true
              | _, some true => true
              | _, _ => false
          if !hasKey then
            return {
              gs0 with
              rng := startSoundPitchRng gs0.rng sfx_oof
            }
    if ld.sidenum1 < 0 then
      throw "EV_VerticalDoor: DR special type on 1-sided linedef"
    match gs0.level.sides[ld.sidenum1.toNatClampNeg]? with
    | none => throw "EV_VerticalDoor: bad back side"
    | some sd =>
      let secIdx := sd.sector.toNat
      match gs0.sectors[secIdx]? with
      | none => throw "EV_VerticalDoor: bad sector"
      | some sec =>
        if sec.specialdata != -1
            && (ld.special == 1 || ld.special == 26 || ld.special == 27
              || ld.special == 28 || ld.special == 117) then
          -- C: closing-door reopen (direction -1 → 1) for player and monster.
          -- Monsters never close an already-moving raise door (direction != -1).
          -- Player reverse-close (1→-1): flip direction, no sound, no new thinker.
          -- Plat-misuse and non-door stay loud-error.
          -- Special 31 falls through to spawn a new door thinker.
          match gs0.mobjs[thingIdx]? with
          | none => throw "EV_VerticalDoor: bad thing"
          | some thing =>
            let payload := sec.specialdata.toNatClampNeg
            match gs0.verticalDoors[payload]? with
            | none => throw "EV_VerticalDoor: existing specialdata not implemented"
            | some door =>
              if door.direction == (-1 : Int32) then
                return {
                  gs0 with
                  verticalDoors :=
                    GameState.arrSet gs0.verticalDoors payload { door with direction := 1 }
                }
              else if thing.player < 0 then
                return gs0
              else
                return {
                  gs0 with
                  verticalDoors :=
                    GameState.arrSet gs0.verticalDoors payload { door with direction := -1 }
                }
        let (ox, oy) :=
          match gs0.level.sectors[secIdx]? with
          | some geo => (geo.soundorgX, geo.soundorgY)
          | none => ((0 : Int32), (0 : Int32))
        let gs := {
          gs0 with
          rng := startSoundPitchRngMaybe gs0.rng sfx_doropn (sectorSoundAudible gs0 ox oy)
        }
        let topheight := findLowestCeilingSurrounding gs secIdx - 4 * FRACUNIT
        let doorType :=
          if ld.special == 31 || ld.special == 32 || ld.special == 33 || ld.special == 34 then
            vld_open
          else
            vld_normal
        let door : VerticalDoor := {
          sector := secIdx.toUInt32
          type_ := doorType
          topheight
          speed := VDOORSPEED
          direction := 1
          topwait := VDOORWAIT
          topcountdown := 0
        }
        let payload := gs.verticalDoors.size
        let gs := { gs with verticalDoors := gs.verticalDoors.push door }
        let (gs, _) := addThinker gs THF_VERTICALDOOR payload.toUInt32
        match gs.sectors[secIdx]? with
        | none => throw "EV_VerticalDoor: lost sector"
        | some sec2 =>
          let gs := {
            gs with
            sectors := GameState.arrSet gs.sectors secIdx { sec2 with specialdata := payload.toInt32 }
          }
          if ld.special == 31 || ld.special == 32 || ld.special == 33 || ld.special == 34 then
            match gs.level.lines[lineIdx]? with
            | none => throw "EV_VerticalDoor: lost line"
            | some ld1 =>
              pure {
                gs with
                level := {
                  gs.level with
                  lines := GameState.arrSet gs.level.lines lineIdx { ld1 with special := 0 }
                }
              }
          else
            pure gs

/-- `P_FindSectorFromLineTag`. `start = -1` begins at sector 0. -/
def findSectorFromLineTag (gs : GameState) (tag : Int32) (start : Int32) : Int32 :=
  Id.run do
    let mut i : Nat := (start + 1).toNatClampNeg
    while i < gs.sectors.size do
      match gs.sectors[i]? with
      | some sec =>
        if sec.tag == tag then
          return i.toInt32
      | none => pure ()
      i := i + 1
    pure (-1 : Int32)

/-- `EV_DoDoor` — `vld_normal` and `vld_open`. Alloc / `P_AddThinker` / `specialdata`
then type arm (direction, topheight, conditional `sfx_doropn`). -/
def evDoDoor (gs0 : GameState) (lineIdx : Nat) (type_ : Int32) :
    Except String (GameState × Bool) := do
  match gs0.level.lines[lineIdx]? with
  | none => throw "EV_DoDoor: bad line"
  | some ld =>
    if type_ != vld_normal && type_ != vld_open then
      throw s!"EV_DoDoor: type {type_} not implemented"
    let mut gs := gs0
    let mut secnum : Int32 := -1
    let mut rtn := false
    let mut looping := true
    while looping do
      secnum := findSectorFromLineTag gs ld.tag secnum
      if secnum < 0 then
        looping := false
      else
        let secIdx := secnum.toNatClampNeg
        match gs.sectors[secIdx]? with
        | none => throw "EV_DoDoor: bad sector"
        | some sec =>
          if sec.specialdata != -1 then
            pure ()
          else
            rtn := true
            let payload := gs.verticalDoors.size
            let door0 : VerticalDoor := {
              sector := secIdx.toUInt32
              type_
              topheight := 0
              speed := VDOORSPEED
              direction := 0
              topwait := VDOORWAIT
              topcountdown := 0
            }
            let gs1 := { gs with verticalDoors := gs.verticalDoors.push door0 }
            let (gs2, _) := addThinker gs1 THF_VERTICALDOOR payload.toUInt32
            match gs2.sectors[secIdx]? with
            | none => throw "EV_DoDoor: lost sector"
            | some sec2 =>
              let gs3 := {
                gs2 with
                sectors := GameState.arrSet gs2.sectors secIdx
                  { sec2 with specialdata := payload.toInt32 }
              }
              let topheight := findLowestCeilingSurrounding gs3 secIdx - 4 * FRACUNIT
              match gs3.verticalDoors[payload]?, gs3.sectors[secIdx]? with
              | some door, some sec3 =>
                let gs4 := {
                  gs3 with
                  verticalDoors := GameState.arrSet gs3.verticalDoors payload
                    { door with direction := 1, topheight }
                }
                if topheight != sec3.ceilingheight then
                  let (ox, oy) :=
                    match gs4.level.sectors[secIdx]? with
                    | some geo => (geo.soundorgX, geo.soundorgY)
                    | none => ((0 : Int32), (0 : Int32))
                  gs := {
                    gs4 with
                    rng := startSoundPitchRngMaybe gs4.rng sfx_doropn
                      (sectorSoundAudible gs4 ox oy)
                  }
                else
                  gs := gs4
              | _, _ => throw "EV_DoDoor: lost door"
    pure (gs, rtn)

/-- 8-byte Doom texture name (NUL-padded). -/
private def textureName8 (s : String) : ByteArray :=
  Id.run do
    let mut arr := ByteArray.mk #[]
    for b in s.toUTF8 do
      if arr.size < 8 then
        arr := arr.push b
    while arr.size < 8 do
      arr := arr.push 0
    return arr

/-- `p_switch.c` `alphSwitchList` EP1 shareware pairs (19). -/
def alphSwitchListEp1 : Array (ByteArray × ByteArray) := #[
  (textureName8 "SW1BRCOM", textureName8 "SW2BRCOM"),
  (textureName8 "SW1BRN1", textureName8 "SW2BRN1"),
  (textureName8 "SW1BRN2", textureName8 "SW2BRN2"),
  (textureName8 "SW1BRNGN", textureName8 "SW2BRNGN"),
  (textureName8 "SW1BROWN", textureName8 "SW2BROWN"),
  (textureName8 "SW1COMM", textureName8 "SW2COMM"),
  (textureName8 "SW1COMP", textureName8 "SW2COMP"),
  (textureName8 "SW1DIRT", textureName8 "SW2DIRT"),
  (textureName8 "SW1EXIT", textureName8 "SW2EXIT"),
  (textureName8 "SW1GRAY", textureName8 "SW2GRAY"),
  (textureName8 "SW1GRAY1", textureName8 "SW2GRAY1"),
  (textureName8 "SW1METAL", textureName8 "SW2METAL"),
  (textureName8 "SW1PIPE", textureName8 "SW2PIPE"),
  (textureName8 "SW1SLAD", textureName8 "SW2SLAD"),
  (textureName8 "SW1STARG", textureName8 "SW2STARG"),
  (textureName8 "SW1STON1", textureName8 "SW2STON1"),
  (textureName8 "SW1STON2", textureName8 "SW2STON2"),
  (textureName8 "SW1STONE", textureName8 "SW2STONE"),
  (textureName8 "SW1STRTN", textureName8 "SW2STRTN")
]

/--
`P_ChangeSwitchTexture` — EP1 shareware `alphSwitchList`; `useAgain=0` clears
`line.special` before the texture scan; no `P_StartButton`.
-/
def changeSwitchTexture (gs0 : GameState) (lineIdx : Nat) (useAgain : Int32) :
    Except String GameState := do
  match gs0.level.lines[lineIdx]? with
  | none => throw "P_ChangeSwitchTexture: bad line"
  | some ld =>
    let ld1 :=
      if useAgain == 0 then { ld with special := 0 } else ld
    if ld.sidenum0 < 0 then
      throw "P_ChangeSwitchTexture: bad side"
    else
      match gs0.level.sides[ld.sidenum0.toNatClampNeg]? with
      | none => throw "P_ChangeSwitchTexture: bad side"
      | some sd =>
        let texTop := sd.toptexture
        let texMid := sd.midtexture
        let texBot := sd.bottomtexture
        let (ox, oy) :=
          match gs0.level.sectors[ld.frontsector.toNatClampNeg]? with
          | some geo => (geo.soundorgX, geo.soundorgY)
          | none => ((0 : Int32), (0 : Int32))
        let mut gs := {
          gs0 with
          level := { gs0.level with lines := GameState.arrSet gs0.level.lines lineIdx ld1 }
        }
        let sideIdx := ld.sidenum0.toNatClampNeg
        let mut sides := gs.level.sides
        let mut flipped := false
        let mut pairIdx : Nat := 0
        while pairIdx < alphSwitchListEp1.size && !flipped do
          match alphSwitchListEp1[pairIdx]? with
          | none => pure ()
          | some (sw1, sw2) =>
            if texTop == sw1 then
              sides := GameState.arrSet sides sideIdx { sd with toptexture := sw2 }
              flipped := true
            else if texTop == sw2 then
              sides := GameState.arrSet sides sideIdx { sd with toptexture := sw1 }
              flipped := true
            else if texMid == sw1 then
              sides := GameState.arrSet sides sideIdx { sd with midtexture := sw2 }
              flipped := true
            else if texMid == sw2 then
              sides := GameState.arrSet sides sideIdx { sd with midtexture := sw1 }
              flipped := true
            else if texBot == sw1 then
              sides := GameState.arrSet sides sideIdx { sd with bottomtexture := sw2 }
              flipped := true
            else if texBot == sw2 then
              sides := GameState.arrSet sides sideIdx { sd with bottomtexture := sw1 }
              flipped := true
            else
              pure ()
          pairIdx := pairIdx + 1
        if flipped then
          gs := {
            gs with
            level := { gs.level with sides := sides }
            rng := startSoundPitchRngMaybe gs.rng sfx_swtchn
              (sectorSoundAudible gs ox oy)
          }
        pure gs

/-- `p_local.h` `USERANGE`. -/
def USERANGE : Int32 := 64 * FRACUNIT

/-- `T_VerticalDoor` — wait, wait-expire, UP, and DOWN `vld_normal`. -/
def verticalDoorThinker (gs0 : GameState) (payload : Nat) : Except String GameState := do
  match gs0.verticalDoors[payload]? with
  | none => throw "T_VerticalDoor: bad payload"
  | some door0 =>
    if door0.direction == 0 then
      let next := door0.topcountdown - (1 : Int32)
      if next != 0 then
        let door := { door0 with topcountdown := next }
        pure { gs0 with verticalDoors := GameState.arrSet gs0.verticalDoors payload door }
      else if door0.type_ != vld_normal then
        throw s!"T_VerticalDoor: type {door0.type_} not implemented"
      else
        let (ox, oy) :=
          match gs0.level.sectors[door0.sector.toNat]? with
          | some geo => (geo.soundorgX, geo.soundorgY)
          | none => ((0 : Int32), (0 : Int32))
        let door := { door0 with direction := -1, topcountdown := next }
        pure {
          gs0 with
          rng := startSoundPitchRngMaybe gs0.rng sfx_dorcls (sectorSoundAudible gs0 ox oy)
          verticalDoors := GameState.arrSet gs0.verticalDoors payload door
        }
    else if door0.direction == (-1 : Int32) then
      if door0.type_ != vld_normal then
        throw s!"T_VerticalDoor: type {door0.type_} not implemented"
      else
        match gs0.sectors[door0.sector.toNat]? with
        | none => throw "T_VerticalDoor: bad sector"
        | some sec =>
          let (gs1, res) ←
            movePlane gs0 door0.sector.toNat door0.speed sec.floorheight false 1 (-1)
          if res == resultPastdest then
            match gs1.sectors[door0.sector.toNat]? with
            | none => throw "T_VerticalDoor: lost sector"
            | some sec1 =>
              let gs := {
                gs1 with
                sectors := GameState.arrSet gs1.sectors door0.sector.toNat { sec1 with specialdata := -1 }
              }
              removeDoorThinker gs payload
          else if res == resultCrushed then
            throw "T_VerticalDoor: crushed reverse not implemented"
          else
            pure gs1
    else if door0.direction != 1 then
      throw s!"T_VerticalDoor: direction {door0.direction} not implemented"
    else
      let (gs1, res) ←
        movePlane gs0 door0.sector.toNat door0.speed door0.topheight false 1 1
      if res == resultPastdest then
        if door0.type_ == vld_normal then
          let door := { door0 with direction := 0, topcountdown := door0.topwait }
          pure { gs1 with verticalDoors := GameState.arrSet gs1.verticalDoors payload door }
        else if door0.type_ == vld_open then
          match gs1.sectors[door0.sector.toNat]? with
          | none => throw "T_VerticalDoor: lost sector"
          | some sec1 =>
            let gs := {
              gs1 with
              sectors := GameState.arrSet gs1.sectors door0.sector.toNat
                { sec1 with specialdata := -1 }
            }
            removeDoorThinker gs payload
        else
          throw s!"T_VerticalDoor: type {door0.type_} not implemented"
      else
        pure gs1

/-- `plattype_e` / `plat_e`. -/
def platPerpetualRaise : Int32 := 0
def platDownWaitUpStay : Int32 := 1
def platRaiseAndChange : Int32 := 2
def platRaiseToNearestAndChange : Int32 := 3
def platBlazeDWUS : Int32 := 4
def plat_up : Int32 := 0
def plat_down : Int32 := 1
def plat_waiting : Int32 := 2
def plat_inStasis : Int32 := 3

/-- `p_spec.h` `PLATSPEED`. -/
def PLATSPEED : Int32 := FRACUNIT

/-- `i_timer.h` `TICRATE` / `p_spec.h` `PLATWAIT`. -/
def TICRATE : Int32 := 35
def PLATWAIT : Int32 := 3

/-- Vanilla adjoining-sector cap for `P_FindNextHighestFloor`. -/
def MAX_ADJOINING_SECTORS : Nat := 20

/--
`P_FindNextHighestFloor` — chocolate-doom vanilla overflow: 21st higher
adjoining overwrites the comparison height; 22nd adjoining (`h == 22`) is
`I_Error`.
-/
def findNextHighestFloor (gs : GameState) (secIdx : Nat) (currentheight : Int32) :
    Except String Int32 := do
  match gs.sectors[secIdx]? with
  | none => throw "P_FindNextHighestFloor: bad sector"
  | some sec =>
    let mut height := currentheight
    let mut heightlist : Array Int32 := #[]
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
              if other.floorheight > height then
                let h := heightlist.size
                if h == MAX_ADJOINING_SECTORS + 2 then
                  throw "Sector with more than 22 adjoining sectors. Vanilla will crash here"
                if h == MAX_ADJOINING_SECTORS + 1 then
                  height := other.floorheight
                heightlist := heightlist.push other.floorheight
      i := i + 1
    if heightlist.size == 0 then
      return currentheight
    let mut minH :=
      match heightlist[0]? with
      | some v => v
      | none => currentheight
    let mut j : Nat := 1
    while j < heightlist.size do
      match heightlist[j]? with
      | some v =>
        if v < minH then
          minH := v
      | none => pure ()
      j := j + 1
    pure minH

/-- `P_AddActivePlat`. -/
def addActivePlat (gs : GameState) (payload : Nat) : Except String GameState := do
  let mut i : Nat := 0
  while i < MAXPLATS do
    match gs.activePlats[i]? with
    | none => throw "P_AddActivePlat: bad slot"
    | some slot =>
      if slot == -1 then
        return { gs with activePlats := GameState.arrSet gs.activePlats i payload.toInt32 }
    i := i + 1
  throw "P_AddActivePlat: no more plats!"

/-- `P_RemoveThinker` for a `THF_PLATRAISE` payload (lazy `THF_REMOVED`). -/
def removePlatThinker (gs : GameState) (payload : Nat) : Except String GameState := do
  let mut found := false
  let mut thinkers := gs.thinkers
  let mut i : Nat := 0
  while i < thinkers.size do
    match thinkers[i]? with
    | none => throw "P_RemoveActivePlat: bad thinker"
    | some th =>
      if th.func == THF_PLATRAISE && th.payload.toNat == payload then
        thinkers := GameState.arrSet thinkers i { th with func := THF_REMOVED }
        found := true
      pure ()
    i := i + 1
  if !found then
    throw s!"P_RemoveActivePlat: no thinker for payload {payload}"
  pure { gs with thinkers }

/-- `P_RemoveActivePlat`. First matching `activePlats` slot. -/
def removeActivePlat (gs0 : GameState) (payload : Nat) : Except String GameState := do
  let mut i : Nat := 0
  while i < MAXPLATS do
    match gs0.activePlats[i]? with
    | none => throw "P_RemoveActivePlat: bad slot"
    | some slot =>
      if slot == payload.toInt32 then
        match gs0.plats[payload]? with
        | none => throw "P_RemoveActivePlat: bad payload"
        | some plat =>
          match gs0.sectors[plat.sector.toNat]? with
          | none => throw "P_RemoveActivePlat: bad sector"
          | some sec =>
            let gs1 := {
              gs0 with
              sectors := GameState.arrSet gs0.sectors plat.sector.toNat
                { sec with specialdata := (-1 : Int32) }
            }
            let gs2 ← removePlatThinker gs1 payload
            return {
              gs2 with
              activePlats := GameState.arrSet gs2.activePlats i (-1 : Int32)
            }
    i := i + 1
  throw "P_RemoveActivePlat: can't find plat!"

/-- `EV_DoPlat` — `raiseToNearestAndChange` and `downWaitUpStay`. -/
def evDoPlat (gs0 : GameState) (lineIdx : Nat) (type_ _amount : Int32) :
    Except String (GameState × Bool) := do
  match gs0.level.lines[lineIdx]? with
  | none => throw "EV_DoPlat: bad line"
  | some ld =>
    if type_ != platRaiseToNearestAndChange && type_ != platDownWaitUpStay then
      throw s!"EV_DoPlat: type {type_} not implemented"
    let mut rtn := false
    let frontPic? : Option ByteArray ←
      if type_ == platRaiseToNearestAndChange then
        if ld.sidenum0 < 0 then
          throw "EV_DoPlat: bad front side"
        else
          match gs0.level.sides[ld.sidenum0.toNatClampNeg]? with
          | none => throw "EV_DoPlat: bad front side"
          | some sd =>
            match gs0.sectors[sd.sector.toNat]? with
            | none => throw "EV_DoPlat: bad front sector"
            | some frontSec => pure (some frontSec.floorpic)
      else
        pure none
    let mut gs := gs0
    let mut secnum : Int32 := -1
    let mut looping := true
    while looping do
      secnum := findSectorFromLineTag gs ld.tag secnum
      if secnum < 0 then
        looping := false
      else
        let secIdx := secnum.toNatClampNeg
        match gs.sectors[secIdx]? with
        | none => throw "EV_DoPlat: bad sector"
        | some sec =>
          if sec.specialdata != -1 then
            pure ()
          else
            rtn := true
            let plat ←
              if type_ == platRaiseToNearestAndChange then
                let high ← findNextHighestFloor gs secIdx sec.floorheight
                pure {
                  sector := secIdx.toUInt32
                  speed := PLATSPEED / (2 : Int32)
                  low := 0
                  high
                  wait := 0
                  count := 0
                  status := plat_up
                  oldstatus := 0
                  crush := false
                  tag := ld.tag
                  type_
                }
              else
                let surroundLow := findLowestFloorSurrounding gs secIdx
                let low :=
                  if surroundLow > sec.floorheight then sec.floorheight else surroundLow
                pure {
                  sector := secIdx.toUInt32
                  speed := PLATSPEED * 4
                  low
                  high := sec.floorheight
                  wait := TICRATE * PLATWAIT
                  count := 0
                  status := plat_down
                  oldstatus := 0
                  crush := false
                  tag := ld.tag
                  type_
                }
            let payload := gs.plats.size
            let gs1 := { gs with plats := gs.plats.push plat }
            let (gs2, _) := addThinker gs1 THF_PLATRAISE payload.toUInt32
            match gs2.sectors[secIdx]? with
            | none => throw "EV_DoPlat: lost sector"
            | some sec2 => do
              let gs3 ←
                if type_ == platRaiseToNearestAndChange then
                  match frontPic? with
                  | none => throw "EV_DoPlat: lost front pic"
                  | some frontPic =>
                    pure {
                      gs2 with
                      sectors := GameState.arrSet gs2.sectors secIdx
                        { sec2 with specialdata := payload.toInt32, floorpic := frontPic, special := 0 }
                    }
                else
                  pure {
                    gs2 with
                    sectors := GameState.arrSet gs2.sectors secIdx
                      { sec2 with specialdata := payload.toInt32 }
                  }
              let (ox, oy) :=
                match gs3.level.sectors[secIdx]? with
                | some geo => (geo.soundorgX, geo.soundorgY)
                | none => ((0 : Int32), (0 : Int32))
              let sfx :=
                if type_ == platRaiseToNearestAndChange then sfx_stnmov else sfx_pstart
              let gs4 := {
                gs3 with
                rng := startSoundPitchRngMaybe gs3.rng sfx
                  (sectorSoundAudible gs3 ox oy)
              }
              gs ← addActivePlat gs4 payload
    pure (gs, rtn)

/-- `P_UseSpecialLine`. -/
def useSpecialLine (gs0 : GameState) (thingIdx : Nat) (lineIdx : Nat) (side : Int32) :
    Except String (GameState × Bool) := do
  match gs0.level.lines[lineIdx]?, gs0.mobjs[thingIdx]? with
  | none, _ => throw "P_UseSpecialLine: bad line"
  | _, none => throw "P_UseSpecialLine: bad thing"
  | some ld, some thing =>
    if side != 0 then
      return (gs0, false)
    if thing.player < 0 then
      if (ld.flags &&& ML_SECRET) != 0 then
        return (gs0, false)
      if ld.special != 1 && ld.special != 32 && ld.special != 33 && ld.special != 34 then
        return (gs0, false)
    if ld.special == 1 || ld.special == 26 || ld.special == 27 || ld.special == 28
        || ld.special == 31 || ld.special == 32 || ld.special == 33 || ld.special == 34 then
      let gs ← evVerticalDoor gs0 lineIdx thingIdx
      pure (gs, true)
    else if ld.special == 20 then
      let (gs, platOk) ← evDoPlat gs0 lineIdx platRaiseToNearestAndChange 0
      if platOk then
        let gs ← changeSwitchTexture gs lineIdx 0
        pure (gs, true)
      else
        pure (gs, false)
    else if ld.special == 103 then
      let (gs, doorOk) ← evDoDoor gs0 lineIdx vld_open
      if doorOk then
        let gs ← changeSwitchTexture gs lineIdx 0
        pure (gs, true)
      else
        pure (gs, false)
    else if ld.special == 62 then
      let (gs, platOk) ← evDoPlat gs0 lineIdx platDownWaitUpStay 1
      if platOk then
        let gs ← changeSwitchTexture gs lineIdx 1
        pure (gs, true)
      else
        pure (gs, false)
    else if ld.special == 90 then
      pure (gs0, true)
    else if ld.special == 117 || ld.special == 118 then
      throw s!"P_UseSpecialLine: door special {ld.special} not implemented"
    else
      throw s!"P_UseSpecialLine: special {ld.special} not implemented"

/-- `PTR_UseTraverse` — line intercepts only (`PT_ADDLINES`). -/
def ptrUseTraverse (gs0 : GameState) (thingIdx : Nat) (inn : Intercept) :
    Except String (GameState × Bool) := do
  if !inn.isaline then
    throw "PTR_UseTraverse: not a line"
  match gs0.level.lines[inn.lineIdx]? with
  | none => throw "PTR_UseTraverse: bad line"
  | some ld =>
    if ld.special == 0 then
      let op ← lineOpening gs0 ld
      if op.openrange <= 0 then
        let gs := {
          gs0 with
          rng := startSoundPitchRngMaybe gs0.rng sfx_noway (originAudible gs0 thingIdx)
        }
        pure (gs, false)
      else
        pure (gs0, true)
    else
      match gs0.mobjs[thingIdx]? with
      | none => throw "PTR_UseTraverse: bad thing"
      | some mo =>
        match gs0.level.vertexes[ld.v1.toNat]? with
        | none => throw "PTR_UseTraverse: bad vertex"
        | some v1 =>
          let side : Int32 :=
            if pointOnLineSide mo.x mo.y ld v1 == 1 then 1 else 0
          let (gs, _) ← useSpecialLine gs0 thingIdx inn.lineIdx side
          pure (gs, false)

/-- `P_UseLines` — path traverse `PT_ADDLINES` only. -/
def useLines (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "P_UseLines: bad player"
  | some player =>
    if player.mo < 0 then
      throw "P_UseLines: no mo"
    let thingIdx := player.mo.toNatClampNeg
    match gs0.mobjs[thingIdx]? with
    | none => throw "P_UseLines: bad mo"
    | some mo =>
      let angle := (mo.angle >>> ANGLETOFINESHIFT.toUInt32) &&& FINEMASK
      match finecosine[angle.toNat]?, finesine[angle.toNat]? with
      | some cosv, some sinv =>
        let span := USERANGE >>> 16  -- USERANGE>>FRACBITS
        let x2 := mo.x + span * cosv
        let y2 := mo.y + span * sinv
        let (gs1, _, _) ← pathTraverse gs0 () mo.x mo.y x2 y2 PT_ADDLINES
          fun gs st inn => do
            let (gs1, cont) ← ptrUseTraverse gs thingIdx inn
            pure (gs1, st, cont)
        pure gs1
      | _, _ => throw "P_UseLines: fine table OOB"

/-- `T_PlatRaise` — status UP + `raiseToNearestAndChange` or `downWaitUpStay`
resultOk + pastdest (waiting, `sfx_pstop`, `removeActivePlat`); status DOWN +
`downWaitUpStay` resultOk + pastdest (waiting, no `removeActivePlat`).
Crush-reverse is not implemented. -/
def platRaiseThinker (gs0 : GameState) (payload : Nat) : Except String GameState := do
  match gs0.plats[payload]? with
  | none => throw "T_PlatRaise: bad payload"
  | some plat0 =>
    if plat0.status == plat_down then
      if plat0.type_ != platDownWaitUpStay then
        throw s!"T_PlatRaise: type {plat0.type_} not implemented"
      else
        let (gs1, res) ←
          movePlane gs0 plat0.sector.toNat plat0.speed plat0.low plat0.crush 0 (-1)
        if res == resultPastdest then
          match gs1.plats[payload]? with
          | none => throw "T_PlatRaise: lost plat"
          | some plat =>
            let gs2 := {
              gs1 with
              plats := GameState.arrSet gs1.plats payload
                { plat with count := plat.wait, status := plat_waiting }
            }
            let (ox, oy) :=
              match gs2.level.sectors[plat0.sector.toNat]? with
              | some geo => (geo.soundorgX, geo.soundorgY)
              | none => ((0 : Int32), (0 : Int32))
            pure {
              gs2 with
              rng := startSoundPitchRngMaybe gs2.rng sfx_pstop
                (sectorSoundAudible gs2 ox oy)
            }
        else if res == resultCrushed then
          throw "T_PlatRaise: crush-reverse not implemented"
        else
          pure gs1
    else if plat0.status == plat_waiting then
      let count := plat0.count - 1
      if count != 0 then
        pure {
          gs0 with
          plats := GameState.arrSet gs0.plats payload { plat0 with count := count }
        }
      else
        match gs0.sectors[plat0.sector.toNat]? with
        | none => throw "T_PlatRaise: bad sector"
        | some sec =>
          let newStatus :=
            if sec.floorheight == plat0.low then plat_up else plat_down
          let gs1 := {
            gs0 with
            plats := GameState.arrSet gs0.plats payload
              { plat0 with count := count, status := newStatus }
          }
          let (ox, oy) :=
            match gs1.level.sectors[plat0.sector.toNat]? with
            | some geo => (geo.soundorgX, geo.soundorgY)
            | none => ((0 : Int32), (0 : Int32))
          pure {
            gs1 with
            rng := startSoundPitchRngMaybe gs1.rng sfx_pstart
              (sectorSoundAudible gs1 ox oy)
          }
    else if plat0.status == plat_inStasis then
      pure gs0
    else if plat0.status != plat_up then
      throw s!"T_PlatRaise: status {plat0.status} not implemented"
    else if plat0.type_ == platDownWaitUpStay then
      let (gs1, res) ←
        movePlane gs0 plat0.sector.toNat plat0.speed plat0.high plat0.crush 0 1
      if res == resultCrushed then
        throw "T_PlatRaise: crush-reverse not implemented"
      else if res == resultPastdest then
        match gs1.plats[payload]? with
        | none => throw "T_PlatRaise: lost plat"
        | some plat =>
          let gs2 := {
            gs1 with
            plats := GameState.arrSet gs1.plats payload
              { plat with count := plat.wait, status := plat_waiting }
          }
          let (ox, oy) :=
            match gs2.level.sectors[plat0.sector.toNat]? with
            | some geo => (geo.soundorgX, geo.soundorgY)
            | none => ((0 : Int32), (0 : Int32))
          let gs3 := {
            gs2 with
            rng := startSoundPitchRngMaybe gs2.rng sfx_pstop
              (sectorSoundAudible gs2 ox oy)
          }
          removeActivePlat gs3 payload
      else
        pure gs1
    else if plat0.type_ != platRaiseToNearestAndChange then
      throw s!"T_PlatRaise: type {plat0.type_} not implemented"
    else
      let (gs1, res) ←
        movePlane gs0 plat0.sector.toNat plat0.speed plat0.high plat0.crush 0 1
      let gs2 :=
        if (gs1.leveltime &&& (7 : UInt32)) == 0 then
          let (ox, oy) :=
            match gs1.level.sectors[plat0.sector.toNat]? with
            | some geo => (geo.soundorgX, geo.soundorgY)
            | none => ((0 : Int32), (0 : Int32))
          {
            gs1 with
            rng := startSoundPitchRngMaybe gs1.rng sfx_stnmov
              (sectorSoundAudible gs1 ox oy)
          }
        else
          gs1
      if res == resultCrushed && !plat0.crush then
        throw "T_PlatRaise: crush-reverse not implemented"
      else if res == resultPastdest then
        match gs2.plats[payload]? with
        | none => throw "T_PlatRaise: lost plat"
        | some plat =>
          let gs3 := {
            gs2 with
            plats := GameState.arrSet gs2.plats payload
              { plat with count := plat.wait, status := plat_waiting }
          }
          let (ox, oy) :=
            match gs3.level.sectors[plat0.sector.toNat]? with
            | some geo => (geo.soundorgX, geo.soundorgY)
            | none => ((0 : Int32), (0 : Int32))
          let gs4 := {
            gs3 with
            rng := startSoundPitchRngMaybe gs3.rng sfx_pstop
              (sectorSoundAudible gs3 ox oy)
          }
          removeActivePlat gs4 payload
      else
        pure gs2

/-- `EV_DoFloor` — `raiseFloor` and `turboLower`. -/
def evDoFloor (gs0 : GameState) (lineIdx : Nat) (type_ : Int32) :
    Except String GameState := do
  if type_ != raiseFloor && type_ != turboLower then
    throw s!"EV_DoFloor: type {type_} not implemented"
  match gs0.level.lines[lineIdx]? with
  | none => throw "EV_DoFloor: bad line"
  | some ld =>
    let mut gs := gs0
    let mut secnum : Int32 := -1
    let mut looping := true
    while looping do
      secnum := findSectorFromLineTag gs ld.tag secnum
      if secnum < 0 then
        looping := false
      else
        let secIdx := secnum.toNatClampNeg
        match gs.sectors[secIdx]? with
        | none => throw "EV_DoFloor: bad sector"
        | some sec =>
          if sec.specialdata != -1 then
            pure ()
          else
            let (direction, speed, dest) :=
              if type_ == raiseFloor then
                let raw := findLowestCeilingSurrounding gs secIdx
                let d := if raw > sec.ceilingheight then sec.ceilingheight else raw
                ((1 : Int32), FLOORSPEED, d)
              else
                let raw := findHighestFloorSurrounding gs secIdx
                let d := if raw != sec.floorheight then raw + 8 * FRACUNIT else raw
                ((-1 : Int32), FLOORSPEED * 4, d)
            let floor0 : FloorMove := {
              sector := secIdx.toUInt32
              type_
              crush := false
              direction
              speed
              floordestheight := dest
            }
            let payload := gs.floors.size
            let gs1 := { gs with floors := gs.floors.push floor0 }
            let (gs2, _) := addThinker gs1 THF_MOVEFLOOR payload.toUInt32
            match gs2.sectors[secIdx]? with
            | none => throw "EV_DoFloor: lost sector"
            | some sec2 =>
              gs := {
                gs2 with
                sectors := GameState.arrSet gs2.sectors secIdx
                  { sec2 with specialdata := payload.toInt32 }
              }
    pure gs

/-- `T_MoveFloor` — floor UP `raiseFloor` and DOWN `turboLower`. -/
def floorMoveThinker (gs0 : GameState) (payload : Nat) : Except String GameState := do
  match gs0.floors[payload]? with
  | none => throw "T_MoveFloor: bad payload"
  | some floor0 =>
    if floor0.type_ != raiseFloor && floor0.type_ != turboLower then
      throw s!"T_MoveFloor: type {floor0.type_} not implemented"
    let dir :=
      if floor0.type_ == raiseFloor then (1 : Int32) else (-1 : Int32)
    let (gs1, res) ←
      movePlane gs0 floor0.sector.toNat floor0.speed floor0.floordestheight floor0.crush 0 dir
    let gs2 :=
      if (gs1.leveltime &&& (7 : UInt32)) == 0 then
        let (ox, oy) :=
          match gs1.level.sectors[floor0.sector.toNat]? with
          | some geo => (geo.soundorgX, geo.soundorgY)
          | none => ((0 : Int32), (0 : Int32))
        {
          gs1 with
          rng := startSoundPitchRngMaybe gs1.rng sfx_stnmov
            (sectorSoundAudible gs1 ox oy)
        }
      else
        gs1
    if res == resultCrushed then
      throw "T_MoveFloor: crush-reverse not implemented"
    else if res == resultPastdest then
      match gs2.sectors[floor0.sector.toNat]? with
      | none => throw "T_MoveFloor: lost sector"
      | some sec1 =>
        let (ox, oy) :=
          match gs2.level.sectors[floor0.sector.toNat]? with
          | some geo => (geo.soundorgX, geo.soundorgY)
          | none => ((0 : Int32), (0 : Int32))
        let gs3 := {
          gs2 with
          sectors := GameState.arrSet gs2.sectors floor0.sector.toNat
            { sec1 with specialdata := -1 }
          rng := startSoundPitchRngMaybe gs2.rng sfx_pstop
            (sectorSoundAudible gs2 ox oy)
        }
        removeFloorThinker gs3 payload
    else
      pure gs2

/-- `EV_LightTurnOn` (`p_lights.c`) — tag scan; `bright≠0` sets `lightlevel`; `bright=0`
max neighbor via `getNextSector`. Instant, no thinkers/rng/sound. -/
def evLightTurnOn (gs0 : GameState) (lineIdx : Nat) (bright : Int32) : Except String GameState := do
  match gs0.level.lines[lineIdx]? with
  | none => throw "EV_LightTurnOn: bad line"
  | some ld =>
    let tag := ld.tag
    let mut gs := gs0
    let mut i : Nat := 0
    while i < gs.sectors.size do
      match gs.sectors[i]? with
      | none => throw "EV_LightTurnOn: bad sector"
      | some sec =>
        if sec.tag == tag then
          let mut level := bright
          if level == 0 then
            let mut j : Nat := 0
            while j < sec.lines.size do
              match sec.lines[j]? with
              | none => pure ()
              | some li =>
                match gs.level.lines[li.toNat]? with
                | none => pure ()
                | some templine =>
                  match getNextSector templine i with
                  | none => pure ()
                  | some otherIdx =>
                    match gs.sectors[otherIdx]? with
                    | none => pure ()
                    | some temp =>
                      if temp.lightlevel > level then
                        level := temp.lightlevel
              j := j + 1
          gs := { gs with sectors := GameState.arrSet gs.sectors i { sec with lightlevel := level } }
      i := i + 1
    pure gs

/-- `P_CrossSpecialLine`. Vanilla 1.9 non-player missiles (`MT_BRUISERSHOT` /
`MT_TROOPSHOT` / `MT_HEADSHOT` / `MT_ROCKET` / `MT_PLASMA` / `MT_BFG`) return
identity before monster-ok. Spec 1, spec 26, spec 27, spec 31, spec 62, and spec 70 are walk
no-ops (DR is use-only; 31 is use-only `vld_open`). Spec 22 is W1 `EV_DoPlat(raiseToNearestAndChange)`. Spec 35 is W1
`EV_LightTurnOn(35)` + clear line special. Spec 88 is WR
`EV_DoPlat(downWaitUpStay)`. Spec 90 is WR `EV_DoDoor(vld_normal)`. Spec 91 is WR
`EV_DoFloor(raiseFloor)` (line special retained). Spec 98 is WR `EV_DoFloor(turboLower)` (line special
retained).
-/
def crossSpecialLine (gs0 : GameState) (lineIdx : Nat) (_side : Nat) (thingIdx : Nat) :
    Except String GameState := do
  match gs0.mobjs[thingIdx]?, gs0.level.lines[lineIdx]? with
  | none, _ => throw "P_CrossSpecialLine: bad thing"
  | _, none => throw "P_CrossSpecialLine: bad line"
  | some thing, some ld =>
    if thing.player < 0 then
      if thing.typeId == Spawn.MT_BRUISERSHOT
        || thing.typeId == Spawn.MT_TROOPSHOT
        || thing.typeId == Spawn.MT_HEADSHOT
        || thing.typeId == Spawn.MT_ROCKET
        || thing.typeId == Spawn.MT_PLASMA
        || thing.typeId == Spawn.MT_BFG then
        return gs0
      let spec := ld.special
      let monsterOk :=
        spec == 4 || spec == 10 || spec == 39 || spec == 88
        || spec == 97 || spec == 125 || spec == 126
      if !monsterOk then
        return gs0
    if ld.special == 1 || ld.special == 26 || ld.special == 27 || ld.special == 31 || ld.special == 62 || ld.special == 70 then
      return gs0
    if ld.special == 22 then
      let (gs, _) ← evDoPlat gs0 lineIdx platRaiseToNearestAndChange 0
      match gs.level.lines[lineIdx]? with
      | none => throw "P_CrossSpecialLine: lost line"
      | some ld1 =>
        pure {
          gs with
          level := { gs.level with lines := GameState.arrSet gs.level.lines lineIdx { ld1 with special := 0 } }
        }
    else if ld.special == 88 then
      let (gs, _) ← evDoPlat gs0 lineIdx platDownWaitUpStay 0
      pure gs
    else if ld.special == 2 then
      let (gs, _) ← evDoDoor gs0 lineIdx vld_open
      match gs.level.lines[lineIdx]? with
      | none => throw "P_CrossSpecialLine: lost line"
      | some ld1 =>
        pure {
          gs with
          level := { gs.level with lines := GameState.arrSet gs.level.lines lineIdx { ld1 with special := 0 } }
        }
    else if ld.special == 90 then
      let (gs, _) ← evDoDoor gs0 lineIdx vld_normal
      pure gs
    else if ld.special == 91 then
      evDoFloor gs0 lineIdx raiseFloor
    else if ld.special == 98 then
      evDoFloor gs0 lineIdx turboLower
    else if ld.special == 35 then
      let gs ← evLightTurnOn gs0 lineIdx 35
      match gs.level.lines[lineIdx]? with
      | none => throw "P_CrossSpecialLine: lost line"
      | some ld1 =>
        pure {
          gs with
          level := { gs.level with lines := GameState.arrSet gs.level.lines lineIdx { ld1 with special := 0 } }
        }
    else
      throw s!"P_CrossSpecialLine: special crossed on line {lineIdx} (unexpected)"

end Doom.Playsim.Spec
