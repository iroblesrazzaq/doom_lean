import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Map
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Thinker

/-!
# Doom.Playsim.Spec

Open subset of `p_spec.c` / `p_switch.c` / `p_doors.c` / `p_floor.c` for the
first DEMO1 vertical door: surrounding-ceiling search, `P_UseSpecialLine`,
`EV_VerticalDoor` / `T_VerticalDoor` (UP + wait countdown), and ceiling-UP
`T_MovePlane`.
-/

namespace Doom.Playsim.Spec

open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Map
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Thinker

/-- `vldoor_e`. -/
def vld_normal : Int32 := 0

/-- `p_spec.h` `VDOORSPEED` / `VDOORWAIT`. -/
def VDOORSPEED : Int32 := 2 * FRACUNIT
def VDOORWAIT : Int32 := 150

/-- `result_e`. -/
def resultOk : Int32 := 0
def resultCrushed : Int32 := 1
def resultPastdest : Int32 := 2

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

/-- `T_MovePlane` — ceiling UP only for this chunk. -/
def movePlane (gs0 : GameState) (secIdx : Nat) (speed dest : Int32)
    (crush : Bool) (floorOrCeiling direction : Int32) :
    Except String (GameState × Int32) := do
  if floorOrCeiling != 1 then
    throw s!"T_MovePlane: floorOrCeiling {floorOrCeiling} not implemented"
  if direction != 1 then
    throw s!"T_MovePlane: direction {direction} not implemented"
  match gs0.sectors[secIdx]? with
  | none => throw "T_MovePlane: bad sector"
  | some sec0 =>
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

/-- Sector `soundorg` audibility vs consoleplayer (degenmobj ≠ player mo). -/
def sectorSoundAudible (gs : GameState) (ox oy : Int32) : Bool :=
  match gs.players[gs.consoleplayer]? with
  | none => true
  | some pl =>
    match gs.mobjs[pl.mo.toNatClampNeg]? with
    | some listener => soundAudible listener.x listener.y ox oy
    | none => true

/-- `EV_VerticalDoor` — special 1 / `vld_normal` only. -/
def evVerticalDoor (gs0 : GameState) (lineIdx : Nat) (thingIdx : Nat) :
    Except String GameState := do
  match gs0.level.lines[lineIdx]? with
  | none => throw "EV_VerticalDoor: bad line"
  | some ld =>
    if ld.special != 1 then
      throw s!"EV_VerticalDoor: special {ld.special} not implemented"
    if ld.sidenum1 < 0 then
      throw "EV_VerticalDoor: DR special type on 1-sided linedef"
    match gs0.level.sides[ld.sidenum1.toNatClampNeg]? with
    | none => throw "EV_VerticalDoor: bad back side"
    | some sd =>
      let secIdx := sd.sector.toNat
      match gs0.sectors[secIdx]? with
      | none => throw "EV_VerticalDoor: bad sector"
      | some sec =>
        if sec.specialdata != -1 then
          -- C: monsters never close an already-moving raise door; player reverse
          -- (direction 1→-1) stays out of this chunk.
          match gs0.mobjs[thingIdx]? with
          | none => throw "EV_VerticalDoor: bad thing"
          | some thing =>
            if thing.player < 0 then
              return gs0
            else
              throw "EV_VerticalDoor: existing specialdata not implemented"
        let (ox, oy) :=
          match gs0.level.sectors[secIdx]? with
          | some geo => (geo.soundorgX, geo.soundorgY)
          | none => ((0 : Int32), (0 : Int32))
        let gs := {
          gs0 with
          rng := startSoundPitchRngMaybe gs0.rng sfx_doropn (sectorSoundAudible gs0 ox oy)
        }
        let topheight := findLowestCeilingSurrounding gs secIdx - 4 * FRACUNIT
        let door : VerticalDoor := {
          sector := secIdx.toUInt32
          type_ := vld_normal
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
          pure { gs with sectors := GameState.arrSet gs.sectors secIdx { sec2 with specialdata := payload.toInt32 } }

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
    if ld.special == 1 then
      let gs ← evVerticalDoor gs0 lineIdx thingIdx
      pure (gs, true)
    else if ld.special == 26 || ld.special == 27 || ld.special == 28 || ld.special == 31
        || ld.special == 32 || ld.special == 33 || ld.special == 34
        || ld.special == 117 || ld.special == 118 then
      throw s!"P_UseSpecialLine: door special {ld.special} not implemented"
    else
      throw s!"P_UseSpecialLine: special {ld.special} not implemented"

/-- `T_VerticalDoor` — wait (`direction == 0`) countdown, then UP `vld_normal`. -/
def verticalDoorThinker (gs0 : GameState) (payload : Nat) : Except String GameState := do
  match gs0.verticalDoors[payload]? with
  | none => throw "T_VerticalDoor: bad payload"
  | some door0 =>
    if door0.direction == 0 then
      let next := door0.topcountdown - (1 : Int32)
      if next != 0 then
        let door := { door0 with topcountdown := next }
        pure { gs0 with verticalDoors := GameState.arrSet gs0.verticalDoors payload door }
      else
        throw "T_VerticalDoor: direction -1 not implemented"
    else if door0.direction != 1 then
      throw s!"T_VerticalDoor: direction {door0.direction} not implemented"
    else if door0.type_ != vld_normal then
      throw s!"T_VerticalDoor: type {door0.type_} not implemented"
    else
      let (gs1, res) ←
        movePlane gs0 door0.sector.toNat door0.speed door0.topheight false 1 1
      if res == resultPastdest then
        let door := { door0 with direction := 0, topcountdown := door0.topwait }
        pure { gs1 with verticalDoors := GameState.arrSet gs1.verticalDoors payload door }
      else
        pure gs1

end Doom.Playsim.Spec
