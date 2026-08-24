/-!
# Doom.Playsim.Mobj

Map-object storage. C pointers become stable array indices (`Int32` / `UInt32`;
`-1` = NULL).
-/

namespace Doom.Playsim.Mobj

structure Mobj where
  traceId : UInt32
  typeId : Int32
  x : Int32
  y : Int32
  z : Int32
  momx : Int32
  momy : Int32
  momz : Int32
  angle : UInt32
  sprite : UInt32
  frame : UInt32
  tics : Int32
  state : UInt32
  health : Int32
  flags : UInt32
  radius : Int32
  height : Int32
  reactiontime : Int32
  lastlook : Int32
  threshold : Int32
  /-- Target mobj index, or `-1`. -/
  target : Int32
  /-- Chase direction (`dirtype_t`); internal only — not in trace payload. -/
  movedir : Int32
  /-- Steps until next `P_NewChaseDir`; internal only — not in trace payload. -/
  movecount : Int32
  floorz : Int32
  ceilingz : Int32
  /-- Subsector index. -/
  subsector : UInt32
  /-- Sector thinglist links (`-1` = NULL). -/
  snext : Int32
  sprev : Int32
  /-- Blockmap thing chain links (`-1` = NULL). -/
  bnext : Int32
  bprev : Int32
  /-- Owning player index, or `-1`. -/
  player : Int32
  deriving Repr

def empty : Mobj := {
  traceId := 0, typeId := 0
  x := 0, y := 0, z := 0
  momx := 0, momy := 0, momz := 0
  angle := 0
  sprite := 0, frame := 0, tics := 0, state := 0
  health := 0, flags := 0, radius := 0, height := 0
  reactiontime := 0, lastlook := 0, threshold := 0, target := -1
  movedir := 0, movecount := 0
  floorz := 0, ceilingz := 0, subsector := 0
  snext := -1, sprev := -1, bnext := -1, bprev := -1, player := -1
}

end Doom.Playsim.Mobj
