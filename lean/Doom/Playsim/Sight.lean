import Doom.Playsim.GameState
import Doom.Playsim.Mobj

/-!
# Doom.Playsim.Sight

`P_CheckSight` REJECT-matrix gate only. If REJECT says an unobstructed LOS is
possible, this chunk stops with a loud error (BSP sight deferred).
-/

namespace Doom.Playsim.Sight

open Doom.Playsim.GameState
open Doom.Playsim.Mobj

/--
REJECT-only `P_CheckSight`.
- bit set → definitively not visible (`false`)
- bit clear → would need BSP → error
-/
def checkSightRejectOnly (gs : GameState) (t1 t2 : Mobj) : Except String Bool := do
  let s1 ←
    match gs.level.subsectors[t1.subsector.toNat]? with
    | some ss => pure ss.sector.toNat
    | none => throw "P_CheckSight: t1 bad subsector"
  let s2 ←
    match gs.level.subsectors[t2.subsector.toNat]? with
    | some ss => pure ss.sector.toNat
    | none => throw "P_CheckSight: t2 bad subsector"
  let numsectors := gs.level.sectors.size
  let pnum := s1 * numsectors + s2
  let bytenum := pnum / 8
  let bitIdx := pnum % 8
  let bitnum : UInt8 := (1 : UInt8) <<< bitIdx.toUInt8
  if bytenum >= gs.level.reject.size then
    throw s!"P_CheckSight: REJECT OOB byte {bytenum}"
  let b ←
    if h : bytenum < gs.level.reject.size then pure (gs.level.reject.get bytenum h)
    else throw "P_CheckSight: REJECT OOB"
  if (b &&& bitnum) != 0 then
    -- trivial rejection
    pure false
  else
    throw s!"P_CheckSight: REJECT clear s1={s1} s2={s2} — BSP sight required (soft-split)"

end Doom.Playsim.Sight
