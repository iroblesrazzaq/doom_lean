import Doom.Playsim.GameState
import Doom.Playsim.Hitscan
import Doom.Playsim.Mobj
import Doom.Playsim.Random

/-!
# Doom.Playsim.SargAttack

`A_SargAttack` body (`p_enemy.c`). Injected `A_FaceTarget` / `P_CheckMeleeRange`
/ `P_DamageMobj` keep this module from importing Enemy. DEMO1 is post-1.2:
melee-only (no `P_LineAttack`), no sfx inside the action.
-/

namespace Doom.Playsim.SargAttack

open Doom.Playsim.GameState
open Doom.Playsim.Mobj
open Doom.Playsim.Random

/-- `A_SargAttack`. C order: no target → return; else face; melee miss →
return with no `P_Random`; hit → `((P_Random%10)+1)*4` then `P_DamageMobj`. -/
def aSargAttackWith
    (aFaceTarget : GameState → Nat → Except String GameState)
    (checkMeleeRange : GameState → Nat → Except String (GameState × Bool))
    (damageMobj : Hitscan.DamageMobjFn)
    (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "A_SargAttack: bad mobj"
  | some actor0 =>
    if actor0.target < 0 then
      return gs0
    let gs ← aFaceTarget gs0 mobjIdx
    let (gs, inMelee) ← checkMeleeRange gs mobjIdx
    if !inMelee then
      return gs
    let (r, rng) := pRandom gs.rng
    let gs := { gs with rng }
    let damage := ((r % 10) + 1) * 4
    match gs.mobjs[mobjIdx]? with
    | none => throw "A_SargAttack: lost before damage"
    | some actor =>
      if actor.target < 0 then
        return gs
      damageMobj gs actor.target.toNatClampNeg (some mobjIdx) (some mobjIdx) damage

end Doom.Playsim.SargAttack
