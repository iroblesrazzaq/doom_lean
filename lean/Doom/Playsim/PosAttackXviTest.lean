import Doom.Harness.Real
import Doom.Playsim.Enemy
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Hitscan
import Doom.Playsim.Info
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Spawn
import Doom.Wad

/-!
P2c-xvi unit checks: `A_PosAttack` no-target no-op, one pellet + puff, action
32 dispatch. Kept out of `EnemyTest.lean` so that file stays under 1k lines.
-/

open Doom.Playsim.Enemy
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Hitscan
open Doom.Playsim.Info
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Spawn
open Doom.Wad

namespace Doom.Playsim.PosAttackXviTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def countType (gs : GameState) (typeId : Int32) : Nat :=
  Id.run do
    let mut n : Nat := 0
    let mut i : Nat := 0
    while i < gs.mobjs.size do
      match gs.mobjs[i]? with
      | some mo =>
        if mo.typeId == typeId then
          n := n + 1
      | none => pure ()
      i := i + 1
    pure n

def checkP2cXviUnits (wad : WadDirectory) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  let gsEmpty := initFromLevel {
      vertexes := #[]
      sectors := #[]
      sides := #[]
      lines := #[]
      segs := #[]
      subsectors := #[]
      nodes := #[]
      things := #[]
      blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
      reject := ByteArray.empty
    } 2 #[true, false, false, false] 0
  -- no-target no-op --------------------------------------------------------
  let gsNoT := { gsEmpty with mobjs := #[{ Doom.Playsim.Mobj.empty with target := -1 }] }
  match aPosAttack gsNoT 0 with
  | Except.error e =>
    ok := (← assert s!"A_PosAttack no target ({e})" false) && ok
  | Except.ok gsNoT1 =>
    ok := (← assert "A_PosAttack no target no RNG"
      (gsNoT1.rng.prndindex == 0 && gsNoT1.rng.rndindex == 0)) && ok
    ok := (← assert "A_PosAttack no target no puff"
      (countType gsNoT1 MT_PUFF == 0)) && ok
  match runMobjAction gsNoT 0 action_A_PosAttack with
  | Except.error e =>
    ok := (← assert s!"action 32 no-target dispatch ({e})" false) && ok
  | Except.ok _ =>
    ok := (← assert "action 32 does not throw" true) && ok

  -- one pellet + puff: possessed fires at a nearby NOBLOOD player ----------
  match Doom.Harness.Real.loadMap wad "E1M1" with
  | Except.error e =>
    ok := (← assert s!"E1M1 load PosAttack ({e})" false) && ok
  | Except.ok level =>
    let gs0 := initFromLevel level 3 #[true, false, false, false] 0
    let cx := gs0.level.blockmap.originX + (64 : Int32) * FRACUNIT
    let cy := gs0.level.blockmap.originY + (64 : Int32) * FRACUNIT
    match spawnMobj gs0 cx cy ONFLOORZ 0 with
    | Except.error e =>
      ok := (← assert s!"spawn PosAttack listener ({e})" false) && ok
    | Except.ok (gs1, pIdx) =>
      match gs1.mobjs[pIdx]? with
      | none => ok := (← assert "PosAttack listener present" false) && ok
      | some pmo =>
        let gs1 := {
          gs1 with
          mobjs := GameState.arrSet gs1.mobjs pIdx
            { pmo with flags := pmo.flags ||| MF_NOBLOOD }
        }
        match spawnMobj gs1 (cx + 16 * FRACUNIT) cy ONFLOORZ MT_POSSESSED with
        | Except.error e =>
          ok := (← assert s!"spawn PosAttack shooter ({e})" false) && ok
        | Except.ok (gs3, sIdx) =>
          match gs3.mobjs[sIdx]? with
          | none => ok := (← assert "PosAttack shooter present" false) && ok
          | some sg0 =>
            let gs4 := {
              gs3 with
              mobjs := GameState.arrSet gs3.mobjs sIdx { sg0 with target := pIdx.toInt32 }
            }
            let nPuff0 := countType gs4 MT_PUFF
            match aPosAttack gs4 sIdx with
            | Except.error e =>
              ok := (← assert s!"A_PosAttack one pellet ({e})" false) && ok
            | Except.ok gs5 =>
              let nPuff1 := countType gs5 MT_PUFF
              ok := (← assert "A_PosAttack one pellet + puff"
                (nPuff1 == nPuff0 + 1)) && ok
              ok := (← assert "A_PosAttack is not 3-pellet"
                (nPuff1 != nPuff0 + 3)) && ok
              ok := (← assert "A_PosAttack sfx_pistol pitch M_Random"
                (gs5.rng.rndindex == gs4.rng.rndindex + 1)) && ok
            match runMobjAction gs4 sIdx action_A_PosAttack with
            | Except.error e =>
              ok := (← assert s!"action 32 attack dispatch ({e})" false) && ok
            | Except.ok gsDisp =>
              ok := (← assert "action 32 attack does not throw" true) && ok
              ok := (← assert "action 32 dispatch one puff"
                (countType gsDisp MT_PUFF == nPuff0 + 1)) && ok
  pure ok

end Doom.Playsim.PosAttackXviTest
