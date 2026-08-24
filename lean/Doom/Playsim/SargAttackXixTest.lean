import Doom.Harness.Real
import Doom.Playsim.Enemy
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.SargAttack
import Doom.Playsim.Spawn
import Doom.Wad

/-!
P2c-xix unit checks: `A_SargAttack` no-target, melee miss (no `P_Random`),
hit (`((P_Random%10)+1)*4`), action 54 dispatch. Kept out of `EnemyTest.lean`
so that file stays under 1k lines.
-/

open Doom.Playsim.Enemy
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.SargAttack
open Doom.Playsim.Spawn
open Doom.Wad

namespace Doom.Playsim.SargAttackXixTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

/-- `MT_SERGEANT` ordinal in `mobjinfo`. -/
def MT_SERGEANT : Int32 := 12

/-- Subtract health only; no pain/kill side effects. -/
private def stubDamage (gs : GameState) (targetIdx : Nat) (_inf _src : Option Nat)
    (damage : Int32) : Except String GameState :=
  match gs.mobjs[targetIdx]? with
  | none => throw "stubDamage: bad target"
  | some mo =>
    pure { gs with mobjs := GameState.arrSet gs.mobjs targetIdx { mo with health := mo.health - damage } }

private def boomDamage (_gs : GameState) (_t : Nat) (_inf _src : Option Nat)
    (_d : Int32) : Except String GameState :=
  throw "A_SargAttack miss must not call P_DamageMobj"

private def emptyLevel : LevelData := {
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
}

def checkP2cXixUnits (wad : WadDirectory) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  let gsEmpty := initFromLevel emptyLevel 2 #[true, false, false, false] 0
  -- no-target no-op --------------------------------------------------------
  let gsNoT := { gsEmpty with mobjs := #[{ Mobj.empty with target := -1 }] }
  match aSargAttack gsNoT 0 with
  | Except.error e =>
    ok := (← assert s!"A_SargAttack no target ({e})" false) && ok
  | Except.ok gsNoT1 =>
    ok := (← assert "A_SargAttack no target no RNG"
      (gsNoT1.rng.prndindex == 0 && gsNoT1.rng.rndindex == 0)) && ok
  match runMobjAction gsNoT 0 action_A_SargAttack with
  | Except.error e =>
    ok := (← assert s!"action 54 no-target dispatch ({e})" false) && ok
  | Except.ok _ =>
    ok := (← assert "action 54 does not throw" true) && ok

  -- SargAttack.lean must not import Enemy (or Combat) ----------------------
  let root ←
    (do
      let cwd ← IO.currentDir
      match cwd.components.getLast? with
      | some "lean" => pure (cwd.parent.getD cwd)
      | _ => pure cwd)
  let sargSrc ← IO.FS.readFile (root / "lean" / "Doom" / "Playsim" / "SargAttack.lean")
  ok := (← assert "SargAttack does not import Enemy"
    (!sargSrc.contains "import Doom.Playsim.Enemy")) && ok
  ok := (← assert "SargAttack does not import Combat"
    (!sargSrc.contains "import Doom.Playsim.Combat")) && ok

  match Doom.Harness.Real.loadMap wad "E1M1" with
  | Except.error e =>
    ok := (← assert s!"E1M1 load SargAttack ({e})" false) && ok
  | Except.ok level =>
    let gs0 := initFromLevel level 3 #[true, false, false, false] 0
    let cx := gs0.level.blockmap.originX + (64 : Int32) * FRACUNIT
    let cy := gs0.level.blockmap.originY + (64 : Int32) * FRACUNIT
    -- miss: target far outside melee; no P_Random, no damage ----------------
    match spawnMobj gs0 (cx + 256 * FRACUNIT) cy ONFLOORZ 1 with
    | Except.error e =>
      ok := (← assert s!"spawn SargAttack miss target ({e})" false) && ok
    | Except.ok (gsM1, tMiss) =>
      match spawnMobj gsM1 cx cy ONFLOORZ MT_SERGEANT with
      | Except.error e =>
        ok := (← assert s!"spawn SargAttack miss sarg ({e})" false) && ok
      | Except.ok (gsM2, sMiss) =>
        match gsM2.mobjs[sMiss]?, gsM2.mobjs[tMiss]? with
        | none, _ => ok := (← assert "SargAttack miss sarg present" false) && ok
        | _, none => ok := (← assert "SargAttack miss target present" false) && ok
        | some sg0, some tgt0 =>
          let hpMiss0 := tgt0.health
          let gsM3 := {
            gsM2 with
            mobjs := GameState.arrSet gsM2.mobjs sMiss { sg0 with target := tMiss.toInt32 }
          }
          let prndM := gsM3.rng.prndindex
          let rndM := gsM3.rng.rndindex
          match aSargAttackWith aFaceTarget checkMeleeRange boomDamage gsM3 sMiss with
          | Except.error e =>
            ok := (← assert s!"A_SargAttack miss ({e})" false) && ok
          | Except.ok gsM4 =>
            ok := (← assert "A_SargAttack miss no P_Random"
              (gsM4.rng.prndindex == prndM)) && ok
            ok := (← assert "A_SargAttack miss no sfx M_Random"
              (gsM4.rng.rndindex == rndM)) && ok
            match gsM4.mobjs[tMiss]? with
            | none => ok := (← assert "SargAttack miss target survived" false) && ok
            | some tgt1 =>
              ok := (← assert "A_SargAttack miss health unchanged"
                (tgt1.health == hpMiss0)) && ok
          match runMobjAction gsM3 sMiss action_A_SargAttack with
          | Except.error e =>
            ok := (← assert s!"action 54 miss dispatch ({e})" false) && ok
          | Except.ok gsDispM =>
            ok := (← assert "action 54 miss does not throw" true) && ok
            ok := (← assert "action 54 miss no P_Random"
              (gsDispM.rng.prndindex == prndM)) && ok

    -- hit: same cell, damage = ((P_Random%10)+1)*4, no sfx ------------------
    match spawnMobj gs0 cx cy ONFLOORZ 1 with
    | Except.error e =>
      ok := (← assert s!"spawn SargAttack hit target ({e})" false) && ok
    | Except.ok (gsH1, tHit) =>
      match gsH1.mobjs[tHit]? with
      | none => ok := (← assert "SargAttack hit target present" false) && ok
      | some tgtA =>
        let gsH1 := {
          gsH1 with
          mobjs := GameState.arrSet gsH1.mobjs tHit { tgtA with health := 10000 }
        }
        match spawnMobj gsH1 cx cy ONFLOORZ MT_SERGEANT with
        | Except.error e =>
          ok := (← assert s!"spawn SargAttack hit sarg ({e})" false) && ok
        | Except.ok (gsH2, sHit) =>
          match gsH2.mobjs[sHit]? with
          | none => ok := (← assert "SargAttack hit sarg present" false) && ok
          | some sgH =>
            let gsH3 := {
              gsH2 with
              mobjs := GameState.arrSet gsH2.mobjs sHit { sgH with target := tHit.toInt32 }
            }
            let prndH := gsH3.rng.prndindex
            let rndH := gsH3.rng.rndindex
            match aSargAttackWith aFaceTarget checkMeleeRange stubDamage gsH3 sHit with
            | Except.error e =>
              ok := (← assert s!"A_SargAttack hit ({e})" false) && ok
            | Except.ok gsH4 =>
              ok := (← assert "A_SargAttack hit drew P_Random"
                (gsH4.rng.prndindex == prndH + 1)) && ok
              ok := (← assert "A_SargAttack hit no sfx M_Random"
                (gsH4.rng.rndindex == rndH)) && ok
              match gsH4.mobjs[tHit]? with
              | none => ok := (← assert "SargAttack hit target survived" false) && ok
              | some tgtB =>
                let dmg := 10000 - tgtB.health
                ok := (← assert "A_SargAttack hit damage ((r%10)+1)*4"
                  (dmg >= 4 && dmg <= 40 && dmg % 4 == 0)) && ok
            -- public 1-liner: target already in seestate so retarget skips A_Chase
            match gsH3.mobjs[tHit]? with
            | none => ok := (← assert "public hit target present" false) && ok
            | some tgtP =>
              let gsPub := {
                gsH3 with
                mobjs := GameState.arrSet gsH3.mobjs tHit { tgtP with state := 176, health := 10000 }
              }
              match aSargAttack gsPub sHit with
              | Except.error e =>
                ok := (← assert s!"A_SargAttack public hit ({e})" false) && ok
              | Except.ok gsPub1 =>
                match gsPub1.mobjs[tHit]? with
                | none => ok := (← assert "public hit target survived" false) && ok
                | some tgtQ =>
                  let dmgP := 10000 - tgtQ.health
                  ok := (← assert "A_SargAttack public hit damage"
                    (dmgP >= 4 && dmgP <= 40 && dmgP % 4 == 0)) && ok
                  ok := (← assert "A_SargAttack public hit no sfx"
                    (gsPub1.rng.rndindex == gsPub.rng.rndindex)) && ok
  pure ok

end Doom.Playsim.SargAttackXixTest
