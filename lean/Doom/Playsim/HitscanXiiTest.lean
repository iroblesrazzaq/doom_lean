import Doom.Harness.Real
import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Hitscan
import Doom.Playsim.Mobj
import Doom.Playsim.Spawn
import Doom.Wad

/-!
P2c-xii unit checks owned by Hitscan: chebyshev falloff, origin `!MF_SHOOTABLE`
skip, failed `P_CheckSight`, in-block `P_RadiusAttack` walk, boss skip, range
gate. Kept out of `EnemyTest.lean` so that file stays under 1k lines.
-/

open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Hitscan
open Doom.Playsim.Mobj
open Doom.Playsim.Spawn
open Doom.Wad

namespace Doom.Playsim.HitscanXiiTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

/-- Subtract health only; no pain/kill side effects. -/
private def stubDamage (gs : GameState) (targetIdx : Nat) (_inf _src : Option Nat)
    (damage : Int32) : Except String GameState :=
  match gs.mobjs[targetIdx]? with
  | none => throw "stubDamage: bad target"
  | some mo =>
    pure { gs with mobjs := arrSet gs.mobjs targetIdx { mo with health := mo.health - damage } }

private def rejectBlocks (gs : GameState) (s1 s2 : Nat) : Bool :=
  let numsectors := gs.level.sectors.size
  let pnum := s1 * numsectors + s2
  let bytenum := pnum / 8
  let bitIdx := pnum % 8
  if h : bytenum < gs.level.reject.size then
    let b := gs.level.reject.get bytenum h
    let bitnum : UInt8 := (1 : UInt8) <<< bitIdx.toUInt8
    (b &&& bitnum) != 0
  else
    false

private def sectorOf (gs : GameState) (mo : Mobj) : Option Nat :=
  match gs.level.subsectors[mo.subsector.toNat]? with
  | some ss => some ss.sector.toNat
  | none => none

/-- A_Explode + P_RadiusAttack PIT units on a real E1M1 blockmap. -/
def checkP2cXiiUnits (wad : WadDirectory) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  match Doom.Harness.Real.loadMap wad "E1M1" with
  | Except.error e =>
    ok := (← assert s!"E1M1 load radius ({e})" false) && ok
  | Except.ok level =>
    let gs0 := initFromLevel level 3 #[true, false, false, false] 0
    let px := (-16 : Int32) <<< 16
    let py := (-496 : Int32) <<< 16
    let off := (40 : Int32) * FRACUNIT
    -- Block-center walk: vanilla `(damage+MAXRADIUS)<<FRACBITS` wrap is ~±56
    -- map units, so a cell-centered pair stays in one block (yl==yh).
    let cx := gs0.level.blockmap.originX + (64 : Int32) * FRACUNIT
    let cy := gs0.level.blockmap.originY + (64 : Int32) * FRACUNIT
    match spawnMobj gs0 cx cy ONFLOORZ 1 with
    | Except.error e =>
      ok := (← assert s!"spawn walk spot ({e})" false) && ok
    | Except.ok (gsW0, wSpot) =>
      match gsW0.mobjs[wSpot]? with
      | none => ok := (← assert "walk spot mobj" false) && ok
      | some spotW =>
        let gsW1 := {
          gsW0 with
          mobjs := arrSet gsW0.mobjs wSpot
            { spotW with flags := spotW.flags &&& (~~~MF_SHOOTABLE), health := 100 }
        }
        match spawnMobj gsW1 cx cy ONFLOORZ 1 with
        | Except.error e =>
          ok := (← assert s!"spawn walk victim ({e})" false) && ok
        | Except.ok (gsW2, wVic) =>
          let gsW2 :=
            match gsW2.mobjs[wVic]? with
            | some v => { gsW2 with mobjs := arrSet gsW2.mobjs wVic { v with health := 100 } }
            | none => gsW2
          match radiusAttack stubDamage gsW2 wSpot none 128 with
          | Except.error e =>
            ok := (← assert s!"P_RadiusAttack ({e})" false) && ok
          | Except.ok gsW3 =>
            match gsW3.mobjs[wVic]?, gsW3.mobjs[wSpot]? with
            | some vic1, some spot1 =>
              ok := (← assert "P_RadiusAttack damages in-block victim"
                (vic1.health < 100)) && ok
              ok := (← assert "P_RadiusAttack skips !MF_SHOOTABLE origin"
                (spot1.health == 100)) && ok
            | _, _ =>
              ok := (← assert "walk mobjs after radius" false) && ok
    match spawnMobj gs0 px py ONFLOORZ 1 with
    | Except.error e =>
      ok := (← assert s!"spawn spot ({e})" false) && ok
    | Except.ok (gs1, spotIdx) =>
      match gs1.mobjs[spotIdx]? with
      | none => ok := (← assert "spot mobj" false) && ok
      | some spot0 =>
        let gs1 := {
          gs1 with
          mobjs := arrSet gs1.mobjs spotIdx
            { spot0 with flags := spot0.flags &&& (~~~MF_SHOOTABLE), health := 100 }
        }
        match spawnMobj gs1 (px + off) py ONFLOORZ 1 with
        | Except.error e =>
          ok := (← assert s!"spawn axis victim ({e})" false) && ok
        | Except.ok (gs2, axisIdx) =>
          match spawnMobj gs2 (px + off) (py + off) ONFLOORZ 1 with
          | Except.error e =>
            ok := (← assert s!"spawn diag victim ({e})" false) && ok
          | Except.ok (gs3, diagIdx) =>
            let gs3 :=
              match gs3.mobjs[axisIdx]?, gs3.mobjs[diagIdx]? with
              | some a, some d =>
                { gs3 with
                  mobjs := arrSet (arrSet gs3.mobjs axisIdx { a with health := 100 })
                    diagIdx { d with health := 100 } }
              | _, _ => gs3
            let st : RadiusAttackState :=
              { spotIdx, sourceIdx := none, bombdamage := 128 }
            match gs3.mobjs[axisIdx]?, gs3.mobjs[diagIdx]? with
            | some axisMo, some diagMo =>
              match pitRadiusAttack stubDamage gs3 st axisIdx axisMo with
              | Except.error e =>
                ok := (← assert s!"PIT axis ({e})" false) && ok
              | Except.ok (gsA, _, _) =>
                match pitRadiusAttack stubDamage gsA st diagIdx diagMo with
                | Except.error e =>
                  ok := (← assert s!"PIT diag ({e})" false) && ok
                | Except.ok (gsD, _, _) =>
                  match gsD.mobjs[axisIdx]?, gsD.mobjs[diagIdx]?, gsD.mobjs[spotIdx]? with
                  | some axis1, some diag1, some spot1 =>
                    ok := (← assert "chebyshev axis==diag damage"
                      (axis1.health == diag1.health && axis1.health < 100)) && ok
                    ok := (← assert "skip origin !MF_SHOOTABLE"
                      (spot1.health == 100)) && ok
                  | _, _, _ =>
                    ok := (← assert "chebyshev mobjs present" false) && ok
            | _, _ =>
              ok := (← assert "axis/diag mobjs" false) && ok

            -- Shootable origin is damaged (no thing==spot pointer skip).
            match gs3.mobjs[spotIdx]? with
            | none => ok := (← assert "origin reload" false) && ok
            | some spotS =>
              let gsS := {
                gs3 with
                mobjs := arrSet gs3.mobjs spotIdx
                  { spotS with flags := spotS.flags ||| MF_SHOOTABLE, health := 100 }
              }
              match gsS.mobjs[spotIdx]? with
              | none => ok := (← assert "origin shootable mobj" false) && ok
              | some spotLive =>
                match pitRadiusAttack stubDamage gsS st spotIdx spotLive with
                | Except.error e =>
                  ok := (← assert s!"PIT origin shootable ({e})" false) && ok
                | Except.ok (gsHit, _, _) =>
                  match gsHit.mobjs[spotIdx]? with
                  | none => ok := (← assert "origin after pit" false) && ok
                  | some hit =>
                    ok := (← assert "shootable origin takes damage"
                      (hit.health < 100)) && ok

            -- Failed sight: same XY, REJECT-opaque subsector.
            match spawnMobj gs1 px py ONFLOORZ 1 with
            | Except.error e =>
              ok := (← assert s!"spawn sight victim ({e})" false) && ok
            | Except.ok (gsV0, visIdx) =>
              match gsV0.mobjs[spotIdx]?, gsV0.mobjs[visIdx]? with
              | some sp, some vis0 =>
                match sectorOf gsV0 sp with
                | none => ok := (← assert "spot sector" false) && ok
                | some spotSec =>
                  let mut rejectSs : Option UInt32 := none
                  let mut si : Nat := 0
                  while si < gsV0.level.subsectors.size && rejectSs.isNone do
                    match gsV0.level.subsectors[si]? with
                    | some ss =>
                      if rejectBlocks gsV0 ss.sector.toNat spotSec then
                        rejectSs := some si.toUInt32
                    | none => pure ()
                    si := si + 1
                  match rejectSs with
                  | none =>
                    ok := (← assert "E1M1 has REJECT pair" false) && ok
                  | some ssIdx =>
                    let gsV := {
                      gsV0 with
                      mobjs := arrSet gsV0.mobjs visIdx
                        { vis0 with
                          subsector := ssIdx
                          health := 100
                          flags := vis0.flags ||| MF_SHOOTABLE }
                    }
                    match gsV.mobjs[visIdx]? with
                    | none => ok := (← assert "sight victim mobj" false) && ok
                    | some vis1 =>
                      match pitRadiusAttack stubDamage gsV st visIdx vis1 with
                      | Except.error e =>
                        ok := (← assert s!"PIT failed sight ({e})" false) && ok
                      | Except.ok (gsOut, _, _) =>
                        match gsOut.mobjs[visIdx]? with
                        | none => ok := (← assert "sight victim after" false) && ok
                        | some vis2 =>
                          ok := (← assert "failed sight skips damage"
                            (vis2.health == 100)) && ok
              | _, _ =>
                ok := (← assert "sight pair present" false) && ok

            -- Cyborg skip via PIT (same XY as spot).
            match spawnMobj gs1 px py ONFLOORZ 21 with
            | Except.error e =>
              ok := (← assert s!"spawn cyborg ({e})" false) && ok
            | Except.ok (gsC0, cybIdx) =>
              match gsC0.mobjs[cybIdx]? with
              | none => ok := (← assert "cyborg mobj" false) && ok
              | some cyb0 =>
                let gsC := {
                  gsC0 with
                  mobjs := arrSet gsC0.mobjs cybIdx { cyb0 with health := 4000 }
                }
                match gsC.mobjs[cybIdx]? with
                | none => ok := (← assert "cyborg live" false) && ok
                | some cybLive =>
                  match pitRadiusAttack stubDamage gsC st cybIdx cybLive with
                  | Except.error e =>
                    ok := (← assert s!"PIT cyborg ({e})" false) && ok
                  | Except.ok (gsC1, _, _) =>
                    match gsC1.mobjs[cybIdx]? with
                    | none => ok := (← assert "cyborg after pit" false) && ok
                    |                     some cyb1 =>
                      ok := (← assert "MT_CYBORG skips concussion"
                        (cyb1.health == 4000)) && ok
            match spawnMobj gs1 px py ONFLOORZ 19 with
            | Except.error e =>
              ok := (← assert s!"spawn spider ({e})" false) && ok
            | Except.ok (gsS0, spIdx) =>
              match gsS0.mobjs[spIdx]? with
              | none => ok := (← assert "spider mobj" false) && ok
              | some sp0 =>
                let gsS := {
                  gsS0 with
                  mobjs := arrSet gsS0.mobjs spIdx { sp0 with health := 3000 }
                }
                match gsS.mobjs[spIdx]? with
                | none => ok := (← assert "spider live" false) && ok
                | some spLive =>
                  match pitRadiusAttack stubDamage gsS st spIdx spLive with
                  | Except.error e =>
                    ok := (← assert s!"PIT spider ({e})" false) && ok
                  | Except.ok (gsS1, _, _) =>
                    match gsS1.mobjs[spIdx]? with
                    | none => ok := (← assert "spider after pit" false) && ok
                    | some sp1 =>
                      ok := (← assert "MT_SPIDER skips concussion"
                        (sp1.health == 3000)) && ok
            match spawnMobj gs1 (px + (200 : Int32) * FRACUNIT) py ONFLOORZ 1 with
            | Except.error e =>
              ok := (← assert s!"spawn far victim ({e})" false) && ok
            | Except.ok (gsF0, farIdx) =>
              match gsF0.mobjs[farIdx]? with
              | none => ok := (← assert "far mobj" false) && ok
              | some far0 =>
                let gsF := {
                  gsF0 with
                  mobjs := arrSet gsF0.mobjs farIdx
                    { far0 with
                      health := 100
                      radius := 0
                      flags := far0.flags ||| MF_SHOOTABLE }
                }
                match gsF.mobjs[farIdx]? with
                | none => ok := (← assert "far live" false) && ok
                | some farLive =>
                  match pitRadiusAttack stubDamage gsF st farIdx farLive with
                  | Except.error e =>
                    ok := (← assert s!"PIT far ({e})" false) && ok
                  | Except.ok (gsF1, _, _) =>
                    match gsF1.mobjs[farIdx]? with
                    | none => ok := (← assert "far after pit" false) && ok
                    | some far1 =>
                      ok := (← assert "dist>=bombdamage skips before sight"
                        (far1.health == 100)) && ok
  pure ok

end Doom.Playsim.HitscanXiiTest
