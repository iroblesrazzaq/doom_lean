import Doom.Harness.Real
import Doom.Playsim.Enemy
import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Map
import Doom.Playsim.Mobj
import Doom.Playsim.Spawn
import Doom.Wad

/-!
P2c-xiii unit checks: `P_SpawnMissile` / `P_CheckMissileSpawn`, `P_ExplodeMissile`,
`PIT_CheckThing` missile branch, `A_TroopAttack` no-target. Kept out of
`EnemyTest.lean` so that file stays under 1k lines.
-/

open Doom.Playsim.Enemy
open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Map
open Doom.Playsim.Mobj
open Doom.Playsim.Spawn
open Doom.Wad

namespace Doom.Playsim.MissileXiiiTest

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

private def stubExplode (gs : GameState) (idx : Nat) : Except String GameState :=
  match gs.mobjs[idx]? with
  | none => throw "stubExplode: bad mobj"
  | some mo =>
    pure {
      gs with
      mobjs := arrSet gs.mobjs idx
        { mo with momx := 0, momy := 0, momz := 0, flags := mo.flags &&& (~~~MF_MISSILE) }
    }

/-- Overlay a null-action state (deathstate 99 has action 0). -/
private def stubSetState (gs : GameState) (idx : Nat) (stnum : UInt32) :
    Except String (GameState × Bool) := do
  match states[stnum.toNat]? with
  | none => throw s!"stubSetState: bad state {stnum}"
  | some st =>
    if st.action != actionNull then
      throw s!"stubSetState: unexpected action {st.action}"
    match gs.mobjs[idx]? with
    | none => throw "stubSetState: bad mobj"
    | some mo =>
      pure ({ gs with mobjs := arrSet gs.mobjs idx {
        mo with state := stnum, tics := st.tics, sprite := st.sprite, frame := st.frame
      } }, true)

def checkP2cXiiiUnits (wad : WadDirectory) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  match Doom.Harness.Real.loadMap wad "E1M1" with
  | Except.error e =>
    ok := (← assert s!"E1M1 load missile ({e})" false) && ok
  | Except.ok level =>
    let gs0 := initFromLevel level 3 #[true, false, false, false] 0
    ok := (← assert "ceilinglineIsSky emptyScratch is false"
      (!ceilinglineIsSky gs0 emptyScratch)) && ok
    let px := (-16 : Int32) <<< 16
    let py := (-496 : Int32) <<< 16
    match spawnMobj gs0 px py ONFLOORZ 11 with
    | Except.error e =>
      ok := (← assert s!"spawn source imp ({e})" false) && ok
    | Except.ok (gs1, srcIdx) =>
      match spawnMobj gs1 (px + 200 * FRACUNIT) py ONFLOORZ 0 with
      | Except.error e =>
        ok := (← assert s!"spawn dest player ({e})" false) && ok
      | Except.ok (gs2, destIdx) =>
        match spawnMissile stubDamage stubExplode gs2 srcIdx destIdx MT_TROOPSHOT with
        | Except.error e =>
          ok := (← assert s!"P_SpawnMissile ({e})" false) && ok
        | Except.ok (gs3, shotIdx) =>
          match gs3.mobjs[shotIdx]? with
          | none => ok := (← assert "troopshot mobj" false) && ok
          | some shot =>
            ok := (← assert "P_SpawnMissile type=MT_TROOPSHOT"
              (shot.typeId == MT_TROOPSHOT)) && ok
            ok := (← assert "P_SpawnMissile target=source"
              (shot.target == srcIdx.toInt32)) && ok
            ok := (← assert "P_SpawnMissile MF_MISSILE"
              ((shot.flags &&& MF_MISSILE) != 0)) && ok
            ok := (← assert "P_SpawnMissile tics>=1" (shot.tics >= 1)) && ok
            ok := (← assert "P_SpawnMissile momx nonzero or momy"
              (shot.momx != 0 || shot.momy != 0)) && ok
            match explodeMissileWith stubSetState gs3 shotIdx with
            | Except.error e =>
              ok := (← assert s!"P_ExplodeMissile ({e})" false) && ok
            | Except.ok gsE =>
              match gsE.mobjs[shotIdx]? with
              | none => ok := (← assert "explode mobj" false) && ok
              | some ex =>
                ok := (← assert "P_ExplodeMissile mom=0"
                  (ex.momx == 0 && ex.momy == 0 && ex.momz == 0)) && ok
                ok := (← assert "P_ExplodeMissile clears MF_MISSILE"
                  ((ex.flags &&& MF_MISSILE) == 0)) && ok
                ok := (← assert "P_ExplodeMissile deathstate 99"
                  (ex.state == 99)) && ok
                ok := (← assert "P_ExplodeMissile tics>=1" (ex.tics >= 1)) && ok
        match spawnMobj gs2 px py ONFLOORZ MT_TROOPSHOT with
        | Except.error e =>
          ok := (← assert s!"spawn overhead shot ({e})" false) && ok
        | Except.ok (gsH, shotH) =>
          match gsH.mobjs[shotH]?, gsH.mobjs[destIdx]? with
          | some sh, some dest =>
            let gsH := {
              gsH with
              mobjs := arrSet gsH.mobjs shotH {
                sh with
                flags := sh.flags ||| MF_MISSILE
                z := dest.z + dest.height + FRACUNIT
                target := srcIdx.toInt32
              }
            }
            match gsH.mobjs[shotH]?, gsH.mobjs[destIdx]? with
            | some sh2, some dest2 =>
              let scr : MapScratch := {
                emptyScratch with
                tmthingIdx := shotH, tmflags := sh2.flags
                tmx := dest2.x, tmy := dest2.y
              }
              match pitCheckThing gsH scr destIdx dest2 none with
              | Except.error e =>
                ok := (← assert s!"PIT over ({e})" false) && ok
              | Except.ok (_, _, cont) =>
                ok := (← assert "PIT_CheckThing over/under continues" cont) && ok
            | _, _ => ok := (← assert "overhead shot after flag" false) && ok
          | _, _ => ok := (← assert "overhead shot/dest" false) && ok
        match spawnMobj gs2 px py ONFLOORZ MT_TROOPSHOT with
        | Except.error e =>
          ok := (← assert s!"spawn hit shot ({e})" false) && ok
        | Except.ok (gsHit, shotHit) =>
          match gsHit.mobjs[shotHit]?, gsHit.mobjs[destIdx]? with
          | some sh, some dest =>
            let gsHit := {
              gsHit with
              mobjs := arrSet gsHit.mobjs shotHit {
                sh with
                flags := sh.flags ||| MF_MISSILE
                z := dest.z
                target := srcIdx.toInt32
              }
            }
            match gsHit.mobjs[shotHit]?, gsHit.mobjs[destIdx]? with
            | some sh2, some dest2 =>
              let scr : MapScratch := {
                emptyScratch with
                tmthingIdx := shotHit, tmflags := sh2.flags
                tmx := dest2.x, tmy := dest2.y
              }
              match pitCheckThing gsHit scr destIdx dest2 none with
              | Except.error e =>
                ok := (← assert "PIT missile hit without DamageMobjFn loud-errors"
                  (e.contains "missile hit with no P_DamageMobj")) && ok
              | Except.ok _ =>
                ok := (← assert "PIT missile hit without DamageMobjFn loud-errors"
                  false) && ok
              match pitCheckThing gsHit scr destIdx dest2 (some stubDamage) with
              | Except.error e =>
                ok := (← assert s!"PIT missile damage ({e})" false) && ok
              | Except.ok (gsD, _, cont) =>
                ok := (← assert "PIT missile damage stops" (!cont)) && ok
                match gsD.mobjs[destIdx]? with
                | some d =>
                  ok := (← assert "PIT missile damage reduces health"
                    (d.health < dest2.health)) && ok
                | none => ok := (← assert "PIT damage dest" false) && ok
            | _, _ => ok := (← assert "hit shot after flag" false) && ok
          | _, _ => ok := (← assert "hit shot/dest" false) && ok
        match spawnMobj gs2 (px + 8 * FRACUNIT) py ONFLOORZ 11 with
        | Except.error e =>
          ok := (← assert s!"spawn same-species imp ({e})" false) && ok
        | Except.ok (gsSp, otherIdx) =>
          match spawnMobj gsSp px py ONFLOORZ MT_TROOPSHOT with
          | Except.error e =>
            ok := (← assert s!"spawn same-species shot ({e})" false) && ok
          | Except.ok (gsSp2, shotSp) =>
            match gsSp2.mobjs[shotSp]?, gsSp2.mobjs[otherIdx]? with
            | some sh, some other =>
              let gsSp2 := {
                gsSp2 with
                mobjs := arrSet gsSp2.mobjs shotSp {
                  sh with
                  flags := sh.flags ||| MF_MISSILE
                  z := other.z
                  target := srcIdx.toInt32
                }
              }
              match gsSp2.mobjs[otherIdx]? with
              | none => ok := (← assert "same-species other" false) && ok
              | some other2 =>
                let scr : MapScratch := {
                  emptyScratch with
                  tmthingIdx := shotSp, tmflags := MF_MISSILE
                  tmx := other2.x, tmy := other2.y
                }
                match pitCheckThing gsSp2 scr otherIdx other2 (some stubDamage) with
                | Except.error e =>
                  ok := (← assert s!"PIT same-species ({e})" false) && ok
                | Except.ok (gsSp3, _, cont) =>
                  ok := (← assert "PIT same-species stops without damage" (!cont)) && ok
                  match gsSp3.mobjs[otherIdx]? with
                  | some o3 =>
                    ok := (← assert "PIT same-species health unchanged"
                      (o3.health == other2.health)) && ok
                  | none => ok := (← assert "same-species after" false) && ok
            | _, _ => ok := (← assert "same-species shot/other" false) && ok
    match spawnMobj gs0 px py ONFLOORZ 11 with
    | Except.error e =>
      ok := (← assert s!"spawn troop for A_TroopAttack ({e})" false) && ok
    | Except.ok (gsA, aIdx) =>
      match aTroopAttack gsA aIdx with
      | Except.error e =>
        ok := (← assert s!"A_TroopAttack no target ({e})" false) && ok
      | Except.ok gsB =>
        ok := (← assert "A_TroopAttack no target is no-op"
          (gsB.mobjs.size == gsA.mobjs.size)) && ok
    match spawnMobj gs0 px py ONFLOORZ 11 with
    | Except.error e =>
      ok := (← assert s!"spawn melee imp ({e})" false) && ok
    | Except.ok (gsM0, meleeIdx) =>
      match spawnMobj gsM0 px py ONFLOORZ 30 with
      | Except.error e =>
        ok := (← assert s!"spawn melee barrel ({e})" false) && ok
      | Except.ok (gsM1, barIdx) =>
        let gsM1 :=
          match gsM1.mobjs[meleeIdx]?, gsM1.mobjs[barIdx]? with
          | some imp, some bar =>
            { gsM1 with
              mobjs := arrSet (arrSet gsM1.mobjs meleeIdx { imp with target := barIdx.toInt32 })
                barIdx { bar with health := 1000 } }
          | _, _ => gsM1
        match aTroopAttack gsM1 meleeIdx with
        | Except.error e =>
          ok := (← assert s!"A_TroopAttack melee ({e})" false) && ok
        | Except.ok gsM2 =>
          ok := (← assert "A_TroopAttack melee does not spawn missile"
            (gsM2.mobjs.size == gsM1.mobjs.size)) && ok
          match gsM2.mobjs[barIdx]? with
          | some bar =>
            ok := (← assert "A_TroopAttack melee damages target"
              (bar.health < 1000)) && ok
          | none => ok := (← assert "melee barrel after" false) && ok
  pure ok

end Doom.Playsim.MissileXiiiTest
