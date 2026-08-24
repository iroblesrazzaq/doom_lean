import Doom.Playsim.Enemy
import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Sound
import Doom.Playsim.Weapons

/-!
P2c-xxxii unit checks: `A_XScream` (action 28) is `S_StartSound(actor, sfx_slop)`
only. Pitch via `startSoundPitchRngMaybe` + `originAudible`. Never `P_Random`.
Never NULL origin (including spider/cyborg). Sound-only: no flag/health/state/
weapon mutation. Kept out of `EnemyTest.lean` so that file stays under 1k.
-/

open Doom.Playsim.Enemy
open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Sound
open Doom.Playsim.Weapons

namespace Doom.Playsim.EnemyXxxiiTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def emptyBlockmap : BlockMap :=
  { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }

private def emptyLevel : LevelData := {
  vertexes := #[]
  sectors := #[]
  sides := #[]
  lines := #[]
  segs := #[]
  subsectors := #[]
  nodes := #[]
  things := #[]
  blockmap := emptyBlockmap
  reject := ByteArray.empty
}

private def screamScene (actorX : Int32) (typeId : Int32) : GameState :=
  let gs0 := initFromLevel emptyLevel 3 #[true, false, false, false] 0
  let listener := {
    Mobj.empty with
    x := 0, y := 0, player := 0, typeId := 0
  }
  let actor := {
    Mobj.empty with
    x := actorX, y := 0, typeId, player := -1
    health := 20
    state := 194
    flags := MF_SOLID ||| MF_SHOOTABLE
  }
  let pl := {
    Player.empty with
    mo := 0, playerstate := PST_LIVE, health := 100
    readyweapon := wp_pistol
    pendingweapon := wp_nochange
    ammo := #[50, 0, 0, 0]
  }
  {
    gs0 with
    mobjs := #[listener, actor]
    players := GameState.arrSet gs0.players 0 pl
  }

private def unchangedActor (before after : GameState) : Bool :=
  match before.mobjs[1]?, after.mobjs[1]? with
  | some a, some b =>
    a.flags == b.flags && a.health == b.health && a.state == b.state
      && a.typeId == b.typeId
  | _, _ => false

private def unchangedWeapons (before after : GameState) : Bool :=
  match before.players[0]?, after.players[0]? with
  | some p0, some p1 =>
    p0.readyweapon == p1.readyweapon && p0.pendingweapon == p1.pendingweapon
      && p0.ammo == p1.ammo && p0.health == p1.health
  | _, _ => false

def checkP2cXxxiiUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  ok := (← assert "sfx_slop=31" (sfx_slop == 31)) && ok

  let gsNear := screamScene FRACUNIT MT_POSSESSED
  match aXScream gsNear 1 with
  | Except.error e =>
    ok := (← assert s!"A_XScream audible ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "A_XScream audible one M_Random"
      (gs1.rng.rndindex == 1 && gs1.rng.prndindex == 0)) && ok
    ok := (← assert "A_XScream audible no actor mutation"
      (unchangedActor gsNear gs1)) && ok
    ok := (← assert "A_XScream audible no weapon mutation"
      (unchangedWeapons gsNear gs1)) && ok

  let gsFar := screamScene (1201 * FRACUNIT) MT_POSSESSED
  match aXScream gsFar 1 with
  | Except.error e =>
    ok := (← assert s!"A_XScream inaudible ({e})" false) && ok
  | Except.ok gsF =>
    ok := (← assert "A_XScream inaudible skips pitch"
      (gsF.rng.rndindex == 0 && gsF.rng.prndindex == 0)) && ok
    ok := (← assert "A_XScream inaudible no actor mutation"
      (unchangedActor gsFar gsF)) && ok

  let gsSpider := screamScene (1201 * FRACUNIT) MT_SPIDER
  match aXScream gsSpider 1 with
  | Except.error e =>
    ok := (← assert s!"A_XScream spider far ({e})" false) && ok
  | Except.ok gsS =>
    ok := (← assert "A_XScream spider far uses originAudible not full-vol"
      (gsS.rng.rndindex == 0 && gsS.rng.prndindex == 0)) && ok

  let gsCyb := screamScene (1201 * FRACUNIT) MT_CYBORG
  match aXScream gsCyb 1 with
  | Except.error e =>
    ok := (← assert s!"A_XScream cyborg far ({e})" false) && ok
  | Except.ok gsC =>
    ok := (← assert "A_XScream cyborg far uses originAudible not full-vol"
      (gsC.rng.rndindex == 0 && gsC.rng.prndindex == 0)) && ok

  match runMobjAction gsNear 1 action_A_XScream with
  | Except.error e =>
    ok := (← assert s!"A_XScream dispatch ({e})" false) && ok
  | Except.ok gsD =>
    ok := (← assert "A_XScream dispatch one M_Random no P_Random"
      (gsD.rng.rndindex == 1 && gsD.rng.prndindex == 0)) && ok
    ok := (← assert "A_XScream dispatch no actor mutation"
      (unchangedActor gsNear gsD)) && ok

  match aXScream gsNear 99 with
  | Except.error e =>
    ok := (← assert "A_XScream bad mobj loud-error"
      (e.contains "A_XScream")) && ok
  | Except.ok _ =>
    ok := (← assert "A_XScream bad mobj should loud-error" false) && ok

  pure ok

end Doom.Playsim.EnemyXxxiiTest
