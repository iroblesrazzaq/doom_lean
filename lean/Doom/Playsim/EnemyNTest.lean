import Doom.Playsim.Enemy
import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Level
import Doom.Playsim.Map
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Sound
import Doom.Playsim.Weapons

/-!
P2c-n unit checks: `A_PlayerScream` (action 26) is `S_StartSound(actor,
sfx_pldeth|sfx_pdiehi)` only. Pitch via `startSoundPitchRngMaybe` +
`originAudible`. Never `P_Random`. Sound-only: no flag/health/state mutation.
Kept out of `EnemyTest.lean` so that file stays under 1k.
-/

open Doom.Playsim.Enemy
open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Level
open Doom.Playsim.Map
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Sound
open Doom.Playsim.Weapons

namespace Doom.Playsim.EnemyNTest

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

private def screamScene (actorX : Int32) (health : Int32) (commercial : Bool) : GameState :=
  let gs0 := initFromLevel emptyLevel 3 #[true, false, false, false] 0
  let listener := {
    Mobj.empty with
    x := 0, y := 0, player := 0, typeId := 0
  }
  let actor := {
    Mobj.empty with
    x := actorX, y := 0, typeId := MT_PLAYER, player := 0
    health
    state := 159
    flags := MF_SOLID ||| MF_SHOOTABLE
  }
  let pl := {
    Player.empty with
    mo := 1, playerstate := PST_DEAD, health := health
    readyweapon := wp_pistol
    pendingweapon := wp_nochange
    ammo := #[50, 0, 0, 0]
  }
  {
    gs0 with
    commercial
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

def checkP2cNUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  ok := (← assert "sfx_pldeth=57" (sfx_pldeth == 57)) && ok
  ok := (← assert "sfx_pdiehi=58" (sfx_pdiehi == 58)) && ok

  let gsNear := screamScene FRACUNIT (-1) false
  match aPlayerScream gsNear 1 with
  | Except.error e =>
    ok := (← assert s!"A_PlayerScream shareware audible ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "A_PlayerScream shareware audible one M_Random"
      (gs1.rng.rndindex == 1 && gs1.rng.prndindex == 0)) && ok
    ok := (← assert "A_PlayerScream shareware audible no actor mutation"
      (unchangedActor gsNear gs1)) && ok
    ok := (← assert "A_PlayerScream shareware audible no weapon mutation"
      (unchangedWeapons gsNear gs1)) && ok

  let gsFar := screamScene (1201 * FRACUNIT) (-1) false
  match aPlayerScream gsFar 1 with
  | Except.error e =>
    ok := (← assert s!"A_PlayerScream player origin far ({e})" false) && ok
  | Except.ok gsF =>
    ok := (← assert "A_PlayerScream player origin always audible one M_Random"
      (gsF.rng.rndindex == 1 && gsF.rng.prndindex == 0)) && ok
    ok := (← assert "A_PlayerScream player origin far no actor mutation"
      (unchangedActor gsFar gsF)) && ok

  let gsCommHi := screamScene FRACUNIT (-51) true
  match aPlayerScream gsCommHi 1 with
  | Except.error e =>
    ok := (← assert s!"A_PlayerScream commercial hi ({e})" false) && ok
  | Except.ok gsH =>
    ok := (← assert "A_PlayerScream commercial health<-50 one M_Random"
      (gsH.rng.rndindex == 1 && gsH.rng.prndindex == 0)) && ok

  let gsCommLo := screamScene FRACUNIT (-1) true
  match aPlayerScream gsCommLo 1 with
  | Except.error e =>
    ok := (← assert s!"A_PlayerScream commercial lo ({e})" false) && ok
  | Except.ok gsL =>
    ok := (← assert "A_PlayerScream commercial health>=-50 one M_Random"
      (gsL.rng.rndindex == 1 && gsL.rng.prndindex == 0)) && ok

  let gsCommEdge := screamScene FRACUNIT (-50) true
  match aPlayerScream gsCommEdge 1 with
  | Except.error e =>
    ok := (← assert s!"A_PlayerScream commercial -50 ({e})" false) && ok
  | Except.ok gsE =>
    ok := (← assert "A_PlayerScream commercial health=-50 uses pldeth path"
      (gsE.rng.rndindex == 1 && gsE.rng.prndindex == 0)) && ok

  match runMobjAction gsNear 1 action_A_PlayerScream with
  | Except.error e =>
    ok := (← assert s!"A_PlayerScream dispatch ({e})" false) && ok
  | Except.ok gsD =>
    ok := (← assert "A_PlayerScream dispatch one M_Random no P_Random"
      (gsD.rng.rndindex == 1 && gsD.rng.prndindex == 0)) && ok
    ok := (← assert "A_PlayerScream dispatch no actor mutation"
      (unchangedActor gsNear gsD)) && ok

  match aPlayerScream gsNear 99 with
  | Except.error e =>
    ok := (← assert "A_PlayerScream bad mobj loud-error"
      (e.contains "A_PlayerScream")) && ok
  | Except.ok _ =>
    ok := (← assert "A_PlayerScream bad mobj should loud-error" false) && ok

  pure ok

end Doom.Playsim.EnemyNTest
