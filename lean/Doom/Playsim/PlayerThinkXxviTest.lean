import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.PlayerThink
import Doom.Playsim.Spawn
import Doom.Playsim.Weapons

/-!
P2c-xxvi unit checks: `P_PlayerInSpecialSector` secret (`special=9`).
Wrapping `secretcount++`, clear `sectors[secIdx].special`, no damage /
`P_Random` / `totalsecret` change. One-shot via `playerThink` skip.
Kept out of `EnemyTest.lean` so that file stays under 1k lines.
-/

open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.PlayerThink
open Doom.Playsim.Spawn
open Doom.Playsim.Weapons

namespace Doom.Playsim.PlayerThinkXxviTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def floorLevel : LevelData := {
  vertexes := #[]
  sectors := #[{
    floorheight := 0, ceilingheight := 10 * FRACUNIT
    floorpic := ByteArray.empty, ceilingpic := ByteArray.empty
    lightlevel := 0, special := 0, tag := 0
    lines := #[], blockbox := #[0, 0, 0, 0]
    soundorgX := 0, soundorgY := 0
  }]
  sides := #[]
  lines := #[]
  segs := #[]
  subsectors := #[{ numsegs := 0, firstseg := 0, sector := 0 }]
  nodes := #[]
  things := #[]
  blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
  reject := ByteArray.empty
}

/-- Player on/off floor; `totalsecret` pre-set so spawn increment is distinct. -/
private def playerScene (secretcount totalsecret z : Int32) : GameState :=
  let gs0 := initFromLevel floorLevel 2 #[true, false, false, false] 0
  let mo := {
    Mobj.empty with
    typeId := 0, player := 0, health := 90, z
    flags := MF_SOLID ||| MF_SHOOTABLE
    height := 56 * FRACUNIT, radius := 16 * FRACUNIT
    subsector := 0
  }
  let player := {
    Player.empty with
    mo := 0, playerstate := PST_LIVE
    health := 90, armorpoints := 85, armortype := 1
    pendingweapon := wp_nochange, readyweapon := wp_pistol
    secretcount
  }
  {
    gs0 with
    totalsecret
    mobjs := #[mo]
    players := GameState.arrSet gs0.players 0 player
  }

/-- Plant `special` into `gs.sectors[0]` so write-back is observable. -/
private def plantSpecial (gs : GameState) (special : Int32) : GameState :=
  match gs.sectors[0]? with
  | some sec => { gs with sectors := GameState.arrSet gs.sectors 0 { sec with special } }
  | none => gs

private def runPlanted (gs : GameState) (special : Int32) : Except String GameState :=
  let gs := plantSpecial gs special
  match gs.sectors[0]? with
  | none => throw "test: missing sector"
  | some sec => playerInSpecialSector gs 0 0 sec

private def secretOk (gs : GameState) (wantSecret totalsecret : Int32) : Bool :=
  match gs.players[0]?, gs.sectors[0]?, gs.mobjs[0]? with
  | some p, some sec, some mo =>
    p.secretcount == wantSecret
      && sec.special == 0
      && gs.totalsecret == totalsecret
      && p.health == 90 && p.armorpoints == 85 && mo.health == 90
      && gs.rng.prndindex == 0 && gs.rng.rndindex == 0
  | _, _, _ => false

def checkP2cXxviUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  match runPlanted (playerScene 0 7 0) 9 with
  | Except.error e =>
    ok := (← assert s!"secret on floor ({e})" false) && ok
  | Except.ok gs =>
    ok := (← assert "secretcount 0→1 wrapping ++" (secretOk gs 1 7)) && ok

  match runPlanted (playerScene Int32.maxValue 7 0) 9 with
  | Except.error e =>
    ok := (← assert s!"secret wrap ({e})" false) && ok
  | Except.ok gs =>
    ok := (← assert "secretcount++ wraps Int32"
      (secretOk gs Int32.minValue 7)) && ok

  match runPlanted (playerScene 0 7 FRACUNIT) 9 with
  | Except.error e =>
    ok := (← assert s!"secret airborne ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]?, gs.sectors[0]? with
    | some p, some sec =>
      ok := (← assert "secret airborne identity"
        (p.secretcount == 0 && sec.special == 9
          && gs.totalsecret == 7 && p.health == 90
          && gs.rng.prndindex == 0 && gs.rng.rndindex == 0)) && ok
    | _, _ => ok := (← assert "secret airborne player/sector" false) && ok

  let gsThink0 :=
    let gs := plantSpecial (playerScene 0 7 0) 9
    match gs.mobjs[0]?, gs.players[0]? with
    | some mo, some p =>
      {
        gs with
        mobjs := #[
          { mo with floorz := 0, ceilingz := 10 * FRACUNIT, z := 0 }
        ]
        players := GameState.arrSet gs.players 0
          { p with viewheight := VIEWHEIGHT, usedown := false }
      }
    | _, _ => gs
  match playerThink gsThink0 0 with
  | Except.error e =>
    ok := (← assert s!"playerThink secret ({e})" false) && ok
  | Except.ok gs1 =>
    match gs1.players[0]?, gs1.sectors[0]? with
    | some p1, some sec1 =>
      ok := (← assert "playerThink secretcount 0→1 special=0"
        (p1.secretcount == 1 && sec1.special == 0
          && gs1.totalsecret == 7 && p1.health == 90
          && p1.armorpoints == 85)) && ok
    | _, _ => ok := (← assert "playerThink secret player/sector" false) && ok
    match playerThink gs1 0 with
    | Except.error e =>
      ok := (← assert s!"playerThink secret skip ({e})" false) && ok
    | Except.ok gs2 =>
      match gs2.players[0]?, gs2.sectors[0]? with
      | some p2, some sec2 =>
        ok := (← assert "playerThink second tic one-shot"
          (p2.secretcount == 1 && sec2.special == 0
            && gs2.totalsecret == 7)) && ok
      | _, _ => ok := (← assert "playerThink skip player/sector" false) && ok

  -- Spawn totalsecret++ must not clear special=9 (playerThink consumes it later). --
  let gsSpawn := spawnSpecials (plantSpecial (playerScene 0 0 0) 9)
  match gsSpawn.sectors[0]? with
  | some sec =>
    ok := (← assert "spawn special=9 totalsecret++ keeps special"
      (gsSpawn.totalsecret == 1 && sec.special == 9)) && ok
  | none => ok := (← assert "spawn special=9 sector" false) && ok

  pure ok

end Doom.Playsim.PlayerThinkXxviTest
