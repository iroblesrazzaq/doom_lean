import Doom.Harness.Real
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.PlayerThink
import Doom.Playsim.Sound
import Doom.Playsim.Spawn
import Doom.Playsim.Spec
import Doom.Playsim.Thinker
import Doom.Wad

/-!
P2c-xvii unit checks: `usedown` latch, `PTR_UseTraverse` miss keep-checking,
wall `sfx_noway` rnd+1, special 1 opens. Kept out of `EnemyTest.lean` so that
file stays under 1k lines.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.MapUtil
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.PlayerThink
open Doom.Playsim.Sound
open Doom.Playsim.Spawn
open Doom.Playsim.Spec
open Doom.Playsim.Thinker
open Doom.Wad

namespace Doom.Playsim.UseLinesXviiTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def dummySector (ceilingheight : Int32) : Sector := {
  floorheight := 0, ceilingheight
  floorpic := ByteArray.empty, ceilingpic := ByteArray.empty
  lightlevel := 0, special := 0, tag := 0
  lines := #[], blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def dummySide (sec : UInt32) : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := ByteArray.empty, bottomtexture := ByteArray.empty
  midtexture := ByteArray.empty, sector := sec
}

private def mkLine (special sidenum1 frontsector backsector : Int32) : Line := {
  v1 := 0, v2 := 1, flags := 0, special, tag := 0
  sidenum0 := 0, sidenum1, dx := 0, dy := FRACUNIT
  slopetype := ST_VERTICAL, bbox := #[FRACUNIT, 0, 0, 0]
  frontsector, backsector
}

private def interceptLine (lineIdx : Nat) : Intercept := {
  frac := 0, isaline := true, lineIdx, thingIdx := 0
}

private def twoSectorLevel (ld : Line) : LevelData := {
  vertexes := #[{ x := 0, y := 0 }, { x := 0, y := FRACUNIT }]
  sectors := #[dummySector (10 * FRACUNIT), dummySector (10 * FRACUNIT)]
  sides := #[dummySide 0, dummySide 1]
  lines := #[ld]
  segs := #[]
  subsectors := #[{ numsegs := 0, firstseg := 0, sector := 0 }]
  nodes := #[]
  things := #[]
  blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
  reject := ByteArray.empty
}

private def withThing (gs0 : GameState) : GameState :=
  let mo : Mobj := { Mobj.empty with player := 0, x := FRACUNIT, y := 0 }
  let p : Player := {
    Player.empty with
    mo := 0, playerstate := PST_LIVE, health := 100, viewheight := VIEWHEIGHT
  }
  { gs0 with
    mobjs := #[mo]
    players := GameState.arrSet gs0.players 0 p
  }

def checkP2cXviiUnits (wad : WadDirectory) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  -- usedown latch (reborn + PlayerThink) -----------------------------------
  ok := (← assert "playerReborn usedown=true" (playerReborn Player.empty).usedown) && ok
  match Doom.Harness.Real.loadMap wad "E1M1" with
  | Except.error e =>
    ok := (← assert s!"E1M1 load useLines ({e})" false) && ok
  | Except.ok level =>
    let gs0 := initFromLevel level 3 #[true, false, false, false] 0
    let px := (-16 : Int32) <<< 16
    let py := (-496 : Int32) <<< 16
    match spawnMobj gs0 px py ONFLOORZ 0 with
    | Except.error e =>
      ok := (← assert s!"spawn useLines player ({e})" false) && ok
    | Except.ok (gs1, pIdx) =>
      match gs1.mobjs[pIdx]? with
      | none => ok := (← assert "useLines player mobj" false) && ok
      | some pmo =>
        let attach (gs : GameState) (usedown : Bool) (buttons : UInt32) : GameState :=
          let p : Player := {
            Player.empty with
            mo := pIdx.toInt32
            playerstate := PST_LIVE
            health := 100
            viewheight := VIEWHEIGHT
            usedown
            cmd := { TicCmd.zero with buttons }
          }
          {
            gs with
            mobjs := GameState.arrSet gs.mobjs pIdx { pmo with player := 0, reactiontime := 0 }
            players := GameState.arrSet gs.players 0 p
          }
        let gsUp := attach gs1 true 0
        match playerThink gsUp 0 with
        | Except.error e =>
          ok := (← assert s!"usedown USE-up ({e})" false) && ok
        | Except.ok gsReleased =>
          match gsReleased.players[0]? with
          | none => ok := (← assert "usedown USE-up player" false) && ok
          | some p =>
            ok := (← assert "usedown USE-up clears latch" (!p.usedown)) && ok
          let vc0 := gsReleased.validcount
          let gsPress := attach gsReleased false BT_USE
          match playerThink gsPress 0 with
          | Except.error e =>
            ok := (← assert s!"usedown first USE ({e})" false) && ok
          | Except.ok gsUsed =>
            match gsUsed.players[0]? with
            | none => ok := (← assert "usedown first USE player" false) && ok
            | some p =>
              ok := (← assert "usedown first USE sets latch" p.usedown) && ok
            ok := (← assert "usedown first USE calls P_UseLines"
              (gsUsed.validcount == vc0 + 1)) && ok
            let vc1 := gsUsed.validcount
            match playerThink gsUsed 0 with
            | Except.error e =>
              ok := (← assert s!"usedown held USE ({e})" false) && ok
            | Except.ok gsSkip =>
              match gsSkip.players[0]? with
              | none => ok := (← assert "usedown held USE player" false) && ok
              | some p =>
                ok := (← assert "usedown held USE keeps latch" p.usedown) && ok
              ok := (← assert "usedown held USE skips P_UseLines"
                (gsSkip.validcount == vc1)) && ok

  -- miss keep-checking -----------------------------------------------------
  let missLine := mkLine 0 1 0 1
  let gsMiss0 := withThing (initFromLevel (twoSectorLevel missLine) 2 #[true, false, false, false] 0)
  match ptrUseTraverse gsMiss0 0 (interceptLine 0) with
  | Except.error e =>
    ok := (← assert s!"PTR_UseTraverse miss ({e})" false) && ok
  | Except.ok (gsMiss, cont) =>
    ok := (← assert "PTR_UseTraverse miss keep-checking" cont) && ok
    ok := (← assert "PTR_UseTraverse miss no RNG"
      (gsMiss.rng.rndindex == gsMiss0.rng.rndindex)) && ok
    ok := (← assert "PTR_UseTraverse miss no door"
      (gsMiss.verticalDoors.size == 0)) && ok
  match ptrUseTraverse gsMiss0 0 { frac := 0, isaline := false, lineIdx := 0, thingIdx := 0 } with
  | Except.error e =>
    ok := (← assert "PTR_UseTraverse !isaline loud-error"
      (e.contains "not a line")) && ok
  | Except.ok _ =>
    ok := (← assert "PTR_UseTraverse !isaline should loud-error" false) && ok

  -- wall noway rnd+1 -------------------------------------------------------
  let wallLine := mkLine 0 (-1) 0 (-1)
  let gsWall0 := withThing (initFromLevel (twoSectorLevel wallLine) 2 #[true, false, false, false] 0)
  match ptrUseTraverse gsWall0 0 (interceptLine 0) with
  | Except.error e =>
    ok := (← assert s!"PTR_UseTraverse wall ({e})" false) && ok
  | Except.ok (gsWall, cont) =>
    ok := (← assert "PTR_UseTraverse wall stops" (!cont)) && ok
    ok := (← assert "PTR_UseTraverse wall noway rnd+1"
      (gsWall.rng.rndindex == gsWall0.rng.rndindex + 1)) && ok

  -- special 1 opens --------------------------------------------------------
  let doorLine := mkLine 1 1 0 1
  let gsDoor0 := withThing (initFromLevel (twoSectorLevel doorLine) 2 #[true, false, false, false] 0)
  match ptrUseTraverse gsDoor0 0 (interceptLine 0) with
  | Except.error e =>
    ok := (← assert s!"PTR_UseTraverse special 1 ({e})" false) && ok
  | Except.ok (gsDoor, cont) =>
    ok := (← assert "PTR_UseTraverse special 1 stops" (!cont)) && ok
    ok := (← assert "PTR_UseTraverse special 1 opens"
      (gsDoor.verticalDoors.size == 1)) && ok
    let mut foundDoor := false
    let mut i : Nat := 0
    while i < gsDoor.thinkers.size do
      match gsDoor.thinkers[i]? with
      | some th =>
        if th.func == THF_VERTICALDOOR then foundDoor := true
      | none => pure ()
      i := i + 1
    ok := (← assert "PTR_UseTraverse special 1 +THF_VERTICALDOOR" foundDoor) && ok
  pure ok

end Doom.Playsim.UseLinesXviiTest
