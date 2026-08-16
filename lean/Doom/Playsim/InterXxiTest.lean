import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Inter
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Thinker
import Doom.Playsim.Weapons

/-!
P2c-xxi unit checks: `SPR_BON1` / `SPR_SHEL` / `SPR_CLIP` in
`P_TouchSpecialThing`. BON1 is C `health++` + cap-if-over-200 (not
`P_GiveBody`). Kept out of `EnemyTest.lean` so that file stays under 1k.
-/

open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Inter
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Thinker
open Doom.Playsim.Weapons

namespace Doom.Playsim.InterXxiTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

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

/-- Player at idx 0, special at idx 1 with a live `THF_MOBJ` thinker. -/
private def specialScene (sprite : UInt32) (specialFlags : UInt32)
    (health : Int32) (ammo : Array Int32) (skill : Int32) : GameState :=
  let gs0 := initFromLevel emptyLevel skill #[true, false, false, false] 0
  let toucher := {
    Mobj.empty with
    player := 0, health, height := 56 * FRACUNIT
    flags := MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let special := {
    Mobj.empty with
    sprite
    flags := specialFlags ||| MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let player := {
    Player.empty with
    mo := 0, health, ammo, maxammo := defaultMaxAmmo
    readyweapon := wp_pistol, pendingweapon := wp_nochange
    weaponowned := #[1, 1, 0, 0, 0, 0, 0, 0, 0]
  }
  let th : Thinker := { traceId := 7, func := THF_MOBJ, payload := 1 }
  {
    gs0 with
    mobjs := #[toucher, special]
    thinkers := #[th]
    players := GameState.arrSet gs0.players 0 player
  }

private def specialRemoved (gs : GameState) : Bool :=
  match gs.thinkers[0]? with
  | some th => th.func == THF_REMOVED
  | none => false

private def pickup (sprite : UInt32) (specialFlags : UInt32)
    (health : Int32) (ammo : Array Int32) (skill : Int32) :
    Except String (Player × GameState) := do
  let gs ← touchSpecialThing (specialScene sprite specialFlags health ammo skill) 1 0
  match gs.players[0]? with
  | none => throw "pickup: player lost"
  | some p => pure (p, gs)

private def bon1Flags (countItem : Bool) : UInt32 :=
  MF_SPECIAL ||| (if countItem then MF_COUNTITEM else (0 : UInt32))

private def shelFlags : UInt32 := MF_SPECIAL

private def clipFlags (dropped : Bool) : UInt32 :=
  MF_SPECIAL ||| (if dropped then MF_DROPPED else (0 : UInt32))

def checkP2cXxiUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  -- BON1: wrapping ++, dual-write player + toucher mobj, COUNTITEM/remove. --
  match pickup SPR_BON1 (bon1Flags true) 82 #[50, 4, 0, 0] 2 with
  | Except.error e =>
    ok := (← assert s!"BON1 82 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BON1 82→83" (p.health == 83)) && ok
    ok := (← assert "BON1 dual-write toucher mobj health"
      (match gs.mobjs[0]? with | some mo => mo.health == 83 | none => false)) && ok
    ok := (← assert "BON1 itemcount+1" (p.itemcount == 1)) && ok
    ok := (← assert "BON1 bonuscount+6" (p.bonuscount == BONUSADD)) && ok
    ok := (← assert "BON1 special removed" (specialRemoved gs)) && ok
  match pickup SPR_BON1 (bon1Flags true) 100 #[50, 4, 0, 0] 2 with
  | Except.error e =>
    ok := (← assert s!"BON1 100 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BON1 100→101 (not P_GiveBody)"
      (p.health == 101 && specialRemoved gs)) && ok
  match pickup SPR_BON1 (bon1Flags true) 199 #[50, 4, 0, 0] 2 with
  | Except.error e =>
    ok := (← assert s!"BON1 199 ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "BON1 199→200" (p.health == 200)) && ok
  match pickup SPR_BON1 (bon1Flags true) 200 #[50, 4, 0, 0] 2 with
  | Except.error e =>
    ok := (← assert s!"BON1 200 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BON1 200 still picks up, cap 200"
      (p.health == 200 && specialRemoved gs && p.bonuscount == BONUSADD)) && ok
    ok := (← assert "BON1 200 dual-write stays 200"
      (match gs.mobjs[0]? with | some mo => mo.health == 200 | none => false)) && ok
  match pickup SPR_BON1 (bon1Flags true) Int32.maxValue #[50, 4, 0, 0] 2 with
  | Except.error e =>
    ok := (← assert s!"BON1 wrap ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BON1 health++ wraps Int32"
      (p.health == Int32.minValue && specialRemoved gs)) && ok
  match pickup SPR_BON1 (bon1Flags false) 82 #[50, 4, 0, 0] 2 with
  | Except.error e =>
    ok := (← assert s!"BON1 no COUNTITEM ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BON1 no COUNTITEM still heals"
      (p.health == 83 && p.itemcount == 0 && p.bonuscount == BONUSADD
        && specialRemoved gs)) && ok

  -- SHEL: one clip of shells; full ammo leaves world unchanged. --
  match pickup SPR_SHEL shelFlags 83 #[34, 4, 0, 0] 2 with
  | Except.error e =>
    ok := (← assert s!"SHEL 4 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "SHEL shells 4→8"
      (match p.ammo[1]? with | some v => v == 8 | none => false)) && ok
    ok := (← assert "SHEL health unchanged" (p.health == 83)) && ok
    ok := (← assert "SHEL itemcount unchanged" (p.itemcount == 0)) && ok
    ok := (← assert "SHEL bonuscount+6" (p.bonuscount == BONUSADD)) && ok
    ok := (← assert "SHEL special removed" (specialRemoved gs)) && ok
  match pickup SPR_SHEL shelFlags 83 #[34, 4, 0, 0] 0 with
  | Except.error e =>
    ok := (← assert s!"SHEL baby ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "SHEL baby doubles 4→12"
      (match p.ammo[1]? with | some v => v == 12 | none => false)) && ok
  match pickup SPR_SHEL shelFlags 83 #[34, 50, 0, 0] 2 with
  | Except.error e =>
    ok := (← assert s!"SHEL full ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "SHEL full ammo unchanged"
      (match p.ammo[1]? with | some v => v == 50 | none => false)) && ok
    ok := (← assert "SHEL full does not remove"
      (!specialRemoved gs && p.bonuscount == 0 && p.itemcount == 0)) && ok

  -- CLIP: found = 1 clip; dropped = half clip; full ammo no-op. --
  match pickup SPR_CLIP (clipFlags false) 83 #[34, 11, 0, 0] 2 with
  | Except.error e =>
    ok := (← assert s!"CLIP found ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "CLIP found 34→44"
      (match p.ammo[0]? with | some v => v == 44 | none => false)) && ok
    ok := (← assert "CLIP found itemcount unchanged" (p.itemcount == 0)) && ok
    ok := (← assert "CLIP found removed" (specialRemoved gs)) && ok
  match pickup SPR_CLIP (clipFlags true) 83 #[34, 11, 0, 0] 2 with
  | Except.error e =>
    ok := (← assert s!"CLIP dropped ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "CLIP dropped 34→39"
      (match p.ammo[0]? with | some v => v == 39 | none => false)) && ok
    ok := (← assert "CLIP dropped itemcount unchanged" (p.itemcount == 0)) && ok
    ok := (← assert "CLIP dropped removed"
      (specialRemoved gs && p.bonuscount == BONUSADD)) && ok
  match pickup SPR_CLIP (clipFlags true) 83 #[200, 11, 0, 0] 2 with
  | Except.error e =>
    ok := (← assert s!"CLIP full ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "CLIP full ammo unchanged"
      (match p.ammo[0]? with | some v => v == 200 | none => false)) && ok
    ok := (← assert "CLIP full does not remove"
      (!specialRemoved gs && p.bonuscount == 0)) && ok
  pure ok

end Doom.Playsim.InterXxiTest
