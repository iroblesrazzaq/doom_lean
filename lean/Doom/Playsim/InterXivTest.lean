import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Inter
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Thinker

/-!
P2c-xiv unit checks: `SPR_BON2` armor bonus in `P_TouchSpecialThing`.
C increment/type assign (not `P_GiveArmor`); P2c-xxix adds the 1.9
`deh_max_armor` cap (`> 200` then 200; pickup at 200 still succeeds).
Kept out of `EnemyTest.lean` so that file stays under 1k lines.
-/

open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Inter
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Thinker

namespace Doom.Playsim.InterXivTest

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

/-- Player at idx 0, BON2 special at idx 1 with a live `THF_MOBJ` thinker. -/
private def bon2Scene (armorpoints armortype : Int32) (countItem : Bool) : GameState :=
  let gs0 := initFromLevel emptyLevel 2 #[true, false, false, false] 0
  let toucher := {
    Mobj.empty with
    player := 0, health := 100, height := 56 * FRACUNIT
    flags := MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let countBit := if countItem then MF_COUNTITEM else (0 : UInt32)
  let special := {
    Mobj.empty with
    sprite := SPR_BON2
    flags := MF_SPECIAL ||| countBit ||| MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let player := {
    Player.empty with
    mo := 0, health := 100, armorpoints, armortype
  }
  let th : Thinker := { traceId := 153, func := THF_MOBJ, payload := 1 }
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

private def pickupBon2 (armorpoints armortype : Int32) (countItem : Bool) :
    Except String (Player × GameState) := do
  let gs ← touchSpecialThing (bon2Scene armorpoints armortype countItem) 1 0
  match gs.players[0]? with
  | none => throw "BON2: player lost"
  | some p => pure (p, gs)

def checkP2cXivUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  match pickupBon2 96 1 true with
  | Except.error e =>
    ok := (← assert s!"BON2 96 type1 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BON2 96 type1 → 97 type1"
      (p.armorpoints == 97 && p.armortype == 1)) && ok
    ok := (← assert "BON2 sets GOTARMBONUS"
      (p.message == some GOTARMBONUS)) && ok
    ok := (← assert "BON2 itemcount+1" (p.itemcount == 1)) && ok
    ok := (← assert "BON2 bonuscount+6" (p.bonuscount == BONUSADD)) && ok
    ok := (← assert "BON2 special removed" (specialRemoved gs)) && ok
  match pickupBon2 5 0 true with
  | Except.error e =>
    ok := (← assert s!"BON2 type0 ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "BON2 type0 → type1 + points++"
      (p.armorpoints == 6 && p.armortype == 1)) && ok
  match pickupBon2 99 1 true with
  | Except.error e =>
    ok := (← assert s!"BON2 99 type1 ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "BON2 99 type1 → 100 type1"
      (p.armorpoints == 100 && p.armortype == 1)) && ok
  match pickupBon2 199 1 true with
  | Except.error e =>
    ok := (← assert s!"BON2 199 ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "BON2 199 type1 → 200 type1"
      (p.armorpoints == 200 && p.armortype == 1)) && ok
  match pickupBon2 200 1 true with
  | Except.error e =>
    ok := (← assert s!"BON2 200 type1 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BON2 200 type1 still picks up, cap 200"
      (p.armorpoints == 200 && p.armortype == 1 && specialRemoved gs
        && p.bonuscount == BONUSADD && p.itemcount == 1)) && ok
  match pickupBon2 200 2 true with
  | Except.error e =>
    ok := (← assert s!"BON2 200 type2 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BON2 200 type2 still picks up, cap 200 type2"
      (p.armorpoints == 200 && p.armortype == 2 && specialRemoved gs
        && p.bonuscount == BONUSADD && p.itemcount == 1)) && ok
  match pickupBon2 50 2 true with
  | Except.error e =>
    ok := (← assert s!"BON2 type2 ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "BON2 type2 keeps type 2"
      (p.armorpoints == 51 && p.armortype == 2)) && ok
  match pickupBon2 Int32.maxValue 1 true with
  | Except.error e =>
    ok := (← assert s!"BON2 wrap ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "BON2 armorpoints++ wraps Int32, no cap"
      (p.armorpoints == Int32.minValue && p.armortype == 1)) && ok
  match pickupBon2 10 1 false with
  | Except.error e =>
    ok := (← assert s!"BON2 no COUNTITEM ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BON2 no COUNTITEM still increments"
      (p.armorpoints == 11 && p.armortype == 1)) && ok
    ok := (← assert "BON2 no COUNTITEM itemcount unchanged"
      (p.itemcount == 0 && p.bonuscount == BONUSADD && specialRemoved gs)) && ok
  pure ok

end Doom.Playsim.InterXivTest
