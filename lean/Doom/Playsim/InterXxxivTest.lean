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
P2c-xxxiv unit checks: `SPR_STIM` in `P_TouchSpecialThing`.
STIM is `P_GiveBody` 10 (`MAXHEALTH=100`), distinct from `SPR_MEDI` 25.
Kept out of `EnemyTest.lean` so that file stays under 1k.
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

namespace Doom.Playsim.InterXxxivTest

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
    (health bonuscount : Int32) (ammo : Array Int32) : GameState :=
  let gs0 := initFromLevel emptyLevel 2 #[true, false, false, false] 0
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
    mo := 0, health, bonuscount
    ammo, maxammo := defaultMaxAmmo
    readyweapon := wp_missile, pendingweapon := wp_nochange
    weaponowned := #[1, 1, 1, 0, 1, 0, 0, 0, 0]
  }
  let th : Thinker := { traceId := 109, func := THF_MOBJ, payload := 1 }
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
    (health bonuscount : Int32) (ammo : Array Int32) :
    Except String (Player × GameState) := do
  let gs ←
    touchSpecialThing (specialScene sprite specialFlags health bonuscount ammo) 1 0
  match gs.players[0]? with
  | none => throw "pickup: player lost"
  | some p => pure (p, gs)

private def stimFlags (countItem : Bool) : UInt32 :=
  MF_SPECIAL ||| (if countItem then MF_COUNTITEM else (0 : UInt32))

private def ammoAt (p : Player) (i : Nat) : Int32 :=
  match p.ammo[i]? with | some v => v | none => 0

private def demoAmmo : Array Int32 := #[54, 13, 0, 3]

def checkP2cXxxivUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  ok := (← assert "SPR_STIM=68" (SPR_STIM == (68 : UInt32))) && ok
  ok := (← assert "SPR_MEDI=69" (SPR_MEDI == (69 : UInt32))) && ok
  ok := (← assert "SPR_STIM != SPR_MEDI" (SPR_STIM != SPR_MEDI)) && ok
  ok := (← assert "bodyMaxHealth=100" (bodyMaxHealth == (100 : Int32))) && ok
  ok := (← assert "wp_nochange=10" (wp_nochange == (10 : Int32))) && ok

  -- giveBody 10: wrapping +=, cap only if over MAXHEALTH=100. --
  let (p75, g75) := giveBody { Player.empty with health := 75 } 10
  ok := (← assert "giveBody 75+10=85" (g75 && p75.health == 85)) && ok
  let (p95, g95) := giveBody { Player.empty with health := 95 } 10
  ok := (← assert "giveBody 95+10 cap 100" (g95 && p95.health == 100)) && ok
  let (p100, g100) := giveBody { Player.empty with health := 100 } 10
  ok := (← assert "giveBody 100 no-op (not BON1 200)" (!g100 && p100.health == 100)) && ok
  let (p150, g150) := giveBody { Player.empty with health := 150 } 10
  ok := (← assert "giveBody 150 no-op (>=100)" (!g150 && p150.health == 150)) && ok
  let (pNeg, gNeg) := giveBody { Player.empty with health := (-5 : Int32) } 10
  ok := (← assert "giveBody negative health still adds"
    (gNeg && pNeg.health == (5 : Int32))) && ok

  -- STIM: GiveBody 10, dual-write, no COUNTITEM, remove + bonuscount += 6. --
  match pickup SPR_STIM (stimFlags false) 75 0 demoAmmo with
  | Except.error e =>
    ok := (← assert s!"STIM 75 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "STIM 75→85" (p.health == 85)) && ok
    ok := (← assert "STIM dual-write toucher mobj health"
      (match gs.mobjs[0]? with | some mo => mo.health == 85 | none => false)) && ok
    ok := (← assert "STIM itemcount unchanged" (p.itemcount == 0)) && ok
    ok := (← assert "STIM bonuscount+6" (p.bonuscount == BONUSADD)) && ok
    ok := (← assert "STIM pendingweapon stays 10" (p.pendingweapon == wp_nochange)) && ok
    ok := (← assert "STIM ammo unchanged"
      (ammoAt p 0 == 54 && ammoAt p 1 == 13 && ammoAt p 2 == 0 && ammoAt p 3 == 3)) && ok
    ok := (← assert "STIM special removed" (specialRemoved gs)) && ok
    ok := (← assert "STIM sfx_itemup draws no M_Random" (gs.rng.rndindex == 0)) && ok
  match pickup SPR_STIM (stimFlags false) 100 0 demoAmmo with
  | Except.error e =>
    ok := (← assert s!"STIM 100 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "STIM 100 world unchanged"
      (p.health == 100 && !specialRemoved gs && p.bonuscount == 0
        && ammoAt p 0 == 54 && p.pendingweapon == wp_nochange)) && ok
    ok := (← assert "STIM 100 no dual-write"
      (match gs.mobjs[0]? with | some mo => mo.health == 100 | none => false)) && ok
  match pickup SPR_STIM (stimFlags false) 95 0 demoAmmo with
  | Except.error e =>
    ok := (← assert s!"STIM 95 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "STIM 95→100 cap"
      (p.health == 100 && specialRemoved gs)) && ok
    ok := (← assert "STIM 95→100 dual-write cap"
      (match gs.mobjs[0]? with | some mo => mo.health == 100 | none => false)) && ok
  match pickup SPR_STIM (stimFlags false) Int32.maxValue 0 demoAmmo with
  | Except.error e =>
    ok := (← assert s!"STIM maxValue ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "STIM maxValue no-op (>=100)"
      (p.health == Int32.maxValue && !specialRemoved gs && p.bonuscount == 0)) && ok
  match pickup SPR_STIM (stimFlags true) 75 0 demoAmmo with
  | Except.error e =>
    ok := (← assert s!"STIM COUNTITEM ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "STIM COUNTITEM itemcount+1"
      (p.health == 85 && p.itemcount == 1 && specialRemoved gs)) && ok

  -- Distinct from MEDI: same scene, GiveBody 25 not 10. --
  match pickup SPR_MEDI (stimFlags false) 75 0 demoAmmo with
  | Except.error e =>
    ok := (← assert s!"MEDI-vs-STIM 75 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "MEDI 75→100 not STIM 85"
      (p.health == 100 && p.health != 85 && specialRemoved gs)) && ok

  -- Unsupported neighbor sprite still loud-errors. --
  match pickup (67 : UInt32) (stimFlags false) 75 0 demoAmmo with
  | Except.error e =>
    ok := (← assert "sprite 67 loud-errors"
      (e.contains "P_TouchSpecialThing" && e.contains "sprite 67")) && ok
  | Except.ok _ =>
    ok := (← assert "sprite 67 should throw" false) && ok
  pure ok

end Doom.Playsim.InterXxxivTest
