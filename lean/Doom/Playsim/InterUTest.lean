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
P2c-u unit checks: `SPR_BKEY` in `P_TouchSpecialThing`.
Mirrors `InterXxiiiTest` YKEY coverage with `it_bluecard`.
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

namespace Doom.Playsim.InterUTest

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
    (health bonuscount : Int32) (cards : Array Bool) (netgame : Bool) : GameState :=
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
    mo := 0, health, bonuscount, cards
    ammo := #[50, 4, 0, 0], maxammo := defaultMaxAmmo
    readyweapon := wp_pistol, pendingweapon := wp_nochange
    weaponowned := #[1, 1, 0, 0, 0, 0, 0, 0, 0]
  }
  let th : Thinker := { traceId := 7, func := THF_MOBJ, payload := 1 }
  {
    gs0 with
    netgame
    mobjs := #[toucher, special]
    thinkers := #[th]
    players := GameState.arrSet gs0.players 0 player
  }

private def noCards : Array Bool := Array.replicate NUMCARDS false

private def blueHeld : Array Bool := #[true, false, false, false, false, false]

private def specialRemoved (gs : GameState) : Bool :=
  match gs.thinkers[0]? with
  | some th => th.func == THF_REMOVED
  | none => false

private def pickup (sprite : UInt32) (specialFlags : UInt32)
    (health bonuscount : Int32) (cards : Array Bool) (netgame : Bool) :
    Except String (Player × GameState) := do
  let gs ←
    touchSpecialThing (specialScene sprite specialFlags health bonuscount cards netgame) 1 0
  match gs.players[0]? with
  | none => throw "pickup: player lost"
  | some p => pure (p, gs)

private def bkeyFlags : UInt32 := MF_SPECIAL

private def blueCard (p : Player) : Bool :=
  match p.cards[it_bluecard]? with
  | some v => v
  | none => false

def checkP2cUUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  -- BKEY: GiveCard assign bonuscount then common += ; SP always removes. --
  match pickup SPR_BKEY bkeyFlags 65 0 noCards false with
  | Except.error e =>
    ok := (← assert s!"BKEY first ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BKEY first cards[blue]" (p.cards == blueHeld)) && ok
    ok := (← assert "BKEY first bonuscount 0→12" (p.bonuscount == (12 : Int32))) && ok
    ok := (← assert "BKEY first health unchanged" (p.health == 65)) && ok
    ok := (← assert "BKEY first pendingweapon stays 10" (p.pendingweapon == wp_nochange)) && ok
    ok := (← assert "BKEY first removed" (specialRemoved gs)) && ok
  match pickup SPR_BKEY bkeyFlags 65 0 blueHeld false with
  | Except.error e =>
    ok := (← assert s!"BKEY held ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BKEY held GiveCard no-op still removes"
      (blueCard p && specialRemoved gs && p.bonuscount == BONUSADD)) && ok
  match pickup SPR_BKEY bkeyFlags 65 0 noCards true with
  | Except.error e =>
    ok := (← assert s!"BKEY netgame ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BKEY netgame grants card" (blueCard p)) && ok
    ok := (← assert "BKEY netgame assign not +=" (p.bonuscount == BONUSADD)) && ok
    ok := (← assert "BKEY netgame does not remove" (!specialRemoved gs)) && ok
  match pickup SPR_BKEY bkeyFlags 65 0 blueHeld true with
  | Except.error e =>
    ok := (← assert s!"BKEY netgame held ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BKEY netgame held no-op no remove"
      (blueCard p && !specialRemoved gs && p.bonuscount == 0)) && ok

  pure ok

end Doom.Playsim.InterUTest
