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
P2c-xxiii unit checks: `SPR_MEDI` / `SPR_YKEY` in `P_TouchSpecialThing`.
MEDI is `P_GiveBody` (MAXHEALTH=100), not BON1's cap-200 `health++`.
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

namespace Doom.Playsim.InterXxiiiTest

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

private def yellowHeld : Array Bool := #[false, true, false, false, false, false]

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

private def mediFlags (countItem : Bool) : UInt32 :=
  MF_SPECIAL ||| (if countItem then MF_COUNTITEM else (0 : UInt32))

private def ykeyFlags : UInt32 := MF_SPECIAL

/-- Two specials: idx 1 then idx 2, for MEDI-then-YKEY / YKEY-then-MEDI. -/
private def twoSpecialScene (sprA sprB : UInt32) (health bonuscount : Int32) : GameState :=
  let gs0 := initFromLevel emptyLevel 2 #[true, false, false, false] 0
  let toucher := {
    Mobj.empty with
    player := 0, health, height := 56 * FRACUNIT
    flags := MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let specA := {
    Mobj.empty with
    sprite := sprA
    flags := MF_SPECIAL ||| MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let specB := {
    Mobj.empty with
    sprite := sprB
    flags := MF_SPECIAL ||| MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let player := {
    Player.empty with
    mo := 0, health, bonuscount
    ammo := #[50, 4, 0, 0], maxammo := defaultMaxAmmo
    readyweapon := wp_pistol, pendingweapon := wp_nochange
    weaponowned := #[1, 1, 0, 0, 0, 0, 0, 0, 0]
  }
  let thA : Thinker := { traceId := 107, func := THF_MOBJ, payload := 1 }
  let thB : Thinker := { traceId := 8, func := THF_MOBJ, payload := 2 }
  {
    gs0 with
    mobjs := #[toucher, specA, specB]
    thinkers := #[thA, thB]
    players := GameState.arrSet gs0.players 0 player
  }

private def thinkerRemoved (gs : GameState) (thIdx : Nat) : Bool :=
  match gs.thinkers[thIdx]? with
  | some th => th.func == THF_REMOVED
  | none => false

private def yellowCard (p : Player) : Bool :=
  match p.cards[it_yellowcard]? with
  | some v => v
  | none => false

def checkP2cXxiiiUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  -- giveBody helper: MAXHEALTH=100, wrapping +=, cap only if over. --
  let (p65, g65) := giveBody { Player.empty with health := 65 } 25
  ok := (← assert "giveBody 65+25=90" (g65 && p65.health == 90)) && ok
  let (p90, g90) := giveBody { Player.empty with health := 90 } 25
  ok := (← assert "giveBody 90+25 cap 100" (g90 && p90.health == 100)) && ok
  let (p100, g100) := giveBody { Player.empty with health := 100 } 25
  ok := (← assert "giveBody 100 no-op (not BON1 200)" (!g100 && p100.health == 100)) && ok
  let (p150, g150) := giveBody { Player.empty with health := 150 } 25
  ok := (← assert "giveBody 150 no-op (>=100)" (!g150 && p150.health == 150)) && ok
  let (pWrap, gWrap) := giveBody { Player.empty with health := 90 } Int32.maxValue
  ok := (← assert "giveBody health+= wraps Int32"
    (gWrap && pWrap.health == (90 : Int32) + Int32.maxValue)) && ok
  let (pNeg, gNeg) := giveBody { Player.empty with health := (-5 : Int32) } 25
  ok := (← assert "giveBody negative health still adds"
    (gNeg && pNeg.health == (20 : Int32))) && ok

  -- MEDI: GiveBody 25, dual-write, no COUNTITEM, remove + bonuscount += 6. --
  match pickup SPR_MEDI (mediFlags false) 65 0 noCards false with
  | Except.error e =>
    ok := (← assert s!"MEDI 65 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "MEDI 65→90" (p.health == 90)) && ok
    ok := (← assert "MEDI dual-write toucher mobj health"
      (match gs.mobjs[0]? with | some mo => mo.health == 90 | none => false)) && ok
    ok := (← assert "MEDI itemcount unchanged" (p.itemcount == 0)) && ok
    ok := (← assert "MEDI bonuscount+6" (p.bonuscount == BONUSADD)) && ok
    ok := (← assert "MEDI pendingweapon stays 10" (p.pendingweapon == wp_nochange)) && ok
    ok := (← assert "MEDI special removed" (specialRemoved gs)) && ok
  match pickup SPR_MEDI (mediFlags false) 100 0 noCards false with
  | Except.error e =>
    ok := (← assert s!"MEDI 100 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "MEDI 100 world unchanged"
      (p.health == 100 && !specialRemoved gs && p.bonuscount == 0)) && ok
  match pickup SPR_MEDI (mediFlags false) 90 0 noCards false with
  | Except.error e =>
    ok := (← assert s!"MEDI 90 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "MEDI 90→100 cap"
      (p.health == 100 && specialRemoved gs)) && ok
  match pickup SPR_MEDI (mediFlags false) Int32.maxValue 0 noCards false with
  | Except.error e =>
    ok := (← assert s!"MEDI maxValue ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "MEDI maxValue no-op (>=100)"
      (p.health == Int32.maxValue && !specialRemoved gs && p.bonuscount == 0)) && ok
  match pickup SPR_MEDI (mediFlags true) 65 0 noCards false with
  | Except.error e =>
    ok := (← assert s!"MEDI COUNTITEM ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "MEDI COUNTITEM itemcount+1"
      (p.health == 90 && p.itemcount == 1 && specialRemoved gs)) && ok

  -- YKEY: GiveCard assign bonuscount then common += ; SP always removes. --
  match pickup SPR_YKEY ykeyFlags 65 0 noCards false with
  | Except.error e =>
    ok := (← assert s!"YKEY first ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "YKEY first cards[yellow]" (p.cards == yellowHeld)) && ok
    ok := (← assert "YKEY first bonuscount 0→12" (p.bonuscount == (12 : Int32))) && ok
    ok := (← assert "YKEY first health unchanged" (p.health == 65)) && ok
    ok := (← assert "YKEY first pendingweapon stays 10" (p.pendingweapon == wp_nochange)) && ok
    ok := (← assert "YKEY first removed" (specialRemoved gs)) && ok
  match pickup SPR_YKEY ykeyFlags 65 0 yellowHeld false with
  | Except.error e =>
    ok := (← assert s!"YKEY held ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "YKEY held GiveCard no-op still removes"
      (yellowCard p && specialRemoved gs && p.bonuscount == BONUSADD)) && ok
  match pickup SPR_YKEY ykeyFlags 65 0 noCards true with
  | Except.error e =>
    ok := (← assert s!"YKEY netgame ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "YKEY netgame grants card" (yellowCard p)) && ok
    ok := (← assert "YKEY netgame assign not +=" (p.bonuscount == BONUSADD)) && ok
    ok := (← assert "YKEY netgame does not remove" (!specialRemoved gs)) && ok
  match pickup SPR_YKEY ykeyFlags 65 0 yellowHeld true with
  | Except.error e =>
    ok := (← assert s!"YKEY netgame held ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "YKEY netgame held no-op no remove"
      (yellowCard p && !specialRemoved gs && p.bonuscount == 0)) && ok

  -- Same-tic order: MEDI += then YKEY assign overwrites. --
  match touchSpecialThing (twoSpecialScene SPR_MEDI SPR_YKEY 65 20) 1 0 with
  | Except.error e =>
    ok := (← assert s!"MEDI-then-YKEY MEDI ({e})" false) && ok
  | Except.ok gsM =>
    match gsM.players[0]? with
    | none => ok := (← assert "MEDI-then-YKEY player after MEDI" false) && ok
    | some pM =>
      ok := (← assert "MEDI-then-YKEY after MEDI hp 90 bonus 26"
        (pM.health == 90 && pM.bonuscount == (26 : Int32))) && ok
    match touchSpecialThing gsM 2 0 with
    | Except.error e =>
      ok := (← assert s!"MEDI-then-YKEY YKEY ({e})" false) && ok
    | Except.ok gsY =>
      match gsY.players[0]? with
      | none => ok := (← assert "MEDI-then-YKEY player after YKEY" false) && ok
      | some pY =>
        ok := (← assert "MEDI-then-YKEY YKEY overwrites bonus 26→12"
          (pY.health == 90 && pY.bonuscount == (12 : Int32) && yellowCard pY)) && ok
        ok := (← assert "MEDI-then-YKEY both removed"
          (thinkerRemoved gsY 0 && thinkerRemoved gsY 1)) && ok
        ok := (← assert "MEDI-then-YKEY dual-write"
          (match gsY.mobjs[0]? with | some mo => mo.health == 90 | none => false)) && ok

  match touchSpecialThing (twoSpecialScene SPR_YKEY SPR_MEDI 65 0) 1 0 with
  | Except.error e =>
    ok := (← assert s!"YKEY-then-MEDI YKEY ({e})" false) && ok
  | Except.ok gsY =>
    match touchSpecialThing gsY 2 0 with
    | Except.error e =>
      ok := (← assert s!"YKEY-then-MEDI MEDI ({e})" false) && ok
    | Except.ok gsM =>
      match gsM.players[0]? with
      | none => ok := (← assert "YKEY-then-MEDI player" false) && ok
      | some p =>
        ok := (← assert "YKEY-then-MEDI bonus 12 then +=6 → 18"
          (p.health == 90 && p.bonuscount == (18 : Int32) && yellowCard p)) && ok

  -- empty / reborn cards are all false. --
  ok := (← assert "empty cards size 6 all false"
    (Player.empty.cards == noCards)) && ok
  ok := (← assert "playerReborn cards size 6 all false"
    ((playerReborn Player.empty).cards == noCards)) && ok
  pure ok

end Doom.Playsim.InterXxiiiTest
