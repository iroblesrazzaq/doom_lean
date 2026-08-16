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
P2c-r unit checks: `SPR_SBOX` shell box in `P_TouchSpecialThing`.
`P_GiveAmmo am_shell 5` (`clipammo[1]=4` → +20 shells at medium). Kept out of
`EnemyTest.lean` so that file stays under 1k.
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

namespace Doom.Playsim.InterRTest

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

private def ownedFistPistol : Array Int32 := #[1, 1, 0, 0, 0, 0, 0, 0, 0]

private def ownedShotgun : Array Int32 := #[1, 1, 1, 0, 0, 0, 0, 0, 0]

/-- Player at idx 0, special at idx 1 with a live `THF_MOBJ` thinker. -/
private def specialScene (sprite : UInt32) (specialFlags : UInt32)
    (health : Int32) (ammo : Array Int32) (readyweapon : Int32)
    (weaponowned : Array Int32) (skill : Int32) : GameState :=
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
    readyweapon, pendingweapon := wp_nochange, weaponowned
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
    (health : Int32) (ammo : Array Int32) (readyweapon : Int32)
    (weaponowned : Array Int32) (skill : Int32) :
    Except String (Player × GameState) := do
  let gs ← touchSpecialThing
    (specialScene sprite specialFlags health ammo readyweapon weaponowned skill) 1 0
  match gs.players[0]? with
  | none => throw "pickup: player lost"
  | some p => pure (p, gs)

private def ammoAt (p : Player) (i : Nat) : Int32 :=
  match p.ammo[i]? with | some v => v | none => 0

private def sboxFlags : UInt32 := MF_SPECIAL

def checkP2cRUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  ok := (← assert "clipammo[1]=4"
    (match clipammo[1]? with | some c => c == 4 | none => false)) && ok
  ok := (← assert "SPR_SBOX=85" (SPR_SBOX == (85 : UInt32))) && ok

  -- SBOX: five clips of shells; medium +20; preference only when oldammo=0. --
  match pickup SPR_SBOX sboxFlags 83 #[34, 7, 0, 0] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"SBOX medium 7 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "SBOX medium shells 7→27"
      (ammoAt p 1 == 27)) && ok
    ok := (← assert "SBOX pending stays nochange (oldammo≠0)"
      (p.pendingweapon == wp_nochange)) && ok
    ok := (← assert "SBOX health unchanged" (p.health == 83)) && ok
    ok := (← assert "SBOX itemcount unchanged" (p.itemcount == 0)) && ok
    ok := (← assert "SBOX bonuscount+6" (p.bonuscount == BONUSADD)) && ok
    ok := (← assert "SBOX special removed" (specialRemoved gs)) && ok
  match pickup SPR_SBOX sboxFlags 83 #[34, 0, 0, 0] wp_fist ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"SBOX zero shells fist ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "SBOX zero shells fist pending shotgun"
      (ammoAt p 1 == 20 && p.pendingweapon == wp_shotgun)) && ok
  match pickup SPR_SBOX sboxFlags 83 #[34, 7, 0, 0] wp_fist ownedFistPistol 0 with
  | Except.error e =>
    ok := (← assert s!"SBOX baby ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "SBOX baby doubles 7→47"
      (ammoAt p 1 == 47)) && ok
  match pickup SPR_SBOX sboxFlags 83 #[34, 50, 0, 0] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"SBOX full ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "SBOX full ammo unchanged" (ammoAt p 1 == 50)) && ok
    ok := (← assert "SBOX full does not remove"
      (!specialRemoved gs && p.bonuscount == 0 && p.itemcount == 0)) && ok
  match pickup SPR_SBOX sboxFlags 83 #[34, 30, 0, 0] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"SBOX cap ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "SBOX 30+20 cap 50"
      (ammoAt p 1 == 50 && specialRemoved gs)) && ok

  -- Unsupported neighbor sprite still loud-errors. --
  match pickup (67 : UInt32) sboxFlags 83 #[34, 7, 0, 0] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert "sprite 67 loud-errors"
      (e.contains "P_TouchSpecialThing" && e.contains "sprite 67")) && ok
  | Except.ok _ =>
    ok := (← assert "sprite 67 should throw" false) && ok
  pure ok

end Doom.Playsim.InterRTest
