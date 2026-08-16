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
P2c-xxviii unit checks: `SPR_LAUN` in `P_TouchSpecialThing`.
LAUN is `P_GiveWeapon wp_missile false` (hardcoded; ignore `MF_DROPPED`).
Two clips of `am_misl` (`clipammo[3]=1`). Kept out of `EnemyTest.lean`
so that file stays under 1k.
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

namespace Doom.Playsim.InterXxviiiTest

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

private def ownedShotgun : Array Int32 := #[1, 1, 1, 0, 0, 0, 0, 0, 0]

private def ownedShotgunMissile : Array Int32 := #[1, 1, 1, 0, 1, 0, 0, 0, 0]

/-- Player at idx 0, special at idx 1 with a live `THF_MOBJ` thinker. -/
private def specialScene (specialFlags : UInt32) (ammo : Array Int32)
    (readyweapon : Int32) (weaponowned : Array Int32) (skill : Int32)
    (netgame : Bool) : GameState :=
  let gs0 := initFromLevel emptyLevel skill #[true, false, false, false] 0
  let toucher := {
    Mobj.empty with
    player := 0, health := 82, height := 56 * FRACUNIT
    flags := MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let special := {
    Mobj.empty with
    sprite := SPR_LAUN
    flags := specialFlags ||| MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let player := {
    Player.empty with
    mo := 0, health := 82, armorpoints := 200, armortype := 2
    ammo, maxammo := defaultMaxAmmo
    readyweapon, pendingweapon := wp_nochange, weaponowned
  }
  let th : Thinker := { traceId := 49, func := THF_MOBJ, payload := 1 }
  {
    gs0 with
    netgame
    mobjs := #[toucher, special]
    thinkers := #[th]
    players := GameState.arrSet gs0.players 0 player
  }

private def specialRemoved (gs : GameState) : Bool :=
  match gs.thinkers[0]? with
  | some th => th.func == THF_REMOVED
  | none => false

private def pickup (specialFlags : UInt32) (ammo : Array Int32)
    (readyweapon : Int32) (weaponowned : Array Int32) (skill : Int32) :
    Except String (Player × GameState) := do
  let gs ← touchSpecialThing
    (specialScene specialFlags ammo readyweapon weaponowned skill false) 1 0
  match gs.players[0]? with
  | none => throw "pickup: player lost"
  | some p => pure (p, gs)

private def ammoAt (p : Player) (i : Nat) : Int32 :=
  match p.ammo[i]? with | some v => v | none => 0

private def ownedAt (p : Player) (i : Nat) : Int32 :=
  match p.weaponowned[i]? with | some v => v | none => 0

private def launFlags (dropped : Bool) : UInt32 :=
  MF_SPECIAL ||| (if dropped then MF_DROPPED else (0 : UInt32))

def checkP2cXxviiiUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  ok := (← assert "SPR_LAUN=90" (SPR_LAUN == (90 : UInt32))) && ok
  ok := (← assert "clipammo[3]=1"
    (match clipammo[3]? with | some c => c == 1 | none => false)) && ok
  ok := (← assert "wp_missile=4" (wp_missile == (4 : Int32))) && ok

  -- Found launcher: two clips, own + pending, ready stays, sfx_wpnup. --
  match pickup (launFlags false) #[49, 9, 0, 5] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"LAUN found ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "LAUN ammo3 5→7" (ammoAt p 3 == 7)) && ok
    ok := (← assert "LAUN ammo012 unchanged"
      (ammoAt p 0 == 49 && ammoAt p 1 == 9 && ammoAt p 2 == 0)) && ok
    ok := (← assert "LAUN weaponowned[4]=1" (ownedAt p 4 == 1)) && ok
    ok := (← assert "LAUN pending=missile" (p.pendingweapon == wp_missile)) && ok
    ok := (← assert "LAUN ready stays shotgun" (p.readyweapon == wp_shotgun)) && ok
    ok := (← assert "LAUN hp/armor unchanged"
      (p.health == 82 && p.armorpoints == 200)) && ok
    ok := (← assert "LAUN bonuscount+6" (p.bonuscount == BONUSADD)) && ok
    ok := (← assert "LAUN itemcount unchanged" (p.itemcount == 0)) && ok
    ok := (← assert "LAUN special removed" (specialRemoved gs)) && ok
    ok := (← assert "LAUN sfx_wpnup draws M_Random" (gs.rng.rndindex == 1)) && ok

  -- Hardcoded dropped=false: MF_DROPPED still two clips, not one. --
  match pickup (launFlags true) #[49, 9, 0, 5] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"LAUN MF_DROPPED ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "LAUN MF_DROPPED still two clips 5→7"
      (ammoAt p 3 == 7 && p.pendingweapon == wp_missile && specialRemoved gs)) && ok
    ok := (← assert "LAUN MF_DROPPED not one clip"
      (ammoAt p 3 != 6)) && ok

  -- Already owned: still gives ammo; pending stays wp_nochange. --
  match pickup (launFlags false) #[49, 9, 0, 5] wp_shotgun ownedShotgunMissile 2 with
  | Except.error e =>
    ok := (← assert s!"LAUN owned ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "LAUN owned ammo3 5→7 pending stays 10"
      (ammoAt p 3 == 7 && p.pendingweapon == wp_nochange
        && ownedAt p 4 == 1 && specialRemoved gs)) && ok

  -- Already owned + full ammo: refuse, world unchanged. --
  match pickup (launFlags false) #[49, 9, 0, 50] wp_shotgun ownedShotgunMissile 2 with
  | Except.error e =>
    ok := (← assert s!"LAUN owned full ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "LAUN owned full world unchanged"
      (ammoAt p 3 == 50 && p.pendingweapon == wp_nochange
        && p.bonuscount == 0 && !specialRemoved gs)) && ok

  -- Not owned + full ammo: still grants weapon (gaveweapon). --
  match pickup (launFlags false) #[49, 9, 0, 50] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"LAUN full unowned ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "LAUN full unowned still grants"
      (ammoAt p 3 == 50 && ownedAt p 4 == 1
        && p.pendingweapon == wp_missile && specialRemoved gs)) && ok

  -- Cap at maxammo 50. --
  match pickup (launFlags false) #[49, 9, 0, 49] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"LAUN cap ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "LAUN 49+2 cap 50"
      (ammoAt p 3 == 50 && specialRemoved gs)) && ok

  -- Baby doubles two clips (4). --
  match pickup (launFlags false) #[49, 9, 0, 5] wp_shotgun ownedShotgun 0 with
  | Except.error e =>
    ok := (← assert s!"LAUN baby ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "LAUN baby doubles 5→9" (ammoAt p 3 == 9)) && ok

  -- Nightmare also doubles. --
  match pickup (launFlags false) #[49, 9, 0, 5] wp_shotgun ownedShotgun 4 with
  | Except.error e =>
    ok := (← assert s!"LAUN nightmare ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "LAUN nightmare doubles 5→9" (ammoAt p 3 == 9)) && ok

  -- Netgame is out of the open subset. --
  match touchSpecialThing
    (specialScene (launFlags false) #[49, 9, 0, 5] wp_shotgun ownedShotgun 2 true)
    1 0 with
  | Except.error e =>
    ok := (← assert "LAUN netgame loud-errors"
      (e.contains "P_GiveWeapon" && e.contains "netgame")) && ok
  | Except.ok _ =>
    ok := (← assert "LAUN netgame should throw" false) && ok
  pure ok

end Doom.Playsim.InterXxviiiTest
