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
P2c-xxvii unit checks: `SPR_ARM2` / `SPR_BROK` in `P_TouchSpecialThing`.
ARM2 is `P_GiveArmor` class 2 (hits=200). BROK is `P_GiveAmmo am_misl 5`
(`clipammo[3]=1`). Zero-ammo missile preference only if `readyweapon` is
fist. Kept out of `EnemyTest.lean` so that file stays under 1k.
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

namespace Doom.Playsim.InterXxviiTest

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

private def ownedShotgunMissile : Array Int32 := #[1, 1, 1, 0, 1, 0, 0, 0, 0]

private def ownedFistMissile : Array Int32 := #[1, 1, 0, 0, 1, 0, 0, 0, 0]

/-- Player at idx 0, special at idx 1 with a live `THF_MOBJ` thinker. -/
private def specialScene (sprite : UInt32) (specialFlags : UInt32)
    (health armorpoints armortype : Int32) (ammo : Array Int32)
    (readyweapon : Int32) (weaponowned : Array Int32) (skill : Int32) :
    GameState :=
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
    mo := 0, health, armorpoints, armortype, ammo, maxammo := defaultMaxAmmo
    readyweapon, pendingweapon := wp_nochange, weaponowned
  }
  let th : Thinker := { traceId := 9, func := THF_MOBJ, payload := 1 }
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
    (health armorpoints armortype : Int32) (ammo : Array Int32)
    (readyweapon : Int32) (weaponowned : Array Int32) (skill : Int32) :
    Except String (Player × GameState) := do
  let gs ← touchSpecialThing
    (specialScene sprite specialFlags health armorpoints armortype ammo
      readyweapon weaponowned skill) 1 0
  match gs.players[0]? with
  | none => throw "pickup: player lost"
  | some p => pure (p, gs)

private def ammoAt (p : Player) (i : Nat) : Int32 :=
  match p.ammo[i]? with | some v => v | none => 0

private def arm2Flags : UInt32 := MF_SPECIAL

private def brokFlags : UInt32 := MF_SPECIAL

/-- Two specials: idx 1 then idx 2, for same-tic ARM2+BROK. -/
private def twoSpecialScene (sprA sprB : UInt32) : GameState :=
  let gs0 := initFromLevel emptyLevel 2 #[true, false, false, false] 0
  let toucher := {
    Mobj.empty with
    player := 0, health := 82, height := 56 * FRACUNIT
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
    mo := 0, health := 82, armorpoints := 85, armortype := 1
    ammo := #[49, 9, 0, 0], maxammo := defaultMaxAmmo
    readyweapon := wp_shotgun, pendingweapon := wp_nochange
    weaponowned := ownedShotgun
  }
  let thA : Thinker := { traceId := 9, func := THF_MOBJ, payload := 1 }
  let thB : Thinker := { traceId := 50, func := THF_MOBJ, payload := 2 }
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

def checkP2cXxviiUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  ok := (← assert "clipammo[3]=1"
    (match clipammo[3]? with | some c => c == 1 | none => false)) && ok
  ok := (← assert "blueArmorClass=2" (blueArmorClass == (2 : Int32))) && ok

  -- giveArmor class 2: refuse if armorpoints >= 200; live 85 grants type 1→2. --
  let (p85, g85) := giveArmor
    { Player.empty with armorpoints := 85, armortype := 1 } blueArmorClass
  ok := (← assert "giveArmor 85 type1 → 200 type2"
    (g85 && p85.armorpoints == 200 && p85.armortype == 2)) && ok
  let (p200, g200) := giveArmor
    { Player.empty with armorpoints := 200, armortype := 2 } blueArmorClass
  ok := (← assert "giveArmor 200 type2 refuse"
    (!g200 && p200.armorpoints == 200 && p200.armortype == 2)) && ok
  let (p199, g199) := giveArmor
    { Player.empty with armorpoints := 199, armortype := 1 } blueArmorClass
  ok := (← assert "giveArmor 199 type1 still grants"
    (g199 && p199.armorpoints == 200 && p199.armortype == 2)) && ok
  let (pMax, gMax) := giveArmor
    { Player.empty with armorpoints := Int32.maxValue, armortype := 1 }
    blueArmorClass
  ok := (← assert "giveArmor maxValue refuse (>=200)"
    (!gMax && pMax.armorpoints == Int32.maxValue && pMax.armortype == 1)) && ok
  let (pNeg, gNeg) := giveArmor
    { Player.empty with armorpoints := (-1 : Int32), armortype := 1 }
    blueArmorClass
  ok := (← assert "giveArmor negative still grants"
    (gNeg && pNeg.armorpoints == 200 && pNeg.armortype == 2)) && ok

  -- ARM2 pickup: live 85<200 grants; full 200 leaves world unchanged. --
  match pickup SPR_ARM2 arm2Flags 82 85 1 #[49, 9, 0, 0] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"ARM2 85 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "ARM2 85→200 type2"
      (p.armorpoints == 200 && p.armortype == 2)) && ok
    ok := (← assert "ARM2 health unchanged" (p.health == 82)) && ok
    ok := (← assert "ARM2 pending stays 10" (p.pendingweapon == wp_nochange)) && ok
    ok := (← assert "ARM2 ready stays shotgun" (p.readyweapon == wp_shotgun)) && ok
    ok := (← assert "ARM2 ammo3 unchanged" (ammoAt p 3 == 0)) && ok
    ok := (← assert "ARM2 bonuscount+6" (p.bonuscount == BONUSADD)) && ok
    ok := (← assert "ARM2 itemcount unchanged" (p.itemcount == 0)) && ok
    ok := (← assert "ARM2 special removed" (specialRemoved gs)) && ok
  match pickup SPR_ARM2 arm2Flags 82 200 2 #[49, 9, 0, 0] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"ARM2 full ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "ARM2 full world unchanged"
      (p.armorpoints == 200 && p.armortype == 2 && !specialRemoved gs
        && p.bonuscount == 0)) && ok

  -- BROK: 5 clips of missiles; shotgun does not set pending; full ammo no-op. --
  match pickup SPR_BROK brokFlags 82 85 1 #[49, 9, 0, 0] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"BROK 0 ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BROK ammo3 0→5" (ammoAt p 3 == 5)) && ok
    ok := (← assert "BROK pending stays 10 (shotgun not fist)"
      (p.pendingweapon == wp_nochange)) && ok
    ok := (← assert "BROK ready stays shotgun" (p.readyweapon == wp_shotgun)) && ok
    ok := (← assert "BROK armor unchanged" (p.armorpoints == 85)) && ok
    ok := (← assert "BROK bonuscount+6" (p.bonuscount == BONUSADD)) && ok
    ok := (← assert "BROK special removed" (specialRemoved gs)) && ok
  match pickup SPR_BROK brokFlags 82 85 1 #[49, 9, 0, 0] wp_shotgun ownedShotgunMissile 2 with
  | Except.error e =>
    ok := (← assert s!"BROK shotgun owned-missile ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "BROK shotgun owned-missile still no pending"
      (ammoAt p 3 == 5 && p.pendingweapon == wp_nochange)) && ok
  match pickup SPR_BROK brokFlags 82 85 1 #[49, 9, 0, 0] wp_fist ownedFistMissile 2 with
  | Except.error e =>
    ok := (← assert s!"BROK fist owned-missile ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "BROK fist owned-missile pending=missile"
      (ammoAt p 3 == 5 && p.pendingweapon == wp_missile)) && ok
  match pickup SPR_BROK brokFlags 82 85 1 #[49, 9, 0, 0] wp_fist ownedFistPistol 2 with
  | Except.error e =>
    ok := (← assert s!"BROK fist no-missile ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "BROK fist no-missile pending stays 10"
      (ammoAt p 3 == 5 && p.pendingweapon == wp_nochange)) && ok
  match pickup SPR_BROK brokFlags 82 85 1 #[49, 9, 0, 3] wp_fist ownedFistMissile 2 with
  | Except.error e =>
    ok := (← assert s!"BROK nonzero oldammo ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "BROK nonzero oldammo no preference"
      (ammoAt p 3 == 8 && p.pendingweapon == wp_nochange)) && ok
  match pickup SPR_BROK brokFlags 82 85 1 #[49, 9, 0, 0] wp_shotgun ownedShotgun 0 with
  | Except.error e =>
    ok := (← assert s!"BROK baby ({e})" false) && ok
  | Except.ok (p, _) =>
    ok := (← assert "BROK baby doubles 0→10" (ammoAt p 3 == 10)) && ok
  match pickup SPR_BROK brokFlags 82 85 1 #[49, 9, 0, 50] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"BROK full ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BROK full ammo unchanged" (ammoAt p 3 == 50)) && ok
    ok := (← assert "BROK full does not remove"
      (!specialRemoved gs && p.bonuscount == 0 && p.pendingweapon == wp_nochange)) && ok
  match pickup SPR_BROK brokFlags 82 85 1 #[49, 9, 0, 48] wp_shotgun ownedShotgun 2 with
  | Except.error e =>
    ok := (← assert s!"BROK cap ({e})" false) && ok
  | Except.ok (p, gs) =>
    ok := (← assert "BROK 48+5 cap 50"
      (ammoAt p 3 == 50 && specialRemoved gs)) && ok

  -- Same-tic ARM2 then BROK: live DEMO1 @1482 shape. --
  match touchSpecialThing (twoSpecialScene SPR_ARM2 SPR_BROK) 1 0 with
  | Except.error e =>
    ok := (← assert s!"ARM2-then-BROK ARM2 ({e})" false) && ok
  | Except.ok gsA =>
    match gsA.players[0]? with
    | none => ok := (← assert "ARM2-then-BROK player after ARM2" false) && ok
    | some pA =>
      ok := (← assert "ARM2-then-BROK after ARM2 armor 200 ammo3 0"
        (pA.armorpoints == 200 && pA.armortype == 2 && ammoAt pA 3 == 0
          && pA.pendingweapon == wp_nochange && pA.health == 82)) && ok
    match touchSpecialThing gsA 2 0 with
    | Except.error e =>
      ok := (← assert s!"ARM2-then-BROK BROK ({e})" false) && ok
    | Except.ok gsB =>
      match gsB.players[0]? with
      | none => ok := (← assert "ARM2-then-BROK player after BROK" false) && ok
      | some pB =>
        ok := (← assert "ARM2-then-BROK armor 200 ammo3 5 pending 10"
          (pB.armorpoints == 200 && pB.armortype == 2 && ammoAt pB 3 == 5
            && pB.pendingweapon == wp_nochange && pB.readyweapon == wp_shotgun
            && pB.health == 82 && pB.bonuscount == (12 : Int32))) && ok
        ok := (← assert "ARM2-then-BROK both removed"
          (thinkerRemoved gsB 0 && thinkerRemoved gsB 1)) && ok

  match touchSpecialThing (twoSpecialScene SPR_BROK SPR_ARM2) 1 0 with
  | Except.error e =>
    ok := (← assert s!"BROK-then-ARM2 BROK ({e})" false) && ok
  | Except.ok gsB =>
    match touchSpecialThing gsB 2 0 with
    | Except.error e =>
      ok := (← assert s!"BROK-then-ARM2 ARM2 ({e})" false) && ok
    | Except.ok gsA =>
      match gsA.players[0]? with
      | none => ok := (← assert "BROK-then-ARM2 player" false) && ok
      | some p =>
        ok := (← assert "BROK-then-ARM2 same grants"
          (p.armorpoints == 200 && p.armortype == 2 && ammoAt p 3 == 5
            && p.pendingweapon == wp_nochange && p.health == 82)) && ok
  pure ok

end Doom.Playsim.InterXxviiTest
