import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Sound
import Doom.Playsim.Thinker
import Doom.Playsim.Weapons

/-!
# Doom.Playsim.Inter

`p_inter.c` open subset: `P_GiveAmmo` / `P_GiveWeapon` / `P_GiveArmor` /
`P_TouchSpecialThing` (`SPR_ARM1` + `SPR_ARM2` + `SPR_SHOT` + `SPR_LAUN` +
`SPR_BON1` + `SPR_BON2` + `SPR_SHEL` + `SPR_SBOX` + `SPR_CLIP` + `SPR_BROK` +
`SPR_STIM` +
`SPR_MEDI` + `SPR_BKEY` + `SPR_YKEY`) and `P_RemoveMobj` (`p_mobj.c`).
-/

namespace Doom.Playsim.Inter

open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.MapUtil
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Sound
open Doom.Playsim.Thinker
open Doom.Playsim.Weapons

/-- `spritenum_t` `SPR_ARM1`. -/
def SPR_ARM1 : UInt32 := 55

/-- `spritenum_t` `SPR_ARM2`. -/
def SPR_ARM2 : UInt32 := 56

/-- `spritenum_t` `SPR_LAUN`. -/
def SPR_LAUN : UInt32 := 90

/-- `spritenum_t` `SPR_SHOT`. -/
def SPR_SHOT : UInt32 := 92

/-- `spritenum_t` `SPR_BON1` (health bonus). -/
def SPR_BON1 : UInt32 := 60

/-- `spritenum_t` `SPR_BON2` (armor helmet). -/
def SPR_BON2 : UInt32 := 61

/-- `spritenum_t` `SPR_BKEY`. -/
def SPR_BKEY : UInt32 := 62

/-- `spritenum_t` `SPR_YKEY`. -/
def SPR_YKEY : UInt32 := 64

/-- `spritenum_t` `SPR_STIM`. -/
def SPR_STIM : UInt32 := 68

/-- `spritenum_t` `SPR_MEDI`. -/
def SPR_MEDI : UInt32 := 69

/-- `spritenum_t` `SPR_CLIP`. -/
def SPR_CLIP : UInt32 := 78

/-- `spritenum_t` `SPR_BROK`. -/
def SPR_BROK : UInt32 := 81

/-- `spritenum_t` `SPR_SHEL`. -/
def SPR_SHEL : UInt32 := 84

/-- `spritenum_t` `SPR_SBOX`. -/
def SPR_SBOX : UInt32 := 85

/-- `clipammo[NUMAMMO]` (`p_inter.c`). -/
def clipammo : Array Int32 := #[10, 4, 20, 1]

/-- `BONUSADD` (`p_inter.c`). -/
def BONUSADD : Int32 := 6

/-- Green armor class (`deh_green_armor_class` default). -/
def greenArmorClass : Int32 := 1

/-- Blue armor class (`deh_blue_armor_class` default). -/
def blueArmorClass : Int32 := 2

/-- `deh_max_health` default (`DEH_DEFAULT_MAX_HEALTH`). BON1 cap. -/
def maxHealth : Int32 := 200

/-- `deh_max_armor` default (`DEH_DEFAULT_MAX_ARMOR`). BON2 cap (always, 1.9). -/
def maxArmor : Int32 := 200

/-- `MAXHEALTH` (`p_local.h`). `P_GiveBody` cap; not BON1's 200. -/
def bodyMaxHealth : Int32 := 100

/-- `d_englsh.h` pickup strings (`p_inter.c` `DEH_String(GOT*)`). -/
def GOTARMOR : String := "Picked up the armor."
def GOTMEGA : String := "Picked up the MegaArmor!"
def GOTHTHBONUS : String := "Picked up a health bonus."
def GOTARMBONUS : String := "Picked up an armor bonus."
def GOTSTIM : String := "Picked up a stimpack."
def GOTMEDINEED : String := "Picked up a medikit that you REALLY need!"
def GOTMEDIKIT : String := "Picked up a medikit."
def GOTBLUECARD : String := "Picked up a blue keycard."
def GOTYELWCARD : String := "Picked up a yellow keycard."
def GOTCLIP : String := "Picked up a clip."
def GOTROCKBOX : String := "Picked up a box of rockets."
def GOTSHELLS : String := "Picked up 4 shotgun shells."
def GOTSHELLBOX : String := "Picked up a box of shotgun shells."
def GOTLAUNCHER : String := "You got the rocket launcher!"
def GOTSHOTGUN : String := "You got the shotgun!"

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

/-- Mark the `THF_MOBJ` thinker for `mobjIdx` as `THF_REMOVED` (lazy free). -/
def markThinkerRemoved (gs : GameState) (mobjIdx : Nat) : Except String GameState := do
  let mut found := false
  let mut thinkers := gs.thinkers
  let mut i : Nat := 0
  while i < thinkers.size do
    match thinkers[i]? with
    | none => throw "P_RemoveMobj: bad thinker"
    | some th =>
      if th.func == THF_MOBJ && th.payload.toNat == mobjIdx then
        thinkers := setArr thinkers i { th with func := THF_REMOVED }
        found := true
      pure ()
    i := i + 1
  if !found then
    throw s!"P_RemoveMobj: no thinker for mobj {mobjIdx}"
  pure { gs with thinkers }

/--
`P_RemoveMobj` — unlink + mark thinker removed. Item-respawn queue is not
modeled (untraced; deathmatch respawn unused for DEMO1).
-/
def removeMobj (gs0 : GameState) (mobjIdx : Nat) : Except String GameState := do
  let gs ← unsetThingPosition gs0 mobjIdx
  markThinkerRemoved gs mobjIdx

/-- `P_GiveArmor` — returns `(player, gave?)`. -/
def giveArmor (player : Player) (armortype : Int32) : Player × Bool :=
  let hits := armortype * 100
  if player.armorpoints >= hits then
    (player, false)
  else
    ({ player with armortype := armortype, armorpoints := hits }, true)

/-- `SPR_BON2` — C `armorpoints++` (wrapping Int32). Cap only if `> maxArmor`.
Type 1 if none; type 1 and 2 stay. Always apply the 1.9 cap (no `gameversion`). -/
def giveArmorBonus (player : Player) : Player :=
  let ap := player.armorpoints + 1
  { player with
    armorpoints := if ap > maxArmor then maxArmor else ap
    armortype := if player.armortype == 0 then (1 : Int32) else player.armortype
  }

/-- `SPR_BON1` — C `health++` (wrapping Int32). Cap only if `> maxHealth`. -/
def giveHealthBonus (player : Player) : Player :=
  let hp := player.health + 1
  { player with health := if hp > maxHealth then maxHealth else hp }

/-- `P_GiveBody` — wrapping `+= num`, cap only if `> bodyMaxHealth`. -/
def giveBody (player : Player) (num : Int32) : Player × Bool :=
  if player.health >= bodyMaxHealth then
    (player, false)
  else
    let hp := player.health + num
    ({ player with health := if hp > bodyMaxHealth then bodyMaxHealth else hp }, true)

/-- `P_GiveCard` — first grant assigns `bonuscount = BONUSADD` (not `+=`). -/
def giveCard (player : Player) (card : Nat) : Player :=
  match player.cards[card]? with
  | some true => player
  | some false =>
    { player with
      bonuscount := BONUSADD
      cards := setArr player.cards card true
    }
  | none => player

private def ammoAt (player : Player) (ammo : Int32) : Int32 :=
  match player.ammo[ammo.toNatClampNeg]? with | some v => v | none => 0

private def maxAmmoAt (player : Player) (ammo : Int32) : Int32 :=
  match player.maxammo[ammo.toNatClampNeg]? with | some v => v | none => 0

private def ownedAt (player : Player) (w : Int32) : Int32 :=
  match player.weaponowned[w.toNatClampNeg]? with | some v => v | none => 0

/-- `P_GiveAmmo` zero-ammo weapon preference (`p_inter.c`). -/
private def ammoPreference (player : Player) (ammo : Int32) : Player :=
  if ammo == am_clip then
    if player.readyweapon == wp_fist then
      { player with
        pendingweapon :=
          if (ownedAt player wp_chaingun) != 0 then wp_chaingun else wp_pistol }
    else player
  else if ammo == am_shell then
    if player.readyweapon == wp_fist || player.readyweapon == wp_pistol then
      if (ownedAt player wp_shotgun) != 0 then
        { player with pendingweapon := wp_shotgun }
      else player
    else player
  else if ammo == am_cell then
    if player.readyweapon == wp_fist || player.readyweapon == wp_pistol then
      if (ownedAt player wp_plasma) != 0 then
        { player with pendingweapon := wp_plasma }
      else player
    else player
  else if ammo == am_misl then
    if player.readyweapon == wp_fist then
      if (ownedAt player wp_missile) != 0 then
        { player with pendingweapon := wp_missile }
      else player
    else player
  else player

/--
`P_GiveAmmo` — `num` is clip loads (`0` = half clip). Baby/nightmare double.
Returns `(player, gave?)`.
-/
def giveAmmo (player : Player) (ammo num gameskill : Int32) :
    Except String (Player × Bool) := do
  if ammo == am_noammo then
    return (player, false)
  if ammo >= NUMAMMO.toInt32 then
    throw s!"P_GiveAmmo: bad type {ammo}"
  if ammoAt player ammo == maxAmmoAt player ammo then
    return (player, false)
  let clip ←
    match clipammo[ammo.toNatClampNeg]? with
    | some c => pure c
    | none => throw s!"P_GiveAmmo: bad type {ammo}"
  let mut qty := if num != 0 then num * clip else clip / 2
  if gameskill == sk_baby || gameskill == sk_nightmare then
    qty := qty <<< 1
  let oldammo := ammoAt player ammo
  let mut now := oldammo + qty
  let cap := maxAmmoAt player ammo
  if now > cap then
    now := cap
  let p := { player with ammo := setArr player.ammo ammo.toNatClampNeg now }
  if oldammo != 0 then
    return (p, true)
  pure (ammoPreference p ammo, true)

/--
`P_GiveWeapon` — netgame path is out of this open subset. Dropped weapons
give one clip; found weapons give two.
-/
def giveWeapon (player : Player) (weapon : Int32) (dropped : Bool)
    (gameskill : Int32) (netgame : Bool) : Except String (Player × Bool) := do
  if netgame then
    throw "P_GiveWeapon: netgame not implemented"
  let wi ←
    match weaponinfo[weapon.toNatClampNeg]? with
    | some w => pure w
    | none => throw s!"P_GiveWeapon: bad weapon {weapon}"
  let mut p := player
  let mut gaveammo := false
  if wi.ammo != am_noammo then
    let clips : Int32 := if dropped then 1 else 2
    let (p1, ga) ← giveAmmo p wi.ammo clips gameskill
    p := p1
    gaveammo := ga
  if ownedAt p weapon != 0 then
    return (p, gaveammo)
  let pOwned := {
    p with
    weaponowned := setArr p.weaponowned weapon.toNatClampNeg 1
    pendingweapon := weapon
  }
  pure (pOwned, true)

/--
`P_TouchSpecialThing` — DEMO1 open subset: reachability + `SPR_ARM1` green
armor, `SPR_ARM2` blue armor, `SPR_SHOT` shotgun, `SPR_LAUN` rocket
launcher (`P_GiveWeapon wp_missile` with hardcoded `dropped=false`),
`SPR_BON1` health bonus, `SPR_BON2` armor bonus, `SPR_SHEL` shells,
`SPR_SBOX` shell box, `SPR_CLIP` bullets, `SPR_BROK` rocket box,
`SPR_STIM` stimpack
(`P_GiveBody` 10), `SPR_MEDI` medikit, `SPR_BKEY` blue card, and `SPR_YKEY`
yellow card.
All other sprites loud-error.
-/
def touchSpecialThing (gs0 : GameState) (specialIdx toucherIdx : Nat) :
    Except String GameState := do
  match gs0.mobjs[specialIdx]?, gs0.mobjs[toucherIdx]? with
  | none, _ => throw "P_TouchSpecialThing: bad special"
  | _, none => throw "P_TouchSpecialThing: bad toucher"
  | some special, some toucher =>
    let delta := special.z - toucher.z
    if delta > toucher.height || delta < (-8) * FRACUNIT then
      return gs0
    if toucher.health <= 0 then
      return gs0
    if toucher.player < 0 then
      throw "P_TouchSpecialThing: toucher has no player"
    let pi := toucher.player.toNatClampNeg
    match gs0.players[pi]? with
    | none => throw "P_TouchSpecialThing: bad player"
    | some player0 =>
      let dropped := (special.flags &&& MF_DROPPED) != 0
      let mut player := player0
      let mut sound := sfx_itemup
      if special.sprite == SPR_ARM1 then
        let (p1, gave) := giveArmor player greenArmorClass
        if !gave then
          return gs0
        player := { p1 with message := some GOTARMOR }
      else if special.sprite == SPR_ARM2 then
        let (p1, gave) := giveArmor player blueArmorClass
        if !gave then
          return gs0
        player := { p1 with message := some GOTMEGA }
      else if special.sprite == SPR_SHOT then
        let (p1, gave) ← giveWeapon player wp_shotgun dropped gs0.gameskill gs0.netgame
        if !gave then
          return gs0
        player := { p1 with message := some GOTSHOTGUN }
        sound := sfx_wpnup
      else if special.sprite == SPR_LAUN then
        let (p1, gave) ← giveWeapon player wp_missile false gs0.gameskill gs0.netgame
        if !gave then
          return gs0
        player := { p1 with message := some GOTLAUNCHER }
        sound := sfx_wpnup
      else if special.sprite == SPR_BON1 then
        player := { giveHealthBonus player with message := some GOTHTHBONUS }
      else if special.sprite == SPR_BON2 then
        player := { giveArmorBonus player with message := some GOTARMBONUS }
      else if special.sprite == SPR_SHEL then
        let (p1, gave) ← giveAmmo player am_shell 1 gs0.gameskill
        if !gave then
          return gs0
        player := { p1 with message := some GOTSHELLS }
      else if special.sprite == SPR_SBOX then
        let (p1, gave) ← giveAmmo player am_shell 5 gs0.gameskill
        if !gave then
          return gs0
        player := { p1 with message := some GOTSHELLBOX }
      else if special.sprite == SPR_CLIP then
        let num : Int32 := if dropped then 0 else 1
        let (p1, gave) ← giveAmmo player am_clip num gs0.gameskill
        if !gave then
          return gs0
        player := { p1 with message := some GOTCLIP }
      else if special.sprite == SPR_BROK then
        let (p1, gave) ← giveAmmo player am_misl 5 gs0.gameskill
        if !gave then
          return gs0
        player := { p1 with message := some GOTROCKBOX }
      else if special.sprite == SPR_STIM then
        let (p1, gave) := giveBody player 10
        if !gave then
          return gs0
        player := { p1 with message := some GOTSTIM }
      else if special.sprite == SPR_MEDI then
        let (p1, gave) := giveBody player 25
        if !gave then
          return gs0
        -- C checks health after `P_GiveBody`.
        player := {
          p1 with
          message := some (if p1.health < 25 then GOTMEDINEED else GOTMEDIKIT)
        }
      else if special.sprite == SPR_BKEY then
        let had := match player.cards[it_bluecard]? with | some true => true | _ => false
        player := giveCard player it_bluecard
        if !had then
          player := { player with message := some GOTBLUECARD }
        if gs0.netgame then
          return { gs0 with players := setArr gs0.players pi player }
      else if special.sprite == SPR_YKEY then
        let had := match player.cards[it_yellowcard]? with | some true => true | _ => false
        player := giveCard player it_yellowcard
        if !had then
          player := { player with message := some GOTYELWCARD }
        if gs0.netgame then
          return { gs0 with players := setArr gs0.players pi player }
      else
        throw s!"P_TouchSpecialThing: unsupported sprite {special.sprite}"
      let mut gs := { gs0 with players := setArr gs0.players pi player }
      if special.sprite == SPR_BON1 || special.sprite == SPR_STIM
          || special.sprite == SPR_MEDI then
        gs := {
          gs with
          mobjs := setArr gs.mobjs toucherIdx { toucher with health := player.health }
        }
      if (special.flags &&& MF_COUNTITEM) != 0 then
        match gs.players[pi]? with
        | none => throw "P_TouchSpecialThing: player lost"
        | some p =>
          gs := {
            gs with
            players := setArr gs.players pi { p with itemcount := p.itemcount + 1 }
          }
      gs ← removeMobj gs specialIdx
      match gs.players[pi]? with
      | none => throw "P_TouchSpecialThing: player lost after remove"
      | some p =>
        let p := { p with bonuscount := p.bonuscount + BONUSADD }
        gs := { gs with players := setArr gs.players pi p }
        if pi == gs.consoleplayer then
          gs := { gs with rng := startSoundPitchRng gs.rng sound }
        pure gs

end Doom.Playsim.Inter
