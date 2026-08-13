/-!
# Doom.Playsim.Player

Player state for spawn + first-tic think (`G_PlayerReborn` / `P_SpawnPlayer` /
`P_PlayerThink`).
-/

namespace Doom.Playsim.Player

def MAXPLAYERS : Nat := 4
def NUMWEAPONS : Nat := 9
def NUMAMMO : Nat := 4
def NUMPSPRITES : Nat := 2
def NUMPOWERS : Nat := 6

/-- `weapontype_t`: `wp_fist`..`wp_supershotgun`, then `NUMWEAPONS`, then `wp_nochange`. -/
def wp_fist : Int32 := 0
def wp_pistol : Int32 := 1
def wp_shotgun : Int32 := 2
def wp_chaingun : Int32 := 3
def wp_missile : Int32 := 4
def wp_plasma : Int32 := 5
def wp_bfg : Int32 := 6
def wp_chainsaw : Int32 := 7
def wp_supershotgun : Int32 := 8
def wp_nochange : Int32 := 10

/-- `d_event.h` `BT_ATTACK`. -/
def BT_ATTACK : UInt32 := 1

def PST_LIVE : Int32 := 0
def PST_DEAD : Int32 := 1
def PST_REBORN : Int32 := 2

def VIEWHEIGHT : Int32 := 41 * 65536  -- 41*FRACUNIT

def ps_weapon : Nat := 0
def ps_flash : Nat := 1

/-- `ticcmd_t` fields traced in §5.1 (demo / net cmd). -/
structure TicCmd where
  forwardmove : Int32
  sidemove : Int32
  angleturn : Int32
  buttons : UInt32
  deriving Repr

def TicCmd.zero : TicCmd := {
  forwardmove := 0
  sidemove := 0
  angleturn := 0
  buttons := 0
}

/-- `pspdef_t`: `state = 0` means NULL. -/
structure Psprite where
  state : UInt32
  tics : Int32
  sx : Int32
  sy : Int32
  deriving Repr

def Psprite.inactive : Psprite := {
  state := 0
  tics := 0
  sx := 0
  sy := 0
}

structure Player where
  /-- Mobj index, or `-1` if none. -/
  mo : Int32
  playerstate : Int32
  health : Int32
  armorpoints : Int32
  /-- Armor class (`armortype`); untraced. Green armor = 1. -/
  armortype : Int32
  readyweapon : Int32
  pendingweapon : Int32
  weaponowned : Array Int32
  ammo : Array Int32
  maxammo : Array Int32
  viewz : Int32
  viewheight : Int32
  deltaviewheight : Int32
  bob : Int32
  refire : Int32
  killcount : Int32
  itemcount : Int32
  secretcount : Int32
  cmd : TicCmd
  psprites : Array Psprite
  /-- Zero-safe stubs (`pw_*`); decremented in `P_PlayerThink` when non-zero. -/
  powers : Array Int32
  damagecount : Int32
  bonuscount : Int32
  usedown : Bool
  attackdown : Bool
  cheats : Int32
  /-- `player_t.extralight` (untraced; `A_Light1` writes it). -/
  extralight : Int32
  deriving Repr

def defaultMaxAmmo : Array Int32 := #[200, 50, 300, 50]

/-- Empty player (pre-reborn). `viewz` is 0 after `G_PlayerReborn` memset. -/
def empty : Player := {
  mo := -1
  playerstate := PST_REBORN
  health := 0
  armorpoints := 0
  armortype := 0
  readyweapon := wp_fist
  pendingweapon := wp_nochange
  weaponowned := Array.replicate NUMWEAPONS 0
  ammo := Array.replicate NUMAMMO 0
  maxammo := defaultMaxAmmo
  viewz := 0
  viewheight := 0
  deltaviewheight := 0
  bob := 0
  refire := 0
  killcount := 0
  itemcount := 0
  secretcount := 0
  cmd := TicCmd.zero
  psprites := Array.replicate NUMPSPRITES Psprite.inactive
  powers := Array.replicate NUMPOWERS 0
  damagecount := 0
  bonuscount := 0
  usedown := false
  attackdown := false
  cheats := 0
  extralight := 0
}

private def arrSet (arr : Array Int32) (i : Nat) (v : Int32) : Array Int32 :=
  if h : i < arr.size then arr.set i v else arr

/-- `G_PlayerReborn` defaults (vanilla dehacked health/bullets = 100/50). -/
def playerReborn (p : Player) : Player :=
  let owned := arrSet (arrSet (Array.replicate NUMWEAPONS (0 : Int32)) 0 1) 1 1
  let ammo := arrSet (Array.replicate NUMAMMO (0 : Int32)) 0 50
  { empty with
    playerstate := PST_LIVE
    health := 100
    readyweapon := wp_pistol
    pendingweapon := wp_pistol
    weaponowned := owned
    ammo := ammo
    maxammo := defaultMaxAmmo
    killcount := p.killcount
    itemcount := p.itemcount
    secretcount := p.secretcount
  }

end Doom.Playsim.Player
