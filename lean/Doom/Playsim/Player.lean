/-!
# Doom.Playsim.Player

Player state subset needed for spawn (`G_PlayerReborn` / `P_SpawnPlayer`).
-/

namespace Doom.Playsim.Player

def MAXPLAYERS : Nat := 4
def NUMWEAPONS : Nat := 9
def NUMAMMO : Nat := 4

/-- `weapontype_t`: `wp_fist`..`wp_supershotgun`, then `NUMWEAPONS`, then `wp_nochange`. -/
def wp_fist : Int32 := 0
def wp_pistol : Int32 := 1
def wp_nochange : Int32 := 10

def PST_LIVE : Int32 := 0
def PST_DEAD : Int32 := 1
def PST_REBORN : Int32 := 2

def VIEWHEIGHT : Int32 := 41 * 65536  -- 41*FRACUNIT

structure Player where
  /-- Mobj index, or `-1` if none. -/
  mo : Int32
  playerstate : Int32
  health : Int32
  armorpoints : Int32
  readyweapon : Int32
  pendingweapon : Int32
  weaponowned : Array Int32
  ammo : Array Int32
  maxammo : Array Int32
  viewz : Int32
  viewheight : Int32
  refire : Int32
  killcount : Int32
  itemcount : Int32
  secretcount : Int32
  deriving Repr

def defaultMaxAmmo : Array Int32 := #[200, 50, 300, 50]

/-- Empty player (pre-reborn). -/
def empty : Player := {
  mo := -1
  playerstate := PST_REBORN
  health := 0
  armorpoints := 0
  readyweapon := wp_fist
  pendingweapon := wp_nochange
  weaponowned := Array.replicate NUMWEAPONS 0
  ammo := Array.replicate NUMAMMO 0
  maxammo := defaultMaxAmmo
  viewz := 1
  viewheight := 0
  refire := 0
  killcount := 0
  itemcount := 0
  secretcount := 0
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
