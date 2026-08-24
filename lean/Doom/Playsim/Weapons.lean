
/-!
# Doom.Playsim.Weapons

Minimal `weaponinfo[]` from oracle `d_items.c` (ammo + state indices).
-/

namespace Doom.Playsim.Weapons

/-- `ammotype_t`: `am_noammo` sits after `NUMAMMO`. -/
def am_clip : Int32 := 0
def am_shell : Int32 := 1
def am_cell : Int32 := 2
def am_misl : Int32 := 3
def am_noammo : Int32 := 5

/-- State indices matching generated `states[]` / `info.h`. -/
def S_NULL : UInt32 := 0
def S_PUNCH : UInt32 := 2
def S_PUNCHDOWN : UInt32 := 3
def S_PUNCHUP : UInt32 := 4
def S_PUNCH1 : UInt32 := 5
def S_PISTOL : UInt32 := 10
def S_PISTOLDOWN : UInt32 := 11
def S_PISTOLUP : UInt32 := 12
def S_PISTOL1 : UInt32 := 13
def S_PISTOL2 : UInt32 := 14
def S_PISTOLFLASH : UInt32 := 17
def S_PLAY : UInt32 := 149
def S_PLAY_ATK1 : UInt32 := 154
def S_PLAY_ATK2 : UInt32 := 155
def S_PLAY_PAIN : UInt32 := 156
def S_BLOOD1 : UInt32 := 90
def S_BLOOD2 : UInt32 := 91
def S_BLOOD3 : UInt32 := 92
/-- `mobjtype_t` `MT_BLOOD`. -/
def MT_BLOOD : Int32 := 38
def S_SGUN : UInt32 := 18
def S_SGUNDOWN : UInt32 := 19
def S_SGUNUP : UInt32 := 20
def S_SGUN1 : UInt32 := 21
def S_SGUN2 : UInt32 := 22
def S_SGUNFLASH1 : UInt32 := 30
def S_SGUNFLASH2 : UInt32 := 31
def S_DSGUN : UInt32 := 32
def S_DSGUNDOWN : UInt32 := 33
def S_DSGUNUP : UInt32 := 34
def S_DSGUN1 : UInt32 := 35
def S_DSGUNFLASH1 : UInt32 := 47
def S_CHAIN : UInt32 := 49
def S_CHAINDOWN : UInt32 := 50
def S_CHAINUP : UInt32 := 51
def S_CHAIN1 : UInt32 := 52
def S_CHAINFLASH1 : UInt32 := 55
def S_MISSILE : UInt32 := 57
def S_MISSILEDOWN : UInt32 := 58
def S_MISSILEUP : UInt32 := 59
def S_MISSILE1 : UInt32 := 60
def S_MISSILE2 : UInt32 := 61
def S_MISSILEFLASH1 : UInt32 := 63
def S_SAW : UInt32 := 67
def S_SAWDOWN : UInt32 := 69
def S_SAWUP : UInt32 := 70
def S_SAW1 : UInt32 := 71
def S_PLASMA : UInt32 := 74
def S_PLASMADOWN : UInt32 := 75
def S_PLASMAUP : UInt32 := 76
def S_PLASMA1 : UInt32 := 77
def S_PLASMAFLASH1 : UInt32 := 79
def S_BFG : UInt32 := 81
def S_BFGDOWN : UInt32 := 82
def S_BFGUP : UInt32 := 83
def S_BFG1 : UInt32 := 84
def S_BFGFLASH1 : UInt32 := 88

structure WeaponInfo where
  ammo : Int32
  upstate : UInt32
  downstate : UInt32
  readystate : UInt32
  atkstate : UInt32
  flashstate : UInt32
  deriving Repr

/-- `weaponinfo[NUMWEAPONS]` from `d_items.c`. -/
def weaponinfo : Array WeaponInfo := #[
  { ammo := am_noammo, upstate := S_PUNCHUP, downstate := S_PUNCHDOWN,
    readystate := S_PUNCH, atkstate := S_PUNCH1, flashstate := S_NULL },
  { ammo := am_clip, upstate := S_PISTOLUP, downstate := S_PISTOLDOWN,
    readystate := S_PISTOL, atkstate := S_PISTOL1, flashstate := S_PISTOLFLASH },
  { ammo := am_shell, upstate := S_SGUNUP, downstate := S_SGUNDOWN,
    readystate := S_SGUN, atkstate := S_SGUN1, flashstate := S_SGUNFLASH1 },
  { ammo := am_clip, upstate := S_CHAINUP, downstate := S_CHAINDOWN,
    readystate := S_CHAIN, atkstate := S_CHAIN1, flashstate := S_CHAINFLASH1 },
  { ammo := am_misl, upstate := S_MISSILEUP, downstate := S_MISSILEDOWN,
    readystate := S_MISSILE, atkstate := S_MISSILE1, flashstate := S_MISSILEFLASH1 },
  { ammo := am_cell, upstate := S_PLASMAUP, downstate := S_PLASMADOWN,
    readystate := S_PLASMA, atkstate := S_PLASMA1, flashstate := S_PLASMAFLASH1 },
  { ammo := am_cell, upstate := S_BFGUP, downstate := S_BFGDOWN,
    readystate := S_BFG, atkstate := S_BFG1, flashstate := S_BFGFLASH1 },
  { ammo := am_noammo, upstate := S_SAWUP, downstate := S_SAWDOWN,
    readystate := S_SAW, atkstate := S_SAW1, flashstate := S_NULL },
  { ammo := am_shell, upstate := S_DSGUNUP, downstate := S_DSGUNDOWN,
    readystate := S_DSGUN, atkstate := S_DSGUN1, flashstate := S_DSGUNFLASH1 }
]

end Doom.Playsim.Weapons
