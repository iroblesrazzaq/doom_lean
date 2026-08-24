/-!
# Doom.Playsim.Flags

`mobjflag_t` bits from oracle `p_mobj.h` (spawn-relevant subset + common flags).
-/

namespace Doom.Playsim.Flags

def MF_SPECIAL : UInt32 := 1
def MF_SOLID : UInt32 := 2
def MF_SHOOTABLE : UInt32 := 4
def MF_NOSECTOR : UInt32 := 8
def MF_NOBLOCKMAP : UInt32 := 16
def MF_AMBUSH : UInt32 := 32
def MF_JUSTHIT : UInt32 := 64
def MF_JUSTATTACKED : UInt32 := 128
def MF_SPAWNCEILING : UInt32 := 256
def MF_NOGRAVITY : UInt32 := 512
def MF_DROPOFF : UInt32 := 0x400
def MF_PICKUP : UInt32 := 0x800
def MF_NOCLIP : UInt32 := 0x1000
def MF_FLOAT : UInt32 := 0x4000
def MF_TELEPORT : UInt32 := 0x8000
def MF_MISSILE : UInt32 := 0x10000
/-- `MF_DROPPED` (`p_mobj.h`). -/
def MF_DROPPED : UInt32 := 0x20000
def MF_CORPSE : UInt32 := 0x100000
/-- `MF_INFLOAT` (`p_mobj.h`). -/
def MF_INFLOAT : UInt32 := 0x200000
def MF_SHADOW : UInt32 := 0x40000
def MF_NOBLOOD : UInt32 := 0x80000
def MF_COUNTKILL : UInt32 := 0x400000
def MF_COUNTITEM : UInt32 := 0x800000
def MF_SKULLFLY : UInt32 := 0x1000000
def MF_NOTDMATCH : UInt32 := 0x2000000
def MF_TRANSLATION : UInt32 := 0xc000000
/-- Shift amount for player color translation (`MF_TRANSSHIFT`). -/
def MF_TRANSSHIFT : UInt32 := 26

end Doom.Playsim.Flags
