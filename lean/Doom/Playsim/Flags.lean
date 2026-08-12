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
def MF_SPAWNCEILING : UInt32 := 256
def MF_COUNTKILL : UInt32 := 0x400000
def MF_COUNTITEM : UInt32 := 0x800000
def MF_NOTDMATCH : UInt32 := 0x2000000
def MF_TRANSLATION : UInt32 := 0xc000000
/-- Shift amount for player color translation (`MF_TRANSSHIFT`). -/
def MF_TRANSSHIFT : UInt32 := 26

end Doom.Playsim.Flags
