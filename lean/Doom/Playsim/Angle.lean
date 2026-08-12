/-!
# Doom.Playsim.Angle

Binary Angle Measurement (BAM) constants and `SlopeDiv`, ported from oracle
`src/tables.h` / `src/tables.c` (vanilla `SlopeDiv`, not any Crispy variant).
Angles are `UInt32` with natural wrapping.
-/

namespace Doom.Playsim.Angle

/-- BAM angle type: unsigned 32-bit, wraps naturally. -/
abbrev Angle := UInt32

def ANG45 : UInt32 := 0x20000000
def ANG90 : UInt32 := 0x40000000
def ANG180 : UInt32 := 0x80000000
def ANG270 : UInt32 := 0xc0000000
def ANG_MAX : UInt32 := 0xffffffff
def ANG1 : UInt32 := ANG45 / 45

/-- Shift from a BAM angle (2^32 range) down to a fine-table index (2^13). -/
def ANGLETOFINESHIFT : Nat := 19

def FINEANGLES : Nat := 8192
def FINEMASK : UInt32 := 8191

def SLOPERANGE : UInt32 := 2048
def SLOPEBITS : Nat := 11
/-- FRACBITS - SLOPEBITS. -/
def DBITS : Nat := 5

/--
Vanilla `SlopeDiv` from oracle `tables.c`:
`den < 512` returns `SLOPERANGE`; otherwise `(num << 3) / (den >> 8)`,
capped at `SLOPERANGE`. All arithmetic is unsigned 32-bit.
-/
def slopeDiv (num den : UInt32) : UInt32 :=
  if den < 512 then
    SLOPERANGE
  else
    let ans := (num <<< 3) / (den >>> 8)
    if ans <= SLOPERANGE then ans else SLOPERANGE

end Doom.Playsim.Angle
