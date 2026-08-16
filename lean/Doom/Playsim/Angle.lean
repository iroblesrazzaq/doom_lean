import Doom.Playsim.Tables

/-!
# Doom.Playsim.Angle

Binary Angle Measurement (BAM) constants and `SlopeDiv`, ported from oracle
`src/tables.h` / `src/tables.c` (vanilla `SlopeDiv`, not any Crispy variant).
Angles are `UInt32` with natural wrapping. Also `R_PointToAngle` /
`R_PointToAngle2` (oracle `r_main.c`).
-/

namespace Doom.Playsim.Angle

open Doom.Playsim.Tables

/-- BAM angle type: unsigned 32-bit, wraps naturally. -/
abbrev Angle := UInt32

def ANG45 : UInt32 := 0x20000000
def ANG90 : UInt32 := 0x40000000
/-- `ANG5` (`ANG90/18`) from `p_user.c`. -/
def ANG5 : UInt32 := ANG90 / 18
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

/--
`R_PointToAngle` relative to origin `(0,0)` after the caller subtracts the
viewpoint (same as C once `viewx`/`viewy` are set). Arguments are signed
fixed_t; only non-negative magnitudes are passed to `slopeDiv`.
-/
def pointToAngle (x0 y0 : Int32) : UInt32 :=
  if x0 == 0 && y0 == 0 then
    0
  else if x0 >= 0 then
    if y0 >= 0 then
      if x0 > y0 then
        tantoangle.getD (slopeDiv y0.toUInt32 x0.toUInt32).toNat 0
      else
        ANG90 - 1 - tantoangle.getD (slopeDiv x0.toUInt32 y0.toUInt32).toNat 0
    else
      let y := -y0
      if x0 > y then
        (0 : UInt32) - tantoangle.getD (slopeDiv y.toUInt32 x0.toUInt32).toNat 0
      else
        ANG270 + tantoangle.getD (slopeDiv x0.toUInt32 y.toUInt32).toNat 0
  else
    let x := -x0
    if y0 >= 0 then
      if x > y0 then
        ANG180 - 1 - tantoangle.getD (slopeDiv y0.toUInt32 x.toUInt32).toNat 0
      else
        ANG90 + tantoangle.getD (slopeDiv x.toUInt32 y0.toUInt32).toNat 0
    else
      let y := -y0
      if x > y then
        ANG180 + tantoangle.getD (slopeDiv y.toUInt32 x.toUInt32).toNat 0
      else
        ANG270 - 1 - tantoangle.getD (slopeDiv x.toUInt32 y.toUInt32).toNat 0

/-- `R_PointToAngle2`. -/
def pointToAngle2 (x1 y1 x2 y2 : Int32) : UInt32 :=
  pointToAngle (x2 - x1) (y2 - y1)

end Doom.Playsim.Angle
