/-!
# Doom.Playsim.Fixed

Fixed-point arithmetic ported from oracle `src/m_fixed.c` / `m_fixed.h`.
All operations use wrapping `Int32`/`Int64` machine arithmetic only.
-/

namespace Doom.Playsim.Fixed

/-- 16.16 fixed point: number of fractional bits. -/
def FRACBITS : Nat := 16

/-- 1.0 in 16.16 fixed point. -/
def FRACUNIT : Int32 := 65536

/--
`FixedMul`: `((int64_t) a * (int64_t) b) >> FRACBITS`, with arithmetic
right shift on the 64-bit product, then truncation back to 32 bits.
-/
def fixedMul (a b : Int32) : Int32 :=
  ((a.toInt64 * b.toInt64) >>> 16).toInt32

/--
Wrapping absolute value matching C `abs()` on two's complement:
`wabs Int32.minValue = Int32.minValue` (wrapping negation, not mathematical
absolute value).
-/
def wabs (a : Int32) : Int32 :=
  if a < 0 then -a else a

/--
`FixedDiv`: if `(abs a >> 14) >= abs b` (signed comparison, arithmetic
shift, C-`abs` wrapping on `minValue`), clamp to `Int32.minValue` when the
operand signs differ (`(a ^^^ b) < 0`) else `Int32.maxValue`. Otherwise
`((int64_t) a << FRACBITS) / b` with C truncated-toward-zero division,
truncated back to 32 bits.
-/
def fixedDiv (a b : Int32) : Int32 :=
  if (wabs a) >>> 14 >= wabs b then
    if (a ^^^ b) < 0 then Int32.minValue else Int32.maxValue
  else
    ((a.toInt64 <<< 16) / b.toInt64).toInt32

end Doom.Playsim.Fixed
