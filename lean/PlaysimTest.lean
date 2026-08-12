import Doom.Playsim
import Doom.Harness.Fnv
import Doom.Harness.TraceFormat

/-!
Behavior tests for the P1a playsim primitives: fixed-point math, angle
constants and SlopeDiv, lookup tables, and the demo-sync RNG. All expected
values are derived by hand or mechanically from the oracle C sources
(`m_fixed.c`, `tables.c`/`tables.h`, `doom/m_random.c`).
-/

open Doom.Playsim.Fixed
open Doom.Playsim.Angle
open Doom.Playsim.Tables
open Doom.Playsim.Random
open Doom.Harness.Fnv

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

/-- Little-endian bytes of a table of 32-bit entries (two's complement). -/
def leBytes32 (xs : Array UInt32) : ByteArray :=
  xs.foldl (init := ByteArray.emptyWithCapacity (xs.size * 4))
    Doom.Harness.TraceFormat.pushU32LE

def i32 (xs : Array Int32) : Array UInt32 := xs.map (·.toUInt32)

def main (_args : List String) : IO UInt32 := do
  let mut ok := true

  -- fixedMul -------------------------------------------------------------
  -- 1.0 * 2.0 = 2.0
  ok := (← assert "fixedMul pos" (fixedMul 65536 131072 == 131072)) && ok
  -- (-1.0) * 1.5 = -1.5
  ok := (← assert "fixedMul mixed-sign" (fixedMul (-65536) 98304 == -98304)) && ok
  -- (-1.0) * (-1.0) = 1.0
  ok := (← assert "fixedMul both-neg" (fixedMul (-65536) (-65536) == 65536)) && ok
  -- (-1 * 1) >> 16 arithmetic shift rounds toward -infinity: result -1, not 0
  ok := (← assert "fixedMul >>16 rounds toward -inf" (fixedMul (-1) 1 == -1)) && ok
  -- 0x40000000^2 = 2^60; >>16 = 2^44; truncated to 32 bits = 0
  ok := (← assert "fixedMul large-magnitude wrap" (fixedMul 0x40000000 0x40000000 == 0)) && ok

  -- fixedDiv -------------------------------------------------------------
  -- 1.0 / 2.0 = 0.5
  ok := (← assert "fixedDiv normal pos" (fixedDiv 65536 131072 == 32768)) && ok
  -- (-65537 << 16) / 131072 = -32768.5 -> T-division truncates toward zero
  ok := (← assert "fixedDiv neg truncation (T-division)"
    (fixedDiv (-65537) 131072 == -32768)) && ok
  -- boundary: |a|>>14 == |b| exactly (65536 == 65536) triggers clamp (>=)
  ok := (← assert "fixedDiv clamp boundary pos"
    (fixedDiv 0x40000000 65536 == 0x7fffffff)) && ok
  ok := (← assert "fixedDiv clamp boundary neg"
    (fixedDiv (-0x40000000) 65536 == Int32.minValue)) && ok
  -- strict clamp: |maxValue|>>14 = 131071 >= 1
  ok := (← assert "fixedDiv clamp strict" (fixedDiv 0x7fffffff 1 == 0x7fffffff)) && ok
  -- a = INT_MIN: C abs wraps to INT_MIN, >>14 arithmetic is negative, so no
  -- clamp; ((INT_MIN)<<16)/1 = -2^47 truncates to 0 in 32 bits
  ok := (← assert "fixedDiv INT_MIN numerator abs edge"
    (fixedDiv Int32.minValue 1 == 0)) && ok
  -- b = INT_MIN: abs(b) wraps negative, comparison 4 >= INT_MIN true -> clamp,
  -- signs differ -> minValue
  ok := (← assert "fixedDiv INT_MIN denominator abs edge"
    (fixedDiv 65536 Int32.minValue == Int32.minValue)) && ok
  -- sanity: Lean Int64 `/` is T-division (truncates toward zero) on negatives
  ok := (← assert "Int64 division is T-division" ((-7 : Int64) / 2 == -3)) && ok

  -- angle constants --------------------------------------------------------
  ok := (← assert "ANG45" (ANG45 == 0x20000000)) && ok
  ok := (← assert "ANG90" (ANG90 == 0x40000000)) && ok
  ok := (← assert "ANG180" (ANG180 == 0x80000000)) && ok
  ok := (← assert "ANG270" (ANG270 == 0xc0000000)) && ok
  ok := (← assert "ANG_MAX" (ANG_MAX == 0xffffffff)) && ok
  -- 0x20000000 / 45 with C unsigned truncating division = 11930464
  ok := (← assert "ANG1" (ANG1 == 11930464)) && ok
  ok := (← assert "FINEMASK" (FINEMASK == 8191)) && ok
  ok := (← assert "SLOPERANGE" (SLOPERANGE == 2048)) && ok

  -- slopeDiv ---------------------------------------------------------------
  ok := (← assert "slopeDiv den<512" (slopeDiv 100 511 == 2048)) && ok
  ok := (← assert "slopeDiv normal" (slopeDiv 1000 1024 == 2000)) && ok
  ok := (← assert "slopeDiv clamp" (slopeDiv 100000 512 == 2048)) && ok

  -- table sizes and spot values --------------------------------------------
  ok := (← assert "finesine size" (finesine.size == 10240)) && ok
  ok := (← assert "finetangent size" (finetangent.size == 4096)) && ok
  ok := (← assert "tantoangle size" (tantoangle.size == 2049)) && ok
  ok := (← assert "finesine[0]" (finesine.getD 0 1 == 25)) && ok
  ok := (← assert "finesine[2047]" (finesine.getD 2047 0 == 65535)) && ok
  ok := (← assert "finesine[4096]" (finesine.getD 4096 0 == -25)) && ok
  ok := (← assert "finesine[10239]" (finesine.getD 10239 0 == 65535)) && ok
  ok := (← assert "finetangent[0]" (finetangent.getD 0 0 == -170910304)) && ok
  ok := (← assert "finetangent[2047]" (finetangent.getD 2047 0 == -25)) && ok
  ok := (← assert "finetangent[2048]" (finetangent.getD 2048 0 == 25)) && ok
  ok := (← assert "finetangent[4095]" (finetangent.getD 4095 0 == 170910304)) && ok
  ok := (← assert "tantoangle[0]" (tantoangle.getD 0 1 == 0)) && ok
  ok := (← assert "tantoangle[1024]" (tantoangle.getD 1024 0 == 0x12e40520)) && ok
  ok := (← assert "tantoangle[2048]" (tantoangle.getD 2048 0 == 0x20000000)) && ok
  ok := (← assert "finecosine[0] == finesine[2048]"
    (finecosine.getD 0 0 == finesine.getD 2048 1 && finecosine.getD 0 0 == 65535)) && ok
  ok := (← assert "finecosine size" (finecosine.size == 8192)) && ok

  -- full-table FNV-1a64 checksums (reference values from scripts/gen_tables.py,
  -- computed from the oracle C source text)
  ok := (← assert "finesine fnv1a64"
    (fnv1a64 (leBytes32 (i32 finesine)) == 0xadd266f0cb7b06b6)) && ok
  ok := (← assert "finetangent fnv1a64"
    (fnv1a64 (leBytes32 (i32 finetangent)) == 0x6e300cfd277525e1)) && ok
  ok := (← assert "tantoangle fnv1a64"
    (fnv1a64 (leBytes32 tantoangle) == 0x56213612f87a0f4a)) && ok
  ok := (← assert "rndtable fnv1a64"
    (fnv1a64 (ByteArray.mk rndtable) == 0xf4d1eea290a1a085)) && ok

  -- RNG ----------------------------------------------------------------
  ok := (← assert "rndtable size" (rndtable.size == 256)) && ok
  ok := (← assert "rndtable[0..4]"
    (rndtable.getD 0 1 == 0 && rndtable.getD 1 0 == 8 && rndtable.getD 2 0 == 109
      && rndtable.getD 3 0 == 220 && rndtable.getD 4 0 == 222)) && ok
  let s0 := clearRandom
  let (r1, s1) := pRandom s0
  let (r2, s2) := pRandom s1
  let (r3, s3) := pRandom s2
  let (r4, s4) := pRandom s3
  let (r5, _) := pRandom s4
  ok := (← assert "first five pRandom after clear"
    (r1 == 8 && r2 == 109 && r3 == 220 && r4 == 222 && r5 == 241)) && ok
  -- index wrap 255 -> 0: draw at prndindex=255 reads rndtable[0]=0
  let (rw, sw) := pRandom { clearRandom with prndindex := 254 }
  let (rw2, sw2) := pRandom sw
  ok := (← assert "prndindex wrap 255->0"
    (rw == ((rndtable.getD 255 0).toUInt32).toInt32 && sw.prndindex == 255
      && rw2 == 0 && sw2.prndindex == 0)) && ok
  -- pSubRandom consumes two consecutive draws in order: 8 - 109 = -101
  let (d, sd) := pSubRandom clearRandom
  ok := (← assert "pSubRandom two draws in order"
    (d == -101 && sd.prndindex == 2)) && ok
  -- mRandom and pRandom advance independent indices
  let (m1, sm1) := mRandom clearRandom
  let (p1, sm2) := pRandom sm1
  let (m2, sm3) := mRandom sm2
  ok := (← assert "mRandom/pRandom independent"
    (m1 == 8 && p1 == 8 && m2 == 109
      && sm3.rndindex == 2 && sm3.prndindex == 1)) && ok

  if ok then
    IO.println "playsim-test: all passed"
    pure 0
  else
    IO.eprintln "playsim-test: FAILURES"
    pure 1
