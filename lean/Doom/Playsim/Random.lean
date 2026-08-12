import Doom.Playsim.Random.Rndtable

/-!
# Doom.Playsim.Random

Doom RNG ported from oracle `src/doom/m_random.c`: a 256-byte lookup table
and two independent indices (`prndindex` for the playsim, `rndindex` for
non-sync effects). Pure state-passing API; C semantics are
`rndtable[(++index) & 0xff]` — the index is incremented (mod 256) *before*
the table read, so the first draw after a clear yields `rndtable[1] = 8`.
The C functions return `int`; ported as `Int32`.
-/

namespace Doom.Playsim.Random

/-- RNG state: two independent indices into `rndtable`. -/
structure RandomState where
  prndindex : UInt32
  rndindex : UInt32
  deriving Repr, DecidableEq

/-- `M_ClearRandom`: both indices reset to 0. -/
def clearRandom : RandomState := { prndindex := 0, rndindex := 0 }

private def tableAt (i : UInt32) : Int32 :=
  ((rndtable.getD i.toNat 0).toUInt32).toInt32

/-- `P_Random`: playsim RNG draw. -/
def pRandom (s : RandomState) : Int32 × RandomState :=
  let i := (s.prndindex + 1) &&& 0xff
  (tableAt i, { s with prndindex := i })

/-- `M_Random`: non-sync RNG draw (independent index). -/
def mRandom (s : RandomState) : Int32 × RandomState :=
  let i := (s.rndindex + 1) &&& 0xff
  (tableAt i, { s with rndindex := i })

/-- `P_SubRandom`: `r1 - r2` of two consecutive `P_Random` draws. -/
def pSubRandom (s : RandomState) : Int32 × RandomState :=
  let (r1, s) := pRandom s
  let (r2, s) := pRandom s
  (r1 - r2, s)

end Doom.Playsim.Random
