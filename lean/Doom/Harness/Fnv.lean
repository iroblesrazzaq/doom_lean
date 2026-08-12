namespace Doom.Harness.Fnv

/-- FNV-1a 64-bit offset basis. -/
def offsetBasis : UInt64 := 0xcbf29ce484222325

/-- FNV-1a 64-bit prime. -/
def prime : UInt64 := 0x100000001b3

/--
FNV-1a 64-bit hash over a `ByteArray`.
Uses wrapping `UInt64` arithmetic (Lean `UInt64` mul/xor already mod 2^64).
-/
def fnv1a64 (data : ByteArray) : UInt64 :=
  data.foldl (init := offsetBasis) fun h b =>
    (h.xor b.toUInt64) * prime

end Doom.Harness.Fnv
