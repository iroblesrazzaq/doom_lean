import Doom.Playsim.Fixed
import Doom.Playsim.Angle
import Doom.Playsim.Tables
import Doom.Playsim.Random
import Doom.Playsim.Level

/-!
# Doom.Playsim

This namespace will hold the actual Doom playsim port.

## Enforced conventions

(a) All playsim arithmetic uses explicitly wrapping `UInt32`/`Int32` operations —
arbitrary-precision `Int` is FORBIDDEN in playsim module signatures (CI lints for it).

(b) Game state lives in a record of `Array`s allocated at level load, threaded through
`ST`/`StateRefT`, and references must stay unshared so `Array.set` is O(1) —
accidental sharing silently turns updates into full copies and tanks tics/second
while remaining correct, which only the perf gate catches.
-/

namespace Doom.Playsim

/-- Placeholder until the playsim port lands. -/
def placeholder : Unit := ()

end Doom.Playsim
