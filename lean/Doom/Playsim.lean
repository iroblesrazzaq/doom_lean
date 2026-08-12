import Doom.Playsim.Fixed
import Doom.Playsim.Angle
import Doom.Playsim.Tables
import Doom.Playsim.Random
import Doom.Playsim.Level
import Doom.Playsim.Info
import Doom.Playsim.Flags
import Doom.Playsim.Thinker
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.GameState
import Doom.Playsim.Bsp
import Doom.Playsim.Demo
import Doom.Playsim.MapUtil
import Doom.Playsim.Map
import Doom.Playsim.Inter
import Doom.Playsim.Spawn
import Doom.Playsim.Weapons
import Doom.Playsim.Psprite
import Doom.Playsim.Sight
import Doom.Playsim.Sound
import Doom.Playsim.Enemy
import Doom.Playsim.Think
import Doom.Playsim.PlayerThink
import Doom.Playsim.Tick
import Doom.Playsim.TraceEmit

/-!
# Doom.Playsim

This namespace holds the Doom playsim port.

## Enforced conventions

(a) All playsim arithmetic uses explicitly wrapping `UInt32`/`Int32` operations —
arbitrary-precision `Int` is FORBIDDEN in playsim module signatures (CI lints for it).

(b) Game state lives in a record of `Array`s allocated at level load, threaded through
`ST`/`StateRefT`, and references must stay unshared so `Array.set` is O(1) —
accidental sharing silently turns updates into full copies and tanks tics/second
while remaining correct, which only the perf gate catches.
-/

namespace Doom.Playsim

/-- Placeholder until broader playsim surface lands. -/
def placeholder : Unit := ()

end Doom.Playsim
