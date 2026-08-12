/-!
# Doom.Playsim.Thinker

Thinker function ids matching `docs/TRACE.md` §6.2 (`THF_*`).
-/

namespace Doom.Playsim.Thinker

def THF_REMOVED : UInt32 := 0
def THF_MOBJ : UInt32 := 1
def THF_VERTICALDOOR : UInt32 := 2
def THF_MOVECEILING : UInt32 := 3
def THF_MOVEFLOOR : UInt32 := 4
def THF_PLATRAISE : UInt32 := 5
def THF_LIGHTFLASH : UInt32 := 6
def THF_STROBEFLASH : UInt32 := 7
def THF_GLOW : UInt32 := 8
def THF_FIREFLICKER : UInt32 := 9
def THF_NULL : UInt32 := 10

/-- One thinker list entry; payload index refers into the matching GameState array. -/
structure Thinker where
  traceId : UInt32
  func : UInt32
  /-- Index into `mobjs` / light-thinker arrays depending on `func`. -/
  payload : UInt32
  deriving Repr

end Doom.Playsim.Thinker
