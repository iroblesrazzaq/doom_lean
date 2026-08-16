import Doom.Wad

/-!
# Doom.Render.Gfx.Flat

Flat lump indexing (`r_data.c` `R_InitFlats`).
-/

namespace Doom.Render.Gfx.Flat

open Doom.Wad

def findMarker (wad : WadDirectory) (name : String) : Except String Nat :=
  match checkNumForName wad name with
  | none => throw s!"missing marker lump {name}"
  | some idx => pure idx

def initFlats (wad : WadDirectory) : Except String (Nat × Nat) := do
  let start ← findMarker wad "F_START"
  let stop ← findMarker wad "F_END"
  let first := start + 1
  let last := stop - 1
  if last < first then throw "invalid flat range"
  pure (first, last - first + 1)

def flatNumForName (wad : WadDirectory) (first : Nat) (name : String) : Except String Nat :=
  match checkNumForName wad name with
  | none => throw s!"R_FlatNumForName: {name} not found"
  | some idx => pure (idx - first)

end Doom.Render.Gfx.Flat
