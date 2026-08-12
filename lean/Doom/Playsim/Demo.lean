/-!
# Doom.Playsim.Demo

Vanilla Doom 1.9 demo header parse (`G_DoPlayDemo`).
-/

namespace Doom.Playsim.Demo

structure DemoHeader where
  version : UInt8
  skill : UInt8
  episode : UInt8
  map : UInt8
  deathmatch : UInt8
  respawn : UInt8
  fast : UInt8
  nomonsters : UInt8
  consoleplayer : UInt8
  playeringame : Array Bool
  deriving Repr

/-- Parse a 13-byte vanilla 1.9 demo header (version byte included). -/
def parseHeader (data : ByteArray) : Except String DemoHeader := do
  if data.size < 13 then
    throw s!"demo header too short: {data.size}"
  let get (i : Nat) : Except String UInt8 :=
    if h : i < data.size then pure (data.get i h) else throw "demo header EOF"
  let version ← get 0
  if version != 109 then
    throw s!"unsupported demo version {version} (want 109)"
  let skill ← get 1
  let episode ← get 2
  let map ← get 3
  let deathmatch ← get 4
  let respawn ← get 5
  let fast ← get 6
  let nomonsters ← get 7
  let consoleplayer ← get 8
  let p0 ← get 9
  let p1 ← get 10
  let p2 ← get 11
  let p3 ← get 12
  pure {
    version, skill, episode, map
    deathmatch, respawn, fast, nomonsters, consoleplayer
    playeringame := #[p0 != 0, p1 != 0, p2 != 0, p3 != 0]
  }

end Doom.Playsim.Demo
