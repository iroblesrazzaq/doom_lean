import Doom.Playsim.Player

/-!
# Doom.Playsim.Demo

Vanilla Doom 1.9 demo header parse (`G_DoPlayDemo`) and ticcmd stream read.
-/

namespace Doom.Playsim.Demo

open Doom.Playsim.Player

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

/-- Demo stream end marker. -/
def DEMOMARKER : UInt8 := 0x80

/-- Sign-extend an 8-bit value to `Int32` (C `signed char` widen). -/
def signExtendI8 (b : UInt8) : Int32 :=
  let u := b.toUInt32
  let bits := if u >= 128 then u ||| (0xffffff00 : UInt32) else u
  bits.toInt32

/--
`G_ReadDemoTiccmd` (shorttics): four bytes after the 13-byte header.
`angleturn = ((signed char)demo_p[2]) << 8` — sign-extended short in the
trace payload (`docs/TRACE.md` cmd_angleturn).
-/
def readDemoTiccmd (data : ByteArray) (cursor : Nat) :
    Except String (Nat × TicCmd) := do
  if cursor >= data.size then
    throw "G_ReadDemoTiccmd: demo EOF"
  let get (i : Nat) : Except String UInt8 :=
    if h : i < data.size then pure (data.get i h) else throw "G_ReadDemoTiccmd: truncated ticcmd"
  let b0 ← get cursor
  if b0 == DEMOMARKER then
    throw "G_ReadDemoTiccmd: DEMOMARKER (demo end)"
  let b1 ← get (cursor + 1)
  let b2 ← get (cursor + 2)
  let b3 ← get (cursor + 3)
  pure (cursor + 4, {
    forwardmove := signExtendI8 b0
    sidemove := signExtendI8 b1
    angleturn := signExtendI8 b2 <<< 8
    buttons := b3.toUInt32
  })

end Doom.Playsim.Demo
