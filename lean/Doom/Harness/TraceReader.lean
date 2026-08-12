import Doom.Harness.TraceFormat

namespace Doom.Harness.TraceReader

open Doom.Harness.TraceFormat

structure ParseState where
  data : ByteArray
  pos : Nat

def ParseState.remaining (s : ParseState) : Nat :=
  s.data.size - s.pos

def getByte? (data : ByteArray) (i : Nat) : Option UInt8 :=
  if h : i < data.size then some (data.get i h) else none

def readU8 (s : ParseState) : Except String (UInt8 × ParseState) :=
  match getByte? s.data s.pos with
  | none => Except.error s!"unexpected end of file at offset {s.pos}"
  | some b => Except.ok (b, { s with pos := s.pos + 1 })

def readU32LE (s : ParseState) : Except String (UInt32 × ParseState) := do
  let (b0, s) ← readU8 s
  let (b1, s) ← readU8 s
  let (b2, s) ← readU8 s
  let (b3, s) ← readU8 s
  let v :=
    b0.toUInt32
      ||| (b1.toUInt32 <<< 8)
      ||| (b2.toUInt32 <<< 16)
      ||| (b3.toUInt32 <<< 24)
  pure (v, s)

def readU64LE (s : ParseState) : Except String (UInt64 × ParseState) := do
  let (b0, s) ← readU8 s
  let (b1, s) ← readU8 s
  let (b2, s) ← readU8 s
  let (b3, s) ← readU8 s
  let (b4, s) ← readU8 s
  let (b5, s) ← readU8 s
  let (b6, s) ← readU8 s
  let (b7, s) ← readU8 s
  let v :=
    b0.toUInt64
      ||| (b1.toUInt64 <<< 8)
      ||| (b2.toUInt64 <<< 16)
      ||| (b3.toUInt64 <<< 24)
      ||| (b4.toUInt64 <<< 32)
      ||| (b5.toUInt64 <<< 40)
      ||| (b6.toUInt64 <<< 48)
      ||| (b7.toUInt64 <<< 56)
  pure (v, s)

def expectMagic (s : ParseState) (b0 b1 b2 b3 : UInt8) (name : String) :
    Except String ParseState := do
  let (c0, s) ← readU8 s
  let (c1, s) ← readU8 s
  let (c2, s) ← readU8 s
  let (c3, s) ← readU8 s
  if c0 == b0 && c1 == b1 && c2 == b2 && c3 == b3 then
    pure s
  else
    Except.error
      s!"bad {name} magic: got {c0.toNat},{c1.toNat},{c2.toNat},{c3.toNat} \
expected {b0.toNat},{b1.toNat},{b2.toNat},{b3.toNat}"

def readHeader (s : ParseState) (magic0 magic1 magic2 magic3 : UInt8) (kind : String) :
    Except String ParseState := do
  let s ← expectMagic s magic0 magic1 magic2 magic3 kind
  let (ver, s) ← readU32LE s
  if ver != 1 then
    Except.error s!"unsupported {kind} version {ver}, expected 1"
  let (flags, s) ← readU32LE s
  if flags != 0 then
    Except.error s!"nonzero {kind} flags {flags} (reserved, must be 0)"
  let (reserved, s) ← readU32LE s
  if reserved != 0 then
    Except.error s!"nonzero {kind} reserved {reserved} (must be 0)"
  pure s

def readPlayer (s : ParseState) : Except String (PlayerRec × ParseState) := do
  let (playerIndex, s) ← readU32LE s
  let (moTraceId, s) ← readU32LE s
  let (x, s) ← readU32LE s
  let (y, s) ← readU32LE s
  let (z, s) ← readU32LE s
  let (momx, s) ← readU32LE s
  let (momy, s) ← readU32LE s
  let (momz, s) ← readU32LE s
  let (angle, s) ← readU32LE s
  let (viewz, s) ← readU32LE s
  let (health, s) ← readU32LE s
  let (armorpoints, s) ← readU32LE s
  let (readyweapon, s) ← readU32LE s
  let (pendingweapon, s) ← readU32LE s
  let (ammo0, s) ← readU32LE s
  let (ammo1, s) ← readU32LE s
  let (ammo2, s) ← readU32LE s
  let (ammo3, s) ← readU32LE s
  let (weaponowned0, s) ← readU32LE s
  let (weaponowned1, s) ← readU32LE s
  let (weaponowned2, s) ← readU32LE s
  let (weaponowned3, s) ← readU32LE s
  let (weaponowned4, s) ← readU32LE s
  let (weaponowned5, s) ← readU32LE s
  let (weaponowned6, s) ← readU32LE s
  let (weaponowned7, s) ← readU32LE s
  let (weaponowned8, s) ← readU32LE s
  let (cmdForwardmove, s) ← readU32LE s
  let (cmdSidemove, s) ← readU32LE s
  let (cmdAngleturn, s) ← readU32LE s
  let (cmdButtons, s) ← readU32LE s
  let p : PlayerRec := {
    playerIndex, moTraceId, x, y, z, momx, momy, momz, angle, viewz,
    health, armorpoints, readyweapon, pendingweapon,
    ammo0, ammo1, ammo2, ammo3,
    weaponowned0, weaponowned1, weaponowned2, weaponowned3, weaponowned4,
    weaponowned5, weaponowned6, weaponowned7, weaponowned8,
    cmdForwardmove, cmdSidemove, cmdAngleturn, cmdButtons
  }
  pure (p, s)

def readMobjPayload (s : ParseState) : Except String (MobjPayload × ParseState) := do
  let (x, s) ← readU32LE s
  let (y, s) ← readU32LE s
  let (z, s) ← readU32LE s
  let (momx, s) ← readU32LE s
  let (momy, s) ← readU32LE s
  let (momz, s) ← readU32LE s
  let (angle, s) ← readU32LE s
  let (state, s) ← readU32LE s
  let (tics, s) ← readU32LE s
  let (health, s) ← readU32LE s
  let (flags, s) ← readU32LE s
  let (type_, s) ← readU32LE s
  pure ({ x, y, z, momx, momy, momz, angle, state, tics, health, flags, type_ }, s)

def readThinker (s : ParseState) : Except String (ThinkerRec × ParseState) := do
  let (traceId, s) ← readU32LE s
  let (func, s) ← readU32LE s
  if func == 1 then
    let (mobj, s) ← readMobjPayload s
    pure ({ traceId, func, mobj := some mobj }, s)
  else
    pure ({ traceId, func, mobj := none }, s)

def readSector (s : ParseState) : Except String (SectorRec × ParseState) := do
  let (sectorIndex, s) ← readU32LE s
  let (floorheight, s) ← readU32LE s
  let (ceilingheight, s) ← readU32LE s
  pure ({ sectorIndex, floorheight, ceilingheight }, s)

/-- Read `n` items using a fuel-bounded loop (no `partial`). -/
def readN {α : Type} (n : Nat) (s : ParseState)
    (readOne : ParseState → Except String (α × ParseState)) :
    Except String (Array α × ParseState) :=
  let rec go (fuel : Nat) (left : Nat) (s : ParseState) (acc : Array α) :
      Except String (Array α × ParseState) :=
    match fuel with
    | 0 =>
      if left == 0 then pure (acc, s)
      else Except.error "internal fuel exhausted while parsing array"
    | fuel' + 1 =>
      match left with
      | 0 => pure (acc, s)
      | left' + 1 => do
        let (x, s) ← readOne s
        go fuel' left' s (acc.push x)
  go (n + 1) n s #[]

def readTicRecord (s : ParseState) : Except String (TicRecord × ParseState) := do
  let (gametic, s) ← readU32LE s
  let (inLevel, s) ← readU32LE s
  let (leveltime, s) ← readU32LE s
  let (rndindex, s) ← readU32LE s
  let (prndindex, s) ← readU32LE s
  let (playerCount, s) ← readU32LE s
  let (players, s) ← readN playerCount.toNat s readPlayer
  let (thinkerCount, s) ← readU32LE s
  let (thinkers, s) ← readN thinkerCount.toNat s readThinker
  let (sectorCount, s) ← readU32LE s
  let (sectors, s) ← readN sectorCount.toNat s readSector
  let (sectorsDigest, s) ← readU64LE s
  pure ({
    gametic, inLevel, leveltime, rndindex, prndindex,
    players, thinkers, sectors, sectorsDigest
  }, s)

/-- Parse all tic records after a validated full-trace header. -/
def readAllRecords (s : ParseState) : Except String (Array TicRecord) :=
  let rec go (fuel : Nat) (s : ParseState) (acc : Array TicRecord) :
      Except String (Array TicRecord) :=
    match fuel with
    | 0 =>
      if s.pos == s.data.size then pure acc
      else Except.error "internal fuel exhausted while parsing records"
    | fuel' + 1 =>
      if s.pos == s.data.size then
        pure acc
      else do
        let (r, s) ← readTicRecord s
        go fuel' s (acc.push r)
  -- Each record is at least 40 bytes; fuel by remaining bytes + 1.
  go (s.remaining + 1) s #[]

/-- Parse a full `.trc` file into tic records. Validates magic and version. -/
def parseTraceBytes (data : ByteArray) : Except String (Array TicRecord) := do
  let s : ParseState := { data, pos := 0 }
  let s ← readHeader s 0x44 0x54 0x52 0x43 "trace"
  readAllRecords s

/-- Read and parse a full `.trc` file from disk. -/
def readTraceFile (path : System.FilePath) : IO (Except String (Array TicRecord)) := do
  let data ← IO.FS.readBinFile path
  pure (parseTraceBytes data)

/-- Parse digests from a `.dig` file (for harness self-tests). -/
def parseDigestBytes (data : ByteArray) : Except String (Array UInt64) := do
  let s : ParseState := { data, pos := 0 }
  let s ← readHeader s 0x44 0x44 0x49 0x47 "digest"
  let rec go (fuel : Nat) (s : ParseState) (acc : Array UInt64) :
      Except String (Array UInt64) :=
    match fuel with
    | 0 =>
      if s.pos == s.data.size then pure acc
      else Except.error "internal fuel exhausted while parsing digests"
    | fuel' + 1 =>
      if s.pos == s.data.size then
        pure acc
      else do
        let (d, s) ← readU64LE s
        go fuel' s (acc.push d)
  go (s.remaining + 1) s #[]

end Doom.Harness.TraceReader
