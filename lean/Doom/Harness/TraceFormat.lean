import Doom.Harness.Fnv

namespace Doom.Harness.TraceFormat

/-! Structures and canonical encoders mirroring `docs/TRACE.md` version 1. -/

/-- Mobj payload: 12 raw `UInt32` bit patterns (§5.2 fields 3–14). -/
structure MobjPayload where
  x : UInt32
  y : UInt32
  z : UInt32
  momx : UInt32
  momy : UInt32
  momz : UInt32
  angle : UInt32
  state : UInt32
  tics : UInt32
  health : UInt32
  flags : UInt32
  type_ : UInt32
  deriving BEq, Repr, Inhabited

/-- Thinker record (§5.2). Payload present iff `func == 1` (`THF_MOBJ`). -/
structure ThinkerRec where
  traceId : UInt32
  func : UInt32
  mobj : Option MobjPayload
  deriving BEq, Repr, Inhabited

/-- Sector record (§5.3); heights stored as raw `UInt32` patterns. -/
structure SectorRec where
  sectorIndex : UInt32
  floorheight : UInt32
  ceilingheight : UInt32
  deriving BEq, Repr, Inhabited

/-- Player record (§5.1); all fields as raw `UInt32` bit patterns. -/
structure PlayerRec where
  playerIndex : UInt32
  moTraceId : UInt32
  x : UInt32
  y : UInt32
  z : UInt32
  momx : UInt32
  momy : UInt32
  momz : UInt32
  angle : UInt32
  viewz : UInt32
  health : UInt32
  armorpoints : UInt32
  readyweapon : UInt32
  pendingweapon : UInt32
  ammo0 : UInt32
  ammo1 : UInt32
  ammo2 : UInt32
  ammo3 : UInt32
  weaponowned0 : UInt32
  weaponowned1 : UInt32
  weaponowned2 : UInt32
  weaponowned3 : UInt32
  weaponowned4 : UInt32
  weaponowned5 : UInt32
  weaponowned6 : UInt32
  weaponowned7 : UInt32
  weaponowned8 : UInt32
  cmdForwardmove : UInt32
  cmdSidemove : UInt32
  cmdAngleturn : UInt32
  cmdButtons : UInt32
  deriving BEq, Repr, Inhabited

/-- One tic record (§5). -/
structure TicRecord where
  gametic : UInt32
  inLevel : UInt32
  leveltime : UInt32
  rndindex : UInt32
  prndindex : UInt32
  players : Array PlayerRec
  thinkers : Array ThinkerRec
  sectors : Array SectorRec
  sectorsDigest : UInt64
  deriving BEq, Repr, Inhabited

/-- Empty FNV-1a64 (non-level `sectors_digest`). -/
def emptyFnv : UInt64 := Fnv.offsetBasis

/-- A deliberately-wrong all-zero tic record (counts empty, `sectorsDigest = 0`). -/
def zeroTicRecord : TicRecord where
  gametic := 0
  inLevel := 0
  leveltime := 0
  rndindex := 0
  prndindex := 0
  players := #[]
  thinkers := #[]
  sectors := #[]
  sectorsDigest := 0

/-- Push one little-endian `UInt32` onto an unshared accumulator (O(1) amortized). -/
def pushU32LE (acc : ByteArray) (v : UInt32) : ByteArray :=
  acc
    |>.push ((v &&& 0xff).toUInt8)
    |>.push (((v >>> 8) &&& 0xff).toUInt8)
    |>.push (((v >>> 16) &&& 0xff).toUInt8)
    |>.push (((v >>> 24) &&& 0xff).toUInt8)

/-- Push one little-endian `UInt64` onto an unshared accumulator. -/
def pushU64LE (acc : ByteArray) (v : UInt64) : ByteArray :=
  acc
    |>.push ((v &&& 0xff).toUInt8)
    |>.push (((v >>> 8) &&& 0xff).toUInt8)
    |>.push (((v >>> 16) &&& 0xff).toUInt8)
    |>.push (((v >>> 24) &&& 0xff).toUInt8)
    |>.push (((v >>> 32) &&& 0xff).toUInt8)
    |>.push (((v >>> 40) &&& 0xff).toUInt8)
    |>.push (((v >>> 48) &&& 0xff).toUInt8)
    |>.push (((v >>> 56) &&& 0xff).toUInt8)

def pushPlayer (acc : ByteArray) (p : PlayerRec) : ByteArray :=
  let acc := pushU32LE acc p.playerIndex
  let acc := pushU32LE acc p.moTraceId
  let acc := pushU32LE acc p.x
  let acc := pushU32LE acc p.y
  let acc := pushU32LE acc p.z
  let acc := pushU32LE acc p.momx
  let acc := pushU32LE acc p.momy
  let acc := pushU32LE acc p.momz
  let acc := pushU32LE acc p.angle
  let acc := pushU32LE acc p.viewz
  let acc := pushU32LE acc p.health
  let acc := pushU32LE acc p.armorpoints
  let acc := pushU32LE acc p.readyweapon
  let acc := pushU32LE acc p.pendingweapon
  let acc := pushU32LE acc p.ammo0
  let acc := pushU32LE acc p.ammo1
  let acc := pushU32LE acc p.ammo2
  let acc := pushU32LE acc p.ammo3
  let acc := pushU32LE acc p.weaponowned0
  let acc := pushU32LE acc p.weaponowned1
  let acc := pushU32LE acc p.weaponowned2
  let acc := pushU32LE acc p.weaponowned3
  let acc := pushU32LE acc p.weaponowned4
  let acc := pushU32LE acc p.weaponowned5
  let acc := pushU32LE acc p.weaponowned6
  let acc := pushU32LE acc p.weaponowned7
  let acc := pushU32LE acc p.weaponowned8
  let acc := pushU32LE acc p.cmdForwardmove
  let acc := pushU32LE acc p.cmdSidemove
  let acc := pushU32LE acc p.cmdAngleturn
  pushU32LE acc p.cmdButtons

def pushThinker (acc : ByteArray) (t : ThinkerRec) : ByteArray :=
  let acc := pushU32LE acc t.traceId
  let acc := pushU32LE acc t.func
  match t.mobj with
  | none => acc
  | some m =>
    let acc := pushU32LE acc m.x
    let acc := pushU32LE acc m.y
    let acc := pushU32LE acc m.z
    let acc := pushU32LE acc m.momx
    let acc := pushU32LE acc m.momy
    let acc := pushU32LE acc m.momz
    let acc := pushU32LE acc m.angle
    let acc := pushU32LE acc m.state
    let acc := pushU32LE acc m.tics
    let acc := pushU32LE acc m.health
    let acc := pushU32LE acc m.flags
    pushU32LE acc m.type_

def pushSector (acc : ByteArray) (s : SectorRec) : ByteArray :=
  let acc := pushU32LE acc s.sectorIndex
  let acc := pushU32LE acc s.floorheight
  pushU32LE acc s.ceilingheight

/-- Canonical encoding of one tic record (§5), linear in record size. -/
def encodeTicRecord (r : TicRecord) : ByteArray :=
  let acc := ByteArray.empty
  let acc := pushU32LE acc r.gametic
  let acc := pushU32LE acc r.inLevel
  let acc := pushU32LE acc r.leveltime
  let acc := pushU32LE acc r.rndindex
  let acc := pushU32LE acc r.prndindex
  let acc := pushU32LE acc r.players.size.toUInt32
  let acc := r.players.foldl pushPlayer acc
  let acc := pushU32LE acc r.thinkers.size.toUInt32
  let acc := r.thinkers.foldl pushThinker acc
  let acc := pushU32LE acc r.sectors.size.toUInt32
  let acc := r.sectors.foldl pushSector acc
  pushU64LE acc r.sectorsDigest

/-- Full-trace file header: magic `DTRC`, version 1, flags 0, reserved 0. -/
def encodeTraceHeader : ByteArray :=
  let acc := ByteArray.empty
  let acc := acc.push 0x44 |>.push 0x54 |>.push 0x52 |>.push 0x43
  let acc := pushU32LE acc 1
  let acc := pushU32LE acc 0
  pushU32LE acc 0

/-- Digest-stream file header: magic `DDIG`, version 1, flags 0, reserved 0. -/
def encodeDigestHeader : ByteArray :=
  let acc := ByteArray.empty
  let acc := acc.push 0x44 |>.push 0x44 |>.push 0x49 |>.push 0x47
  let acc := pushU32LE acc 1
  let acc := pushU32LE acc 0
  pushU32LE acc 0

/-- Append `src` onto an unshared accumulator via linear pushes. -/
def appendBytes (acc : ByteArray) (src : ByteArray) : ByteArray :=
  src.foldl ByteArray.push acc

/-- Encode a full `.trc` file (header + records). -/
def encodeTraceFile (records : Array TicRecord) : ByteArray :=
  records.foldl (init := encodeTraceHeader) fun acc r =>
    appendBytes acc (encodeTicRecord r)

/-- Encode a `.dig` file; each digest is FNV-1a64 of that record's canonical bytes. -/
def encodeDigestFileFromRecords (records : Array TicRecord) : ByteArray :=
  records.foldl (init := encodeDigestHeader) fun acc r =>
    pushU64LE acc (Fnv.fnv1a64 (encodeTicRecord r))

/-- Write `pathBase.trc` and `pathBase.dig` for the given records. -/
def writeTracePair (pathBase : System.FilePath) (records : Array TicRecord) : IO Unit := do
  let trcPath := pathBase.toString ++ ".trc"
  let digPath := pathBase.toString ++ ".dig"
  IO.FS.writeBinFile trcPath (encodeTraceFile records)
  IO.FS.writeBinFile digPath (encodeDigestFileFromRecords records)


end Doom.Harness.TraceFormat
