#!/usr/bin/env python3
"""Shared Doom trace format library (TRACE.md version 1).

Implements FNV-1a64, full-trace (.trc) and digest-stream (.dig) header/record
encode/decode. Field names match docs/TRACE.md exactly.
"""

from __future__ import annotations

import struct
from dataclasses import dataclass, field
from typing import BinaryIO, List, Optional, Sequence, Tuple

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

FNV_OFFSET = 0xCBF29CE484222325
FNV_PRIME = 0x100000001B3
FNV_MOD = 1 << 64

FORMAT_VERSION = 1

MAGIC_TRC = b"DTRC"
MAGIC_DIG = b"DDIG"

HEADER_SIZE = 16
MO_NULL_TRACE_ID = 0xFFFFFFFF
THF_MOBJ = 1

THF_NAMES = {
    0: "THF_REMOVED",
    1: "THF_MOBJ",
    2: "THF_VERTICALDOOR",
    3: "THF_MOVECEILING",
    4: "THF_MOVEFLOOR",
    5: "THF_PLATRAISE",
    6: "THF_LIGHTFLASH",
    7: "THF_STROBEFLASH",
    8: "THF_GLOW",
    9: "THF_FIREFLICKER",
    10: "THF_NULL",
}


class TraceFormatError(ValueError):
    """Raised for magic/version/flags/decode errors."""


# ---------------------------------------------------------------------------
# FNV-1a64
# ---------------------------------------------------------------------------

def fnv1a64(data: bytes) -> int:
    """FNV-1a 64-bit hash over *data* (unsigned, mod 2^64)."""
    h = FNV_OFFSET
    for b in data:
        h ^= b
        h = (h * FNV_PRIME) % FNV_MOD
    return h


def fnv1a64_hex(data: bytes) -> str:
    """Lowercase 16-digit hex of FNV-1a64(*data*)."""
    return f"{fnv1a64(data):016x}"


# ---------------------------------------------------------------------------
# Headers
# ---------------------------------------------------------------------------

@dataclass
class FileHeader:
    magic: bytes
    version: int
    flags: int
    reserved: int

    def encode(self) -> bytes:
        return self.magic + struct.pack("<III", self.version, self.flags, self.reserved)


def encode_trc_header(
    version: int = FORMAT_VERSION, flags: int = 0, reserved: int = 0
) -> bytes:
    return FileHeader(MAGIC_TRC, version, flags, reserved).encode()


def encode_dig_header(
    version: int = FORMAT_VERSION, flags: int = 0, reserved: int = 0
) -> bytes:
    return FileHeader(MAGIC_DIG, version, flags, reserved).encode()


def parse_header(data: bytes, expected_magic: Optional[bytes] = None) -> FileHeader:
    if len(data) < HEADER_SIZE:
        raise TraceFormatError(
            f"header too short: got {len(data)} bytes, need {HEADER_SIZE}"
        )
    magic = data[0:4]
    version, flags, reserved = struct.unpack_from("<III", data, 4)
    if expected_magic is not None and magic != expected_magic:
        raise TraceFormatError(
            f"bad magic: expected {expected_magic!r}, got {magic!r}"
        )
    return FileHeader(magic=magic, version=version, flags=flags, reserved=reserved)


def read_trc_header(fp: BinaryIO) -> FileHeader:
    raw = fp.read(HEADER_SIZE)
    hdr = parse_header(raw, MAGIC_TRC)
    if hdr.version != FORMAT_VERSION:
        raise TraceFormatError(
            f"format version mismatch: expected v{FORMAT_VERSION}, got v{hdr.version}"
        )
    if hdr.flags != 0:
        raise TraceFormatError(f"nonzero flags in .trc header: {hdr.flags}")
    return hdr


def read_dig_header(fp: BinaryIO) -> FileHeader:
    raw = fp.read(HEADER_SIZE)
    hdr = parse_header(raw, MAGIC_DIG)
    if hdr.version != FORMAT_VERSION:
        raise TraceFormatError(
            f"format version mismatch: expected v{FORMAT_VERSION}, got v{hdr.version}"
        )
    if hdr.flags != 0:
        raise TraceFormatError(f"nonzero flags in .dig header: {hdr.flags}")
    return hdr


def validate_dig_header_bytes(data: bytes) -> FileHeader:
    """Parse a digest header without enforcing version == 1 (for mismatch reporting)."""
    return parse_header(data[:HEADER_SIZE] if len(data) >= HEADER_SIZE else data, MAGIC_DIG)


# ---------------------------------------------------------------------------
# Structured records
# ---------------------------------------------------------------------------

@dataclass
class MobjPayload:
    x: int
    y: int
    z: int
    momx: int
    momy: int
    momz: int
    angle: int
    state: int
    tics: int
    health: int
    flags: int
    type: int


@dataclass
class PlayerRecord:
    player_index: int
    mo_trace_id: int
    x: int
    y: int
    z: int
    momx: int
    momy: int
    momz: int
    angle: int
    viewz: int
    health: int
    armorpoints: int
    readyweapon: int
    pendingweapon: int
    ammo: List[int]  # length 4: ammo[0..3]
    weaponowned: List[int]  # length 9: weaponowned[0..8]
    cmd_forwardmove: int
    cmd_sidemove: int
    cmd_angleturn: int
    cmd_buttons: int


@dataclass
class ThinkerRecord:
    trace_id: int
    func: int
    mobj: Optional[MobjPayload] = None


@dataclass
class SectorRecord:
    sector_index: int
    floorheight: int
    ceilingheight: int


@dataclass
class TicRecord:
    gametic: int
    in_level: int
    leveltime: int
    rndindex: int
    prndindex: int
    players: List[PlayerRecord] = field(default_factory=list)
    thinkers: List[ThinkerRecord] = field(default_factory=list)
    sectors: List[SectorRecord] = field(default_factory=list)
    sectors_digest: int = FNV_OFFSET


# ---------------------------------------------------------------------------
# Encode / decode helpers
# ---------------------------------------------------------------------------

def _pack_u32(v: int) -> bytes:
    return struct.pack("<I", v & 0xFFFFFFFF)


def _pack_i32(v: int) -> bytes:
    return struct.pack("<i", v)


def _pack_u64(v: int) -> bytes:
    return struct.pack("<Q", v & 0xFFFFFFFFFFFFFFFF)


def encode_player(p: PlayerRecord) -> bytes:
    if len(p.ammo) != 4:
        raise TraceFormatError(f"ammo must have length 4, got {len(p.ammo)}")
    if len(p.weaponowned) != 9:
        raise TraceFormatError(
            f"weaponowned must have length 9, got {len(p.weaponowned)}"
        )
    parts = [
        _pack_u32(p.player_index),
        _pack_u32(p.mo_trace_id),
        _pack_i32(p.x),
        _pack_i32(p.y),
        _pack_i32(p.z),
        _pack_i32(p.momx),
        _pack_i32(p.momy),
        _pack_i32(p.momz),
        _pack_u32(p.angle),
        _pack_i32(p.viewz),
        _pack_i32(p.health),
        _pack_i32(p.armorpoints),
        _pack_i32(p.readyweapon),
        _pack_i32(p.pendingweapon),
    ]
    for a in p.ammo:
        parts.append(_pack_i32(a))
    for w in p.weaponowned:
        parts.append(_pack_i32(w))
    parts.extend(
        [
            _pack_i32(p.cmd_forwardmove),
            _pack_i32(p.cmd_sidemove),
            _pack_i32(p.cmd_angleturn),
            _pack_u32(p.cmd_buttons),
        ]
    )
    return b"".join(parts)


def encode_thinker(t: ThinkerRecord) -> bytes:
    parts = [_pack_u32(t.trace_id), _pack_u32(t.func)]
    if t.func == THF_MOBJ:
        if t.mobj is None:
            raise TraceFormatError("THF_MOBJ thinker missing mobj payload")
        m = t.mobj
        parts.extend(
            [
                _pack_i32(m.x),
                _pack_i32(m.y),
                _pack_i32(m.z),
                _pack_i32(m.momx),
                _pack_i32(m.momy),
                _pack_i32(m.momz),
                _pack_u32(m.angle),
                _pack_i32(m.state),
                _pack_i32(m.tics),
                _pack_i32(m.health),
                _pack_u32(m.flags),
                _pack_i32(m.type),
            ]
        )
    elif t.mobj is not None:
        raise TraceFormatError(
            f"non-MOBJ thinker func={t.func} must not carry mobj payload"
        )
    return b"".join(parts)


def encode_sector(s: SectorRecord) -> bytes:
    return _pack_u32(s.sector_index) + _pack_i32(s.floorheight) + _pack_i32(s.ceilingheight)


def encode_tic_record(rec: TicRecord) -> bytes:
    """Canonical byte encoding of one tic record (TRACE.md §5)."""
    parts = [
        _pack_u32(rec.gametic),
        _pack_u32(rec.in_level),
        _pack_u32(rec.leveltime),
        _pack_u32(rec.rndindex),
        _pack_u32(rec.prndindex),
        _pack_u32(len(rec.players)),
    ]
    for p in rec.players:
        parts.append(encode_player(p))
    parts.append(_pack_u32(len(rec.thinkers)))
    for t in rec.thinkers:
        parts.append(encode_thinker(t))
    parts.append(_pack_u32(len(rec.sectors)))
    for s in rec.sectors:
        parts.append(encode_sector(s))
    parts.append(_pack_u64(rec.sectors_digest))
    return b"".join(parts)


class _Reader:
    def __init__(self, data: bytes, offset: int = 0):
        self.data = data
        self.offset = offset

    def remaining(self) -> int:
        return len(self.data) - self.offset

    def u32(self) -> int:
        if self.remaining() < 4:
            raise TraceFormatError("truncated while reading u32")
        (v,) = struct.unpack_from("<I", self.data, self.offset)
        self.offset += 4
        return v

    def i32(self) -> int:
        if self.remaining() < 4:
            raise TraceFormatError("truncated while reading i32")
        (v,) = struct.unpack_from("<i", self.data, self.offset)
        self.offset += 4
        return v

    def u64(self) -> int:
        if self.remaining() < 8:
            raise TraceFormatError("truncated while reading u64")
        (v,) = struct.unpack_from("<Q", self.data, self.offset)
        self.offset += 8
        return v


def decode_player(r: _Reader) -> PlayerRecord:
    player_index = r.u32()
    mo_trace_id = r.u32()
    x = r.i32()
    y = r.i32()
    z = r.i32()
    momx = r.i32()
    momy = r.i32()
    momz = r.i32()
    angle = r.u32()
    viewz = r.i32()
    health = r.i32()
    armorpoints = r.i32()
    readyweapon = r.i32()
    pendingweapon = r.i32()
    ammo = [r.i32() for _ in range(4)]
    weaponowned = [r.i32() for _ in range(9)]
    cmd_forwardmove = r.i32()
    cmd_sidemove = r.i32()
    cmd_angleturn = r.i32()
    cmd_buttons = r.u32()
    return PlayerRecord(
        player_index=player_index,
        mo_trace_id=mo_trace_id,
        x=x,
        y=y,
        z=z,
        momx=momx,
        momy=momy,
        momz=momz,
        angle=angle,
        viewz=viewz,
        health=health,
        armorpoints=armorpoints,
        readyweapon=readyweapon,
        pendingweapon=pendingweapon,
        ammo=ammo,
        weaponowned=weaponowned,
        cmd_forwardmove=cmd_forwardmove,
        cmd_sidemove=cmd_sidemove,
        cmd_angleturn=cmd_angleturn,
        cmd_buttons=cmd_buttons,
    )


def decode_thinker(r: _Reader) -> ThinkerRecord:
    trace_id = r.u32()
    func = r.u32()
    mobj = None
    if func == THF_MOBJ:
        mobj = MobjPayload(
            x=r.i32(),
            y=r.i32(),
            z=r.i32(),
            momx=r.i32(),
            momy=r.i32(),
            momz=r.i32(),
            angle=r.u32(),
            state=r.i32(),
            tics=r.i32(),
            health=r.i32(),
            flags=r.u32(),
            type=r.i32(),
        )
    return ThinkerRecord(trace_id=trace_id, func=func, mobj=mobj)


def decode_sector(r: _Reader) -> SectorRecord:
    return SectorRecord(
        sector_index=r.u32(),
        floorheight=r.i32(),
        ceilingheight=r.i32(),
    )


def decode_tic_record(data: bytes, offset: int = 0) -> Tuple[TicRecord, int]:
    """Decode one tic record starting at *offset*. Returns (record, new_offset)."""
    r = _Reader(data, offset)
    gametic = r.u32()
    in_level = r.u32()
    leveltime = r.u32()
    rndindex = r.u32()
    prndindex = r.u32()
    player_count = r.u32()
    players = [decode_player(r) for _ in range(player_count)]
    thinker_count = r.u32()
    thinkers = [decode_thinker(r) for _ in range(thinker_count)]
    active_sector_count = r.u32()
    sectors = [decode_sector(r) for _ in range(active_sector_count)]
    sectors_digest = r.u64()
    rec = TicRecord(
        gametic=gametic,
        in_level=in_level,
        leveltime=leveltime,
        rndindex=rndindex,
        prndindex=prndindex,
        players=players,
        thinkers=thinkers,
        sectors=sectors,
        sectors_digest=sectors_digest,
    )
    return rec, r.offset


def decode_all_tic_records(body: bytes) -> List[TicRecord]:
    """Decode every tic record from the body of a .trc file (after header)."""
    records: List[TicRecord] = []
    offset = 0
    while offset < len(body):
        rec, offset = decode_tic_record(body, offset)
        records.append(rec)
    return records


def read_full_trace(path: str) -> Tuple[FileHeader, List[TicRecord], List[bytes]]:
    """Read a .trc file. Returns (header, records, raw_record_bytes_list)."""
    with open(path, "rb") as fp:
        hdr = read_trc_header(fp)
        body = fp.read()
    records: List[TicRecord] = []
    raws: List[bytes] = []
    offset = 0
    while offset < len(body):
        start = offset
        rec, offset = decode_tic_record(body, offset)
        records.append(rec)
        raws.append(body[start:offset])
    return hdr, records, raws


# ---------------------------------------------------------------------------
# Digest stream
# ---------------------------------------------------------------------------

def read_digest_stream(path: str) -> Tuple[FileHeader, List[int]]:
    """Read a .dig file. Returns (header, list of u64 digests)."""
    with open(path, "rb") as fp:
        raw = fp.read()
    if len(raw) < HEADER_SIZE:
        raise TraceFormatError(f"digest file too short: {path}")
    hdr = parse_header(raw[:HEADER_SIZE], MAGIC_DIG)
    body = raw[HEADER_SIZE:]
    if len(body) % 8 != 0:
        raise TraceFormatError(
            f"digest stream body length {len(body)} is not a multiple of 8: {path}"
        )
    digests = list(struct.unpack(f"<{len(body) // 8}Q", body)) if body else []
    return hdr, digests


def write_digest_stream(
    path: str,
    digests: Sequence[int],
    version: int = FORMAT_VERSION,
    flags: int = 0,
    reserved: int = 0,
) -> None:
    with open(path, "wb") as fp:
        fp.write(encode_dig_header(version, flags, reserved))
        for d in digests:
            fp.write(_pack_u64(d))


def write_full_trace(
    path: str,
    records: Sequence[TicRecord],
    version: int = FORMAT_VERSION,
    flags: int = 0,
    reserved: int = 0,
) -> List[bytes]:
    """Write a .trc file. Returns the list of encoded record byte strings."""
    raws: List[bytes] = []
    with open(path, "wb") as fp:
        fp.write(encode_trc_header(version, flags, reserved))
        for rec in records:
            raw = encode_tic_record(rec)
            raws.append(raw)
            fp.write(raw)
    return raws


def digests_from_records(records: Sequence[TicRecord]) -> Tuple[List[int], List[bytes]]:
    """Encode records and compute per-tic digests. Returns (digests, raws)."""
    digests: List[int] = []
    raws: List[bytes] = []
    for rec in records:
        raw = encode_tic_record(rec)
        raws.append(raw)
        digests.append(fnv1a64(raw))
    return digests, raws


def write_trace_pair(
    trc_path: str,
    dig_path: str,
    records: Sequence[TicRecord],
    version: int = FORMAT_VERSION,
) -> None:
    """Write matching .trc and .dig files for *records*."""
    raws = write_full_trace(trc_path, records, version=version)
    digests = [fnv1a64(r) for r in raws]
    write_digest_stream(dig_path, digests, version=version)
