#!/usr/bin/env python3
"""Synthetic Doom trace builder for self-tests and acceptance testing.

Builds small valid .trc/.dig pairs conforming to docs/TRACE.md.
Can be imported as a library or invoked as a CLI.
"""

from __future__ import annotations

import argparse
import os
import struct
import sys
from typing import List, Optional, Sequence

# Allow running as a script from tools/ or elsewhere.
_TOOLS_DIR = os.path.dirname(os.path.abspath(__file__))
if _TOOLS_DIR not in sys.path:
    sys.path.insert(0, _TOOLS_DIR)

from tracelib import (  # noqa: E402
    FNV_OFFSET,
    MobjPayload,
    PlayerRecord,
    SectorRecord,
    ThinkerRecord,
    TicRecord,
    THF_MOBJ,
    fnv1a64,
    write_trace_pair,
)


def make_player(
    player_index: int = 0,
    mo_trace_id: int = 1,
    x: int = 0,
    y: int = 0,
    z: int = 0,
    momx: int = 0,
    momy: int = 0,
    momz: int = 0,
    angle: int = 0,
    viewz: int = 41 << 16,
    health: int = 100,
    armorpoints: int = 0,
    readyweapon: int = 1,
    pendingweapon: int = 9,
    ammo: Optional[Sequence[int]] = None,
    weaponowned: Optional[Sequence[int]] = None,
    cmd_forwardmove: int = 0,
    cmd_sidemove: int = 0,
    cmd_angleturn: int = 0,
    cmd_buttons: int = 0,
) -> PlayerRecord:
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
        ammo=list(ammo) if ammo is not None else [50, 0, 0, 0],
        weaponowned=list(weaponowned) if weaponowned is not None else [0, 1, 0, 0, 0, 0, 0, 0, 0],
        cmd_forwardmove=cmd_forwardmove,
        cmd_sidemove=cmd_sidemove,
        cmd_angleturn=cmd_angleturn,
        cmd_buttons=cmd_buttons,
    )


def make_mobj(
    x: int = 0,
    y: int = 0,
    z: int = 0,
    momx: int = 0,
    momy: int = 0,
    momz: int = 0,
    angle: int = 0,
    state: int = 0,
    tics: int = 1,
    health: int = 100,
    flags: int = 0,
    type: int = 0,
) -> MobjPayload:
    return MobjPayload(
        x=x,
        y=y,
        z=z,
        momx=momx,
        momy=momy,
        momz=momz,
        angle=angle,
        state=state,
        tics=tics,
        health=health,
        flags=flags,
        type=type,
    )


def make_thinker(
    trace_id: int,
    func: int = THF_MOBJ,
    mobj: Optional[MobjPayload] = None,
) -> ThinkerRecord:
    if func == THF_MOBJ and mobj is None:
        mobj = make_mobj()
    return ThinkerRecord(trace_id=trace_id, func=func, mobj=mobj)


def make_sector(
    sector_index: int,
    floorheight: int = 0,
    ceilingheight: int = 128 << 16,
) -> SectorRecord:
    return SectorRecord(
        sector_index=sector_index,
        floorheight=floorheight,
        ceilingheight=ceilingheight,
    )


def sectors_digest_from_heights(heights: Sequence[tuple]) -> int:
    """Compute sectors_digest from a list of (floorheight, ceilingheight) pairs."""
    buf = b"".join(
        struct.pack("<ii", fh, ch) for fh, ch in heights
    )
    return fnv1a64(buf)


def make_tic(
    gametic: int,
    in_level: int = 1,
    leveltime: Optional[int] = None,
    rndindex: int = 0,
    prndindex: int = 0,
    players: Optional[List[PlayerRecord]] = None,
    thinkers: Optional[List[ThinkerRecord]] = None,
    sectors: Optional[List[SectorRecord]] = None,
    sectors_digest: Optional[int] = None,
    numsectors: int = 0,
) -> TicRecord:
    """Build one tic record.

    If in_level == 0, forces the non-level rule from TRACE.md §5.
    If sectors_digest is None and in_level != 0, hashes *numsectors* zero-height
    pairs (or derives from the active sector list only when numsectors == 0 and
    no full array is known — then uses FNV of empty / provided value).
    """
    if in_level == 0:
        return TicRecord(
            gametic=gametic,
            in_level=0,
            leveltime=0,
            rndindex=rndindex,
            prndindex=prndindex,
            players=[],
            thinkers=[],
            sectors=[],
            sectors_digest=FNV_OFFSET,
        )
    if leveltime is None:
        leveltime = gametic
    if players is None:
        players = [make_player()]
    if thinkers is None:
        thinkers = [make_thinker(1)]
    if sectors is None:
        sectors = []
    if sectors_digest is None:
        if numsectors > 0:
            sectors_digest = sectors_digest_from_heights(
                [(0, 128 << 16)] * numsectors
            )
        else:
            sectors_digest = FNV_OFFSET
    return TicRecord(
        gametic=gametic,
        in_level=1,
        leveltime=leveltime,
        rndindex=rndindex,
        prndindex=prndindex,
        players=players,
        thinkers=thinkers,
        sectors=sectors,
        sectors_digest=sectors_digest,
    )


def make_default_trace(
    n_tics: int = 3,
    n_players: int = 1,
    n_thinkers: int = 2,
    n_sectors_active: int = 1,
    numsectors: int = 4,
) -> List[TicRecord]:
    """Build a small in-level trace of *n_tics* records with evolving state."""
    records: List[TicRecord] = []
    for i in range(n_tics):
        players = [
            make_player(
                player_index=p,
                mo_trace_id=p + 1,
                x=(i + 1) * 1000 + p,
                y=(i + 1) * 2000,
                momx=i * 10 + p,
                health=100 - i,
            )
            for p in range(n_players)
        ]
        thinkers = []
        for t in range(n_thinkers):
            tid = t + 1
            if t % 2 == 0:
                thinkers.append(
                    make_thinker(
                        tid,
                        THF_MOBJ,
                        make_mobj(x=i * 100 + tid, momx=i, health=100 - i, type=t),
                    )
                )
            else:
                # Non-mobj thinker (e.g. vertical door)
                thinkers.append(make_thinker(tid, func=2, mobj=None))
        sectors = [
            make_sector(s, floorheight=i * (1 << 16), ceilingheight=128 << 16)
            for s in range(n_sectors_active)
        ]
        # Whole-array digest: numsectors pairs; active ones get the floor heights.
        heights = []
        for s in range(numsectors):
            if s < n_sectors_active:
                heights.append((i * (1 << 16), 128 << 16))
            else:
                heights.append((0, 128 << 16))
        records.append(
            make_tic(
                gametic=i,
                in_level=1,
                leveltime=i,
                rndindex=(i * 3) % 256,
                prndindex=(i * 7) % 256,
                players=players,
                thinkers=thinkers,
                sectors=sectors,
                sectors_digest=sectors_digest_from_heights(heights),
            )
        )
    return records


def write_pair(
    out_dir: str,
    stem: str,
    records: Sequence[TicRecord],
    version: int = 1,
) -> tuple:
    """Write out_dir/stem.trc and out_dir/stem.dig. Returns (trc_path, dig_path)."""
    os.makedirs(out_dir, exist_ok=True)
    trc_path = os.path.join(out_dir, f"{stem}.trc")
    dig_path = os.path.join(out_dir, f"{stem}.dig")
    write_trace_pair(trc_path, dig_path, records, version=version)
    return trc_path, dig_path


def main(argv: Optional[Sequence[str]] = None) -> int:
    p = argparse.ArgumentParser(
        description="Build a small synthetic Doom .trc/.dig pair for testing."
    )
    p.add_argument("out_dir", help="Output directory")
    p.add_argument("--stem", default="synth", help="Filename stem (default: synth)")
    p.add_argument("--tics", type=int, default=3, help="Number of tics")
    p.add_argument("--players", type=int, default=1, help="Players per in-level tic")
    p.add_argument("--thinkers", type=int, default=2, help="Thinkers per tic")
    p.add_argument(
        "--active-sectors", type=int, default=1, help="Active sector records per tic"
    )
    p.add_argument(
        "--numsectors", type=int, default=4, help="Total sectors for sectors_digest"
    )
    p.add_argument("--version", type=int, default=1, help="Format version in headers")
    args = p.parse_args(argv)

    records = make_default_trace(
        n_tics=args.tics,
        n_players=args.players,
        n_thinkers=args.thinkers,
        n_sectors_active=args.active_sectors,
        numsectors=args.numsectors,
    )
    trc, dig = write_pair(args.out_dir, args.stem, records, version=args.version)
    print(f"wrote {trc}")
    print(f"wrote {dig}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
