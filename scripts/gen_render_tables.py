#!/usr/bin/env python3
"""Generate Lean renderer lookup tables from oracle C constants."""

from __future__ import annotations

import struct
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "lean" / "Doom" / "Render" / "Tables.lean"

LIGHTLEVELS = 16
MAXLIGHTSCALE = 48
MAXLIGHTZ = 128
NUMCOLORMAPS = 32
FRACBITS = 16
FRACUNIT = 1 << FRACBITS
SCREENWIDTH = 320
DISTMAP = 2
LIGHTSEGSHIFT = 4
LIGHTSCALESHIFT = 12
LIGHTZSHIFT = 20


def fixed_div(a: int, b: int) -> int:
    if b == 0:
        return 0x7FFFFFFF if a >= 0 else -0x80000000
    if ((abs(a) >> 14) >= abs(b)):
        return -0x80000000 if (a ^ b) < 0 else 0x7FFFFFFF
    return (a << FRACBITS) // b


def gen_zlight() -> list[list[int]]:
    rows: list[list[int]] = []
    for i in range(LIGHTLEVELS):
        startmap = ((LIGHTLEVELS - 1 - i) * 2) * NUMCOLORMAPS // LIGHTLEVELS
        row: list[int] = []
        for j in range(MAXLIGHTZ):
            scale = fixed_div((SCREENWIDTH // 2) * FRACUNIT, (j + 1) << LIGHTZSHIFT)
            scale >>= LIGHTSCALESHIFT
            level = startmap - scale // DISTMAP
            if level < 0:
                level = 0
            if level >= NUMCOLORMAPS:
                level = NUMCOLORMAPS - 1
            row.append(level)
        rows.append(row)
    return rows


def gen_scalelight(viewwidth: int = 288, detailshift: int = 0) -> list[list[int]]:
    rows: list[list[int]] = []
    for i in range(LIGHTLEVELS):
        startmap = ((LIGHTLEVELS - 1 - i) * 2) * NUMCOLORMAPS // LIGHTLEVELS
        row: list[int] = []
        for j in range(MAXLIGHTSCALE):
            level = startmap - j * SCREENWIDTH // (viewwidth << detailshift) // DISTMAP
            if level < 0:
                level = 0
            if level >= NUMCOLORMAPS:
                level = NUMCOLORMAPS - 1
            row.append(level)
        rows.append(row)
    return rows


def emit() -> str:
    zlight = gen_zlight()
    scalelight = gen_scalelight()
    lines = [
        "/-!",
        "# Doom.Render.Tables",
        "",
        "Generated lighting map level indices (`r_main.c`, `r_data.c`).",
        "-/",
        "",
        "namespace Doom.Render.Tables",
        "",
        f"def lightLevels : Nat := {LIGHTLEVELS}",
        f"def maxLightScale : Nat := {MAXLIGHTSCALE}",
        f"def maxLightZ : Nat := {MAXLIGHTZ}",
        f"def numColormaps : Nat := {NUMCOLORMAPS}",
        "",
        "/-- `zlight[i][j]` colormap level index. -/",
        "def zlight : Array (Array Nat) := #[",
    ]
    for row in zlight:
        inner = ", ".join(str(x) for x in row)
        lines.append(f"  #[{inner}],")
    lines.append("]")
    lines.append("")
    lines.append("/-- `scalelight[i][j]` for default viewwidth 288. -/")
    lines.append("def scalelight : Array (Array Nat) := #[")
    for row in scalelight:
        inner = ", ".join(str(x) for x in row)
        lines.append(f"  #[{inner}],")
    lines.append("]")
    lines.append("")
    lines.append("end Doom.Render.Tables")
    lines.append("")
    return "\n".join(lines)


def main() -> None:
    OUT.write_text(emit())
    print(f"wrote {OUT}")


if __name__ == "__main__":
    main()
