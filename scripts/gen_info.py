#!/usr/bin/env python3
"""Mechanically generate Lean info tables from oracle info.c / info.h.

Emits:
  lean/Doom/Playsim/Info/Action.lean   (action enum ordinals)
  lean/Doom/Playsim/Info/States.lean   (states array)
  lean/Doom/Playsim/Info/Mobjinfo.lean (mobjinfo array)
  lean/Doom/Playsim/Info/Sprnames.lean (sprnames as 4-byte names)

Also prints FNV-1a64 checksums and entry counts for the Lean test suite.

Deterministic: rerunning must produce a byte-identical no-op diff.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
INFO_C = ROOT / "oracle" / "chocolate-doom" / "src" / "doom" / "info.c"
INFO_H = ROOT / "oracle" / "chocolate-doom" / "src" / "doom" / "info.h"
SOUNDS_H = ROOT / "oracle" / "chocolate-doom" / "src" / "doom" / "sounds.h"
MOBJ_H = ROOT / "oracle" / "chocolate-doom" / "src" / "doom" / "p_mobj.h"
OUT_DIR = ROOT / "lean" / "Doom" / "Playsim" / "Info"

CHUNK = 64

FNV_OFFSET = 0xCBF29CE484222325
FNV_PRIME = 0x100000001B3
FRACUNIT = 65536


def fnv1a64(data: bytes) -> int:
    h = FNV_OFFSET
    for b in data:
        h = ((h ^ b) * FNV_PRIME) & 0xFFFFFFFFFFFFFFFF
    return h


def le_i32(v: int) -> bytes:
    return (v & 0xFFFFFFFF).to_bytes(4, "little")


def write_if_changed(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.exists() and path.read_text() == content:
        return
    path.write_text(content)


def parse_named_enum(src: str, first: str, sentinel: str) -> list[str]:
    m = re.search(
        rf"typedef enum\s*\{{[\s\S]*?({re.escape(first)}[\s\S]*?){re.escape(sentinel)}",
        src,
    )
    if m is None:
        raise SystemExit(f"enum {first}..{sentinel} not found")
    return re.findall(rf"{first[:2]}_[A-Za-z0-9_]+|{re.escape(first)}", m.group(1)) \
        if False else re.findall(r"[A-Za-z_][A-Za-z0-9_]*", m.group(1))


def parse_spr_enum(info_h: str) -> list[str]:
    m = re.search(r"typedef enum\s*\{\s*(SPR_TROO[\s\S]*?)NUMSPRITES", info_h)
    if m is None:
        raise SystemExit("spritenum_t not found")
    return re.findall(r"SPR_[A-Z0-9]+", m.group(1))


def parse_state_enum(info_h: str) -> list[str]:
    m = re.search(r"typedef enum\s*\{\s*(S_NULL[\s\S]*?)NUMSTATES", info_h)
    if m is None:
        raise SystemExit("statenum_t not found")
    return re.findall(r"S_[A-Z0-9_]+", m.group(1))


def parse_mobjtype_enum(info_h: str) -> list[str]:
    m = re.search(r"typedef enum\s*\{\s*(MT_PLAYER[\s\S]*?)NUMMOBJTYPES", info_h)
    if m is None:
        raise SystemExit("mobjtype_t not found")
    return re.findall(r"MT_[A-Z0-9_]+", m.group(1))


def parse_sfx_enum(sounds_h: str) -> list[str]:
    m = re.search(r"typedef enum\s*\{\s*(sfx_None[\s\S]*?)NUMSFX", sounds_h)
    if m is None:
        raise SystemExit("sfxenum_t not found")
    return re.findall(r"sfx_[A-Za-z0-9_]+", m.group(1))


def parse_mf_flags(mobj_h: str) -> dict[str, int]:
    out: dict[str, int] = {}
    for name, raw in re.findall(r"(MF_[A-Z0-9_]+)\s*=\s*([^,\n]+)", mobj_h):
        raw = raw.strip().rstrip(",")
        if raw.startswith("0x") or raw.startswith("0X"):
            out[name] = int(raw, 16)
        else:
            out[name] = int(raw, 0)
    return out


def parse_actions(info_c: str) -> list[str]:
    acts = ["NULL"]
    seen = {"NULL"}
    for a in re.findall(r"void\s+(A_[A-Za-z0-9_]+)\s*\(", info_c):
        if a not in seen:
            seen.add(a)
            acts.append(a)
    return acts


def parse_sprnames(info_c: str) -> list[str]:
    m = re.search(r"const\s+char\s*\*\s*sprnames\[\]\s*=\s*\{(.*?)\};", info_c, re.S)
    if m is None:
        # chocolate-doom may use char *sprnames[] without const
        m = re.search(r"char\s*\*\s*sprnames\[\]\s*=\s*\{(.*?)\};", info_c, re.S)
    if m is None:
        raise SystemExit("sprnames not found")
    names = re.findall(r'"([^"]*)"', m.group(1))
    # trailing NULL pointer is not a string; stop before empty if any
    return [n for n in names if n]


def parse_states(info_c: str) -> list[tuple[str, int, int, str, str, int, int]]:
    m = re.search(r"state_t\s+states\[NUMSTATES\]\s*=\s*\{(.*)\n\};", info_c, re.S)
    if m is None:
        raise SystemExit("states[] not found")
    body = m.group(1)
    pat = re.compile(
        r"\{(SPR_[A-Z0-9]+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*,\s*"
        r"\{(NULL|A_[A-Za-z0-9]+)\}\s*,\s*(S_[A-Z0-9_]+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\}"
    )
    ents = pat.findall(body)
    return [
        (spr, int(frame), int(tics), act, nxt, int(m1), int(m2))
        for spr, frame, tics, act, nxt, m1, m2 in ents
    ]


def split_top_structs(body: str) -> list[str]:
    body = re.sub(r"//.*?$", "", body, flags=re.M)
    objs: list[str] = []
    i = 0
    while i < len(body):
        if body[i] == "{":
            depth = 0
            j = i
            while j < len(body):
                if body[j] == "{":
                    depth += 1
                elif body[j] == "}":
                    depth -= 1
                    if depth == 0:
                        objs.append(body[i + 1 : j])
                        i = j + 1
                        break
                j += 1
            else:
                break
        else:
            i += 1
    return objs


def eval_expr(expr: str, env: dict[str, int]) -> int:
    expr = expr.strip()
    if not expr:
        raise SystemExit("empty expr")
    # tokenize identifiers / numbers / ops
    tokens = re.findall(r"[A-Za-z_][A-Za-z0-9_]*|0[xX][0-9a-fA-F]+|-?\d+|[|*+-]", expr)
    if not tokens:
        raise SystemExit(f"bad expr: {expr!r}")

    def atom(tok: str) -> int:
        if tok in env:
            return env[tok]
        if tok.startswith(("0x", "0X")):
            return int(tok, 16)
        return int(tok, 10)

    # replace idents with values, evaluate | and * and unary -
    # shunting-yard light: support unary -, *, +, |, left-assoc
    vals: list[int] = []
    ops: list[str] = []
    prec = {"|": 1, "+": 2, "-": 2, "*": 3}

    def apply() -> None:
        op = ops.pop()
        if op == "u-":
            vals.append((-vals.pop()) & 0xFFFFFFFF if False else -vals.pop())
            return
        b = vals.pop()
        a = vals.pop()
        if op == "|":
            vals.append(a | b)
        elif op == "+":
            vals.append(a + b)
        elif op == "-":
            vals.append(a - b)
        elif op == "*":
            vals.append(a * b)
        else:
            raise SystemExit(f"unknown op {op}")

    expect_atom = True
    for tok in tokens:
        if expect_atom:
            if tok == "-":
                ops.append("u-")
            else:
                vals.append(atom(tok))
                expect_atom = False
        else:
            if tok not in prec:
                raise SystemExit(f"expected op in {expr!r}, got {tok}")
            while ops and ops[-1] != "u-" and prec.get(ops[-1], 0) >= prec[tok]:
                apply()
            while ops and ops[-1] == "u-":
                apply()
            ops.append(tok)
            expect_atom = True
    while ops:
        apply()
    if len(vals) != 1:
        raise SystemExit(f"eval failed for {expr!r}")
    return vals[0]


def parse_mobjinfo(info_c: str, env: dict[str, int]) -> list[list[int]]:
    m = re.search(r"mobjinfo_t\s+mobjinfo\[NUMMOBJTYPES\]\s*=\s*\{(.*)\n\};", info_c, re.S)
    if m is None:
        raise SystemExit("mobjinfo[] not found")
    objs = split_top_structs(m.group(1))
    rows: list[list[int]] = []
    for obj in objs:
        fields = [f.strip() for f in obj.split(",") if f.strip()]
        if len(fields) != 23:
            raise SystemExit(f"expected 23 mobjinfo fields, got {len(fields)}: {fields[:3]}")
        rows.append([eval_expr(f, env) for f in fields])
    return rows


def emit_action_module(actions: list[str]) -> str:
    lines = [
        "/-! Action function ordinals from oracle info.c forward decls.",
        "GENERATED by scripts/gen_info.py — do not edit by hand. -/",
        "",
        "namespace Doom.Playsim.Info",
        "",
        "/-- Stable action id: 0 = NULL, then declaration order of `A_*` in info.c. -/",
        "abbrev ActionId := UInt32",
        "",
    ]
    for i, name in enumerate(actions):
        lean_name = "actionNull" if name == "NULL" else "action" + name[2:]  # A_Look -> actionLook
        if name == "NULL":
            lean_name = "actionNull"
        else:
            lean_name = "action_" + name  # keep A_ prefix uniqueness
        lines.append(f"def {lean_name} : ActionId := {i}")
    lines += ["", f"def NUM_ACTIONS : Nat := {len(actions)}", "", "end Doom.Playsim.Info", ""]
    return "\n".join(lines)


def emit_states_module(
    states: list[tuple[int, int, int, int, int, int, int]],
) -> str:
    # StateRecord as flat parallel arrays to keep elaboration light, or one structure array.
    lines = [
        "/-! `states[]` from oracle info.c.",
        "GENERATED by scripts/gen_info.py — do not edit by hand. -/",
        "",
        "namespace Doom.Playsim.Info",
        "",
        "structure State where",
        "  sprite : UInt32",
        "  frame : UInt32",
        "  tics : Int32",
        "  action : UInt32",
        "  nextstate : UInt32",
        "  misc1 : Int32",
        "  misc2 : Int32",
        "  deriving Repr",
        "",
    ]
    chunks = [states[i : i + CHUNK] for i in range(0, len(states), CHUNK)]
    for ci, chunk in enumerate(chunks):
        lines.append(f"private def statesChunk{ci} : Array State := #[")
        for ri, (spr, frame, tics, act, nxt, m1, m2) in enumerate(chunk):
            comma = "," if ri + 1 < len(chunk) else ""
            lines.append(
                "  { "
                f"sprite := {spr}, frame := {frame}, tics := {tics}, "
                f"action := {act}, nextstate := {nxt}, misc1 := {m1}, misc2 := {m2}"
                f" }}{comma}"
            )
        lines.append("]")
        lines.append("")
    concat = " ++ ".join(f"statesChunk{ci}" for ci in range(len(chunks)))
    lines.append(f"/-- {len(states)}-entry `states[]`, bit-identical field layout to oracle. -/")
    lines.append(f"def states : Array State := {concat}")
    lines.append("")
    lines.append(f"def NUMSTATES : Nat := {len(states)}")
    lines.append("")
    lines.append("end Doom.Playsim.Info")
    lines.append("")
    return "\n".join(lines)


def emit_mobjinfo_module(rows: list[list[int]]) -> str:
    lines = [
        "/-! `mobjinfo[]` from oracle info.c.",
        "GENERATED by scripts/gen_info.py — do not edit by hand. -/",
        "",
        "namespace Doom.Playsim.Info",
        "",
        "structure MobjInfo where",
        "  doomednum : Int32",
        "  spawnstate : UInt32",
        "  spawnhealth : Int32",
        "  seestate : UInt32",
        "  seesound : Int32",
        "  reactiontime : Int32",
        "  attacksound : Int32",
        "  painstate : UInt32",
        "  painchance : Int32",
        "  painsound : Int32",
        "  meleestate : UInt32",
        "  missilestate : UInt32",
        "  deathstate : UInt32",
        "  xdeathstate : UInt32",
        "  deathsound : Int32",
        "  speed : Int32",
        "  radius : Int32",
        "  height : Int32",
        "  mass : Int32",
        "  damage : Int32",
        "  activesound : Int32",
        "  flags : UInt32",
        "  raisestate : UInt32",
        "  deriving Repr",
        "",
    ]
    field_names = [
        "doomednum", "spawnstate", "spawnhealth", "seestate", "seesound",
        "reactiontime", "attacksound", "painstate", "painchance", "painsound",
        "meleestate", "missilestate", "deathstate", "xdeathstate", "deathsound",
        "speed", "radius", "height", "mass", "damage", "activesound", "flags",
        "raisestate",
    ]
    # signed vs unsigned fields
    unsigned = {
        "spawnstate", "seestate", "painstate", "meleestate", "missilestate",
        "deathstate", "xdeathstate", "flags", "raisestate",
    }
    chunks = [rows[i : i + CHUNK] for i in range(0, len(rows), CHUNK)]
    for ci, chunk in enumerate(chunks):
        lines.append(f"private def mobjinfoChunk{ci} : Array MobjInfo := #[")
        for ri, row in enumerate(chunk):
            parts = []
            for name, val in zip(field_names, row):
                if name in unsigned:
                    parts.append(f"{name} := {val & 0xFFFFFFFF}")
                else:
                    # emit as signed decimal for Int32
                    if val >= 2**31:
                        val = val - 2**32
                    parts.append(f"{name} := {val}")
            comma = "," if ri + 1 < len(chunk) else ""
            lines.append("  { " + ", ".join(parts) + f" }}{comma}")
        lines.append("]")
        lines.append("")
    concat = " ++ ".join(f"mobjinfoChunk{ci}" for ci in range(len(chunks)))
    lines.append(f"/-- {len(rows)}-entry `mobjinfo[]`. -/")
    lines.append(f"def mobjinfo : Array MobjInfo := {concat}")
    lines.append("")
    lines.append(f"def NUMMOBJTYPES : Nat := {len(rows)}")
    lines.append("")
    lines.append("end Doom.Playsim.Info")
    lines.append("")
    return "\n".join(lines)


def emit_sprnames_module(names: list[str]) -> str:
    lines = [
        "/-! `sprnames[]` from oracle info.c (4-char sprite names).",
        "GENERATED by scripts/gen_info.py — do not edit by hand. -/",
        "",
        "namespace Doom.Playsim.Info",
        "",
        "/-- Pack four ASCII chars into a UInt32 (little-endian byte order). -/",
        "def packSprName (a b c d : UInt8) : UInt32 :=",
        "  a.toUInt32 ||| (b.toUInt32 <<< 8) ||| (c.toUInt32 <<< 16) ||| (d.toUInt32 <<< 24)",
        "",
        "def sprnames : Array UInt32 := #[",
    ]
    for i, name in enumerate(names):
        raw = (name + "\0\0\0\0")[:4]
        bs = [ord(c) for c in raw]
        comma = "," if i + 1 < len(names) else ""
        lines.append(
            f"  packSprName {bs[0]} {bs[1]} {bs[2]} {bs[3]}{comma}  -- {name!r}"
        )
    lines += [
        "]",
        "",
        f"def NUMSPRITES : Nat := {len(names)}",
        "",
        "end Doom.Playsim.Info",
        "",
    ]
    return "\n".join(lines)


def emit_info_root() -> str:
    return "\n".join(
        [
            "import Doom.Playsim.Info.Action",
            "import Doom.Playsim.Info.States",
            "import Doom.Playsim.Info.Mobjinfo",
            "import Doom.Playsim.Info.Sprnames",
            "",
            "/-!",
            "# Doom.Playsim.Info",
            "",
            "Generated Doom `states` / `mobjinfo` / `sprnames` tables.",
            "-/",
            "",
            "namespace Doom.Playsim.Info",
            "end Doom.Playsim.Info",
            "",
        ]
    )


def main() -> None:
    info_c = INFO_C.read_text()
    info_h = INFO_H.read_text()
    sounds_h = SOUNDS_H.read_text()
    mobj_h = MOBJ_H.read_text()

    spr_enum = parse_spr_enum(info_h)
    state_enum = parse_state_enum(info_h)
    mobj_enum = parse_mobjtype_enum(info_h)
    sfx_enum = parse_sfx_enum(sounds_h)
    mf = parse_mf_flags(mobj_h)
    actions = parse_actions(info_c)
    sprnames = parse_sprnames(info_c)
    raw_states = parse_states(info_c)

    if len(sprnames) != len(spr_enum):
        raise SystemExit(f"sprnames {len(sprnames)} != SPR enum {len(spr_enum)}")
    if len(raw_states) != len(state_enum):
        raise SystemExit(f"states {len(raw_states)} != S_ enum {len(state_enum)}")

    spr_ix = {n: i for i, n in enumerate(spr_enum)}
    state_ix = {n: i for i, n in enumerate(state_enum)}
    action_ix = {n: i for i, n in enumerate(actions)}
    sfx_ix = {n: i for i, n in enumerate(sfx_enum)}

    env: dict[str, int] = {"FRACUNIT": FRACUNIT}
    env.update(spr_ix)
    env.update(state_ix)
    env.update(sfx_ix)
    env.update(mf)
    # meleestate sometimes written as bare 0 meaning S_NULL
    env["S_NULL"] = state_ix["S_NULL"]

    states = []
    for spr, frame, tics, act, nxt, m1, m2 in raw_states:
        states.append(
            (
                spr_ix[spr],
                frame & 0xFFFFFFFF,
                tics,
                action_ix[act],
                state_ix[nxt],
                m1,
                m2,
            )
        )

    mobjinfo = parse_mobjinfo(info_c, env)
    if len(mobjinfo) != len(mobj_enum):
        raise SystemExit(f"mobjinfo {len(mobjinfo)} != MT_ enum {len(mobj_enum)}")

    write_if_changed(OUT_DIR / "Action.lean", emit_action_module(actions))
    write_if_changed(OUT_DIR / "States.lean", emit_states_module(states))
    write_if_changed(OUT_DIR / "Mobjinfo.lean", emit_mobjinfo_module(mobjinfo))
    write_if_changed(OUT_DIR / "Sprnames.lean", emit_sprnames_module(sprnames))
    write_if_changed(ROOT / "lean" / "Doom" / "Playsim" / "Info.lean", emit_info_root())

    # checksums: states as packed i32 fields; mobjinfo similarly; sprnames as 4 bytes each
    st_bytes = bytearray()
    for spr, frame, tics, act, nxt, m1, m2 in states:
        for v in (spr, frame, tics, act, nxt, m1, m2):
            st_bytes += le_i32(v)
    mi_bytes = bytearray()
    for row in mobjinfo:
        for v in row:
            mi_bytes += le_i32(v)
    sn_bytes = bytearray()
    for name in sprnames:
        raw = (name + "\0\0\0\0")[:4]
        sn_bytes += raw.encode("ascii")

    print(f"count(states) = {len(states)}")
    print(f"count(mobjinfo) = {len(mobjinfo)}")
    print(f"count(sprnames) = {len(sprnames)}")
    print(f"count(actions) = {len(actions)}")
    print(f"fnv1a64(states) = 0x{fnv1a64(bytes(st_bytes)):016x}")
    print(f"fnv1a64(mobjinfo) = 0x{fnv1a64(bytes(mi_bytes)):016x}")
    print(f"fnv1a64(sprnames) = 0x{fnv1a64(bytes(sn_bytes)):016x}")


if __name__ == "__main__":
    main()
