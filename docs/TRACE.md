# Doom Trace Format, version 1 (normative)

This document is the single source of truth for the trace format used to verify the Lean 4
port of Doom against the C reference oracle (patched Chocolate Doom). Every producer (the
oracle, the Lean port, test stubs) and every consumer (`tools/tracediff`, CI gates) MUST
implement exactly what is written here. Any change to this document is a format version
bump.

The words MUST / MUST NOT are normative.

---

## 1. Dump point

A producer emits exactly one tic record per gametic, **immediately after `G_Ticker`
returns**, once per iteration of the game loop's tic-advancement, before any rendering,
sound, or I/O for that tic.

In the oracle this is in `src/doom/d_net.c`, function `D_RunTic`, on the line immediately
following the `G_Ticker()` call. A future Lean implementation MUST dump at the semantically
identical point: after all of the per-tic game logic for gametic *G* has run (player think,
thinker list run, specials, respawn queue — i.e. everything `P_Ticker` and the rest of
`G_Ticker` does) and before gametic *G+1* begins.

The first record of a run corresponds to the first `G_Ticker` call of the process. With
`-tracedemo` (see §8) the demo lump is opened inside that first `G_Ticker` call, so record 0
is already in-level.

## 2. Artefacts

Each run produces two files:

- **Full trace** (`.trc`) — every field of every tic record, encoded as in §4–§6. Used only
  for diagnosis.
- **Digest stream** (`.dig`) — one 64-bit FNV-1a hash per tic, computed over the canonical
  byte encoding of that tic's record (§7). This is what the differ compares.

## 3. Canonical encoding rules

- All integers are **little-endian**, fixed width, no padding, no alignment, packed
  back-to-back.
- Unless stated otherwise every field is 32 bits. Source values narrower than 32 bits are
  widened to 32 bits before encoding: signed values (e.g. `ticcmd_t.forwardmove`, a signed
  char) are **sign-extended**; unsigned values (e.g. `ticcmd_t.buttons`, an unsigned char)
  are **zero-extended**.
- Every value is serialized as its raw two's-complement / unsigned bit pattern. Never as a
  decimal string, never as a float, never through any lossy conversion. `fixed_t` and
  `angle_t` values are emitted as their raw 32-bit patterns.
- Field order is fixed and is exactly the order given in this document. Adding, removing, or
  reordering a field is a format version bump.
- No pointers, no addresses, no timestamps, no wall-clock values, no iteration over any
  hash-table or otherwise unordered container.

Notation below: `u32` = unsigned 32-bit LE, `i32` = signed (two's-complement) 32-bit LE,
`u64` = unsigned 64-bit LE.

## 4. File headers

### 4.1 Full trace file

| offset | type | value |
|---|---|---|
| 0 | 4 bytes | magic: ASCII `D T R C` (bytes `44 54 52 43`) |
| 4 | u32 | format version = `1` |
| 8 | u32 | flags = `0` (reserved, MUST be 0) |
| 12 | u32 | reserved = `0` |

The header is followed by zero or more tic records (§5) until end of file. Records are
variable-length and self-delimiting via their count fields; there is no per-record length
prefix and no trailer.

### 4.2 Digest stream file

| offset | type | value |
|---|---|---|
| 0 | 4 bytes | magic: ASCII `D D I G` (bytes `44 44 49 47`) |
| 4 | u32 | format version = `1` |
| 8 | u32 | flags = `0` (reserved, MUST be 0) |
| 12 | u32 | reserved = `0` |

The header is followed by one `u64` per tic record, in tic order, until end of file. The
*k*-th u64 (0-based) is the digest (§7) of the *k*-th tic record of the corresponding full
trace. A producer MUST emit both files from the same run; digest *k* MUST equal the FNV-1a64
of the exact bytes of record *k* as they appear in the full trace.

## 5. Tic record layout

Every tic record has the following layout, in this exact order:

```
u32  gametic
u32  in_level            ; 1 if gamestate == GS_LEVEL at the dump point, else 0
u32  leveltime
u32  rndindex            ; index of the M_Random table
u32  prndindex           ; index of the P_Random table
u32  player_count
     player_record * player_count          (§5.1)
u32  thinker_count
     thinker_record * thinker_count        (§5.2)
u32  active_sector_count
     sector_record * active_sector_count   (§5.3)
u64  sectors_digest                        (§5.4)
```

**Non-level tics.** If `in_level == 0` (intermission, finale, demo screen), the record MUST
be emitted with: `leveltime = 0`, `player_count = 0`, `thinker_count = 0`,
`active_sector_count = 0`, and `sectors_digest = 0xCBF29CE484222325` (the FNV-1a64 hash of
the empty byte string). `gametic`, `rndindex`, and `prndindex` are emitted with their real
values. No player, thinker, or sector state may be read outside a level.

### 5.1 Player record

One record per player with `playeringame[i]` true, in ascending player index order.

| # | type | field | source |
|---|---|---|---|
| 1 | u32 | player_index | `i` (0–3) |
| 2 | u32 | mo_trace_id | trace_id of `player->mo` (§6.1); `0xFFFFFFFF` if `mo == NULL` |
| 3 | i32 | x | `player->mo->x` (fixed_t; 0 if `mo == NULL`, same for 4–9) |
| 4 | i32 | y | `player->mo->y` |
| 5 | i32 | z | `player->mo->z` |
| 6 | i32 | momx | `player->mo->momx` |
| 7 | i32 | momy | `player->mo->momy` |
| 8 | i32 | momz | `player->mo->momz` |
| 9 | u32 | angle | `player->mo->angle` (angle_t) |
| 10 | i32 | viewz | `player->viewz` (fixed_t) |
| 11 | i32 | health | `player->health` |
| 12 | i32 | armorpoints | `player->armorpoints` |
| 13 | i32 | readyweapon | `player->readyweapon` (enum ordinal) |
| 14 | i32 | pendingweapon | `player->pendingweapon` (enum ordinal; vanilla uses `wp_nochange` = 9) |
| 15–18 | i32 ×4 | ammo[0..3] | `player->ammo[a]`, a = 0..NUMAMMO-1 (NUMAMMO = 4), ascending |
| 19–27 | i32 ×9 | weaponowned[0..8] | `player->weaponowned[w]`, w = 0..NUMWEAPONS-1 (NUMWEAPONS = 9), ascending |
| 28 | i32 | cmd_forwardmove | `player->cmd.forwardmove` (signed char, sign-extended) |
| 29 | i32 | cmd_sidemove | `player->cmd.sidemove` (signed char, sign-extended) |
| 30 | i32 | cmd_angleturn | `player->cmd.angleturn` (signed short, sign-extended) |
| 31 | u32 | cmd_buttons | `player->cmd.buttons` (unsigned char, zero-extended) |

`player_count` is the number of records emitted (the count of in-game players), not
`MAXPLAYERS`.

### 5.2 Thinker record

One record per thinker in **linked-list order**, walking `thinkercap.next` until reaching
`&thinkercap` again, at the dump point. All thinkers on the list are emitted, including
those marked for removal.

| # | type | field |
|---|---|---|
| 1 | u32 | trace_id (§6.1) |
| 2 | u32 | func (§6.2) |

If and only if `func == 1` (`THF_MOBJ`), the record continues with the mobj payload:

| # | type | field | source |
|---|---|---|---|
| 3 | i32 | x | `mobj->x` (fixed_t) |
| 4 | i32 | y | `mobj->y` |
| 5 | i32 | z | `mobj->z` |
| 6 | i32 | momx | `mobj->momx` |
| 7 | i32 | momy | `mobj->momy` |
| 8 | i32 | momz | `mobj->momz` |
| 9 | u32 | angle | `mobj->angle` (angle_t) |
| 10 | i32 | state | index of `mobj->state` in the global `states[]` array (i.e. `mobj->state - states`) |
| 11 | i32 | tics | `mobj->tics` |
| 12 | i32 | health | `mobj->health` |
| 13 | u32 | flags | `mobj->flags` |
| 14 | i32 | type | `mobj->type` (mobjtype_t ordinal) |

Thinkers with any other `func` value carry no payload; their observable effects are captured
by the sector section and by subsequent mobj/player state.

### 5.3 Sector record

One record per sector whose `specialdata` pointer is non-NULL at the dump point (i.e. a
mover — door, plat, floor, ceiling — is attached), in **ascending sector index order**
(index into the level's `sectors[]` array):

| # | type | field |
|---|---|---|
| 1 | u32 | sector_index |
| 2 | i32 | floorheight (fixed_t) |
| 3 | i32 | ceilingheight (fixed_t) |

### 5.4 Whole-sector-array digest

`sectors_digest` is the FNV-1a64 (§7) of the byte string formed by concatenating, for every
sector `s` in ascending index order `0 .. numsectors-1`:

```
i32 sectors[s].floorheight
i32 sectors[s].ceilingheight
```

(8 bytes per sector, little-endian, no separators). For a non-level tic it is the hash of
the empty string, `0xCBF29CE484222325`.

## 6. Identity rules

### 6.1 trace_id

Every thinker is assigned a `u32 trace_id` from a global counter at the moment it is added
to the thinker list (`P_AddThinker` in the oracle). The counter starts at `1` and is
**reset to 1 at level load** (`P_SetupLevel`), before any level thinkers are spawned.
`trace_id` 0 is never assigned; `0xFFFFFFFF` is reserved as "no object". IDs are never
reused within a level and never reassigned. A player's `mo_trace_id` is the trace_id its
map object received when spawned.

Because vanilla Doom spawns level objects in a deterministic order, two correct
implementations assign identical trace_ids. The differ uses trace_id to distinguish
*ordering* divergence (same ids, different list positions) from *content* divergence (same
id, different fields) from *set* divergence (id present on one side only).

### 6.2 Thinker function enum (THF)

Thinker function identity is emitted as a stable enum, never as a pointer:

| value | name | vanilla function |
|---|---|---|
| 0 | THF_REMOVED | `(actionf_t)(-1)` — thinker awaiting removal by `P_RunThinkers` |
| 1 | THF_MOBJ | `P_MobjThinker` |
| 2 | THF_VERTICALDOOR | `T_VerticalDoor` |
| 3 | THF_MOVECEILING | `T_MoveCeiling` |
| 4 | THF_MOVEFLOOR | `T_MoveFloor` |
| 5 | THF_PLATRAISE | `T_PlatRaise` |
| 6 | THF_LIGHTFLASH | `T_LightFlash` |
| 7 | THF_STROBEFLASH | `T_StrobeFlash` |
| 8 | THF_GLOW | `T_Glow` |
| 9 | THF_FIREFLICKER | `T_FireFlicker` |
| 10 | THF_NULL | NULL function — thinker in stasis (plats/ceilings) |

A producer encountering a thinker function not in this table MUST abort with an error exit
code. It MUST NOT emit a placeholder value.

## 7. Digest algorithm

FNV-1a, 64-bit:

```
hash = 0xCBF29CE484222325            ; offset basis
for each byte b of the input, in order:
    hash = hash XOR b
    hash = (hash * 0x00000100000001B3) mod 2^64
```

The per-tic digest input is the complete canonical byte encoding of the tic record, from the
first byte of `gametic` through the last byte of `sectors_digest` inclusive, exactly as
written to the full trace. All digest arithmetic is unsigned, wrapping mod 2^64.

## 8. Oracle invocation and exit codes (informative for other producers, normative for the oracle)

```
chocolate-doom [-iwad <wad>] -tracedemo <DEMOn | file.lmp> -trace <out.trc> -digest <out.dig>
               [-maxtics <N>] [-fbtics <t1,t2,...>] [-fbdir <dir>] [-nodraw]
```

- `-tracedemo X`: if `X` is a path to an existing file, it is loaded as an external demo and
  played; otherwise `X` MUST name a demo lump in the loaded WADs (e.g. `DEMO1`). Playback is
  single-demo: the process exits at demo end instead of looping.
- `-maxtics N`: stop after emitting N tic records, flush, exit.
- `-fbtics` / `-fbdir`: see §9.
- The oracle forces the SDL dummy video driver and no sound/music. It MUST abort with a
  non-zero exit code if the dummy video driver is not active after init — it never silently
  falls back to a real display.

Exit codes:

| code | meaning |
|---|---|
| 0 | demo ended normally, or `-maxtics` cap reached; all output flushed |
| 2 | trace-harness error (bad arguments, unknown thinker function, I/O failure) |
| 255 | engine error (`I_Error`), e.g. demo desync or missing lump |

## 9. Framebuffer dumps (renderer gate; separate from the playsim trace)

When `-fbtics t1,t2,...` is given, at the **end of the display frame** for each listed
gametic the oracle writes `<fbdir>/fb_<gametic>.ppm`: a binary PPM (`P6`, width 320, height
200, maxval 255) where each pixel is the 8-bit framebuffer index mapped through the active
palette. The palette used is the raw 768-byte PLAYPAL data most recently passed to
`I_SetPalette`, **before** gamma correction, so the output is independent of user config.
Alongside each PPM, `<fbdir>/fb_<gametic>.fnv` contains the lowercase-hex FNV-1a64 of the
concatenation: 64000 framebuffer index bytes (row-major, top-left first) followed by the 768
raw palette bytes, and a trailing newline.

Framebuffer comparison is a separate gate (`tools/fbcheck`) with its own exit code; it is
never folded into the playsim digest.

## 10. Conformance checklist for a new producer

1. Header magic/version/flags exactly as §4.
2. Dump point exactly as §1.
3. Field order and widths exactly as §5; counts precede each variable-length section.
4. Sign/zero extension rules as §3.
5. trace_id assignment as §6.1; THF enum as §6.2; abort on unknown function.
6. Digest = FNV-1a64 of the record bytes as §7; digest file entry k matches record k.
7. Non-level tic rule as §5.
8. Nothing non-deterministic anywhere in either file.
