import Doom.Harness.TraceFormat
import Doom.Harness.TraceReader

/-!
P2c-iii behavior lock: DEMO1 A_Look wake + A_Chase open subset.

E2E contract (public surface `verify --impl real --tics 62`):
- candidate digests 0..59 must match fixtures/demo1.dig (no "TIC N" field
  mismatch for N < 60).
- First tracediff field divergence exactly at tic 60; player-path only
  (player XY/mom/armor + player mobj thinker[1]; armor bonus tid 204 retained
  because slide/pickup are next chunk). No monster field mismatches
  (tid 155/156/157).
- Monsters + RNG still match fixture at tic 60 (and troop 156 wake @61).
- Fixture goldens (tracelib on fixtures/demo1.trc):
  - tid=157 @47: x/y/angle/state = (-6667456,-2473152,0xa0000000,209)
  - steps @47/50 diagonal -376000 each axis; @53/56/59 cardinal west -524288
  - angle 0xa0000000→0x80000000 @56
  - prnd 137@47, 141@53, 146@60; rnd Δ=+2 @47 (ST + seesound pitch)
  - troop 155 wakes @60 (442→444); troop 156 @61
-/

open Doom.Harness.TraceFormat
open Doom.Harness.TraceReader

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" => pure (cwd.parent.getD cwd)
  | _ => pure cwd

/-- Hard-coded from fixture extraction (see module docstring). -/
def expectedRndindex : Array UInt32 := #[1, 2, 67, 68, 69, 70]

/-- Fixture goldens: `(tic, x, y, momx, momy)` for DEMO1 player 0, tics 6..26. -/
def playerXyGoldens : Array (Nat × Int32 × Int32 × Int32 × Int32) := #[
  (6, (-14680061 : Int32), -40902656, 2, -7424),
  (7, (-14680059 : Int32), -40910080, 1, -6728),
  (8, (-14680058 : Int32), -40916808, 0, -6098),
  (9, (-14680058 : Int32), -40922906, 0, -5527),
  (10, (-14680098 : Int32), -40826035, -37, 87789),
  (11, (-14680175 : Int32), -40635848, -70, 172356),
  (12, (-14680285 : Int32), -40361094, -100, 248995),
  (13, (-14680425 : Int32), -40009701, -127, 318449),
  (14, (-14680592 : Int32), -39588854, -152, 381392),
  (15, (-14683296 : Int32), -39105095, -2451, 438406),
  (16, (-14688299 : Int32), -38564322, -4534, 490075),
  (17, (-14700405 : Int32), -37972129, -10972, 536674),
  (18, (-14718949 : Int32), -37333337, -16806, 578905),
  (19, (-14743327 : Int32), -36652314, -22093, 617177),
  (20, (-14772992 : Int32), -35933019, -26884, 651861),
  (21, (-14807448 : Int32), -35179040, -31226, 683293),
  (22, (-14846246 : Int32), -34393629, -35161, 711778),
  (23, (-14888979 : Int32), -33579733, -38727, 737593),
  (24, (-14935278 : Int32), -32740022, -41959, 760988),
  (25, (-14984809 : Int32), -31876916, -44888, 782189),
  (26, (-15037269 : Int32), -30992609, -47542, 801403)
]

/-- Fixture goldens: `(tic, z, momz, viewz)` for DEMO1 player 0, tics 27..46. -/
def playerZGoldens : Array (Nat × Int32 × Int32 × Int32) := #[
  (27, (0 : Int32), -131072, 3114808),
  (28, -131072, -196608, 2686976),
  (29, -327680, -262144, 2555904),
  (30, -589824, -327680, 2359296),
  (31, -917504, -393216, 2097152),
  (32, -1048576, 0, 1769472),
  (33, -1048576, 0, 1218856),
  (34, -1048576, 0, 1142304),
  (35, -1048576, 0, 1114184),
  (36, -1048576, 0, 1137216),
  (37, -1048576, 0, 1209176),
  (38, -1048576, 0, 1323024),
  (39, -1048576, 0, 1467656),
  (40, -1048576, 0, 1628952),
  (41, -1048576, 0, 1791168),
  (42, -1048576, 0, 1938472),
  (43, -1048576, 0, 2056496),
  (44, -1048576, 0, 2133704),
  (45, 0, 0, 2162576),
  (46, 0, 0, 2271352)
]

/-- tid=157 wake/chase goldens: `(tic, x, y, angle, state)`. -/
def troop157Goldens : Array (Nat × Int32 × Int32 × UInt32 × UInt32) := #[
  (47, (-6667456 : Int32), -2473152, 0xa0000000, 209),
  (50, (-7043456 : Int32), -2849152, 0xa0000000, 210),
  (53, (-7567744 : Int32), -2849152, 0xa0000000, 211),
  (56, (-8092032 : Int32), -2849152, 0x80000000, 212),
  (59, (-8616320 : Int32), -2849152, 0x80000000, 213)
]

/-- Parse first divergence tic number from tracediff output, if any. -/
def firstMismatchTic (out : String) : Option Nat :=
  let key := "AT TIC "
  let parts := out.splitOn key
  if parts.length < 2 then
    none
  else
    match parts[1]? with
    | none => none
    | some after =>
      let numStr := after.takeWhile (fun c => c.isDigit)
      numStr.toNat?

/-- True when tracediff field list names a chasing monster (not player/armor). -/
def mentionsMonsterChase (out : String) : Bool :=
  out.contains "trace_id=155" || out.contains "trace_id=156" ||
    out.contains "trace_id=157"

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let root ← defaultRoot
  let verifyOut := root / ".agent_tmp" / "digest_p2c_verify"
  IO.FS.createDirAll verifyOut
  let v ← IO.Process.output {
    cmd := "lake"
    args := #[
      "exe", "verify", "--",
      "--iwad", (root / "fixtures" / "wads" / "doom1.wad").toString,
      "--demo", "DEMO1",
      "--ref-digest", (root / "fixtures" / "demo1.dig").toString,
      "--impl", "real",
      "--tics", "62",
      "--out-dir", verifyOut.toString,
      "--ref-trace", (root / "fixtures" / "demo1.trc").toString,
      "--root", root.toString
    ]
    cwd := root / "lean"
  }
  IO.println v.stdout
  if !v.stderr.isEmpty then IO.eprintln v.stderr
  let vout := v.stdout ++ v.stderr
  ok := (← assert "verify: ran real impl" (vout.contains "real: wrote")) && ok

  let candTrc := verifyOut / "candidate.trc"
  let candDig := verifyOut / "candidate.dig"
  let trcBytes ← IO.FS.readBinFile candTrc
  match parseTraceBytes trcBytes with
  | Except.error e =>
    ok := (← assert s!"parse candidate.trc ({e})" false) && ok
  | Except.ok recs =>
    ok := (← assert "candidate has >= 62 tics" (recs.size >= 62)) && ok
    let mut i : Nat := 0
    while i < 6 && i < recs.size do
      match recs[i]?, expectedRndindex[i]? with
      | some rec, some want =>
        ok := (← assert s!"tic {i} rndindex={want}" (rec.rndindex == want)) && ok
      | _, _ =>
        ok := (← assert s!"tic {i} present" false) && ok
      i := i + 1
    -- Soft land: no sfx_oof pitch draw; rndindex advances +1/tic (ST_Ticker).
    let mut ri : Nat := 27
    while ri <= 46 && ri < recs.size do
      match recs[ri]?, recs[ri - 1]? with
      | some cur, some prev =>
        ok := (← assert s!"tic {ri} rndindex Δ=+1"
          (cur.rndindex == prev.rndindex + 1)) && ok
      | _, _ =>
        ok := (← assert s!"tic {ri} rndindex pair" false) && ok
      ri := ri + 1
    let mut gi : Nat := 0
    while gi < playerXyGoldens.size do
      match playerXyGoldens[gi]? with
      | none => ok := (← assert "golden row" false) && ok
      | some (tic, x, y, momx, momy) =>
        match recs[tic]? with
        | none => ok := (← assert s!"tic {tic} present for XY golden" false) && ok
        | some rec =>
          match rec.players[0]? with
          | none => ok := (← assert s!"tic {tic} player0" false) && ok
          | some p =>
            ok := (← assert s!"tic {tic} player x"
              (p.x == x.toUInt32)) && ok
            ok := (← assert s!"tic {tic} player y"
              (p.y == y.toUInt32)) && ok
            ok := (← assert s!"tic {tic} player momx"
              (p.momx == momx.toUInt32)) && ok
            ok := (← assert s!"tic {tic} player momy"
              (p.momy == momy.toUInt32)) && ok
      gi := gi + 1
    let mut zi : Nat := 0
    while zi < playerZGoldens.size do
      match playerZGoldens[zi]? with
      | none => ok := (← assert "Z golden row" false) && ok
      | some (tic, z, momz, viewz) =>
        match recs[tic]? with
        | none => ok := (← assert s!"tic {tic} present for Z golden" false) && ok
        | some rec =>
          match rec.players[0]? with
          | none => ok := (← assert s!"tic {tic} player0 Z" false) && ok
          | some p =>
            ok := (← assert s!"tic {tic} player z"
              (p.z == z.toUInt32)) && ok
            ok := (← assert s!"tic {tic} player momz"
              (p.momz == momz.toUInt32)) && ok
            ok := (← assert s!"tic {tic} player viewz"
              (p.viewz == viewz.toUInt32)) && ok
      zi := zi + 1
    -- Last matching player XY (tic 59) before slide-needed break @60.
    match recs[59]? with
    | none => ok := (← assert "tic 59 present" false) && ok
    | some rec =>
      match rec.players[0]? with
      | none => ok := (← assert "tic 59 player0" false) && ok
      | some p =>
        ok := (← assert "tic 59 player x=-24573036"
          (p.x == (-24573036 : Int32).toUInt32)) && ok
        ok := (← assert "tic 59 player y=-8374393"
          (p.y == (-8374393 : Int32).toUInt32)) && ok
    -- Wake RNG + chase goldens
    match recs[46]?, recs[47]? with
    | some r46, some r47 =>
      ok := (← assert "tic 47 prndindex=137" (r47.prndindex == 137)) && ok
      ok := (← assert "tic 47 Δprnd=+3"
        (r47.prndindex == r46.prndindex + 3)) && ok
      ok := (← assert "tic 47 Δrnd=+2"
        (r47.rndindex == r46.rndindex + 2)) && ok
    | _, _ =>
      ok := (← assert "tic 46/47 present for RNG golden" false) && ok
    match recs[53]? with
    | some r => ok := (← assert "tic 53 prndindex=141" (r.prndindex == 141)) && ok
    | none => ok := (← assert "tic 53 present" false) && ok
    match recs[60]? with
    | some r =>
      ok := (← assert "tic 60 prndindex=146" (r.prndindex == 146)) && ok
      ok := (← assert "tic 60 rndindex=127" (r.rndindex == 127)) && ok
    | none => ok := (← assert "tic 60 present" false) && ok
    let mut ti : Nat := 0
    while ti < troop157Goldens.size do
      match troop157Goldens[ti]? with
      | none => ok := (← assert "157 golden row" false) && ok
      | some (tic, x, y, ang, st) =>
        match recs[tic]? with
        | none => ok := (← assert s!"tic {tic} for tid157" false) && ok
        | some rec =>
          let mut found := false
          let mut hi : Nat := 0
          while hi < rec.thinkers.size do
            match rec.thinkers[hi]? with
            | some th =>
              if th.traceId == 157 then
                match th.mobj with
                | some mo =>
                  found := true
                  ok := (← assert s!"tic {tic} tid157 x" (mo.x == x.toUInt32)) && ok
                  ok := (← assert s!"tic {tic} tid157 y" (mo.y == y.toUInt32)) && ok
                  ok := (← assert s!"tic {tic} tid157 angle" (mo.angle == ang)) && ok
                  ok := (← assert s!"tic {tic} tid157 state" (mo.state == st)) && ok
                | none => pure ()
              pure ()
            | none => pure ()
            hi := hi + 1
          ok := (← assert s!"tic {tic} tid157 present" found) && ok
      ti := ti + 1
    -- Troop wakes (monsters match at 60+ even though digest breaks on player)
    let mut found155 := false
    let mut found156 := false
    match recs[60]? with
    | some rec =>
      let mut hi : Nat := 0
      while hi < rec.thinkers.size do
        match rec.thinkers[hi]? with
        | some th =>
          if th.traceId == 155 then
            match th.mobj with
            | some mo =>
              found155 := true
              ok := (← assert "tic 60 tid155 state=444" (mo.state == 444)) && ok
              ok := (← assert "tic 60 tid155 x"
                (mo.x == (16401216 : Int32).toUInt32)) && ok
              ok := (← assert "tic 60 tid155 y"
                (mo.y == (3818304 : Int32).toUInt32)) && ok
            | none => pure ()
          pure ()
        | none => pure ()
        hi := hi + 1
    | none => pure ()
    match recs[61]? with
    | some rec =>
      let mut hi : Nat := 0
      while hi < rec.thinkers.size do
        match rec.thinkers[hi]? with
        | some th =>
          if th.traceId == 156 then
            match th.mobj with
            | some mo =>
              found156 := true
              ok := (← assert "tic 61 tid156 state=444" (mo.state == 444)) && ok
            | none => pure ()
          pure ()
        | none => pure ()
        hi := hi + 1
    | none => pure ()
    ok := (← assert "tic 60 tid155 present" found155) && ok
    ok := (← assert "tic 61 tid156 present" found156) && ok

  let diff ← IO.Process.output {
    cmd := "python3"
    args := #[
      (root / "tools" / "tracediff").toString,
      (root / "fixtures" / "demo1.dig").toString,
      candDig.toString,
      "--ref-trace",
      (root / "fixtures" / "demo1.trc").toString,
      "--cand-trace",
      candTrc.toString
    ]
  }
  IO.println diff.stdout
  if !diff.stderr.isEmpty then IO.eprintln diff.stderr
  let dout := diff.stdout ++ diff.stderr
  match firstMismatchTic dout with
  | none =>
    ok := (← assert "tracediff: expected divergence at tic 60 (got none)" false) && ok
  | some tic =>
    ok := (← assert s!"tracediff: first divergence tic == 60 (got {tic})"
      (tic == 60)) && ok
    ok := (← assert "tracediff: tic 60 mentions player"
      (dout.contains "player")) && ok
    ok := (← assert "tracediff: tic 60 has no monster chase field diffs"
      (!mentionsMonsterChase dout)) && ok

  if ok then
    IO.println "ALL DEMO1 DIGEST P2c-iii CHECKS PASSED"
    pure 0
  else
    IO.eprintln "SOME DEMO1 DIGEST CHECKS FAILED"
    pure 1
