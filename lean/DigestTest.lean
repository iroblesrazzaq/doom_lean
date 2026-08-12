import Doom.Harness.TraceFormat
import Doom.Harness.TraceReader

/-!
P2c-iv behavior lock: DEMO1 `P_SlideMove` + green armor pickup (`SPR_ARM1`).

E2E contract (public surface `verify --impl real --tics 77`):
- candidate digests 0..76 must match fixtures/demo1.dig.
- Negative smoke (next-chunk entry): `verify --tics 78` loud-errors with
  exact message `A_Chase: missilestate not implemented` (chase-fire /
  missilestate open subset is intentionally out of scope for this chunk).
- Fixture goldens (tracelib on fixtures/demo1.trc):
  - @60 armorpoints=100, tid 204 absent, thinker_count=229
  - player slide XY/mom window 59..75
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

/-- Slide-window player XY/mom goldens (fixture), tics 59..75. -/
def playerSlideGoldens : Array (Nat × Int32 × Int32 × Int32 × Int32) := #[
  (59, (-24573036 : Int32), -8374393, -509314, 321963),
  (60, (-25127244 : Int32), -8221328, -215638, -35969),
  (61, (-25127244 : Int32), -8245162, -205398, -21600),
  (62, (-25332642 : Int32), -8266762, -186142, -19575),
  (63, (-25509020 : Int32), -8299493, -159843, -29663),
  (64, (-25663813 : Int32), -8338064, -140282, -34955),
  (65, (-25782764 : Int32), -8449990, -107800, -101433),
  (66, (-25882993 : Int32), -8653542, -90833, -184469),
  (67, (-25973814 : Int32), -8870779, -82307, -196872),
  (68, (-26073589 : Int32), -9168550, -90422, -269855),
  (69, (-26175210 : Int32), -9488366, -92095, -289834),
  (70, (-26288480 : Int32), -9842381, -102651, -320827),
  (71, (-26410203 : Int32), -10206278, -110312, -329782),
  (72, (-26525116 : Int32), -10545209, -104140, -307157),
  (73, (-26638726 : Int32), -10868180, -102960, -292693),
  (74, (-26741686 : Int32), -11160873, -93308, -265254),
  (75, (-26829957 : Int32), -11419667, -79996, -234533)
]

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
      "--tics", "77",
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

  let boundOut := verifyOut / "bound"
  IO.FS.createDirAll boundOut
  let vBound ← IO.Process.output {
    cmd := "lake"
    args := #[
      "exe", "verify", "--",
      "--iwad", (root / "fixtures" / "wads" / "doom1.wad").toString,
      "--demo", "DEMO1",
      "--ref-digest", (root / "fixtures" / "demo1.dig").toString,
      "--impl", "real",
      "--tics", "78",
      "--out-dir", boundOut.toString,
      "--root", root.toString
    ]
    cwd := root / "lean"
  }
  let bout := vBound.stdout ++ vBound.stderr
  if !vBound.stderr.isEmpty then IO.eprintln vBound.stderr
  ok := (← assert "verify --tics 78: missilestate loud-error"
    (bout.contains "A_Chase: missilestate not implemented")) && ok
  ok := (← assert "verify --tics 78: did not complete write"
    (!bout.contains "real: wrote")) && ok

  let candTrc := verifyOut / "candidate.trc"
  let candDig := verifyOut / "candidate.dig"
  let trcBytes ← IO.FS.readBinFile candTrc
  match parseTraceBytes trcBytes with
  | Except.error e =>
    ok := (← assert s!"parse candidate.trc ({e})" false) && ok
  | Except.ok recs =>
    ok := (← assert "candidate has >= 77 tics" (recs.size >= 77)) && ok
    let mut i : Nat := 0
    while i < 6 && i < recs.size do
      match recs[i]?, expectedRndindex[i]? with
      | some rec, some want =>
        ok := (← assert s!"tic {i} rndindex={want}" (rec.rndindex == want)) && ok
      | _, _ =>
        ok := (← assert s!"tic {i} present" false) && ok
      i := i + 1
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
      ok := (← assert "tic 60 thinker_count=229" (r.thinkers.size == 229)) && ok
      let mut found204 := false
      let mut hi : Nat := 0
      while hi < r.thinkers.size do
        match r.thinkers[hi]? with
        | some th =>
          if th.traceId == 204 then found204 := true
        | none => pure ()
        hi := hi + 1
      ok := (← assert "tic 60 tid204 absent" (!found204)) && ok
      match r.players[0]? with
      | none => ok := (← assert "tic 60 player0" false) && ok
      | some p =>
        ok := (← assert "tic 60 armorpoints=100"
          (p.armorpoints == (100 : Int32).toUInt32)) && ok
    | none => ok := (← assert "tic 60 present" false) && ok
    let mut si : Nat := 0
    while si < playerSlideGoldens.size do
      match playerSlideGoldens[si]? with
      | none => ok := (← assert "slide golden row" false) && ok
      | some (tic, x, y, momx, momy) =>
        match recs[tic]? with
        | none => ok := (← assert s!"tic {tic} present for slide golden" false) && ok
        | some rec =>
          match rec.players[0]? with
          | none => ok := (← assert s!"tic {tic} player0 slide" false) && ok
          | some p =>
            ok := (← assert s!"tic {tic} slide x" (p.x == x.toUInt32)) && ok
            ok := (← assert s!"tic {tic} slide y" (p.y == y.toUInt32)) && ok
            ok := (← assert s!"tic {tic} slide momx" (p.momx == momx.toUInt32)) && ok
            ok := (← assert s!"tic {tic} slide momy" (p.momy == momy.toUInt32)) && ok
      si := si + 1
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

  let digCmp ← IO.Process.output {
    cmd := "python3"
    args := #[
      "-c",
      "import sys; sys.path.insert(0, sys.argv[1]);\n" ++
      "from tracelib import read_digest_stream\n" ++
      "_, c = read_digest_stream(sys.argv[2]); _, r = read_digest_stream(sys.argv[3])\n" ++
      "assert len(c) >= 77, len(c)\n" ++
      "bad = [i for i in range(77) if c[i] != r[i]]\n" ++
      "print('OK' if not bad else 'BAD ' + str(bad[:5])); sys.exit(0 if not bad else 1)\n",
      (root / "tools").toString,
      candDig.toString,
      (root / "fixtures" / "demo1.dig").toString
    ]
  }
  IO.println digCmp.stdout
  if !digCmp.stderr.isEmpty then IO.eprintln digCmp.stderr
  ok := (← assert "digests 0..76 match fixture"
    (digCmp.exitCode == 0 && digCmp.stdout.contains "OK")) && ok

  if ok then
    IO.println "ALL DEMO1 DIGEST P2c-iv CHECKS PASSED (K=76)"
    pure 0
  else
    IO.eprintln "SOME DEMO1 DIGEST CHECKS FAILED"
    pure 1
