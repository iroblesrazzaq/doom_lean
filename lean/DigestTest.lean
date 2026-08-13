import Doom.Harness.TraceFormat
import Doom.Harness.TraceReader

set_option maxHeartbeats 800000

/-!
P2c-ix behavior lock: DEMO1 A_Scream + A_Fall + door-wait through K=134.

E2E contract (public surface `verify --impl real --tics 135`):
- candidate digests 0..134 must match fixtures/demo1.dig.
- Negative smoke (next-chunk entry): `verify --tics 136` loud-errors on
  `P_TouchSpecialThing: unsupported sprite 92`.
- Fixture goldens (tracelib on fixtures/demo1.trc), retained P2c-viii plus:
  - @116 scream: tid157 222→223 tics=5 fl 0x500482
    rnd 192→194 prnd 83→86 ceil 3670016→3801088
  - @121 fall+dest: tid157 223→224 tics=5 fl 0x500480 ceil=4456448
  - @122 ceil stays 4456448 nsec=1
  - @131 tid157 st=226 tics=-1
  - @134 pend=10 ready=1 ammo=[46,0,0,0] tid238 present nth=233
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

/-- P2c-vii fixture goldens: S_NULL remove @92/93 and A_ReFire hit @99/108. -/
def checkP2cViiGoldens (recs : Array TicRecord) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  match recs[92]? with
  | none => ok := (← assert "tic 92 present" false) && ok
  | some rec =>
    ok := (← assert "tic 92 thinker_count=231" (rec.thinkers.size == 231)) && ok
    let mut found231r := false
    let mut hi : Nat := 0
    while hi < rec.thinkers.size do
      match rec.thinkers[hi]? with
      | some th =>
        if th.traceId == 231 then
          found231r := true
          ok := (← assert "tic 92 tid231 THF_REMOVED" (th.func == 0)) && ok
          ok := (← assert "tic 92 tid231 no payload"
            (match th.mobj with | none => true | some _ => false)) && ok
      | none => pure ()
      hi := hi + 1
    ok := (← assert "tic 92 tid231 present" found231r) && ok
  match recs[93]? with
  | none => ok := (← assert "tic 93 present" false) && ok
  | some rec =>
    ok := (← assert "tic 93 thinker_count=230" (rec.thinkers.size == 230)) && ok
    let mut found231a := false
    let mut hi : Nat := 0
    while hi < rec.thinkers.size do
      match rec.thinkers[hi]? with
      | some th =>
        if th.traceId == 231 then found231a := true
      | none => pure ()
      hi := hi + 1
    ok := (← assert "tic 93 tid231 absent" (!found231a)) && ok
  match recs[98]?, recs[99]? with
  | some r98, some r99 =>
    ok := (← assert "tic 99 rndindex Δ=+2"
      (r99.rndindex == r98.rndindex + 2 && r98.rndindex == 171 && r99.rndindex == 173)) && ok
    ok := (← assert "tic 99 thinker_count=231" (r99.thinkers.size == 231)) && ok
    match r98.players[0]?, r99.players[0]? with
    | some p98, some p99 =>
      ok := (← assert "tic 98 ammo0=49" (p98.ammo0 == (49 : Int32).toUInt32)) && ok
      ok := (← assert "tic 99 ammo0=48" (p99.ammo0 == (48 : Int32).toUInt32)) && ok
    | _, _ =>
      ok := (← assert "tic 98/99 player0" false) && ok
    let mut found157d := false
    let mut found233 := false
    let mut hi : Nat := 0
    while hi < r99.thinkers.size do
      match r99.thinkers[hi]? with
      | some th =>
        if th.traceId == 157 then
          match th.mobj with
          | some mo =>
            found157d := true
            ok := (← assert "tic 99 tid157 hp=10"
              (mo.health == (10 : Int32).toUInt32)) && ok
            ok := (← assert "tic 99 tid157 state=217" (mo.state == 217)) && ok
            ok := (← assert "tic 99 tid157 tics=10"
              (mo.tics == (10 : Int32).toUInt32)) && ok
            ok := (← assert "tic 99 tid157 flags" (mo.flags == 0x400086)) && ok
          | none => pure ()
        if th.traceId == 233 then
          match th.mobj with
          | some mo =>
            found233 := true
            ok := (← assert "tic 99 tid233 type=38" (mo.type_ == 38)) && ok
            ok := (← assert "tic 99 tid233 state=90" (mo.state == 90)) && ok
            ok := (← assert "tic 99 tid233 tics=4"
              (mo.tics == (4 : Int32).toUInt32)) && ok
            ok := (← assert "tic 99 tid233 x"
              (mo.x == (-11630415 : Int32).toUInt32)) && ok
            ok := (← assert "tic 99 tid233 y"
              (mo.y == (-5146798 : Int32).toUInt32)) && ok
            ok := (← assert "tic 99 tid233 z"
              (mo.z == (2130179 : Int32).toUInt32)) && ok
            ok := (← assert "tic 99 tid233 momz"
              (mo.momz == (65536 : Int32).toUInt32)) && ok
          | none => pure ()
      | none => pure ()
      hi := hi + 1
    ok := (← assert "tic 99 tid157 present" found157d) && ok
    ok := (← assert "tic 99 tid233 BLOOD present" found233) && ok
  | _, _ =>
    ok := (← assert "tic 98/99 present for ReFire golden" false) && ok
  match recs[108]? with
  | none => ok := (← assert "tic 108 present" false) && ok
  | some rec =>
    ok := (← assert "tic 108 thinker_count=231" (rec.thinkers.size == 231)) && ok
    match rec.players[0]? with
    | none => ok := (← assert "tic 108 player0" false) && ok
    | some p =>
      ok := (← assert "tic 108 ammo0=48" (p.ammo0 == (48 : Int32).toUInt32)) && ok
      ok := (← assert "tic 108 health=100" (p.health == (100 : Int32).toUInt32)) && ok
      ok := (← assert "tic 108 armorpoints=100"
        (p.armorpoints == (100 : Int32).toUInt32)) && ok
      ok := (← assert "tic 108 pendingweapon=10"
        (p.pendingweapon == (10 : Int32).toUInt32)) && ok
    ok := (← assert "tic 108 nsec=1" (rec.sectors.size == 1)) && ok
    match rec.sectors[0]? with
    | none => ok := (← assert "tic 108 sector rec" false) && ok
    | some sec =>
      ok := (← assert "tic 108 sec71" (sec.sectorIndex == 71)) && ok
      ok := (← assert "tic 108 sec71 ceil=2752512"
        (sec.ceilingheight == (2752512 : Int32).toUInt32)) && ok
    let mut found157e := false
    let mut found233b := false
    let mut hi : Nat := 0
    while hi < rec.thinkers.size do
      match rec.thinkers[hi]? with
      | some th =>
        if th.traceId == 157 then
          match th.mobj with
          | some mo =>
            found157e := true
            ok := (← assert "tic 108 tid157 state=217" (mo.state == 217)) && ok
            ok := (← assert "tic 108 tid157 tics=1"
              (mo.tics == (1 : Int32).toUInt32)) && ok
            ok := (← assert "tic 108 tid157 hp=10"
              (mo.health == (10 : Int32).toUInt32)) && ok
            ok := (← assert "tic 108 tid157 flags" (mo.flags == 0x400086)) && ok
          | none => pure ()
        if th.traceId == 233 then
          match th.mobj with
          | some mo =>
            found233b := true
            ok := (← assert "tic 108 tid233 state=91" (mo.state == 91)) && ok
            ok := (← assert "tic 108 tid233 tics=3"
              (mo.tics == (3 : Int32).toUInt32)) && ok
            ok := (← assert "tic 108 tid233 momz"
              (mo.momz == (0 : Int32).toUInt32)) && ok
          | none => pure ()
      | none => pure ()
      hi := hi + 1
    ok := (← assert "tic 108 tid157 present" found157e) && ok
    ok := (← assert "tic 108 tid233 present" found233b) && ok
  pure ok

/-- P2c-viii fixture goldens: shotgunner hit @109 and first kill @113. -/
def checkP2cViiiGoldens (recs : Array TicRecord) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  match recs[108]?, recs[109]? with
  | some r108, some r109 =>
    ok := (← assert "tic 109 rndindex 183→185"
      (r108.rndindex == 183 && r109.rndindex == 185)) && ok
    ok := (← assert "tic 109 prndindex 23→53"
      (r108.prndindex == 23 && r109.prndindex == 53)) && ok
    ok := (← assert "tic 109 thinker_count=234" (r109.thinkers.size == 234)) && ok
    match r108.players[0]?, r109.players[0]? with
    | some p108, some p109 =>
      ok := (← assert "tic 108 health=100" (p108.health == (100 : Int32).toUInt32)) && ok
      ok := (← assert "tic 109 health=98" (p109.health == (98 : Int32).toUInt32)) && ok
      ok := (← assert "tic 108 armor=100"
        (p108.armorpoints == (100 : Int32).toUInt32)) && ok
      ok := (← assert "tic 109 armor=99"
        (p109.armorpoints == (99 : Int32).toUInt32)) && ok
      ok := (← assert "tic 108 player momx"
        (p108.momx == (385991 : Int32).toUInt32)) && ok
      ok := (← assert "tic 108 player momy"
        (p108.momy == (211916 : Int32).toUInt32)) && ok
      ok := (← assert "tic 109 player momx"
        (p109.momx == (311956 : Int32).toUInt32)) && ok
      ok := (← assert "tic 109 player momy"
        (p109.momy == (167819 : Int32).toUInt32)) && ok
    | _, _ =>
      ok := (← assert "tic 108/109 player0" false) && ok
    let mut foundP := false
    let mut found234 := false
    let mut found235 := false
    let mut found236 := false
    let mut hi : Nat := 0
    while hi < r109.thinkers.size do
      match r109.thinkers[hi]? with
      | some th =>
        if th.traceId == 1 then
          match th.mobj with
          | some mo =>
            foundP := true
            ok := (← assert "tic 109 player state=156" (mo.state == 156)) && ok
            ok := (← assert "tic 109 player JUSTHIT" (mo.flags == 0x2000c46)) && ok
            ok := (← assert "tic 109 player mo hp=98"
              (mo.health == (98 : Int32).toUInt32)) && ok
          | none => pure ()
        if th.traceId == 234 then
          match th.mobj with
          | some mo =>
            found234 := true
            ok := (← assert "tic 109 tid234 PUFF type=37" (mo.type_ == 37)) && ok
            ok := (← assert "tic 109 tid234 state=94" (mo.state == 94)) && ok
            ok := (← assert "tic 109 tid234 tics=4"
              (mo.tics == (4 : Int32).toUInt32)) && ok
            ok := (← assert "tic 109 tid234 momz"
              (mo.momz == (65536 : Int32).toUInt32)) && ok
          | none => pure ()
        if th.traceId == 235 then
          match th.mobj with
          | some mo =>
            found235 := true
            ok := (← assert "tic 109 tid235 PUFF type=37" (mo.type_ == 37)) && ok
            ok := (← assert "tic 109 tid235 state=93" (mo.state == 93)) && ok
            ok := (← assert "tic 109 tid235 tics=1"
              (mo.tics == (1 : Int32).toUInt32)) && ok
          | none => pure ()
        if th.traceId == 236 then
          match th.mobj with
          | some mo =>
            found236 := true
            ok := (← assert "tic 109 tid236 BLOOD type=38" (mo.type_ == 38)) && ok
            ok := (← assert "tic 109 tid236 state=92" (mo.state == 92)) && ok
            ok := (← assert "tic 109 tid236 tics=7"
              (mo.tics == (7 : Int32).toUInt32)) && ok
            ok := (← assert "tic 109 tid236 x"
              (mo.x == (-18755179 : Int32).toUInt32)) && ok
            ok := (← assert "tic 109 tid236 y"
              (mo.y == (-9498320 : Int32).toUInt32)) && ok
            ok := (← assert "tic 109 tid236 z"
              (mo.z == (1996144 : Int32).toUInt32)) && ok
            ok := (← assert "tic 109 tid236 momz"
              (mo.momz == (65536 : Int32).toUInt32)) && ok
          | none => pure ()
      | none => pure ()
      hi := hi + 1
    ok := (← assert "tic 109 player mo present" foundP) && ok
    ok := (← assert "tic 109 tid234 PUFF present" found234) && ok
    ok := (← assert "tic 109 tid235 PUFF present" found235) && ok
    ok := (← assert "tic 109 tid236 BLOOD present" found236) && ok
  | _, _ =>
    ok := (← assert "tic 108/109 present for first-kill golden" false) && ok
  match recs[112]?, recs[113]? with
  | some r112, some r113 =>
    ok := (← assert "tic 113 thinker_count=236" (r113.thinkers.size == 236)) && ok
    match r112.players[0]?, r113.players[0]? with
    | some p112, some p113 =>
      ok := (← assert "tic 112 ammo0=48" (p112.ammo0 == (48 : Int32).toUInt32)) && ok
      ok := (← assert "tic 113 ammo0=47" (p113.ammo0 == (47 : Int32).toUInt32)) && ok
    | _, _ =>
      ok := (← assert "tic 112/113 player0" false) && ok
    let mut found157k := false
    let mut found237 := false
    let mut found238 := false
    let mut hi : Nat := 0
    while hi < r113.thinkers.size do
      match r113.thinkers[hi]? with
      | some th =>
        if th.traceId == 157 then
          match th.mobj with
          | some mo =>
            found157k := true
            ok := (← assert "tic 113 tid157 hp=-5"
              (mo.health == (-5 : Int32).toUInt32)) && ok
            ok := (← assert "tic 113 tid157 state=222" (mo.state == 222)) && ok
            ok := (← assert "tic 113 tid157 tics=3"
              (mo.tics == (3 : Int32).toUInt32)) && ok
            ok := (← assert "tic 113 tid157 flags" (mo.flags == 0x500482)) && ok
          | none => pure ()
        if th.traceId == 237 then
          match th.mobj with
          | some mo =>
            found237 := true
            ok := (← assert "tic 113 tid237 BLOOD type=38" (mo.type_ == 38)) && ok
            ok := (← assert "tic 113 tid237 state=90" (mo.state == 90)) && ok
            ok := (← assert "tic 113 tid237 tics=5"
              (mo.tics == (5 : Int32).toUInt32)) && ok
          | none => pure ()
        if th.traceId == 238 then
          match th.mobj with
          | some mo =>
            found238 := true
            ok := (← assert "tic 113 tid238 MT_SHOTGUN type=77" (mo.type_ == 77)) && ok
            ok := (← assert "tic 113 tid238 flags" (mo.flags == 0x20001)) && ok
            ok := (← assert "tic 113 tid238 x"
              (mo.x == (-11194424 : Int32).toUInt32)) && ok
            ok := (← assert "tic 113 tid238 y"
              (mo.y == (-3230019 : Int32).toUInt32)) && ok
            ok := (← assert "tic 113 tid238 z" (mo.z == (0 : Int32).toUInt32)) && ok
          | none => pure ()
      | none => pure ()
      hi := hi + 1
    ok := (← assert "tic 113 tid157 present" found157k) && ok
    ok := (← assert "tic 113 tid237 BLOOD present" found237) && ok
    ok := (← assert "tic 113 tid238 shotgun drop present" found238) && ok
  | _, _ =>
    ok := (← assert "tic 112/113 present for kill golden" false) && ok
  pure ok

/-- P2c-ix fixture goldens: A_Scream @116, A_Fall+door dest @121, wait @122, corpse @131/@134. -/
def checkP2cIxGoldens (recs : Array TicRecord) (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  match recs[115]?, recs[116]? with
  | some r115, some r116 =>
    ok := (← assert "tic 116 rndindex 192→194"
      (r115.rndindex == 192 && r116.rndindex == 194)) && ok
    ok := (← assert "tic 116 prndindex 83→86"
      (r115.prndindex == 83 && r116.prndindex == 86)) && ok
    ok := (← assert "tic 116 nsec=1" (r116.sectors.size == 1)) && ok
    match r115.sectors[0]?, r116.sectors[0]? with
    | some s115, some s116 =>
      ok := (← assert "tic 115 sec71 ceil=3670016"
        (s115.ceilingheight == (3670016 : Int32).toUInt32)) && ok
      ok := (← assert "tic 116 sec71 ceil=3801088"
        (s116.ceilingheight == (3801088 : Int32).toUInt32)) && ok
    | _, _ =>
      ok := (← assert "tic 115/116 sector rec" false) && ok
    let mut found115 := false
    let mut found116 := false
    let mut hi : Nat := 0
    while hi < r115.thinkers.size do
      match r115.thinkers[hi]? with
      | some th =>
        if th.traceId == 157 then
          match th.mobj with
          | some mo =>
            found115 := true
            ok := (← assert "tic 115 tid157 state=222" (mo.state == 222)) && ok
          | none => pure ()
      | none => pure ()
      hi := hi + 1
    hi := 0
    while hi < r116.thinkers.size do
      match r116.thinkers[hi]? with
      | some th =>
        if th.traceId == 157 then
          match th.mobj with
          | some mo =>
            found116 := true
            ok := (← assert "tic 116 tid157 state=223" (mo.state == 223)) && ok
            ok := (← assert "tic 116 tid157 tics=5"
              (mo.tics == (5 : Int32).toUInt32)) && ok
            ok := (← assert "tic 116 tid157 flags" (mo.flags == 0x500482)) && ok
          | none => pure ()
      | none => pure ()
      hi := hi + 1
    ok := (← assert "tic 115 tid157 present" found115) && ok
    ok := (← assert "tic 116 tid157 present" found116) && ok
  | _, _ =>
    ok := (← assert "tic 115/116 present for scream golden" false) && ok
  match recs[120]?, recs[121]? with
  | some r120, some r121 =>
    ok := (← assert "tic 121 nsec=1" (r121.sectors.size == 1)) && ok
    match r121.sectors[0]? with
    | none => ok := (← assert "tic 121 sector rec" false) && ok
    | some sec =>
      ok := (← assert "tic 121 sec71 ceil=4456448"
        (sec.ceilingheight == (4456448 : Int32).toUInt32)) && ok
    let mut found120 := false
    let mut found121 := false
    let mut hi : Nat := 0
    while hi < r120.thinkers.size do
      match r120.thinkers[hi]? with
      | some th =>
        if th.traceId == 157 then
          match th.mobj with
          | some mo =>
            found120 := true
            ok := (← assert "tic 120 tid157 state=223" (mo.state == 223)) && ok
            ok := (← assert "tic 120 tid157 flags" (mo.flags == 0x500482)) && ok
          | none => pure ()
      | none => pure ()
      hi := hi + 1
    hi := 0
    while hi < r121.thinkers.size do
      match r121.thinkers[hi]? with
      | some th =>
        if th.traceId == 157 then
          match th.mobj with
          | some mo =>
            found121 := true
            ok := (← assert "tic 121 tid157 state=224" (mo.state == 224)) && ok
            ok := (← assert "tic 121 tid157 tics=5"
              (mo.tics == (5 : Int32).toUInt32)) && ok
            ok := (← assert "tic 121 tid157 flags" (mo.flags == 0x500480)) && ok
          | none => pure ()
      | none => pure ()
      hi := hi + 1
    ok := (← assert "tic 120 tid157 present" found120) && ok
    ok := (← assert "tic 121 tid157 present" found121) && ok
  | _, _ =>
    ok := (← assert "tic 120/121 present for fall golden" false) && ok
  match recs[122]? with
  | none => ok := (← assert "tic 122 present" false) && ok
  | some rec =>
    ok := (← assert "tic 122 nsec=1" (rec.sectors.size == 1)) && ok
    match rec.sectors[0]? with
    | none => ok := (← assert "tic 122 sector rec" false) && ok
    | some sec =>
      ok := (← assert "tic 122 sec71 ceil stays 4456448"
        (sec.ceilingheight == (4456448 : Int32).toUInt32)) && ok
  match recs[131]? with
  | none => ok := (← assert "tic 131 present" false) && ok
  | some rec =>
    let mut found131 := false
    let mut hi : Nat := 0
    while hi < rec.thinkers.size do
      match rec.thinkers[hi]? with
      | some th =>
        if th.traceId == 157 then
          match th.mobj with
          | some mo =>
            found131 := true
            ok := (← assert "tic 131 tid157 state=226" (mo.state == 226)) && ok
            ok := (← assert "tic 131 tid157 tics=-1"
              (mo.tics == (-1 : Int32).toUInt32)) && ok
          | none => pure ()
      | none => pure ()
      hi := hi + 1
    ok := (← assert "tic 131 tid157 present" found131) && ok
  match recs[134]? with
  | none => ok := (← assert "tic 134 present" false) && ok
  | some rec =>
    ok := (← assert "tic 134 thinker_count=233" (rec.thinkers.size == 233)) && ok
    match rec.players[0]? with
    | none => ok := (← assert "tic 134 player0" false) && ok
    | some p =>
      ok := (← assert "tic 134 pendingweapon=10"
        (p.pendingweapon == (10 : Int32).toUInt32)) && ok
      ok := (← assert "tic 134 readyweapon=1"
        (p.readyweapon == (1 : Int32).toUInt32)) && ok
      ok := (← assert "tic 134 ammo=[46,0,0,0]"
        (p.ammo0 == (46 : Int32).toUInt32
          && p.ammo1 == (0 : Int32).toUInt32
          && p.ammo2 == (0 : Int32).toUInt32
          && p.ammo3 == (0 : Int32).toUInt32)) && ok
    let mut found238 := false
    let mut hi : Nat := 0
    while hi < rec.thinkers.size do
      match rec.thinkers[hi]? with
      | some th =>
        if th.traceId == 238 then
          found238 := true
      | none => pure ()
      hi := hi + 1
    ok := (← assert "tic 134 tid238 present" found238) && ok
  pure ok

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
      "--tics", "135",
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
      "--tics", "136",
      "--out-dir", boundOut.toString,
      "--root", root.toString
    ]
    cwd := root / "lean"
  }
  let bout := vBound.stdout ++ vBound.stderr
  if !vBound.stderr.isEmpty then IO.eprintln vBound.stderr
  ok := (← assert "verify --tics 136: sprite 92 loud-error"
    (bout.contains "P_TouchSpecialThing: unsupported sprite 92"
      && !bout.contains "A_Scream: not implemented")) && ok
  ok := (← assert "verify --tics 136: did not complete write"
    (!bout.contains "real: wrote")) && ok

  let candTrc := verifyOut / "candidate.trc"
  let candDig := verifyOut / "candidate.dig"
  let trcBytes ← IO.FS.readBinFile candTrc
  match parseTraceBytes trcBytes with
  | Except.error e =>
    ok := (← assert s!"parse candidate.trc ({e})" false) && ok
  | Except.ok recs =>
    ok := (← assert "candidate has >= 135 tics" (recs.size >= 135)) && ok
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

    -- P2c-v hitscan goldens ------------------------------------------------
    match recs[77]? with
    | none => ok := (← assert "tic 77 present" false) && ok
    | some rec =>
      ok := (← assert "tic 77 thinker_count=229" (rec.thinkers.size == 229)) && ok
      let mut found157 := false
      let mut hi : Nat := 0
      while hi < rec.thinkers.size do
        match rec.thinkers[hi]? with
        | some th =>
          if th.traceId == 157 then
            match th.mobj with
            | some mo =>
              found157 := true
              ok := (← assert "tic 77 tid157 state=217" (mo.state == 217)) && ok
              ok := (← assert "tic 77 tid157 angle" (mo.angle == 0x952d7840)) && ok
              ok := (← assert "tic 77 tid157 flags" (mo.flags == 0x400086)) && ok
            | none => pure ()
        | none => pure ()
        hi := hi + 1
      ok := (← assert "tic 77 tid157 present" found157) && ok
    match recs[81]? with
    | none => ok := (← assert "tic 81 present" false) && ok
    | some rec =>
      match rec.players[0]? with
      | none => ok := (← assert "tic 81 player0" false) && ok
      | some p =>
        ok := (← assert "tic 81 cmd_buttons=1" (p.cmdButtons == 1)) && ok
        ok := (← assert "tic 81 ammo0=50" (p.ammo0 == (50 : Int32).toUInt32)) && ok
      let mut foundP := false
      let mut found38 := false
      let mut hi : Nat := 0
      while hi < rec.thinkers.size do
        match rec.thinkers[hi]? with
        | some th =>
          if th.traceId == 1 then
            match th.mobj with
            | some mo =>
              foundP := true
              ok := (← assert "tic 81 player state=154" (mo.state == 154)) && ok
            | none => pure ()
          if th.traceId == 38 then
            match th.mobj with
            | some mo =>
              found38 := true
              ok := (← assert "tic 81 tid38 state=176" (mo.state == 176)) && ok
            | none => pure ()
        | none => pure ()
        hi := hi + 1
      ok := (← assert "tic 81 player mo present" foundP) && ok
      ok := (← assert "tic 81 tid38 present" found38) && ok
    match recs[85]? with
    | none => ok := (← assert "tic 85 present" false) && ok
    | some rec =>
      match rec.players[0]? with
      | none => ok := (← assert "tic 85 player0" false) && ok
      | some p =>
        ok := (← assert "tic 85 ammo0=49" (p.ammo0 == (49 : Int32).toUInt32)) && ok
      let mut found231 := false
      let mut found157b := false
      let mut hi : Nat := 0
      while hi < rec.thinkers.size do
        match rec.thinkers[hi]? with
        | some th =>
          if th.traceId == 231 then
            match th.mobj with
            | some mo =>
              found231 := true
              ok := (← assert "tic 85 tid231 type=38" (mo.type_ == 38)) && ok
              ok := (← assert "tic 85 tid231 state=92" (mo.state == 92)) && ok
              ok := (← assert "tic 85 tid231 tics=7"
                (mo.tics == (7 : Int32).toUInt32)) && ok
              ok := (← assert "tic 85 tid231 flags" (mo.flags == 0x10)) && ok
              ok := (← assert "tic 85 tid231 x"
                (mo.x == (-11247242 : Int32).toUInt32)) && ok
              ok := (← assert "tic 85 tid231 y"
                (mo.y == (-3733580 : Int32).toUInt32)) && ok
              ok := (← assert "tic 85 tid231 z"
                (mo.z == (2028214 : Int32).toUInt32)) && ok
              ok := (← assert "tic 85 tid231 momz"
                (mo.momz == (65536 : Int32).toUInt32)) && ok
            | none => pure ()
          if th.traceId == 157 then
            match th.mobj with
            | some mo =>
              found157b := true
              ok := (← assert "tic 85 tid157 hp=25"
                (mo.health == (25 : Int32).toUInt32)) && ok
              ok := (← assert "tic 85 tid157 state=220" (mo.state == 220)) && ok
              ok := (← assert "tic 85 tid157 flags" (mo.flags == 0x4000c6)) && ok
            | none => pure ()
        | none => pure ()
        hi := hi + 1
      ok := (← assert "tic 85 tid231 BLOOD present" found231) && ok
      ok := (← assert "tic 85 tid157 present" found157b) && ok
    match recs[87]? with
    | none => ok := (← assert "tic 87 present" false) && ok
    | some rec =>
      ok := (← assert "tic 87 nsec=0" (rec.sectors.size == 0)) && ok
      let mut found231b := false
      let mut found157c := false
      let mut hi : Nat := 0
      while hi < rec.thinkers.size do
        match rec.thinkers[hi]? with
        | some th =>
          if th.traceId == 231 then
            match th.mobj with
            | some mo =>
              found231b := true
              ok := (← assert "tic 87 blood tics=5"
                (mo.tics == (5 : Int32).toUInt32)) && ok
              ok := (← assert "tic 87 blood momz"
                (mo.momz == (-131072 : Int32).toUInt32)) && ok
            | none => pure ()
          if th.traceId == 157 then
            match th.mobj with
            | some mo =>
              found157c := true
              ok := (← assert "tic 87 tid157 state=221" (mo.state == 221)) && ok
              ok := (← assert "tic 87 tid157 tics=3"
                (mo.tics == (3 : Int32).toUInt32)) && ok
            | none => pure ()
        | none => pure ()
        hi := hi + 1
      ok := (← assert "tic 87 blood present" found231b) && ok
      ok := (← assert "tic 87 tid157 present" found157c) && ok

    -- P2c-vi first vertical door goldens ------------------------------------
    match recs[87]?, recs[88]? with
    | some r87, some r88 =>
      ok := (← assert "tic 88 rndindex Δ=+2"
        (r88.rndindex == r87.rndindex + 2)) && ok
      ok := (← assert "tic 88 nsec=1" (r88.sectors.size == 1)) && ok
      match r88.sectors[0]? with
      | none => ok := (← assert "tic 88 sector rec" false) && ok
      | some sec =>
        ok := (← assert "tic 88 sec71" (sec.sectorIndex == 71)) && ok
        ok := (← assert "tic 88 sec71 ceil=131072"
          (sec.ceilingheight == (131072 : Int32).toUInt32)) && ok
      let mut found232 := false
      let mut hi : Nat := 0
      while hi < r88.thinkers.size do
        match r88.thinkers[hi]? with
        | some th =>
          if th.traceId == 232 then
            found232 := true
            ok := (← assert "tic 88 tid232 THF_VERTICALDOOR" (th.func == 2)) && ok
        | none => pure ()
        hi := hi + 1
      ok := (← assert "tic 88 tid232 present" found232) && ok
    | _, _ =>
      ok := (← assert "tic 87/88 present for door golden" false) && ok
    match recs[91]? with
    | none => ok := (← assert "tic 91 present" false) && ok
    | some rec =>
      ok := (← assert "tic 91 nsec=1" (rec.sectors.size == 1)) && ok
      match rec.sectors[0]? with
      | none => ok := (← assert "tic 91 sector rec" false) && ok
      | some sec =>
        ok := (← assert "tic 91 sec71 ceil=524288"
          (sec.ceilingheight == (524288 : Int32).toUInt32)) && ok
      let mut found231d := false
      let mut hi : Nat := 0
      while hi < rec.thinkers.size do
        match rec.thinkers[hi]? with
        | some th =>
          if th.traceId == 231 then
            match th.mobj with
            | some mo =>
              found231d := true
              ok := (← assert "tic 91 blood tics=1"
                (mo.tics == (1 : Int32).toUInt32)) && ok
            | none => pure ()
        | none => pure ()
        hi := hi + 1
      ok := (← assert "tic 91 tid231 present" found231d) && ok
    ok := (← checkP2cViiGoldens recs ok) && ok
    ok := (← checkP2cViiiGoldens recs ok) && ok
    ok := (← checkP2cIxGoldens recs ok) && ok

  let digCmp ← IO.Process.output {
    cmd := "python3"
    args := #[
      "-c",
      "import sys; sys.path.insert(0, sys.argv[1]);\n" ++
      "from tracelib import read_digest_stream\n" ++
      "_, c = read_digest_stream(sys.argv[2]); _, r = read_digest_stream(sys.argv[3])\n" ++
      "assert len(c) >= 135, len(c)\n" ++
      "bad = [i for i in range(135) if c[i] != r[i]]\n" ++
      "print('OK' if not bad else 'BAD ' + str(bad[:5])); sys.exit(0 if not bad else 1)\n",
      (root / "tools").toString,
      candDig.toString,
      (root / "fixtures" / "demo1.dig").toString
    ]
  }
  IO.println digCmp.stdout
  if !digCmp.stderr.isEmpty then IO.eprintln digCmp.stderr
  ok := (← assert "digests 0..134 match fixture"
    (digCmp.exitCode == 0 && digCmp.stdout.contains "OK")) && ok

  if ok then
    IO.println "ALL DEMO1 DIGEST P2c-ix CHECKS PASSED (K=134)"
    pure 0
  else
    IO.eprintln "SOME DEMO1 DIGEST CHECKS FAILED"
    pure 1
