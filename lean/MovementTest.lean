import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Map
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.PlayerThink
import Doom.Playsim.Random
import Doom.Playsim.Sound
import Doom.Playsim.Tables
import Doom.Playsim.Think
import Doom.Wad

/-!
P2c-iv implementation tests: `P_HitSlideLine` / `P_InterceptVector` units,
plus retained P2c-ii Z/thrust/friction/opening and P2c-i coverage.
-/

open Doom.Wad
open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Level
open Doom.Playsim.Map
open Doom.Playsim.MapUtil
open Doom.Playsim.PlayerThink
open Doom.Playsim.Random
open Doom.Playsim.Sound
open Doom.Playsim.Tables
open Doom.Playsim.Think

-- Qualify Mobj / Player / GameState helpers to avoid `empty` / `arrSet` clashes.

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

def loadMap (wad : WadDirectory) (label : String) : Except String LevelData := do
  match checkNumForName wad label with
  | none => throw s!"missing {label}"
  | some idx =>
    let things ← mapLumpData wad idx ML_THINGS
    let linedefs ← mapLumpData wad idx ML_LINEDEFS
    let sidedefs ← mapLumpData wad idx ML_SIDEDEFS
    let vertexes ← mapLumpData wad idx ML_VERTEXES
    let segs ← mapLumpData wad idx ML_SEGS
    let ssectors ← mapLumpData wad idx ML_SSECTORS
    let nodes ← mapLumpData wad idx ML_NODES
    let sectors ← mapLumpData wad idx ML_SECTORS
    let reject ← mapLumpData wad idx ML_REJECT
    let blockmap ← mapLumpData wad idx ML_BLOCKMAP
    buildLevel things linedefs sidedefs vertexes segs ssectors nodes sectors reject blockmap

/-- Minimal GS with one player mobj for thrust / Z tests. -/
def thrustFixture : Doom.Playsim.GameState.GameState :=
  let emptyLevel : LevelData := {
    vertexes := #[]
    sectors := #[]
    sides := #[]
    lines := #[]
    segs := #[]
    subsectors := #[]
    nodes := #[]
    things := #[]
    blockmap := { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }
    reject := ByteArray.empty
  }
  let gs0 := Doom.Playsim.GameState.initFromLevel emptyLevel 2 #[true, false, false, false] 0
  let mo := {
    Doom.Playsim.Mobj.empty with
    player := 0
    floorz := 0
    ceilingz := 128 * FRACUNIT
    height := 56 * FRACUNIT
  }
  let p := {
    Doom.Playsim.Player.empty with
    mo := 0
    playerstate := Doom.Playsim.Player.PST_LIVE
    viewheight := Doom.Playsim.Player.VIEWHEIGHT
  }
  { gs0 with
    mobjs := #[mo]
    players := Doom.Playsim.GameState.arrSet gs0.players 0 p
  }

/-- GS with player above floor (airborne Z). -/
def airborneFixture (z momz floorz : Int32) : Doom.Playsim.GameState.GameState :=
  let gs := thrustFixture
  match gs.mobjs[0]? with
  | none => gs
  | some mo =>
    { gs with mobjs := #[ { mo with z, momz, floorz } ] }

def main (_args : List String) : IO UInt32 := do
  let mut ok := true

  -- FRICTION FixedMul samples (from DEMO1 tic-26 moms) ----------------
  ok := (← assert "FixedMul FRICTION 801403"
    (fixedMul (801403 : Int32) FRICTION == (726271 : Int32))) && ok
  ok := (← assert "FixedMul FRICTION -47542"
    (fixedMul (-47542 : Int32) FRICTION == (-43085 : Int32))) && ok

  -- First-fall momz: momz==0 airborne → -GRAVITY*2 ------------------------------
  match zMovement (airborneFixture 0 0 (-16 * FRACUNIT)) 0 with
  | Except.error e =>
    ok := (← assert s!"zMovement first-fall ({e})" false) && ok
  | Except.ok gs =>
    match gs.mobjs[0]? with
    | none => ok := (← assert "zMovement first-fall mo" false) && ok
    | some mo =>
      ok := (← assert "first-fall momz=-GRAVITY*2"
        (mo.momz == -GRAVITY * 2)) && ok
      ok := (← assert "first-fall z unchanged (momz was 0)"
        (mo.z == 0)) && ok

  -- Gravity decrement when already falling ------------------------------------
  match zMovement (airborneFixture (-131072) (-131072) (-16 * FRACUNIT)) 0 with
  | Except.error e =>
    ok := (← assert s!"zMovement gravity dec ({e})" false) && ok
  | Except.ok gs =>
    match gs.mobjs[0]? with
    | none => ok := (← assert "gravity dec mo" false) && ok
    | some mo =>
      ok := (← assert "gravity momz -= GRAVITY"
        (mo.momz == -131072 - GRAVITY)) && ok
      ok := (← assert "gravity z += prior momz"
        (mo.z == -131072 + -131072)) && ok

  -- Smooth step-up: player z < floorz adjusts viewheight / deltaviewheight ----
  let stepFloor : Int32 := 0
  let stepZ : Int32 := -8 * FRACUNIT
  let stepGs0 := airborneFixture stepZ 0 stepFloor
  match zMovement stepGs0 0 with
  | Except.error e =>
    ok := (← assert s!"zMovement step-up ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]?, gs.mobjs[0]? with
    | some pl, some mo =>
      let expectedVh := Doom.Playsim.Player.VIEWHEIGHT - (stepFloor - stepZ)
      ok := (← assert "step-up viewheight"
        (pl.viewheight == expectedVh)) && ok
      ok := (← assert "step-up deltaviewheight"
        (pl.deltaviewheight == (Doom.Playsim.Player.VIEWHEIGHT - expectedVh) >>> 3)) && ok
      -- momz was 0 so floor hit clamps z without hard-land; no gravity branch.
      ok := (← assert "step-up z clamped to floorz" (mo.z == stepFloor)) && ok
      ok := (← assert "step-up momz stays 0" (mo.momz == 0)) && ok
    | _, _ =>
      ok := (← assert "step-up player/mo" false) && ok

  -- Soft floor clamp (impact momz > -GRAVITY*8): no RNG, momz=0, z=floorz ------
  let softImpact : Int32 := -393216  -- DEMO1 land; > -524288
  let softFloor : Int32 := -1048576
  -- Start above floor so z+momz crosses floorz.
  let softGs0 :=
    let g := airborneFixture (softFloor - softImpact) softImpact softFloor
    g
  let rngBefore := softGs0.rng.rndindex
  match zMovement softGs0 0 with
  | Except.error e =>
    ok := (← assert s!"zMovement soft land ({e})" false) && ok
  | Except.ok gs =>
    match gs.mobjs[0]? with
    | none => ok := (← assert "soft land mo" false) && ok
    | some mo =>
      ok := (← assert "soft land momz=0" (mo.momz == 0)) && ok
      ok := (← assert "soft land z=floorz" (mo.z == softFloor)) && ok
      ok := (← assert "soft land no sfx RNG"
        (gs.rng.rndindex == rngBefore)) && ok

  -- Hard land: deltaviewheight = momz>>3 + one sfx_oof pitch draw --------------
  let hardMomz : Int32 := -GRAVITY * 8 - 1
  let hardFloor : Int32 := 0
  let hardGs0 :=
    let g := airborneFixture (hardFloor - hardMomz) hardMomz hardFloor
    { g with rng := { g.rng with rndindex := 10 } }
  match zMovement hardGs0 0 with
  | Except.error e =>
    ok := (← assert s!"zMovement hard land ({e})" false) && ok
  | Except.ok gs =>
    match gs.mobjs[0]?, gs.players[0]? with
    | some mo, some pl =>
      ok := (← assert "hard land momz=0" (mo.momz == 0)) && ok
      ok := (← assert "hard land z=floorz" (mo.z == hardFloor)) && ok
      ok := (← assert "hard land deltaviewheight"
        (pl.deltaviewheight == hardMomz >>> 3)) && ok
      ok := (← assert "hard land sfx_oof RNG +1"
        (gs.rng.rndindex == 11)) && ok
    | _, _ =>
      ok := (← assert "hard land mo/player" false) && ok

  -- sfx_oof pitch helper: one M_Random; itemup: none --------------------------
  let rng0 : RandomState := { prndindex := 0, rndindex := 50 }
  ok := (← assert "sfx_oof pitch draw"
    ((startSoundPitchRng rng0 sfx_oof).rndindex == 51)) && ok
  ok := (← assert "sfx_itemup no pitch draw"
    ((startSoundPitchRng rng0 sfx_itemup).rndindex == 50)) && ok

  -- Airborne calcHeight: final viewz = z + viewheight (C overwrite order) -----
  let airGs := airborneFixture (-131072) (-196608) (-1048576)
  match calcHeight airGs 0 false with
  | Except.error e =>
    ok := (← assert s!"calcHeight airborne ({e})" false) && ok
  | Except.ok gs =>
    match gs.players[0]?, gs.mobjs[0]? with
    | some pl, some mo =>
      ok := (← assert "airborne viewz = z+viewheight"
        (pl.viewz == mo.z + pl.viewheight)) && ok
    | _, _ =>
      ok := (← assert "airborne calcHeight player" false) && ok

  -- P_Thrust at angle 0: cos=finesine[2048], sin=finesine[0] ----------
  let move : Int32 := 50 * 2048
  match finecosine[0]?, finesine[0]? with
  | some cos0, some sin0 =>
    match thrust thrustFixture 0 (0 : UInt32) move with
    | Except.error e =>
      ok := (← assert s!"P_Thrust angle0 ({e})" false) && ok
    | Except.ok gs =>
      match gs.mobjs[0]? with
      | none => ok := (← assert "P_Thrust angle0 mo" false) && ok
      | some mo =>
        ok := (← assert "P_Thrust angle0 momx"
          (mo.momx == fixedMul move cos0)) && ok
        ok := (← assert "P_Thrust angle0 momy"
          (mo.momy == fixedMul move sin0)) && ok
  | _, _ =>
    ok := (← assert "fine tables present" false) && ok

  -- P_Thrust at ANG90: fine index 2048 --------------------------------
  match finecosine[2048]?, finesine[2048]? with
  | some cos90, some sin90 =>
    match thrust thrustFixture 0 ANG90 move with
    | Except.error e =>
      ok := (← assert s!"P_Thrust ANG90 ({e})" false) && ok
    | Except.ok gs =>
      match gs.mobjs[0]? with
      | none => ok := (← assert "P_Thrust ANG90 mo" false) && ok
      | some mo =>
        ok := (← assert "P_Thrust ANG90 momx"
          (mo.momx == fixedMul move cos90)) && ok
        ok := (← assert "P_Thrust ANG90 momy"
          (mo.momy == fixedMul move sin90)) && ok
        ok := (← assert "P_Thrust ANG90 nearly north"
          (mo.momx == 0 || wabs mo.momx < 100)) && ok
  | _, _ =>
    ok := (← assert "fine tables ANG90" false) && ok

  -- P_LineOpening on E1M5 ---------------------------------------------
  let root ← defaultRoot
  let wad ← loadFile (root / "fixtures" / "wads" / "doom1.wad")
  match loadMap wad "E1M5" with
  | Except.error e =>
    ok := (← assert s!"load E1M5 ({e})" false) && ok
  | Except.ok level =>
    let gs := Doom.Playsim.GameState.initFromLevel level 3 #[true, false, false, false] 0
    -- line 706: floors -16 / 0 → openbottom=0, lowfloor=-16*FRACUNIT
    match level.lines[706]? with
    | none => ok := (← assert "line 706 present" false) && ok
    | some ld =>
      match lineOpening gs ld with
      | Except.error e =>
        ok := (← assert s!"lineOpening 706 ({e})" false) && ok
      | Except.ok op =>
        ok := (← assert "line706 openbottom=0" (op.openbottom == 0)) && ok
        ok := (← assert "line706 lowfloor=-16*FRACUNIT"
          (op.lowfloor == (-16 : Int32) * FRACUNIT)) && ok
        ok := (← assert "line706 opentop=72*FRACUNIT"
          (op.opentop == (72 : Int32) * FRACUNIT)) && ok
        ok := (← assert "line706 openrange"
          (op.openrange == op.opentop - op.openbottom)) && ok
    -- flat 2-sided line 49: floors 0/0 ceils 72/72
    match level.lines[49]? with
    | none => ok := (← assert "line 49 present" false) && ok
    | some ld =>
      match lineOpening gs ld with
      | Except.error e =>
        ok := (← assert s!"lineOpening 49 ({e})" false) && ok
      | Except.ok op =>
        ok := (← assert "line49 openbottom=0" (op.openbottom == 0)) && ok
        ok := (← assert "line49 lowfloor=0" (op.lowfloor == 0)) && ok
        ok := (← assert "line49 opentop=72*FRACUNIT"
          (op.opentop == (72 : Int32) * FRACUNIT)) && ok
    -- Blockmap link round-trip at player-ish coords from DEMO1 spawn
    let px : Int32 := -14680064
    let py : Int32 := -40894464
    let mo := {
      Doom.Playsim.Mobj.empty with
      x := px, y := py, radius := 16 * FRACUNIT, height := 56 * FRACUNIT
    }
    let gs1 := { gs with mobjs := #[mo] }
    match setThingPosition gs1 0 with
    | Except.error e =>
      ok := (← assert s!"setThingPosition ({e})" false) && ok
    | Except.ok gs2 =>
      let bmap := gs2.level.blockmap
      let bx := ashrMapBlock (px - bmap.originX)
      let byCoord := ashrMapBlock (py - bmap.originY)
      let bi := (byCoord * bmap.width + bx).toNatClampNeg
      let head := match gs2.blocklinks[bi]? with | some v => v | none => (-2 : Int32)
      ok := (← assert "blocklinks head after set" (head == 0)) && ok
      match gs2.mobjs[0]? with
      | none => ok := (← assert "mo after set" false) && ok
      | some mo2 =>
        ok := (← assert "bprev null after set" (mo2.bprev == -1)) && ok
      match unsetThingPosition gs2 0 with
      | Except.error e =>
        ok := (← assert s!"unsetThingPosition ({e})" false) && ok
      | Except.ok gs3 =>
        let head3 := match gs3.blocklinks[bi]? with | some v => v | none => (-2 : Int32)
        ok := (← assert "blocklinks empty after unset" (head3 == -1)) && ok

  -- P_InterceptVector (alias of Sight.interceptVector2 semantics) -------------
  let v2 : Divline := { x := 0, y := 0, dx := 100 * 65536, dy := 0 }
  let v1 : Divline := { x := 50 * 65536, y := -10 * 65536, dx := 0, dy := 20 * 65536 }
  ok := (← assert "interceptVector crossing"
    (interceptVector v2 v1 == 32768)) && ok
  ok := (← assert "interceptVector parallel den0"
    (interceptVector v2 { x := 0, y := 0, dx := 50 * 65536, dy := 0 } == 0)) && ok

  -- P_HitSlideLine H/V axis zeroing (E1M5 real linedefs) ---------------------
  let sl0 : SlideState := {
    slidemoIdx := 0
    bestslidefrac := 0
    bestslideline := 0
    secondslidefrac := 0
    secondslideline := 0
    tmxmove := 1000
    tmymove := 2000
  }
  match loadMap (← loadFile ((← defaultRoot) / "fixtures" / "wads" / "doom1.wad")) "E1M5" with
  | Except.error e =>
    ok := (← assert s!"load E1M5 for HitSlideLine ({e})" false) && ok
  | Except.ok level =>
    let gs := Doom.Playsim.GameState.initFromLevel level 3 #[true, false, false, false] 0
    let mut foundH : Option Line := none
    let mut foundV : Option Line := none
    let mut foundD : Option Line := none
    let mut li : Nat := 0
    while li < level.lines.size do
      match level.lines[li]? with
      | some ld =>
        if foundH.isNone && ld.slopetype == ST_HORIZONTAL then foundH := some ld
        if foundV.isNone && ld.slopetype == ST_VERTICAL then foundV := some ld
        if foundD.isNone && (ld.slopetype == ST_POSITIVE || ld.slopetype == ST_NEGATIVE) then
          foundD := some ld
      | none => pure ()
      li := li + 1
    match foundH with
    | none => ok := (← assert "horizontal line exists" false) && ok
    | some ld =>
      match hitSlideLine gs sl0 ld with
      | Except.error e => ok := (← assert s!"HitSlideLine H ({e})" false) && ok
      | Except.ok sl =>
        ok := (← assert "HitSlideLine H zeros momy" (sl.tmymove == 0)) && ok
        ok := (← assert "HitSlideLine H keeps momx" (sl.tmxmove == 1000)) && ok
    match foundV with
    | none => ok := (← assert "vertical line exists" false) && ok
    | some ld =>
      match hitSlideLine gs sl0 ld with
      | Except.error e => ok := (← assert s!"HitSlideLine V ({e})" false) && ok
      | Except.ok sl =>
        ok := (← assert "HitSlideLine V zeros momx" (sl.tmxmove == 0)) && ok
        ok := (← assert "HitSlideLine V keeps momy" (sl.tmymove == 2000)) && ok
    match foundD with
    | none => ok := (← assert "diagonal line exists" false) && ok
    | some ld =>
      match level.vertexes[ld.v1.toNat]? with
      | none => ok := (← assert "diag v1" false) && ok
      | some v1 =>
        let mo := {
          Doom.Playsim.Mobj.empty with
          x := v1.x
          y := v1.y - 2 * FRACUNIT
        }
        let gs2 := { gs with mobjs := #[mo] }
        let slD := { sl0 with tmxmove := FRACUNIT, tmymove := 0 }
        match hitSlideLine gs2 slD ld with
        | Except.error e =>
          ok := (← assert s!"HitSlideLine diag ({e})" false) && ok
        | Except.ok sl =>
          -- Hand-check vs C: reflection must keep |newlen| ≤ |mom| scale
          let before := aproxDistance (FRACUNIT : Int32) 0
          let after := aproxDistance sl.tmxmove sl.tmymove
          ok := (← assert "HitSlideLine diag |mom| not grown"
            (after <= before + 1)) && ok

  if ok then
    IO.println "movement-test: all passed"
    pure 0
  else
    IO.eprintln "movement-test: SOME FAILED"
    pure 1
