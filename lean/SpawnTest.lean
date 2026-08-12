import Doom.Playsim
import Doom.Playsim.Angle
import Doom.Playsim.Bsp
import Doom.Playsim.Demo
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Random
import Doom.Playsim.Spawn
import Doom.Playsim.Thinker
import Doom.Wad
import Doom.Harness.Fnv
import Doom.Harness.TraceFormat
import Doom.Playsim.SpawnExpect

/-!
P2a-i behavior + unit tests: DEMO1 header + E1M5 spawn (no ticker).
-/

open Doom.Wad
open Doom.Playsim.Angle
open Doom.Playsim.Bsp
open Doom.Playsim.Demo
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Random
open Doom.Playsim.Spawn
open Doom.Playsim.Thinker
open Doom.Playsim.SpawnExpect
open Doom.Harness.Fnv
open Doom.Harness.TraceFormat

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  let parts := cwd.components
  match parts.getLast? with
  | some "lean" =>
    match cwd.parent with
    | some p => pure p
    | none => pure cwd
  | _ => pure cwd

def loadIwad : IO WadDirectory := do
  let root ← defaultRoot
  loadFile (root / "fixtures" / "wads" / "doom1.wad")

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

/-- FNV of packed state fields (matches `scripts/gen_info.py`). -/
def fnvStates : UInt64 :=
  Id.run do
    let mut acc := ByteArray.emptyWithCapacity (states.size * 28)
    for st in states do
      acc := Doom.Harness.TraceFormat.pushU32LE acc st.sprite
      acc := Doom.Harness.TraceFormat.pushU32LE acc st.frame
      acc := Doom.Harness.TraceFormat.pushU32LE acc st.tics.toUInt32
      acc := Doom.Harness.TraceFormat.pushU32LE acc st.action
      acc := Doom.Harness.TraceFormat.pushU32LE acc st.nextstate
      acc := Doom.Harness.TraceFormat.pushU32LE acc st.misc1.toUInt32
      acc := Doom.Harness.TraceFormat.pushU32LE acc st.misc2.toUInt32
    pure (fnv1a64 acc)

def fnvMobjinfo : UInt64 :=
  Id.run do
    let mut acc := ByteArray.emptyWithCapacity (mobjinfo.size * 92)
    for mi in mobjinfo do
      for v in #[
        mi.doomednum.toUInt32, mi.spawnstate, mi.spawnhealth.toUInt32, mi.seestate,
        mi.seesound.toUInt32, mi.reactiontime.toUInt32, mi.attacksound.toUInt32,
        mi.painstate, mi.painchance.toUInt32, mi.painsound.toUInt32,
        mi.meleestate, mi.missilestate, mi.deathstate, mi.xdeathstate,
        mi.deathsound.toUInt32, mi.speed.toUInt32, mi.radius.toUInt32, mi.height.toUInt32,
        mi.mass.toUInt32, mi.damage.toUInt32, mi.activesound.toUInt32, mi.flags, mi.raisestate
      ] do
        acc := Doom.Harness.TraceFormat.pushU32LE acc v
    pure (fnv1a64 acc)

def fnvSprnames : UInt64 :=
  Id.run do
    let mut acc := ByteArray.emptyWithCapacity (sprnames.size * 4)
    for n in sprnames do
      acc := Doom.Harness.TraceFormat.pushU32LE acc n
    pure (fnv1a64 acc)

def thfHist (gs : GameState) : (Nat × Nat × Nat × Nat) :=
  Id.run do
    let mut mobjN := 0
    let mut lf := 0
    let mut st := 0
    let mut gl := 0
    for th in gs.thinkers do
      if th.func == THF_MOBJ then mobjN := mobjN + 1
      else if th.func == THF_LIGHTFLASH then lf := lf + 1
      else if th.func == THF_STROBEFLASH then st := st + 1
      else if th.func == THF_GLOW then gl := gl + 1
    pure (mobjN, lf, st, gl)

def main (_args : List String) : IO UInt32 := do
  let mut ok := true

  -- generated table checksums / counts ---------------------------------
  ok := (← assert "NUMSTATES" (NUMSTATES == 967)) && ok
  ok := (← assert "NUMMOBJTYPES" (NUMMOBJTYPES == 137)) && ok
  ok := (← assert "NUMSPRITES" (NUMSPRITES == 138)) && ok
  ok := (← assert "states.size" (states.size == 967)) && ok
  ok := (← assert "mobjinfo.size" (mobjinfo.size == 137)) && ok
  ok := (← assert "sprnames.size" (sprnames.size == 138)) && ok
  ok := (← assert "fnv states"
    (fnvStates == (0x48211e9b8fba9620 : UInt64))) && ok
  ok := (← assert "fnv mobjinfo"
    (fnvMobjinfo == (0x57e1f97f8ae9a2d8 : UInt64))) && ok
  ok := (← assert "fnv sprnames"
    (fnvSprnames == (0x8d064b519dd2a2a6 : UInt64))) && ok

  -- unit: skill filter -------------------------------------------------
  ok := (← assert "skillBit medium" (skillBit sk_medium == 2)) && ok
  ok := (← assert "skillBit baby" (skillBit sk_baby == 1)) && ok
  ok := (← assert "skillBit nightmare" (skillBit sk_nightmare == 4)) && ok
  ok := (← assert "skillAllows medium bit2"
    (skillAllows 2 sk_medium && !skillAllows 1 sk_medium)) && ok
  ok := (← assert "isDoom2OnlyThing arachnotron" (isDoom2OnlyThing 68)) && ok
  ok := (← assert "isDoom2OnlyThing zombieman false" (!isDoom2OnlyThing 3004)) && ok

  -- unit: spawn-tics formula -------------------------------------------
  let rng0 := clearRandom
  let (t1, rng1) := randomizeSpawnTics 10 rng0
  let (rExpect, _) := pRandom rng0
  ok := (← assert "spawn tics formula" (t1 == 1 + (rExpect % 10))) && ok
  ok := (← assert "spawn tics zero" ((randomizeSpawnTics 0 rng1).1 == 0)) && ok

  -- DEMO1 header -------------------------------------------------------
  let wad ← loadIwad
  match checkNumForName wad "DEMO1" with
  | none =>
    ok := (← assert "DEMO1 lump" false) && ok
  | some demoIdx =>
    match lumpData wad demoIdx with
    | Except.error e =>
      ok := (← assert s!"DEMO1 bytes ({e})" false) && ok
    | Except.ok demoBytes =>
      match parseHeader demoBytes with
      | Except.error e =>
        ok := (← assert s!"DEMO1 header ({e})" false) && ok
      | Except.ok hdr =>
        ok := (← assert "DEMO1 version" (hdr.version == 109)) && ok
        ok := (← assert "DEMO1 skill" (hdr.skill == 2)) && ok
        ok := (← assert "DEMO1 episode" (hdr.episode == 1)) && ok
        ok := (← assert "DEMO1 map" (hdr.map == 5)) && ok
        ok := (← assert "DEMO1 deathmatch" (hdr.deathmatch == 0)) && ok
        ok := (← assert "DEMO1 respawn" (hdr.respawn == 0)) && ok
        ok := (← assert "DEMO1 fast" (hdr.fast == 0)) && ok
        ok := (← assert "DEMO1 nomonsters" (hdr.nomonsters == 0)) && ok
        ok := (← assert "DEMO1 consoleplayer" (hdr.consoleplayer == 0)) && ok
        ok := (← assert "DEMO1 playeringame"
          (hdr.playeringame == #[true, false, false, false])) && ok

        match loadMap wad "E1M5" with
        | Except.error e =>
          ok := (← assert s!"load E1M5 ({e})" false) && ok
        | Except.ok level =>
          -- BSP: player start thing type 1
          let mut playerThing : Option Thing := none
          for t in level.things do
            if t.typeId == 1 && playerThing.isNone then
              playerThing := some t
          match playerThing with
          | none =>
            ok := (← assert "E1M5 player start" false) && ok
          | some pt =>
            let px := pt.x <<< 16
            let py := pt.y <<< 16
            match pointInSubsector level px py with
            | Except.error e =>
              ok := (← assert s!"player BSP ({e})" false) && ok
            | Except.ok ss =>
              ok := (← assert "player subsector found" (ss.toNat < level.subsectors.size)) && ok

          match setupSpawnedLevel level (hdr.skill.toUInt32.toInt32) hdr.playeringame
              hdr.consoleplayer.toNat with
          | Except.error e =>
            ok := (← assert s!"spawn E1M5 ({e})" false) && ok
          | Except.ok gs =>
            ok := (← assert "thinker_count" (gs.thinkers.size == 230)) && ok
            let (mN, lf, st, gl) := thfHist gs
            ok := (← assert "THF mobj" (mN == 216)) && ok
            ok := (← assert "THF lightflash" (lf == 2)) && ok
            ok := (← assert "THF strobeflash" (st == 4)) && ok
            ok := (← assert "THF glow" (gl == 8)) && ok

            -- trace ids 1..230 in insertion order
            let mut idsOk := gs.thinkers.size == 230
            let mut i : Nat := 0
            while i < gs.thinkers.size do
              match gs.thinkers[i]? with
              | some th =>
                if th.traceId != (i + 1).toUInt32 then idsOk := false
              | none => idsOk := false
              i := i + 1
            ok := (← assert "trace_ids 1..230" idsOk) && ok

            match gs.players[0]? with
            | none =>
              ok := (← assert "player0 present" false) && ok
            | some p =>
              ok := (← assert "player mo_trace_id" (p.mo >= 0)) && ok
              match gs.mobjs[p.mo.toNatClampNeg]? with
              | none =>
                ok := (← assert "player mobj" false) && ok
              | some (mo : Mobj) =>
                ok := (← assert "player mo.traceId" (mo.traceId == 1)) && ok
                ok := (← assert "player type MT_PLAYER" (mo.typeId == 0)) && ok
                ok := (← assert "player x" (mo.x == (-14680064 : Int32))) && ok
                ok := (← assert "player y" (mo.y == (-40894464 : Int32))) && ok
                ok := (← assert "player z" (mo.z == 0)) && ok
                ok := (← assert "player angle" (mo.angle == (1073741824 : UInt32))) && ok
              ok := (← assert "readyweapon pistol" (p.readyweapon == 1)) && ok
              ok := (← assert "pendingweapon wp_nochange"
                (p.pendingweapon == wp_nochange && p.pendingweapon == 10)) && ok
              ok := (← assert "player health" (p.health == 100)) && ok
              ok := (← assert "ammo"
                (p.ammo == #[(50 : Int32), 0, 0, 0])) && ok
              ok := (← assert "weaponowned fist+pistol"
                (p.weaponowned[0]? == some 1 && p.weaponowned[1]? == some 1)) && ok

            -- first non-player / last mobj types
            let mut firstNon : Option Int32 := none
            let mut lastTy : Option Int32 := none
            for th in gs.thinkers do
              if th.func == THF_MOBJ then
                match gs.mobjs[th.payload.toNat]? with
                | some (mo : Mobj) =>
                  if mo.typeId != 0 && firstNon.isNone then
                    firstNon := some mo.typeId
                  lastTy := some mo.typeId
                | none => pure ()
            ok := (← assert "first non-player type MT_MISC4"
              (firstNon == some 47)) && ok
            ok := (← assert "last mobj type MT_MISC23" (lastTy == some 70)) && ok

            -- 216-mobj fixture cross-check (spawn-invariant fields)
            let mut mobjThinkers : Array (UInt32 × Int32 × Int32 × Int32 × Int32 × UInt32) := #[]
            for th in gs.thinkers do
              if th.func == THF_MOBJ then
                match gs.mobjs[th.payload.toNat]? with
                | some (mo : Mobj) =>
                  mobjThinkers := mobjThinkers.push
                    (mo.traceId, mo.typeId, mo.x, mo.y, mo.z, mo.angle)
                | none => pure ()
            ok := (← assert "mobj thinker count" (mobjThinkers.size == 216)) && ok
            ok := (← assert "expect fixture size" (expects.size == 216)) && ok
            let mut crossOk := mobjThinkers.size == expects.size
            let mut j : Nat := 0
            while j < mobjThinkers.size && j < expects.size do
              match mobjThinkers[j]?, expects[j]? with
              | some (tid, ty, x, y, z, ang), some e =>
                if !(tid == e.traceId && ty == e.typeId && x == e.x && y == e.y
                    && z == e.z && ang == e.angle) then
                  if crossOk then
                    IO.eprintln s!"mobj mismatch at {j}: got tid={tid} ty={ty} xyz=({x},{y},{z}) ang={ang} expect tid={e.traceId} ty={e.typeId}"
                  crossOk := false
              | _, _ => crossOk := false
              j := j + 1
            ok := (← assert "216-mobj fixture cross-check" crossOk) && ok

            -- post-spawn prndindex golden (spawn-only; no ticker).
            -- Observed from Lean spawn of DEMO1/E1M5: 120.
            -- Must be < fixture tic0 prndindex=121 (tic0 includes first-tick draws).
            -- Draws: each mobj lastlook; non-player tics>0 reroll; lightflash + unsync strobe.
            let prnd := gs.rng.prndindex
            IO.println s!"INFO: post-spawn prndindex={prnd}"
            ok := (← assert "prndindex < 121" (prnd < 121)) && ok
            ok := (← assert "prndindex golden" (prnd == (120 : UInt32))) && ok

            -- regression: deathmatch must not spawn coop player starts
            let gsDM := { gs with deathmatch := true }
            let pThing : Thing := { x := 0, y := 0, angle := 90, typeId := 1, options := 7 }
            match spawnMapThing gsDM pThing with
            | Except.error e =>
              ok := (← assert s!"deathmatch skip ({e})" false) && ok
            | Except.ok gs2 =>
              ok := (← assert "deathmatch skips player start"
                (gs2.thinkers.size == gs.thinkers.size && gs2.mobjs.size == gs.mobjs.size)) && ok

  if ok then
    IO.println "ALL SPAWN TESTS PASSED"
    pure 0
  else
    IO.eprintln "SOME SPAWN TESTS FAILED"
    pure 1
