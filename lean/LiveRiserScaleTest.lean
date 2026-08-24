import Doom.Harness.DisplaySim
import Doom.Playsim.Angle
import Doom.Playsim.Demo
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Spawn
import Doom.Playsim.Tables
import Doom.Playsim.Tick
import Doom.Render.Bsp
import Doom.Render.Constants
import Doom.Render.Data
import Doom.Render.Seg
import Doom.Render.Types
import Doom.Render.Util
import Doom.Render.View
import Doom.Wad

/-!
# LiveRiserScaleTest

Live DEMO1 tic 35: stored north BROWNGRN riser (E1M5 linedef 707 / seg 418)
`rw_scale` / `pixlow` / column `yh` at x=160 vs C goldens from C formulas on
the live view and WAD geometry. Does not use stored Lean `rwScale` as the
expected-pix input.
-/

open Doom.Harness.DisplaySim
open Doom.Playsim.Angle
open Doom.Playsim.Demo
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Spawn
open Doom.Playsim.Tables
open Doom.Playsim.Tick
open Doom.Render.Bsp
open Doom.Render.Constants
open Doom.Render.Data
open Doom.Render.Seg
open Doom.Render.Types
open Doom.Render.Util
open Doom.Render.View
open Doom.Wad

private def e1m5SegIdx : Nat := 418
private def e1m5Linedef : UInt32 := 707
private def probeX : Int32 := 160
private def lastGametic : Nat := 35

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

private def loadMap (wad : WadDirectory) (label : String) : Except String LevelData := do
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

/-- C `R_ScaleFromGlobalAngle` including `num << detailshift` (`r_main.c`). -/
private def cScaleFromGlobalAngle (visangle viewangle rwNormalAngle : UInt32)
    (rwDistance projection : Int32) (detailshift : Nat) : Int32 :=
  let anglea := ANG90 + (visangle - viewangle)
  let angleb := ANG90 + (visangle - rwNormalAngle)
  let sinea := finesine.getD ((anglea >>> ANGLETOFINESHIFT.toUInt32).toNat) 0
  let sineb := finesine.getD ((angleb >>> ANGLETOFINESHIFT.toUInt32).toNat) 0
  let num : Int32 := (fixedMul projection sineb) <<< Int32.ofNat detailshift
  let den := fixedMul rwDistance sinea
  if den > (num >>> 16) then
    let scale0 := fixedDiv num den
    let scale1 := if scale0 > 64 * FRACUNIT then 64 * FRACUNIT else scale0
    if scale1 < 256 then 256 else scale1
  else
    64 * FRACUNIT

/-- C `R_StoreWallRange` distance + scale (`r_segs.c` L397–423). -/
private def cDistanceScale (viewx viewy : Int32) (viewangle : UInt32)
    (v1x v1y : Int32) (segAngle rwAngle1 : UInt32) (start stop : Int32)
    (xtoviewangle : Array UInt32) (projection : Int32) (detailshift : Nat) :
    Int32 × Int32 × Int32 :=
  let rwNormalAngle := segAngle + ANG90
  let offsetangle0 := cabs (rwNormalAngle.toInt32 - rwAngle1.toInt32)
  let offsetangle := if offsetangle0 > ANG90.toInt32 then ANG90.toInt32 else offsetangle0
  let distangle := ANG90 - offsetangle.toUInt32
  let hyp := pointToDist viewx viewy v1x v1y
  let sineval := finesine.getD ((distangle >>> ANGLETOFINESHIFT.toUInt32).toNat) 0
  let rwDistance := fixedMul hyp sineval
  let visStart := viewangle + xtoviewangle.getD (i32ToNat start) 0
  let rwScale :=
    cScaleFromGlobalAngle visStart viewangle rwNormalAngle rwDistance projection detailshift
  let rwScaleStep :=
    if stop > start then
      let visStop := viewangle + xtoviewangle.getD (i32ToNat stop) 0
      let scale2 :=
        cScaleFromGlobalAngle visStop viewangle rwNormalAngle rwDistance projection detailshift
      (scale2 - rwScale) / (stop - start)
    else
      (0 : Int32)
  (rwDistance, rwScale, rwScaleStep)

/-- C pixlow / pixlowstep after `worldlow >>= 4` (`r_segs.c` L688–700). -/
private def cPix (centery rwScale rwScaleStep world : Int32) : Int32 × Int32 :=
  let centeryfrac := centery <<< 16
  let world4 := world >>> 4
  let pix := (centeryfrac >>> 4) - fixedMul world4 rwScale
  let pixstep := -fixedMul rwScaleStep world4
  (pix, pixstep)

/-- C `yh = bottomfrac >> HEIGHTBITS`, then `floorclip[x]-1` (`r_segs.c` L234–237). -/
private def cYhAt (bottomfrac bottomstep start x floorclip : Int32) : Int32 :=
  let yh0 := (bottomfrac + (x - start) * bottomstep) >>> 12
  if yh0 >= floorclip then floorclip - 1 else yh0

private def runtimeFloor (gs : GameState) (secIdx : Int32) : Int32 :=
  match gs.sectors[i32ToNat secIdx]? with
  | some r => r.floorheight
  | none =>
    match gs.level.sectors[i32ToNat secIdx]? with
    | some s => s.floorheight
    | none => 0

private def isLiveRiser (rec : DrawSegRecord) (v1x v1y v2x v2y : Int32) : Bool :=
  rec.replayMeta.v1x == v1x && rec.replayMeta.v1y == v1y &&
    rec.replayMeta.v2x == v2x && rec.replayMeta.v2y == v2y &&
    rec.replayMeta.x1 <= probeX && rec.replayMeta.x2 >= probeX &&
    rec.storeRes.bottomtexture != 0 &&
    rec.storeRes.worldlow > rec.storeRes.worldbottom

private def findLiveRiser (drawSegs : Array DrawSegRecord) (v1x v1y v2x v2y : Int32) :
    Option DrawSegRecord :=
  Id.run do
    let mut found : Option DrawSegRecord := none
    let mut i := 0
    while i < drawSegs.size && found.isNone do
      match drawSegs[i]? with
      | some rec =>
        if isLiveRiser rec v1x v1y v2x v2y then
          found := some rec
      | none => pure ()
      i := i + 1
    found

private def runDemo1ToGametic (gs0 : GameState) (lastInclusive : Nat) :
    Except String GameState := Id.run do
  let mut gs := gs0
  let mut disp := initDisplay
  let mut err : Option String := none
  let mut g : Nat := 0
  while g <= lastInclusive && err.isNone do
    match gTicker { gs with gametic := g.toUInt32 } with
    | Except.error e => err := some e
    | Except.ok gs1 =>
      gs := gs1
      if g > 0 then
        let (disp', rng') := onFrame disp gs.rng gsLevel
        disp := disp'
        gs := { gs with rng := rng' }
    g := g + 1
  match err with
  | some e => Except.error e
  | none => Except.ok gs

private def dumpMismatch (label : String) (leanVal cVal : Int32) : IO Unit :=
  IO.eprintln s!"  {label}: lean={leanVal} c={cVal}"

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let root ← defaultRoot
  let wad ← loadFile (root / "fixtures" / "wads" / "doom1.wad")
  let demoBytes ←
    match checkNumForName wad "DEMO1" with
    | none =>
      ok := (← assert "DEMO1 lump" false) && ok
      pure ByteArray.empty
    | some idx =>
      match lumpData wad idx with
      | Except.error e =>
        ok := (← assert s!"DEMO1 bytes ({e})" false) && ok
        pure ByteArray.empty
      | Except.ok b => pure b
  if !ok then
    IO.eprintln "live-riser-scale-test: FAILURES"
    return 1
  match parseHeader demoBytes with
  | Except.error e =>
    IO.eprintln s!"live-riser-scale-test FAIL: demo header {e}"
    return 1
  | Except.ok hdr =>
    match loadMap wad s!"E{hdr.episode}M{hdr.map}" with
    | Except.error e =>
      IO.eprintln s!"live-riser-scale-test FAIL: load map {e}"
      return 1
    | Except.ok level =>
      match setupSpawnedLevel level (hdr.skill.toUInt32.toInt32) hdr.playeringame
          hdr.consoleplayer.toNat with
      | Except.error e =>
        IO.eprintln s!"live-riser-scale-test FAIL: spawn {e}"
        return 1
      | Except.ok gs0 =>
        let gs0 := {
          gs0 with
          demoBytes
          demoCursor := 13
          demoplayback := true
          gametic := 0
        }
        match runDemo1ToGametic gs0 lastGametic with
        | Except.error e =>
          IO.eprintln s!"live-riser-scale-test FAIL: ticker {e}"
          return 1
        | Except.ok gs =>
          match initData wad with
          | Except.error e =>
            IO.eprintln s!"live-riser-scale-test FAIL: initData {e}"
            return 1
          | Except.ok data =>
            match renderBspFromGame data wad gs Framebuffer.initBlack with
            | Except.error e =>
              IO.eprintln s!"live-riser-scale-test FAIL: renderBsp {e}"
              return 1
            | Except.ok frame =>
              match gs.level.segs[e1m5SegIdx]? with
              | none =>
                ok := (← assert "E1M5 seg 418 present" false) && ok
              | some seg =>
                ok := (← assert "seg 418 linedef 707" (seg.linedef == e1m5Linedef)) && ok
                match gs.level.vertexes[i32ToNat seg.v1.toInt32]?,
                      gs.level.vertexes[i32ToNat seg.v2.toInt32]? with
                | none, _ =>
                  ok := (← assert "seg 418 v1" false) && ok
                | _, none =>
                  ok := (← assert "seg 418 v2" false) && ok
                | some v1, some v2 =>
                  match findLiveRiser frame.drawSegs v1.x v1.y v2.x v2.y with
                  | none =>
                    ok := (← assert "stored L707/seg418 covering x=160" false) && ok
                    IO.eprintln s!"  drawSegIdx={frame.drawSegIdx} n={frame.drawSegs.size}"
                  | some rec =>
                    let start := rec.replayMeta.x1
                    let stop := rec.replayMeta.x2
                    let viewx := frame.view.viewx
                    let viewy := frame.view.viewy
                    let viewangle := frame.view.viewangle
                    let viewz := frame.viewz
                    let rwAngle1 := pointToAngle2 viewx viewy v1.x v1.y
                    let (cDist, cScale, cScaleStep) :=
                      cDistanceScale viewx viewy viewangle v1.x v1.y seg.angle rwAngle1
                        start stop frame.view.mapping.xtoviewangle
                        defaultProjection 0
                    let cWorldBottom := runtimeFloor gs seg.frontsector - viewz
                    let cWorldLow := runtimeFloor gs seg.backsector - viewz
                    let (cPixlow, cPixlowstep) :=
                      cPix defaultCentery cScale cScaleStep cWorldLow
                    let (cBottomfrac, cBottomstep) :=
                      cPix defaultCentery cScale cScaleStep cWorldBottom
                    let cYh := cYhAt cBottomfrac cBottomstep start probeX defaultViewheight
                    let leanYh :=
                      cYhAt rec.storeRes.bottomfrac rec.storeRes.bottomstep start probeX
                        defaultViewheight
                    let dx := probeX - start
                    let cPixlowAtX := cPixlow + dx * cPixlowstep
                    let leanPixlowAtX := rec.storeRes.pixlow + dx * rec.storeRes.pixlowstep
                    let cMid := (cPixlowAtX + Int32.ofNat (heightUnit - 1)) >>> 12
                    let leanMid := (leanPixlowAtX + Int32.ofNat (heightUnit - 1)) >>> 12
                    ok := (← assert "stored L707/seg418 covering x=160" true) && ok
                    ok := (← assert "worldlow"
                      (rec.storeRes.worldlow == cWorldLow)) && ok
                    ok := (← assert "worldbottom"
                      (rec.storeRes.worldbottom == cWorldBottom)) && ok
                    ok := (← assert "rwScale"
                      (rec.storeRes.rwScale == cScale)) && ok
                    ok := (← assert "rwScaleStep"
                      (rec.storeRes.rwScaleStep == cScaleStep)) && ok
                    ok := (← assert "pixlow"
                      (rec.storeRes.pixlow == cPixlow)) && ok
                    ok := (← assert "pixlowstep"
                      (rec.storeRes.pixlowstep == cPixlowstep)) && ok
                    ok := (← assert "yh at x=160" (leanYh == cYh)) && ok
                    ok := (← assert "mid at x=160" (leanMid == cMid)) && ok
                    let expFloor := if cMid <= cYh then cMid else cYh + 1
                    ok := (← assert "floorclip at x=160 is mid"
                      (rec.storeRes.floorclip.getD (i32ToNat probeX) 200 == expFloor)) && ok
                    let dumpPath := root / ".agent_tmp" / "live_riser_scale.txt"
                    IO.FS.createDirAll (root / ".agent_tmp")
                    IO.FS.writeFile dumpPath <|
                      s!"viewx={viewx}\nviewy={viewy}\nviewangle={viewangle}\nviewz={viewz}\n" ++
                      s!"v1x={v1.x}\nv1y={v1.y}\nv2x={v2.x}\nv2y={v2.y}\n" ++
                      s!"segAngle={seg.angle}\nrwAngle1={rwAngle1}\n" ++
                      s!"x1={start}\nx2={stop}\n" ++
                      s!"xtoview_start={frame.view.mapping.xtoviewangle.getD (i32ToNat start) 0}\n" ++
                      s!"xtoview_stop={frame.view.mapping.xtoviewangle.getD (i32ToNat stop) 0}\n" ++
                      s!"cDist={cDist}\ncScale={cScale}\ncScaleStep={cScaleStep}\n" ++
                      s!"cPixlow={cPixlow}\ncPixlowstep={cPixlowstep}\ncYh={cYh}\ncMid={cMid}\n" ++
                      s!"cWorldLow={cWorldLow}\ncWorldBottom={cWorldBottom}\n" ++
                      s!"leanScale={rec.storeRes.rwScale}\nleanScaleStep={rec.storeRes.rwScaleStep}\n" ++
                      s!"leanPixlow={rec.storeRes.pixlow}\nleanPixlowstep={rec.storeRes.pixlowstep}\n" ++
                      s!"leanYh={leanYh}\nleanMid={leanMid}\nleanWorldLow={rec.storeRes.worldlow}\n" ++
                      s!"leanWorldBottom={rec.storeRes.worldbottom}\n" ++
                      s!"leanFloorclip160={rec.storeRes.floorclip.getD (i32ToNat probeX) 200}\n" ++
                      s!"expFloor={expFloor}\n"
                    if !ok then
                      IO.eprintln s!"  viewx={viewx} viewy={viewy} viewangle={viewangle} viewz={viewz}"
                      IO.eprintln s!"  v1=({v1.x},{v1.y}) v2=({v2.x},{v2.y}) segAngle={seg.angle}"
                      IO.eprintln s!"  rwAngle1={rwAngle1} x1={start} x2={stop} cDist={cDist}"
                      dumpMismatch "worldlow" rec.storeRes.worldlow cWorldLow
                      dumpMismatch "worldbottom" rec.storeRes.worldbottom cWorldBottom
                      dumpMismatch "rwScale" rec.storeRes.rwScale cScale
                      dumpMismatch "rwScaleStep" rec.storeRes.rwScaleStep cScaleStep
                      dumpMismatch "pixlow" rec.storeRes.pixlow cPixlow
                      dumpMismatch "pixlowstep" rec.storeRes.pixlowstep cPixlowstep
                      dumpMismatch "yh" leanYh cYh
                      dumpMismatch "mid" leanMid cMid
                      dumpMismatch "floorclip160" (rec.storeRes.floorclip.getD (i32ToNat probeX) 200) expFloor
                      dumpMismatch "bottomfrac" rec.storeRes.bottomfrac cBottomfrac
  if ok then
    IO.println "live-riser-scale-test: ALL PASS"
    pure 0
  else
    IO.eprintln "live-riser-scale-test: FAILURES"
    pure 1
