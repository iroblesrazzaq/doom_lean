import Doom.Harness.DisplaySim
import Doom.Playsim.Demo
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Spawn
import Doom.Playsim.Tick
import Doom.Render.Bsp
import Doom.Render.Data
import Doom.Render.Gfx.Texture
import Doom.Render.Main
import Doom.Render.Plane
import Doom.Render.Types
import Doom.Render.Util
import Doom.Render.View
import Doom.Wad

/-!
# plane-visplane-sb9-test

DEMO1 tic 35 visplane identity at C default screenblocks=9
(viewwidth=288, viewheight=144, view at (16,12)). Goldens from the throwaway
C probe under `.agent_tmp/visplane_sb9/`. View-local columns; not sb=10.
-/

open Doom.Harness.DisplaySim
open Doom.Playsim.Demo
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Spawn
open Doom.Playsim.Tick
open Doom.Render.Bsp
open Doom.Render.Data
open Doom.Render.Gfx.Texture
open Doom.Render.Main
open Doom.Render.Plane
open Doom.Render.Types
open Doom.Render.Util
open Doom.Render.View
open Doom.Wad

private def lastGametic : Nat := 35
private def probeX : Nat := 237
private def markR : Nat := 287
private def bandY0 : Nat := 0
private def bandY1 : Nat := 94
private def visplaneSentinel : UInt8 := 255

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

private def lumpName (wad : WadDirectory) (idx : Nat) : String :=
  match wad.entries[idx]? with
  | some e => byteArrayToName e.name
  | none => "?"

private def colMark (arr : Array UInt8) (x : Nat) : Nat :=
  (arr.getD x visplaneSentinel).toNat

private def overlapBand (vp : VisPlane) (x : Nat) : Option (Nat × Nat) :=
  let t := colMark vp.top x
  let b := (vp.bottom.getD x 0).toNat
  if vp.minx > Int32.ofNat x || Int32.ofNat x > vp.maxx then
    none
  else if t == visplaneSentinel.toNat then
    none
  else
    let ov0 := max t bandY0
    let ov1 := min b bandY1
    if ov0 <= ov1 then some (ov0, ov1) else none

private def formatVp (i : Nat) (vp : VisPlane) (name : String) : String :=
  s!"vp[{i}] pic={vp.picnum} name={name} height={vp.height} lightlevel={vp.lightlevel}" ++
    s!" minx={vp.minx} maxx={vp.maxx}" ++
    s!" top0={colMark vp.top 0} bottom0={(vp.bottom.getD 0 0).toNat}" ++
    s!" top237={colMark vp.top probeX} bottom237={(vp.bottom.getD probeX 0).toNat}" ++
    s!" top287={colMark vp.top markR} bottom287={(vp.bottom.getD markR 0).toNat}"

private def dumpLean (path : System.FilePath) (vs : ViewSize) (frame : BspFrameResult)
    (data : RenderData) (wad : WadDirectory) : IO Unit := do
  let mut acc :=
    s!"lastvisplane={frame.planes.lastvisplane} firstFlat={data.textureTables.firstFlat}" ++
      s!" skyflatnum={data.textureTables.skyFlatNum}" ++
      s!" viewx={frame.view.viewx} viewy={frame.view.viewy} viewz={frame.viewz}" ++
      s!" viewangle={frame.view.viewangle}" ++
      s!" viewwidth={vs.viewwidth} viewheight={vs.viewheight}" ++
      s!" viewwindowx={vs.viewwindowx} viewwindowy={vs.viewwindowy}\n"
  let mut i := 0
  while i < frame.planes.lastvisplane do
    match frame.planes.visplanes[i]? with
    | none => pure ()
    | some vp =>
      let name := lumpName wad (data.textureTables.firstFlat + vp.picnum)
      acc := acc ++ formatVp i vp name ++ "\n"
    i := i + 1
  let mut coveringN := 0
  i := 0
  while i < frame.planes.lastvisplane do
    match frame.planes.visplanes[i]? with
    | none => pure ()
    | some vp =>
      match overlapBand vp probeX with
      | none => pure ()
      | some (ov0, ov1) =>
        let name := lumpName wad (data.textureTables.firstFlat + vp.picnum)
        acc := acc ++
          s!"COVERING x=237 ly=0..94 vp[{i}] pic={vp.picnum} name={name}" ++
            s!" height={vp.height} lightlevel={vp.lightlevel}" ++
            s!" minx={vp.minx} maxx={vp.maxx}" ++
            s!" top237={colMark vp.top probeX} bottom237={(vp.bottom.getD probeX 0).toNat}" ++
            s!" overlap_ly={ov0}..{ov1}\n"
        coveringN := coveringN + 1
    i := i + 1
  acc := acc ++ s!"covering_count={coveringN}\n"
  acc := acc ++ s!"drawseg_count={frame.drawSegs.size}\n"
  let mut di := 0
  while di < frame.drawSegs.size do
    match frame.drawSegs[di]? with
    | none => pure ()
    | some rec =>
      let x1 := rec.replayMeta.x1
      let x2 := rec.replayMeta.x2
      let covers := x1 <= 256 && x2 >= 221
      acc := acc ++ s!"ds[{di}] x1={x1} x2={x2} covers_221_256={covers}" ++
        s!" markc={rec.storeRes.markceiling} markf={rec.storeRes.markfloor}" ++
        s!" toptex={rec.storeRes.toptexture} bottex={rec.storeRes.bottomtexture}" ++
        s!" mid={rec.storeRes.midtexture}\n"
    di := di + 1
  let clipRecs := frame.drawSegs.filter fun rec =>
    rec.replayMeta.x1 <= Int32.ofNat probeX && rec.replayMeta.x2 >= Int32.ofNat probeX
  match clipRecs.back? with
  | none => acc := acc ++ "no drawseg covers x=237\n"
  | some rec =>
    acc := acc ++
      s!"clip237 ceil={rec.storeRes.ceilingclip.getD probeX 999}" ++
        s!" floor={rec.storeRes.floorclip.getD probeX 999}" ++
        s!" x1={rec.replayMeta.x1} x2={rec.replayMeta.x2}\n"
  IO.FS.writeFile path acc

private structure VpExpect where
  picnum : Nat
  name : String
  height : Int32
  lightlevel : Int32
  minx : Int32
  maxx : Int32
  top0 : Nat
  bottom0 : Nat
  top237 : Nat
  bottom237 : Nat
  top287 : Nat
  bottom287 : Nat

/-- C dump `.agent_tmp/visplane_sb9/c_visplanes_35.txt`. -/
private def cVisplanes : Array VpExpect := #[
  {
    picnum := 53, name := "NUKAGE3", height := -1048576, lightlevel := 144,
    minx := 0, maxx := 287, top0 := 98, bottom0 := 143,
    top237 := 114, bottom237 := 143, top287 := 118, bottom287 := 143
  },
  {
    picnum := 32, name := "CEIL3_5", height := 4718592, lightlevel := 144,
    minx := 0, maxx := 287, top0 := 0, bottom0 := 29,
    top237 := 0, bottom237 := 58, top287 := 0, bottom287 := 31
  },
  {
    picnum := 53, name := "NUKAGE3", height := -1048576, lightlevel := 112,
    minx := 0, maxx := 12, top0 := 97, bottom0 := 97,
    top237 := 255, bottom237 := 90, top287 := 255, bottom287 := 91
  },
  {
    picnum := 6, name := "FLOOR3_3", height := 0, lightlevel := 144,
    minx := 13, maxx := 287, top0 := 255, bottom0 := 113,
    top237 := 77, bottom237 := 93, top287 := 85, bottom287 := 95
  }
]

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
    IO.eprintln "plane-visplane-sb9-test: FAILURES"
    return 1
  match parseHeader demoBytes with
  | Except.error e =>
    IO.eprintln s!"plane-visplane-sb9-test FAIL: demo header {e}"
    return 1
  | Except.ok hdr =>
    match loadMap wad s!"E{hdr.episode}M{hdr.map}" with
    | Except.error e =>
      IO.eprintln s!"plane-visplane-sb9-test FAIL: load map {e}"
      return 1
    | Except.ok level =>
      match setupSpawnedLevel level (hdr.skill.toUInt32.toInt32) hdr.playeringame
          hdr.consoleplayer.toNat with
      | Except.error e =>
        IO.eprintln s!"plane-visplane-sb9-test FAIL: spawn {e}"
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
          IO.eprintln s!"plane-visplane-sb9-test FAIL: ticker {e}"
          return 1
        | Except.ok gs =>
          match initData wad with
          | Except.error e =>
            IO.eprintln s!"plane-visplane-sb9-test FAIL: initData {e}"
            return 1
          | Except.ok data =>
            let (vs, _) := executeSetViewSize (setViewSize 9 0)
            ok := (← assert "viewwidth=288" (vs.viewwidth == 288)) && ok
            ok := (← assert "viewheight=144" (vs.viewheight == 144)) && ok
            ok := (← assert "viewwindowx=16" (vs.viewwindowx == 16)) && ok
            ok := (← assert "viewwindowy=12" (vs.viewwindowy == 12)) && ok
            match renderBspFromGame data wad gs Framebuffer.initBlack vs with
            | Except.error e =>
              IO.eprintln s!"plane-visplane-sb9-test FAIL: renderBsp {e}"
              return 1
            | Except.ok frame =>
              let dumpDir := root / ".agent_tmp" / "visplane_sb9"
              IO.FS.createDirAll dumpDir
              dumpLean (dumpDir / "lean_visplanes_35.txt") vs frame data wad
              ok := (← assert "lastvisplane=4" (frame.planes.lastvisplane == 4)) && ok
              ok := (← assert "firstFlat=1207" (data.textureTables.firstFlat == 1207)) && ok
              ok := (← assert "skyflatnum=54" (data.textureTables.skyFlatNum == 54)) && ok
              ok := (← assert "viewx" (frame.view.viewx == -15601163)) && ok
              ok := (← assert "viewy" (frame.view.viewy == -22747147)) && ok
              ok := (← assert "viewz" (frame.viewz == 1114184)) && ok
              ok := (← assert "viewangle" (frame.view.viewangle.toNat == 1291845632)) && ok
              let mut coveringN := 0
              let mut i := 0
              while i < cVisplanes.size do
                match cVisplanes[i]?, frame.planes.visplanes[i]? with
                | none, _ =>
                  ok := (← assert s!"c visplane {i} present" false) && ok
                | _, none =>
                  ok := (← assert s!"lean visplane {i} present" false) && ok
                | some exp, some vp =>
                  let name := lumpName wad (data.textureTables.firstFlat + vp.picnum)
                  ok := (← assert s!"vp[{i}] pic={exp.picnum}" (vp.picnum == exp.picnum)) && ok
                  ok := (← assert s!"vp[{i}] name={exp.name}" (name == exp.name)) && ok
                  ok := (← assert s!"vp[{i}] height" (vp.height == exp.height)) && ok
                  ok := (← assert s!"vp[{i}] lightlevel" (vp.lightlevel == exp.lightlevel)) && ok
                  ok := (← assert s!"vp[{i}] minx" (vp.minx == exp.minx)) && ok
                  ok := (← assert s!"vp[{i}] maxx" (vp.maxx == exp.maxx)) && ok
                  let in0 := vp.minx <= 0 && (0 : Int32) <= vp.maxx
                  let in237 := vp.minx <= Int32.ofNat probeX && Int32.ofNat probeX <= vp.maxx
                  let in287 := vp.minx <= Int32.ofNat markR && Int32.ofNat markR <= vp.maxx
                  ok := (← assert s!"vp[{i}] top0" (colMark vp.top 0 == exp.top0)) && ok
                  if in0 && exp.top0 != visplaneSentinel.toNat then
                    ok := (← assert s!"vp[{i}] bottom0"
                      ((vp.bottom.getD 0 0).toNat == exp.bottom0)) && ok
                  ok := (← assert s!"vp[{i}] top237"
                    (colMark vp.top probeX == exp.top237)) && ok
                  if in237 && exp.top237 != visplaneSentinel.toNat then
                    ok := (← assert s!"vp[{i}] bottom237"
                      ((vp.bottom.getD probeX 0).toNat == exp.bottom237)) && ok
                  ok := (← assert s!"vp[{i}] top287"
                    (colMark vp.top markR == exp.top287)) && ok
                  if in287 && exp.top287 != visplaneSentinel.toNat then
                    ok := (← assert s!"vp[{i}] bottom287"
                      ((vp.bottom.getD markR 0).toNat == exp.bottom287)) && ok
                  match overlapBand vp probeX with
                  | some _ => coveringN := coveringN + 1
                  | none => pure ()
                i := i + 1
              ok := (← assert "covering_count=2" (coveringN == 2)) && ok
              match frame.planes.visplanes[1]?, frame.planes.visplanes[3]? with
              | some ceilVp, some floorVp =>
                ok := (← assert "covering vp[1] overlap 0..58"
                  (overlapBand ceilVp probeX == some (0, 58))) && ok
                ok := (← assert "covering vp[3] overlap 77..93"
                  (overlapBand floorVp probeX == some (77, 93))) && ok
              | _, _ =>
                ok := (← assert "covering visplanes exist" false) && ok
              ok := (← assert "drawseg_count=17" (frame.drawSegs.size == 17)) && ok
              match frame.drawSegs[16]? with
              | none =>
                ok := (← assert "ds[16] present" false) && ok
              | some rec =>
                ok := (← assert "ds[16] x1=221" (rec.replayMeta.x1 == 221)) && ok
                ok := (← assert "ds[16] x2=256" (rec.replayMeta.x2 == 256)) && ok
  if ok then
    IO.println "plane-visplane-sb9-test: ALL PASS"
    pure 0
  else
    IO.eprintln "plane-visplane-sb9-test: FAILURES"
    pure 1
