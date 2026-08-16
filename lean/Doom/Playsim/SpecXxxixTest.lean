import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xxxix unit checks: `findLowestFloorSurrounding`, floor DOWN `T_MovePlane`,
`EV_DoPlat` `downWaitUpStay`, `T_PlatRaise` plat_down resultOk, `P_CrossSpecialLine`
spec 88 (special stays 88). Kept out of `EnemyTest.lean` so that file stays
under 1k.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXxxixTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def emptyBlockmap : BlockMap :=
  { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }

private def twoSided (front back : Int32) : Line := {
  v1 := 0, v2 := 0, flags := 4, special := 0, tag := 0
  sidenum0 := 0, sidenum1 := 0, dx := 0, dy := 0
  slopetype := 0, bbox := #[0, 0, 0, 0]
  frontsector := front, backsector := back
}

private def dummySec (floor ceiling : Int32) (tag : Int32) (lines : Array UInt32) : Sector := {
  floorheight := floor, ceilingheight := ceiling
  floorpic := ByteArray.empty, ceilingpic := ByteArray.empty
  lightlevel := 0, special := 0, tag
  lines, blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def dummySide (sector : UInt32) : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := ByteArray.empty, bottomtexture := ByteArray.empty
  midtexture := ByteArray.empty, sector
}

private def adjoiningFloorLevel (neighborFloor : Int32) : LevelData :=
  let sec0 := dummySec 0 (10 * FRACUNIT) 3 #[0]
  let sec1 := dummySec neighborFloor (10 * FRACUNIT) 0 #[]
  let line := twoSided 0 1
  {
    vertexes := #[]
    sectors := #[sec0, sec1]
    sides := #[dummySide 0, dummySide 1]
    lines := #[line]
    segs := #[]
    subsectors := #[]
    nodes := #[]
    things := #[]
    blockmap := emptyBlockmap
    reject := ByteArray.empty
  }

private def mkGs (player special tag : Int32) : GameState :=
  let line : Line := {
    v1 := 0, v2 := 0, flags := 0, special, tag
    sidenum0 := 0, sidenum1 := 0, dx := 0, dy := 0
    slopetype := 0, bbox := #[0, 0, 0, 0]
    frontsector := 0, backsector := 0
  }
  let level : LevelData := {
    vertexes := #[]
    sectors := #[dummySec 0 (10 * FRACUNIT) tag #[]]
    sides := #[dummySide 0]
    lines := #[line]
    segs := #[]
    subsectors := #[]
    nodes := #[]
    things := #[]
    blockmap := emptyBlockmap
    reject := ByteArray.empty
  }
  let gs0 := initFromLevel level 2 #[true, false, false, false] 0
  { gs0 with mobjs := #[{ Mobj.empty with player }] }

def checkP2cXxxixUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

  let gsAdj := initFromLevel (adjoiningFloorLevel (-104 * FRACUNIT)) 2 #[true, false, false, false] 0
  ok := (← assert "P_FindLowestFloorSurrounding neighbor"
    (findLowestFloorSurrounding gsAdj 0 == -104 * FRACUNIT)) && ok
  ok := (← assert "P_FindLowestFloorSurrounding self when no lower"
    (findLowestFloorSurrounding gsAdj 1 == -104 * FRACUNIT)) && ok

  ok := (← assert "TICRATE=35" (TICRATE == 35)) && ok
  ok := (← assert "PLATWAIT=3" (PLATWAIT == 3)) && ok

  let gsF0 := initFromLevel (adjoiningFloorLevel 0) 2 #[true, false, false, false] 0
  match movePlane gsF0 0 (PLATSPEED * 4) (-4 * FRACUNIT) false 0 (-1) with
  | Except.error e =>
    ok := (← assert s!"T_MovePlane floor DOWN step ({e})" false) && ok
  | Except.ok (gs1, res) =>
    ok := (← assert "T_MovePlane floor DOWN step ok" (res == resultOk)) && ok
    match gs1.sectors[0]? with
    | none => ok := (← assert "T_MovePlane floor DOWN sector" false) && ok
    | some sec =>
      ok := (← assert "T_MovePlane floor DOWN -= PLATSPEED*4"
        (sec.floorheight == -4 * FRACUNIT)) && ok
  let gsFPast :=
    match gsF0.sectors[0]? with
    | none => gsF0
    | some sec =>
      { gsF0 with sectors := GameState.arrSet gsF0.sectors 0 { sec with floorheight := -101 * FRACUNIT } }
  match movePlane gsFPast 0 (PLATSPEED * 4) (-104 * FRACUNIT) false 0 (-1) with
  | Except.error e =>
    ok := (← assert s!"T_MovePlane floor DOWN pastdest ({e})" false) && ok
  | Except.ok (gsPast, res) =>
    ok := (← assert "T_MovePlane floor DOWN pastdest" (res == resultPastdest)) && ok
    match gsPast.sectors[0]? with
    | none => ok := (← assert "T_MovePlane floor DOWN pastdest sector" false) && ok
    | some sec =>
      ok := (← assert "T_MovePlane floor DOWN pastdest snaps dest"
        (sec.floorheight == -104 * FRACUNIT)) && ok

  let dwusLevel :=
    let sec0 := dummySec 0 (10 * FRACUNIT) 0 #[0]
    let sec1 := dummySec 0 (10 * FRACUNIT) 3 #[1]
    let sec2 := dummySec (-104 * FRACUNIT) (10 * FRACUNIT) 0 #[]
    let line0 := { (twoSided 0 1) with special := 88, tag := 3 }
    let line1 := twoSided 1 2
    {
      vertexes := #[]
      sectors := #[sec0, sec1, sec2]
      sides := #[dummySide 0, dummySide 1, dummySide 2]
      lines := #[line0, line1]
      segs := #[]
      subsectors := #[]
      nodes := #[]
      things := #[]
      blockmap := emptyBlockmap
      reject := ByteArray.empty
    }
  let gsDwus0 := initFromLevel dwusLevel 2 #[true, false, false, false] 0
  let rnd0 := gsDwus0.rng.rndindex
  match evDoPlat gsDwus0 0 platDownWaitUpStay 0 with
  | Except.error e =>
    ok := (← assert s!"EV_DoPlat downWaitUpStay ({e})" false) && ok
  | Except.ok (gsDwus, platOk) =>
    ok := (← assert "EV_DoPlat rtn true" platOk) && ok
    ok := (← assert "EV_DoPlat downWaitUpStay one plat" (gsDwus.plats.size == 1)) && ok
    match gsDwus.plats[0]? with
    | none => ok := (← assert "EV_DoPlat plat payload" false) && ok
    | some plat =>
      ok := (← assert "EV_DoPlat status DOWN" (plat.status == plat_down)) && ok
      ok := (← assert "EV_DoPlat speed PLATSPEED*4"
        (plat.speed == PLATSPEED * 4)) && ok
      ok := (← assert "EV_DoPlat low=-104*FRACUNIT"
        (plat.low == -104 * FRACUNIT)) && ok
      ok := (← assert "EV_DoPlat high=0" (plat.high == 0)) && ok
      ok := (← assert "EV_DoPlat wait=105" (plat.wait == TICRATE * PLATWAIT)) && ok
      ok := (← assert "EV_DoPlat crush false" (!plat.crush)) && ok
    match gsDwus.sectors[1]? with
    | none => ok := (← assert "EV_DoPlat tagged sector" false) && ok
    | some sec =>
      ok := (← assert "EV_DoPlat no floorpic copy"
        (sec.floorpic == ByteArray.empty)) && ok
      ok := (← assert "EV_DoPlat sec.special unchanged"
        (sec.special == 0)) && ok
      ok := (← assert "EV_DoPlat specialdata payload" (sec.specialdata == 0)) && ok
    ok := (← assert "EV_DoPlat sfx_pstart rnd+1"
      (gsDwus.rng.rndindex == rnd0 + 1)) && ok
    let mut foundPlatTh := false
    for th in gsDwus.thinkers do
      if th.func == THF_PLATRAISE && th.payload == 0 then
        foundPlatTh := true
    ok := (← assert "EV_DoPlat THF_PLATRAISE thinker" foundPlatTh) && ok
    match platRaiseThinker gsDwus 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise DOWN step ({e})" false) && ok
    | Except.ok gsDown =>
      match gsDown.sectors[1]? with
      | none => ok := (← assert "T_PlatRaise DOWN sector" false) && ok
      | some sec =>
        ok := (← assert "T_PlatRaise DOWN floor -= PLATSPEED*4"
          (sec.floorheight == -4 * FRACUNIT)) && ok
    let gsWait : GameState :=
      match gsDwus.plats[0]? with
      | none => gsDwus
      | some plat =>
        ({
          gsDwus with
          plats := GameState.arrSet gsDwus.plats 0 { plat with status := plat_waiting, count := 3 }
        })
    match platRaiseThinker gsWait 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise waiting decrement ({e})" false) && ok
    | Except.ok gsW =>
      match gsW.plats[0]? with
      | some plat =>
        ok := (← assert "T_PlatRaise waiting count 3→2"
          (plat.count == 2 && plat.status == plat_waiting)) && ok
      | none =>
        ok := (← assert "T_PlatRaise waiting plat" false) && ok
  let dwusTaggedLevel :=
    let sec0 := dummySec 0 (10 * FRACUNIT) 0 #[0]
    let sec1 := dummySec 0 (10 * FRACUNIT) 3 #[1]
    let sec2 := dummySec (-104 * FRACUNIT) (10 * FRACUNIT) 0 #[]
    let line0 := { (twoSided 0 1) with special := 88, tag := 3 }
    let line1 := twoSided 1 2
    {
      vertexes := #[]
      sectors := #[sec0, sec1, sec2]
      sides := #[dummySide 0, dummySide 1, dummySide 2]
      lines := #[line0, line1]
      segs := #[]
      subsectors := #[]
      nodes := #[]
      things := #[]
      blockmap := emptyBlockmap
      reject := ByteArray.empty
    }
  let gs88Base := initFromLevel dwusTaggedLevel 2 #[true, false, false, false] 0
  let gs88 := { gs88Base with mobjs := #[{ Mobj.empty with player := 0 }] }
  match Spec.crossSpecialLine gs88 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player walk spec 88 ({e})" false) && ok
  | Except.ok gs1 =>
    ok := (← assert "player walk spec 88 keeps special 88"
      (match gs1.level.lines[0]? with
       | some ld => ld.special == 88
       | none => false)) && ok
    ok := (← assert "player walk spec 88 creates plat"
      (gs1.plats.size == 1)) && ok
    ok := (← assert "player walk spec 88 THF_PLATRAISE"
      (gs1.thinkers.size == gs88.thinkers.size + 1)) && ok

  pure ok

end Doom.Playsim.SpecXxxixTest
