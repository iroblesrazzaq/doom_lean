import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xliii unit checks: `T_PlatRaise` plat_up `downWaitUpStay` resultOk (no
`sfx_stnmov`). UP pastdest is P2c-xliv (`SpecXlivTest`).
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXliiiTest

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

def checkP2cXliiiUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0

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
  match evDoPlat gsDwus0 0 platDownWaitUpStay 0 with
  | Except.error e =>
    ok := (← assert s!"EV_DoPlat downWaitUpStay ({e})" false) && ok
  | Except.ok (gsDwus, platOk) =>
    ok := (← assert "EV_DoPlat rtn true" platOk) && ok
    let gsUp : GameState :=
      match gsDwus.sectors[1]?, gsDwus.plats[0]? with
      | some sec, some plat =>
        {
          gsDwus with
          sectors := GameState.arrSet gsDwus.sectors 1
            { sec with floorheight := plat.low }
          plats := GameState.arrSet gsDwus.plats 0 { plat with status := plat_up }
        }
      | _, _ => gsDwus
    let rndBeforeUp := gsUp.rng.rndindex
    match platRaiseThinker gsUp 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise UP resultOk ({e})" false) && ok
    | Except.ok gsStep =>
      match gsStep.sectors[1]?, gsUp.plats[0]? with
      | some sec, some plat =>
        ok := (← assert "T_PlatRaise UP floor += PLATSPEED*4"
          (sec.floorheight == plat.low + PLATSPEED * 4)) && ok
      | _, _ =>
        ok := (← assert "T_PlatRaise UP resultOk sector/plat" false) && ok
      ok := (← assert "T_PlatRaise UP resultOk no sfx_stnmov"
        (gsStep.rng.rndindex == rndBeforeUp)) && ok

  pure ok

end Doom.Playsim.SpecXliiiTest
