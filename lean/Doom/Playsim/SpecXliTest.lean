import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xli unit checks: `T_PlatRaise` plat_waiting countdown → flip UP/DOWN +
`sfx_pstart` (no `movePlane` on flip tic); `plat_inStasis` no-op.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXliTest

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

def checkP2cXliUnits (ok0 : Bool) : IO Bool := do
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
    let gsWaitDec : GameState :=
      match gsDwus.plats[0]? with
      | none => gsDwus
      | some plat =>
        {
          gsDwus with
          plats := GameState.arrSet gsDwus.plats 0
            { plat with status := plat_waiting, count := 5 }
        }
    match platRaiseThinker gsWaitDec 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise waiting decrement ({e})" false) && ok
    | Except.ok gsDec =>
      match gsDec.plats[0]? with
      | none => ok := (← assert "T_PlatRaise waiting decrement plat" false) && ok
      | some plat =>
        ok := (← assert "T_PlatRaise waiting count 5→4"
          (plat.count == 4 && plat.status == plat_waiting)) && ok
      match gsDec.sectors[1]? with
      | none => ok := (← assert "T_PlatRaise waiting decrement sector" false) && ok
      | some sec =>
        ok := (← assert "T_PlatRaise waiting decrement floor unchanged"
          (sec.floorheight == 0)) && ok
      ok := (← assert "T_PlatRaise waiting decrement no sfx"
        (gsDec.rng.rndindex == gsWaitDec.rng.rndindex)) && ok

    let gsAtLow : GameState :=
      match gsDwus.sectors[1]?, gsDwus.plats[0]? with
      | some sec, some plat =>
        {
          gsDwus with
          sectors := GameState.arrSet gsDwus.sectors 1
            { sec with floorheight := plat.low }
          plats := GameState.arrSet gsDwus.plats 0
            { plat with status := plat_waiting, count := 1 }
        }
      | _, _ => gsDwus
    let rndBeforeFlip := gsAtLow.rng.rndindex
    match platRaiseThinker gsAtLow 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise waiting flip ({e})" false) && ok
    | Except.ok gsFlip =>
      match gsFlip.plats[0]?, gsAtLow.sectors[1]? with
      | some plat, some secBefore =>
        ok := (← assert "T_PlatRaise waiting flip count=0 status=up"
          (plat.count == 0 && plat.status == plat_up)) && ok
        match gsFlip.sectors[1]? with
        | none => ok := (← assert "T_PlatRaise waiting flip sector" false) && ok
        | some secAfter =>
          ok := (← assert "T_PlatRaise waiting flip floor unchanged"
            (secAfter.floorheight == secBefore.floorheight)) && ok
      | _, _ =>
        ok := (← assert "T_PlatRaise waiting flip plat/sector" false) && ok
      ok := (← assert "T_PlatRaise waiting flip sfx_pstart rnd+1"
        (gsFlip.rng.rndindex == rndBeforeFlip + 1)) && ok

    let gsCountZero : GameState :=
      match gsDwus.plats[0]? with
      | none => gsDwus
      | some plat =>
        ({
          gsDwus with
          plats := GameState.arrSet gsDwus.plats 0 { plat with status := plat_waiting, count := 0 }
        })
    match platRaiseThinker gsCountZero 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise waiting count=0 ({e})" false) && ok
    | Except.ok gsZ =>
      match gsZ.plats[0]? with
      | some plat =>
        ok := (← assert "T_PlatRaise waiting count=0→-1 no flip"
          (plat.count == -1 && plat.status == plat_waiting)) && ok
      | none =>
        ok := (← assert "T_PlatRaise waiting count=0 plat" false) && ok
      ok := (← assert "T_PlatRaise waiting count=0 no sfx"
        (gsZ.rng.rndindex == gsCountZero.rng.rndindex)) && ok

    let gsStasis : GameState :=
      match gsDwus.plats[0]? with
      | none => gsDwus
      | some plat =>
        { gsDwus with plats := GameState.arrSet gsDwus.plats 0 { plat with status := plat_inStasis } }
    match platRaiseThinker gsStasis 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise inStasis ({e})" false) && ok
    | Except.ok gsNoop =>
      match gsNoop.plats[0]? with
      | some plat =>
        ok := (← assert "T_PlatRaise inStasis status unchanged"
          (plat.status == plat_inStasis)) && ok
      | none =>
        ok := (← assert "T_PlatRaise inStasis plat" false) && ok
      ok := (← assert "T_PlatRaise inStasis rng unchanged"
        (gsNoop.rng.rndindex == gsStasis.rng.rndindex)) && ok

  pure ok

end Doom.Playsim.SpecXliTest
