import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Sound
import Doom.Playsim.Spec
import Doom.Playsim.Thinker

/-!
P2c-xxii unit checks: floor-UP `T_MovePlane`, `P_FindSectorFromLineTag`,
`P_FindNextHighestFloor` overflow, `EV_DoPlat` / `T_PlatRaise` UP + pastdest
`P_RemoveActivePlat`, spec 22 `P_CrossSpecialLine`. Kept out of
`EnemyTest.lean` so that file stays under 1k.
-/

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj
open Doom.Playsim.Sound
open Doom.Playsim.Spec
open Doom.Playsim.Thinker

namespace Doom.Playsim.SpecXxTest

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def emptyBlockmap : BlockMap :=
  { originX := 0, originY := 0, width := 0, height := 0, lump := #[] }

private def dummySec (floorheight tag : Int32) (lines : Array UInt32)
    (floorpic : ByteArray := ByteArray.empty) : Sector := {
  floorheight, ceilingheight := 10 * FRACUNIT
  floorpic, ceilingpic := ByteArray.empty
  lightlevel := 0, special := 0, tag
  lines, blockbox := #[0, 0, 0, 0]
  soundorgX := 0, soundorgY := 0
}

private def twoSided (front back : Int32) : Line := {
  v1 := 0, v2 := 0, flags := ML_TWOSIDED, special := 0, tag := 0
  sidenum0 := 0, sidenum1 := 0, dx := 0, dy := 0
  slopetype := 0, bbox := #[0, 0, 0, 0]
  frontsector := front, backsector := back
}

private def emptyLevel (sectors : Array Sector) (sides : Array Side) (lines : Array Line) :
    LevelData := {
  vertexes := #[]
  sectors
  sides
  lines
  segs := #[]
  subsectors := #[]
  nodes := #[]
  things := #[]
  blockmap := emptyBlockmap
  reject := ByteArray.empty
}

private def adjoiningLevel (n : Nat) : LevelData :=
  Id.run do
    let mut lns : Array Line := #[]
    let mut idxs : Array UInt32 := #[]
    let mut i : Nat := 0
    while i < n do
      lns := lns.push (twoSided 0 (i + 1).toInt32)
      idxs := idxs.push i.toUInt32
      i := i + 1
    let mut secs : Array Sector := #[dummySec 0 0 idxs]
    i := 0
    while i < n do
      secs := secs.push (dummySec (FRACUNIT * (i + 1).toInt32) 0 #[])
      i := i + 1
    emptyLevel secs #[] lns

private def dummySide (sec : UInt32) : Side := {
  textureoffset := 0, rowoffset := 0
  toptexture := ByteArray.empty, bottomtexture := ByteArray.empty
  midtexture := ByteArray.empty, sector := sec
}

def checkP2cXxUnits (ok0 : Bool) : IO Bool := do
  let mut ok := ok0
  let gsP0 := initFromLevel (emptyLevel #[dummySec 0 0 #[]] #[] #[]) 2 #[true, false, false, false] 0

  match movePlane gsP0 0 VDOORSPEED (10 * FRACUNIT) false 0 1 with
  | Except.error e =>
    ok := (← assert s!"T_MovePlane floor UP step ({e})" false) && ok
  | Except.ok (gsF, res) =>
    ok := (← assert "T_MovePlane floor UP step ok" (res == resultOk)) && ok
    match gsF.sectors[0]? with
    | none => ok := (← assert "T_MovePlane floor UP sector" false) && ok
    | some sec =>
      ok := (← assert "T_MovePlane floor UP += VDOORSPEED"
        (sec.floorheight == VDOORSPEED)) && ok
  match movePlane gsP0 0 VDOORSPEED FRACUNIT false 0 1 with
  | Except.error e =>
    ok := (← assert s!"T_MovePlane floor UP pastdest ({e})" false) && ok
  | Except.ok (gsPast, res) =>
    ok := (← assert "T_MovePlane floor UP pastdest" (res == resultPastdest)) && ok
    match gsPast.sectors[0]? with
    | none => ok := (← assert "T_MovePlane floor UP pastdest sector" false) && ok
    | some sec =>
      ok := (← assert "T_MovePlane floor UP pastdest snaps dest"
        (sec.floorheight == FRACUNIT)) && ok
  match movePlane gsP0 0 VDOORSPEED (-10 * FRACUNIT) false 0 (-1) with
  | Except.error e =>
    ok := (← assert s!"T_MovePlane floor DOWN step ({e})" false) && ok
  | Except.ok (gsDn, res) =>
    ok := (← assert "T_MovePlane floor DOWN step ok" (res == resultOk)) && ok
    match gsDn.sectors[0]? with
    | none => ok := (← assert "T_MovePlane floor DOWN sector" false) && ok
    | some sec =>
      ok := (← assert "T_MovePlane floor DOWN -= VDOORSPEED"
        (sec.floorheight == -VDOORSPEED)) && ok

  let tagSecs := #[dummySec 0 1 #[], dummySec 0 7 #[], dummySec 0 7 #[]]
  let gsTag := initFromLevel (emptyLevel tagSecs #[] #[]) 2 #[true, false, false, false] 0
  ok := (← assert "P_FindSectorFromLineTag first"
    (findSectorFromLineTag gsTag 7 (-1) == (1 : Int32))) && ok
  ok := (← assert "P_FindSectorFromLineTag next"
    (findSectorFromLineTag gsTag 7 1 == (2 : Int32))) && ok
  ok := (← assert "P_FindSectorFromLineTag none"
    (findSectorFromLineTag gsTag 7 2 == (-1 : Int32))) && ok
  ok := (← assert "P_FindSectorFromLineTag missing tag"
    (findSectorFromLineTag gsTag 99 (-1) == (-1 : Int32))) && ok

  let gsNone := initFromLevel (adjoiningLevel 0) 2 #[true, false, false, false] 0
  match findNextHighestFloor gsNone 0 0 with
  | Except.error e =>
    ok := (← assert s!"P_FindNextHighestFloor empty ({e})" false) && ok
  | Except.ok h =>
    ok := (← assert "P_FindNextHighestFloor no neighbor = current" (h == 0)) && ok
  let gsOne := initFromLevel (adjoiningLevel 1) 2 #[true, false, false, false] 0
  match findNextHighestFloor gsOne 0 0 with
  | Except.error e =>
    ok := (← assert s!"P_FindNextHighestFloor one ({e})" false) && ok
  | Except.ok h =>
    ok := (← assert "P_FindNextHighestFloor one neighbor" (h == FRACUNIT)) && ok
  let gs22 := initFromLevel (adjoiningLevel 22) 2 #[true, false, false, false] 0
  match findNextHighestFloor gs22 0 0 with
  | Except.error e =>
    ok := (← assert s!"P_FindNextHighestFloor 22 ({e})" false) && ok
  | Except.ok h =>
    ok := (← assert "P_FindNextHighestFloor 22 adjoining ok" (h == FRACUNIT)) && ok
  let gs23 := initFromLevel (adjoiningLevel 23) 2 #[true, false, false, false] 0
  match findNextHighestFloor gs23 0 0 with
  | Except.error e =>
    ok := (← assert "P_FindNextHighestFloor 23 I_Error"
      (e.contains "22 adjoining")) && ok
  | Except.ok _ =>
    ok := (← assert "P_FindNextHighestFloor 23 should I_Error" false) && ok

  let frontPic := ByteArray.mk #[70, 76, 79, 79, 82, 49, 0, 0]
  let platLine : Line := {
    twoSided 1 2 with special := 22, tag := 5, sidenum0 := 0
  }
  let platLevel := emptyLevel
    #[
      dummySec 0 0 #[] frontPic,
      dummySec 0 5 #[0],
      dummySec (5 * FRACUNIT) 0 #[]
    ]
    #[dummySide 0]
    #[platLine]
  let gsPlat0 := initFromLevel platLevel 2 #[true, false, false, false] 0
  match evDoPlat gsPlat0 0 platRaiseToNearestAndChange 0 with
  | Except.error e =>
    ok := (← assert s!"EV_DoPlat raiseToNearestAndChange ({e})" false) && ok
  | Except.ok (gsPlat, platOk) =>
    ok := (← assert "EV_DoPlat rtn true" platOk) && ok
    ok := (← assert "EV_DoPlat one plat" (gsPlat.plats.size == 1)) && ok
    match gsPlat.plats[0]? with
    | none => ok := (← assert "EV_DoPlat plat payload" false) && ok
    | some plat =>
      ok := (← assert "EV_DoPlat speed PLATSPEED/2"
        (plat.speed == PLATSPEED / (2 : Int32))) && ok
      ok := (← assert "EV_DoPlat status UP" (plat.status == plat_up)) && ok
      ok := (← assert "EV_DoPlat high next floor" (plat.high == 5 * FRACUNIT)) && ok
      ok := (← assert "EV_DoPlat crush false" (!plat.crush)) && ok
    match gsPlat.sectors[1]? with
    | none => ok := (← assert "EV_DoPlat tagged sector" false) && ok
    | some sec =>
      ok := (← assert "EV_DoPlat copies floorpic" (sec.floorpic == frontPic)) && ok
      ok := (← assert "EV_DoPlat sec.special=0" (sec.special == 0)) && ok
      ok := (← assert "EV_DoPlat specialdata payload" (sec.specialdata == 0)) && ok
    ok := (← assert "EV_DoPlat sfx_stnmov rnd+1"
      (gsPlat.rng.rndindex == gsPlat0.rng.rndindex + 1)) && ok
    ok := (← assert "EV_DoPlat activePlats[0]=0"
      (match gsPlat.activePlats[0]? with
       | some s => s == 0
       | none => false)) && ok
    let mut foundPlatTh := false
    for th in gsPlat.thinkers do
      if th.func == THF_PLATRAISE && th.payload == 0 then
        foundPlatTh := true
    ok := (← assert "EV_DoPlat THF_PLATRAISE thinker" foundPlatTh) && ok
    match evDoPlat gsPlat 0 platRaiseToNearestAndChange 0 with
    | Except.error e =>
      ok := (← assert s!"EV_DoPlat busy skip ({e})" false) && ok
    | Except.ok (gsBusy, platOk) =>
      ok := (← assert "EV_DoPlat busy rtn false" (!platOk)) && ok
      ok := (← assert "EV_DoPlat busy sector skip no second plat"
        (gsBusy.plats.size == 1)) && ok
      ok := (← assert "EV_DoPlat busy skip no extra sfx"
        (gsBusy.rng.rndindex == gsPlat.rng.rndindex)) && ok
    match platRaiseThinker gsPlat 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise UP step ({e})" false) && ok
    | Except.ok gsUp =>
      match gsUp.sectors[1]? with
      | none => ok := (← assert "T_PlatRaise sector" false) && ok
      | some sec =>
        ok := (← assert "T_PlatRaise floor += PLATSPEED/2"
          (sec.floorheight == PLATSPEED / (2 : Int32))) && ok
    let gsWait : GameState :=
      match gsPlat.plats[0]? with
      | none => gsPlat
      | some plat =>
        ({
          gsPlat with
          plats := GameState.arrSet gsPlat.plats 0 { plat with status := plat_waiting, count := 3 }
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
    let gsNearDest :=
      match gsPlat.sectors[1]?, gsPlat.plats[0]? with
      | some sec, some plat =>
        {
          gsPlat with
          leveltime := 5
          sectors := GameState.arrSet gsPlat.sectors 1
            { sec with floorheight := plat.high - 1 }
        }
      | _, _ => gsPlat
    let rndBeforePast := gsNearDest.rng.rndindex
    match platRaiseThinker gsNearDest 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise pastdest ({e})" false) && ok
    | Except.ok gsPast =>
      match gsPast.sectors[1]?, gsPast.plats[0]?, gsPlat.plats[0]? with
      | some sec, some plat, some plat0 =>
        ok := (← assert "T_PlatRaise pastdest floor snaps to high"
          (sec.floorheight == plat0.high)) && ok
        ok := (← assert "T_PlatRaise pastdest status=waiting count=wait"
          (plat.status == plat_waiting && plat.count == plat.wait
            && plat.wait == plat0.wait)) && ok
        ok := (← assert "T_PlatRaise pastdest specialdata=-1"
          (sec.specialdata == -1)) && ok
      | _, _, _ =>
        ok := (← assert "T_PlatRaise pastdest sector/plat" false) && ok
      let mut foundRemoved := false
      for th in gsPast.thinkers do
        if th.payload == 0 && th.func == THF_REMOVED then
          foundRemoved := true
      ok := (← assert "T_PlatRaise pastdest THF_REMOVED" foundRemoved) && ok
      ok := (← assert "T_PlatRaise pastdest activePlats[0]=-1"
        (match gsPast.activePlats[0]? with
         | some s => s == (-1 : Int32)
         | none => false)) && ok
      ok := (← assert "T_PlatRaise pastdest pstop rnd+1"
        (gsPast.rng.rndindex == rndBeforePast + 1)) && ok
    let gsPastSfx : GameState := { gsNearDest with leveltime := 8 }
    let rndBeforeSfx := gsPastSfx.rng.rndindex
    match platRaiseThinker gsPastSfx 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise stnmov-before-pstop ({e})" false) && ok
    | Except.ok gsBoth =>
      ok := (← assert "T_PlatRaise stnmov-before-pstop rnd+2"
        (gsBoth.rng.rndindex == rndBeforeSfx + 2)) && ok
    match removeActivePlat gsPlat0 0 with
    | Except.error e =>
      ok := (← assert "P_RemoveActivePlat missing I_Error"
        (e.contains "can't find plat")) && ok
    | Except.ok _ =>
      ok := (← assert "P_RemoveActivePlat missing should I_Error" false) && ok
    let gsSlot1 := {
      gsPlat with
      activePlats :=
        GameState.arrSet (GameState.arrSet gsPlat.activePlats 0 (99 : Int32)) 1 0
    }
    match removeActivePlat gsSlot1 0 with
    | Except.error e =>
      ok := (← assert s!"P_RemoveActivePlat first matching slot ({e})" false) && ok
    | Except.ok gsCleared =>
      ok := (← assert "P_RemoveActivePlat first match keeps earlier slot"
        (match gsCleared.activePlats[0]? with
         | some s => s == (99 : Int32)
         | none => false)) && ok
      ok := (← assert "P_RemoveActivePlat first match clears matching slot"
        (match gsCleared.activePlats[1]? with
         | some s => s == (-1 : Int32)
         | none => false)) && ok

  let gsFull := Id.run do
    let mut gs := gsPlat0
    let mut i : Nat := 0
    while i < MAXPLATS do
      gs := { gs with activePlats := GameState.arrSet gs.activePlats i 0 }
      i := i + 1
    gs
  match addActivePlat gsFull 0 with
  | Except.error e =>
    ok := (← assert "P_AddActivePlat full loud-error" (e.contains "no more plats")) && ok
  | Except.ok _ =>
    ok := (← assert "P_AddActivePlat full should loud-error" false) && ok

  let gsPlatLt : GameState := { gsPlat0 with leveltime := 8 }
  match evDoPlat gsPlatLt 0 platRaiseToNearestAndChange 0 with
  | Except.error e =>
    ok := (← assert s!"EV_DoPlat for periodic sfx ({e})" false) && ok
  | Except.ok (gsP8, _) =>
    let rndAfterAdd := gsP8.rng.rndindex
    match platRaiseThinker gsP8 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise periodic sfx ({e})" false) && ok
    | Except.ok gsSfx =>
      ok := (← assert "T_PlatRaise sfx when leveltime&7==0"
        (gsSfx.rng.rndindex == rndAfterAdd + 1)) && ok
  let gsPlatLt3 : GameState := { gsPlat0 with leveltime := 3 }
  match evDoPlat gsPlatLt3 0 platRaiseToNearestAndChange 0 with
  | Except.error e =>
    ok := (← assert s!"EV_DoPlat for no periodic sfx ({e})" false) && ok
  | Except.ok (gsP3, _) =>
    let rndAfterAdd := gsP3.rng.rndindex
    match platRaiseThinker gsP3 0 with
    | Except.error e =>
      ok := (← assert s!"T_PlatRaise no periodic ({e})" false) && ok
    | Except.ok gsNoSfx =>
      ok := (← assert "T_PlatRaise no sfx when leveltime&7!=0"
        (gsNoSfx.rng.rndindex == rndAfterAdd)) && ok

  let walkLine : Line := { platLine with special := 22, tag := 5 }
  let gsWalk := initFromLevel
    (emptyLevel
      #[dummySec 0 0 #[] frontPic, dummySec 0 5 #[0], dummySec (5 * FRACUNIT) 0 #[]]
      #[dummySide 0]
      #[walkLine])
    2 #[true, false, false, false] 0
  let gsWalk := { gsWalk with mobjs := #[{ Doom.Playsim.Mobj.empty with player := 0 }] }
  match Spec.crossSpecialLine gsWalk 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"player spec 22 ({e})" false) && ok
  | Except.ok gs22 =>
    ok := (← assert "player spec 22 W1 special=0"
      (match gs22.level.lines[0]? with
       | some ld => ld.special == 0
       | none => false)) && ok
    ok := (← assert "player spec 22 spawned plat" (gs22.plats.size == 1)) && ok
  let gsMon := { gsWalk with mobjs := #[{ Doom.Playsim.Mobj.empty with player := -1 }] }
  match Spec.crossSpecialLine gsMon 0 0 0 with
  | Except.error e =>
    ok := (← assert s!"monster spec 22 ({e})" false) && ok
  | Except.ok gsM =>
    ok := (← assert "monster spec 22 no-op special stays"
      (match gsM.level.lines[0]? with
       | some ld => ld.special == 22 && gsM.plats.size == 0
       | none => false)) && ok
  let badLine : Line := { walkLine with special := 99 }
  let gsBad := { gsWalk with level := { gsWalk.level with lines := #[badLine] } }
  match Spec.crossSpecialLine gsBad 0 0 0 with
  | Except.error e =>
    ok := (← assert "unhandled special loud-error"
      (e.contains "P_CrossSpecialLine")) && ok
  | Except.ok _ =>
    ok := (← assert "unhandled special should loud-error" false) && ok

  pure ok

end Doom.Playsim.SpecXxTest
