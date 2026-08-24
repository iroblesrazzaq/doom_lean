import Doom.Playsim.Angle
import Doom.Playsim.Demo
import Doom.Playsim.Enemy
import Doom.Playsim.GameState
import Doom.Playsim.Player
import Doom.Playsim.PlayerThink
import Doom.Playsim.Random
import Doom.Playsim.Think

/-!
# Doom.Playsim.Tick

`P_Ticker` / `G_Ticker` (level + demoplayback) and `ST_Ticker`.
-/

namespace Doom.Playsim.Tick

open Doom.Playsim.Angle
open Doom.Playsim.Demo
open Doom.Playsim.Enemy
open Doom.Playsim.GameState
open Doom.Playsim.Player
open Doom.Playsim.PlayerThink
open Doom.Playsim.Random
open Doom.Playsim.Think

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

/-- `i_timer.h` `TICRATE`. -/
def ST_TICRATE : Int32 := 35
def ST_NUMPAINFACES : Int32 := 5
def ST_NUMSTRAIGHTFACES : Int32 := 3
def ST_FACESTRIDE : Int32 := 8
def ST_TURNOFFSET : Int32 := 3
def ST_OUCHOFFSET : Int32 := 5
def ST_EVILGRINOFFSET : Int32 := 6
def ST_RAMPAGEOFFSET : Int32 := 7
def ST_GODFACE : Int32 := 40
def ST_DEADFACE : Int32 := 41
def ST_EVILGRINCOUNT : Int32 := 70
def ST_STRAIGHTFACECOUNT : Int32 := 17
def ST_TURNCOUNT : Int32 := 35
def ST_RAMPAGEDELAY : Int32 := 70
def ST_MUCHPAIN : Int32 := 20

/-- `ST_calcPainOffset`. -/
def calcPainOffset (st : StFaceState) (health0 : Int32) : StFaceState × Int32 :=
  let health := if health0 > 100 then (100 : Int32) else health0
  if health != st.painOldhealth then
    let lastcalc := ST_FACESTRIDE * (((100 - health) * ST_NUMPAINFACES) / 101)
    ({ st with painOffset := lastcalc, painOldhealth := health }, lastcalc)
  else
    (st, st.painOffset)

/-- `ST_updateFaceWidget`. -/
def updateFaceWidget (gs : GameState) (plyr : Player) (stRandom : Int32) : StFaceState :=
  Id.run do
    let mut st := gs.stFace
    if st.priority < 10 then
      if plyr.health == 0 then
        st := { st with priority := 9, faceindex := ST_DEADFACE, facecount := 1 }
    if st.priority < 9 then
      if plyr.bonuscount != 0 then
        let mut doevilgrin := false
        let mut owned := st.oldweaponsowned
        let mut i : Nat := 0
        while i < NUMWEAPONS do
          let cur := match plyr.weaponowned[i]? with | some v => v | none => (0 : Int32)
          let old := match owned[i]? with | some v => v | none => (0 : Int32)
          if old != cur then
            doevilgrin := true
            owned := setArr owned i cur
          i := i + 1
        st := { st with oldweaponsowned := owned }
        if doevilgrin then
          let (st1, pain) := calcPainOffset st plyr.health
          st := { st1 with
            priority := 8
            facecount := ST_EVILGRINCOUNT
            faceindex := pain + ST_EVILGRINOFFSET
          }
    if st.priority < 8 then
      if plyr.damagecount != 0 && plyr.attacker >= 0 && plyr.attacker != plyr.mo then
        match gs.mobjs[plyr.mo.toNatClampNeg]?, gs.mobjs[plyr.attacker.toNatClampNeg]? with
        | some pmo, some att =>
          st := { st with priority := 7 }
          if plyr.health - st.oldhealth > ST_MUCHPAIN then
            let (st1, pain) := calcPainOffset st plyr.health
            st := { st1 with facecount := ST_TURNCOUNT, faceindex := pain + ST_OUCHOFFSET }
          else
            let badguyangle := pointToAngle2 pmo.x pmo.y att.x att.y
            let turnRight :=
              if badguyangle > pmo.angle then
                (badguyangle - pmo.angle) > ANG180
              else
                (pmo.angle - badguyangle) <= ANG180
            let diffang :=
              if badguyangle > pmo.angle then badguyangle - pmo.angle
              else pmo.angle - badguyangle
            let (st1, pain) := calcPainOffset st plyr.health
            let face :=
              if diffang < ANG45 then pain + ST_RAMPAGEOFFSET
              else if turnRight then pain + ST_TURNOFFSET
              else pain + ST_TURNOFFSET + 1
            st := { st1 with facecount := ST_TURNCOUNT, faceindex := face }
        | _, _ => pure ()
    if st.priority < 7 then
      if plyr.damagecount != 0 then
        if plyr.health - st.oldhealth > ST_MUCHPAIN then
          let (st1, pain) := calcPainOffset st plyr.health
          st := { st1 with
            priority := 7
            facecount := ST_TURNCOUNT
            faceindex := pain + ST_OUCHOFFSET
          }
        else
          let (st1, pain) := calcPainOffset st plyr.health
          st := { st1 with
            priority := 6
            facecount := ST_TURNCOUNT
            faceindex := pain + ST_RAMPAGEOFFSET
          }
    if st.priority < 6 then
      if plyr.attackdown then
        if st.lastattackdown == -1 then
          st := { st with lastattackdown := ST_RAMPAGEDELAY }
        else
          let next := st.lastattackdown - 1
          if next == 0 then
            let (st1, pain) := calcPainOffset st plyr.health
            st := { st1 with
              priority := 5
              faceindex := pain + ST_RAMPAGEOFFSET
              facecount := 1
              lastattackdown := 1
            }
          else
            st := { st with lastattackdown := next }
      else
        st := { st with lastattackdown := -1 }
    if st.priority < 5 then
      let god :=
        (plyr.cheats &&& CF_GODMODE) != 0 ||
          (match plyr.powers[pw_invulnerability]? with
            | some n => n != 0
            | none => false)
      if god then
        st := { st with priority := 4, faceindex := ST_GODFACE, facecount := 1 }
    if st.facecount == 0 then
      let (st1, pain) := calcPainOffset st plyr.health
      st := { st1 with
        faceindex := pain + (stRandom % (3 : Int32))
        facecount := ST_STRAIGHTFACECOUNT
        priority := 0
      }
    { st with facecount := st.facecount - 1 }

private def setFlatTrans (arr : Array Int32) (i v : Int32) : Array Int32 :=
  Id.run do
    let idx := i.toNatClampNeg
    let mut a := arr
    let mut j := a.size
    while j <= idx do
      a := a.push (Int32.ofNat j)
      j := j + 1
    pure (GameState.arrSet a idx v)

/--
`P_UpdateSpecials` flat cycle (`p_spec.c`). Texture translation, scrolling 48,
and buttons are not this chunk. Uses wrapping `Int32`/`UInt32`.
-/
def updateSpecials (gs : GameState) : GameState :=
  Id.run do
    let mut trans := gs.flattranslation
    let lt := gs.leveltime.toInt32
    let mut a : Nat := 0
    while a < gs.picAnims.size do
      match gs.picAnims[a]? with
      | none => pure ()
      | some anim =>
        if !anim.istexture then do
          let mut i := anim.basepic
          let stop := anim.basepic + anim.numpics
          while i < stop do
            let pic := anim.basepic + ((lt / anim.speed + i) % anim.numpics)
            trans := setFlatTrans trans i pic
            i := i + 1
      a := a + 1
    { gs with flattranslation := trans }

/--
`P_RespawnSpecials` — commercial item respawn queue. Empty / early-out when
queue idle (Doom 1 shareware path).
-/
def respawnSpecials (gs : GameState) : GameState := gs

/-- `ST_Ticker`: one `M_Random`, then `ST_updateWidgets` / face, then `st_oldhealth`. -/
def stTicker (gs : GameState) : GameState :=
  let (stRandom, rng) := mRandom gs.rng
  let gs := { gs with rng }
  match gs.players[gs.consoleplayer]? with
  | none => gs
  | some plyr =>
    let st := updateFaceWidget gs plyr stRandom
    { gs with stFace := { st with oldhealth := plyr.health } }

/--
`AM_Ticker` — no-op while automap inactive (default). Traced state unaffected.
-/
def amTicker (gs : GameState) : GameState := gs

/-- `i_timer.h` `TICRATE` × 4 (`hu_stuff.h` `HU_MSGTIMEOUT`). -/
def HU_MSGTIMEOUT : Int32 := 140

/-- C `m_menu.c` `showMessages` default. Menu toggle is not this chunk. -/
def showMessages : Bool := true

/--
`HU_Ticker` message path only (no chat/netgame/`message_dontfuckwithme`).
-/
def huTicker (gs0 : GameState) : GameState :=
  Id.run do
    let mut hu := gs0.hu
    if hu.messageCounter != 0 then
      let next := hu.messageCounter - 1
      hu := { hu with messageCounter := next }
      if next == 0 then
        hu := { hu with messageOn := false }
    let mut gs := { gs0 with hu }
    if showMessages then
      match gs.players[gs.consoleplayer]? with
      | none => pure ()
      | some plyr =>
        match plyr.message with
        | none => pure ()
        | some msg =>
          gs := {
            gs with
            players := setArr gs.players gs.consoleplayer { plyr with message := none }
            hu := {
              hu with
              messageText := msg
              messageOn := true
              messageCounter := HU_MSGTIMEOUT
            }
          }
    pure gs

/-- `P_Ticker`. -/
def pTicker (gs0 : GameState) : Except String GameState := do
  let mut gs := gs0
  let mut i : Nat := 0
  while i < MAXPLAYERS do
    match gs.playeringame[i]? with
    | some true =>
      gs ← playerThink gs i
    | _ => pure ()
    i := i + 1
  gs ← runThinkers gs
  gs := updateSpecials gs
  gs := respawnSpecials gs
  pure { gs with leveltime := gs.leveltime + 1 }

/-- Read demo ticcmds into each in-game player's `cmd`. -/
def readDemoCmds (gs0 : GameState) : Except String GameState := do
  let mut gs := gs0
  let mut i : Nat := 0
  while i < MAXPLAYERS do
    match gs.playeringame[i]? with
    | some true =>
      let (cursor, cmd) ← readDemoTiccmd gs.demoBytes gs.demoCursor
      match gs.players[i]? with
      | none => throw "G_Ticker: missing player"
      | some p =>
        gs := {
          gs with
          demoCursor := cursor
          players := setArr gs.players i { p with cmd }
        }
    | _ => pure ()
    i := i + 1
  pure gs

/--
`G_Ticker` for `GS_LEVEL` + `demoplayback`:
read ticcmds → `P_Ticker` → `ST_Ticker` → `AM_Ticker` → `HU_Ticker`.
Dump point is immediately after this returns (`docs/TRACE.md` §1).
-/
def gTicker (gs0 : GameState) : Except String GameState := do
  let mut gs := gs0
  if gs.demoplayback then
    gs ← readDemoCmds gs
  gs ← pTicker gs
  gs := stTicker gs
  gs := amTicker gs
  gs := huTicker gs
  pure gs

end Doom.Playsim.Tick
