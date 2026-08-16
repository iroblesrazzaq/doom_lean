import Doom.Playsim.Flags
import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Inter
import Doom.Playsim.Level
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Thinker
import Doom.Playsim.Tick
import Doom.Render.Gfx.Patch
import Doom.Render.Hud
import Doom.Render.Types
import Doom.Wad

/-!
HU message path: BON2 → GOTARMBONUS → HU_Ticker consume; tic-35 off;
HUlib spacing/clip; HU_Drawer STCFN patches.
-/

open Doom.Playsim.Flags
open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Inter
open Doom.Playsim.Level
open Doom.Playsim.Thinker
open Doom.Playsim.Tick
open Doom.Render.Gfx.Patch
open Doom.Render.Hud
open Doom.Render.Types
open Doom.Wad

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" => pure (cwd.parent.getD cwd)
  | _ => pure cwd

private def loadIwad : IO WadDirectory := do
  let root ← defaultRoot
  loadFile (root / "fixtures" / "wads" / "doom1.wad")

private def emptyLevel : LevelData := {
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

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def bon2Scene : GameState :=
  let gs0 := initFromLevel emptyLevel 2 #[true, false, false, false] 0
  let toucher := {
    Doom.Playsim.Mobj.empty with
    player := 0, health := 100, height := 56 * FRACUNIT
    flags := MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let special := {
    Doom.Playsim.Mobj.empty with
    sprite := SPR_BON2
    flags := MF_SPECIAL ||| MF_NOSECTOR ||| MF_NOBLOCKMAP
  }
  let player := { Doom.Playsim.Player.empty with mo := 0, health := 100 }
  let th : Thinker := { traceId := 1, func := THF_MOBJ, payload := 1 }
  {
    gs0 with
    mobjs := #[toucher, special]
    thinkers := #[th]
    players := Doom.Playsim.GameState.arrSet gs0.players 0 player
    hu := HuState.init
  }

private def withMessage (gs : GameState) (on : Bool) (text : String) : GameState :=
  { gs with hu := { gs.hu with messageOn := on, messageText := text, messageCounter := HU_MSGTIMEOUT } }

private def repeatChar (c : Char) (n : Nat) : String :=
  Id.run do
    let mut s := ""
    let mut i := 0
    while i < n do
      s := s.push c
      i := i + 1
    pure s

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let wad ← loadIwad
  let gs0 := initFromLevel emptyLevel 2 #[true, false, false, false] 0

  let started := huStart gs0
  ok := (← assert "HU_Start message_on=false" (!started.hu.messageOn)) && ok
  let ticked := huTicker started
  ok := (← assert "tic-35 path: no player.message keeps message_on=false"
    (!ticked.hu.messageOn && ticked.hu.messageCounter == 0)) && ok

  match touchSpecialThing bon2Scene 1 0 with
  | Except.error e =>
    ok := (← assert s!"BON2 pickup ({e})" false) && ok
  | Except.ok gsPick =>
    match gsPick.players[0]? with
    | none =>
      ok := (← assert "BON2 player" false) && ok
    | some p =>
      ok := (← assert "BON2 sets GOTARMBONUS"
        (p.message == some GOTARMBONUS)) && ok
      let gsHu := huTicker gsPick
      match gsHu.players[0]? with
      | none =>
        ok := (← assert "HU consume player" false) && ok
      | some p2 =>
        ok := (← assert "HU_Ticker clears player.message" (p2.message == none)) && ok
        ok := (← assert "HU_Ticker message_on + timeout 140"
          (gsHu.hu.messageOn
            && gsHu.hu.messageCounter == HU_MSGTIMEOUT
            && gsHu.hu.messageText == GOTARMBONUS)) && ok
        let mut gsOff := gsHu
        let mut n : Nat := 0
        while n < 139 do
          gsOff := huTicker gsOff
          n := n + 1
        ok := (← assert "message still on after 139 ticks"
          gsOff.hu.messageOn) && ok
        gsOff := huTicker gsOff
        ok := (← assert "message_on false after HU_MSGTIMEOUT ticks"
          (!gsOff.hu.messageOn && gsOff.hu.messageCounter == 0)) && ok

  match drawTextLine wad Framebuffer.initBlack 0 0 " A" with
  | Except.error e =>
    ok := (← assert s!"space draw: {e}" false) && ok
  | Except.ok spaced =>
    match drawTextLine wad Framebuffer.initBlack 4 0 "A" with
    | Except.error e =>
      ok := (← assert s!"A-at-4 draw: {e}" false) && ok
    | Except.ok at4 =>
      ok := (← assert "space / <'!' / >'_' advance 4"
        (spaced.pixels == at4.pixels)) && ok
  match drawTextLine wad Framebuffer.initBlack 0 0 "{A" with
  | Except.error e =>
    ok := (← assert s!"outrange draw: {e}" false) && ok
  | Except.ok outr =>
    match drawTextLine wad Framebuffer.initBlack 4 0 "A" with
    | Except.error e =>
      ok := (← assert s!"outrange A-at-4: {e}" false) && ok
    | Except.ok at4 =>
      ok := (← assert "char > '_' advances 4 like space"
        (outr.pixels == at4.pixels)) && ok

  match checkNumForName wad (stcfnName 'A'.toNat) with
  | none =>
    ok := (← assert "STCFN065 present" false) && ok
  | some idx =>
    match loadPatch wad idx with
    | Except.error e =>
      ok := (← assert s!"STCFN065 header: {e}" false) && ok
    | Except.ok (hdr, _) =>
      let w := hdr.width.toUInt32.toNat
      if w == 0 then
        ok := (← assert "STCFN065 width > 0" false) && ok
      else
        let n := screenWidth / w
        let ones := repeatChar 'A' n
        let extra := repeatChar 'A' (n + 1)
        match drawTextLine wad Framebuffer.initBlack 0 0 ones,
              drawTextLine wad Framebuffer.initBlack 0 0 extra with
        | Except.ok a, Except.ok b =>
          ok := (← assert "HUlib clips glyph when x+w > 320"
            (a.pixels == b.pixels)) && ok
        | Except.error e, _ =>
          ok := (← assert s!"clip ones: {e}" false) && ok
        | _, Except.error e =>
          ok := (← assert s!"clip extra: {e}" false) && ok

  let gsMsg := withMessage gs0 true GOTARMBONUS
  match drawer wad gsMsg Framebuffer.initBlack with
  | Except.error e =>
    ok := (← assert s!"HU_Drawer: {e}" false) && ok
  | Except.ok fb =>
    let mut painted := false
    let mut x : Nat := 0
    while x < 80 && !painted do
      if Framebuffer.get fb x 0 != 0 || Framebuffer.get fb x 1 != 0 then
        painted := true
      x := x + 1
    ok := (← assert "HU_Drawer paints STCFN at HU_MSGX/Y=0,0" painted) && ok
  match drawer wad gs0 Framebuffer.initBlack with
  | Except.error e =>
    ok := (← assert s!"HU_Drawer off: {e}" false) && ok
  | Except.ok fbOff =>
    ok := (← assert "HU_Drawer no-op when message_on=false"
      (fbOff.pixels == Framebuffer.initBlack.pixels)) && ok

  if ok then
    IO.println "hu-test: all passed"
    pure 0
  else
    IO.eprintln "hu-test: FAILURES"
    pure 1
