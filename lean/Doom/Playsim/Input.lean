import Doom.Playsim.Player

/-!
# Doom.Playsim.Input

Keyboard `gamekeydown` + `G_BuildTiccmd` / `G_Responder` (keydown/keyup only).
Vanilla tables from `g_game.c` / `m_controls.c`. Mouse, joystick, weapon
change, pause, and save are out of scope.
-/

namespace Doom.Playsim.Input

open Doom.Playsim.Player

def NUMKEYS : Nat := 256
def SLOWTURNTICS : Int32 := 6
def TICDUP : Int32 := 1

/-- `doomkeys.h` / `m_controls.c` defaults that already drive playsim. -/
def KEY_RIGHTARROW : Nat := 0xae
def KEY_LEFTARROW : Nat := 0xac
def KEY_UPARROW : Nat := 0xad
def KEY_DOWNARROW : Nat := 0xaf
def KEY_ESCAPE : Nat := 27
def KEY_RSHIFT : Nat := 0x80 + 0x36
def KEY_RCTRL : Nat := 0x80 + 0x1d
def KEY_RALT : Nat := 0x80 + 0x38

def key_right : Nat := KEY_RIGHTARROW
def key_left : Nat := KEY_LEFTARROW
def key_up : Nat := KEY_UPARROW
def key_down : Nat := KEY_DOWNARROW
def key_strafeleft : Nat := ','.toNat
def key_straferight : Nat := '.'.toNat
def key_fire : Nat := KEY_RCTRL
def key_use : Nat := ' '.toNat
def key_strafe : Nat := KEY_RALT
def key_speed : Nat := KEY_RSHIFT
def key_escape : Nat := KEY_ESCAPE

/-- `g_game.c` `forwardmove[2]`. -/
def forwardmove : Array Int32 := #[0x19, 0x32]
/-- `g_game.c` `sidemove[2]`. -/
def sidemove : Array Int32 := #[0x18, 0x28]
/-- `g_game.c` `angleturn[3]` (+ slow turn). -/
def angleturn : Array Int32 := #[640, 1280, 320]
/-- `MAXPLMOVE` = `forwardmove[1]`. -/
def MAXPLMOVE : Int32 := 0x32

structure InputState where
  gamekeydown : Array Bool
  turnheld : Int32

def InputState.init : InputState := {
  gamekeydown := Array.replicate NUMKEYS false
  turnheld := 0
}

private def setBool (arr : Array Bool) (i : Nat) (v : Bool) : Array Bool :=
  if h : i < arr.size then arr.set i v else arr

def setKey (inp : InputState) (k : Nat) (down : Bool) : InputState :=
  if k < NUMKEYS then
    { inp with gamekeydown := setBool inp.gamekeydown k down }
  else
    inp

def keyPressed (inp : InputState) (k : Nat) : Bool :=
  match inp.gamekeydown[k]? with
  | some b => b
  | none => false

/-- `G_Responder` keydown/keyup only (`NUMKEYS=256`). -/
def gResponder (inp : InputState) (down : Bool) (key : UInt32) : InputState :=
  setKey inp key.toNat down

private def tableGet (arr : Array Int32) (i : Nat) : Int32 :=
  match arr[i]? with
  | some v => v
  | none => 0

private def clampMove (v : Int32) : Int32 :=
  if v > MAXPLMOVE then MAXPLMOVE
  else if v < -MAXPLMOVE then -MAXPLMOVE
  else v

/--
`G_BuildTiccmd` for keyboard + `ticdup=1`. No mouse, joystick, weapon keys,
chat, pause, or save.
-/
def buildTiccmd (inp0 : InputState) : InputState × TicCmd :=
  Id.run do
    let strafe := keyPressed inp0 key_strafe
    let speed : Nat := if keyPressed inp0 key_speed then 1 else 0
    let turning := keyPressed inp0 key_right || keyPressed inp0 key_left
    let inp :=
      if turning then { inp0 with turnheld := inp0.turnheld + TICDUP }
      else { inp0 with turnheld := 0 }
    let tspeed : Nat :=
      if inp.turnheld < SLOWTURNTICS then 2 else speed
    let mut cmd := TicCmd.zero
    let mut forward : Int32 := 0
    let mut side : Int32 := 0
    if strafe then
      if keyPressed inp key_right then
        side := side + tableGet sidemove speed
      if keyPressed inp key_left then
        side := side - tableGet sidemove speed
    else
      if keyPressed inp key_right then
        cmd := { cmd with angleturn := cmd.angleturn - tableGet angleturn tspeed }
      if keyPressed inp key_left then
        cmd := { cmd with angleturn := cmd.angleturn + tableGet angleturn tspeed }
    if keyPressed inp key_up then
      forward := forward + tableGet forwardmove speed
    if keyPressed inp key_down then
      forward := forward - tableGet forwardmove speed
    if keyPressed inp key_strafeleft then
      side := side - tableGet sidemove speed
    if keyPressed inp key_straferight then
      side := side + tableGet sidemove speed
    if keyPressed inp key_fire then
      cmd := { cmd with buttons := cmd.buttons ||| BT_ATTACK }
    if keyPressed inp key_use then
      cmd := { cmd with buttons := cmd.buttons ||| BT_USE }
    cmd := {
      cmd with
      forwardmove := cmd.forwardmove + clampMove forward
      sidemove := cmd.sidemove + clampMove side
    }
    (inp, cmd)

end Doom.Playsim.Input
