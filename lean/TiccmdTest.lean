import Doom.Playsim.Input
import Doom.Playsim.Player

/-!
# ticcmd-test

G_BuildTiccmd unit tests: walk, run, turn, slow-turn, strafe-mod, `,`/`.`,
fire, use, clamp. No SDL.
-/

open Doom.Playsim.Input
open Doom.Playsim.Player

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def hold (keys : Array Nat) : InputState :=
  keys.foldl (fun s k => setKey s k true) InputState.init

private def cmdOf (keys : Array Nat) : TicCmd :=
  (buildTiccmd (hold keys)).2

def main (_args : List String) : IO UInt32 := do
  let mut ok := true

  let idle := cmdOf #[]
  ok := (← assert "idle zero" (idle == TicCmd.zero)) && ok

  let walk := cmdOf #[key_up]
  ok := (← assert "walk forward 0x19" (walk.forwardmove == (0x19 : Int32))) && ok
  ok := (← assert "walk no side" (walk.sidemove == 0)) && ok
  ok := (← assert "walk no turn" (walk.angleturn == 0)) && ok

  let run := cmdOf #[key_up, key_speed]
  ok := (← assert "run forward 0x32" (run.forwardmove == (0x32 : Int32))) && ok

  let back := cmdOf #[key_down]
  ok := (← assert "walk back -0x19" (back.forwardmove == (-0x19 : Int32))) && ok

  let (s1, turn1) := buildTiccmd (hold #[key_right])
  ok := (← assert "slow turn first tic -320" (turn1.angleturn == (-320 : Int32))) && ok
  ok := (← assert "slow turnheld 1" (s1.turnheld == (1 : Int32))) && ok

  let mut st := hold #[key_right]
  let mut last : TicCmd := TicCmd.zero
  let mut i : Nat := 0
  while i < 6 do
    let (st', cmd) := buildTiccmd st
    st := st'
    last := cmd
    i := i + 1
  ok := (← assert "turnheld after 6" (st.turnheld == (6 : Int32))) && ok
  ok := (← assert "walk turn after slow -640" (last.angleturn == (-640 : Int32))) && ok

  let mut rst := hold #[key_right, key_speed]
  let mut rlast : TicCmd := TicCmd.zero
  i := 0
  while i < 6 do
    let (rst', cmd) := buildTiccmd rst
    rst := rst'
    rlast := cmd
    i := i + 1
  ok := (← assert "run turn after slow -1280" (rlast.angleturn == (-1280 : Int32))) && ok

  let left1 := cmdOf #[key_left]
  ok := (← assert "slow turn left +320" (left1.angleturn == (320 : Int32))) && ok

  let strafeR := cmdOf #[key_strafe, key_right]
  ok := (← assert "strafe-mod right is side" (strafeR.sidemove == (0x18 : Int32))) && ok
  ok := (← assert "strafe-mod no angle" (strafeR.angleturn == 0)) && ok

  let comma := cmdOf #[key_strafeleft]
  ok := (← assert "comma strafe left" (comma.sidemove == (-0x18 : Int32))) && ok

  let period := cmdOf #[key_straferight]
  ok := (← assert "period strafe right" (period.sidemove == (0x18 : Int32))) && ok

  let fire := cmdOf #[key_fire]
  ok := (← assert "fire BT_ATTACK" (fire.buttons == BT_ATTACK)) && ok

  let use := cmdOf #[key_use]
  ok := (← assert "use BT_USE" (use.buttons == BT_USE)) && ok

  let both := cmdOf #[key_fire, key_use]
  ok := (← assert "fire+use bits" (both.buttons == (BT_ATTACK ||| BT_USE))) && ok

  let clamp := cmdOf #[key_strafe, key_right, key_straferight, key_speed]
  ok := (← assert "side clamp to MAXPLMOVE" (clamp.sidemove == MAXPLMOVE)) && ok

  let down := InputState.init
  let downK := gResponder down true key_up.toUInt32
  ok := (← assert "keydown sets gamekeydown" (keyPressed downK key_up)) && ok
  let upK := gResponder downK false key_up.toUInt32
  ok := (← assert "keyup clears gamekeydown" (!keyPressed upK key_up)) && ok

  let esc := gResponder InputState.init true key_escape.toUInt32
  let escCmd := (buildTiccmd esc).2
  ok := (← assert "escape does not set buttons" (escCmd.buttons == 0)) && ok
  ok := (← assert "escape does not move" (escCmd == TicCmd.zero)) && ok

  let (held, _) := buildTiccmd (hold #[key_right])
  let released := { held with gamekeydown := InputState.init.gamekeydown }
  let (reset, _) := buildTiccmd released
  ok := (← assert "turnheld resets on release" (reset.turnheld == 0)) && ok

  let cancelTurn := cmdOf #[key_left, key_right]
  ok := (← assert "left+right cancel turn" (cancelTurn.angleturn == 0)) && ok

  let cancelWalk := cmdOf #[key_up, key_down]
  ok := (← assert "up+down cancel walk" (cancelWalk.forwardmove == 0)) && ok

  let runSide := cmdOf #[key_straferight, key_speed]
  ok := (← assert "run period sidemove 0x28" (runSide.sidemove == (0x28 : Int32))) && ok

  let runStrafeMod := cmdOf #[key_strafe, key_right, key_speed]
  ok := (← assert "run strafe-mod side 0x28" (runStrafeMod.sidemove == (0x28 : Int32))) && ok
  ok := (← assert "run strafe-mod no angle" (runStrafeMod.angleturn == 0)) && ok

  if ok then
    IO.println "ticcmd-test PASS"
    pure 0
  else
    pure 1
