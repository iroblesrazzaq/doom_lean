import Doom.Playsim.Random

/-!
# Doom.Harness.DisplaySim

Display/wipe side effects that advance `M_Random` (`rndindex`) outside the
playsim ticker. Mirrors chocolate-doom `D_Display` / `wipe_initMelt` RNG only
(no framebuffer).

Attribution (oracle `chocolate-doom/src/doom`):
* `f_wipe.c` `wipe_initMelt`: `y[0] = -(M_Random()%16)` then
  `for (i=1; i<width; i++)` another `M_Random` → exactly `width` draws.
* `i_video.h` `SCREENWIDTH` = 320, and `d_main.c` `D_RunFrame` calls
  `wipe_ScreenWipe(..., SCREENWIDTH, ...)` → **320** `M_Random` draws.
* `d_net.c` `RunTic`: `G_Ticker` then `OTrace_TicDump`.
* `d_main.c` `D_DoomLoop`: bootstrap `TryRunTics` (tic 0, no display), then
  `D_RunFrame` loop = tic + optional wipe.
* `d_main.c` `wipegamestate` defaults to `GS_DEMOSCREEN`; first level frame
  with `gamestate == GS_LEVEL` triggers one wipe. (`G_DoLoadLevel` may set
  `wipegamestate = -1` to force a wipe on later level loads — not needed for
  DEMO1 single-level.)

Arithmetic for DEMO1: after tic 1 dump `rndindex=2`; wipe +320 → 66; tic 2
`ST_Ticker` +1 → fixture `rndindex=67`.
-/

namespace Doom.Harness.DisplaySim

open Doom.Playsim.Random

/-- `gamestate_t` ordinals from `doomdef.h`. -/
def gsLevel : Int32 := 0
def gsIntermission : Int32 := 1
def gsFinale : Int32 := 2
def gsDemoscreen : Int32 := 3

/-- Vanilla `SCREENWIDTH` (`i_video.h`). -/
def screenWidth : Nat := 320

/-- C `wipegamestate` (`d_main.c`). -/
structure DisplayState where
  wipegamestate : Int32
  deriving Repr, DecidableEq

/-- Default matches C `wipegamestate = GS_DEMOSCREEN`. -/
def initDisplay : DisplayState := { wipegamestate := gsDemoscreen }

/--
`wipe_initMelt` RNG side-effect only: exactly `SCREENWIDTH` discarded
`M_Random` draws (`f_wipe.c`).
-/
def wipeInitMelt (s0 : RandomState) : RandomState :=
  let rec go (n : Nat) (s : RandomState) : RandomState :=
    match n with
    | 0 => s
    | n' + 1 =>
      let (_, s') := mRandom s
      go n' s'
  go screenWidth s0

/--
Harness stand-in for the post-tic display step: if `gamestate != wipegamestate`,
run melt-init RNG then set `wipegamestate := gamestate` (as `D_Display` does).
Returns updated display state and RNG (caller writes RNG back onto `GameState`).
-/
def onFrame (disp : DisplayState) (rng : RandomState) (gamestate : Int32) :
    DisplayState × RandomState :=
  if gamestate != disp.wipegamestate then
    ({ wipegamestate := gamestate }, wipeInitMelt rng)
  else
    (disp, rng)

end Doom.Harness.DisplaySim
