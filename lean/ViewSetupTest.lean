import Doom.Render.View

/-!
# ViewSetupTest

R1d-viewsetup unit tests: `initViewMapping` goldens for viewwidth 320, centerx 160.
-/

open Doom.Render.View

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let m := initViewMapping 320 160

  ok := (← assert "viewangletox size" (m.viewangletox.size == 4096)) && ok
  ok := (← assert "xtoviewangle size" (m.xtoviewangle.size == 321)) && ok

  ok := (← assert "viewangletox[0]"
    (m.viewangletox.getD 0 0 == 320)) && ok
  ok := (← assert "viewangletox[2048]"
    (m.viewangletox.getD 2048 0 == 160)) && ok
  ok := (← assert "viewangletox[4095]"
    (m.viewangletox.getD 4095 0 == 0)) && ok

  ok := (← assert "xtoviewangle[0]"
    (m.xtoviewangle.getD 0 0 == 537395200)) && ok
  ok := (← assert "xtoviewangle[160]"
    (m.xtoviewangle.getD 160 0 == 0)) && ok
  ok := (← assert "xtoviewangle[319]"
    (m.xtoviewangle.getD 319 0 == 3760193536)) && ok

  ok := (← assert "clipangle"
    (m.clipangle == 537395200)) && ok

  if ok then
    IO.println "view-setup-test: ALL PASS"
    pure 0
  else
    IO.eprintln "view-setup-test: FAILURES"
    pure 1
