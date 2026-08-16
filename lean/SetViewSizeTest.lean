import Doom.Playsim.Fixed
import Doom.Render.Main
import Doom.Render.View

/-!
# SetViewSizeTest

R1x-setviewsize: `R_SetViewSize` records pending size; `R_ExecuteSetViewSize`
for defaults `blocks=10`, `detail=0` matches C (`r_main.c`).
-/

open Doom.Playsim.Fixed
open Doom.Render.Main
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

  let pending := setViewSize 10 0
  ok := (← assert "setViewSize setsizeneeded" pending.setsizeneeded) && ok
  ok := (← assert "setViewSize setblocks" (pending.setblocks == 10)) && ok
  ok := (← assert "setViewSize setdetail" (pending.setdetail == 0)) && ok

  let (vs, pending') := executeSetViewSize pending
  ok := (← assert "execute clears setsizeneeded" (!pending'.setsizeneeded)) && ok
  ok := (← assert "scaledviewwidth" (vs.scaledviewwidth == 320)) && ok
  ok := (← assert "viewwidth" (vs.viewwidth == 320)) && ok
  ok := (← assert "viewheight" (vs.viewheight == 168)) && ok
  ok := (← assert "centerx" (vs.centerx == 160)) && ok
  ok := (← assert "centery" (vs.centery == 84)) && ok
  ok := (← assert "centerxfrac" (vs.centerxfrac == (160 : Int32) <<< 16)) && ok
  ok := (← assert "centeryfrac" (vs.centeryfrac == (84 : Int32) <<< 16)) && ok
  ok := (← assert "projection" (vs.projection == vs.centerxfrac)) && ok
  ok := (← assert "viewwindowx" (vs.viewwindowx == 0)) && ok
  ok := (← assert "viewwindowy" (vs.viewwindowy == 0)) && ok
  ok := (← assert "screenheightarray size" (vs.screenheightarray.size == 320)) && ok
  ok := (← assert "screenheightarray all 168"
    (vs.screenheightarray.all (· == (168 : Int32)))) && ok
  ok := (← assert "yslope size" (vs.yslope.size == 168)) && ok
  ok := (← assert "yslope[84]" (vs.yslope.getD 84 0 == 20971520)) && ok
  ok := (← assert "yslope[99]" (vs.yslope.getD 99 0 == 676500)) && ok
  ok := (← assert "yslope[100]" (vs.yslope.getD 100 0 == 635500)) && ok
  ok := (← assert "distscale size" (vs.distscale.size == 320)) && ok
  ok := (← assert "distscale[160]" (vs.distscale.getD 160 0 == 65537)) && ok
  ok := (← assert "distscale[0]" (vs.distscale.getD 0 0 == 92789)) && ok
  ok := (← assert "distscale[319]" (vs.distscale.getD 319 0 == 92364)) && ok
  ok := (← assert "pspritescale" (vs.pspritescale == FRACUNIT)) && ok
  ok := (← assert "pspriteiscale" (vs.pspriteiscale == FRACUNIT)) && ok

  let (vs11, _) := executeSetViewSize (setViewSize 11 0)
  ok := (← assert "blocks=11 viewheight" (vs11.viewheight == 200)) && ok
  ok := (← assert "blocks=11 centery" (vs11.centery == 100)) && ok

  let (vs9, _) := executeSetViewSize (setViewSize 9 0)
  ok := (← assert "blocks=9 scaledviewwidth" (vs9.scaledviewwidth == 288)) && ok
  ok := (← assert "blocks=9 viewwidth" (vs9.viewwidth == 288)) && ok
  ok := (← assert "blocks=9 viewheight" (vs9.viewheight == 144)) && ok
  ok := (← assert "blocks=9 centery" (vs9.centery == 72)) && ok
  ok := (← assert "blocks=9 projection is centerxfrac" (vs9.projection == vs9.centerxfrac)) && ok
  ok := (← assert "blocks=9 viewwindowx" (vs9.viewwindowx == 16)) && ok
  ok := (← assert "blocks=9 viewwindowy" (vs9.viewwindowy == 12)) && ok

  if ok then
    IO.println "setviewsize-test: ALL PASS"
    pure 0
  else
    IO.eprintln "setviewsize-test: FAILURES"
    pure 1
