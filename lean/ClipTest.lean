import Doom.Render.Clip
import Doom.Render.Constants

/-!
# ClipTest

R1c-clip unit tests: clip seg list + record-only wall ranges.
-/

open Doom.Render.Clip
open Doom.Render.Constants

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def seg (first last : Int32) : ClipRange := ⟨first, last⟩

private def fresh : ClipState :=
  clearClipSegs emptyClipState

def main (_args : List String) : IO UInt32 := do
  let mut ok := true

  -- 1. clearClipSegs golden
  let st1 := fresh
  ok := (← assert "clearClipSegs sentinel0"
    (st1.solidsegs[0]? == some (seg clipSentinelFirst (-1 : Int32)))) && ok
  ok := (← assert "clearClipSegs sentinel1"
    (st1.solidsegs[1]? == some (seg defaultViewwidth clipSentinelLast))) && ok
  ok := (← assert "clearClipSegs newend" (st1.newend == 2)) && ok

  -- 2. clearDrawSegs isolation
  let st2a := fresh
  match clipSolidWallSegment st2a 50 100 with
  | Except.error e =>
    IO.eprintln s!"setup solid insert failed: {e}"
    ok := false
  | Except.ok st2b =>
    let st2c := clearDrawSegs st2b
    ok := (← assert "clearDrawSegs resets drawSegIdx" (st2c.drawSegIdx == 0)) && ok
    ok := (← assert "clearDrawSegs keeps newend"
      (st2c.newend == st2b.newend && st2c.newend == 3)) && ok
    ok := (← assert "clearDrawSegs keeps solidsegs"
      (st2c.solidsegs[1]? == some (seg 50 100))) && ok

  -- 3. pass on empty clip
  let st3 := fresh
  match clipPassWallSegment st3 10 200 with
  | Except.error e =>
    IO.eprintln s!"pass empty failed: {e}"
    ok := false
  | Except.ok st3b =>
    ok := (← assert "pass empty records full range"
      (st3b.recordedWalls == #[(10, 200)])) && ok

  -- 4. solid insert in gap
  let st4 := fresh
  match clipSolidWallSegment st4 50 100 with
  | Except.error e =>
    IO.eprintln s!"solid insert failed: {e}"
    ok := false
  | Except.ok st4b =>
    ok := (← assert "solid insert newend" (st4b.newend == 3)) && ok
    ok := (← assert "solid insert seg"
      (st4b.solidsegs[1]? == some (seg 50 100))) && ok
    ok := (← assert "solid insert records"
      (st4b.recordedWalls == #[(50, 100)])) && ok

  -- 5. full occlude solid
  let st5 :=
    match clipSolidWallSegment fresh 50 100 with
    | Except.ok s => s
    | Except.error e =>
      let _ := IO.eprintln s!"solid setup failed: {e}"
      fresh
  match clipSolidWallSegment st5 60 90 with
  | Except.error e =>
    IO.eprintln s!"solid occlude failed: {e}"
    ok := false
  | Except.ok st5b =>
    ok := (← assert "solid occlude no new record"
      (st5b.recordedWalls == st5.recordedWalls)) && ok
    ok := (← assert "solid occlude unchanged seg"
      (st5b.solidsegs[1]? == some (seg 50 100))) && ok

  -- 6. full occlude pass
  let st6 :=
    match clipSolidWallSegment fresh 50 100 with
    | Except.ok s => s
    | Except.error _ => fresh
  match clipPassWallSegment st6 60 90 with
  | Except.error e =>
    IO.eprintln s!"pass occlude failed: {e}"
    ok := false
  | Except.ok st6b =>
    ok := (← assert "pass occlude no record"
      (st6b.recordedWalls == st6.recordedWalls)) && ok

  -- 7. pass-through window above
  let st7 :=
    match clipSolidWallSegment fresh 50 100 with
    | Except.ok s => s
    | Except.error _ => fresh
  match clipPassWallSegment st7 10 40 with
  | Except.error e =>
    IO.eprintln s!"pass above failed: {e}"
    ok := false
  | Except.ok st7b =>
    ok := (← assert "pass above records fragment"
      (st7b.recordedWalls.back? == some (10, 40))) && ok

  -- 8. pass-through window below
  let st8 :=
    match clipSolidWallSegment fresh 50 100 with
    | Except.ok s => s
    | Except.error _ => fresh
  match clipPassWallSegment st8 101 105 with
  | Except.error e =>
    IO.eprintln s!"pass below failed: {e}"
    ok := false
  | Except.ok st8b =>
    ok := (← assert "pass below records gap"
      (st8b.recordedWalls.back? == some (101, 105))) && ok

  -- 9. adjacent-pixel touching first-1 / last+1
  let st9 :=
    match clipSolidWallSegment fresh 50 100 with
    | Except.ok s => s
    | Except.error _ => fresh
  match clipPassWallSegment st9 49 49 with
  | Except.error e =>
    IO.eprintln s!"adjacent low failed: {e}"
    ok := false
  | Except.ok st9a =>
    ok := (← assert "adjacent first-1 records"
      (st9a.recordedWalls.back? == some (49, 49))) && ok
    match clipPassWallSegment st9a 101 150 with
    | Except.error e =>
      IO.eprintln s!"adjacent high failed: {e}"
      ok := false
    | Except.ok st9b =>
      ok := (← assert "adjacent last+1 records gap"
        (st9b.recordedWalls.back? == some (101, 150))) && ok

  -- 10. solid crunch/merge bridging gap
  let st10a :=
    match clipSolidWallSegment fresh 50 60 with
    | Except.ok s => s
    | Except.error _ => fresh
  let st10b :=
    match clipSolidWallSegment st10a 80 90 with
    | Except.ok s => s
    | Except.error _ => st10a
  match clipSolidWallSegment st10b 55 85 with
  | Except.error e =>
    IO.eprintln s!"crunch merge failed: {e}"
    ok := false
  | Except.ok st10c =>
    ok := (← assert "crunch merge newend" (st10c.newend == 3)) && ok
    ok := (← assert "crunch merge seg"
      (st10c.solidsegs[1]? == some (seg 50 90))) && ok
    ok := (← assert "crunch merge records gap"
      (st10c.recordedWalls.contains (61, 79))) && ok

  -- solidsegs overflow loud-error
  let stOver := { fresh with newend := maxSegs }
  match clipSolidWallSegment stOver 10 20 with
  | Except.error _ =>
    ok := (← assert "solidsegs overflow errors" true) && ok
  | Except.ok _ =>
    ok := (← assert "solidsegs overflow errors" false) && ok

  -- storeWallRange silent overflow at maxDrawSegs
  let stFull := { (clearDrawSegs fresh) with drawSegIdx := maxDrawSegs }
  let stFull' := storeWallRange stFull 1 2
  ok := (← assert "storeWallRange overflow silent"
    (stFull'.drawSegIdx == maxDrawSegs &&
      stFull'.recordedWalls == stFull.recordedWalls)) && ok

  if ok then
    IO.println "clip-test: ALL PASS"
    pure 0
  else
    IO.eprintln "clip-test: FAILURES"
    pure 1
