/-!
# doom-window-test

Playable-window E2E contract (public `lake exe doom` CLI):

- missing `-iwad` prints usage and exits 2
- `SDL_VIDEODRIVER=dummy` + `-iwad <doom1.wad> --quit-after-tics 1` exits 0
- dummy + `--quit-after-tics 2` exits 0 (bootstrap tic + one displayed frame)
-/

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" => pure (cwd.parent.getD cwd)
  | _ => pure cwd

private def doomBin : IO System.FilePath := do
  let app ← IO.appPath
  pure (app.parent.getD app / "doom")

private def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let bin ← doomBin
  if !(← bin.pathExists) then
    IO.eprintln s!"FAIL: missing doom binary at {bin}"
    return 1
  let root ← defaultRoot
  let iwad := root / "fixtures" / "wads" / "doom1.wad"
  if !(← iwad.pathExists) then
    IO.eprintln s!"FAIL: missing IWAD {iwad}"
    return 1

  let missing ← IO.Process.output {
    cmd := bin.toString
    args := #[]
  }
  ok := (← assert "missing -iwad exit 2" (missing.exitCode == 2)) && ok
  let usageText := missing.stdout ++ missing.stderr
  let usageOk := usageText.contains "iwad" || usageText.contains "usage"
  ok := (← assert "missing -iwad prints usage" usageOk) && ok

  let dummy1 ← IO.Process.output {
    cmd := bin.toString
    args := #["-iwad", iwad.toString, "--quit-after-tics", "1"]
    env := #[("SDL_VIDEODRIVER", some "dummy")]
  }
  if dummy1.exitCode != 0 then
    IO.eprintln dummy1.stdout
    IO.eprintln dummy1.stderr
  ok := (← assert "dummy --quit-after-tics 1 exit 0" (dummy1.exitCode == 0)) && ok

  let dummy2 ← IO.Process.output {
    cmd := bin.toString
    args := #["-iwad", iwad.toString, "--quit-after-tics", "2"]
    env := #[("SDL_VIDEODRIVER", some "dummy")]
  }
  if dummy2.exitCode != 0 then
    IO.eprintln dummy2.stdout
    IO.eprintln dummy2.stderr
  ok := (← assert "dummy --quit-after-tics 2 exit 0" (dummy2.exitCode == 0)) && ok

  let badQuit ← IO.Process.output {
    cmd := bin.toString
    args := #["-iwad", iwad.toString, "--quit-after-tics"]
  }
  ok := (← assert "truncated --quit-after-tics exit 2" (badQuit.exitCode == 2)) && ok

  let badQuitVal ← IO.Process.output {
    cmd := bin.toString
    args := #["-iwad", iwad.toString, "--quit-after-tics", "nope"]
  }
  ok := (← assert "invalid --quit-after-tics exit 2" (badQuitVal.exitCode == 2)) && ok

  if ok then
    IO.println "doom-window-test PASS"
    pure 0
  else
    pure 1
