import Doom.Harness.Fnv
import Doom.Render.Dump
import Doom.Render.Palette
import Doom.Render.Types

/-!
# dump-hex-test

Isolated FNV hex-dump contract: `writeFnv` emits 16 lowercase MSB-first hex
digits plus newline (`fprintf %016llx`), including vector `0xee13be115dc0314e`.
-/

open Doom.Harness.Fnv
open Doom.Render.Dump
open Doom.Render.Palette
open Doom.Render.Types

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" => pure (cwd.parent.getD cwd)
  | _ => pure cwd

/-- Spec encoder: nibble 15 … nibble 0, lowercase, matching C `%016llx`. -/
private def msbHex16 (h : UInt64) : String :=
  String.ofList <| List.range 16 |>.map fun i =>
    let n := ((h >>> (UInt64.ofNat ((15 - i) * 4))) &&& 0xf).toNat
    if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  if msbHex16 0xee13be115dc0314e != "ee13be115dc0314e" then
    IO.eprintln "dump-hex-test FAIL: spec vector 0xee13be115dc0314e"
    ok := false
  if msbHex16 0 != "0000000000000000" then
    IO.eprintln "dump-hex-test FAIL: spec vector 0 pad"
    ok := false
  if msbHex16 0xf != "000000000000000f" then
    IO.eprintln "dump-hex-test FAIL: spec vector 0xf pad"
    ok := false
  let root ← defaultRoot
  let outDir := root / ".agent_tmp" / "dump_hex_cand"
  IO.FS.createDirAll outDir
  let path := outDir / "vec.fnv"
  let fb := Framebuffer.initBlack
  let pal := ByteArray.mk (Array.replicate playpalSize (UInt8.ofNat 0))
  writeFnv path fb pal
  let got ← IO.FS.readFile path
  let data := fb.pixels.append pal
  let want := msbHex16 (fnv1a64 data) ++ "\n"
  if got != want then
    IO.eprintln "dump-hex-test FAIL: writeFnv hex mismatch"
    IO.eprintln s!"  got  {got}"
    IO.eprintln s!"  want {want}"
    ok := false
  if got.length != 17 then
    IO.eprintln s!"dump-hex-test FAIL: size {got.length}, expected 17"
    ok := false
  if ok then
    IO.println "dump-hex-test PASS: MSB-first hex including ee13be115dc0314e"
    pure 0
  else
    pure 1
