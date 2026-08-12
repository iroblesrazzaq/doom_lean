import Doom.Harness.Stubs
import Doom.Harness.Real

/-!
# verify

Doom Lean verification harness.

Resolves `tools/tracediff` relative to `--root`. Default `--root` is the parent of the
current working directory when the cwd's last component is `lean`, otherwise the cwd itself.
-/

open Doom.Harness.Stubs
open Doom.Harness.Real

private def usage : String :=
  "usage: verify --iwad PATH --demo NAME --ref-digest REF.dig --impl (stub-zero|stub-drift|real) " ++
  "--out-dir DIR [--tics N] [--ref-trace REF.trc] [--root ROOT]"

inductive ImplKind where
  | stubZero
  | stubDrift
  | real
  deriving BEq, Repr

structure VerifyArgs where
  iwad : System.FilePath
  demo : String
  refDigest : System.FilePath
  impl : ImplKind
  outDir : System.FilePath
  tics : Option Nat
  refTrace : Option System.FilePath
  root : Option System.FilePath
  deriving Repr

private def parseImpl (s : String) : Except String ImplKind :=
  match s with
  | "stub-zero" => Except.ok .stubZero
  | "stub-drift" => Except.ok .stubDrift
  | "real" => Except.ok .real
  | _ => Except.error s!"unknown --impl {s} (expected stub-zero|stub-drift|real)"

private def parseArgs (args : List String) : Except String VerifyArgs :=
  let rec go (rest : List String) (acc : VerifyArgs) : Except String VerifyArgs :=
    match rest with
    | [] =>
      if acc.iwad.toString == "" then Except.error "missing --iwad PATH"
      else if acc.demo == "" then Except.error "missing --demo NAME"
      else if acc.refDigest.toString == "" then Except.error "missing --ref-digest REF.dig"
      else if acc.outDir.toString == "" then Except.error "missing --out-dir DIR"
      else Except.ok acc
    | "--iwad" :: p :: rest => go rest { acc with iwad := p }
    | "--demo" :: p :: rest => go rest { acc with demo := p }
    | "--ref-digest" :: p :: rest => go rest { acc with refDigest := p }
    | "--impl" :: p :: rest => do
      let impl ← parseImpl p
      go rest { acc with impl }
    | "--out-dir" :: p :: rest => go rest { acc with outDir := p }
    | "--tics" :: nStr :: rest =>
      match nStr.toNat? with
      | none => Except.error s!"invalid --tics value: {nStr}"
      | some n => go rest { acc with tics := some n }
    | "--ref-trace" :: p :: rest => go rest { acc with refTrace := some p }
    | "--root" :: p :: rest => go rest { acc with root := some p }
    | other :: _ => Except.error s!"unknown or incomplete argument: {other}"
  go args {
    iwad := ""
    demo := ""
    refDigest := ""
    impl := .stubZero
    outDir := ""
    tics := none
    refTrace := none
    root := none
  }

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  let parts := cwd.components
  match parts.getLast? with
  | some "lean" =>
    match cwd.parent with
    | some p => pure p
    | none => pure cwd
  | _ => pure cwd

private def resolveRoot (explicit : Option System.FilePath) : IO System.FilePath :=
  match explicit with
  | some r => pure r
  | none => defaultRoot

private def stripLakeSep (args : List String) : List String :=
  match args with
  | "--" :: rest => rest
  | _ => args

def main (args : List String) : IO UInt32 := do
  match parseArgs (stripLakeSep args) with
  | Except.error e => do
    IO.eprintln e
    IO.eprintln usage
    pure 2
  | Except.ok va => do
    IO.println s!"verify: iwad={va.iwad} demo={va.demo} impl={repr va.impl}"
    let root ← resolveRoot va.root
    let candBase : System.FilePath := va.outDir / "candidate"
    try
      IO.FS.createDirAll va.outDir
      match va.impl with
      | .stubZero =>
        match va.tics with
        | none => do
          IO.eprintln "stub-zero impl requires --tics N"
          return 2
        | some n =>
          runStubZero n candBase
      | .stubDrift =>
        match va.refTrace with
        | none => do
          IO.eprintln "stub-drift impl requires --ref-trace REF.trc"
          return 2
        | some ref => do
          let result ← runStubDrift ref candBase
          match result with
          | Except.error e => do
            IO.eprintln e
            return 1
          | Except.ok tid =>
            IO.println s!"perturbed trace_id={tid} at tic 500"
      | .real =>
        match va.tics with
        | none => do
          IO.eprintln "real impl requires --tics N"
          return 2
        | some n => do
          match ← runReal va.iwad va.demo n candBase with
          | Except.error e => do
            IO.eprintln e
            return 1
          | Except.ok () =>
            IO.println s!"real: wrote {n} tic(s) to {candBase}"
      let tracediff := root / "tools" / "tracediff"
      let mut diffArgs : Array String := #[
        tracediff.toString,
        va.refDigest.toString,
        (va.outDir / "candidate.dig").toString
      ]
      match va.refTrace with
      | some rt =>
        diffArgs := diffArgs.push "--ref-trace" |>.push rt.toString
      | none => pure ()
      diffArgs := diffArgs.push "--cand-trace" |>.push (va.outDir / "candidate.trc").toString
      IO.println s!"spawning: python3 {diffArgs}"
      let child ← IO.Process.spawn {
        cmd := "python3"
        args := diffArgs
        stdout := .inherit
        stderr := .inherit
      }
      child.wait
    catch e => do
      let msg := toString e
      if msg.contains "No such file" || msg.contains "no such file" || msg.contains "enoent" then
        IO.eprintln s!"failed to spawn tools/tracediff at {root / "tools" / "tracediff"}: {e}"
        IO.eprintln "ensure --root points at the workspace root containing tools/tracediff"
      else
        IO.eprintln s!"verify failed: {e}"
      pure 1
