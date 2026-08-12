import Doom.Harness.Stubs

open Doom.Harness.Stubs

private def usage : String :=
  "usage: stub-drift --ref-trace REF.trc --out PATH_BASE"

private def parseArgs (args : List String) : Except String (System.FilePath × System.FilePath) :=
  let rec go (rest : List String) (ref? : Option System.FilePath) (out? : Option System.FilePath) :
      Except String (System.FilePath × System.FilePath) :=
    match rest with
    | [] =>
      match ref?, out? with
      | some r, some o => Except.ok (r, o)
      | none, _ => Except.error "missing --ref-trace REF.trc"
      | _, none => Except.error "missing --out PATH_BASE"
    | "--ref-trace" :: p :: rest => go rest (some p) out?
    | "--out" :: p :: rest => go rest ref? (some p)
    | other :: _ => Except.error s!"unknown or incomplete argument: {other}"
  go args none none

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
  | Except.ok (refTrace, pathBase) => do
    try
      let result ← runStubDrift refTrace pathBase
      match result with
      | Except.error e => do
        IO.eprintln e
        pure 1
      | Except.ok tid => do
        IO.println s!"perturbed trace_id={tid} at tic 500"
        pure 0
    catch e => do
      IO.eprintln s!"stub-drift failed: {e}"
      pure 1
