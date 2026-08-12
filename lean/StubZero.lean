import Doom.Harness.Stubs

open Doom.Harness.Stubs

private def usage : String :=
  "usage: stub-zero --tics N --out PATH_BASE"

private def parseArgs (args : List String) : Except String (Nat × System.FilePath) :=
  let rec go (rest : List String) (tics? : Option Nat) (out? : Option System.FilePath) :
      Except String (Nat × System.FilePath) :=
    match rest with
    | [] =>
      match tics?, out? with
      | some n, some p =>
        if n == 0 then Except.error "--tics must be > 0" else Except.ok (n, p)
      | none, _ => Except.error "missing --tics N"
      | _, none => Except.error "missing --out PATH_BASE"
    | "--tics" :: nStr :: rest =>
      match nStr.toNat? with
      | none => Except.error s!"invalid --tics value: {nStr}"
      | some n => go rest (some n) out?
    | "--out" :: p :: rest => go rest tics? (some p)
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
  | Except.ok (n, pathBase) => do
    try
      runStubZero n pathBase
      pure 0
    catch e => do
      IO.eprintln s!"stub-zero failed: {e}"
      pure 1
