import Doom.Playsim.Fixed
import Doom.Wad

/-!
# Doom.Render.Util

Small helpers shared across renderer modules.
-/

namespace Doom.Render.Util

open Doom.Playsim.Fixed
open Doom.Wad

/-- C `abs()` on signed 32-bit (wrapping negation). -/
def cabs (a : Int32) : Int32 := wabs a

def u32AsI32 (n : UInt32) : Int32 := n.toInt32

def i32AsU32 (n : Int32) : UInt32 := n.toUInt32

def i32ToNat (n : Int32) : Nat :=
  if n < 0 then 0 else (n.toInt).natAbs

def usize16 (n : Int32) : Nat :=
  if n < 0 then 0 else (n.toInt).natAbs

def setByte (bs : ByteArray) (i : Nat) (v : UInt8) : ByteArray :=
  if h : i < bs.size then bs.set i v h else bs

def readI32LE (data : ByteArray) (off : Nat) : Except String Int32 := do
  match data[off]?, data[off + 1]?, data[off + 2]?, data[off + 3]? with
  | some b0, some b1, some b2, some b3 =>
    let u :=
      b0.toUInt32
        ||| (b1.toUInt32 <<< 8)
        ||| (b2.toUInt32 <<< 16)
        ||| (b3.toUInt32 <<< 24)
    pure u.toInt32
  | _, _, _, _ => throw s!"EOF reading i32 at {off}"

def readI16LE (data : ByteArray) (off : Nat) : Except String Int32 := do
  match data[off]?, data[off + 1]? with
  | some b0, some b1 =>
    let u := b0.toUInt16 ||| (b1.toUInt16 <<< 8)
    pure u.toInt16.toInt32
  | _, _ => throw s!"EOF reading i16 at {off}"

def readU16LE (data : ByteArray) (off : Nat) : Except String UInt16 := do
  match data[off]?, data[off + 1]? with
  | some b0, some b1 =>
    pure (b0.toUInt16 ||| (b1.toUInt16 <<< 8))
  | _, _ => throw s!"EOF reading u16 at {off}"

def lumpNameHash (name : ByteArray) : Nat :=
  let rec go (i : Nat) (result : Nat) : Nat :=
    if i >= 8 then result
    else
      match name[i]? with
      | none => result
      | some b =>
        if b == 0 then result
        else
          let c := if b >= 97 && b <= 122 then b - 32 else b
          go (i + 1) ((((result <<< 5) ^^^ result) ^^^ c.toNat))
  go 0 5381

def namesEqual8 (a b : ByteArray) : Bool := namesEqualCI8 a b

def isNoTexture (name : ByteArray) : Bool :=
  match name[0]? with
  | some b => b == 45
  | none => false

def findLumpByName8 (wad : WadDirectory) (target : ByteArray) : Option Nat :=
  let t := if target.size >= 8 then target.extract 0 8 else target
  Id.run do
    let mut i := wad.entries.size
    let mut found : Option Nat := none
    while i > 0 && found.isNone do
      i := i - 1
      match wad.entries[i]? with
      | some e =>
        if namesEqual8 e.name t then found := some i
      | none => pure ()
    pure found

def arrSetI32 (arr : Array Int32) (i : Nat) (v : Int32) : Array Int32 :=
  if h : i < arr.size then arr.set i v else arr

def arrSetNat (arr : Array Nat) (i : Nat) (v : Nat) : Array Nat :=
  if h : i < arr.size then arr.set i v else arr

def arrSetU8 (arr : Array UInt8) (i : Nat) (v : UInt8) : Array UInt8 :=
  if h : i < arr.size then arr.set i v else arr

def arrSet {α} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

end Doom.Render.Util
