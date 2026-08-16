import Doom.Render.Types
import Doom.Render.Util
import Doom.Wad

/-!
# Doom.Render.Things.Init

Sprite rotation tables (`r_things.c` `R_InitSprites` / `R_InitSpriteDefs` /
`R_InstallSpriteLump`).
-/

namespace Doom.Render.Things

open Doom.Render.Types
open Doom.Render.Util
open Doom.Wad

/-- `spriteframe_t`: `rotate=false` uses lump[0] for every angle. -/
structure SpriteFrame where
  rotate : Bool
  lump : Array Int32
  flip : Array UInt8
  deriving BEq, Repr

/-- `spritedef_t`. -/
structure SpriteDef where
  numframes : Nat
  spriteframes : Array SpriteFrame
  deriving BEq, Repr

/-- C `memset(sprtemp, -1)`: `rotate` is tri-state until a lump is installed. -/
private structure SpriteTempFrame where
  rotate : Option Bool
  lump : Array Int32
  flip : Array UInt8

private def maxSpriteFrames : Nat := 29

private def emptyTempFrame : SpriteTempFrame :=
  {
    rotate := none
    lump := Array.replicate 8 (-1 : Int32)
    flip := Array.replicate 8 (0 : UInt8)
  }

private def emptySprTemp : Array SpriteTempFrame :=
  Array.replicate maxSpriteFrames emptyTempFrame

def mkNegonearray : Array Int32 :=
  Array.replicate screenWidth (-1 : Int32)

/-- `screenheightarray[i] = viewheight` (`r_main.c` `R_ExecuteSetViewSize`). -/
def mkScreenheightarray (viewheight : Int32) : Array Int32 :=
  Array.replicate screenWidth viewheight

private def packName4Upper (name : ByteArray) : UInt32 :=
  let b (i : Nat) : UInt8 :=
    match name[i]? with
    | some x => toUpperAscii x
    | none => 0
  (b 0).toUInt32 ||| ((b 1).toUInt32 <<< 8) ||| ((b 2).toUInt32 <<< 16) |||
    ((b 3).toUInt32 <<< 24)

private def packUpperU32 (n : UInt32) : UInt32 :=
  let b0 := toUpperAscii n.toUInt8
  let b1 := toUpperAscii (n >>> 8).toUInt8
  let b2 := toUpperAscii (n >>> 16).toUInt8
  let b3 := toUpperAscii (n >>> 24).toUInt8
  b0.toUInt32 ||| (b1.toUInt32 <<< 8) ||| (b2.toUInt32 <<< 16) ||| (b3.toUInt32 <<< 24)

private def sprNameString (n : UInt32) : String :=
  String.ofList [
    Char.ofNat (n &&& (255 : UInt32)).toNat,
    Char.ofNat ((n >>> 8) &&& (255 : UInt32)).toNat,
    Char.ofNat ((n >>> 16) &&& (255 : UInt32)).toNat,
    Char.ofNat ((n >>> 24) &&& (255 : UInt32)).toNat
  ]

private def frameChar (frame : Int32) : Char :=
  Char.ofNat (65 + i32ToNat frame)

private def rotChar (rotation : Int32) : Char :=
  Char.ofNat (49 + i32ToNat rotation)

/-- `name[off] - 'A'`, `name[off+1] - '0'` as signed 32-bit (C then passes unsigned). -/
private def parseFrameRot (name : ByteArray) (off : Nat) : Int32 × Int32 :=
  let frame :=
    match name[off]? with
    | some b => b.toUInt32.toInt32 - (65 : Int32)
    | none => (0 : Int32) - (65 : Int32)
  let rotation :=
    match name[off + 1]? with
    | some b => b.toUInt32.toInt32 - (48 : Int32)
    | none => (0 : Int32) - (48 : Int32)
  (frame, rotation)

/-- `R_InstallSpriteLump`. `lump` is the WAD directory index (shareware: scan `l`). -/
private def installSpriteLump (firstSprite : Nat) (spritename : UInt32)
    (frames : Array SpriteTempFrame) (maxframe : Int32)
    (lump : Nat) (frame rotation : Int32) (flipped : Bool) :
    Except String (Array SpriteTempFrame × Int32) := do
  if frame.toUInt32 >= maxSpriteFrames.toUInt32 || rotation.toUInt32 > 8 then
    throw s!"R_InstallSpriteLump: Bad frame characters in lump {lump}"
  let fi := i32ToNat frame
  let maxframe := if frame > maxframe then frame else maxframe
  let tf := frames.getD fi emptyTempFrame
  let rel := Int32.ofNat lump - Int32.ofNat firstSprite
  let flipByte : UInt8 := if flipped then 1 else 0
  let nameStr := sprNameString spritename
  if rotation == 0 then
    match tf.rotate with
    | some false =>
      throw s!"R_InitSprites: Sprite {nameStr} frame {frameChar frame} has multip rot=0 lump"
    | some true =>
      throw s!"R_InitSprites: Sprite {nameStr} frame {frameChar frame} has rotations and a rot=0 lump"
    | none =>
      let tf' := {
        rotate := some false
        lump := Array.replicate 8 rel
        flip := Array.replicate 8 flipByte
      }
      pure (arrSet frames fi tf', maxframe)
  else
    match tf.rotate with
    | some false =>
      throw s!"R_InitSprites: Sprite {nameStr} frame {frameChar frame} has rotations and a rot=0 lump"
    | some true | none =>
      let rotIdx := i32ToNat (rotation - 1)
      if tf.lump.getD rotIdx (-1 : Int32) != -1 then
        throw s!"R_InitSprites: Sprite {nameStr} : {frameChar frame} : {rotChar (rotation - 1)} has two lumps mapped to it"
      let tf' := {
        rotate := some true
        lump := arrSetI32 tf.lump rotIdx rel
        flip := arrSetU8 tf.flip rotIdx flipByte
      }
      pure (arrSet frames fi tf', maxframe)

private def checkTempFrames (spritename : UInt32) (frames : Array SpriteTempFrame)
    (maxframe : Int32) : Except String Unit := do
  let nameStr := sprNameString spritename
  let mut frame : Nat := 0
  let n := i32ToNat (maxframe + 1)
  while frame < n do
    let tf := frames.getD frame emptyTempFrame
    let fc := Char.ofNat (65 + frame)
    match tf.rotate with
    | none =>
      throw s!"R_InitSprites: No patches found for {nameStr} frame {fc}"
    | some false =>
      pure ()
    | some true =>
      let mut r : Nat := 0
      while r < 8 do
        if tf.lump.getD r (-1 : Int32) == -1 then
          throw s!"R_InitSprites: Sprite {nameStr} frame {fc} is missing rotations"
        r := r + 1
    frame := frame + 1

private def copyTempFrames (frames : Array SpriteTempFrame) (numframes : Nat) :
    Array SpriteFrame :=
  Id.run do
    let mut out : Array SpriteFrame := Array.emptyWithCapacity numframes
    let mut i : Nat := 0
    while i < numframes do
      let tf := frames.getD i emptyTempFrame
      let rotate :=
        match tf.rotate with
        | some r => r
        | none => false
      out := out.push { rotate, lump := tf.lump, flip := tf.flip }
      i := i + 1
    pure out

private def namesMatch (lumpName : ByteArray) (sprname : UInt32) : Bool :=
  packName4Upper lumpName == packUpperU32 sprname

/-- `R_InitSpriteDefs`. Scans `[firstSprite, lastSprite]` inclusive. Empty scan
    when `lastSprite < firstSprite`. -/
def initSpriteDefs (wad : WadDirectory) (namelist : Array UInt32)
    (firstSprite lastSprite : Nat) : Except String (Array SpriteDef) := do
  if namelist.size == 0 then
    return (#[] : Array SpriteDef)
  let mut sprites : Array SpriteDef := Array.emptyWithCapacity namelist.size
  let mut i : Nat := 0
  while i < namelist.size do
    let spritename := namelist.getD i 0
    let mut frames := emptySprTemp
    let mut maxframe : Int32 := -1
    if firstSprite <= lastSprite then
      let mut l := firstSprite
      let mut scanning := true
      while scanning do
        if l < wad.entries.size then
          match wad.entries[l]? with
          | none => pure ()
          | some e =>
            if namesMatch e.name spritename then
              let (frame, rotation) := parseFrameRot e.name 4
              let inst ← installSpriteLump firstSprite spritename frames maxframe
                l frame rotation false
              frames := inst.fst
              maxframe := inst.snd
              match e.name[6]? with
              | some b =>
                if b != 0 then
                  let (frame2, rotation2) := parseFrameRot e.name 6
                  let inst2 ← installSpriteLump firstSprite spritename frames maxframe
                    l frame2 rotation2 true
                  frames := inst2.fst
                  maxframe := inst2.snd
              | none => pure ()
        if l >= lastSprite then
          scanning := false
        else
          l := l + 1
    if maxframe == -1 then
      sprites := sprites.push { numframes := 0, spriteframes := #[] }
    else
      checkTempFrames spritename frames maxframe
      let numframes := i32ToNat (maxframe + 1)
      sprites := sprites.push {
        numframes
        spriteframes := copyTempFrames frames numframes
      }
    i := i + 1
  pure sprites

/-- `R_InitSprites`: `negonearray[SCREENWIDTH] = -1`, then `R_InitSpriteDefs`. -/
def initSprites (wad : WadDirectory) (namelist : Array UInt32)
    (firstSprite lastSprite : Nat) : Except String (Array SpriteDef × Array Int32) := do
  let sprites ← initSpriteDefs wad namelist firstSprite lastSprite
  pure (sprites, mkNegonearray)

end Doom.Render.Things
