import Doom.Playsim.Info
import Doom.Render.Data
import Doom.Render.Gfx.Sprite
import Doom.Render.Things.Init
import Doom.Wad

/-!
# InitSpritesTest

R1t-initsprites: `R_InitSprites` / `R_InitSpriteDefs` against doom1.wad goldens
and synthetic WAD loud-error cases.
-/

open Doom.Playsim.Info
open Doom.Render.Data
open Doom.Render.Things
open Doom.Wad

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" =>
    match cwd.parent with
    | some p => pure p
    | none => pure cwd
  | _ => pure cwd

private def loadIwad : IO WadDirectory := do
  let root ← defaultRoot
  loadFile (root / "fixtures" / "wads" / "doom1.wad")

private def namedLumps (names : Array String) : WadDirectory :=
  {
    identification := ByteArray.mk #[73, 87, 65, 68]
    numlumps := names.size
    infotableofs := 0
    entries := names.map (fun n => { filepos := 0, size := 0, name := nameTo8 n })
    data := ByteArray.empty
  }

private def pack4 (a b c d : UInt8) : UInt32 := packSprName a b c d

private def TEST : UInt32 := pack4 84 69 83 84  -- 'TEST'
private def COLU : UInt32 := pack4 67 79 76 85
private def PUNG : UInt32 := pack4 80 85 78 71
private def TROO : UInt32 := pack4 84 82 79 79
private def VILE : UInt32 := pack4 86 73 76 69
private def CYBR : UInt32 := pack4 67 89 66 82
private def TLP2 : UInt32 := pack4 84 76 80 50

private def findSpr (sprites : Array SpriteDef) (namelist : Array UInt32)
    (name : UInt32) : Option SpriteDef :=
  Id.run do
    let mut found : Option SpriteDef := none
    let mut i : Nat := 0
    while i < namelist.size do
      if namelist.getD i 0 == name then
        found := sprites[i]?
        i := namelist.size
      else
        i := i + 1
    pure found

private def allEqI32 (a : Array Int32) (v : Int32) : Bool :=
  Id.run do
    let mut ok := a.size == 8
    let mut i : Nat := 0
    while i < a.size do
      if a.getD i (v + 1) != v then ok := false
      i := i + 1
    pure ok

private def allEqU8 (a : Array UInt8) (v : UInt8) : Bool :=
  Id.run do
    let mut ok := a.size == 8
    let mut i : Nat := 0
    while i < a.size do
      if a.getD i (v + 1) != v then ok := false
      i := i + 1
    pure ok

private def allRotateFalse (frames : Array SpriteFrame) : Bool :=
  Id.run do
    let mut ok := true
    let mut i : Nat := 0
    while i < frames.size do
      match frames[i]? with
      | some f => if f.rotate then ok := false
      | none => ok := false
      i := i + 1
    pure ok

private def expectError (name : String) (got : Except String (Array SpriteDef))
    (want : String) : IO Bool :=
  match got with
  | Except.error e => assert name (e == want)
  | Except.ok _ => assert name false

def main (_args : List String) : IO UInt32 := do
  let mut ok := true

  -- Public surface: initData on doom1.wad
  let wad ← loadIwad
  let data ← match initData wad with
    | Except.error e =>
      IO.eprintln s!"initData failed: {e}"
      return 1
    | Except.ok d => pure d

  ok := (← assert "firstSprite=553" (data.firstSprite == 553)) && ok
  ok := (← assert "sprites.size=138" (data.sprites.size == 138)) && ok
  ok := (← assert "negonearray size=320" (data.negonearray.size == 320)) && ok
  ok := (← assert "negonearray all -1"
    (data.negonearray == Array.replicate 320 (-1 : Int32))) && ok

  let mut present : Nat := 0
  let mut empty : Nat := 0
  let mut si : Nat := 0
  while si < data.sprites.size do
    match data.sprites[si]? with
    | some d =>
      if d.numframes == 0 then empty := empty + 1 else present := present + 1
    | none => pure ()
    si := si + 1
  ok := (← assert "61 present" (present == 61)) && ok
  ok := (← assert "77 empty" (empty == 77)) && ok

  match findSpr data.sprites sprnames COLU with
  | none => ok := (← assert "COLU present" false) && ok
  | some colu =>
    ok := (← assert "COLU numframes=1" (colu.numframes == 1 && colu.spriteframes.size == 1)) && ok
    match colu.spriteframes[0]? with
    | none => ok := (← assert "COLU frame A" false) && ok
    | some fr =>
      ok := (← assert "COLU rotate=false" (fr.rotate == false)) && ok
      ok := (← assert "COLU lump=439" (allEqI32 fr.lump (439 : Int32))) && ok
      ok := (← assert "COLU flip=0" (allEqU8 fr.flip 0)) && ok

  match findSpr data.sprites sprnames PUNG with
  | none => ok := (← assert "PUNG present" false) && ok
  | some pung =>
    ok := (← assert "PUNG numframes=4" (pung.numframes == 4 && pung.spriteframes.size == 4)) && ok
    ok := (← assert "PUNG all rotate=false" (allRotateFalse pung.spriteframes)) && ok

  match findSpr data.sprites sprnames TROO with
  | none => ok := (← assert "TROO present" false) && ok
  | some troo =>
    match troo.spriteframes[0]? with
    | none => ok := (← assert "TROO frame A" false) && ok
    | some fr =>
      ok := (← assert "TROO A rotate=true" (fr.rotate == true)) && ok
      ok := (← assert "TROO A lumps"
        (fr.lump == #[(149 : Int32), 150, 151, 152, 153, 152, 151, 150])) && ok
      ok := (← assert "TROO A flip"
        (fr.flip == #[(0 : UInt8), 0, 0, 0, 0, 1, 1, 1])) && ok

  match findSpr data.sprites sprnames VILE, findSpr data.sprites sprnames CYBR,
      findSpr data.sprites sprnames TLP2 with
  | some v, some c, some t =>
    ok := (← assert "VILE/CYBR/TLP2 empty"
      (v.numframes == 0 && c.numframes == 0 && t.numframes == 0)) && ok
  | _, _, _ =>
    ok := (← assert "VILE/CYBR/TLP2 empty" false) && ok

  -- Empty namelist
  match initSpriteDefs (namedLumps #[]) #[] 0 0 with
  | Except.error e =>
    IO.eprintln e
    ok := (← assert "empty namelist" false) && ok
  | Except.ok s =>
    ok := (← assert "empty namelist" (s == #[])) && ok

  -- Shareware hole: namelist entry with no lumps → numframes=0, no error
  let holeWad := namedLumps #["OTHERA0"]
  match initSpriteDefs holeWad #[TEST] 0 0 with
  | Except.error e =>
    IO.eprintln e
    ok := (← assert "missing sprite numframes=0" false) && ok
  | Except.ok s =>
    ok := (← assert "missing sprite numframes=0"
      (s.size == 1 && (s.getD 0 { numframes := 1, spriteframes := #[] }).numframes == 0)) && ok

  -- Inclusive lastSprite: matching lump at last index is installed
  match initSpriteDefs (namedLumps #["DUMMY", "TESTA0"]) #[TEST] 1 1 with
  | Except.error e =>
    IO.eprintln e
    ok := (← assert "inclusive lastSprite" false) && ok
  | Except.ok s =>
    match s[0]? with
    | none => ok := (← assert "inclusive lastSprite" false) && ok
    | some defn =>
      match defn.spriteframes[0]? with
      | none => ok := (← assert "inclusive lastSprite" false) && ok
      | some fr =>
        ok := (← assert "inclusive lastSprite"
          (defn.numframes == 1 && fr.rotate == false && allEqI32 fr.lump 0)) && ok

  -- Flip pair colliding with an existing rotation uses the same duplicate error
  ok := (← expectError "flip pair duplicate rotation"
    (initSpriteDefs (namedLumps #[
      "TESTA1", "TESTA2A8", "TESTA3", "TESTA4", "TESTA5", "TESTA6", "TESTA7", "TESTA8"])
      #[TEST] 0 7)
    "R_InitSprites: Sprite TEST : A : 8 has two lumps mapped to it") && ok
  let rangeWad := namedLumps #["TESTA0", "DUMMY", "TESTB0"]
  match initSpriteDefs rangeWad #[TEST] 1 1 with
  | Except.error e =>
    IO.eprintln e
    ok := (← assert "scan range skips outside" false) && ok
  | Except.ok s =>
    ok := (← assert "scan range skips outside"
      (s.size == 1 && (s.getD 0 { numframes := 1, spriteframes := #[] }).numframes == 0)) && ok

  -- Flip pair uses scan index l (not last-name-wins remap). Extra TESTA2A8
  -- at index 7 is outside [first,last]; name lookup would pick 7.
  let flipWad := namedLumps #[
    "TESTA1", "TESTA2A8", "TESTA3", "TESTA4", "TESTA5", "TESTA6", "TESTA7", "TESTA2A8"]
  match initSpriteDefs flipWad #[TEST] 0 6 with
  | Except.error e =>
    IO.eprintln e
    ok := (← assert "flip pair uses l" false) && ok
  | Except.ok s =>
    match s[0]? with
    | none => ok := (← assert "flip pair uses l" false) && ok
    | some defn =>
      match defn.spriteframes[0]? with
      | none => ok := (← assert "flip pair uses l" false) && ok
      | some fr =>
        ok := (← assert "flip pair uses l"
          (fr.rotate == true &&
            fr.lump.getD 1 (-99 : Int32) == 1 &&
            fr.lump.getD 7 (-99 : Int32) == 1 &&
            fr.flip.getD 1 9 == 0 &&
            fr.flip.getD 7 9 == 1)) && ok

  -- Rotation 0: all 8 angles, rotate=false
  match initSpriteDefs (namedLumps #["TESTA0"]) #[TEST] 0 0 with
  | Except.error e =>
    IO.eprintln e
    ok := (← assert "rot0 all eight" false) && ok
  | Except.ok s =>
    match s[0]? with
    | none => ok := (← assert "rot0 all eight" false) && ok
    | some defn =>
      match defn.spriteframes[0]? with
      | none => ok := (← assert "rot0 all eight" false) && ok
      | some fr =>
        ok := (← assert "rot0 all eight"
          (fr.rotate == false && allEqI32 fr.lump 0 && allEqU8 fr.flip 0)) && ok

  -- Case-insensitive 4-char name match (C strncasecmp); frame letter stays 'A'+n
  match initSpriteDefs (namedLumps #["testA0"]) #[TEST] 0 0 with
  | Except.error e =>
    IO.eprintln e
    ok := (← assert "lowercase lump name" false) && ok
  | Except.ok s =>
    ok := (← assert "lowercase lump name"
      (s.size == 1 && (s.getD 0 { numframes := 0, spriteframes := #[] }).numframes == 1)) && ok

  -- Loud errors
  ok := (← expectError "frame>=29"
    (initSpriteDefs (namedLumps #["TEST^0"]) #[TEST] 0 0)
    "R_InstallSpriteLump: Bad frame characters in lump 0") && ok

  ok := (← expectError "rotation>8"
    (initSpriteDefs (namedLumps #["TESTA9"]) #[TEST] 0 0)
    "R_InstallSpriteLump: Bad frame characters in lump 0") && ok

  ok := (← expectError "mix rot0 and rotations"
    (initSpriteDefs (namedLumps #["TESTA0", "TESTA1"]) #[TEST] 0 1)
    "R_InitSprites: Sprite TEST frame A has rotations and a rot=0 lump") && ok

  ok := (← expectError "mix rotations then rot0"
    (initSpriteDefs (namedLumps #["TESTA1", "TESTA0"]) #[TEST] 0 1)
    "R_InitSprites: Sprite TEST frame A has rotations and a rot=0 lump") && ok

  ok := (← expectError "duplicate rot0"
    (initSpriteDefs (namedLumps #["TESTA0", "TESTA0"]) #[TEST] 0 1)
    "R_InitSprites: Sprite TEST frame A has multip rot=0 lump") && ok

  ok := (← expectError "two lumps same rotation"
    (initSpriteDefs (namedLumps #["TESTA1", "TESTA1"]) #[TEST] 0 1)
    "R_InitSprites: Sprite TEST : A : 1 has two lumps mapped to it") && ok

  ok := (← expectError "used frame with no patches"
    (initSpriteDefs (namedLumps #["TESTA0", "TESTC0"]) #[TEST] 0 1)
    "R_InitSprites: No patches found for TEST frame B") && ok

  ok := (← expectError "missing rotations"
    (initSpriteDefs (namedLumps #["TESTA1", "TESTA2", "TESTA3", "TESTA4", "TESTA5", "TESTA6", "TESTA7"])
      #[TEST] 0 6)
    "R_InitSprites: Sprite TEST frame A is missing rotations") && ok

  -- Gfx.Sprite.initSprites still used for metrics (not renamed)
  match Doom.Render.Gfx.Sprite.initSprites wad with
  | Except.error e =>
    IO.eprintln e
    ok := (← assert "Gfx.Sprite.initSprites first=553" false) && ok
  | Except.ok (first, _) =>
    ok := (← assert "Gfx.Sprite.initSprites first=553" (first == 553)) && ok

  if ok then
    IO.println "init-sprites-test: ALL PASS"
    pure 0
  else
    IO.eprintln "init-sprites-test: FAILURES"
    pure 1
