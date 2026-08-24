import Doom.Render.Constants

/-!
# Doom.Render.Clip

Solid/pass wall clip ranges (`r_bsp.c` `R_ClearClipSegs`, `R_ClearDrawSegs`,
`R_ClipSolidWallSegment`, `R_ClipPassWallSegment`) and record-only wall ranges
(`r_segs.c` `R_StoreWallRange` shape).
-/

namespace Doom.Render.Clip

open Doom.Render.Constants

structure ClipRange where
  first : Int32
  last : Int32
  deriving Repr, Inhabited, DecidableEq, BEq

def defaultViewwidth : Int32 := 320

def clipSentinelFirst : Int32 := -2147483647
def clipSentinelLast : Int32 := 2147483647

structure ClipState where
  solidsegs : Array ClipRange
  newend : Nat
  viewwidth : Int32 := defaultViewwidth
  drawSegIdx : Nat := 0
  recordedWalls : Array (Int32 × Int32) := #[]
  deriving Repr

private def initSolidSegs : Array ClipRange :=
  Array.replicate maxSegs (⟨0, 0⟩ : ClipRange)

private def getSeg (segs : Array ClipRange) (i : Nat) : ClipRange :=
  match segs[i]? with
  | some r => r
  | none => ⟨0, 0⟩

private def setSeg (segs : Array ClipRange) (i : Nat) (r : ClipRange) : Array ClipRange :=
  if h : i < segs.size then segs.set i r else segs

private def setSegAt (st : ClipState) (i : Nat) (r : ClipRange) : ClipState :=
  { st with solidsegs := setSeg st.solidsegs i r }

private def findTouchSeg (st : ClipState) (first : Int32) : Nat :=
  let rec loop (idx : Nat) : Nat :=
    if idx >= st.newend then idx
    else if (getSeg st.solidsegs idx).last < first - 1 then loop (idx + 1) else idx
  loop 0

/-- When `some`, invoked instead of `storeWallRange`; threads auxiliary draw state. -/
abbrev StoreWallHook (σ : Type) := σ → Int32 → Int32 → Except String σ

/-- `R_StoreWallRange` — record-only append; silent no-op at `maxDrawSegs`. -/
def storeWallRange (st : ClipState) (start stop : Int32) : ClipState :=
  if start > stop then
    st
  else if st.drawSegIdx >= maxDrawSegs then
    st
  else
    { st with
      drawSegIdx := st.drawSegIdx + 1
      recordedWalls := st.recordedWalls.push (start, stop) }

private def applyStore {σ} (st : ClipState) (start stop : Int32) (aux : σ)
    (hook : Option (StoreWallHook σ)) : Except String (ClipState × σ) :=
  match hook with
  | none => pure (storeWallRange st start stop, aux)
  | some f => do
    let aux' ← f aux start stop
    pure (st, aux')

private def insertSolidSeg (st : ClipState) (startIdx : Nat) (first last : Int32) :
    Except String ClipState := do
  if st.newend >= maxSegs then
    throw "solidsegs overflow"
  let rec shift (segs : Array ClipRange) (i : Nat) : Array ClipRange :=
    if i ≤ startIdx then segs
    else shift (setSeg segs i (getSeg segs (i - 1))) (i - 1)
  let segs := setSeg (shift st.solidsegs st.newend) startIdx ⟨first, last⟩
  pure { st with solidsegs := segs, newend := st.newend + 1 }

private def crunch (st : ClipState) (startIdx nextIdx : Nat) : ClipState :=
  if nextIdx == startIdx then
    st
  else
    let numCopy := st.newend - (nextIdx + 1)
    let rec copyLoop (segs : Array ClipRange) (i : Nat) : Array ClipRange :=
      if i ≥ numCopy then segs
      else copyLoop (setSeg segs (startIdx + 1 + i) (getSeg segs (nextIdx + 1 + i))) (i + 1)
    { st with solidsegs := copyLoop st.solidsegs 0, newend := startIdx + st.newend - nextIdx }

/-- `R_ClearClipSegs` -/
def clearClipSegs (st : ClipState) : ClipState :=
  let segs :=
    initSolidSegs
      |> fun a => setSeg a 0 ⟨clipSentinelFirst, (-1 : Int32)⟩
      |> fun a => setSeg a 1 ⟨st.viewwidth, clipSentinelLast⟩
  { st with solidsegs := segs, newend := 2 }

/-- `R_ClearDrawSegs` -/
def clearDrawSegs (st : ClipState) : ClipState :=
  { st with drawSegIdx := 0 }

/-- `R_ClipSolidWallSegment` — optional `hook` draws instead of record-only `storeWallRange`. -/
def clipSolidWallSegmentWith {σ} (st : ClipState) (first last : Int32) (aux : σ)
    (hook : Option (StoreWallHook σ)) : Except String (ClipState × σ) := do
  let mut state := st
  let mut drawAux := aux
  let mut startIdx := findTouchSeg state first
  let mut startSeg := getSeg state.solidsegs startIdx

  if first < startSeg.first then
    if last < startSeg.first - 1 then
      let (state', drawAux') ← applyStore state first last drawAux hook
      state := state'
      drawAux := drawAux'
      return (← insertSolidSeg state startIdx first last, drawAux)
    else
      let (state', drawAux') ← applyStore state first (startSeg.first - 1) drawAux hook
      state := state'
      drawAux := drawAux'
      state := setSegAt state startIdx ⟨first, startSeg.last⟩
      startSeg := getSeg state.solidsegs startIdx

  if last <= startSeg.last then
    return (state, drawAux)

  let mut nextIdx := startIdx
  while nextIdx + 1 < state.newend &&
      last >= (getSeg state.solidsegs (nextIdx + 1)).first - 1 do
    let gapFirst := (getSeg state.solidsegs nextIdx).last + 1
    let gapLast := (getSeg state.solidsegs (nextIdx + 1)).first - 1
    let (state', drawAux') ← applyStore state gapFirst gapLast drawAux hook
    state := state'
    drawAux := drawAux'
    nextIdx := nextIdx + 1
    let nextLast := (getSeg state.solidsegs nextIdx).last
    if last <= nextLast then
      state := setSegAt state startIdx ⟨startSeg.first, nextLast⟩
      return (crunch state startIdx nextIdx, drawAux)

  let (state', drawAux') ←
    applyStore state ((getSeg state.solidsegs nextIdx).last + 1) last drawAux hook
  state := state'
  drawAux := drawAux'
  state := setSegAt state startIdx ⟨startSeg.first, last⟩
  pure (crunch state startIdx nextIdx, drawAux)

/-- Record-only `R_ClipSolidWallSegment` (default tests). -/
def clipSolidWallSegment (st : ClipState) (first last : Int32) : Except String ClipState := do
  let (st', _) ← clipSolidWallSegmentWith st first last () none
  pure st'

/-- `R_ClipPassWallSegment` — optional `hook` draws instead of record-only `storeWallRange`. -/
def clipPassWallSegmentWith {σ} (st : ClipState) (first last : Int32) (aux : σ)
    (hook : Option (StoreWallHook σ)) : Except String (ClipState × σ) := do
  let mut state := st
  let mut drawAux := aux
  let mut startIdx := findTouchSeg state first
  let startSeg := getSeg state.solidsegs startIdx

  if first < startSeg.first then
    if last < startSeg.first - 1 then
      let (state', drawAux') ← applyStore state first last drawAux hook
      return (state', drawAux')
    else
      let (state', drawAux') ← applyStore state first (startSeg.first - 1) drawAux hook
      state := state'
      drawAux := drawAux'

  let startSeg' := getSeg state.solidsegs startIdx
  if last <= startSeg'.last then
    return (state, drawAux)

  let mut idx := startIdx
  while idx + 1 < state.newend &&
      last >= (getSeg state.solidsegs (idx + 1)).first - 1 do
    let gapFirst := (getSeg state.solidsegs idx).last + 1
    let gapLast := (getSeg state.solidsegs (idx + 1)).first - 1
    let (state', drawAux') ← applyStore state gapFirst gapLast drawAux hook
    state := state'
    drawAux := drawAux'
    idx := idx + 1
    if last <= (getSeg state.solidsegs idx).last then
      return (state, drawAux)

  let (state', drawAux') ←
    applyStore state ((getSeg state.solidsegs idx).last + 1) last drawAux hook
  pure (state', drawAux')

/-- Record-only `R_ClipPassWallSegment` (default tests). -/
def clipPassWallSegment (st : ClipState) (first last : Int32) : Except String ClipState := do
  let (st', _) ← clipPassWallSegmentWith st first last () none
  pure st'

def emptyClipState : ClipState :=
  { solidsegs := initSolidSegs, newend := 0 }

end Doom.Render.Clip
