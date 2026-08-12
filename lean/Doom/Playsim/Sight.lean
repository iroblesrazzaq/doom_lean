import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Playsim.Mobj

/-!
# Doom.Playsim.Sight

`p_sight.c`: REJECT gate + BSP line-of-sight (`P_CheckSight` / `P_CrossBSPNode` /
`P_CrossSubsector` / `P_DivlineSide` / `P_InterceptVector2`).
-/

namespace Doom.Playsim.Sight

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Playsim.Mobj

/-- `divline_t`. -/
structure Divline where
  x : Int32
  y : Int32
  dx : Int32
  dy : Int32

/-- Mutable sight walk state (C globals + per-linedef validcount stamps). -/
structure SightWalk where
  sightzstart : Int32
  topslope : Int32
  bottomslope : Int32
  strace : Divline
  t2x : Int32
  t2y : Int32
  lineValidcount : Array Int32

/--
`P_DivlineSide`: side 0 (front), 1 (back), or 2 (on).
Ports the vanilla horizontal-on bug (`x == node->y`).
-/
def divlineSide (x y : Int32) (node : Divline) : Nat :=
  if node.dx == 0 then
    if x == node.x then 2
    else if x <= node.x then
      if node.dy > 0 then 1 else 0
    else
      if node.dy < 0 then 1 else 0
  else if node.dy == 0 then
    -- VANILLA BUG: compares `x` to `node->y`, not `y`.
    if x == node.y then 2
    else if y <= node.y then
      if node.dx < 0 then 1 else 0
    else
      if node.dx > 0 then 1 else 0
  else
    let dx := x - node.x
    let dy := y - node.y
    let left := (node.dy >>> 16) * (dx >>> 16)
    let right := (dy >>> 16) * (node.dx >>> 16)
    if right < left then 0
    else if left == right then 2
    else 1

/--
`P_InterceptVector2`: fractional intercept along `v2`, including `den == 0 → 0`
and FixedDiv overflow clamps.
-/
def interceptVector2 (v2 v1 : Divline) : Int32 :=
  let den := fixedMul (v1.dy >>> 8) v2.dx - fixedMul (v1.dx >>> 8) v2.dy
  if den == 0 then
    0
  else
    let num :=
      fixedMul ((v1.x - v2.x) >>> 8) v1.dy + fixedMul ((v2.y - v1.y) >>> 8) v1.dx
    fixedDiv num den

private def setLineVC (arr : Array Int32) (i : Nat) (v : Int32) : Array Int32 :=
  if h : i < arr.size then arr.set i v else arr

private def sectorHeights (gs : GameState) (secIdx : Int32) : Except String (Int32 × Int32) := do
  if secIdx < 0 then
    throw "P_CrossSubsector: null sector"
  match gs.sectors[secIdx.toNatClampNeg]? with
  | none => throw s!"P_CrossSubsector: bad sector {secIdx}"
  | some sec => pure (sec.floorheight, sec.ceilingheight)

/-- `P_CrossSubsector`. Returns `true` if strace crosses the subsector. -/
def crossSubsector (gs : GameState) (walk0 : SightWalk) (validcount : Int32)
    (num : Nat) : Except String (Bool × SightWalk) := do
  match gs.level.subsectors[num]? with
  | none => throw s!"P_CrossSubsector: bad ss {num}"
  | some sub =>
    let mut walk := walk0
    let mut count : Nat := sub.numsegs.toNat
    let mut segIdx : Nat := sub.firstseg.toNat
    let mut ok := true
    let mut guard : Nat := count + 1
    while ok && count > 0 && guard > 0 do
      guard := guard - 1
      count := count - 1
      match gs.level.segs[segIdx]? with
      | none => throw s!"P_CrossSubsector: bad seg {segIdx}"
      | some seg =>
        segIdx := segIdx + 1
        let lineIdx := seg.linedef.toNat
        let already ←
          match walk.lineValidcount[lineIdx]? with
          | some vc => pure (vc == validcount)
          | none => throw s!"P_CrossSubsector: lineValidcount OOB {lineIdx}"
        if already then
          pure ()
        else
          walk := { walk with lineValidcount := setLineVC walk.lineValidcount lineIdx validcount }
          match gs.level.lines[lineIdx]? with
          | none => throw s!"P_CrossSubsector: bad linedef {lineIdx}"
          | some line =>
            match gs.level.vertexes[line.v1.toNat]?, gs.level.vertexes[line.v2.toNat]? with
            | some v1, some v2 =>
              let s1 := divlineSide v1.x v1.y walk.strace
              let s2 := divlineSide v2.x v2.y walk.strace
              if s1 == s2 then
                pure ()
              else
                let divl : Divline := {
                  x := v1.x
                  y := v1.y
                  dx := v2.x - v1.x
                  dy := v2.y - v1.y
                }
                let s1' := divlineSide walk.strace.x walk.strace.y divl
                let s2' := divlineSide walk.t2x walk.t2y divl
                if s1' == s2' then
                  pure ()
                else if line.backsector < 0 then
                  ok := false
                else if (line.flags &&& ML_TWOSIDED) == 0 then
                  ok := false
                else
                  let (ffloor, fceil) ← sectorHeights gs seg.frontsector
                  let (bfloor, bceil) ← sectorHeights gs seg.backsector
                  if ffloor == bfloor && fceil == bceil then
                    pure ()
                  else
                    let opentop := if fceil < bceil then fceil else bceil
                    let openbottom := if ffloor > bfloor then ffloor else bfloor
                    if openbottom >= opentop then
                      ok := false
                    else
                      let frac := interceptVector2 walk.strace divl
                      let mut topslope := walk.topslope
                      let mut bottomslope := walk.bottomslope
                      if ffloor != bfloor then
                        let slope := fixedDiv (openbottom - walk.sightzstart) frac
                        if slope > bottomslope then
                          bottomslope := slope
                      if fceil != bceil then
                        let slope := fixedDiv (opentop - walk.sightzstart) frac
                        if slope < topslope then
                          topslope := slope
                      walk := { walk with topslope, bottomslope }
                      if topslope <= bottomslope then
                        ok := false
            | _, _ => throw s!"P_CrossSubsector: bad vertices on line {lineIdx}"
    pure (ok, walk)

/--
`P_CrossBSPNode` with explicit fuel (`numnodes + 1`). Child `0xffff` matches
vanilla signed-short `-1` → subsector 0.
-/
def crossBSPNode (gs : GameState) (walk0 : SightWalk) (validcount : Int32)
    (bspnum : UInt32) (fuel : Nat) : Except String (Bool × SightWalk) :=
  match fuel with
  | 0 => throw "P_CrossBSPNode: fuel exhausted"
  | fuel' + 1 =>
    if bspnum == (0xffff : UInt32) then
      crossSubsector gs walk0 validcount 0
    else if (bspnum &&& NF_SUBSECTOR) != 0 then
      crossSubsector gs walk0 validcount (bspnum &&& (~~~NF_SUBSECTOR)).toNat
    else
      match gs.level.nodes[bspnum.toNat]? with
      | none => throw s!"P_CrossBSPNode: bad node {bspnum}"
      | some bsp =>
        let nodeDL : Divline := { x := bsp.x, y := bsp.y, dx := bsp.dx, dy := bsp.dy }
        let side0 := divlineSide walk0.strace.x walk0.strace.y nodeDL
        let side := if side0 == 2 then 0 else side0
        let child := if side == 0 then bsp.child0 else bsp.child1
        match crossBSPNode gs walk0 validcount child fuel' with
        | Except.error e => Except.error e
        | Except.ok (false, walk) => Except.ok (false, walk)
        | Except.ok (true, walk) =>
          let sideEnd := divlineSide walk.t2x walk.t2y nodeDL
          if side == sideEnd then
            Except.ok (true, walk)
          else
            let child2 := if (side ^^^ 1) == 0 then bsp.child0 else bsp.child1
            crossBSPNode gs walk validcount child2 fuel'

/--
Full `P_CheckSight`: REJECT then BSP walk. Updates `validcount` /
`lineValidcount` on `GameState`.
-/
def checkSight (gs0 : GameState) (t1 t2 : Mobj) : Except String (GameState × Bool) := do
  let s1 ←
    match gs0.level.subsectors[t1.subsector.toNat]? with
    | some ss => pure ss.sector.toNat
    | none => throw "P_CheckSight: t1 bad subsector"
  let s2 ←
    match gs0.level.subsectors[t2.subsector.toNat]? with
    | some ss => pure ss.sector.toNat
    | none => throw "P_CheckSight: t2 bad subsector"
  let numsectors := gs0.level.sectors.size
  let pnum := s1 * numsectors + s2
  let bytenum := pnum / 8
  let bitIdx := pnum % 8
  let bitnum : UInt8 := (1 : UInt8) <<< bitIdx.toUInt8
  if bytenum >= gs0.level.reject.size then
    throw s!"P_CheckSight: REJECT OOB byte {bytenum}"
  let b ←
    if h : bytenum < gs0.level.reject.size then pure (gs0.level.reject.get bytenum h)
    else throw "P_CheckSight: REJECT OOB"
  if (b &&& bitnum) != 0 then
    pure (gs0, false)
  else
    let validcount := gs0.validcount + 1
    let sightzstart := t1.z + t1.height - (t1.height >>> 2)
    let topslope := (t2.z + t2.height) - sightzstart
    let bottomslope := t2.z - sightzstart
    let walk0 : SightWalk := {
      sightzstart
      topslope
      bottomslope
      strace := {
        x := t1.x
        y := t1.y
        dx := t2.x - t1.x
        dy := t2.y - t1.y
      }
      t2x := t2.x
      t2y := t2.y
      lineValidcount := gs0.lineValidcount
    }
    let root : UInt32 :=
      if gs0.level.nodes.size == 0 then (0xffff : UInt32)
      else (gs0.level.nodes.size - 1).toUInt32
    let fuel := gs0.level.nodes.size + 1
    let (ok, walk) ← crossBSPNode gs0 walk0 validcount root fuel
    pure ({ gs0 with validcount, lineValidcount := walk.lineValidcount }, ok)

end Doom.Playsim.Sight
