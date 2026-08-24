import Doom.Playsim.Fixed
import Doom.Playsim.Level

/-!
# Doom.Playsim.Bsp

`R_PointOnSide` / `R_PointInSubsector` over `LevelData.nodes`.
-/


namespace Doom.Playsim.Bsp

open Doom.Playsim.Fixed
open Doom.Playsim.Level

/-- `R_PointOnSide`: 0 = front, 1 = back. -/
def pointOnSide (x y : Int32) (node : Node) : Nat :=
  if node.dx == 0 then
    if x <= node.x then
      if node.dy > 0 then 1 else 0
    else
      if node.dy < 0 then 1 else 0
  else if node.dy == 0 then
    if y <= node.y then
      if node.dx < 0 then 1 else 0
    else
      if node.dx > 0 then 1 else 0
  else
    let dx := x - node.x
    let dy := y - node.y
    let mixed :=
      (node.dy.toUInt32 ^^^ node.dx.toUInt32 ^^^ dx.toUInt32 ^^^ dy.toUInt32) &&& (0x80000000 : UInt32)
    if mixed != 0 then
      if ((node.dy.toUInt32 ^^^ dx.toUInt32) &&& (0x80000000 : UInt32)) != 0 then
        1
      else
        0
    else
      let left := fixedMul (node.dy >>> 16) dx
      let right := fixedMul dy (node.dx >>> 16)
      if right < left then 0 else 1

/--
`R_PointInSubsector`: walk BSP from root (`nodes.size - 1`) until a subsector
child (`NF_SUBSECTOR` bit). Returns subsector index.
-/
def pointInSubsector (level : LevelData) (x y : Int32) : Except String UInt32 :=
  if level.nodes.size == 0 then
    if level.subsectors.size == 0 then
      Except.error "pointInSubsector: no subsectors"
    else
      Except.ok 0
  else
    Id.run do
      let mut nodenum : UInt32 := (level.nodes.size - 1).toUInt32
      let mut guard : Nat := level.nodes.size + 2
      let mut result : Except String UInt32 :=
        Except.error "pointInSubsector: BSP walk overflow"
      while guard > 0 do
        guard := guard - 1
        if (nodenum &&& NF_SUBSECTOR) != 0 then
          result := Except.ok (nodenum &&& (~~~NF_SUBSECTOR))
          guard := 0
        else
          match level.nodes[nodenum.toNat]? with
          | none =>
            result := Except.error s!"pointInSubsector: bad node {nodenum}"
            guard := 0
          | some node =>
            let side := pointOnSide x y node
            nodenum := if side == 0 then node.child0 else node.child1
      pure result

end Doom.Playsim.Bsp
