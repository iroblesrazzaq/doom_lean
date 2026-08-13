import Doom.Playsim.Angle
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Level
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj
import Doom.Playsim.Random
import Doom.Playsim.Spawn
import Doom.Playsim.Tables
import Doom.Playsim.Weapons

/-!
# Doom.Playsim.Hitscan

`P_AimLineAttack` / `P_LineAttack` / puff+blood (`p_map.c` / `p_mobj.c`).
Does not import Enemy or Combat; `P_LineAttack` takes a `P_DamageMobj` callback.
-/

namespace Doom.Playsim.Hitscan

open Doom.Playsim.Angle
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Level
open Doom.Playsim.MapUtil
open Doom.Playsim.Mobj
open Doom.Playsim.Random
open Doom.Playsim.Spawn
open Doom.Playsim.Tables
open Doom.Playsim.Weapons

/-- `p_local.h` `MISSILERANGE`. -/
def MISSILERANGE : Int32 := 32 * 64 * FRACUNIT
/-- `p_local.h` `MELEERANGE`. -/
def MELEERANGE : Int32 := 64 * FRACUNIT
/-- Vanilla `SCREENHEIGHT` / `SCREENWIDTH` (`i_video.h`). -/
def SCREENHEIGHT : Int32 := 200
def SCREENWIDTH : Int32 := 320

def S_PUFF1 : UInt32 := 93
def S_PUFF3 : UInt32 := 95
def MT_PUFF : Int32 := 37

/-- `P_DamageMobj` callback so Hitscan does not import Enemy. -/
abbrev DamageMobjFn :=
  GameState → Nat → Option Nat → Option Nat → Int32 → Except String GameState

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

private def setMo (gs : GameState) (i : Nat) (mo : Mobj) : GameState :=
  { gs with mobjs := setArr gs.mobjs i mo }

/-- Overlay a null-action state (blood/puff overlays); action states loud-error. -/
private def applyNullActionState (gs : GameState) (idx : Nat) (stnum : UInt32) :
    Except String GameState := do
  match states[stnum.toNat]? with
  | none => throw s!"Hitscan applyState: bad state {stnum}"
  | some st =>
    if st.action != actionNull then
      throw s!"Hitscan applyState: unexpected action {st.action} in state {stnum}"
    if st.tics == 0 then
      throw s!"Hitscan applyState: tics=0 recurse not implemented in state {stnum}"
    match gs.mobjs[idx]? with
    | none => throw "Hitscan applyState: bad mobj"
    | some mo =>
      pure (setMo gs idx {
        mo with
        state := stnum
        tics := st.tics
        sprite := st.sprite
        frame := st.frame
      })

/-- `P_ShootSpecialLine` — no-op unless special 24/46/47 (then loud-error). -/
def shootSpecialLine (gs : GameState) (thingIdx : Nat) (ld : Line) :
    Except String Unit := do
  match gs.mobjs[thingIdx]? with
  | none => throw "P_ShootSpecialLine: bad thing"
  | some thing =>
    let mut ok := true
    if thing.player < 0 then
      ok := ld.special == 46
    if !ok then
      return ()
    if ld.special == 24 || ld.special == 46 || ld.special == 47 then
      throw s!"P_ShootSpecialLine: special {ld.special} not implemented"
    else
      pure ()

/-- Hitscan scratch (`shootthing` / `linetarget` / `trace` / slopes). -/
structure AttackState where
  shootthingIdx : Nat
  shootz : Int32
  attackrange : Int32
  aimslope : Int32
  laDamage : Int32
  /-- Aimed mobj index, or `-1`. -/
  linetarget : Int32
  topslope : Int32
  bottomslope : Int32
  trace : Divline

private def mkTrace (bmap : BlockMap) (x1_0 y1_0 x2 y2 : Int32) : Divline :=
  let x1 :=
    if ((x1_0 - bmap.originX) &&& MAPBMASK) == 0 then x1_0 + FRACUNIT else x1_0
  let y1 :=
    if ((y1_0 - bmap.originY) &&& MAPBMASK) == 0 then y1_0 + FRACUNIT else y1_0
  { x := x1, y := y1, dx := x2 - x1, dy := y2 - y1 }

private def aimSlopes : Int32 :=
  (SCREENHEIGHT / 2) * FRACUNIT / (SCREENWIDTH / 2)

private def byteAt (pic : ByteArray) (i : Nat) : UInt8 :=
  match pic[i]? with | some b => b | none => 0

private def isSkyPic (pic : ByteArray) : Bool :=
  pic.size >= 6 && byteAt pic 0 == 70 && byteAt pic 1 == 95 &&
    byteAt pic 2 == 83 && byteAt pic 3 == 75 && byteAt pic 4 == 89 &&
    byteAt pic 5 == 49

/-- `P_SpawnPuff`. -/
def spawnPuff (gs0 : GameState) (attackrange x y z0 : Int32) :
    Except String GameState := do
  let (d, rng) := pSubRandom gs0.rng
  let z := z0 + (d <<< 10)
  let (gs1, idx) ← Spawn.spawnMobj { gs0 with rng } x y z MT_PUFF
  match gs1.mobjs[idx]? with
  | none => throw "P_SpawnPuff: lost mobj"
  | some th0 =>
    let gs2 := setMo gs1 idx { th0 with momz := FRACUNIT }
    match gs2.mobjs[idx]? with
    | none => throw "P_SpawnPuff: lost after momz"
    | some th1 =>
      let (r, rng) := pRandom gs2.rng
      let mut tics := th1.tics - (r &&& 3)
      if tics < 1 then tics := 1
      let gs3 := { setMo gs2 idx { th1 with tics } with rng }
      if attackrange == MELEERANGE then
        applyNullActionState gs3 idx S_PUFF3
      else
        pure gs3

/-- `P_SpawnBlood`. -/
def spawnBlood (gs0 : GameState) (x y z0 : Int32) (damage : Int32) :
    Except String GameState := do
  let (d, rng) := pSubRandom gs0.rng
  let z := z0 + (d <<< 10)
  let (gs1, idx) ← Spawn.spawnMobj { gs0 with rng } x y z MT_BLOOD
  match gs1.mobjs[idx]? with
  | none => throw "P_SpawnBlood: lost mobj"
  | some th0 =>
    let gs2 := setMo gs1 idx { th0 with momz := FRACUNIT * 2 }
    match gs2.mobjs[idx]? with
    | none => throw "P_SpawnBlood: lost after momz"
    | some th1 =>
      let (r, rng) := pRandom gs2.rng
      let mut tics := th1.tics - (r &&& 3)
      if tics < 1 then tics := 1
      let gs3 := { setMo gs2 idx { th1 with tics } with rng }
      if damage <= 12 && damage >= 9 then
        applyNullActionState gs3 idx S_BLOOD2
      else if damage < 9 then
        applyNullActionState gs3 idx S_BLOOD3
      else
        pure gs3

private def shootHitLine (gs0 : GameState) (st0 : AttackState) (inn : Intercept)
    (li : Line) : Except String (GameState × AttackState × Bool) := do
  let frac := inn.frac - fixedDiv (4 * FRACUNIT) st0.attackrange
  let x := st0.trace.x + fixedMul st0.trace.dx frac
  let y := st0.trace.y + fixedMul st0.trace.dy frac
  let z := st0.shootz + fixedMul st0.aimslope (fixedMul frac st0.attackrange)
  match gs0.level.sectors[li.frontsector.toNatClampNeg]? with
  | none => throw "PTR_ShootTraverse: bad frontsector"
  | some front =>
    if isSkyPic front.ceilingpic then
      let ceilH :=
        match gs0.sectors[li.frontsector.toNatClampNeg]? with
        | some s => s.ceilingheight
        | none => front.ceilingheight
      if z > ceilH then
        return (gs0, st0, false)
      if li.backsector >= 0 then
        match gs0.level.sectors[li.backsector.toNatClampNeg]? with
        | some back =>
          if isSkyPic back.ceilingpic then
            return (gs0, st0, false)
        | none => pure ()
    let gs ← spawnPuff gs0 st0.attackrange x y z
    pure (gs, st0, false)

/-- `PTR_AimTraverse`. -/
def ptrAimTraverse (gs : GameState) (st0 : AttackState) (inn : Intercept) :
    Except String (GameState × AttackState × Bool) := do
  if inn.isaline then
    match gs.level.lines[inn.lineIdx]? with
    | none => throw "PTR_AimTraverse: bad line"
    | some li =>
      if (li.flags &&& ML_TWOSIDED) == 0 then
        return (gs, st0, false)
      let op ← lineOpening gs li
      if op.openbottom >= op.opentop then
        return (gs, st0, false)
      let dist := fixedMul st0.attackrange inn.frac
      let mut st := st0
      let frontFloor :=
        match gs.sectors[li.frontsector.toNatClampNeg]? with
        | some s => s.floorheight
        | none => (0 : Int32)
      let frontCeil :=
        match gs.sectors[li.frontsector.toNatClampNeg]? with
        | some s => s.ceilingheight
        | none => (0 : Int32)
      let backFloor :=
        match gs.sectors[li.backsector.toNatClampNeg]? with
        | some s => s.floorheight
        | none => (0 : Int32)
      let backCeil :=
        match gs.sectors[li.backsector.toNatClampNeg]? with
        | some s => s.ceilingheight
        | none => (0 : Int32)
      if li.backsector < 0 || frontFloor != backFloor then
        let slope := fixedDiv (op.openbottom - st.shootz) dist
        if slope > st.bottomslope then
          st := { st with bottomslope := slope }
      if li.backsector < 0 || frontCeil != backCeil then
        let slope := fixedDiv (op.opentop - st.shootz) dist
        if slope < st.topslope then
          st := { st with topslope := slope }
      if st.topslope <= st.bottomslope then
        pure (gs, st, false)
      else
        pure (gs, st, true)
  else
    match gs.mobjs[inn.thingIdx]? with
    | none => throw "PTR_AimTraverse: bad thing"
    | some th =>
      if inn.thingIdx == st0.shootthingIdx then
        return (gs, st0, true)
      if (th.flags &&& MF_SHOOTABLE) == 0 then
        return (gs, st0, true)
      let dist := fixedMul st0.attackrange inn.frac
      let mut thingtopslope := fixedDiv (th.z + th.height - st0.shootz) dist
      if thingtopslope < st0.bottomslope then
        return (gs, st0, true)
      let mut thingbottomslope := fixedDiv (th.z - st0.shootz) dist
      if thingbottomslope > st0.topslope then
        return (gs, st0, true)
      if thingtopslope > st0.topslope then
        thingtopslope := st0.topslope
      if thingbottomslope < st0.bottomslope then
        thingbottomslope := st0.bottomslope
      let st := {
        st0 with
        aimslope := (thingtopslope + thingbottomslope) / 2
        linetarget := inn.thingIdx.toInt32
      }
      pure (gs, st, false)

/-- `PTR_ShootTraverse`. -/
def ptrShootTraverse (damageMobj : DamageMobjFn) (gs0 : GameState) (st0 : AttackState)
    (inn : Intercept) : Except String (GameState × AttackState × Bool) := do
  if inn.isaline then
    match gs0.level.lines[inn.lineIdx]? with
    | none => throw "PTR_ShootTraverse: bad line"
    | some li =>
      shootSpecialLine gs0 st0.shootthingIdx li
      if (li.flags &&& ML_TWOSIDED) == 0 then
        return (← shootHitLine gs0 st0 inn li)
      let op ← lineOpening gs0 li
      let dist := fixedMul st0.attackrange inn.frac
      let mut hitline := false
      if li.backsector < 0 then
        let slopeB := fixedDiv (op.openbottom - st0.shootz) dist
        if slopeB > st0.aimslope then hitline := true
        let slopeT := fixedDiv (op.opentop - st0.shootz) dist
        if slopeT < st0.aimslope then hitline := true
      else
        match gs0.sectors[li.frontsector.toNatClampNeg]?,
              gs0.sectors[li.backsector.toNatClampNeg]? with
        | some front, some back =>
          if front.floorheight != back.floorheight then
            let slope := fixedDiv (op.openbottom - st0.shootz) dist
            if slope > st0.aimslope then hitline := true
          if front.ceilingheight != back.ceilingheight then
            let slope := fixedDiv (op.opentop - st0.shootz) dist
            if slope < st0.aimslope then hitline := true
        | _, _ => throw "PTR_ShootTraverse: bad sector"
      if hitline then
        shootHitLine gs0 st0 inn li
      else
        pure (gs0, st0, true)
  else
    match gs0.mobjs[inn.thingIdx]? with
    | none => throw "PTR_ShootTraverse: bad thing"
    | some th =>
      if inn.thingIdx == st0.shootthingIdx then
        return (gs0, st0, true)
      if (th.flags &&& MF_SHOOTABLE) == 0 then
        return (gs0, st0, true)
      let dist := fixedMul st0.attackrange inn.frac
      let thingtopslope := fixedDiv (th.z + th.height - st0.shootz) dist
      if thingtopslope < st0.aimslope then
        return (gs0, st0, true)
      let thingbottomslope := fixedDiv (th.z - st0.shootz) dist
      if thingbottomslope > st0.aimslope then
        return (gs0, st0, true)
      let frac := inn.frac - fixedDiv (10 * FRACUNIT) st0.attackrange
      let x := st0.trace.x + fixedMul st0.trace.dx frac
      let y := st0.trace.y + fixedMul st0.trace.dy frac
      let z := st0.shootz + fixedMul st0.aimslope (fixedMul frac st0.attackrange)
      let mut gs := gs0
      if (th.flags &&& MF_NOBLOOD) != 0 then
        gs ← spawnPuff gs st0.attackrange x y z
      else
        gs ← spawnBlood gs x y z st0.laDamage
      if st0.laDamage != 0 then
        gs ← damageMobj gs inn.thingIdx (some st0.shootthingIdx) (some st0.shootthingIdx)
          st0.laDamage
      pure (gs, st0, false)

private def xy2 (_gs : GameState) (mo : Mobj) (angle : UInt32) (distance : Int32) :
    Except String (Int32 × Int32) := do
  let fineIdx := (angle >>> ANGLETOFINESHIFT.toUInt32) &&& FINEMASK
  match finecosine[fineIdx.toNat]?, finesine[fineIdx.toNat]? with
  | some cosv, some sinv =>
    pure (mo.x + (distance >>> 16) * cosv, mo.y + (distance >>> 16) * sinv)
  | _, _ => throw "P_AimLineAttack: fine table OOB"

/-- `P_AimLineAttack`. Returns `(gs, slope, linetargetIdx or -1)`. -/
def aimLineAttack (gs0 : GameState) (mobjIdx : Nat) (angle : UInt32) (distance : Int32) :
    Except String (GameState × Int32 × Int32) := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_AimLineAttack: bad mobj"
  | some t1 =>
    let (x2, y2) ← xy2 gs0 t1 angle distance
    let shootz := t1.z + (t1.height >>> 1) + 8 * FRACUNIT
    let slope0 := aimSlopes
    let st0 : AttackState := {
      shootthingIdx := mobjIdx
      shootz
      attackrange := distance
      aimslope := 0
      laDamage := 0
      linetarget := -1
      topslope := slope0
      bottomslope := -slope0
      trace := mkTrace gs0.level.blockmap t1.x t1.y x2 y2
    }
    let (gs1, st1, _) ← pathTraverse gs0 st0 t1.x t1.y x2 y2
      (PT_ADDLINES ||| PT_ADDTHINGS) fun gs st inn =>
        ptrAimTraverse gs st inn
    if st1.linetarget >= 0 then
      pure (gs1, st1.aimslope, st1.linetarget)
    else
      pure (gs1, 0, -1)

/-- `P_LineAttack`. -/
def lineAttack (damageMobj : DamageMobjFn) (gs0 : GameState) (mobjIdx : Nat)
    (angle : UInt32) (distance slope damage : Int32) : Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_LineAttack: bad mobj"
  | some t1 =>
    let (x2, y2) ← xy2 gs0 t1 angle distance
    let shootz := t1.z + (t1.height >>> 1) + 8 * FRACUNIT
    let st0 : AttackState := {
      shootthingIdx := mobjIdx
      shootz
      attackrange := distance
      aimslope := slope
      laDamage := damage
      linetarget := -1
      topslope := 0
      bottomslope := 0
      trace := mkTrace gs0.level.blockmap t1.x t1.y x2 y2
    }
    let (gs1, _, _) ← pathTraverse gs0 st0 t1.x t1.y x2 y2
      (PT_ADDLINES ||| PT_ADDTHINGS) fun gs st inn =>
        ptrShootTraverse damageMobj gs st inn
    pure gs1

end Doom.Playsim.Hitscan
