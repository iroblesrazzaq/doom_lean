import Doom.Playsim.Angle
import Doom.Playsim.Enemy
import Doom.Playsim.Fixed
import Doom.Playsim.Flags
import Doom.Playsim.GameState
import Doom.Playsim.Info
import Doom.Playsim.Level
import Doom.Playsim.MapUtil
import Doom.Playsim.Mobj
import Doom.Playsim.Player
import Doom.Playsim.Psprite
import Doom.Playsim.Random
import Doom.Playsim.Sound
import Doom.Playsim.Spawn
import Doom.Playsim.Tables
import Doom.Playsim.Think
import Doom.Playsim.Weapons

/-!
# Doom.Playsim.Combat

Hitscan + pistol fire + `P_DamageMobj` enemy path (`p_map.c` / `p_pspr.c` /
`p_inter.c` / `p_mobj.c`) for DEMO1 first pistol hit. Psprite must not import
Map; GameState is threaded from `PlayerThink`.
-/

namespace Doom.Playsim.Combat

open Doom.Playsim.Angle
open Doom.Playsim.Enemy
open Doom.Playsim.Fixed
open Doom.Playsim.Flags
open Doom.Playsim.GameState
open Doom.Playsim.Info
open Doom.Playsim.Level
open Doom.Playsim.MapUtil
open Doom.Playsim.Mobj
open Doom.Playsim.Player
open Doom.Playsim.Psprite
open Doom.Playsim.Random
open Doom.Playsim.Sound
open Doom.Playsim.Spawn
open Doom.Playsim.Tables
open Doom.Playsim.Think
open Doom.Playsim.Weapons

/-- `p_local.h` `MISSILERANGE`. -/
def MISSILERANGE : Int32 := 32 * 64 * FRACUNIT
/-- `P_BulletSlope` / `P_AimLineAttack` range. -/
def AIMRANGE : Int32 := 16 * 64 * FRACUNIT
/-- `p_local.h` `BASETHRESHOLD`. -/
def BASETHRESHOLD : Int32 := 100
/-- Vanilla `SCREENHEIGHT` / `SCREENWIDTH` (`i_video.h`). -/
def SCREENHEIGHT : Int32 := 200
def SCREENWIDTH : Int32 := 320

private def setArr {α : Type} (arr : Array α) (i : Nat) (v : α) : Array α :=
  if h : i < arr.size then arr.set i v else arr

private def setMo (gs : GameState) (i : Nat) (mo : Mobj) : GameState :=
  { gs with mobjs := setArr gs.mobjs i mo }

private def setPlayer (gs : GameState) (i : Nat) (p : Player) : GameState :=
  { gs with players := setArr gs.players i p }

private def ammoAt (p : Player) (i : Int32) : Int32 :=
  match p.ammo[i.toNatClampNeg]? with | some v => v | none => 0

private def setAmmo (p : Player) (i : Int32) (v : Int32) : Player :=
  { p with ammo := setArr p.ammo i.toNatClampNeg v }

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
        let (gs4, _) ← Enemy.setMobjState gs3 idx S_BLOOD2
        pure gs4
      else if damage < 9 then
        let (gs4, _) ← Enemy.setMobjState gs3 idx S_BLOOD3
        pure gs4
      else
        pure gs3

/-- `P_DamageMobj` enemy path (player victim / kill are loud-errors). -/
def damageMobj (gs0 : GameState) (targetIdx : Nat) (inflictorIdx sourceIdx : Option Nat)
    (damage0 : Int32) : Except String GameState := do
  match gs0.mobjs[targetIdx]? with
  | none => throw "P_DamageMobj: bad target"
  | some target0 =>
    if (target0.flags &&& MF_SHOOTABLE) == 0 then
      return gs0
    if target0.health <= 0 then
      return gs0
    let mut gs := gs0
    let mut target := target0
    let mut damage := damage0
    if (target.flags &&& MF_SKULLFLY) != 0 then
      target := { target with momx := 0, momy := 0, momz := 0 }
      gs := setMo gs targetIdx target
    if target.player >= 0 then
      throw "P_DamageMobj: player victim not implemented"
    let skipThrust :=
      match inflictorIdx with
      | none => true
      | some _ =>
        if (target.flags &&& MF_NOCLIP) != 0 then true
        else
          match sourceIdx with
          | none => false
          | some si =>
            match gs.mobjs[si]? with
            | none => false
            | some src =>
              if src.player < 0 then false
              else
                match gs.players[src.player.toNatClampNeg]? with
                | some pl => pl.readyweapon == wp_chainsaw
                | none => false
    if !skipThrust then
      match inflictorIdx, gs.mobjs[targetIdx]? with
      | some ii, some tgt =>
        match gs.mobjs[ii]? with
        | none => throw "P_DamageMobj: bad inflictor"
        | some inf =>
          let mut ang := pointToAngle2 inf.x inf.y tgt.x tgt.y
          let info ←
            match mobjinfo[tgt.typeId.toNatClampNeg]? with
            | some i => pure i
            | none => throw "P_DamageMobj: missing mobjinfo"
          let mut thrust := damage * (FRACUNIT >>> 3) * 100 / info.mass
          if damage < 40 && damage > tgt.health && tgt.z - inf.z > 64 * FRACUNIT then
            let (r, rng) := pRandom gs.rng
            gs := { gs with rng }
            if (r &&& 1) != 0 then
              ang := ang + ANG180
              thrust := thrust * 4
          let fine := (ang >>> ANGLETOFINESHIFT.toUInt32) &&& FINEMASK
          match finecosine[fine.toNat]?, finesine[fine.toNat]? with
          | some cosv, some sinv =>
            match gs.mobjs[targetIdx]? with
            | none => throw "P_DamageMobj: lost before thrust"
            | some t2 =>
              gs := setMo gs targetIdx {
                t2 with
                momx := t2.momx + fixedMul thrust cosv
                momy := t2.momy + fixedMul thrust sinv
              }
          | _, _ => throw "P_DamageMobj: fine table OOB"
      | _, _ => throw "P_DamageMobj: thrust missing mobj"
    match gs.mobjs[targetIdx]? with
    | none => throw "P_DamageMobj: lost before health"
    | some t3 =>
      let health := t3.health - damage
      gs := setMo gs targetIdx { t3 with health }
      if health <= 0 then
        throw "P_KillMobj: not implemented"
      let info ←
        match mobjinfo[t3.typeId.toNatClampNeg]? with
        | some i => pure i
        | none => throw "P_DamageMobj: missing mobjinfo"
      match gs.mobjs[targetIdx]? with
      | none => throw "P_DamageMobj: lost before pain"
      | some t4 =>
        let (rPain, rng) := pRandom gs.rng
        gs := { gs with rng }
        if rPain < info.painchance && (t4.flags &&& MF_SKULLFLY) == 0 then
          let t4 := { t4 with flags := t4.flags ||| MF_JUSTHIT }
          gs := setMo gs targetIdx t4
          let (gs1, _) ← Enemy.setMobjState gs targetIdx info.painstate
          gs := gs1
        match gs.mobjs[targetIdx]? with
        | none => throw "P_DamageMobj: lost before retarget"
        | some t5 =>
          let t5 := { t5 with reactiontime := 0 }
          gs := setMo gs targetIdx t5
          let srcOk :=
            match sourceIdx with
            | none => false
            | some si =>
              if si == targetIdx then false
              else
                match gs.mobjs[si]? with
                | some src => src.typeId != MT_VILE
                | none => false
          if (t5.threshold == 0 || t5.typeId == MT_VILE) && srcOk then
            match sourceIdx with
            | none => pure gs
            | some si =>
              match gs.mobjs[targetIdx]? with
              | none => throw "P_DamageMobj: lost on retarget"
              | some t6 =>
                let t6 := { t6 with target := si.toInt32, threshold := BASETHRESHOLD }
                gs := setMo gs targetIdx t6
                if t6.state == info.spawnstate && info.seestate != 0 then
                  let (gs1, _) ← Enemy.setMobjState gs targetIdx info.seestate
                  pure gs1
                else
                  pure gs
          else
            pure gs

private def byteAt (pic : ByteArray) (i : Nat) : UInt8 :=
  match pic[i]? with | some b => b | none => 0

private def isSkyPic (pic : ByteArray) : Bool :=
  pic.size >= 6 && byteAt pic 0 == 70 && byteAt pic 1 == 95 &&
    byteAt pic 2 == 83 && byteAt pic 3 == 75 && byteAt pic 4 == 89 &&
    byteAt pic 5 == 49

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
def ptrShootTraverse (gs0 : GameState) (st0 : AttackState) (inn : Intercept) :
    Except String (GameState × AttackState × Bool) := do
  if inn.isaline then
    match gs0.level.lines[inn.lineIdx]? with
    | none => throw "PTR_ShootTraverse: bad line"
    | some li =>
      shootSpecialLine gs0 st0.shootthingIdx li
      if (li.flags &&& ML_TWOSIDED) == 0 then
        throw "P_SpawnPuff: not implemented"
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
        match gs0.level.sectors[li.frontsector.toNatClampNeg]? with
        | none => throw "PTR_ShootTraverse: bad frontsector"
        | some front =>
          if isSkyPic front.ceilingpic then
            return (gs0, st0, false)
          throw "P_SpawnPuff: not implemented"
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
        throw "P_SpawnPuff: not implemented"
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
def lineAttack (gs0 : GameState) (mobjIdx : Nat) (angle : UInt32) (distance slope damage : Int32) :
    Except String GameState := do
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
        ptrShootTraverse gs st inn
    pure gs1

/-- `P_BulletSlope`. Returns `(gs, bulletslope)`. -/
def bulletSlope (gs0 : GameState) (mobjIdx : Nat) : Except String (GameState × Int32) := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_BulletSlope: bad mobj"
  | some mo =>
    let mut an := mo.angle
    let (gs1, slope1, lt1) ← aimLineAttack gs0 mobjIdx an AIMRANGE
    if lt1 >= 0 then
      return (gs1, slope1)
    an := an + ((1 : UInt32) <<< 26)
    let (gs2, slope2, lt2) ← aimLineAttack gs1 mobjIdx an AIMRANGE
    if lt2 >= 0 then
      return (gs2, slope2)
    an := an - ((2 : UInt32) <<< 26)
    let (gs3, slope3, _) ← aimLineAttack gs2 mobjIdx an AIMRANGE
    pure (gs3, slope3)

/-- `P_GunShot`. -/
def gunShot (gs0 : GameState) (mobjIdx : Nat) (accurate : Bool) (bulletslope : Int32) :
    Except String GameState := do
  match gs0.mobjs[mobjIdx]? with
  | none => throw "P_GunShot: bad mobj"
  | some mo =>
    let (r, rng) := pRandom gs0.rng
    let damage := 5 * (r % 3 + 1)
    let mut gs := { gs0 with rng }
    let mut angle := mo.angle
    if !accurate then
      let (d, rng2) := pSubRandom gs.rng
      gs := { gs with rng := rng2 }
      angle := angle + (d.toUInt32 <<< 18)
    lineAttack gs mobjIdx angle MISSILERANGE bulletslope damage

/-- `P_CheckAmmo` true-path; loud-error if a weapon switch would occur. -/
def checkAmmo (player : Player) : Except String Unit := do
  let wi ← weaponAt player.readyweapon
  let ammo := wi.ammo
  let count : Int32 :=
    if player.readyweapon == wp_bfg then (40 : Int32)
    else if player.readyweapon == wp_supershotgun then 2
    else 1
  if ammo == am_noammo then
    return
  if ammoAt player ammo >= count then
    return
  throw "P_CheckAmmo: weapon switch not implemented"

/-- `DecreaseAmmo` (in-range clip only for this open subset). -/
def decreaseAmmo (player : Player) (ammonum amount : Int32) : Except String Player :=
  if ammonum < NUMAMMO.toInt32 then
    pure (setAmmo player ammonum (ammoAt player ammonum - amount))
  else
    throw "DecreaseAmmo: maxammo overflow path not implemented"

/-- `P_FireWeapon`. -/
def fireWeapon (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "P_FireWeapon: bad player"
  | some player =>
    checkAmmo player
    match gs0.mobjs[player.mo.toNatClampNeg]? with
    | none => throw "P_FireWeapon: bad mo"
    | some _ =>
      let (gs1, _) ← Enemy.setMobjState gs0 player.mo.toNatClampNeg S_PLAY_ATK1
      match gs1.players[playerIdx]? with
      | none => throw "P_FireWeapon: player lost"
      | some p1 =>
        let wi ← weaponAt p1.readyweapon
        match states[wi.atkstate.toNat]? with
        | none => throw "P_FireWeapon: bad atkstate"
        | some st =>
          if st.action != actionNull then
            throw s!"P_FireWeapon: unexpected atk action {st.action}"
          let psp := getPsp p1.psprites ps_weapon
          let p2 := {
            p1 with
            psprites := setPsp p1.psprites ps_weapon
              { psp with state := wi.atkstate, tics := st.tics }
          }
          let gs2 := setPlayer gs1 playerIdx p2
          noiseAlert gs2 p2.mo.toNatClampNeg p2.mo.toNatClampNeg

/-- `A_WeaponReady`. -/
def weaponReady (gs0 : GameState) (playerIdx : Nat) (position : Nat) :
    Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "A_WeaponReady: bad player"
  | some player0 =>
    let mut gs := gs0
    let mut player := player0
    match gs.mobjs[player.mo.toNatClampNeg]? with
    | none => throw "A_WeaponReady: bad mo"
    | some mo =>
      if mo.state == S_PLAY_ATK1 || mo.state == S_PLAY_ATK2 then
        let (gs1, _) ← Enemy.setMobjState gs player.mo.toNatClampNeg S_PLAY
        gs := gs1
        match gs.players[playerIdx]? with
        | some p => player := p
        | none => throw "A_WeaponReady: player lost after ATK reset"
      if player.readyweapon == wp_chainsaw then
        throw "A_WeaponReady: chainsaw idle sound not implemented"
      if player.pendingweapon != wp_nochange || player.health == 0 then
        throw "A_WeaponReady: weapon down/change not implemented"
      if (player.cmd.buttons &&& BT_ATTACK) != 0 then
        if !player.attackdown ||
            (player.readyweapon != wp_missile && player.readyweapon != wp_bfg) then
          player := { player with attackdown := true }
          gs := setPlayer gs playerIdx player
          gs ← fireWeapon gs playerIdx
          return gs
      else
        player := { player with attackdown := false }
      let psp0 := getPsp player.psprites position
      let ang0 := (128 * gs.leveltime) &&& FINEMASK
      let sx :=
        match finecosine[ang0.toNat]? with
        | some c => FRACUNIT + fixedMul player.bob c
        | none => psp0.sx
      let ang1 := ang0 &&& ((FINEANGLES.toUInt32 / 2) - 1)
      let sy :=
        match finesine[ang1.toNat]? with
        | some s => WEAPONTOP + fixedMul player.bob s
        | none => psp0.sy
      let player' := {
        player with
        psprites := setPsp player.psprites position { psp0 with sx, sy }
      }
      pure (setPlayer gs playerIdx player')

/-- `A_FirePistol`. -/
def firePistol (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "A_FirePistol: bad player"
  | some player0 =>
    let mut gs := { gs0 with rng := startSoundPitchRng gs0.rng sfx_pistol }
    let (gs1, _) ← Enemy.setMobjState gs player0.mo.toNatClampNeg S_PLAY_ATK2
    gs := gs1
    match gs.players[playerIdx]? with
    | none => throw "A_FirePistol: player lost"
    | some p1 =>
      let wi ← weaponAt p1.readyweapon
      let p2 ← decreaseAmmo p1 wi.ammo 1
      match states[wi.flashstate.toNat]? with
      | none => throw "A_FirePistol: bad flashstate"
      | some stFlash =>
        let pspF := getPsp p2.psprites ps_flash
        let mut p3 := {
          p2 with
          psprites := setPsp p2.psprites ps_flash
            { pspF with state := wi.flashstate, tics := stFlash.tics }
          extralight := 1
        }
        if stFlash.action == action_A_Light1 then
          p3 := { p3 with extralight := 1 }
        else if stFlash.action != actionNull then
          throw s!"A_FirePistol: unexpected flash action {stFlash.action}"
        gs := setPlayer gs playerIdx p3
        let (gsB, slope) ← bulletSlope gs p3.mo.toNatClampNeg
        gs := gsB
        match gs.players[playerIdx]? with
        | none => throw "A_FirePistol: player lost before shot"
        | some p4 =>
          gunShot gs p4.mo.toNatClampNeg (p4.refire == 0) slope

/-- `A_ReFire`. -/
def reFire (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  match gs0.players[playerIdx]? with
  | none => throw "A_ReFire: bad player"
  | some player =>
    if (player.cmd.buttons &&& BT_ATTACK) != 0 &&
        player.pendingweapon == wp_nochange &&
        player.health != 0 then
      let gs := setPlayer gs0 playerIdx { player with refire := player.refire + 1 }
      fireWeapon gs playerIdx
    else
      let gs := setPlayer gs0 playerIdx { player with refire := 0 }
      match gs.players[playerIdx]? with
      | none => throw "A_ReFire: player lost"
      | some p =>
        checkAmmo p
        pure gs

/-- GameState-threaded `P_SetPsprite` (weapon fire + raise). -/
def setPsprite (gs0 : GameState) (playerIdx : Nat) (position : Nat) (stnum0 : UInt32) :
    Except String GameState := do
  let mut gs := gs0
  let mut stnum := stnum0
  let mut guard : Nat := 0
  while guard < 100000 do
    guard := guard + 1
    match gs.players[playerIdx]? with
    | none => throw "P_SetPsprite: bad player"
    | some p0 =>
      if stnum == 0 then
        return setPlayer gs playerIdx
          { p0 with psprites := setPsp p0.psprites position Psprite.inactive }
      match states[stnum.toNat]? with
      | none => throw s!"P_SetPsprite: bad state {stnum}"
      | some st =>
        let psp0 := getPsp p0.psprites position
        let mut psp := { psp0 with state := stnum, tics := st.tics }
        if st.misc1 != 0 then
          psp := { psp with sx := st.misc1 * FRACUNIT, sy := st.misc2 * FRACUNIT }
        let p1 := { p0 with psprites := setPsp p0.psprites position psp }
        gs := setPlayer gs playerIdx p1
        let mut contRaise := false
        if st.action == action_A_Raise then
          match gs.players[playerIdx]? with
          | none => throw "P_SetPsprite: player lost on raise"
          | some pR =>
            let psp1 := getPsp pR.psprites position
            let sy := psp1.sy - RAISESPEED
            if sy > WEAPONTOP then
              gs := setPlayer gs playerIdx
                { pR with psprites := setPsp pR.psprites position { psp1 with sy } }
            else
              let pTop := {
                pR with
                psprites := setPsp pR.psprites position { psp1 with sy := WEAPONTOP }
              }
              gs := setPlayer gs playerIdx pTop
              let wi ← weaponAt pTop.readyweapon
              stnum := wi.readystate
              contRaise := true
        else if st.action == action_A_WeaponReady then
          gs ← weaponReady gs playerIdx position
        else if st.action == action_A_FirePistol then
          gs ← firePistol gs playerIdx
        else if st.action == action_A_Light1 then
          match gs.players[playerIdx]? with
          | none => throw "A_Light1: bad player"
          | some pL =>
            gs := setPlayer gs playerIdx { pL with extralight := 1 }
        else if st.action == action_A_Light0 then
          match gs.players[playerIdx]? with
          | none => throw "A_Light0: bad player"
          | some pL =>
            gs := setPlayer gs playerIdx { pL with extralight := 0 }
        else if st.action == action_A_ReFire then
          gs ← reFire gs playerIdx
        else if st.action == action_A_GunFlash then
          throw "A_GunFlash: not implemented"
        else if st.action != actionNull then
          throw s!"P_SetPsprite: unimplemented psprite action {st.action} in state {stnum}"
        if !contRaise then
          match gs.players[playerIdx]? with
          | none => throw "P_SetPsprite: player lost after action"
          | some p2 =>
            let psp2 := getPsp p2.psprites position
            if psp2.tics != 0 then
              return gs
            match states[psp2.state.toNat]? with
            | none => throw "P_SetPsprite: state lost"
            | some st2 => stnum := st2.nextstate
  throw "P_SetPsprite: cycle limit"

/-- GameState-threaded `P_MovePsprites`. -/
def movePsprites (gs0 : GameState) (playerIdx : Nat) : Except String GameState := do
  let mut gs := gs0
  let mut i : Nat := 0
  while i < NUMPSPRITES do
    match gs.players[playerIdx]? with
    | none => throw "P_MovePsprites: bad player"
    | some p =>
      let psp := getPsp p.psprites i
      if psp.state != 0 then
        if psp.tics != -1 then
          let tics := psp.tics - 1
          gs := setPlayer gs playerIdx
            { p with psprites := setPsp p.psprites i { psp with tics } }
          if tics == 0 then
            match gs.players[playerIdx]? with
            | none => throw "P_MovePsprites: player lost"
            | some _ =>
              match states[psp.state.toNat]? with
              | none => throw "P_MovePsprites: bad state"
              | some st =>
                gs ← setPsprite gs playerIdx i st.nextstate
    i := i + 1
  match gs.players[playerIdx]? with
  | none => throw "P_MovePsprites: player lost at copy"
  | some p =>
    let w := getPsp p.psprites ps_weapon
    let f := getPsp p.psprites ps_flash
    pure (setPlayer gs playerIdx
      { p with psprites := setPsp p.psprites ps_flash { f with sx := w.sx, sy := w.sy } })

end Doom.Playsim.Combat
