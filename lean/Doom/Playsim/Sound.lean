import Doom.Playsim.Fixed
import Doom.Playsim.Random

/-!
# Doom.Playsim.Sound

Sound-path `M_Random` (`rndindex`) draws that affect the playsim digest.
Audio output itself is not simulated — only the pitch-variation RNG from
`S_StartSound` (`s_sound.c`).
-/

namespace Doom.Playsim.Sound

open Doom.Playsim.Fixed
open Doom.Playsim.Random

/-- `sfxenum_t` values needed for pitch-branch rules (`sounds.h`, `sfx_None = 0`). -/
def sfx_pistol : Nat := 1
def sfx_shotgn : Nat := 2
def sfx_sawup : Nat := 10
def sfx_sawhit : Nat := 13
def sfx_itemup : Nat := 32
def sfx_doropn : Nat := 20
def sfx_oof : Nat := 34
def sfx_posit1 : Nat := 36
def sfx_posit2 : Nat := 37
def sfx_posit3 : Nat := 38
def sfx_bgsit1 : Nat := 39
def sfx_bgsit2 : Nat := 40
def sfx_podth1 : Nat := 59
def sfx_podth2 : Nat := 60
def sfx_podth3 : Nat := 61
def sfx_bgdth1 : Nat := 62
def sfx_bgdth2 : Nat := 63
def sfx_tink : Nat := 87

/--
Advance `rndindex` exactly as `S_StartSound` does for pitch variation once the
call has passed volume / audibility gates.

- `sfx_sawup..sfx_sawhit`: one `M_Random`
- otherwise, unless `sfx_itemup` / `sfx_tink`: one `M_Random`
- `sfx_itemup` / `sfx_tink`: no draw

`sfx_oof` (`SOUND` with no link) takes the generic branch → one draw when a
hard landing calls `S_StartSound(mo, sfx_oof)`.
-/
def startSoundPitchRng (rng : RandomState) (sfxId : Nat) : RandomState :=
  if sfxId >= sfx_sawup && sfxId <= sfx_sawhit then
    (mRandom rng).2
  else if sfxId != sfx_itemup && sfxId != sfx_tink then
    (mRandom rng).2
  else
    rng

/-- `s_sound.c` `S_CLIPPING_DIST` / `S_CLOSE_DIST` / `S_ATTENUATOR`. -/
def S_CLIPPING_DIST : Int32 := 1200 * FRACUNIT
def S_CLOSE_DIST : Int32 := 200 * FRACUNIT
def S_ATTENUATOR : Int32 := 1000
/-- Vanilla `S_Init(sfxVolume * 8)` with default `sfxVolume = 8`. -/
def snd_SfxVolume : Int32 := 64

/-- Approx Euclidean distance used by `S_AdjustSoundParams`. -/
def approxSoundDist (lx ly sx sy : Int32) : Int32 :=
  let adx := wabs (lx - sx)
  let ady := wabs (ly - sy)
  adx + ady - ((if adx < ady then adx else ady) >>> 1)

/--
`S_AdjustSoundParams` audibility for `gamemap != 8` (E1M1). Inaudible
origins return before the pitch `M_Random` in `S_StartSound`.
-/
def soundAudible (lx ly sx sy : Int32) : Bool :=
  let dist := approxSoundDist lx ly sx sy
  if dist > S_CLIPPING_DIST then
    false
  else if dist < S_CLOSE_DIST then
    snd_SfxVolume > 0
  else
    let vol := (snd_SfxVolume * ((S_CLIPPING_DIST - dist) >>> 16)) / S_ATTENUATOR
    vol > 0

/-- Pitch RNG only when `S_StartSound` would not early-out as inaudible. -/
def startSoundPitchRngMaybe (rng : RandomState) (sfxId : Nat) (audible : Bool) :
    RandomState :=
  if audible then startSoundPitchRng rng sfxId else rng

end Doom.Playsim.Sound
