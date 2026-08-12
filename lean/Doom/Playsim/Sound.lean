import Doom.Playsim.Random

/-!
# Doom.Playsim.Sound

Sound-path `M_Random` (`rndindex`) draws that affect the playsim digest.
Audio output itself is not simulated — only the pitch-variation RNG from
`S_StartSound` (`s_sound.c`).
-/

namespace Doom.Playsim.Sound

open Doom.Playsim.Random

/-- `sfxenum_t` values needed for pitch-branch rules (`sounds.h`, `sfx_None = 0`). -/
def sfx_sawup : Nat := 10
def sfx_sawhit : Nat := 13
def sfx_itemup : Nat := 32
def sfx_oof : Nat := 34
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

end Doom.Playsim.Sound
