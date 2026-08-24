import Doom.Playsim.Tables.Finesine
import Doom.Playsim.Tables.Finetangent
import Doom.Playsim.Tables.Tantoangle

/-!
# Doom.Playsim.Tables

Re-exports the mechanically generated lookup tables (see
`scripts/gen_tables.py`) and defines `finecosine` as `finesine` viewed at
offset `FINEANGLES/4 = 2048`, matching the oracle
`finecosine = &finesine[FINEANGLES/4]`.
-/

namespace Doom.Playsim.Tables

/--
`finecosine` is `finesine` phase-shifted by 2048 entries (8192 entries,
covering the indices the oracle pointer is used with).
-/
def finecosine : Array Int32 := finesine.extract 2048 finesine.size

end Doom.Playsim.Tables
