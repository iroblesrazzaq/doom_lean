import Doom.Playsim.Fixed

/-!
# Doom.Render.Constants

Renderer constants from oracle `r_main.h`, `r_plane.c`, `r_segs.c`, `r_things.h`.
-/

namespace Doom.Render.Constants

open Doom.Playsim.Fixed

def lightLevels : Nat := 16
def maxLightScale : Nat := 48
def lightScaleShift : Nat := 12
def maxLightZ : Nat := 128
def lightZShift : Nat := 20
def numColorMaps : Nat := 32
def maxVisPlanes : Nat := 128
def maxDrawSegs : Nat := 256
def maxVisSprites : Nat := 128
def maxOpenings : Nat := 320 * 64
def heightBits : Nat := 12
def heightUnit : Nat := 1 <<< heightBits
def fieldOfView : Nat := 2048
def angletoskyShift : Nat := 22
def maxSegs : Nat := 320 / 2 + 1
def sbarHeight : Nat := 32
def screenBlocksDefault : Nat := 9
def detailShift : Nat := 0
def lightSegShift : Nat := 4
def mlDontpegBottom : Int32 := 16
def mlDontpegTop : Int32 := 8
def silBottom : Nat := 1
def silTop : Nat := 2
def shrtMax : Nat := 32767
def ffFrameMask : UInt32 := 0x7fff
def ffFullbright : UInt32 := 0x8000
def minZ : Int32 := 4 * FRACUNIT

end Doom.Render.Constants
