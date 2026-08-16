import Doom.Playsim.Fixed
import Doom.Playsim.GameState
import Doom.Render.Bsp
import Doom.Render.Data
import Doom.Render.Plane
import Doom.Render.Things
import Doom.Render.Types
import Doom.Render.Util
import Doom.Render.View
import Doom.Wad

/-!
# Doom.Render.Main

`R_SetupFrame` / `R_RenderPlayerView` (`r_main.c`).
-/

namespace Doom.Render.Main

open Doom.Playsim.Fixed
open Doom.Playsim.GameState
open Doom.Render.Bsp
open Doom.Render.Data
open Doom.Render.Plane
open Doom.Render.Things
open Doom.Render.Types
open Doom.Render.Util
open Doom.Render.View
open Doom.Wad

/--
`R_ExecuteSetViewSize`. Applies a pending `R_SetViewSize`. C default is
`screenblocks=9`, `detail=0`; unit tests still exercise `blocks=10`.
-/
def executeSetViewSize (p : ViewSizePending) : ViewSize × ViewSizePending :=
  let screenW : Int32 := Int32.ofNat Doom.Render.Types.screenWidth
  let screenH : Int32 := Int32.ofNat Doom.Render.Types.screenHeight
  let scaledviewwidth : Int32 :=
    if p.setblocks == 11 then screenW
    else p.setblocks * (32 : Int32)
  let viewheight : Int32 :=
    if p.setblocks == 11 then screenH
    else (p.setblocks * (168 : Int32) / (10 : Int32)) &&& (~~~ (7 : Int32))
  let detailshift : Nat := i32ToNat p.setdetail
  let viewwidth : Int32 := scaledviewwidth >>> Int32.ofNat detailshift
  let centery : Int32 := viewheight / (2 : Int32)
  let centerx : Int32 := viewwidth / (2 : Int32)
  let centerxfrac : Int32 := centerx <<< (16 : Int32)
  let centeryfrac : Int32 := centery <<< (16 : Int32)
  let projection : Int32 := centerxfrac
  let (viewwindowx, viewwindowy) := initBuffer scaledviewwidth viewheight
  let mapping := initViewMapping viewwidth centerx
  let pspritescale : Int32 := (FRACUNIT * viewwidth) / screenW
  let pspriteiscale : Int32 := (FRACUNIT * screenW) / viewwidth
  let slopes :=
    initPlaneSlopeTables initPlaneDrawCtx viewwidth viewheight mapping.xtoviewangle detailshift
  let vs : ViewSize := {
    scaledviewwidth, viewwidth, viewheight, centerx, centery,
    centerxfrac, centeryfrac, projection, detailshift, viewwindowx, viewwindowy,
    mapping, screenheightarray := mkScreenheightarray viewheight,
    yslope := slopes.yslope, distscale := slopes.distscale,
    pspritescale, pspriteiscale
  }
  (vs, { p with setsizeneeded := false })

def renderPlayerView (data : RenderData) (wad : WadDirectory) (gs : GameState) (fb : Framebuffer)
    (vs : ViewSize := {}) (fuzzpos : Nat := 0) : Except String (Framebuffer × Nat) := do
  if gs.level.nodes.size == 0 then
    throw "R_RenderPlayerView: no BSP nodes"
  let data :=
    if vs.screenheightarray.size == 0 then data
    else { data with screenheightarray := vs.screenheightarray }
  let frame ← renderBspFromGame data wad gs fb vs
  let extralight :=
    match gs.players[gs.consoleplayer]? with
    | some p => p.extralight
    | none => (0 : Int32)
  let fb' ← drawPlanes data wad frame.fb frame.planes frame.view frame.viewz extralight vs
    gs.flattranslation
  drawMaskedEx data wad gs fb' frame.drawSegs frame.visSprites vs fuzzpos

end Doom.Render.Main
