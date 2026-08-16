import Lake

open System Lake DSL

package doom where version := v!"0.1.0"

@[default_target] lean_lib Doom

/-- `pkg-config` flags; drop `SDL2main` so it cannot replace Lean's `main`. -/
def pkgConfigFlags (args : Array String) : IO (Array String) := do
  try
    let r ← IO.Process.output { cmd := "pkg-config", args }
    if r.exitCode != 0 then
      return #[]
    let flat := r.stdout.replace "\n" " " |>.replace "\t" " "
    let parts :=
      flat.splitOn " " |>.filter fun s =>
        !s.isEmpty && s != "-lSDL2main" &&
          !s.startsWith "-Wl,-framework" && s != "-framework" && s != "Cocoa"
    return parts.toArray
  catch _ =>
    return #[]

def sdlCflags : Array String := run_io pkgConfigFlags #["--cflags", "sdl2"]
def sdlLibs : Array String := run_io pkgConfigFlags #["--libs", "sdl2"]

target i_sdl_o pkg : FilePath := do
  let src := pkg.dir / "native" / "i_sdl.c"
  let oFile := pkg.buildDir / "native" / "i_sdl.o"
  let srcJob ← inputTextFile src
  buildO oFile srcJob sdlCflags #["-fPIC"] "cc"

lean_exe «stub-zero» where root := `StubZero
lean_exe «stub-drift» where root := `StubDrift
lean_exe verify where root := `Verify
lean_exe «perf-encode» where root := `PerfEncode
lean_exe «harness-test» where root := `HarnessTest
lean_exe «playsim-test» where root := `PlaysimTest
lean_exe «map-test» where root := `MapTest
lean_exe «spawn-test» where root := `SpawnTest
lean_exe «tic0-test» where root := `Tic0Test
lean_exe «digest-test» where root := `DigestTest
lean_exe «movement-test» where root := `MovementTest
lean_exe «enemy-test» where root := `EnemyTest
lean_exe «render-data-test» where root := `RenderDataTest
lean_exe «fb-test» where root := `FbTest
lean_exe «dump-hex-test» where root := `DumpHexTest
lean_exe «blitter-test» where root := `BlitterTest
lean_exe «clip-test» where root := `ClipTest
lean_exe «view-setup-test» where root := `ViewSetupTest
lean_exe «setviewsize-test» where root := `SetViewSizeTest
lean_exe «addline-test» where root := `AddlineTest
lean_exe «bspnode-test» where root := `BspnodeTest
lean_exe «segloop-test» where root := `SegloopTest
lean_exe «storewall-test» where root := `StorewallTest
lean_exe «storewall-twosided-test» where root := `StorewallTwosidedTest
lean_exe «bsp-storewall-solid-test» where root := `BspStorewallSolidTest
lean_exe «bsp-storewall-twosided-test» where root := `BspStorewallTwosidedTest
lean_exe «bsp-masked-replay-test» where root := `BspMaskedReplayTest
lean_exe «plane-visplane-test» where root := `PlaneVisplaneTest
lean_exe «plane-makespans-test» where root := `PlaneMakespansTest
lean_exe «plane-spans-test» where root := `PlaneSpansTest
lean_exe «plane-visplane-sb9-test» where root := `PlaneVisplaneSb9Test
lean_exe «live-riser-scale-test» where root := `LiveRiserScaleTest
lean_exe «live-wall-column-test» where root := `LiveWallColumnTest
lean_exe «init-sprites-test» where root := `InitSpritesTest
lean_exe «add-sprites-test» where root := `AddSpritesTest
lean_exe «draw-sprites-test» where root := `DrawSpritesTest
lean_exe «draw-psprites-test» where root := `DrawPspritesTest
lean_exe «display-test» where root := `DisplayTest
lean_exe «stwidgets-test» where root := `StwidgetsTest
lean_exe «hu-test» where root := `HuTest
lean_exe «flat-anim-test» where root := `FlatAnimTest
lean_exe «ticcmd-test» where root := `TiccmdTest
lean_exe «doom-window-test» where root := `DoomWindowTest

lean_exe doom where
  root := `DoomMain
  moreLeancArgs := #["-include", "native/i_sdl.h"]
  moreLinkObjs := #[i_sdl_o]
  moreLinkArgs := sdlLibs
