import Lake
open Lake DSL

open Lean in
/-- Resolve a bare shared-library filename (e.g. `libpng.so`) to an absolute
path at configuration time by probing the standard Linux library
directories. We link Vulkan and libpng by absolute path rather than
`-L`/`-l` so that no system library directory is added to lld's search
path — adding one would make lld resolve libc against the system glibc,
shadowing the bundled glibc whose startup object Lean's runtime needs (see
`platformLinkArgs`). Falls back to the Debian/Ubuntu amd64 multiarch path so
a missing library still names a concrete file in the error. -/
elab "sysLib!" nameStx:str : term => do
  let name := nameStx.getString
  let dirs := #["/usr/lib/x86_64-linux-gnu", "/usr/lib/aarch64-linux-gnu",
                "/lib/x86_64-linux-gnu", "/usr/lib64", "/usr/lib", "/lib"]
  let mut path := s!"/usr/lib/x86_64-linux-gnu/{name}"
  for d in dirs do
    let cand := s!"{d}/{name}"
    if ← System.FilePath.pathExists cand then
      path := cand
      break
  return mkStrLit path

open Lean in
/-- The Lean toolchain's library directory (`<sysroot>/lib`), resolved at
configuration time via `lean --print-prefix`. The Linux build links with the
system compiler (`LEAN_CC=cc`, required because Lean's bundled glibc is older
than the system's — see the README), and in that mode Lake does not add this
directory to the link line, yet Lean's bundled C++ runtime, libuv, GMP,
OpenSSL, and libunwind all live there. (This directory holds no libc, so
adding it does not disturb glibc resolution.) -/
elab "leanLibDir!" : term => do
  let out ← IO.Process.output { cmd := "lean", args := #["--print-prefix"] }
  let pfx := out.stdout.trimAscii.toString
  return mkStrLit (pfx ++ "/lib")

open Lean in
/-- The macOS SDK path, resolved at configuration time via
`xcrun --show-sdk-path` (the linker needs it as `-syslibroot` to find the
AudioToolbox / CoreFoundation frameworks). Falls back to the Command Line
Tools default when `xcrun` is missing or fails — including on Linux, where
this term is elaborated but never used. -/
elab "macSdkPath!" : term => do
  let fallback := "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"
  let path ← try
    let out ← IO.Process.output { cmd := "xcrun", args := #["--show-sdk-path"] }
    let p := out.stdout.trimAscii.toString
    pure (if out.exitCode == 0 && p != "" then p else fallback)
  catch _ => pure fallback
  return mkStrLit path

open Lean in
/-- The Homebrew prefix, resolved at configuration time: Apple-silicon Macs
install Homebrew under `/opt/homebrew`, Intel Macs under `/usr/local`. Probe
for the former and fall back to the latter. On Linux this term is elaborated
but never used. -/
elab "brewPrefix!" : term => do
  let armPrefix := "/opt/homebrew"
  let path := if ← System.FilePath.pathExists armPrefix then armPrefix
              else "/usr/local"
  return mkStrLit path

/-- Linker flags differ per platform. On macOS, SDL3 and the Vulkan loader
(MoltenVK underneath) come from Homebrew, and PNG decoding and MIDI music
use Apple frameworks. `c/shell.c` forces `main` onto the process main
thread there (`LEAN_MAIN_USE_THREAD=0`, Cocoa requires it), giving up the
big worker-thread stack Lean normally provides; enlarging the main stack
back (`-Wl,-stack_size,0x4000000`) would be the vanilla-safe choice, but
the toolchain's ld64.lld does not implement that option — it warned and
ignored it on every link — and the game demonstrably runs fine on the
default main stack, so the flag was dropped. On Linux, SDL3/Vulkan/libpng
are ordinary shared libraries, and Lean keeps its large-stacked worker
thread, so none of the macOS-specific flags apply. -/
def platformLinkArgs : Array String :=
  if System.Platform.isOSX then
    #["-L" ++ brewPrefix! ++ "/lib", "-lSDL3", "-lvulkan",
      "-Wl,-syslibroot," ++ macSdkPath!,
      "-framework", "AudioToolbox",
      "-framework", "CoreFoundation",
      "-framework", "ImageIO",
      "-framework", "CoreGraphics",
      "-Wl,-rpath," ++ brewPrefix! ++ "/lib"]
  else
    -- Linux: link SDL3 (built/installed under /usr/local by default), the
    -- system Vulkan loader, and libpng (used for PNG decode in place of
    -- Apple's ImageIO).
    --
    -- Vulkan and libpng are named by absolute path rather than `-L … -l…`
    -- on purpose: Lean bundles its own glibc, and adding a system library
    -- dir (e.g. /usr/lib/x86_64-linux-gnu) to the search path makes lld
    -- resolve libc against the *system* glibc, whose newer versions dropped
    -- the `__libc_csu_init/fini` symbols Lean's startup object still needs.
    -- A positional `.so` input links the library without polluting the
    -- search path, so Lean's bundled glibc stays authoritative. SDL3 lives
    -- under /usr/local (not a glibc dir), so a plain `-L`/`-rpath` is fine.
    #["-L/usr/local/lib", "-lSDL3", "-Wl,-rpath,/usr/local/lib",
      sysLib! "libvulkan.so", sysLib! "libpng.so", "-lm",
      -- LEAN_CC=cc omits the toolchain lib dir where Lean's C++ runtime,
      -- libuv, GMP, etc. live; name it so the system linker finds them.
      "-L", leanLibDir!]

/-- C-compile flags for `c/shell.c`. macOS needs Homebrew's include prefix;
Linux finds SDL3 under /usr/local/include and Vulkan/libpng in the default
system include path. -/
def platformCcArgs : Array String :=
  if System.Platform.isOSX then #["-I", brewPrefix! ++ "/include"]
  else #["-I", "/usr/local/include"]

package dill where
  moreLinkArgs := platformLinkArgs

lean_lib Dill

@[default_target]
lean_exe dill where
  root := `Main

/-! The test suites and their support libraries all live under `Tests/`
(`srcDir` below); module names are unchanged, so their imports are too. -/

/-- Helpers shared by the test binaries: the pass/fail counter, the
map-loading helpers, and the renderer completeness probe. Its own library
so both `tests` and `wadtests` can import it without either becoming a
dependency of the other. -/
lean_lib TestSupport where
  srcDir := "Tests"

/-- `lake test`: the engine's behaviour against `doom.wad`. Fast enough to
run on every change. -/
@[test_driver]
lean_exe tests where
  root := `Tests
  srcDir := "Tests"

/-- `lake exe wadtests`: the broad sweep over every IWAD and PWAD in the
project root — every map decodes, every texture resolves, and the renderer
paints every pixel across every episode. Kept out of `lake test` because it
scales with which data files happen to be present, and because it is large
enough that sharing a module with the main suite made both recompile. -/
lean_exe wadtests where
  root := `WadTests
  srcDir := "Tests"

/-- The vanilla Doom reference tables (transcribed from linuxdoom-1.10),
compiled standalone — it imports nothing — for `vanillatests` to diff
against. -/
lean_lib VanillaData where
  srcDir := "Tests"

/-- `lake exe vanillatests`: the vanilla-fidelity golden diff — DILL's actor,
weapon, animation, switch and speed tables against `VanillaData`, the
mechanical transcription of linuxdoom-1.10. Needs no WAD. Kept out of
`lake test` so the fast suite's shape does not change; run it whenever
`Dill/Game/Info.lean` or its neighbours are touched. -/
lean_exe vanillatests where
  root := `VanillaTests
  srcDir := "Tests"

target shell.o pkg : System.FilePath := do
  let oFile := pkg.buildDir / "c" / "shell.o"
  let srcJob ← inputTextFile <| pkg.dir / "c" / "shell.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString] ++ platformCcArgs
  buildO oFile srcJob weakArgs #["-O2"] "cc" getLeanTrace

extern_lib libleanshell pkg := do
  let job ← fetch <| pkg.target ``shell.o
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "leanshell") #[job]
