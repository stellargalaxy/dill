# DILL — Doom in Lean Language

![DILL logo](dill_logo.png)

A playable implementation of Doom (1993) written in Lean 4, rendered through
Vulkan. The design is a **functional core with an imperative shell**:
everything that *is* Doom — WAD parsing, level geometry, the software
renderer, the 35 Hz simulation — is pure Lean; a small C file behind a
fourteen-function FFI handles the window, GPU, keyboard, audio, and clock.

It runs the retail **Doom** and **Doom II** IWADs — plus other compatible
IWADs and PWADs — with the full bestiary and arsenal: monsters, weapons
(including Doom II's super shotgun), pickups, doors, lifts and teleporters,
sound and music, save/load, cheats, and level/episode progression, all the way
to the Icon of Sin. See [`DESIGN.md`](DESIGN.md) for the architecture.

DILL was vibe-coded using Anthropic's Fable, Opus 4.8, and Opus 5 models. DILL is guaranteed to have zero bugs because it was written in Lean. And, because DILL is written in a functional programming language, the number of bugs is further decreased.

## AI Notice

This project was entirely vibe coded using Anthropic Fable, Opus 4.8, and Opus 5.0.

## Requirements

DILL runs on **macOS (Apple Silicon)** and **Linux (x86-64 or arm64)**. The
`lakefile.lean` detects the host platform (`System.Platform.isOSX`) and picks
the right libraries and linker flags; `c/shell.c` is `#ifdef`-split so the
same source compiles on both. On macOS it links MoltenVK, AudioToolbox, and
ImageIO; on Linux it links the native Vulkan loader, SDL3, and libpng.

Common to both:

- **Lean 4.32.0** via [`elan`](https://github.com/leanprover/elan). The
  pinned toolchain in `lean-toolchain` is fetched automatically on first
  build.
- **A Doom IWAD** — the retail data file. `doom.wad` (Ultimate Doom) and
  `doom2.wad` both work, as do other compatible IWADs (e.g. Final Doom's
  `tnt.wad` / `plutonia.wad`). Place a copy in the repo root (or pass a path);
  PWADs can be layered on top with `-file` (see [Run](#run)).

### macOS

- **Xcode Command Line Tools** (`xcode-select --install`) — the linker
  needs the macOS SDK for the AudioToolbox / CoreFoundation frameworks.
- **Homebrew packages:**

  ```sh
  brew install sdl3 molten-vk vulkan-loader vulkan-headers
  ```

The macOS SDK path is resolved automatically at build configuration time
via `xcrun --show-sdk-path` (falling back to the Command Line Tools
default under `/Library/Developer/CommandLineTools/SDKs`), so no
adjustment is needed for Xcode-only or non-standard SDK installs.

### Linux

You need SDL3, the Vulkan loader + headers, libpng, and a Vulkan driver for
your GPU (Mesa on Intel/AMD, or the NVIDIA driver).

- **Debian / Ubuntu:**

  ```sh
  sudo apt install libvulkan-dev libpng-dev mesa-vulkan-drivers vulkan-tools
  # SDL3 (Ubuntu 24.10+/Debian 13+ have it packaged):
  sudo apt install libsdl3-dev
  ```

  If `libsdl3-dev` is not in your archive (older LTS releases), build SDL3
  from source — it installs to `/usr/local`, where the build expects it:

  ```sh
  sudo apt install build-essential cmake ninja-build git \
      libx11-dev libxext-dev libxrandr-dev libxcursor-dev libxfixes-dev \
      libxi-dev libxss-dev libxtst-dev libxkbcommon-dev libwayland-dev \
      wayland-protocols libdecor-0-dev libgbm-dev libasound2-dev libpulse-dev
  git clone --depth 1 --branch release-3.2.x https://github.com/libsdl-org/SDL.git
  cd SDL && cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
  cmake --build build && sudo cmake --install build && sudo ldconfig
  ```

- **Fedora:** `sudo dnf install SDL3-devel vulkan-loader-devel
  vulkan-headers libpng-devel mesa-vulkan-drivers vulkan-tools`
- **Arch:** `sudo pacman -S sdl3 vulkan-icd-loader vulkan-headers libpng
  vulkan-tools` plus your GPU's Vulkan driver (e.g. `vulkan-radeon`,
  `vulkan-intel`, or `nvidia-utils`).

The linker resolves libvulkan and libpng by scanning the standard system
library directories (Debian/Ubuntu multiarch, `lib64`, `lib`); SDL3 is
expected under `/usr/local/lib` or `/usr/lib`. If yours lives elsewhere,
adjust the Linux branch of `platformLinkArgs` in `lakefile.lean`.

> **Music note:** the soundtrack uses the OS's built-in General MIDI synth,
> which only exists on macOS. On Linux the MUS/MIDI music is silently
> skipped — **sound effects still play** (they go through SDL audio).

## Build

**macOS:**

```sh
lake build
```

**Linux:**

```sh
LEAN_CC=cc lake build
```

This compiles the Lean sources, builds `c/shell.c` into a static library,
and links the `dill` executable.

`LEAN_CC=cc` on Linux tells Lake to link with the **system** C compiler
instead of the one bundled with the Lean toolchain. It is required because
the toolchain bundles an old glibc (2.26), while a modern SDL3 is built
against your system glibc (2.3x) and needs symbols the bundled one lacks —
linking through the system compiler targets the system glibc and its loader,
so everything resolves. Set it once for your shell if you prefer:

```sh
export LEAN_CC=cc          # then plain `lake build`, `lake test`, etc.
```

(any working `cc`/`clang` will do). macOS builds do not need it.

## Run

```sh
lake exe dill doom.wad
```

You boot to the title screen; press `Enter`, `Space`, `Esc`, or fire for the
menu, then **New Game**. The window is fullscreen and renders 16:9
widescreen by default.

- `lake exe dill doom2.wad` — play Doom II (or any other IWAD).
- `lake exe dill doom.wad E1M3` — start on a specific map (`MAP07` on Doom II).
- `lake exe dill doom2.wad -file mymegawad.wad` — layer one or more PWADs over
  the IWAD.
- `lake exe dill doom.wad --classic` — present the original 4:3 picture
  (letterboxed) instead of widescreen.
- `lake exe dill doom.wad --fit` — letterbox rather than crop to cover, so
  the full 16:9 width survives on a display too narrow to fill without
  losing the edges.

### Controls

| Key | Action |
|---|---|
| `W` `A` `S` `D` | move / strafe |
| `↑` `↓` / `←` `→` | move forward-back / turn |
| mouse | turn |
| `Ctrl` or left-click | fire |
| `1`–`7` | select weapon — `1` fist/chainsaw, `2` pistol, `3` shotgun/super shotgun, `4` chaingun, `5` rocket, `6` plasma, `7` BFG (`1` and `3` toggle) |
| `Shift` | run |
| `Space` | use (doors, switches, lifts) |
| `Tab` | automap |
| `Esc` | menu (arrows + `Enter` to navigate) |
| `F5` / `F9` | quicksave / quickload (slot 1) |

An optional **`overlay.png`** in the working directory is composited over
every frame using its alpha channel (a HUD skin, scanlines, a vignette —
whatever you like). It is decoded at its native pixel size and drawn 1:1
against the display's own resolution, anchored at the top-left — never
scaled, and never through the 426×200 framebuffer, so it stays crisp.
Author it at your display's resolution.

Cheats, typed during play: `dilldqd` (god), `dillkfa` / `dillfa` (weapons &
ammo), `dillclip` or `dillspispopd` (no clipping), `dillmypos`, `dillddt`
(reveal the whole automap), and `dillclev##` to warp — the two digits read as
an episode + map on Doom and a level number on Doom II (`dillclev17` → E1M7,
or MAP17 under Doom II).

## Test

```sh
lake test              # on Linux: LEAN_CC=cc lake test (or export it once)
```

Runs the golden test suite against the real `doom.wad` in the repo root:
WAD/level/asset decoding, movement and collision, doors and specials,
combat and autoaim, saves, cheats, and renderer completeness.

## Other commands

The executable also exposes some non-graphical utilities, handy for
inspection and debugging:

```sh
lake exe dill info  doom.wad             # print the WAD lump directory
lake exe dill map   doom.wad E1M1        # print a map's stats
lake exe dill ppm   doom.wad STARTAN3 out.ppm  # dump a texture/flat/sprite
lake exe dill view  doom.wad E1M1 out.ppm      # render one frame offline
lake exe dill music doom.wad E1M1 out.mid      # dump a map's music as MIDI
lake exe dill fly   doom.wad E1M1        # free-fly camera (no physics)
lake exe dill bench doom.wad E1M1 200    # frames/sec benchmark
lake exe dill pattern                    # open a window with a test pattern
```

## Layout

```
Dill/            functional core (pure Lean)
  Wad, Level, Assets, Music      WAD parsing and decoding
  Render/                        the software renderer
  Game/                          simulation: mobjs, AI, combat, specials, save
  Ui.lean                        menus, HUD, frame composition (pure)
Dill/Shell.lean  the @[extern] FFI boundary
c/shell.c        imperative shell: SDL3 + Vulkan + AudioToolbox
Main.lean        the game loop and CLI
Tests/           golden tests: the fast suite, the WAD sweep, and the
                 vanilla-fidelity diff against transcribed linuxdoom tables
doom.wad         game data
```
