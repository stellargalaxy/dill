# Dill — Doom in Idiomatic Lean 4

Dill is an implementation of Doom (1993) in Lean 4, rendered through Vulkan.
It is built as a **functional core with an imperative shell**: everything that
*is* Doom — WAD parsing, level geometry, the renderer, the simulation — is pure
Lean; everything that touches the operating system — the window, the GPU, the
keyboard, the clock, the speaker — lives behind a fourteen-function C
interface.

The style throughout favors **readability and simplicity** over cleverness or
vanilla-Doom bit-exactness.

## Scope

A playable game across the retail Doom and Doom II IWADs — every episode of
Ultimate Doom and MAP01–MAP32 of Doom II, up to the Icon of Sin — plus
compatible IWADs and PWADs layered on with `-file`:

- Load a retail IWAD (`doom.wad`, `doom2.wad`, …); render maps faithfully with the classic
  software renderer: textured walls, flats, sky, sprites with 8 view
  rotations, sector lighting with distance diminishing.
- Move with momentum physics, collision, stairs, gravity; doors, lifts,
  walk-over triggers, the exit switch.
- Fight: the full bestiary — zombiemen, shotgun guys, chaingunners, imps,
  demons, spectres (shadow-drawn), cacodemons, lost souls and pain
  elementals (flying), Hell Knights and Barons, revenants (with homing
  tracers), mancubi, arachnotrons, arch-viles (which raise the dead), the
  Wolfenstein SS, Commander Keen, the Cyberdemon and Spider Mastermind, and
  the Icon of Sin's spawn cubes — vanilla state machines, `A_Chase` AI,
  line-of-sight wake-ups, noise alerts, pain chances, infighting, corpses,
  ammo drops, the fireball types, and the lost soul's charge.
- Arsenal: all nine weapons — fist, chainsaw, pistol, shotgun, super
  shotgun, chaingun, rocket launcher, plasma rifle, BFG9000 — with vanilla
  timings, spreads, and damage dice; hitscan autoaim; player-launched
  projectiles (rockets
  with splash, plasma bolts, the BFG ball + its spray fan); four ammo
  pools (bullets, shells, rockets, cells) with a capacity-doubling
  backpack; exploding barrels.
- Pickups (health, armor, all ammo and weapons, backpack, keys), a
  fullscreen HUD (weapon sprite with bob and muzzle flash, red status
  digits, and the animated marine face — health brackets, idle glances,
  ouch/pain, evil grin, god and dead faces), pain/bonus palette tints,
  death and restart.
- An optional `overlay.png` in the working directory, composited over
  every frame through its alpha channel **at the display's native
  resolution** — the shell decodes it (macOS ImageIO, libpng on Linux) at
  its own pixel size and software-composites it 1:1 over the upscaled game
  each present, anchored top-left and never scaled, so it does not pass
  through the 426×200 framebuffer and stays crisp. The game framebuffer is
  unchanged; ~5 ms/frame CPU at 2880×1800.

Also in: sound effects (weapons, monsters, doors, pickups — the WAD's DMX
lumps through SDL audio with distance attenuation), music (the MUS lumps
converted to MIDI in pure Lean — `Dill/Music.lean` — and played by the
OS's General MIDI synth, looping), sector light effects (blink/strobe/
glow), damaging nukage floors, and episode progression — each exit loads
the next map (secret exits detour via ExM9), carrying your vitals and
arsenal.

Also: stair builders (specials 7/8) and teleporters (39/97 for anything that
walks, 125/126 for monsters only — crossed from the front, as vanilla's
`EV_Teleport` insists, so you can step back off a landing pad), so the deeper
episode maps play; a title screen and menus (TITLEPIC, M_DOOM, the skull
cursor, the STCFN menu font): New Game, four save/load slots on disk
(`Dill/Game/Save.lean` — a readable text snapshot of the entire game
state, floats as 16.16 fixed-point, deterministic on reload), Quit.
Fullscreen by default.

The frame is rendered widescreen — 426×200, which is 16:9 under Doom's
1.2-tall pixels — with the same focal length as vanilla, so the extra
width is extra field of view. It *covers* the display (filling it edge to
edge, cropping the sliver the display's exact aspect can't fit — no black
bars). `--classic` letterboxes the middle 320 columns instead:
pixel-identical to the original 4:3 picture.

Hitscan aims like vanilla (`P_AimLineAttack`): a ±0.625 slope window is
narrowed through every wall opening along the ray, the first target that
fits sets the shot's slope — so bullets climb staircases — and only then
do walls stop the shot, at its actual height.

Between maps: the intermission tally (WIMAP0, level-name graphics,
kills/items/secrets percentages, D_INTER music). Kills, items, and secret
sectors are counted vanilla-style (lost souls don't count; a secret is
scored the first time you step in it).

Out of scope: demo playback.

Controls: WASD/arrows move, mouse turns, Ctrl/left-click fires, 1–7 select
weapons (1 toggles fist/chainsaw, 3 toggles shotgun/super shotgun), Shift
runs, Space uses, Tab shows the automap, Esc opens the menu (arrows +
Enter drive it), F5/F9 quicksave/quickload (slot 1).

Cheats (typed during play, confirmed by an on-screen message): `dilldqd`
(god), `dillkfa` / `dillfa` (arsenal, with/without keys), `dillclip` or
`dillspispopd` (no clipping), `dillmypos`, `dillddt` (reveal the whole
automap — vanilla kept this separate from the arsenal cheat, and so does
DILL), and `dillclev`⟨e⟩⟨m⟩ — e.g. `dillclev17` warps to E1M7 at a fresh
pistol start. `Dill/Game/Cheats.lean` holds the pure scanner and effects;
the shell only feeds typed keys.

## Architecture

```
┌────────────────────────── functional core (pure) ──────────────────────────┐
│                                                                             │
│  doom.wad bytes ──▶ Wad ──▶ Level (geometry, BSP)     Input ──┐             │
│                        └──▶ Assets (palette, textures)        ▼             │
│                                                        tick : Input →       │
│                                                          GameState →        │
│                                                          GameState          │
│                                                               │             │
│  render : Assets → Level → ViewState → Frame (426×200 RGBA) ◀─┘             │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
        ▲                                                        │
        │ ByteArray (file read)                                  │ ByteArray (frame)
┌───────┴───────────────── imperative shell (IO + C) ────────────▼───────────┐
│  Main.lean: the game loop                                                   │
│  c/shell.c: SDL3 window/input + Vulkan swapchain/blit  (14 functions)       │
└─────────────────────────────────────────────────────────────────────────────┘
```

The shell knows nothing about Doom; the core knows nothing about the OS. The
boundary types are deliberately primitive: `ByteArray` in (file contents),
`ByteArray` out (RGBA frame), `UInt64` in (input snapshot).

### Functional core

| Module | Responsibility |
|---|---|
| `Dill/Bytes.lean` | Little-endian readers over `ByteArray` (u16/i16/u32, 8-byte names) |
| `Dill/Wad.lean` | WAD header + lump directory; lookup by name; pure `Except` parsing |
| `Dill/Level.lean` | Map lumps → typed arrays: vertexes, linedefs, sidedefs, sectors, segs, subsectors, BSP nodes, things, blockmap |
| `Dill/Assets.lean` | `PLAYPAL`, `COLORMAP`, composited wall textures, flats, sprites |
| `Dill/Render/` | The software renderer (below) |
| `Dill/Game/` | `GameState`, 35 Hz `tick`, movement, collision, doors/lifts |
| `Dill/Game/Info.lean` | distilled `info.c`: actor physiques + state tables |
| `Dill/Game/Mobj.lean` | live map objects; spawning; state stepping |
| `Dill/Game/Enemy.lean` | `A_Look`/`A_Chase`/`P_NewChaseDir`, monster attacks |
| `Dill/Game/Combat.lean` | sight checks, hitscan, missiles, radius damage |
| `Dill/Game/Player.lean` | weapon state machines, trigger logic, pickups |
| `Dill/Game/Random.lean` | deterministic byte RNG threaded through `GameState` |
| `Dill/Ui.lean` | the menus, the HUD assembly, and frame composition — pure |

**The renderer** follows Doom's original algorithm, front to back:

1. `Bsp.lean` — walk the BSP tree from the view point; visit subsectors in
   front-to-back order; back-face-cull each seg.
2. `Walls.lean` — project each seg to screen columns; clip against the
   *solidsegs* horizontal occlusion list; draw textured columns (upper / lower
   / middle, with Doom's texture-pegging rules); record *drawsegs* for sprite
   clipping and clip ranges feeding the visplanes.
3. `Planes.lean` — floors and ceilings accumulate into *visplanes* during the
   wall pass, then render as constant-z horizontal spans of flat texture.
4. `Things.lean` — sprites sorted by depth, drawn as masked columns.
   Occlusion against walls uses a per-pixel z-buffer written by the wall
   and flat passes — simpler than vanilla's drawseg clip lists, same
   picture. A parallel buffer records each flat pixel's surface height so
   a sprite is never clipped by the very floor it stands on (vanilla
   sprites always win against their own ground). See-through middle
   textures (grates) are deferred jobs drawn after the flats, also
   z-tested.

Lighting is authentic: sector light level plus distance diminishing, applied
by indexing `COLORMAP` before the palette lookup.

The frame is drawn as 426×200 palette indices, then mapped through `PLAYPAL`
to RGBA once per frame.

**Two deliberate departures from vanilla Doom:**

- **`Float`, not 16.16 fixed-point.** Doom used fixed-point and binary-angle
  lookup tables because 1993 CPUs had no fast FPU. We keep the *algorithms*
  (BSP, visplanes, clipping) and drop the *arithmetic*: plain `Float` radians
  and `Float.tan` read better than `finetangent` tables. Consequence: output
  is not bit-identical to vanilla, and demo playback compatibility is off the
  table. Visual fidelity is the bar.
- **Pure functions, local mutation.** Hot loops use `Id.run do` with `mut`
  locals and `ByteArray.set!` — pure at the interface, in-place inside (Lean's
  functional-but-in-place style). No `IO` anywhere in the core.

**The simulation** is a single pure step function:

```lean
tick : Input → GameState → GameState   -- exactly 35 Hz, like the original
```

`GameState` holds the player (position, angle, z, momentum) and the active
sector movers (doors and lifts in motion). Collision uses the WAD's blockmap:
slide along walls, step up ≤ 24 units, respect ceiling clearance, fall with
gravity. Because `tick` is pure, gameplay is testable by folding a scripted
list of `Input`s and asserting the path the player takes.

**The menus** get the same treatment (`Dill/Ui.lean`):

```lean
Ui.step : Session → Input → String → Ui → GameState → Ui × GameState × Array UiEffect
composeFrame : Session → Ui → GameState → ByteArray
```

`Ui.step` is to the UI what `tick` is to the game — the typed letters come in
as a `String` (so cheat scanning is pure too), and everything needing IO
leaves as a `UiEffect` for `Main.lean` to perform: read a slot, start a map,
cue the music, quit. `composeFrame` then paints the world with the menu or
the tally over it. Both are pure, so a menu path and a composed frame can be
asserted in `Tests.lean` without opening a window.

### Imperative shell

`c/shell.c` exposes exactly fourteen functions to Lean (via `@[extern]`
declarations in `Dill/Shell.lean`):

| C function | Lean signature | Job |
|---|---|---|
| `dill_init` | `UInt32 → UInt32 → String → IO Unit` | SDL3 window; Vulkan instance/device/swapchain (MoltenVK via `VK_KHR_portability_enumeration`); staging buffer |
| `dill_present` | `@&ByteArray → IO Unit` | present the 426×200 frame: `vkCmdBlitImage` to the swapchain (aspect-scaled), or, when an overlay is active, software-composite at native res and copy to the swapchain |
| `dill_poll` | `IO UInt64` | pump SDL events → input snapshot |
| `dill_typed` | `IO UInt64` | letters/digits typed since last poll (cheat codes) |
| `dill_ticks` | `IO UInt32` | milliseconds since init, for the 35 Hz accumulator |
| `dill_shutdown` | `IO Unit` | teardown |
| `dill_sound_load` | `UInt32 → UInt32 → @&ByteArray → IO Unit` | stash a raw 8-bit mono clip in a mixer slot |
| `dill_sound_play` | `UInt32 → Float → IO Unit` | play a slot at a gain; 8 channels mixed by SDL |
| `dill_music_play` | `@&ByteArray → IO Unit` | loop a Standard MIDI File on the system GM synth (AudioToolbox) |
| `dill_music_stop` | `IO Unit` | silence the music |
| `dill_aspect` | `UInt32 → IO Unit` | present full 16:9 or the classic 4:3 crop |
| `dill_fit` | `UInt32 → IO Unit` | cover the display (crop to fill) or letterbox to keep the full 16:9 width |
| `dill_set_overlay` | `@&String → IO UInt32` | load a PNG (macOS ImageIO) to composite at native resolution over every frame |
| `dill_decode_png` | `@&String → UInt32 → UInt32 → IO (Option ByteArray)` | decode a PNG to an RGBA buffer at a given size (e.g. the optional `dill_logo.png` title) |

**Sound** stays functional-core-friendly: the simulation appends
`(sfx, x, y)` events to `GameState.sounds`; the shell drains them each
frame, attenuates by distance to the player, and feeds the mixer.

**Input** crosses the FFI as one packed `UInt64`: a held-keys bitmask (WASD /
arrows move, Shift run, Space use, Esc pause/menu) plus a window-close quit
flag in the low bits, and the mouse-x delta since last poll (SDL
relative-mouse mode) as a signed 16-bit field in the high bits. The core
decodes it with a pure `Input.decode : UInt64 → Input`.

**Timing:** `Main.lean` polls input and renders every frame, but steps the
simulation on a fixed 35 Hz accumulator — Doom's tic rate — so movement speed
is framerate-independent.

**Why Vulkan for a software renderer?** The GPU's only job is getting the
frame on screen — upload and blit, ~15 API calls, no shaders, no descriptor
sets. That keeps the FFI surface small and stable while still being a real
Vulkan swapchain (MoltenVK on macOS).

## Build

- Lean 4.32.0 (pinned in `lean-toolchain`), Lake build.
- `lakefile.lean` compiles `c/shell.c` into an `extern_lib` and links SDL3 +
  the Vulkan loader. It detects the host with `System.Platform.isOSX`: on
  macOS it pulls those from Homebrew and adds the AudioToolbox / ImageIO /
  CoreGraphics frameworks; on Linux it links the native Vulkan loader and
  libpng (used for PNG decode in place of ImageIO). `c/shell.c` is
  correspondingly `#ifdef __APPLE__`-split. See the README for per-platform
  packages and the Linux `LEAN_CC=cc` requirement.
- `lake exe dill doom.wad [MAP]` runs the game. Other subcommands:
  `info` (WAD directory), `map` (level stats), `ppm` (dump a texture/flat/
  sprite as an image), `view` (render one frame offline to a PPM — the
  renderer is pure, so frames are reproducible test artifacts), `music`
  (dump a map's soundtrack as MIDI), `fly` (free camera, no physics),
  `bench` (frames/sec), `pattern` (shell smoke test).
- `lake test` runs golden tests against the real WAD: lump structure,
  level and asset decoding, movement physics, and door/lift/exit behavior,
  all asserted against known E1M1 geometry.
- macOS note: Lean normally runs `main` on a worker thread; Cocoa windowing
  requires the process main thread, so `c/shell.c` sets
  `LEAN_MAIN_USE_THREAD=0` in a constructor before Lean's entry point reads
  it.

## Milestones

1. **WAD reader** — `dill info` prints the lump directory; golden tests.
2. **Level + assets** — E1M1 and textures decode; verified by PPM dumps.
3. **Shell** — test pattern in a window through SDL3 + Vulkan.
4. **Walls** — BSP + textured columns; free-look camera around E1M1.
5. **Planes + lighting** — visplanes, flats, COLORMAP.
6. **Movement** — physics + collision; walk the level.
7. **Sprites + masked walls** — things render; grates draw correctly.
8. **Doors + lifts** — E1M1 fully walkable. *Playable slice complete.*

## Conventions

- Readability first: small modules, descriptive names, doc comments on every
  public definition. Comments explain *Doom's rules* (why 24 units, what a
  visplane is), not the Lean syntax.
- Parsing returns `Except String α` with messages that name the lump and
  offset; no panics on malformed data. `IO` appears only in `Main.lean` and
  `Dill/Shell.lean`.
- Data is held in `Array`/`ByteArray` (contiguous, cache-friendly), indexed by
  the WAD's own integer indices.
