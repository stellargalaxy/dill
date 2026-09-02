import Dill.Level
import Dill.Assets

/-!
# The frame, the view, and column drawing

The renderer draws a 426×200 frame of palette indices, converted to RGBA
once at the end. Screen space: x grows right, y grows down, the eye looks
through column `centerX` at row `centerY` with a 90° horizontal FOV.

`DrawState` is the per-frame mutable state threaded through `RenderM`.
Doom's occlusion logic lives in three per-column arrays:

- `solid[x]`: the column is completely drawn; nothing behind shows.
- `ceilClip[x]` / `floorClip[x]`: rows at or above / at or below these are
  already covered. Portals (two-sided lines) narrow them as the BSP walk
  moves front to back.
-/

namespace Dill.Render

/-- The frame is rendered widescreen: 426×200 with Doom's 1.2 tall pixels
is 16:9. The classic 4:3 look is the middle 320 columns — the shell crops
at present time, so both aspects come from one renderer. -/
def screenW : Nat := 426
def screenH : Nat := 200
def centerX : Float := 213.0
def centerY : Float := 100.0
/-- π, as the renderer has always spelled it. (Not `3.141592653589793`,
the correctly rounded double: frames are byte-compared against references
rendered with this literal, so it must not change.) -/
def pi : Float := 3.14159265358979
/-- Eye-to-screen distance in pixels; unchanged from the 320-wide focal,
so the extra width widens the field of view (≈106°) instead of zooming. -/
def focal : Float := 160.0

/-! ## Fractional constants

A *decimal* float literal — anything written with a `.` or an exponent —
elaborates through `OfScientific`, and Lean's `Float.ofScientific` is
**bignum** arithmetic: GMP `mpz` allocation, division and free, run on every
evaluation rather than folded at compile time. In a per-column or per-pixel
loop that dominates the frame; a sampling profile of a dense map put
`Float.ofScientific` and the GMP calls beneath it above every drawing
routine, and a micro-benchmark makes an inline `4.0` about **twelve times**
the cost of the same value reached by name (3.1 s against 0.27 s over three
million uses).

Two ways out, both used here. A whole-number literal can simply drop its
`.0`: `4` goes through `OfNat`, which is a single cheap conversion and
measures identical to a named constant — so the hot paths below spell
whole numbers `0`, `1`, `2`, `16`, `1024`. A genuinely fractional value has
no such spelling, so it is named here instead; a top-level `def` is
evaluated once, at module initialisation.

Cold code — parsing, setup, the once-per-frame paths — keeps its decimal
literals, where they read better and cost nothing that matters. -/

/-- Half a pixel: the sample-grid offset (`rowAtOrBelow`, `colAtOrAfter`,
and every place that asks where the *centre* of pixel `i` falls). -/
def half : Float := 0.5

/-- The near clip plane, in world units of forward depth. Shared by the seg
clipper and the bounding-box cull so the two cannot disagree about what
counts as behind the eye. -/
def nearPlane : Float := 0.05

/-- x-offset that centers vanilla's 320-wide screen layout (HUD, menus)
in the wide frame. -/
def hudX (x : Int) : Int := x + (Int.ofNat screenW - 320) / 2

/-- The camera: a map position, an eye height (world z), and a facing angle
in radians counterclockwise from east — everything `render` needs to know
about the player. -/
structure View where
  x      : Float
  y      : Float
  height : Float
  angle  : Float
  /-- A screen-wide colormap override (Doom's `fixedcolormap`): row 0 for the
  light-amp goggles' fullbright, row 32 for invulnerability's inverse map.
  `none` = ordinary sector + distance lighting. -/
  fixedColormap : Option Nat := none
  /-- The current game tic, so animated flats/textures pick their frame. -/
  tics : Nat := 0
  deriving Repr, Inhabited

/-- World point → (forward depth, rightward offset) relative to the view. -/
@[inline] def View.toCamera (v : View) (wx wy : Float) : Float × Float :=
  let dx := wx - v.x
  let dy := wy - v.y
  (dx * Float.cos v.angle + dy * Float.sin v.angle,
   dx * Float.sin v.angle - dy * Float.cos v.angle)

/-- The screen row containing height `h` (relative to the eye) at inverse
depth `iz`: rows are counted down from `centerY`. -/
@[inline] def projectRow (h iz : Float) : Float :=
  centerY - focal * h * iz

/-- First pixel row whose center lies at or below screen coordinate `y`. -/
@[inline] def rowAtOrBelow (y : Float) : Int := ifloor (y + half)

/-- First pixel column whose center lies at or right of screen coordinate
`x`. The same sample-grid rule as `rowAtOrBelow` — pixel `i` is sampled at
`i + 0.5` on either axis — under the name that reads correctly when the
axis is horizontal. Walls, sprites and the bbox cull all map endpoints to
columns through this, which is what keeps their column ranges agreeing. -/
@[inline] def colAtOrAfter (x : Float) : Int := ifloor (x + half)

/-- A live thing, as the renderer needs to see it: where, which sprite
family and frame, facing which way (for the 8 view rotations). -/
structure RenderMobj where
  x      : Float
  y      : Float
  z      : Float
  angle  : Float
  sprite : String
  frame  : Char
  bright : Bool
  /-- Spectres: drawn as a dark shade. -/
  shadow : Bool := false
  /-- The body's collision radius. Only the room-visibility gate reads it:
  a wide monster's centre can sit in a sector the BSP walk never reached
  while its flank is in plain view. -/
  radius : Float := 0
  deriving Inhabited

/-- Read-only inputs for a frame. -/
structure Ctx where
  level  : Level
  assets : Assets
  view   : View
  mobjs  : Array RenderMobj := #[]

/-- The sky is a ceiling flat with a magic name. -/
def skyFlat : String := "F_SKY1"

/-- A pending horizontal surface (floor/ceiling): per-column row ranges
accumulated during the wall pass, drawn afterwards. A column is empty
while `top[x] > bottom[x]`. -/
structure Visplane where
  height : Float
  flat   : String
  light  : Nat
  minX   : Nat
  maxX   : Nat
  top    : Array Int
  bottom : Array Int
  deriving Inhabited

/-- The all-empty column arrays every fresh visplane starts from — the
`top > bottom` per-column sentinel — allocated once at module init and
shared by every `Visplane.empty`. Sharing is sound because Lean arrays are
copy-on-write: `planeSpan`'s `set!` copies a plane's columns the first time
it writes them, so the shared originals are never mutated. A fresh pair per
`findPlane` miss was two 426-entry allocations per plane, every frame. -/
def Visplane.emptyTop : Array Int := Array.replicate screenW 1
def Visplane.emptyBottom : Array Int := Array.replicate screenW 0

def Visplane.empty (height : Float) (flat : String) (light : Nat) : Visplane :=
  { height, flat, light
    minX := screenW, maxX := 0
    top := Visplane.emptyTop, bottom := Visplane.emptyBottom }

/-- Per-frame mutable drawing state.

`zbuf` holds the inverse depth (`1/z`, bigger = closer) of the wall or
flat drawn at each pixel — 0 where nothing (or sky) was drawn. Sprites and
see-through walls test against it instead of vanilla's drawseg clip lists:
less machinery, same picture. -/
structure DrawState where
  frame     : ByteArray
  zbuf      : FloatArray
  /-- World height of the flat drawn at each pixel; `noSurface` where a
  wall (or nothing) owns it. Lets sprites stand *on* their floor: vanilla
  never clips a sprite against the very ground it rests on. -/
  zfloor    : FloatArray
  solid     : Array Bool
  /-- How many columns are not yet solid; 0 = the screen is sealed and the
  BSP walk can stop. Kept in step with `solid` so the walk's check is O(1)
  instead of a 426-entry scan per node. -/
  openCols  : Nat
  ceilClip  : Array Int
  floorClip : Array Int
  planes    : Array Visplane
  /-- Sectors whose subsectors the BSP walk actually reached this frame.
  A mobj in an unvisited sector is in a room the player cannot see, so it
  is never drawn — the structural guard against sprites showing through
  walls, matching vanilla's per-subsector `R_AddSprites`. Empty means
  "not tracked", treated as all-visible. -/
  visited   : Array Bool
  /-- Vanilla's running `fuzzpos`: the `fuzzTable` cursor, advanced once
  per fuzz pixel drawn, carried across columns and sprites. -/
  fuzzPos   : Nat
  /-- Per-pixel coverage (1 = drawn), recorded by `drawPic` — but only when
  a caller has seeded a screen-sized buffer (`withFrameMask` does; see it
  for why a compositor needs coverage rather than a transparent palette
  index). Empty means "not tracked", which is what every ordinary render
  runs with, so the world pass pays nothing for this. -/
  cover     : ByteArray

/-- Sentinel for `zfloor`: not a flat surface. -/
def noSurface : Float := 1e30

/-- Vanilla's `fuzzoffset` table (`r_draw.c`): the spectre fuzz samples the
background one row up (`-1`) or down (`+1`) following this 50-entry pattern.
Doom walks it with a running `fuzzpos` that advances once per drawn pixel
and carries across columns and frames; `DrawState.fuzzPos` plays that role
within a frame — columns of different heights leave the cursor at ever-
different phases, which is what makes the fuzz look like noise rather than
a weave — and the tic clock offsets it so the pattern crawls between
frames. -/
def fuzzTable : Array Int :=
  #[ 1,-1, 1,-1, 1, 1,-1,
     1, 1,-1, 1, 1, 1,-1,
     1, 1, 1,-1,-1,-1,-1,
     1,-1,-1, 1, 1, 1, 1,-1,
     1,-1, 1, 1,-1,-1, 1, 1,
    -1,-1,-1,-1, 1, 1,-1,-1,
     1,-1, 1, 1,-1 ]

abbrev RenderM := StateM DrawState

def DrawState.init : DrawState :=
  { frame     := ByteArray.mk (Array.replicate (screenW * screenH) 0)
    zbuf      := FloatArray.mk (Array.replicate (screenW * screenH) 0.0)
    zfloor    := FloatArray.mk (Array.replicate (screenW * screenH) noSurface)
    solid     := Array.replicate screenW false
    openCols  := screenW
    ceilClip  := Array.replicate screenW (-1)
    floorClip := Array.replicate screenW (Int.ofNat screenH)
    planes    := #[]
    visited   := #[]
    fuzzPos   := 0
    cover     := ByteArray.empty }

/-! Mutation helpers: take a buffer out of the state (leaving a dummy in its
place), fill it with `set!` in a plain local loop, then put it back.

Note what this is *not* for. It is tempting to say the take is what makes
the buffer uniquely referenced so `set!` can mutate in place — but on this
toolchain `modify fun s => { s with frame := s.frame.set! i v }` already
updates in place, and measurably so: hold the number of `set!` calls fixed
and grow the array a hundredfold and the running time does not move, which
it would if either form were copying.

What the take/put pair actually buys is the *monadic* round trip. Written
the direct way, every pixel pays a `modify` — a closure call, a projection
of every `DrawState` field and a reconstruction — where hoisting the
buffer into a `mut` local pays that once per column. That is worth having in
a per-pixel loop and worth nothing anywhere else, so the rest of the state
(`ceilClip`, `floorClip`, `solid`, `planes`) is updated through `modify`
directly, and correctly so. -/

@[inline] def takeFrame : RenderM ByteArray :=
  modifyGet fun s => (s.frame, { s with frame := ByteArray.empty })

@[inline] def putFrame (b : ByteArray) : RenderM Unit :=
  modify fun s => { s with frame := b }

@[inline] def takeZ : RenderM FloatArray :=
  modifyGet fun s => (s.zbuf, { s with zbuf := FloatArray.mk #[] })

@[inline] def putZ (z : FloatArray) : RenderM Unit :=
  modify fun s => { s with zbuf := z }

@[inline] def takeZFloor : RenderM FloatArray :=
  modifyGet fun s => (s.zfloor, { s with zfloor := FloatArray.mk #[] })

@[inline] def putZFloor (z : FloatArray) : RenderM Unit :=
  modify fun s => { s with zfloor := z }

@[inline] def takeCover : RenderM ByteArray :=
  modifyGet fun s => (s.cover, { s with cover := ByteArray.empty })

@[inline] def putCover (b : ByteArray) : RenderM Unit :=
  modify fun s => { s with cover := b }

/-- Doom's light model: sector light level (0–255) picks a base colormap,
distance brightens toward it. `iz` is inverse depth; `boost` is the ±1
"fake contrast" applied to axis-aligned walls. Returns a colormap row. -/
def lightColormap (light : Nat) (iz : Float) (boost : Int := 0)
    (fixed : Option Nat := none) : Nat :=
  match fixed with
  | some c => c
  | none =>
    let lightNum : Int := Int.ofNat (min light 255 / 16) + boost
    let startMap : Int := (15 - max 0 (min 15 lightNum)) * 4
    let scaleIdx : Int := min 47 (ifloor (focal * iz * 16))
    (max 0 (min 31 (startMap - scaleIdx / 2))).toNat

/-- Mark column `x` solid, keeping `openCols` in step. Callers reach this
only for columns that are not yet solid (the wall loop `continue`s past
solid ones), so the count is simply decremented. -/
@[inline] def markSolid (x : Nat) : RenderM Unit :=
  modify fun s =>
    { s with solid := s.solid.set! x true, openCols := s.openCols - 1 }

/-- Paint one fuzz pixel (vanilla `R_DrawFuzzColumn`): the colour is the
pixel one row up or down (per `fuzzTable` at cursor `pos`), dimmed through
colormap row 6, so what's behind shows through as a wobbling shadow.
Callers walk `pos` from `DrawState.fuzzPos` (plus the tic phase), advance
it once per pixel drawn, and store it back — vanilla's running `fuzzpos`. -/
@[inline] def fuzzPixel (assets : Assets) (frame : ByteArray) (x y pos : Nat) :
    ByteArray :=
  let off := fuzzTable[pos % 50]!
  let sy := (max 0 (min (Int.ofNat screenH - 1) (Int.ofNat y + off))).toNat
  let bg := frame.get! (sy * screenW + x)
  frame.set! (y * screenW + x) (assets.colormap.get! (6 * 256 + bg.toNat))

/-- Write depth `iz` over a column span without touching colour. A closed
portal (a shut door) is a solid wall for occlusion, but its upper/lower
textures may not cover every row of the column; the uncovered rows would
otherwise keep the far depth and let a sprite in the sealed-off room show
through. This seals them. -/
def sealZColumn (iz : Float) (x : Nat) (y0 y1 : Int) : RenderM Unit := do
  if y1 < y0 then return
  let mut zbuf ← takeZ
  for y in [y0.toNat : y1.toNat + 1] do
    zbuf := zbuf.set! (y * screenW + x) iz
  putZ zbuf

/-- Draw one textured wall column at screen column `x`, rows `y0..y1`
inclusive: texture column `tcol`, texture row `v0` at `y0` advancing `dv`
per row, colors remapped through colormap row `cm`, depth `iz` recorded
for the sprite pass. -/
def drawTexColumn (assets : Assets) (tex : Picture) (tcol : Nat) (cm : Nat)
    (iz : Float) (x : Nat) (y0 y1 : Int) (v0 dv : Float) : RenderM Unit := do
  if y1 < y0 then return
  let base := tcol * tex.height
  let cmBase := cm * 256
  let mut frame ← takeFrame
  let mut zbuf ← takeZ
  let mut v := v0
  for y in [y0.toNat : y1.toNat + 1] do
    let texRow := ((ifloor v) % Int.ofNat tex.height).toNat
    let texel := tex.pixels.get! (base + texRow)
    let color := assets.colormap.get! (cmBase + texel.toNat)
    frame := frame.set! (y * screenW + x) color
    zbuf := zbuf.set! (y * screenW + x) iz
    v := v + dv
  putFrame frame
  putZ zbuf

/-- Like `drawTexColumn` for see-through surfaces (grates, sprites): skips
transparent texels, skips texture rows outside `0..height` (masked walls
don't tile vertically), and draws a pixel only when nearer than what's
there. `writeZ` is set for masked walls, clear for sprites (which are
already painted back to front). -/
def drawMaskedColumn (assets : Assets) (tex : Picture) (tcol : Nat) (cm : Nat)
    (iz : Float) (writeZ : Bool) (x : Nat) (y0 y1 : Int) (v0 dv : Float)
    (groundZ : Option Float := none) (fuzz : Bool := false)
    (fuzzPhase : Nat := 0) : RenderM Unit := do
  if y1 < y0 then return
  let base := tcol * tex.height
  let cmBase := cm * 256
  let mut fpos := (← get).fuzzPos
  let mut frame ← takeFrame
  let mut zbuf ← takeZ
  -- read-only: this pass never writes `zfloor`, so it is read straight out
  -- of the state once instead of being taken and put back. The take/put pair
  -- exists to hoist the per-pixel `modify` out of the loop (see the note
  -- above); a buffer only ever read has no `modify` to hoist.
  let zfloor := (← get).zfloor
  let mut v := v0
  for y in [y0.toNat : y1.toNat + 1] do
    let vi := ifloor v
    if 0 ≤ vi && vi < Int.ofNat tex.height then
      let texRow := vi.toNat
      -- Nearer surfaces win — except the flat the sprite stands on: its
      -- own ground never hides it (bottles sit on the floor, not in it).
      -- Caveat: `zfloor` records only the surface *height*, not which
      -- sector drew it, so any *floor* within 1.0 world units of the feet
      -- passes — including a different, nearer floor that happens to sit
      -- at the same height. Vanilla's drawseg clipping would catch that
      -- case only when a step (a lower texture) separates the two floors;
      -- at equal heights there is no step, so vanilla shows the sprite
      -- too, and the tolerance (1.0 unit, covering float error in saved/
      -- restored heights) is close enough for the rest. Ceilings never
      -- enter `zfloor` (see `drawSpan`): an overhang's underside commonly
      -- sits exactly at a neighbouring ledge's floor height, and a sprite
      -- on that ledge must not shine through the lip.
      let visible := iz ≥ zbuf.get! (y * screenW + x)
        || (match groundZ with
            | some gz => Float.abs (zfloor.get! (y * screenW + x) - gz) < 1
            | none => false)
      if tex.mask.get! (base + texRow) != 0 && visible then
        if fuzz then
          -- Spectres: the sprite is only a mask; `fuzzPixel` paints the
          -- darkened background so the wall shows through as a wobbling
          -- shadow rather than a solid silhouette.
          frame := fuzzPixel assets frame x y (fpos + fuzzPhase)
          fpos := fpos + 1
        else
          let texel := tex.pixels.get! (base + texRow)
          frame := frame.set! (y * screenW + x)
            (assets.colormap.get! (cmBase + texel.toNat))
          if writeZ then
            zbuf := zbuf.set! (y * screenW + x) iz
    v := v + dv
  putFrame frame
  putZ zbuf
  if fuzz then
    let fposNow := fpos
    modify fun s => { s with fuzzPos := fposNow }

/-- Palette-index frame → RGBA bytes for the shell. `pal` selects one of
PLAYPAL's 14 palettes: 0 normal, 1–8 pain reds, 9–12 bonus golds. -/
def toRGBA (assets : Assets) (frame : ByteArray) (pal : Nat := 0) :
    ByteArray := Id.run do
  let base := min pal 13 * 768
  -- preallocated and written with `set!` like every other buffer here:
  -- `push` would re-check capacity four times per pixel
  let mut out := ByteArray.mk (Array.replicate (screenW * screenH * 4) 0)
  for i in [0 : screenW * screenH] do
    let c := base + (frame.get! i).toNat * 3
    let o := i * 4
    out := out.set! o (assets.palettes.get! c)
    out := out.set! (o + 1) (assets.palettes.get! (c + 1))
    out := out.set! (o + 2) (assets.palettes.get! (c + 2))
    out := out.set! (o + 3) 255
  return out

end Dill.Render
