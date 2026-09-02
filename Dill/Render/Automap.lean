import Dill.Render.Frame

/-!
# The automap

Tab drops you into an overhead line drawing of the level: one-sided walls in
red, height-change ledges in gold and grey, the player a green arrow at the
centre. Only linedefs the player has walked past are shown, unless a computer
area-map has been collected.

The lines are drawn with **Xiaolin Wu's antialiased algorithm** — each pixel a
coverage-weighted blend over the black background, so the diagonal walls read
as smooth strokes instead of jagged stair-steps.
-/

namespace Dill.Render

/-- What the automap needs to know about the world it's drawing. -/
structure MapView where
  x     : Float           -- player position (map is centred here)
  y     : Float
  angle : Float
  /-- Pixels-per-map-unit zoom (larger is more zoomed in, showing less of
  the level). -/
  scale : Float := 0.22
  /-- Reveal every linedef (a computer area-map was picked up). -/
  revealAll : Bool := false
  deriving Inhabited

/-- The player arrow's barbs: the forward direction rotated ±150°, and how
far back from the tip they reach. Named (and the trigonometry done once)
for the reason given under "Fractional constants" in `Render/Frame.lean` —
a decimal literal is bignum arithmetic on every evaluation. -/
private def barbCos : Float := Float.cos 2.61799
private def barbSin : Float := Float.sin 2.61799
private def barbLength : Float := 0.6

private def ipart (x : Float) : Int := ifloor x
/-- The fractional part. Straight through `Float.floor` rather than out to
`Int` and back: `Float.ofInt` is on the same bignum path as `Float.ofNat`
(see `blend`), and the round trip also loses its footing on values too large
for the `UInt64` conversion inside `ifloor`. -/
private def fpart (x : Float) : Float := x - x.floor
private def rfpart (x : Float) : Float := 1 - fpart x
private def round (x : Float) : Int := ifloor (x + half)

/-- Coverage is quantised onto 0…256 so the channel mix can be integer;
named so the conversion factor is not itself a per-pixel literal. -/
private def covScale : Float := 256

/-- Blend colour `(r,g,b)` at coverage `a` over the pixel already in `buf`.

Coverage crosses into integer arithmetic exactly once, here, and the three
channels are then mixed as `Nat`s. `Float.ofNat` on a *runtime* value is not
the register move it looks like — it goes by way of `Float.ofScientific`,
which is bignum, and measures around 45× a plain `Nat` add (285 ms against
6 ms over three million conversions). Mixing per channel in floats spent six
of those on every pixel, which made the antialiasing — not the line walking
— the automap's dominant cost. -/
private def blend (buf : ByteArray) (x y : Int) (r g b : UInt8) (a : Float) :
    ByteArray :=
  if a ≤ 0 || x < 0 || y < 0 || x ≥ Int.ofNat screenW || y ≥ Int.ofNat screenH then
    buf
  else Id.run do
    let cov := min 256 (max 0 (ifloor (a * covScale))).toNat
    let inv := 256 - cov
    let i := (y.toNat * screenW + x.toNat) * 4
    -- `old * inv + new * cov` peaks at 255 * 256, so the shift lands back in
    -- range; the `min` is there for the reader, not for the arithmetic
    let over := fun (old new : UInt8) =>
      UInt8.ofNat (min 255 ((old.toNat * inv + new.toNat * cov) / 256))
    let mut buf := buf
    buf := buf.set! i     (over buf[i]!     r)
    buf := buf.set! (i+1) (over buf[i+1]!   g)
    buf := buf.set! (i+2) (over buf[i+2]!   b)
    buf := buf.set! (i+3) 255
    return buf

/-- Draw one antialiased line into `buf` (Xiaolin Wu). Coverage on the two
pixels straddling the true line is proportional to how close the line passes
to each, giving smooth edges without a shader. -/
def wuLine (buf : ByteArray) (x0 y0 x1 y1 : Float) (r g b : UInt8) :
    ByteArray := Id.run do
  let mut buf := buf
  let steep := Float.abs (y1 - y0) > Float.abs (x1 - x0)
  -- work in a space where the line is shallow, un-swapping at plot time
  let (x0, y0, x1, y1) := if steep then (y0, x0, y1, x1) else (x0, y0, x1, y1)
  let (x0, y0, x1, y1) := if x0 > x1 then (x1, y1, x0, y0) else (x0, y0, x1, y1)
  -- Early out when the line lies wholly off-screen on the *perpendicular*
  -- axis. The driven-axis walk below clamps its own range, but a non-steep
  -- line 500 px above the screen still walked its full column range with
  -- every `blend` bounds-rejected. Everything plotted sits within a pixel
  -- of the segment's own y-range (|grad| ≤ 1 in the swapped space, so the
  -- endpoint extrapolations stray at most half a step), so a 2-pixel margin
  -- is conservative; `blend`'s per-pixel clipping makes skipping the walk
  -- byte-identical to running it.
  let yLimitF := Float.ofNat (if steep then screenW else screenH)
  if max y0 y1 < -2 || min y0 y1 > yLimitF + 2 then return buf
  let dx := x1 - x0
  let dy := y1 - y0
  let grad := if dx == 0 then 1 else dy / dx
  -- a pixel plot that undoes the steep swap
  let plot := fun (buf : ByteArray) (px py : Int) (a : Float) =>
    if steep then blend buf py px r g b a else blend buf px py r g b a
  -- first endpoint
  let xend := round x0
  let yend := y0 + grad * (Float.ofInt xend - x0)
  let xgap := rfpart (x0 + half)
  let ypxl1 := ipart yend
  buf := plot buf xend ypxl1       (rfpart yend * xgap)
  buf := plot buf xend (ypxl1 + 1) (fpart yend * xgap)
  let mut intery := yend + grad
  -- second endpoint
  let xend2 := round x1
  let yend2 := y1 + grad * (Float.ofInt xend2 - x1)
  let xgap2 := fpart (x1 + half)
  let ypxl2 := ipart yend2
  buf := plot buf xend2 ypxl2       (rfpart yend2 * xgap2)
  buf := plot buf xend2 (ypxl2 + 1) (fpart yend2 * xgap2)
  -- The span between. The walk stays in `Int`: a level is far wider than the
  -- screen, so `xend` is routinely negative, and clamping it through `toNat`
  -- would restart the loop at 0 while `intery` still held the y-value from
  -- the true (off-screen) endpoint — drawing the line in at the wrong height
  -- instead of clipping it away. Skipping straight to the first on-screen
  -- column keeps `intery` in step.
  -- in the swapped (steep) space the walked axis is the screen's y
  let xLimit := Int.ofNat (if steep then screenH else screenW)
  let mut xi := xend + 1
  if xi < 0 then
    intery := intery + grad * Float.ofInt (-xi)
    xi := 0
  while xi < xend2 && xi < xLimit do
    buf := plot buf xi (ipart intery)       (rfpart intery)
    buf := plot buf xi (ipart intery + 1)   (fpart intery)
    intery := intery + grad
    xi := xi + 1
  return buf

/-- Colour a linedef by what it separates (vanilla's automap palette). -/
private def lineColor (lvl : Level) (line : Linedef) : UInt8 × UInt8 × UInt8 :=
  match line.back with
  | none => (0xD8, 0x00, 0x00)        -- one-sided wall: red
  | some back =>
    let f := lvl.sectors[lvl.sidedefs[line.front]!.sector]!
    let b := lvl.sectors[lvl.sidedefs[back]!.sector]!
    if f.floorH != b.floorH then (0xC0, 0x80, 0x30)   -- floor ledge: gold
    else if f.ceilH != b.ceilH then (0x88, 0x88, 0x88) -- ceiling step: grey
    else (0x44, 0x44, 0x44)                            -- passable: dim grey

/-- Render the whole automap to an RGBA frame. With `over` supplied — an RGBA frame the size of the
screen — the lines are drawn straight onto it, so the map reads as an
overlay on the live view; without it they go on opaque black, which is
what vanilla does. -/
def automap (lvl : Level) (mv : MapView) (seen : Array Bool)
    (over : Option ByteArray := none) : ByteArray :=
  Id.run do
    let mut buf := match over with
      | some base => base
      | none => Id.run do
        let mut b := ByteArray.mk (Array.replicate (screenW * screenH * 4) 0)
        -- opaque black background
        for p in [0 : screenW * screenH] do
          b := b.set! (p * 4 + 3) 255
        return b
    let cx := Float.ofNat screenW / 2
    let cy := Float.ofNat screenH / 2
    -- world → screen: centre on the player, north up (world +y is up)
    let toScreen := fun (wx wy : Float) =>
      (cx + (wx - mv.x) * mv.scale, cy - (wy - mv.y) * mv.scale)
    -- An empty `seen` means "not tracked", and is treated as fully revealed —
    -- what `GameState.seen` and `Save.lean` both promise ("a loaded game shows
    -- the map revealed"). A save carries no trail, and `markSeen` will not
    -- start one for an array whose size does not match the linedef count, so
    -- reading each index against a default of `false` instead left the automap
    -- permanently blank for the rest of any loaded game. Decided once, outside
    -- the loop, as `roomVisible` does for the sprite pass's `visited`.
    let untracked := seen.isEmpty
    for i in [0 : lvl.linedefs.size] do
      let line := lvl.linedefs[i]!
      if !mv.revealAll && !untracked && !(seen[i]?.getD false) then continue
      let p1 := lvl.vertexes[line.v1]!
      let p2 := lvl.vertexes[line.v2]!
      let (sx0, sy0) := toScreen p1.x p1.y
      let (sx1, sy1) := toScreen p2.x p2.y
      let (r, g, b) := lineColor lvl line
      buf := wuLine buf sx0 sy0 sx1 sy1 r g b
    -- the player arrow: a shaft with two barbs, pointing where they face
    let dx := Float.cos mv.angle
    let dy := Float.sin mv.angle
    let arrow := fun (buf : ByteArray) (ax ay bx by_ : Float) =>
      wuLine buf (cx + ax) (cy - ay) (cx + bx) (cy - by_) 0x40 0xF0 0x40
    let l := 11
    buf := arrow buf (-dx * l) (-dy * l) (dx * l) (dy * l)   -- shaft
    -- barbs: rotate the forward dir by ±150° and draw back from the tip
    let barb := fun (buf : ByteArray) (s : Float) =>
      let bx := dx * barbCos - dy * (s * barbSin)
      let by_ := dx * (s * barbSin) + dy * barbCos
      arrow buf (dx * l) (dy * l)
        (dx * l + bx * (l * barbLength)) (dy * l + by_ * (l * barbLength))
    buf := barb buf 1
    buf := barb buf (-1)
    return buf

/-- Paint a palette-index HUD frame over an RGBA base (used to keep the
status readout on top of the automap). `mask` is the coverage the HUD bake
produced (`withFrameMask`, 1 = drawn): a pixel is composited exactly when
the HUD drew it. Treating palette index 0 as transparent instead dropped
every genuinely drawn index-0 texel — the black outlines of the STTNUM
digits and STCFN glyphs — and let the map lines show through them. -/
def compositeHud (assets : Assets) (base hud mask : ByteArray) : ByteArray :=
  Id.run do
    let mut out := base
    let pal := assets.palette
    for p in [0 : screenW * screenH] do
      if mask[p]! != 0 then
        let c := (hud[p]!).toNat * 3
        let o := p * 4
        out := out.set! o     (pal.get! c)
        out := out.set! (o+1) (pal.get! (c+1))
        out := out.set! (o+2) (pal.get! (c+2))
        out := out.set! (o+3) 255
    return out

end Dill.Render
