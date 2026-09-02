import Dill.Maps
import Dill.Level
import Dill.Render.Frame

/-!
# Visplanes: floors and ceilings

Walls can't draw the horizontal surfaces — a floor's screen extent isn't
known until every wall in front of it has been drawn. So during the wall
pass each column contributes its visible floor/ceiling rows to a *visplane*:
a (height, flat, light) surface with a per-column row range. After the BSP
walk, each visplane is carved into horizontal spans (Doom's `R_MakeSpans`)
and the spans are texture-mapped in world space, where a constant-z surface
has constant depth per row.

The sky is a visplane too — a ceiling whose flat is `F_SKY1` — but it's
drawn as *columns* of the episode's sky texture, panned by view angle,
always at full brightness.
-/

namespace Dill.Render

/-- Every sky wall/ceiling joins one logical sky plane. -/
private def planeKey (height : Float) (flat : String) (light : Nat) :
    Float × Nat :=
  if flat == skyFlat then (0, 0) else (height, light)

/-- Are columns `x1..x2` of `p` all still unclaimed?

Only the overlap with the plane's own span is examined — vanilla
`R_CheckPlane` narrows to `intrl`…`intrh` the same way. Outside `minX…maxX`
every column is empty by construction: `planeSpan` is the only writer, and
it widens the span on every write, so a column it never touched still holds
the `top > bottom` sentinel `Visplane.empty` laid down. A plane that has not
reached this stretch of the screen is therefore answered without reading its
per-column arrays at all, which is the common case — this runs for every
candidate plane of every seg, and a frame carries hundreds of planes. -/
private def columnsFree (p : Visplane) (x1 x2 : Nat) : Bool := Id.run do
  -- parenthesized: application binds tighter than `+`, so the bound has to
  -- be spelled out to mean `(min x2 p.maxX) + 1` beyond doubt
  for x in [max x1 p.minX : (min x2 p.maxX) + 1] do
    if p.top[x]! ≤ p.bottom[x]! then return false
  return true

/-- Find (or create) a visplane for this surface that has room for columns
`x1..x2` — Doom's `R_FindPlane` + `R_CheckPlane`. -/
def findPlane (height : Float) (flat : String) (light : Nat) (x1 x2 : Nat) :
    RenderM Nat := do
  let (height, light) := planeKey height flat light
  let s ← get
  for i in [0 : s.planes.size] do
    let p := s.planes[i]!
    if p.height == height && p.flat == flat && p.light == light
        && columnsFree p x1 x2 then
      return i
  -- Take the new plane's index *before* the push: reading `s` afterwards
  -- would keep a second reference to the planes array alive across the
  -- `modify` and force the push to copy it.
  let idx := s.planes.size
  modify fun s =>
    { s with planes := s.planes.push (Visplane.empty height flat light) }
  return idx

/-- Record rows `a..b` of column `x` as belonging to visplane `i`. -/
def planeSpan (i x : Nat) (a b : Int) : RenderM Unit := do
  if a > b then return
  modify fun s =>
    { s with planes := s.planes.modify i fun p =>
        { p with top := p.top.set! x a, bottom := p.bottom.set! x b
                 minX := min p.minX x, maxX := max p.maxX x } }

/-- Texture-map one horizontal span of a flat: row `y`, columns `x1..x2`,
surface `dz` above the eye (negative for floors). `cosA`/`sinA` are the
cos/sin of the frame-constant view angle, computed once in `drawPlanes`
rather than per span. -/
private def drawSpan (assets : Assets) (view : View) (cosA sinA : Float)
    (flat : ByteArray)
    (light : Nat) (dz : Float) (y : Nat) (x1 x2 : Nat) : RenderM Unit := do
  -- yoff is a half-integer, never 0: the horizon falls between rows 99
  -- and 100.
  let yoff := Float.ofNat y + half - centerY
  let f := -focal * dz / yoff
  if f ≤ 0 then return
  let cm := lightColormap light (1 / f) (fixed := view.fixedColormap)
  let cmBase := cm * 256
  let dr := f / focal
  let r0 := (Float.ofNat x1 + half - centerX) * dr
  let mut wx := view.x + cosA * f + sinA * r0
  let mut wy := view.y + sinA * f - cosA * r0
  let iz := 1 / f
  let surfaceZ := dz + view.height
  -- Only a floor — a flat below the eye, `dz < 0` — can be ground a sprite
  -- stands on. A *ceiling* at the same height as a distant ledge floor is
  -- the usual overhang construction (the underside of a lip is cut at the
  -- ledge's own floor height, e.g. MAP05's corridor lip over the imp
  -- closets), and it must keep clipping sprites like any other nearer
  -- surface, so it stays out of `zfloor`.
  let isFloor := dz < 0
  let mut frame ← takeFrame
  let mut zbuf ← takeZ
  let mut zfloor ← takeZFloor
  for x in [x1 : x2 + 1] do
    let ix := ((ifloor wx) % 64).toNat
    let iy := ((ifloor (-wy)) % 64).toNat
    let texel := flat.get! (iy * 64 + ix)
    frame := frame.set! (y * screenW + x) (assets.colormap.get! (cmBase + texel.toNat))
    zbuf := zbuf.set! (y * screenW + x) iz
    if isFloor then
      zfloor := zfloor.set! (y * screenW + x) surfaceZ
    wx := wx + sinA * dr
    wy := wy - cosA * dr
  putFrame frame
  putZ zbuf
  putZFloor zfloor

/-- Draw a sky column: the sky texture pans with the view angle and never
dims with distance. Vanilla maps the full circle to 1024 sky columns
(`angle = (viewangle + xtoviewangle[x]) >> ANGLETOSKYSHIFT`), so the stock
256-wide skies cover 90° of turn and repeat four times per revolution,
while a wider PWAD sky covers proportionally more of the horizon. The
mapping is *positive* in the ray angle, like vanilla's — which means the
texture reads right-to-left across the screen (Doom's sky has always been
drawn mirrored relative to the stored patch). -/
private def drawSkyColumn (assets : Assets) (sky : Picture) (view : View)
    (x : Nat) (a b : Int) : RenderM Unit := do
  if a > b then return
  let colAngle := Float.atan ((centerX - (Float.ofNat x + half)) / focal)
  let circleFrac := (view.angle + colAngle) / (2 * pi)
  let tcol := ((ifloor (circleFrac * 1024)) % Int.ofNat sky.width).toNat
  let base := tcol * sky.height
  let mut frame ← takeFrame
  for y in [a.toNat : b.toNat + 1] do
    -- Doom anchors sky row 100 at the horizon; with centerY = 100 the
    -- texture row is simply the screen row.
    let v := y % sky.height
    let texel := sky.pixels.get! (base + v)
    frame := frame.set! (y * screenW + x) (assets.colormap.get! texel.toNat)
  putFrame frame

/-- Draw one visplane by stitching per-column row ranges into horizontal
spans — Doom's `R_MakeSpans`.

`spanStart0` is scratch — the column where each row's open span began —
allocated once per frame by `drawPlanes` and threaded through every plane
rather than reallocated here. Reuse is safe without clearing: a row's entry
is read only when a span *closes* on that row, and every span closed was
opened (and its entry written) by this same plane — the walk starts from
the empty range `t1 > b1`, so no row closes before it opens. -/
def drawPlane (ctx : Ctx) (p : Visplane) (cosA sinA : Float)
    (spanStart0 : Array Nat) : RenderM (Array Nat) := do
  if p.maxX < p.minX then return spanStart0
  if p.flat == skyFlat then
    -- Episode n uses SKYn; Doom II switches sky by act (see `Dill.Maps`).
    let skyName := ((MapId.parse ctx.level.name).map MapId.sky).getD "SKY1"
    let some sky := ctx.assets.textures.get? skyName | return spanStart0
    for x in [p.minX : p.maxX + 1] do
      drawSkyColumn ctx.assets sky ctx.view x p.top[x]! p.bottom[x]!
    return spanStart0
  let some flat := ctx.assets.flats.get? (Assets.animName p.flat ctx.view.tics)
    | return spanStart0
  let dz := p.height - ctx.view.height
  let span := drawSpan ctx.assets ctx.view cosA sinA flat p.light dz
  let mut spanStart := spanStart0
  let mut t1 : Int := 1
  let mut b1 : Int := 0
  -- One virtual column past maxX flushes every open span.
  for x in [p.minX : p.maxX + 2] do
    let (t2, b2) : Int × Int :=
      if x ≤ p.maxX then (p.top[x]!, p.bottom[x]!) else (1, 0)
    let mut t1' := t1
    let mut b1' := b1
    -- rows where the previous column's span ends here
    while t1' < t2 && t1' ≤ b1' do
      span t1'.toNat spanStart[t1'.toNat]! (x - 1)
      t1' := t1' + 1
    while b1' > b2 && b1' ≥ t1' do
      span b1'.toNat spanStart[b1'.toNat]! (x - 1)
      b1' := b1' - 1
    -- rows where a new span opens at this column
    let mut t2' := t2
    let mut b2' := b2
    while t2' < t1 && t2' ≤ b2' do
      spanStart := spanStart.set! t2'.toNat x
      t2' := t2' + 1
    while b2' > b1 && b2' ≥ t2' do
      spanStart := spanStart.set! b2'.toNat x
      b2' := b2' - 1
    t1 := t2
    b1 := b2
  return spanStart

/-- Draw all visplanes accumulated by the wall pass. -/
def drawPlanes (ctx : Ctx) : RenderM Unit := do
  let n := (← get).planes.size
  -- The view angle is constant for the frame, so its cos/sin are computed
  -- once here instead of once per span. The expressions are exactly the
  -- ones `drawSpan` evaluated, so the Float results are bit-identical.
  let cosA := Float.cos ctx.view.angle
  let sinA := Float.sin ctx.view.angle
  -- one scratch buffer for every plane's span starts (see `drawPlane`)
  let mut spanStart : Array Nat := Array.replicate screenH 0
  for i in [0 : n] do
    spanStart ← drawPlane ctx (← get).planes[i]! cosA sinA spanStart

end Dill.Render
