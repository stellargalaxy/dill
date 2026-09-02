import Dill.Render.Frame

/-!
# Sprites

Live mobjs drawn as billboards. The game hands the renderer a list of
`RenderMobj`s (position, sprite family, animation frame, facing); the
renderer picks the right view rotation — monsters seen from behind show
their backs — and mirrors the lumps stored flipped.

Occlusion against walls and flats is the z-buffer written by those passes;
sprites are painted back to front so they layer among themselves.
-/

namespace Dill.Render

/-- A thing projected onto the screen, awaiting its paint order. -/
structure Vissprite where
  pic    : Picture
  flip   : Bool
  iz     : Float
  /-- Exact screen x of the sprite's left edge. -/
  xStart : Float
  /-- Screen pixels per texel. -/
  scale  : Float
  /-- World z of the sprite's top edge. -/
  topZ   : Float
  /-- World z of the thing's feet: the flat at this height never hides it. -/
  footZ  : Float
  cm     : Nat
  /-- A spectre / blursphere target: drawn as fuzz over the background. -/
  fuzz   : Bool := false
  /-- Position in the mobj list: the depth sort's tie-break, so sprites
  at exactly equal depth keep a stable paint order across frames. -/
  order  : Nat := 0
  deriving Inhabited

/-- The half-sector bias that centres rotation 0 on the camera, and the
width of one of the 8 rotation sectors. Both are constant, and both sit in
the per-sprite path — named so they are computed at module initialisation
rather than through `Float.ofScientific` on every sprite (see the note on
fractional constants in `Render/Frame.lean`). -/
private def rotationBias : Float := pi * 1.125
private def rotationStep : Float := pi / 4

/-- Doom's rotation pick: which of the 8 views of `m` faces the camera. -/
private def rotationFor (view : View) (m : RenderMobj) : Nat :=
  let toThing := Float.atan2 (m.y - view.y) (m.x - view.x)
  let rel := toThing - m.angle + rotationBias
  let turns := rel / rotationStep
  ((ifloor turns) % 8).toNat

/-- Is any part of this body standing in a sector the BSP walk reached?

The centre answers for almost everything, and costs the one BSP descent the
light level needs anyway. But a body is a box, not a point: a Spider
Mastermind is 128 units to its own edge, and judging it by its centre alone
made the whole sprite wink out the moment that centre crossed into a room
the walk had not entered — while most of it was still in plain sight. The
four corners are consulted only when the centre has already said no, so the
extra descents are paid on the reject path and a visible mobj still costs
exactly one. -/
private def roomVisible (ctx : Ctx) (visited : Array Bool) (m : RenderMobj)
    (secIdx : Nat) : Bool := Id.run do
  -- an empty `visited` means "not tracked": draw everything
  if visited.isEmpty then return true
  if visited[secIdx]?.getD true then return true
  if m.radius ≤ 0 then return false
  let r := m.radius
  for (dx, dy) in [(r, r), (-r, r), (r, -r), (-r, -r)] do
    if visited[ctx.level.sectorAt (m.x + dx) (m.y + dy)]?.getD true then
      return true
  return false

/-- Project the visible mobjs. `visited` marks the sectors the BSP walk
reached; a mobj standing wholly in unvisited sectors is in an unseen room
and is skipped, so it can never show through the wall in front of it. -/
def collectSprites (ctx : Ctx) (visited : Array Bool) : Array Vissprite := Id.run do
  let mut out := #[]
  for m in ctx.mobjs do
    -- the mobj's sector, found once (a full BSP descent): it gates room
    -- visibility and supplies the light level below
    let secIdx := ctx.level.sectorAt m.x m.y
    if !roomVisible ctx visited m secIdx then continue
    let some rots := ctx.assets.spriteRots.get? (m.sprite.push m.frame)
      | continue
    let rot := rots[rotationFor ctx.view m]!
    let some pic := ctx.assets.sprites.get? rot.lump | continue
    let (f, r) := ctx.view.toCamera m.x m.y
    if f < 4 then continue
    let iz := 1 / f
    let scale := focal * iz
    let sec := ctx.level.sectors[secIdx]!
    let xCenter := centerX + focal * r * iz
    let leftOff := if rot.flipped
      then Float.ofNat pic.width - Float.ofInt pic.leftOffset
      else Float.ofInt pic.leftOffset
    out := out.push
      { pic, iz, scale, flip := rot.flipped
        order  := out.size
        xStart := xCenter - leftOff * scale
        topZ   := m.z + Float.ofInt pic.topOffset
        footZ  := m.z
        -- purely MF_SHADOW, as in vanilla: a spectre stays fuzz under
        -- invulnerability and the light-amp too (the fuzz draw reads the
        -- background through colormap row 6, ignoring `cm` entirely)
        fuzz   := m.shadow
        cm     := match ctx.view.fixedColormap with
                  | some c => c
                  | none => if m.bright then 0
                            else lightColormap sec.light iz }
  return out

/-- Draw one projected sprite, clipped by the z-buffer. -/
def drawSprite (ctx : Ctx) (spr : Vissprite) : RenderM Unit := do
  let x1 : Int := max 0 (colAtOrAfter spr.xStart)
  let xEnd := spr.xStart + Float.ofNat spr.pic.width * spr.scale
  let x2 : Int := min (Int.ofNat screenW - 1) (colAtOrAfter xEnd - 1)
  if x2 < x1 then return
  let topY := projectRow (spr.topZ - ctx.view.height) spr.iz
  let y0 : Int := max 0 (rowAtOrBelow topY)
  let yEnd := topY + Float.ofNat spr.pic.height * spr.scale
  let y1 : Int := min (Int.ofNat screenH - 1) (rowAtOrBelow yEnd - 1)
  if y1 < y0 then return
  let dv := 1 / spr.scale
  let v0 := (Float.ofInt y0 + half - topY) * dv
  for x in [x1.toNat : x2.toNat + 1] do
    let tcol := ifloor ((Float.ofNat x + half - spr.xStart) * dv)
    if 0 ≤ tcol && tcol < Int.ofNat spr.pic.width then
      let tcol := if spr.flip then spr.pic.width - 1 - tcol.toNat else tcol.toNat
      drawMaskedColumn ctx.assets spr.pic tcol spr.cm spr.iz
        (writeZ := false) x y0 y1 v0 dv (groundZ := some spr.footZ)
        (fuzz := spr.fuzz) (fuzzPhase := ctx.view.tics)

/-- Draw all things, farthest first; ties broken by mobj order, so a
corpse pile at one spot doesn't reshuffle between frames (`qsort` alone
is unstable). -/
def drawSprites (ctx : Ctx) : RenderM Unit := do
  let visited := (← get).visited
  let sprites := (collectSprites ctx visited).qsort fun a b =>
    a.iz < b.iz || (a.iz == b.iz && a.order < b.order)
  for spr in sprites do
    drawSprite ctx spr

end Dill.Render
