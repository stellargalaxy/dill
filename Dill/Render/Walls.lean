import Dill.Level
import Dill.Render.Frame
import Dill.Render.Planes

/-!
# Wall segments

Each seg the BSP walk visits is projected onto the screen and drawn column
by column, honoring the occlusion state built up by everything drawn in
front of it. One-sided segs fill their columns and mark them solid;
two-sided segs (portals) draw their upper and lower steps and narrow the
vertical clip ranges.

Doom's original does this with binary angles and lookup tables; here the
endpoints go through a camera transform, get clipped to the near plane, and
1/z and u/z are interpolated linearly across screen columns (perspective-
correct texturing). Same algorithm, plainer math.
-/

namespace Dill.Render

/-- What a seg needs drawn, gathered before the column loop. -/
structure WallJob where
  side  : Sidedef
  front : Sector
  back  : Option Sector
  /-- Screen x of the wall's two ends (floats; columns clamp to them). -/
  x1    : Float
  x2    : Float
  /-- 1/depth at the two ends. -/
  iz1   : Float
  iz2   : Float
  /-- Texture u divided by depth at the two ends. -/
  uoz1  : Float
  uoz2  : Float
  /-- Fake contrast: axis-aligned walls get ±1 light. -/
  boost : Int

/-- Camera-transform and clip a seg; `none` when it faces away or lies
outside the view. -/
private def prepare (ctx : Ctx) (seg : Seg) : Option WallJob := do
  let lvl := ctx.level
  let line := lvl.linedefs[seg.linedef]!
  let sideIdx := if seg.backSide then line.back.getD line.front else line.front
  let side := lvl.sidedefs[sideIdx]!
  let front := lvl.sectors[side.sector]!
  let back := (lvl.segBackSector seg).map (lvl.sectors[·]!)
  let p1 := lvl.vertexes[seg.v1]!
  let p2 := lvl.vertexes[seg.v2]!

  -- Back-face cull: the front of a seg is to the right of v1→v2.
  let cross := (p2.x - p1.x) * (ctx.view.y - p1.y)
             - (p2.y - p1.y) * (ctx.view.x - p1.x)
  if cross ≥ 0 then none

  let (f1, r1) := ctx.view.toCamera p1.x p1.y
  let (f2, r2) := ctx.view.toCamera p2.x p2.y
  let segLen := Float.sqrt ((p2.x - p1.x) ^ 2 + (p2.y - p1.y) ^ 2)

  -- Clip to the near plane; tN is where the seg crosses it (0..1 along v1→v2).
  let near := nearPlane
  if f1 ≤ near && f2 ≤ near then none
  let tN := (near - f1) / (f2 - f1)
  let (cf1, cr1, t1) :=
    if f1 ≤ near then (near, r1 + (r2 - r1) * tN, tN) else (f1, r1, 0)
  let (cf2, cr2, t2) :=
    if f2 ≤ near then (near, r1 + (r2 - r1) * tN, tN) else (f2, r2, 1)
  let x1 := centerX + focal * cr1 / cf1
  let x2 := centerX + focal * cr2 / cf2
  if x2 ≤ x1 then none

  let u1 := seg.offset + t1 * segLen
  let u2 := seg.offset + t2 * segLen
  let boost : Int := if p1.y == p2.y then -1 else if p1.x == p2.x then 1 else 0
  return { side, front, back, x1, x2
           iz1 := 1 / cf1, iz2 := 1 / cf2
           uoz1 := u1 / cf1, uoz2 := u2 / cf2, boost }

/-- Vertical texture anchors — the world z where texture row 0 sits — for
the middle, upper, and lower sections, per Doom's pegging rules. `midH`
and `upH` are the heights of the (already resolved) middle and upper
textures, 0 when absent. Unused slots are 0: a one-sided wall has no
upper/lower steps, and a two-sided wall's middle is the deferred masked
pass, which pegs against the *opening* rather than the front sector and so
computes its own anchor (`drawSeg`'s tail). -/
private def anchors (line : Linedef) (job : WallJob)
    (midH upH : Float) : Float × Float × Float :=
  let f := job.front
  match job.back with
  | none =>
    let mid := if line.has Linedef.lowerUnpegged
      then f.floorH + midH else f.ceilH
    (mid, 0, 0)
  | some b =>
    let upper := if line.has Linedef.upperUnpegged
      then f.ceilH else b.ceilH + upH
    let lower := if line.has Linedef.lowerUnpegged
      then f.ceilH else b.floorH
    (0, upper, lower)

/-- The screen columns a wall job covers, clamped to the frame; `none`
when it lands entirely off-screen. The same endpoint→column mapping is
used for the wall pass and the deferred masked pass. -/
def WallJob.columns (job : WallJob) : Option (Nat × Nat) :=
  let ix1 : Int := max 0 (colAtOrAfter job.x1)
  let ix2 : Int := min (Int.ofNat screenW - 1) (colAtOrAfter job.x2 - 1)
  if ix2 < ix1 then none else some (ix1.toNat, ix2.toNat)

/-- Per-column interpolants shared by the wall and masked passes: inverse
depth, texture u, colormap row, and world units per pixel row at screen
column `x`. -/
@[inline] def WallJob.interp (job : WallJob) (view : View) (x : Nat) :
    Float × Float × Nat × Float :=
  let t := (Float.ofNat x + half - job.x1) / (job.x2 - job.x1)
  let iz := job.iz1 + (job.iz2 - job.iz1) * t
  let u := (job.uoz1 + (job.uoz2 - job.uoz1) * t) / iz + job.side.xOffset
  let cm := lightColormap job.front.light iz job.boost (fixed := view.fixedColormap)
  let dv := 1 / (focal * iz)
  (iz, u, cm, dv)

/-- A deferred see-through middle texture (grates and the like). It can't
be painted during the front-to-back wall pass — the flats behind it are
painted later — so it waits until planes are done and relies on the
z-buffer for occlusion. -/
structure MaskedJob where
  tex     : Picture
  job     : WallJob
  /-- World z of texture row 0 (vanilla's masked pegging rules). -/
  anchor  : Float
  /-- World z of the opening the texture hangs in. -/
  openTop : Float
  openBot : Float

/-- Draw one seg. Returns the masked-middle job to paint later, if any. -/
def drawSeg (ctx : Ctx) (seg : Seg) : RenderM (Option MaskedJob) := do
  let some job := prepare ctx seg | return none
  let line := ctx.level.linedefs[seg.linedef]!
  let view := ctx.view
  -- Resolve the seg's three textures once, up front: name → animation
  -- frame → picture is two hash probes, still wasteful to repeat for
  -- every column.
  let texture? := fun (name : String) =>
    if name == "-" then none
    else ctx.assets.textures.get? (Assets.animName name view.tics)
  let midTex := texture? job.side.middle
  let upTex  := texture? job.side.upper
  let loTex  := texture? job.side.lower
  let heightOf := fun (t : Option Picture) =>
    match t with
    | some t => Float.ofNat t.height
    | none   => 0
  let (midAnchor, upperAnchor, lowerAnchor) :=
    anchors line job (heightOf midTex) (heightOf upTex)

  let frontCeilRaw := job.front.ceilH - view.height
  let frontFloor := job.front.floorH - view.height
  -- The sky hack ("hack to allow height changes in outdoor areas"): between
  -- two sky ceilings the step is neither drawn nor clipped. Vanilla does
  -- this by dropping the front wall top to the back ceiling (`worldtop =
  -- worldhigh`), and the direction matters: when such a portal is *closed*
  -- (a solid sky barrier) the sky plane must reach down to the barrier
  -- top, which is exactly where the lowered wall top puts it.
  let bothSky := job.back.any fun b =>
    job.front.ceilFlat == skyFlat && b.ceilFlat == skyFlat
  let backCeil := job.back.map (·.ceilH - view.height)
  let frontCeil := if bothSky then backCeil.getD frontCeilRaw else frontCeilRaw
  let backFloor := job.back.map (·.floorH - view.height)
  -- A closed portal is a solid wall. Vanilla (`R_AddLine`) seals on the
  -- two *raw*-height comparisons — the sky hack never affects them. The
  -- third clause additionally seals zero-height back sectors outright
  -- (vanilla windows them behind overlapping upper and lower textures —
  -- the same picture, sealed cheaper here); it must not apply under the
  -- sky hack, where a zero-height sky barrier below the front ceiling is
  -- a rim to see over (E3M9's courtyards), not a wall.
  let closed := match backCeil, backFloor with
    | some bc, some bf =>
      bc ≤ frontFloor || bf ≥ frontCeilRaw || (!bothSky && bc ≤ bf)
    | _, _ => false

  let some (ix1, ix2) := job.columns | return none

  -- Which of the front sector's planes does this seg reveal? A portal only
  -- reveals a plane if it changes at the line (otherwise the back subsector
  -- draws it); a closed portal reveals both.
  --
  -- Two sky ceilings at *different* heights are the exception: the sky
  -- hack treats them as one continuous sky, so an *open* portal neither
  -- draws nor clips the step. Without this a low sky ceiling in front
  -- (Inferno's courtyards) marks the ceiling at its own height and clips
  -- away the top of every taller wall behind it — the door and brick
  -- sheared off halfway up.
  --
  -- A *closed* portal, though, marks both planes even under the sky hack
  -- (vanilla's closed-door override runs after it, unconditionally): a
  -- sealed sky barrier must paint sky above its lower texture, because
  -- the seal guarantees nothing behind it ever will. Skipping this left
  -- black bands over every IWAD sky wall (E1M8, E3M9, MAP21, …).
  let markCeiling := closed || (!bothSky && (job.back.isNone || job.back.any
    fun b => b.ceilH != job.front.ceilH || b.ceilFlat != job.front.ceilFlat
      || b.light != job.front.light))
  let markFloor := job.back.isNone || closed || job.back.any fun b =>
    b.floorH != job.front.floorH || b.floorFlat != job.front.floorFlat
      || b.light != job.front.light
  let ceilPlane ← if markCeiling then
      some <$> findPlane job.front.ceilH job.front.ceilFlat job.front.light
        ix1 ix2
    else pure none
  let floorPlane ← if markFloor then
      some <$> findPlane job.front.floorH job.front.floorFlat job.front.light
        ix1 ix2
    else pure none

  -- Did the wall loop find any column still open? When every column was
  -- already solid this seg contributes nothing — including its masked
  -- middle, which would only be z-rejected pixel by pixel in `drawMasked`
  -- (whatever sealed the columns was drawn earlier in the front-to-back
  -- walk, so it is nearer), so no job is emitted for it below.
  let mut anyOpen := false
  for x in [ix1 : ix2 + 1] do
    let s ← get
    if s.solid[x]! then continue
    anyOpen := true
    let ceilClip := s.ceilClip[x]!
    let floorClip := s.floorClip[x]!

    let (iz, u, cm, dv) := job.interp view x
    let tcol := fun (tex : Picture) => ((ifloor u) % Int.ofNat tex.width).toNat
    -- Texture v at pixel row y0: anchor is the world z of texture row 0,
    -- each row down covers dv world units at this depth.
    let v0 := fun (anchor : Float) (y0 : Int) =>
      let zAtY0 := view.height + (centerY - (Float.ofInt y0 + half)) * dv
      (anchor - zAtY0) + job.side.yOffset

    let yCeil := rowAtOrBelow (projectRow frontCeil iz)
    let yFloor := rowAtOrBelow (projectRow frontFloor iz) - 1
    let top := max yCeil (ceilClip + 1)
    let bot := min yFloor (floorClip - 1)

    -- The ceiling shows above the wall top, the floor below the wall bottom.
    if let some cp := ceilPlane then
      planeSpan cp x (ceilClip + 1) (min (yCeil - 1) (floorClip - 1))
    if let some fp := floorPlane then
      planeSpan fp x (max (yFloor + 1) (ceilClip + 1)) (floorClip - 1)

    match backCeil, backFloor with
    | none, _ | _, none =>
      -- One-sided: the middle texture fills the column, then it is done.
      if let some tex := midTex then
        drawTexColumn ctx.assets tex (tcol tex) cm iz x top bot
          (v0 midAnchor top) dv
      -- Seal the whole uncovered band, not just the rows the texture
      -- reached. Vanilla's drawseg for a one-sided wall clips sprites over
      -- its entire column (`sprtopclip = screenheightarray`); here the
      -- z-buffer plays that part, and the rows above the wall — sky, which
      -- paints no depth of its own — would otherwise stay at depth 0 and
      -- let a monster two rooms away draw straight over the horizon.
      sealZColumn iz x (ceilClip + 1) (floorClip - 1)
      markSolid x
    | some bc, some bf =>
      -- A clip only narrows when this seg drew a wall section there or
      -- marked the plane; an invisible flush portal (same heights, same
      -- flat, same light) must leave the clips alone, or it eats floor
      -- and ceiling rows that no other seg will ever paint (vanilla
      -- narrows them in exactly these two spots of `RenderSegLoop`).
      let mut newCeil := ceilClip
      let mut newFloor := floorClip
      -- Upper step down to the back ceiling.
      let yBackCeil := rowAtOrBelow (projectRow bc iz)
      if bc < frontCeil && upTex.isSome then
        if let some tex := upTex then
          let hi := min bot (yBackCeil - 1)
          drawTexColumn ctx.assets tex (tcol tex) cm iz x top hi
            (v0 upperAnchor top) dv
        newCeil := max ceilClip (min (yBackCeil - 1) (floorClip - 1))
      else if markCeiling then
        newCeil := max ceilClip (yCeil - 1)
      -- Lower step up from the back floor.
      let yBackFloor := rowAtOrBelow (projectRow bf iz)
      if bf > frontFloor && loTex.isSome then
        if let some tex := loTex then
          let lo := max top yBackFloor
          drawTexColumn ctx.assets tex (tcol tex) cm iz x lo bot
            (v0 lowerAnchor lo) dv
        newFloor := min floorClip (max yBackFloor (ceilClip + 1))
      else if markFloor then
        newFloor := min floorClip (yFloor + 1)
      -- A closed portal seals the column outright — for occlusion too, not
      -- just for further walls: any row its textures did not cover still
      -- needs depth, or a sprite in the sealed room shows through. As
      -- above, that means the whole uncovered band, so a sky ceiling over
      -- the shut door does not leave a depth-free gap.
      if closed then
        sealZColumn iz x (ceilClip + 1) (floorClip - 1)
        markSolid x
      else
        -- rebind: a `mut` variable can't be captured by the closure below
        let ceilNow := newCeil
        let floorNow := newFloor
        modify fun s =>
          { s with ceilClip  := s.ceilClip.set! x ceilNow
                   floorClip := s.floorClip.set! x floorNow }

  -- A see-through middle texture on a portal is drawn after the planes.
  match job.back with
  | none => return none
  | some b =>
    if closed then return none
    if !anyOpen then return none
    let some tex := midTex | return none
    let openTop := min job.front.ceilH b.ceilH
    let openBot := max job.front.floorH b.floorH
    let anchor := if line.has Linedef.lowerUnpegged
      then openBot + Float.ofNat tex.height else openTop
    return some { tex, job, anchor, openTop, openBot }

/-- Paint a deferred masked middle texture; the z-buffer supplies all the
occlusion the wall pass would otherwise have to remember. -/
def drawMasked (ctx : Ctx) (m : MaskedJob) : RenderM Unit := do
  let job := m.job
  let view := ctx.view
  let some (ix1, ix2) := job.columns | return
  for x in [ix1 : ix2 + 1] do
    let (iz, u, cm, dv) := job.interp view x
    let tcol := ((ifloor u) % Int.ofNat m.tex.width).toNat
    let y0 := max 0 (rowAtOrBelow (projectRow (m.openTop - view.height) iz))
    let y1 := min (Int.ofNat screenH - 1)
      (rowAtOrBelow (projectRow (m.openBot - view.height) iz) - 1)
    let zAtY0 := view.height + (centerY - (Float.ofInt y0 + half)) * dv
    let v0 := (m.anchor - zAtY0) + job.side.yOffset
    drawMaskedColumn ctx.assets m.tex tcol cm iz (writeZ := true) x y0 y1 v0 dv

end Dill.Render
