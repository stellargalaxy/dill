import Dill.Render.Walls
import Dill.Render.Planes
import Dill.Render.Things
import Dill.Render.Hud

/-!
# The BSP walk

Doom's central trick: the map's BSP tree is walked view-first, so subsectors
are visited strictly front to back. Walls are drawn during the walk;
occlusion is just "never draw over what's already there" (the clip state in
`DrawState`). The walk stops early once every column is sealed.

A full frame is: walls (collecting see-through middles for later), then the
visplanes the walls accumulated, then the deferred masked walls, then
sprites, then the HUD — the same order vanilla ends up with.
-/

namespace Dill.Render

/-- Conservative visibility test for a BSP child's bounding box — Doom's
`R_CheckBBox`, in float. The box's corners project to a screen column
range (the same endpoint→column mapping segs use, so the range covers
every column any seg inside the box can touch); the subtree is skipped
when the box lies wholly behind the near plane, wholly off-screen, or
covers only columns already sealed solid. Every doubtful case — a corner
behind the near plane leaves the range unbounded — descends. -/
def bboxVisible (view : View) (box : BBox) : RenderM Bool := do
  let near := nearPlane
  -- the four corners, unrolled into scalars: this runs twice per BSP node
  -- visited, and building an array of boxed corner tuples each call was
  -- pure allocator traffic
  let (f1, r1) := view.toCamera box.left  box.top
  let (f2, r2) := view.toCamera box.left  box.bottom
  let (f3, r3) := view.toCamera box.right box.top
  let (f4, r4) := view.toCamera box.right box.bottom
  if f1 ≤ near && f2 ≤ near && f3 ≤ near && f4 ≤ near then
    return false                        -- wholly behind the view plane
  if f1 ≤ near || f2 ≤ near || f3 ≤ near || f4 ≤ near then
    return true                         -- straddles it: columns unbounded
  let sx1 := centerX + focal * r1 / f1
  let sx2 := centerX + focal * r2 / f2
  let sx3 := centerX + focal * r3 / f3
  let sx4 := centerX + focal * r4 / f4
  let minSx := min (min sx1 sx2) (min sx3 sx4)
  let maxSx := max (max sx1 sx2) (max sx3 sx4)
  let x1 : Int := max 0 (colAtOrAfter minSx)
  let x2 : Int := min (Int.ofNat screenW - 1) (colAtOrAfter maxSx - 1)
  if x2 < x1 then return false          -- wholly off-screen left or right
  let solid := (← get).solid
  for x in [x1.toNat : x2.toNat + 1] do
    if !solid[x]! then return true
  return false                          -- every column already sealed

/-- Visit a BSP subtree front to back, drawing walls and collecting the
deferred masked-texture jobs.

`fuel` bounds the descent. A well-formed tree is acyclic, so it reaches each
node at most once and the node count is depth enough; but `Level.validate`
only range-checks child indices — it cannot rule out a node reachable from
its own subtree, and that would otherwise recurse until the stack died.
`Level.subsectorAt` caps its own walk for exactly this reason. Counting the
fuel down also makes the recursion structural, so this needs no `partial`.

The jobs are pushed onto one accumulator threaded down the walk, not
concatenated on the way back up — `near ++ far` at every node copied the
whole array once per level of the return path (O(jobs × depth)). The final
order is identical: near side's jobs first, then the far side's, which the
masked/sprite passes depend on. -/
def renderChild (ctx : Ctx) (fuel : Nat) (child : BspChild) :
    RenderM (Array MaskedJob) :=
  go fuel child #[]
where
  go : Nat → BspChild → Array MaskedJob → RenderM (Array MaskedJob)
  | 0, _, acc => return acc
  | _ + 1, .subsector i, acc => do
    let some sub := ctx.level.subsectors[i]? | return acc
    -- mark this subsector's sector reached, so its mobjs may be drawn
    if sub.count > 0 then
      let s := ctx.level.segFrontSector ctx.level.segs[sub.first]!
      modify fun st =>
        if s < st.visited.size then { st with visited := st.visited.set! s true }
        else st
    let mut masked := acc
    for k in [sub.first : sub.first + sub.count] do
      if let some m ← drawSeg ctx ctx.level.segs[k]! then
        masked := masked.push m
    return masked
  | fuel + 1, .node i, acc => do
    let some n := ctx.level.nodes[i]? | return acc
    let onRight := n.pointOnRight ctx.view.x ctx.view.y
    let (nearChild, nearBox, farChild, farBox) :=
      if onRight then (n.right, n.rightBox, n.left, n.leftBox)
      else (n.left, n.leftBox, n.right, n.rightBox)
    let acc ←
      if ← bboxVisible ctx.view nearBox then go fuel nearChild acc
      else pure acc
    if (← get).openCols == 0 then
      return acc
    if ← bboxVisible ctx.view farBox then
      go fuel farChild acc
    else
      return acc

/-- Render one frame to 426×200 palette indices. -/
def renderPalette (level : Level) (assets : Assets) (view : View)
    (mobjs : Array RenderMobj := #[]) (hud : Option HudInfo := none) :
    ByteArray :=
  let ctx : Ctx := { level, assets, view, mobjs }
  -- A tiny map can be a single subsector with no nodes at all; the root
  -- is then that subsector, not node `0 - 1` (which, in `Nat`, is node 0
  -- of an empty array).
  let root := if level.nodes.isEmpty then BspChild.subsector 0
              else BspChild.node (level.nodes.size - 1)
  let paint : RenderM Unit := do
    modify fun st =>
      { st with visited := Array.replicate level.sectors.size false }
    -- one step per node, plus the leaf the last one hands off to
    let masked ← renderChild ctx (level.nodes.size + 1) root
    drawPlanes ctx
    for m in masked do
      drawMasked ctx m
    drawSprites ctx
    if let some hud := hud then
      drawHud assets hud
  (paint.run DrawState.init).2.frame

/-- Render one frame to 426×200 RGBA, ready for `Shell.present`. -/
def render (level : Level) (assets : Assets) (view : View)
    (mobjs : Array RenderMobj := #[]) (hud : Option HudInfo := none)
    (pal : Nat := 0) : ByteArray :=
  toRGBA assets (renderPalette level assets view mobjs hud) pal

end Dill.Render
