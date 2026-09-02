import Dill.Render.Frame
import Dill.Game.Random

/-!
# The melt screen wipe

Doom's `wipe.c`. Whenever the game changes screens — a level starts, the
tally comes up, a save is loaded — the picture you were looking at melts
away instead of cutting. Every vertical strand of the old screen slides
straight down off the bottom edge, uncovering the new screen behind it,
and each strand starts falling at its own moment, so the old picture tears
apart raggedly rather than dropping as one sheet.

All the state that takes is one number per column: `y`, the row the top of
that strand of the old screen currently sits on. It begins *negative* — a
short delay, counted up one row per tic before the strand is released —
then accelerates downward until it is past the last row.
-/

namespace Dill.Render

/-- Vanilla melts the 320-wide screen in 2-pixel columns (`wipe.c` copies
`short`s, two palette indices at a time). Our frame is wider, not finer:
its pixels are the same size, so keeping the columns 2 pixels wide keeps
every strand exactly as thick as the original's, and the wide frame simply
gets more of them — 426 = 2 × 213 where vanilla had 160. Melting
1-pixel columns would instead halve the strand width and read as a much
finer, un-Doomlike drizzle. -/
def wipeColW : Nat := 2

/-- How many melting columns span the frame. -/
def wipeCols : Nat := (screenW + wipeColW - 1) / wipeColW

/-- A melt in progress: the current top row of each column of the old
screen. Negative means the column has not started falling yet; `screenH`
means it has fallen clear off the bottom. -/
structure Wipe where
  offsets : Array Int
  deriving Inhabited, Repr

/-- Doom's `wipe_initMelt`: the first column starts up to 15 rows above the
top edge, and each column after it wanders one row up or down from its
neighbour, held inside that same 16-row band. Neighbours therefore start
within a row of each other, which is what makes the melt tear in connected
strands instead of per-column static. -/
def Wipe.init (rng : Rng) : Wipe × Rng := Id.run do
  let (b, rng0) := rng.next
  let mut rng := rng0
  let mut ys := Array.replicate wipeCols (0 : Int)
  ys := ys.set! 0 (-(Int.ofNat (b % 16)))
  for c in [1 : wipeCols] do
    let (b, rng') := rng.next
    rng := rng'
    -- one of -1, 0, +1 away from the column to the left, clamped so no
    -- column waits longer than 15 tics or starts already falling
    let y := ys[c - 1]! + (Int.ofNat (b % 3) - 1)
    ys := ys.set! c (max (-15) (min 0 y))
  return ({ offsets := ys }, rng)

/-- The melt is over once every column has fallen past the last row. -/
def Wipe.done (w : Wipe) : Bool := w.offsets.all (· ≥ Int.ofNat screenH)

/-- One wipe tic (`wipe_doMelt`). A waiting column counts up toward 0 at
one row per tic. A falling one gathers speed: while it is still in the top
16 rows it moves `y + 1` rows — so 1, 2, 4, 8, 16 — and past that it
settles into a steady 8 rows per tic, stopping level with the bottom edge.
That works out to about 40 tics, a bit over a second at 35 Hz. -/
def Wipe.step (w : Wipe) : Wipe := Id.run do
  let mut ys := w.offsets
  for c in [0 : ys.size] do
    let y := ys[c]!
    if y < 0 then
      ys := ys.set! c (y + 1)
    else if y < Int.ofNat screenH then
      let dy := if y < 16 then y + 1 else 8
      ys := ys.set! c (min (Int.ofNat screenH) (y + dy))
  return { w with offsets := ys }

/-- The frame to present: the new screen, with each column of the old
screen laid back over it shifted down by that column's offset. The top `y`
rows of a column are therefore the new screen, and below them comes that
column of the old screen from its own row 0 downward; whatever slides past
the last row is simply gone. A column still waiting (`y ≤ 0`) shows the old
screen untouched, so tic 0 of a wipe is pixel-for-pixel the old frame. -/
def Wipe.compose (w : Wipe) (old new : ByteArray) : ByteArray := Id.run do
  -- Starts from `new` and overwrites the fallen part with `old`: one copy
  -- of the frame, then in-place stores (Lean's functional-but-in-place
  -- `set!`, as the renderer's column loops do).
  let mut out := new
  for c in [0 : w.offsets.size] do
    let shift := (max 0 w.offsets[c]!).toNat
    if shift ≥ screenH then continue
    for x in [c * wipeColW : min screenW ((c + 1) * wipeColW)] do
      for row in [0 : screenH - shift] do
        out := out.set! ((row + shift) * screenW + x)
          (old.get! (row * screenW + x))
  return out

end Dill.Render
