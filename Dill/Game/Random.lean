/-!
# The dice

Doom rolls everything — damage, pain, AI whims — from one byte-valued
stream. Vanilla used a fixed 256-byte table; we use a small LCG instead
(the Float renderer already forgoes demo compatibility). What matters is
kept: byte range 0–255, deterministic from a seed carried in `GameState`,
so scripted-input games replay identically in tests.
-/

namespace Dill

structure Rng where
  seed : UInt32 := 0x1ea4c0de
  deriving Repr, Inhabited

namespace Rng

/-- Next byte (0–255) and the advanced generator. -/
def next (r : Rng) : Nat × Rng :=
  let seed := r.seed * 1103515245 + 12345
  (((seed >>> 16) &&& 0xFF).toNat, { seed })

/-- `next a - next b`: Doom's idiom for a symmetric spread around zero. -/
def diff (r : Rng) : Int × Rng :=
  let (a, r) := r.next
  let (b, r) := r.next
  ((a : Int) - (b : Int), r)

/-- Sum of `n` rolls of `1..sides` (e.g. `damage (3, 8)` = 3d8).

`sides` is floored at 1 because Lean's `%` leaves `b % 0 = b`: a zero-sided
die would quietly roll 1–256 instead of raising anything. No actor asks for
one today — the `(0, 0)` default never rolls, since `n` is 0 with it — but a
one-field edit in `Info.lean` is all it would take, and a damage roll two
orders of magnitude too large is not a failure anyone would trace back
here. -/
def dice (r : Rng) (n sides : Nat) : Nat × Rng := Id.run do
  let sides := max 1 sides
  let mut total := 0
  let mut r := r
  for _ in [0:n] do
    let (b, r') := r.next
    total := total + b % sides + 1
    r := r'
  return (total, r)

end Rng
end Dill
