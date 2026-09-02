import Dill.Game.Combat
import Dill.Game.Specials

/-!
# Sector light effects

Doom's rooms breathe: sector specials make lights blink, strobe, glow, and
flicker. Each effect is a small thinker stepping the sector's light level
between its own brightness and the darkest neighboring sector, exactly as
`p_lights.c` does. Spawned once at level start, advanced every tic.
-/

namespace Dill

namespace GameState

/-- Scan the level's sector specials and spawn their thinkers — the light
effects, plus the two *timed doors* vanilla's `P_SpawnSpecials` seeds in
the same pass: special 10 closes its sector's ceiling like a door after
30 seconds (`P_SpawnDoorCloseIn30`), and 14 opens it like a raise door
after five minutes (`P_SpawnDoorRaiseIn5Mins`). Both ride the ordinary
door mover with a `delay` countdown, and both spend the sector special,
as vanilla zeroes it. -/
def spawnLights (g : GameState) : GameState := Id.run do
  let mut fx : Array LightFx := #[]
  let mut g := g
  for s in [0 : g.level.sectors.size] do
    let sec := g.level.sectors[s]!
    let maxL := sec.light
    let minL := g.level.minNeighborLight s
    -- Only the strobes fall back to full darkness when the whole
    -- neighbourhood is one brightness (`P_SpawnStrobeFlash`'s
    -- `minlight == maxlight` check); blink, glow and flicker keep the
    -- equal bounds and simply hold steady, as vanilla's spawners do.
    let strobeMin := if minL == maxL then 0 else minL
    match sec.special with
    | 1 =>
      let (roll, g') := g.rand
      g := g'
      -- parenthesized: `&&&` binds looser than `+`, so `roll &&& 63 + 1`
      -- would mean `roll &&& 64` — a count of 0 or 64, never 1–64
      fx := fx.push (.blink s minL maxL ((roll &&& 63) + 1))
    -- 4 (strobe + 20% damage) is a *fast* strobe: vanilla case 4 spawns
    -- `P_SpawnStrobeFlash(sector, FASTDARK, 0)`, same as 2. The final
    -- argument is `inSync`: 2/3/4 seed their opening count `(P_Random()&7)+1`
    -- so neighbouring strobes fall out of phase, while the *synchronized*
    -- variants 12/13 all start on the same beat (count 1).
    | 2 | 4 =>
      let (roll, g') := g.rand
      g := g'
      fx := fx.push (.strobe s strobeMin maxL 15 ((roll &&& 7) + 1))
    | 3 =>
      let (roll, g') := g.rand
      g := g'
      fx := fx.push (.strobe s strobeMin maxL 35 ((roll &&& 7) + 1))
    | 13 =>
      fx := fx.push (.strobe s strobeMin maxL 15 1)
    | 12 =>
      fx := fx.push (.strobe s strobeMin maxL 35 1)
    | 8 =>
      fx := fx.push (.glow s minL maxL (up := false))
    | 17 =>
      -- vanilla `P_SpawnFireFlicker` jitters down toward the darkest
      -- neighbour *plus 16*, so the fire never quite reaches it
      fx := fx.push (.flicker s (minL + 16) maxL 4)
    | 10 =>
      -- after 30 seconds the ceiling shuts one-way and stays: a close-only
      -- door (closing, no return) behind a 30-second delay
      g := { g with
        level := { g.level with
          sectors := g.level.sectors.modify s ({ · with special := 0 }) }
        movers := g.movers.push
          (.door s sec.ceilH 0 true true Speeds.door (delay := 30 * 35)) }
    | 14 =>
      -- after five minutes it opens like any raise door — open, wait,
      -- close — aiming 4 under the lowest neighbouring ceiling
      g := { g with
        level := { g.level with
          sectors := g.level.sectors.modify s ({ · with special := 0 }) }
        movers := g.movers.push
          (.door s (g.level.lowestNeighborCeil s - 4) Speeds.doorWait
            false false Speeds.door (delay := 5 * 60 * 35)) }
    | _ => pure ()
  return { g with lights := fx }

/-- Advance one light thinker; returns its continuation. -/
private def stepLight (g : GameState) : LightFx → GameState × LightFx
  | .blink s minL maxL count =>
    if count > 1 then (g, .blink s minL maxL (count - 1))
    else
      -- toggle: long bright stretches, brief dark ones (vanilla 64/7)
      let cur := g.level.sectors[s]!.light
      let (roll, g) := g.rand
      if cur == maxL then
        ({ g with level := g.level.setLight s minL }
        , .blink s minL maxL ((roll &&& 7) + 1))
      else
        ({ g with level := g.level.setLight s maxL }
        , .blink s minL maxL ((roll &&& 63) + 1))
  | .strobe s minL maxL darkTime count =>
    if count > 1 then (g, .strobe s minL maxL darkTime (count - 1))
    else
      let cur := g.level.sectors[s]!.light
      if cur == minL then
        ({ g with level := g.level.setLight s maxL }
        , .strobe s minL maxL darkTime 5)
      else
        ({ g with level := g.level.setLight s minL }
        , .strobe s minL maxL darkTime darkTime)
  | .glow s minL maxL up =>
    let cur : Int := Int.ofNat g.level.sectors[s]!.light
    let (next, up') :=
      if up then
        let n := cur + 8
        if n ≥ maxL then (Int.ofNat maxL, false) else (n, true)
      else
        let n := cur - 8
        if n ≤ minL then (Int.ofNat minL, true) else (n, false)
    ({ g with level := g.level.setLight s next.toNat }, .glow s minL maxL up')
  | .flicker s minL maxL count =>
    if count > 1 then (g, .flicker s minL maxL (count - 1))
    else
      let (roll, g) := g.rand
      let light := max minL (maxL - (roll &&& 3) * 16)
      ({ g with level := g.level.setLight s light }, .flicker s minL maxL 4)

/-- Advance every light one tic. -/
def stepLights (g : GameState) : GameState := Id.run do
  let mut g := g
  let mut fx := #[]
  for l in g.lights do
    let (g', l') := stepLight g l
    g := g'
    fx := fx.push l'
  return { g with lights := fx }

/-- Special floors bite: standing in nukage costs health every 32 tics
(specials 7 = 5, 5 = 10, 4/16 = 20, like vanilla). A radiation suit shrugs
the hazards off — all but the worst two, which vanilla lets through on a
5-in-256 roll even while suited.

Special 11 is the odd one, the E1M8 finale: it cancels god mode, grinds you
down 20 at a time, and ends the episode once you are nearly out. Its checks
run every tic, not just on the damage beat, and the suit is no help. -/
def damageFloor (g : GameState) : GameState := Id.run do
  if g.status.dead then return g
  let sec := g.level.sectors[g.level.sectorAt g.player.x g.player.y]!
  -- vanilla only bites a player standing on the floor itself
  if g.player.z > sec.floorH then return g
  let onBeat := g.tics % 32 == 0
  let suited := g.status.radsuitTics > 0
  match sec.special with
  | 7 => return if onBeat && !suited then g.damagePlayer 5 else g
  | 5 => return if onBeat && !suited then g.damagePlayer 10 else g
  | 4 | 16 =>
    -- `!ironfeet || P_Random () < 5`: unsuited it always bites and no roll
    -- is spent; suited, vanilla rolls every tic and leaks about 2% of them
    if !suited then
      return if onBeat then g.damagePlayer 20 else g
    let (roll, g) := g.rand
    return if onBeat && roll < 5 then g.damagePlayer 20 else g
  | 11 =>
    let g := { g with status := { g.status with god := false } }
    let g := if onBeat then g.damagePlayer 20 else g
    -- `damagePlayer` refuses to let this floor kill outright, so the exit
    -- always gets to fire first
    return if g.status.health ≤ 10 then { g with exited := true } else g
  | _ => return g

end GameState
end Dill
