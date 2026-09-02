import Dill.Maps
import Dill.Game.State
import Dill.Game.Sfx
import Dill.Render.Frame

/-!
# Combat

Damage and the ways of delivering it: hitscan bullets (`lineAttack`),
flying missiles, melee, and radius blasts. Everything ends up in
`damageMobj`/`damagePlayer`, which run the pain/death logic.

The map traversals this leans on — `Level.checkSight` (can A see B),
`Level.checkBody` (can a body of some size stand at a point) and
`Level.soundFlood` (which sectors hear a shot) — are pure geometry and live
in `Dill/Level.lean` with the rest of it.
-/

namespace Dill

namespace GameState

/-! RNG threading helpers. -/

def rand (g : GameState) : Nat × GameState :=
  let (v, rng) := g.rng.next
  (v, { g with rng })

def randDiff (g : GameState) : Int × GameState :=
  let (v, rng) := g.rng.diff
  (v, { g with rng })

def randDice (g : GameState) (n sides : Nat) : Nat × GameState :=
  let (v, rng) := g.rng.dice n sides
  (v, { g with rng })

/-- Write a mobj back. This is the *only* path by which a live mobj's `x`/`y`
change — every other writer either spawns (which indexes the new mobj) or
touches non-positional fields — which is what lets `mobjGrid` stay exact
rather than merely approximate. Keep it that way: a position written around
this function would go unindexed and stop colliding. -/
def setMobj (g : GameState) (i : Nat) (m : Mobj) : GameState :=
  let old := g.mobjs[i]!
  let g := { g with mobjs := g.mobjs.set! i m }
  if old.x == m.x && old.y == m.y then g
  else { g with mobjGrid := g.mobjGrid.move i old.x old.y m.x m.y }

/-- A gunshot's noise (vanilla `P_NoiseAlert`): flood the alert from the
player's sector and remember which sectors heard it. -/
def alertSound (g : GameState) : GameState :=
  let start := g.level.sectorAt g.player.x g.player.y
  { g with alerted := g.level.soundFlood start g.alerted }

/-- Is any solid mobj (other than `exceptUid`) or the player in the way of
a body of `radius` at `(x, y)`? -/
def mobjBlocked (g : GameState) (exceptUid : Nat) (x y radius : Float)
    (blockedByPlayer : Bool := true) : Bool := Id.run do
  for i in g.mobjsNear x y radius do
    let m := g.mobjs[i]!
    if m.removed || !m.solid || m.uid == exceptUid then continue
    -- Doom bodies are *squares*, not circles: `PIT_CheckThing` blocks when
    -- both |dx| and |dy| are inside the summed radii, so two things can
    -- stand closer corner-to-corner than face-to-face
    let reach := m.info.radius + radius
    if Float.abs (m.x - x) < reach && Float.abs (m.y - y) < reach then
      return true
  if blockedByPlayer && !g.status.dead then
    let p := g.player
    let reach := Player.radius + radius
    if Float.abs (p.x - x) < reach && Float.abs (p.y - y) < reach then
      return true
  return false

/-- Monsters killed by the player leave their trademark gift. -/
private def dropFor : ActorKind → Option ActorKind
  | .zombieman  => some (.item "CLIP" "A")
  | .shotgunGuy => some (.item "SHOT" "A")
  -- Doom II's two extra gun-carriers drop theirs the same way
  | .chaingunner => some (.item "MGUN" "A")
  | .wolfSS     => some (.item "CLIP" "A")
  | _ => none

/-- Vanilla's infighting species test (`PIT_CheckThing`): a monster's missile
does no damage to another of its own kind, and the Hell Knight and Baron count
as a single kind for this purpose. Hitscan, melee and splash ignore it — only
missiles pass harmlessly through kin. -/
def sameSpecies (a b : ActorKind) : Bool :=
  a == b
  || (a == .hellKnight && b == .baron) || (a == .baron && b == .hellKnight)

/-- Boss deaths (vanilla `A_BossDeath`). Which monster counts, and what
clearing them does, is per-map rather than one rule with a different
monster: E1M8 and E4M8 lower the tag-666 floors, E4M6 blasts open the
tag-666 doors, and E2M8/E3M8 simply end the level. `m` is the boss that
just fell (still on the roster, health written). -/
private def bossDeath (g : GameState) (m : Mobj) : GameState := Id.run do
  let mut g := g
  let here := MapId.parse g.level.name
  let bossKind : Option ActorKind := match here with
    | some (.episode 1 8) => some .baron
    | some (.episode 2 8) => some .cyberdemon
    | some (.episode 3 8) => some .spiderMastermind
    | some (.episode 4 6) => some .cyberdemon
    | some (.episode 4 8) => some .spiderMastermind
    -- Doom II hangs both of its specials on MAP07, keyed by which of the
    -- two species you finish off.
    | some (.level 7) =>
      if m.kind == .mancubus then some .mancubus
      else if m.kind == .arachnotron then some .arachnotron
      else none
    | _ => none
  let isBoss := match bossKind with
    | some k => k == m.kind
    | none => false
  -- Vanilla checks "make sure there is a player alive for victory"
  -- (p_enemy.c A_BossDeath) before acting: a boss that dies alongside
  -- the last player ends nothing. That the special fires at the instant
  -- of death — vanilla defers it a few tics into the death animation —
  -- is an intentional simplification.
  if isBoss && !g.status.dead then
    let othersLeft := g.mobjs.any fun b =>
      b.kind == m.kind && !b.corpse && b.health > 0 && b.uid != m.uid
    if !othersLeft then
      match here with
      | some (.episode 4 6) =>
        for si in (g.level.sectorsTagged 666) do
          -- the height an ordinary door opens to, so the boss-death door
          -- and every other door in the game agree on what "open" means
          let top := g.level.lowestNeighborCeil si - 4
          g := { g with movers := g.movers.push (.door si top 0 false true) }
      -- MAP07: the last mancubus drops the tag-666 floors, the last
      -- arachnotron raises the tag-667 ones into steps. Vanilla raises
      -- 667 by the shortest lower texture; this goes to the next higher
      -- neighbouring floor, which builds the same climb out.
      | some (.level 7) =>
        let (tag, raise) := if m.kind == .arachnotron then (667, true)
                            else (666, false)
        for si in (g.level.sectorsTagged tag) do
          let lo := g.level.lowestNeighborFloor si
          -- Vanilla lowers to the lowest neighbour; and *raises* by the
          -- shortest lower texture, which here means a step the player
          -- can still climb from the arena — not a flush 32-unit wall.
          -- Cap the rise 24 above the lowest neighbour so the exit stays
          -- reachable whether or not you rode the platform up.
          let target := if raise then min (g.level.nextHigherNeighborFloor si)
                                          (lo + 24)
                        else lo
          let mv := if raise then Mover.floorUp si target Speeds.floor
                    else Mover.floorDown si target Speeds.floor
          g := { g with movers := g.movers.push mv }
      | some (.episode 1 8) | some (.episode 4 8) =>
        for si in (g.level.sectorsTagged 666) do
          let lowest := g.level.lowestNeighborFloor si
          g := { g with movers := g.movers.push (.floorDown si lowest Speeds.floor) }
      | _ => g := { g with exited := true }
  return g

/-- Vanilla `P_KillMobj`: the lethal half of a `damageMobj` — death (or gib)
state, the kill tally, the trademark drop, the Keen doors, then the boss
specials. `m` is the victim as it stood before the blow; `health` is the
post-blow (non-positive) value, and `kmomX`/`kmomY` carry the knockback so
the corpse slides. -/
private def killMobj (g : GameState) (i : Nat) (m : Mobj) (health : Int)
    (kmomX kmomY : Float) : GameState := Id.run do
  let mut g := g
  -- overkill bursts the body into giblets: below −spawnHealth, take the
  -- extreme-death animation if the actor has one (vanilla P_DamageMobj)
  let gib := health < -m.info.health && m.info.xdeathState.isSome
  if m.info.countKill then
    g := { g with kills := g.kills + 1 }
  match (if gib then m.info.xdeathState else m.info.deathState) with
  | some death =>
    g := g.setMobj i { (m.setState death) with
      health, corpse := true, momX := kmomX, momY := kmomY }
  | none =>
    -- no death animation in the tables (cannot happen for the shipped
    -- actors): the kill still lands — write the health and retire the
    -- body, rather than silently dropping a lethal blow
    g := g.setMobj i { m with
      health, corpse := true, removed := true, momX := kmomX, momY := kmomY }
  if let some kind := dropFor m.kind then
    let (g', j) := g.spawn kind m.x m.y m.angle
    -- vanilla `MF_DROPPED`: half ammo, and no credit on the item tally
    g := { g' with mobjs := g'.mobjs.modify j ({ · with dropped := true }) }
  -- Commander Keen (vanilla `A_KeenDie`): map-agnostic, unlike the boss
  -- specials — wherever the last Keen dies, the tag-666 doors open.
  -- Vanilla hangs it on the Keen's death frames rather than on MAP32, so a
  -- PWAD can use Keens anywhere.
  if m.kind == .commanderKeen then
    let keensLeft := g.mobjs.any fun b =>
      b.kind == .commanderKeen && !b.corpse && b.health > 0 && b.uid != m.uid
    if !keensLeft then
      for si in (g.level.sectorsTagged 666) do
        -- the height an ordinary door opens to, so the Keen door and
        -- every other door in the game agree on what "open" means
        let top := g.level.lowestNeighborCeil si - 4
        g := { g with movers := g.movers.push (.door si top 0 false true) }
  return bossDeath g m

/-- Hurt a mobj: enter pain sometimes, die at zero. Damage from the player
(`sourceUid` 0) wakes it toward the player; damage from another mobj makes it
turn on that attacker (infighting), renewing its `threshold` so it stays fixed
there for a spell. Arch-viles are never infighting targets, and ignore their
own threshold.

Clear `wakes` for damage with no attacker behind it — a crusher, say — which
vanilla models by passing a null `source`: such a blow neither retargets nor
rouses a sleeping monster. -/
def damageMobj (g : GameState) (i : Nat) (damage : Nat)
    (sourceUid : Nat := 0) (inflictor : Option (Float × Float) := none)
    (wakes : Bool := true) : GameState := Id.run do
  let m := g.mobjs[i]!
  if !m.shootable then return g
  let health := m.health - damage
  -- Knockback (vanilla `P_DamageMobj`): a hit with a positional inflictor
  -- shoves the target away from it, harder the lighter its `mass`, capped at
  -- `MAXMOVE`. Applies whether the blow is fatal or not, so corpses slide.
  let (kmomX, kmomY) := match inflictor with
    | some (ix, iy) =>
      let ang := Float.atan2 (m.y - iy) (m.x - ix)
      let thrust := min 30.0 (Float.ofNat damage * 12.5 / m.info.mass)
      (m.momX + thrust * Float.cos ang, m.momY + thrust * Float.sin ang)
    | none => (m.momX, m.momY)
  if health ≤ 0 then
    return killMobj g i m health kmomX kmomY
  let mut g := g
  -- being hurt clears the reaction delay: it can fire back this instant
  let mut m := { m with health, momX := kmomX, momY := kmomY, reactionTime := 0 }
  -- Retargeting (vanilla P_DamageMobj): a hit retargets the victim onto its
  -- attacker — the player included — and renews its threshold, unless it is
  -- still locked on a current enemy (threshold), the blow is its own, or the
  -- attacker is an arch-vile, which nothing turns on. An arch-vile ignores
  -- the threshold and retargets at once. Same-species missiles never reach
  -- here (they pass through kin in `moveMissile`), so same-kind
  -- hitscan/melee/splash — which vanilla *does* let infight — falls through
  -- naturally. `wakes` distinguishes the player (sourceUid 0) from vanilla's
  -- null source (a crusher, a nukage floor), which retargets nothing.
  if sourceUid != m.uid && (m.threshold == 0 || m.kind == .archVile) then
    if sourceUid == 0 && wakes then
      m := { m with target := 0, threshold := 100 }
    else if sourceUid != 0 then
      if let some si := g.mobjIdx? sourceUid then
        if g.mobjs[si]!.kind != .archVile then
          m := { m with target := sourceUid, threshold := 100 }
  -- Getting shot is very persuasive: acquire an enemy and wake up. Vanilla
  -- does this inside the retarget block above, so damage with no attacker
  -- behind it — a crusher, a nukage floor — leaves a sleeping monster
  -- asleep; `wakes` carries that distinction, since a `sourceUid` of 0
  -- already means "the player" here.
  if wakes && !m.awake then
    m := m.setState (m.info.seeState.getD m.state)
    m := { m with awake := true, moveCount := 0 }
  let (roll, g') := g.rand
  g := g'
  -- a charging lost soul (MF_SKULLFLY) is immune to flinching; everything else
  -- rolls against its pain chance and, on a hit, resolves to fight back
  if roll < m.info.painChance && !m.charging then
    m := { m with justHit := true }
    if let some pain := m.info.painState then
      m := m.setState pain
  -- Vanilla zeroes a `MF_SKULLFLY` target's momentum, which makes the skull
  -- "slam into something" on its next move — `P_XYMovement` clears the flag
  -- and drops it back to its spawn state. A hit ends the dive, in other
  -- words; this reaches the same place directly. It runs after the pain
  -- roll so the roll still sees a charging skull and skips it, as vanilla's
  -- flag test does.
  if m.charging then
    m := { (m.setState m.info.spawnState) with
             charging := false, momX := 0, momY := 0, momZ := 0 }
  return g.setMobj i m

/-- Hurt the player. Green armour soaks a third of the blow and blue a half;
gods soak anything short of a telefrag. -/
def damagePlayer (g : GameState) (damage : Nat)
    (inflictor : Option (Float × Float) := none) : GameState :=
  if g.status.dead then g else
  -- "take half damage in trainer mode" — vanilla halves a *player's* damage
  -- on I'm Too Young To Die before anything else looks at it, so the
  -- knockback below is softer too.
  let damage := if g.skill == 1 then damage / 2 else damage
  -- Knockback lands before the god/armor logic (vanilla `P_DamageMobj`):
  -- even an invulnerable player — or one rocket-jumping off their own blast —
  -- is shoved away from the inflictor, capped at `MAXMOVE`.
  let g := match inflictor with
    | some (ix, iy) =>
      let p := g.player
      let ang := Float.atan2 (p.y - iy) (p.x - ix)
      let thrust := min 30.0 (Float.ofNat damage * 12.5 / 100.0)
      { g with player := { p with
          momX := p.momX + thrust * Float.cos ang
          momY := p.momY + thrust * Float.sin ang } }
    | none => g
  -- Vanilla's "end of game hell hack": standing on a special-11 floor,
  -- nothing can take the last point of health. That is what lets E1M8's
  -- finale grind you to 10 and exit rather than killing you.
  let damage :=
    if (g.level.sectors[g.level.sectorAt g.player.x g.player.y]!).special == 11
        && Int.ofNat damage ≥ g.status.health
    then (max 0 (g.status.health - 1)).toNat else damage
  -- Vanilla guards god and invulnerability with `damage < 1000`, so a
  -- telefrag's 10000 kills through both.
  if damage < 1000 && (g.status.god || g.status.invulnTics > 0) then g else
  let st := g.status
  -- Green armour soaks a third of each blow, blue a half; points with no
  -- jacket behind them soak nothing. Running the points out takes the
  -- jacket with them (vanilla clears `armortype`).
  let want := if st.armorType == 2 then damage / 2
              else if st.armorType == 1 then damage / 3
              else 0
  let saved := min want st.armor.toNat
  let dealt := damage - saved
  let health := st.health - dealt
  let st := { st with
    armor := st.armor - saved
    armorType := if st.armor - saved ≤ 0 then 0 else st.armorType
    health := max 0 health
    damageCount := min 100 (st.damageCount + dealt)
    dead := health ≤ 0 }
  let g := { g with status := st }
  if dealt == 0 then g
  else
    -- Vanilla `A_PlayerScream`: dying more than 50 below zero gets the
    -- longer DSPDIEHI wail instead of DSPLDETH. The lump only ships with
    -- Doom II — on Doom 1 the loader simply has nothing in that slot, which
    -- is vanilla's `gamemode == commercial` gate by other means.
    let cry := if st.dead then
                 if health < -50 then Sfx.plDeathHi else Sfx.plDeath
               else Sfx.plPain
    g.playSound cry g.player.x g.player.y

def spawnEffect (g : GameState) (kind : ActorKind) (x y z : Float) :
    GameState :=
  let (g, i) := g.spawn kind x y 0
  { g with mobjs := g.mobjs.modify i ({ · with z }) }

/-! ## Hitscan

A shot is a ray, the wall crossings along it, and the things standing on it.
`shotCrossings` and `shotTargets` gather those two lists — sorted by distance,
which is the order both `PTR_AimTraverse` and `PTR_ShootTraverse` walk them
in — and `aimThrough` is the aim traversal itself. `lineAttack` and
`aimLineAttack` are then the two things you can do with the same gather:
fire, or only settle the slope. -/

/-- A wall crossing along a shot: how far along, the vertical opening there,
and whether the floors and the ceilings actually differ across the line.

Vanilla narrows the aim window against an edge only when that edge is a real
step, so a line with matching floors never raises the bottom of the window
however high that shared floor sits. A one-sided wall gets an inverted
(empty) opening, which closes the window outright. -/
private structure Crossing where
  t            : Float
  openBot      : Float
  openTop      : Float
  floorsDiffer : Bool
  ceilsDiffer  : Bool
  /-- The linedef's front-sector ceiling — height, and whether its flat is
  the sky. `PTR_ShootTraverse` needs both: a shot that leaves through the
  sky must not leave a puff floating on it. -/
  frontCeilH   : Float
  frontCeilSky : Bool
  /-- A back-sector ceiling that is *also* sky marks vanilla's "sky hack
  wall" (the invisible upper between two outdoor sectors), which likewise
  swallows the shot without a puff. -/
  backCeilSky  : Bool
  deriving Inhabited

/-- A thing standing on the shot's ray: how far along, the span of height it
occupies, and which mobj it is — `none` for the player. -/
private structure ShotTarget where
  t    : Float
  zBot : Float
  zTop : Float
  idx  : Option Nat
  deriving Inhabited

/-- The wall crossings along a shot, nearest first. Only the lines in the
blockmap cells the ray actually crosses are considered. -/
private def shotCrossings (g : GameState) (sx sy dx dy range : Float) :
    Array Crossing := Id.run do
  let mut crossings : Array Crossing := #[]
  for li in g.level.linesAlong sx sy (sx + dx * range) (sy + dy * range) do
    let line := g.level.linedefs[li]!
    let some t := g.level.rayHitsLine line sx sy dx dy | continue
    if t ≤ 0 || t ≥ range then continue
    let f := g.level.sectors[g.level.sidedefs[line.front]!.sector]!
    let frontCeilH := f.ceilH
    let frontCeilSky := f.ceilFlat == Render.skyFlat
    match line.back with
    | none =>
      crossings := crossings.push
        { t, openBot := 1.0e30, openTop := -1.0e30
          floorsDiffer := true, ceilsDiffer := true
          frontCeilH, frontCeilSky, backCeilSky := false }
    | some back =>
      let b := g.level.sectors[g.level.sidedefs[back]!.sector]!
      crossings := crossings.push
        { t, openBot := max f.floorH b.floorH, openTop := min f.ceilH b.ceilH
          floorsDiffer := f.floorH != b.floorH
          ceilsDiffer  := f.ceilH != b.ceilH
          frontCeilH, frontCeilSky
          backCeilSky := b.ceilFlat == Render.skyFlat }
  return crossings.qsort (·.t < ·.t)

/-- The shootable things the ray passes within a radius of, nearest first.
The player joins the list only for a monster's fire (`fromPlayer` clear) —
the player's own bullets never seek them out. -/
private def shotTargets (g : GameState) (sx sy dx dy range : Float)
    (fromPlayer : Bool) : Array ShotTarget := Id.run do
  let mut targets : Array ShotTarget := #[]
  -- The grid, not the whole roster: a volley calls this once per pellet, and
  -- the super shotgun looses twenty. Anything within reach of the ray is
  -- within half its length of the ray's midpoint, so one query there covers
  -- it — the same trick `moveMissile` uses for its swept path.
  for i in g.mobjsNear (sx + dx * range / 2) (sy + dy * range / 2) (range / 2) do
    let m := g.mobjs[i]!
    if m.removed || !m.shootable then continue
    let toX := m.x - sx
    let toY := m.y - sy
    let tAlong := toX * dx + toY * dy
    if tAlong ≤ 0 || tAlong ≥ range then continue
    -- Vanilla `PIT_AddThingIntercepts` crosses the body's *box* corner to
    -- corner, so a shot sees a square's support width: half-width
    -- `r·(|cos θ|+|sin θ|)`, from r face-on up to √2·r along a diagonal —
    -- wider than the inscribed circle at every angle but the four cardinals.
    if Float.abs (toX * dy - toY * dx)
        > m.info.radius * (Float.abs dx + Float.abs dy) then continue
    targets := targets.push
      { t := tAlong, zBot := m.z, zTop := m.z + m.info.height, idx := some i }
  if !fromPlayer && !g.status.dead then
    let p := g.player
    let tAlong := (p.x - sx) * dx + (p.y - sy) * dy
    if 0 < tAlong && tAlong < range
        && Float.abs ((p.x - sx) * dy - (p.y - sy) * dx)
             ≤ Player.radius * (Float.abs dx + Float.abs dy) then
      targets := targets.push
        { t := tAlong, zBot := p.z, zTop := p.z + Player.height, idx := none }
  return targets.qsort (·.t < ·.t)

/-- Vanilla `PTR_AimTraverse`: narrow the ±0.625 slope window (Doom's aim
cone) through each opening in turn and lock onto the first target whose
height range still fits — which is why bullets climb staircases and drop
down ledges. Returns the target's distance, the slope settled on, and which
target it was. Once the window closes, nothing further along can fit, so the
walk is over. -/
private def aimThrough (crossings : Array Crossing) (targets : Array ShotTarget)
    (sz : Float) : Option (Float × Float × Option Nat) := Id.run do
  let mut lo := -0.625
  let mut hi := 0.625
  let mut ci := 0
  for tgt in targets do
    while ci < crossings.size && crossings[ci]!.t < tgt.t && lo < hi do
      let c := crossings[ci]!
      -- vanilla `PTR_AimTraverse` stops the traverse outright at a closed
      -- opening (`openbottom >= opentop`), before any slope narrowing —
      -- nothing past a shut door can be aimed at, whatever the window says
      if c.openBot ≥ c.openTop then return none
      if c.floorsDiffer then lo := max lo ((c.openBot - sz) / c.t)
      if c.ceilsDiffer  then hi := min hi ((c.openTop - sz) / c.t)
      ci := ci + 1
    if lo ≥ hi then return none
    let tl := (tgt.zBot - sz) / tgt.t
    let th := (tgt.zTop - sz) / tgt.t
    if th > lo && tl < hi then
      return some (tgt.t, (max tl lo + min th hi) / 2, tgt.idx)
  return none

/-- A hitscan shot.

With no `slope` given this is vanilla's `P_AimLineAttack` followed by the
shot: `aimThrough` settles the aim, and whatever it locked onto is hit.

Passing a `slope` instead is `PTR_ShootTraverse`: the aim has already been
settled for the whole volley (see `bulletSlope`) and this fires along it,
hitting the first thing the ray genuinely passes through and stopping at the
first opening it cannot clear. That is how a shotgun's pellets share one
vertical aim while scattering horizontally. -/
def lineAttack (g : GameState) (sx sy sz angle range : Float)
    (damage : Nat) (fromPlayer : Bool) (sourceUid : Nat := 0)
    (slope : Option Float := none) : GameState := Id.run do
  -- A monster's tracer reaches gun-triggered lines too — vanilla
  -- `PTR_ShootTraverse` runs `P_ShootSpecialLine` for any shooter, which
  -- lets only special 46 through for a non-player. The player's shots are
  -- flagged at the trigger pull (`firedShot`), so only the monsters'
  -- are recorded here; `tick` drains both into the same deferred pass.
  let g := if fromPlayer then g
    else { g with monsterShots := g.monsterShots.push (sx, sy, angle, range) }
  let dx := Float.cos angle
  let dy := Float.sin angle
  let crossings := shotCrossings g sx sy dx dy range
  let targets := shotTargets g sx sy dx dy range fromPlayer
  let mut aim : Option (Float × Float × Option Nat) := none
  let mut stopAt : Option Crossing := none   -- the wall that halted the shot
  match slope with
  | some s =>
    -- `PTR_ShootTraverse`: fly the settled slope, stop at the first opening
    -- the ray cannot clear, hit the first thing it passes through
    let mut ci := 0
    for tgt in targets do
      if aim.isSome || stopAt.isSome then continue
      while ci < crossings.size && crossings[ci]!.t < tgt.t && stopAt.isNone do
        let c := crossings[ci]!
        let z := sz + s * c.t
        if z ≤ c.openBot || z ≥ c.openTop then stopAt := some c
        ci := ci + 1
      if stopAt.isSome then continue
      let z := sz + s * tgt.t
      if tgt.zBot ≤ z && z ≤ tgt.zTop then aim := some (tgt.t, s, tgt.idx)
    -- nothing hit yet: keep walking the remaining walls for the puff
    if aim.isNone && stopAt.isNone then
      for k in [ci : crossings.size] do
        let c := crossings[k]!
        if stopAt.isNone then
          let z := sz + s * c.t
          if z ≤ c.openBot || z ≥ c.openTop then stopAt := some c
  | none =>
    aim := aimThrough crossings targets sz
  -- Vanilla `PTR_ShootTraverse`'s "don't shoot the sky!": no puff when the
  -- shot crossed above a sky ceiling, or struck the sky hack wall between
  -- two sky-ceilinged sectors — the bullet just flies off into the air.
  let hitSky := fun (c : Crossing) (z : Float) =>
    c.frontCeilSky && (z > c.frontCeilH || c.backCeilSky)
  match aim with
  | some (tt, slope, some i) =>
    let m := g.mobjs[i]!
    -- A spectre's fuzz (`MF_SHADOW`) does *not* deflect the player's
    -- bullets in vanilla — it only widens a monster's aim (`blurJitter`).
    let g := g.damageMobj i damage (sourceUid := sourceUid)
      (inflictor := some (sx, sy))
    let kind := if m.info.noBlood then ActorKind.puff else .blood
    return g.spawnEffect kind (sx + dx * (tt - 4)) (sy + dy * (tt - 4))
      (sz + slope * tt)
  | some (_, _, none) =>
    let g := g.damagePlayer damage (inflictor := some (sx, sy))
    return g.spawnEffect .blood g.player.x g.player.y (g.player.z + 32)
  | none =>
    -- Nothing hit. Puff against the wall that stopped the shot: at the slope
    -- it was fired along, or — with no slope settled — flying level.
    match slope, stopAt with
    | some s, some c =>
      let z := sz + s * c.t
      if hitSky c z then return g
      return g.spawnEffect .puff (sx + dx * (c.t - 4)) (sy + dy * (c.t - 4)) z
    | some _, none => return g
    | none, _ =>
      for c in crossings do
        if sz ≤ c.openBot || sz ≥ c.openTop then
          let z := sz + (4.0 - 8.0 * (c.t / range))
          if hitSky c z then return g
          return g.spawnEffect .puff (sx + dx * (c.t - 4)) (sy + dy * (c.t - 4))
            z
      return g

/-- `P_AimLineAttack` keeping hold of *what* it locked onto — distance,
slope, and target (`none` = the player) — for callers like the BFG spray
that need to hurt the thing found, not just aim at it. -/
private def aimTarget (g : GameState) (sx sy sz angle range : Float)
    (fromPlayer : Bool) : Option (Float × Float × Option Nat) :=
  let dx := Float.cos angle
  let dy := Float.sin angle
  aimThrough (shotCrossings g sx sy dx dy range)
             (shotTargets g sx sy dx dy range fromPlayer) sz

/-- The slope `P_AimLineAttack` settles on, and whether it found anything —
the aim half of a shot, with no damage done. -/
def aimLineAttack (g : GameState) (sx sy sz angle range : Float)
    (fromPlayer : Bool) : Option Float :=
  (g.aimTarget sx sy sz angle range fromPlayer).map (·.2.1)

/-- Vanilla `P_BulletSlope`: settle the vertical aim for a volley. It tries
dead ahead, then a sixty-fourth of a circle either side — that sweep is why
Doom's autoaim forgives a target you are not quite lined up on. The slope it
returns is then shared by every pellet of the shot. -/
def bulletSlope (g : GameState) (sx sy sz angle range : Float)
    (fromPlayer : Bool) : Float :=
  let sweep : Float := 6.28318530718 / 64.0    -- vanilla's `1<<26` in BAM
  match aimLineAttack g sx sy sz angle range fromPlayer with
  | some s => s
  | none =>
    match aimLineAttack g sx sy sz (angle + sweep) range fromPlayer with
    | some s => s
    | none =>
      match aimLineAttack g sx sy sz (angle - sweep) range fromPlayer with
      | some s => s
      | none => 0.0

/-- Launch a fireball of `kind` from `shooter` toward the player.

`target` is the uid the projectile itself hunts — 0 (the player) for
everything but the revenant's tracer, which homes. It is set here, on the
spawn, rather than by the caller afterwards: the caller has no handle on the
mobj it just created, and reaching for it as "the last one in the array" is
only correct for as long as nothing else spawns between the missile and the
write. A launch puff or a smoke trail added to this function would silently
send the tracer chasing the wrong thing. -/
def spawnMissile (g : GameState) (shooter : Mobj) (tx ty tz : Float)
    (kind : ActorKind := .impBall) (angleJitter : Float := 0)
    (target : Nat := 0) : GameState :=
  let sz := shooter.z + 32
  let dist := max 1.0 (shooter.distanceTo tx ty)
  let angle := Float.atan2 (ty - shooter.y) (tx - shooter.x) + angleJitter
  let info := ActorInfo.ofKind kind
  -- each projectile leaves the barrel with its own launch cue (vanilla plays
  -- the missile's `seesound`); the revenant's tracer is silent here
  let g := match kind.launchSfx with
    | some s => g.playSound s shooter.x shooter.y
    | none => g
  let (g, i) := g.spawn kind shooter.x shooter.y angle
  { g with mobjs := g.mobjs.modify i fun m =>
      { m with z := sz
               momX := info.speed * Float.cos angle
               momY := info.speed * Float.sin angle
               momZ := (tz - sz) / dist * info.speed
               shooterUid := shooter.uid, target } }

/-- Barrel/rocket blast: up to `maxDamage` at the center, fading with
distance, sight-checked like vanilla so walls shield. `sourceUid` is the mobj
that set off the blast (0 = player/none), so caught monsters turn on it. -/
def radiusDamage (g : GameState) (x y z : Float) (maxDamage : Nat)
    (sourceUid : Nat := 0) : GameState := Id.run do
  let mut g := g
  let reach := Float.ofNat maxDamage
  for i in g.mobjsNear x y reach do
    let m := g.mobjs[i]!
    if m.removed || !m.shootable then continue
    -- vanilla `PIT_RadiusAttack`: the cyberdemon and the spider mastermind
    -- take no blast damage at all — a cyberdemon cannot rocket itself down
    if m.kind == .cyberdemon || m.kind == .spiderMastermind then continue
    -- vanilla `PIT_RadiusAttack` measures to the *square* body's edge:
    -- Chebyshev `max |dx| |dy|`, minus the thing's radius, floored at 0
    let dist := max 0.0
      (max (Float.abs (m.x - x)) (Float.abs (m.y - y)) - m.info.radius)
    if dist ≥ reach then continue
    if !g.level.checkSight x y (z + 16) m.x m.y (m.z + m.info.height / 2) then
      continue
    g := g.damageMobj i (maxDamage - dist.toUInt64.toNat)
      (sourceUid := sourceUid) (inflictor := some (x, y))
  if !g.status.dead then
    let p := g.player
    let dist := max 0.0
      (max (Float.abs (p.x - x)) (Float.abs (p.y - y)) - Player.radius)
    if dist < reach
        && g.level.checkSight x y (z + 16) p.x p.y (p.z + 32) then
      g := g.damagePlayer (maxDamage - dist.toUInt64.toNat)
        (inflictor := some (x, y))
  return g

/-- The BFG spray (vanilla `A_BFGSpray`, run by the burst's third state —
16 tics after the ball lands): forty independent aim-traces from the
*shooter* — not from where the ball burst — fanned over 90° of the ball's
heading, 2.25° apart. Each ray that locks a target rolls its own 15d8 and
leaves the green flare on it, so a big target filling the fan soaks many
rays at once, and a monster standing in front shields the ones behind it,
ray by ray. -/
def bfgSpray (g : GameState) (m : Mobj) : GameState := Id.run do
  let mut g := g
  let p := g.player
  -- vanilla traces with the shooter's `shootz`
  let sz := p.shootZ
  for i in [0:40] do
    -- `mo->angle - ANG90/2 + ANG90/40*i`: the fan starts 45° left of the
    -- ball's heading and steps 2.25° right per ray
    let a := m.angle - 0.78539816340 + 0.03926990817 * Float.ofNat i
    -- a full aim-traverse over vanilla's 16*64 = 1024 spray range: the
    -- ±0.625 window narrows through wall openings, the first target that
    -- fits takes the ray, and geometry shields whatever is behind it
    if let some (_, _, some j) := g.aimTarget p.x p.y sz a 1024 true then
      -- the green flare vanilla leaves on each thing the spray catches
      let t := g.mobjs[j]!
      g := (g.spawn .bfgPuff t.x t.y t.angle).1
      let (dmg, g') := g.randDice 15 8      -- 15d8: up to 120 per ray
      g := g'.damageMobj j dmg (inflictor := some (p.x, p.y))
  return g

/-- Vanilla `P_SpawnPlayerMissile`'s aim: a full aim-traverse at the view
angle, and if that locks nothing, again a sixty-fourth of a circle
(`1<<26` BAM) to either side. Whichever try finds a target sets *both* the
missile's slope and its angle — the rocket flies toward the monster you
almost faced, not straight ahead past it. This is the only reason a rocket
reaches a monster on a ledge, or a BFG ball anything that flies. No target
on any try: the view angle, flying level. -/
private def missileAim (g : GameState) : Float × Float := Id.run do
  let p := g.player
  let sz := p.shootZ                          -- vanilla's `shootz`
  let sweep : Float := 6.28318530718 / 64.0
  for a in #[p.angle, p.angle + sweep, p.angle - sweep] do
    -- vanilla aims player missiles over 16*64 = 1024, not the full
    -- MISSILERANGE the shot itself can fly
    if let some s := g.aimLineAttack p.x p.y sz a 1024 true then
      return (a, s)
  return (p.angle, 0.0)

/-- Launch a missile of `kind` from the player, along the autoaim's angle
and slope. Marked `fromPlayer` so it hurts monsters, not its owner. -/
def spawnPlayerMissile (g : GameState) (kind : ActorKind) : GameState :=
  let p := g.player
  let (a, slope) := g.missileAim
  let info := ActorInfo.ofKind kind
  -- (the firing sound is played by `fireWeapon`, per weapon)
  -- Spawn at the player, not ahead of them: a fixed forward offset lands
  -- *past* a wall you're pressed against, so the missile skips it and never
  -- detonates. Starting here lets `moveMissile` cross that wall on tic one
  -- and burst on the near side (splashing the player, as in vanilla).
  let (g, i) := g.spawn kind p.x p.y a
  { g with mobjs := g.mobjs.modify i fun m =>
      { m with z := p.z + 32
               momX := info.speed * Float.cos a
               momY := info.speed * Float.sin a
               momZ := info.speed * slope
               fromPlayer := true, shooterUid := 0 } }

/-- Explode a missile in place: switch to its death animation and, for
rockets, blast the neighborhood; for the BFG ball, spray. -/
def explodeMissile (g : GameState) (i : Nat) : GameState :=
  let m := g.mobjs[i]!
  -- each missile dies with its own `deathsound`: DSBAREXP for rockets and
  -- the revenant's tracer, DSRXPLOD for the BFG ball, DSFIRXPL for the rest
  let g := g.playSound ((m.kind.deathSfx).getD Sfx.fireExpl) m.x m.y
  let g := if m.info.blastRadius > 0
    then g.radiusDamage m.x m.y m.z m.info.blastRadius
           (sourceUid := if m.fromPlayer then 0 else m.shooterUid) else g
  match m.info.deathState with
  | some death => g.setMobj i { (g.mobjs[i]!.setState death) with
      corpse := true, momX := 0, momY := 0, momZ := 0 }
  | none => g.setMobj i { m with removed := true }

/-- The closest a swept point comes to `(px, py)` in the Chebyshev metric
`max |dx| |dy|` — the distance vanilla's *square* bodies collide by
(`PIT_CheckThing` tests `|dx|` and `|dy|` against the summed radii).
Sweeping the whole tic keeps a fast missile from stepping clean over a
thin target between two tics. The Chebyshev distance is convex along the
segment, so its minimum sits at an endpoint, where one axis's own distance
bottoms out, or where the two axes' distances cross — five candidates. -/
private def segDistCheby (x1 y1 x2 y2 px py : Float) : Float := Id.run do
  let ox := x1 - px
  let oy := y1 - py
  let dx := x2 - x1
  let dy := y2 - y1
  let distAt := fun (t : Float) =>
    max (Float.abs (ox + t * dx)) (Float.abs (oy + t * dy))
  let mut best := min (distAt 0) (distAt 1)
  for c in #[if dx != 0 then some (-ox / dx) else none,
             if dy != 0 then some (-oy / dy) else none,
             if dx - dy != 0 then some ((oy - ox) / (dx - dy)) else none,
             if dx + dy != 0 then some ((-oy - ox) / (dx + dy)) else none] do
    if let some t := c then
      if 0 < t && t < 1 then best := min best (distAt t)
  return best

/-- Fly a missile one tic; explode on wall, thing, player, floor, ceiling. -/
def moveMissile (g : GameState) (i : Nat) : GameState := Id.run do
  let m := g.mobjs[i]!
  let nx := m.x + m.momX
  let ny := m.y + m.momY
  let nz := m.z + m.momZ
  -- Things and the player: tested against the whole tic's path, not just its
  -- endpoint — a plasma bolt covers 25 units a tic, more than a small
  -- monster's diameter, and an endpoint test would tunnel straight through.
  -- Anything within `reach` of that path is within half its length plus
  -- `reach` of its midpoint, so one query around the midpoint covers it.
  let midX := (m.x + nx) / 2
  let midY := (m.y + ny) / 2
  let halfLen := Float.sqrt ((nx - m.x) ^ 2 + (ny - m.y) ^ 2) / 2
  for j in g.mobjsNear midX midY (halfLen + m.info.radius) do
    let t := g.mobjs[j]!
    if t.removed || t.uid == m.uid || t.uid == m.shooterUid then continue
    if !(t.solid || t.shootable) then continue
    let reach := t.info.radius + m.info.radius
    if segDistCheby m.x m.y nx ny t.x t.y ≥ reach then continue
    if nz > t.z + t.info.height || nz + m.info.height < t.z then continue
    let mut g := g
    -- a monster's missile passes harmlessly through its own kind (and the
    -- Knight/Baron pair): it still bursts on contact, but deals no damage —
    -- only the player is ever hit by same-species fire (vanilla PIT_CheckThing)
    let sameKind := !m.fromPlayer &&
      ((g.mobjIdx? m.shooterUid).map (g.mobjs[·]!)).any (sameSpecies ·.kind t.kind)
    if t.shootable && !sameKind then
      let (n, sides) := m.info.damageDice
      let (dmg, g') := g.randDice n sides
      -- a fellow monster hit by this missile turns on the shooter
      g := g'.damageMobj j (dmg * m.info.damageMult)
        (sourceUid := if m.fromPlayer then 0 else m.shooterUid)
        (inflictor := some (nx, ny))
    return g.explodeMissile i
  if m.shooterUid != 0 && !m.fromPlayer && !g.status.dead then
    let p := g.player
    let reach := Player.radius + m.info.radius
    if segDistCheby m.x m.y nx ny p.x p.y < reach
        && nz ≤ p.z + Player.height && nz + m.info.height ≥ p.z then
      let (n, sides) := m.info.damageDice
      let (dmg, g) := g.randDice n sides
      let g := g.damagePlayer (dmg * m.info.damageMult) (inflictor := some (nx, ny))
      return g.explodeMissile i
  -- walls, floor, ceiling: a missile flies at any height, so it is blocked
  -- by a crossed line only when it's outside that line's vertical opening
  let sec := g.level.sectors[g.level.sectorAt nx ny]!
  if nz ≤ sec.floorH || nz + m.info.height ≥ sec.ceilH then
    return g.explodeMissile i
  for li in (g.level.linesNear ((m.x + nx) / 2) ((m.y + ny) / 2)
      (Float.abs m.momX + Float.abs m.momY + m.info.radius)) do
    let line := g.level.linedefs[li]!
    -- the direction is this tic's whole movement, so `t` is a fraction of it
    let some t := g.level.rayHitsLine line m.x m.y m.momX m.momY | continue
    if t ≤ 0 || t > 1 then continue
    match line.back with
    | none => return g.explodeMissile i
    | some back =>
      let f := g.level.sectors[g.level.sidedefs[line.front]!.sector]!
      let b := g.level.sectors[g.level.sidedefs[back]!.sector]!
      if nz + m.info.height ≥ min f.ceilH b.ceilH then
        -- Vanilla `P_XYMovement`'s sky hack: a missile stopped by a
        -- ceiling drop whose far side is sky has flown off into the sky —
        -- it is removed silently rather than exploding on thin air.
        -- ("Does not handle sky floors", says the original; nor does this.)
        if b.ceilFlat == Render.skyFlat then
          return g.setMobj i { m with removed := true }
        return g.explodeMissile i
      if nz ≤ max f.floorH b.floorH then
        return g.explodeMissile i
  return g.setMobj i { m with x := nx, y := ny, z := nz }

end GameState
end Dill
