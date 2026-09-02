import Dill.Game.Combat
import Dill.Game.Specials

/-!
# Monster brains

`P_Enemy` distilled. Dormant monsters `look` for the player (sight within a
half-circle, or recent gunfire noise). Woken monsters `chase`: shamble
toward the player in 45° compass steps (`newChaseDir` is vanilla's dance,
fallbacks and all), and periodically bite or shoot.

All the state lives in the mobj: `moveDir`/`moveCount` drive the shamble,
`justAttacked` forces movement between attacks.
-/

namespace Dill

namespace GameState

private def angleTo (m : Mobj) (x y : Float) : Float :=
  Float.atan2 (y - m.y) (x - m.x)

private def playerDist (g : GameState) (m : Mobj) : Float :=
  m.distanceTo g.player.x g.player.y

private def seesPlayer (g : GameState) (m : Mobj) : Bool :=
  g.level.checkSight m.x m.y (m.z + m.info.height * 0.75)
    g.player.x g.player.y (g.player.z + Player.viewHeight)

/-! ## Targets — a monster hunts the player, or another monster it's
feuding with (infighting). -/

/-- What `m`'s `target` uid resolves to right now. Vanilla's attack code
guards on `!actor->target || actor->target->health <= 0`; the uid scheme
(0 = the player) needs the same three-way split spelled out, or "target
gone" silently collapses into "target is the player" and an infight
attack whose victim just died lands on the player instead. -/
private inductive EnemyTarget where
  /-- Hunting the player (uid 0, the default). -/
  | player
  /-- Feuding with a live monster, at this `mobjs` index. -/
  | mobj (i : Nat)
  /-- The target uid names a monster that died or was removed. Attacks
  fizzle and refires break off, as vanilla's health check makes them; only
  `aChase` may swing the monster onto the player from here, and only with
  actual sight. -/
  | gone

private def enemyTarget (g : GameState) (m : Mobj) : EnemyTarget :=
  if m.target == 0 then .player
  else match g.mobjIdx? m.target with
    | some i =>
      let t := g.mobjs[i]!
      if !t.removed && t.shootable then .mobj i else .gone
    | none => .gone

/-- Where `m`'s current enemy is (centre-of-mass height); `none` when the
target is gone, so a caller fizzles its attack rather than aim it at the
player. -/
private def enemyPos (g : GameState) (m : Mobj) :
    Option (Float × Float × Float) :=
  match enemyTarget g m with
  | .player => some (g.player.x, g.player.y, g.player.z + Player.viewHeight)
  | .mobj i => let t := g.mobjs[i]!; some (t.x, t.y, t.z + t.info.height / 2)
  | .gone => none

/-- Distance to the enemy; a gone target counts as infinitely far, so no
range check ever passes against it. -/
private def enemyDist (g : GameState) (m : Mobj) : Float :=
  match enemyPos g m with
  | some (x, y, _) => m.distanceTo x y
  | none => 1.0e30

private def enemyAlive (g : GameState) (m : Mobj) : Bool :=
  match enemyTarget g m with
  | .player => !g.status.dead
  | .mobj _ => true
  | .gone => false

/-- Nothing sees a gone target: this one `false` is what fizzles the
arch-vile's flame, and freezes `aFire`, when the victim dies mid-wind-up. -/
private def seesEnemy (g : GameState) (m : Mobj) : Bool :=
  match enemyPos g m with
  | some (x, y, z) =>
    g.level.checkSight m.x m.y (m.z + m.info.height * 0.75) x y z
  | none => false

/-- Vanilla `P_CheckMeleeRange`: the enemy has to be inside `MELEERANGE - 20`
of its own edge — 44 plus its radius — *and* in sight. The sight test is what
stops a monster biting through a wall; the distance test runs first because it
is far cheaper and is almost always the one that fails. -/
private def inMeleeRange (g : GameState) (m : Mobj) : Bool :=
  match enemyTarget g m with
  | .player =>
    m.distanceTo g.player.x g.player.y
      < Player.meleeRange - 20.0 + Player.radius && g.seesEnemy m
  | .mobj i =>
    let t := g.mobjs[i]!
    m.distanceTo t.x t.y
      < Player.meleeRange - 20.0 + t.info.radius && g.seesEnemy m
  | .gone => false

/-- Is `m`'s enemy blurred — an invisible player, or a spectre it's feuding
with? Vanilla's `MF_SHADOW`. -/
private def enemyBlurred (g : GameState) (m : Mobj) : Bool :=
  match enemyTarget g m with
  | .mobj i => g.mobjs[i]!.info.shadow
  | .player => g.status.invisTics > 0
  | .gone => false

/-- The aim error against a blurred enemy: a wide random angle, or nothing
against a clear target. Vanilla's `A_FaceTarget` bends the shot by
`(P_Random-P_Random)<<21` at a `MF_SHADOW` foe — one BAM step is `2π/2048`
radian, so `randDiff` scales by that. It is exactly twice the ordinary attack
spread (`<<20`, `2π/4096`), which is what makes the blursphere worth grabbing;
the old `0.0008` was a quarter of this and barely widened the shot. -/
private def blurJitter (g : GameState) (m : Mobj) : Float × GameState :=
  if g.enemyBlurred m then
    let (s, g) := g.randDiff
    (Float.ofInt s * 0.00306796, g)
  else (0.0, g)

/-- Deal `dmg` to `m`'s enemy — the player, or a target monster (which then
retaliates against `m`). A gone target takes the blow with it: the damage
lands on no one, exactly vanilla's `P_DamageMobj` refusing a corpse. -/
private def damageEnemy (g : GameState) (m : Mobj) (dmg : Nat) : GameState :=
  match enemyTarget g m with
  | .mobj i => g.damageMobj i dmg (sourceUid := m.uid) (inflictor := some (m.x, m.y))
  | .player => g.damagePlayer dmg (inflictor := some (m.x, m.y))
  | .gone => g

/-- Vanilla `P_Move`'s blocked-move door bid: the special lines the refused
step touched go to `P_UseSpecialLine`, and if one starts something the move
still counts as "good" — the monster stands and waits for the door rather
than turning away. The non-player gate there (p_switch.c) is strict: an
`ML_SECRET` line is refused outright, and of the manual doors only the
plain DR type 1 survives — the locked 32/33/34 pass the gate only to die at
`EV_VerticalDoor`'s key check, a monster carrying no keys — so only
ordinary doors ever open for monsters. -/
private def tryUseLines (g : GameState) (m : Mobj) (nx ny : Float) :
    Option GameState := Id.run do
  for li in g.level.linesNear nx ny (m.info.radius + 1) do
    let line := g.level.linedefs[li]!
    if line.special != 1 || line.has 0x20 then continue  -- 0x20 = ML_SECRET
    if !g.level.boxCrossesLine line nx ny m.info.radius then continue
    -- A door already in motion is left alone: vanilla parks the blocked
    -- monster at DI_NODIR while it opens, so it can never re-press a DR
    -- line and reverse a door it just opened.
    let some back := line.back | continue
    if g.movers.any (·.sector == g.level.sidedefs[back]!.sector) then continue
    if let some g' := g.activateLineOpt li (byUse := true) then
      return some g'
  return none

/-- One compass step in `moveDir`: walls, steps ≤ 24, no big drop-offs,
and no walking into other things. Monsters glue to the floor. -/
private def tryWalk (g : GameState) (i : Nat) : GameState × Bool :=
  let m := g.mobjs[i]!
  let nx := m.x + m.info.speed * Float.cos (dirAngle m.moveDir)
  let ny := m.y + m.info.speed * Float.sin (dirAngle m.moveDir)
  let flying := m.info.flying
  -- a blocked step's last resort: put any touched door line to use, and
  -- wait on the spot if that opened one (see `tryUseLines`)
  let blocked := fun (g : GameState) =>
    match tryUseLines g m nx ny with
    | some g' => (g', true)
    | none => (g, false)
  let result := g.level.checkBody nx ny m.z m.info.radius m.info.height
    (maxStep := if flying then 1.0e9 else Player.maxStep)
    (maxDrop := if flying then none else some 24) (isMonster := true)
  match result with
  | none =>
    -- Vanilla `floatok` (`P_Move`): a flyer refused only because its
    -- altitude is wrong — the opening ahead is tall enough, its body is
    -- just above (a doorway lintel) or below it — floats FLOATSPEED
    -- toward the opening's floor this tic instead of giving up, staying
    -- inside its current sector's slab. Without this a cacodemon hovers
    -- against an open door forever, too high to fit through.
    if flying then
      match g.level.openingNear nx ny m.info.radius (isMonster := true) with
      | some (fz, cz) =>
        if cz - fz ≥ m.info.height then
          let sec := g.level.sectors[g.level.sectorAt m.x m.y]!
          let z := if m.z < fz then m.z + Speeds.floatDrift
                   else m.z - Speeds.floatDrift
          let z := max sec.floorH (min (sec.ceilH - m.info.height) z)
          (g.setMobj i { m with z }, true)
        else blocked g
      | none => blocked g
    else blocked g
  | some floorZ =>
    if g.mobjBlocked m.uid nx ny m.info.radius then (g, false)
    else
      -- walkers glue to the floor; floaters hold altitude (popped up if
      -- the terrain rises into them)
      let z := if flying then max m.z floorZ else floorZ
      let g := g.setMobj i { m with x := nx, y := ny, z }
      -- a step that crossed a line works its special, as `P_TryMove` does
      -- for anything that moves — this is how monsters take teleporters
      (g.crossSpecialsMobj i m.x m.y, true)

/-- Vanilla `P_NewChaseDir`: the diagonal toward the enemy first, then the
two axes, then carry on the way we were already going, then sweep, then turn
around; walk a random 0–15 tics before rethinking. -/
private def newChaseDir (g : GameState) (i : Nat) : GameState := Id.run do
  let m := g.mobjs[i]!
  -- Vanilla steers by `actor->target`, which during an infight is the
  -- monster being feuded with — not the player. Reading the player here sent
  -- a monster walking away from the thing it was actually fighting. A gone
  -- target steers nowhere: both axes stay `none` and the monster holds its
  -- heading or casts about — it never beelines blind for the player.
  let (dx, dy) := match g.enemyPos m with
    | some (ex, ey, _) => (ex - m.x, ey - m.y)
    | none => (0.0, 0.0)
  -- compass: 0=E 1=NE 2=N 3=NW 4=W 5=SW 6=S 7=SE
  let dirEW : Option Nat := if dx > 10 then some 0 else if dx < -10 then some 4 else none
  let dirNS : Option Nat := if dy > 10 then some 2 else if dy < -10 then some 6 else none
  let turnaround := (m.moveDir + 4) % 8
  let mut candidates : Array Nat := #[]
  -- diagonal first
  match dirEW, dirNS with
  | some ew, some ns =>
    candidates := candidates.push (match ew, ns with
      | 0, 2 => 1 | 0, 6 => 7 | 4, 2 => 3 | _, _ => 5)
  | _, _ => pure ()
  -- Then the two axes. Vanilla leads with east/west and swaps when the
  -- north/south gap is the wider one — or on a 55-in-256 roll, so a monster
  -- pressed against one axis still tries the other from time to time.
  let (swapRoll, gSwap) := g.rand
  let mut g := gSwap
  let axes := if swapRoll > 200 || Float.abs dy > Float.abs dx
              then #[dirNS, dirEW] else #[dirEW, dirNS]
  for a in axes do
    if let some d := a then candidates := candidates.push d
  -- With no direct route open, vanilla holds its current heading before it
  -- starts casting about — without this a blocked monster flails instead of
  -- following the wall it was already walking along.
  candidates := candidates.push m.moveDir
  -- random sweep, then turnaround as a last resort
  let (roll, g') := g.rand
  g := g'
  if roll &&& 1 == 0 then
    for d in [0:8] do candidates := candidates.push d
  else
    for d in [0:8] do candidates := candidates.push (7 - d)
  -- turnaround is tried only when everything else failed
  let ordered := (candidates.filter (· != turnaround)).push turnaround
  for d in ordered do
    let (roll2, g2) := g.rand
    g := g2
    let m := g.mobjs[i]!
    let g2 := g.setMobj i { m with moveDir := d, moveCount := roll2 &&& 15 }
    let (g3, moved) := tryWalk g2 i
    if moved then return g3
    g := g.setMobj i m  -- restore direction; try the next candidate
  -- boxed in: stand still a moment
  return g.setMobj i { g.mobjs[i]! with moveCount := 0 }

/-- Wake a monster up: snarl and give chase. The snarl is rolled among the
kind's variants (`Sfx.varied`), so a squad of zombiemen does not bark in
chorus. -/
private def wake (g : GameState) (i : Nat) : GameState :=
  let m := g.mobjs[i]!
  let (g, sfx) := match m.kind.sightSfx with
    | some s => let (roll, g) := g.rand; (g, some (Sfx.varied s roll))
    | none => (g, none)
  let g := match sfx with
    | some s =>
      -- Vanilla plays the Cyberdemon's and Spider Mastermind's sight roars
      -- with a NULL origin — full volume everywhere. DILL approximates by
      -- sounding them at the player, where distance attenuation is zero.
      if m.kind == ActorKind.cyberdemon || m.kind == ActorKind.spiderMastermind then
        g.playSound s g.player.x g.player.y
      else g.playSound s m.x m.y
    | none => g
  let m := m.setState (m.info.seeState.getD m.state)
  g.setMobj i { m with awake := true, moveCount := 0 }

/-- `A_Look`, in vanilla's two stages.

First the sector's `soundtarget` — the alert flooded through the map's
geometry by a shot. A monster that hears one wakes on the noise alone; a
deaf (`MF_AMBUSH`) one still answers it, but only if it can *see* the
player, and then from any direction — the facing cone belongs to the second
stage, not this one.

Failing that, `P_LookForPlayers`: the player must be in sight and within
±90° of the way the monster faces, unless they are inside melee range, at
which point it notices regardless. -/
def aLook (g : GameState) (i : Nat) : GameState :=
  if g.status.dead then g else
  let m := g.mobjs[i]!
  let p := g.player
  let alerted := g.alerted[g.level.sectorAt m.x m.y]?.getD false
  let byNoise := alerted && (!m.ambush || g.seesPlayer m)
  let facing :=
    let rel := Float.abs (wrapAngle ((angleTo m p.x p.y) - m.angle))
    rel ≤ 1.5707963 || g.playerDist m < 64
  if byNoise || (facing && g.seesPlayer m) then g.wake i else g

/-- Vanilla `P_CheckMissileRange`: eagerness falls off with distance — the
roll is against the distance itself, so the further away the quarry the more
often a monster holds fire. A monster that was just wounded fights back at
once (`MF_JUSTHIT`); one still inside its reaction delay holds fire.

The per-monster adjustments are vanilla's own, and they are what give the
heavies their character: the Cyberdemon, the Spider Mastermind and the lost
soul halve the distance and so fire about twice as readily; the revenant
refuses to shoot inside 196 (it punches instead) and halves beyond that; the
arch-vile will not start its flame past 896; and the Cyberdemon's ceiling is
160 rather than 200. -/
private def checkMissileRange (g : GameState) (m : Mobj) :
    Bool × GameState :=
  if !g.seesEnemy m then (false, g) else
  if m.justHit then (true, g) else
  if m.reactionTime > 0 then (false, g) else
  let dist := g.enemyDist m - 64
  let dist := if m.info.meleeState.isNone then dist - 128 else dist
  -- the arch-vile simply will not attack from across a hall
  if m.kind == .archVile && dist > 14 * 64 then (false, g) else
  -- the revenant closes to punching range instead of firing
  if m.kind == .revenant && dist < 196 then (false, g) else
  let dist := match m.kind with
    | .revenant | .cyberdemon | .spiderMastermind | .lostSoul => dist / 2
    | _ => dist
  let dist := min 200.0 dist
  let dist := if m.kind == .cyberdemon then min 160.0 dist else dist
  let (roll, g) := g.rand
  ((Float.ofNat roll ≥ dist), g)

/-- A_Chase: the heartbeat of a woken monster. -/
def aChase (g : GameState) (i : Nat) : GameState := Id.run do
  let mut g := g
  let m := g.mobjs[i]!
  -- A missile attack forces a repositioning step first — except on
  -- Nightmare, where vanilla skips the reposition (the monster presses
  -- straight on) though clearing the flag still costs it this A_Chase.
  if m.justAttacked then
    g := g.setMobj i { m with justAttacked := false }
    if g.skill == 5 then return g
    return g.newChaseDir i
  -- count down the reaction delay (holds ranged fire after waking) and the
  -- infighting threshold (keeps the monster fixed on its enemy for a while);
  -- a threshold lapses at once if that enemy is dead or gone
  let m := if m.reactionTime > 0 then { m with reactionTime := m.reactionTime - 1 } else m
  let m := if m.threshold > 0 then
             { m with threshold :=
                 match enemyTarget g m with
                 | .gone => 0
                 | _ => m.threshold - 1 }
           else m
  g := g.setMobj i m
  -- The infight target has died or vanished: vanilla looks for the player
  -- with `P_LookForPlayers (allaround=true)` — which demands actual SIGHT,
  -- only the facing cone waived — and swings onto them if seen. Unseen, the
  -- monster stands down to its spawn state over the corpse, where `A_Look`
  -- re-wakes it on sight or noise. It never beelines blind.
  if let .gone := enemyTarget g m then
    if !g.status.dead && g.seesPlayer m then
      return g.setMobj i { m with target := 0 }
    return g.setMobj i { (m.setState m.info.spawnState) with
      target := 0, awake := false }
  -- Hunting the player and the player is dead: `P_LookForPlayers` comes up
  -- empty the same way, and the monster stands down over the corpse.
  if !g.enemyAlive m then
    return g.setMobj i (m.setState m.info.spawnState)
  -- Melee? (vanilla sets MF_JUSTATTACKED for missile attacks only.)
  -- Entering the melee state plays the monster's `attacksound`: of the
  -- melee-capable kinds only the demon and spectre carry one — the bark —
  -- while the imp, revenant and barons close in silently.
  if m.info.meleeState.isSome && g.inMeleeRange m then
    if m.kind == .demon || m.kind == .spectre then
      g := g.playSound Sfx.sargAtk m.x m.y
    return g.setMobj i (m.setState m.info.meleeState.get!)
  -- Ranged? Below Nightmare — which is also where DILL folds vanilla's
  -- -fast — a monster mid-walk (`movecount` still running) does not even
  -- consider a missile; it only shoots from a move boundary. That
  -- walk-then-shoot rhythm is most of the cadence of a vanilla firefight.
  if m.info.missileState.isSome && (m.moveCount == 0 || g.skill == 5) then
    let (attack, g') := g.checkMissileRange m
    g := g'
    if attack then
      let m := g.mobjs[i]!
      let m := m.setState m.info.missileState.get!
      return g.setMobj i { m with justAttacked := true, justHit := false }
  -- an idle mutter now and then while hunting (vanilla A_Chase: 3/256)
  let (act, gA) := g.rand
  g := gA
  if act < 3 then
    let m := g.mobjs[i]!
    if let some s := m.kind.activeSfx then g := g.playSound s m.x m.y
  -- shamble on
  let m := g.mobjs[i]!
  if m.moveCount == 0 then
    return g.newChaseDir i
  -- Vanilla's `A_Chase` turns toward `movedir` a step at a time — it snaps
  -- the angle to the 45° grid and moves one notch per tic — rather than
  -- flipping straight to it. A monster changing direction visibly swings
  -- round, and shows the sprite rotations in between.
  let want := dirAngle m.moveDir
  let turn := Id.run do
    let step := 3.14159265358979 / 4.0
    -- snap to the grid, then close the gap by at most one notch
    let cur := Float.ofInt (ifloor (m.angle / step + 0.5)) * step
    let d := wrapAngle (want - cur)
    if Float.abs d < step * 0.5 then return want
    return cur + (if d > 0 then step else -step)
  g := g.setMobj i { m with moveCount := m.moveCount - 1, angle := turn }
  let (g', moved) := tryWalk g i
  g := g'
  -- Floaters drift toward the enemy's level — but only when horizontally
  -- close relative to the height gap (vanilla `P_ZMovement`'s
  -- `dist < 3·|delta|` gate). Far from the enemy the drift stays out of
  -- the way, which is what lets the blocked-move float in `tryWalk` duck
  -- a cacodemon under a doorway instead of being pushed back up.
  let m := g.mobjs[i]!
  if m.info.flying then
    -- (the target survived the gone-check above, but a teleport special
    -- worked by `tryWalk` this very tic can still have telefragged it)
    if let some (ex, ey, ez) := g.enemyPos m then
      let dz := ez - m.z
      let dist := Float.abs (ex - m.x) + Float.abs (ey - m.y)
      if dist < 3 * Float.abs dz then
        let sec := g.level.sectors[g.level.sectorAt m.x m.y]!
        let z := max sec.floorH (min (sec.ceilH - m.info.height)
          (m.z + max (-Speeds.floatDrift) (min Speeds.floatDrift dz)))
        g := g.setMobj i { m with z }
  if !moved then
    return g.newChaseDir i
  return g

def aFaceTarget (g : GameState) (i : Nat) : GameState :=
  let m := g.mobjs[i]!
  -- a gone target turns nobody's head (vanilla `if (!actor->target) return`)
  match g.enemyPos m with
  | some (x, y, _) => g.setMobj i { m with angle := angleTo m x y }
  | none => g

/-! Monster attacks. Spreads and damage dice are vanilla's. -/

/-- One scattered monster bullet: ±22° wander (wider at a blurred enemy),
3×1d5, fired along a `slope` the caller settled once for the whole burst —
vanilla settles it before the pellet loop. -/
private def monsterShot (g : GameState) (m : Mobj) (sz slope : Float) :
    GameState :=
  let (spread, g) := g.randDiff
  let (jit, g) := g.blurJitter m
  let (dmg, g) := g.randDice 1 5
  g.lineAttack m.x m.y sz
    (m.angle + Float.ofInt spread * 0.001534 + jit) Player.missileRange (dmg * 3) false
    (sourceUid := m.uid) (slope := some slope)

/-- The zombie family's shared attack: face the enemy, bark, aim, loose
`shots` bullets. Vanilla aims down the true facing, *then* scatters each
shot — monsters take a single `P_AimLineAttack`; the three-angle sweep of
`P_BulletSlope` belongs to the player alone. -/
private def zombieAttack (g : GameState) (i : Nat) (sfx : Nat)
    (shots : Nat := 1) : GameState := Id.run do
  let mut g := g.aFaceTarget i
  let m := g.mobjs[i]!
  g := g.playSound sfx m.x m.y
  let sz := m.z + m.info.height / 2 + 8
  let slope := (aimLineAttack g m.x m.y sz m.angle Player.missileRange false).getD 0.0
  for _ in [0:shots] do
    g := monsterShot g m sz slope
  return g

/-- Zombieman: one bullet. -/
def aPosAttack (g : GameState) (i : Nat) : GameState :=
  zombieAttack g i Sfx.pistol

/-- Shotgun guy: three of the same, over one shared aim. -/
def aSPosAttack (g : GameState) (i : Nat) : GameState :=
  zombieAttack g i Sfx.shotgun (shots := 3)

/-- Imp: claw in melee, fireball otherwise — or neither, when the enemy is
gone (the melee test fails and there is nowhere to throw the ball). -/
def aTroopAttack (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  if g.inMeleeRange m then
    let g := g.playSound Sfx.claw m.x m.y
    let (dmg, g) := g.randDice 1 8
    g.damageEnemy m (dmg * 3)
  else match g.enemyPos m with
    | none => g
    | some (tx, ty, tz) =>
      let (jit, g) := g.blurJitter m
      g.spawnMissile m tx ty tz (angleJitter := jit)

/-- Cacodemon: bite up close, ball of lightning otherwise. -/
def aHeadAttack (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  if g.inMeleeRange m then
    -- no bite sound: vanilla's `A_HeadAttack` plays nothing at all
    let (dmg, g) := g.randDice 1 6
    g.damageEnemy m (dmg * 10)
  else match g.enemyPos m with
    | none => g
    | some (tx, ty, tz) =>
      let (jit, g) := g.blurJitter m
      g.spawnMissile m tx ty tz .cacoBall (angleJitter := jit)

/-- Baron of Hell: a raking claw or a green fireball. -/
def aBruisAttack (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  if g.inMeleeRange m then
    let g := g.playSound Sfx.claw m.x m.y
    let (dmg, g) := g.randDice 1 8
    g.damageEnemy m (dmg * 10)
  else match g.enemyPos m with
    | none => g
    | some (tx, ty, tz) =>
      let (jit, g) := g.blurJitter m
      g.spawnMissile m tx ty tz .baronBall (angleJitter := jit)

/-- Cyberdemon: a rocket, and nothing at all up close. -/
def aCyberAttack (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  match g.enemyPos m with
  | none => g
  | some (tx, ty, tz) =>
    let (jit, g) := g.blurJitter m
    g.spawnMissile m tx ty tz .rocket (angleJitter := jit)

/-- A_SpidRefire: hold the trigger as long as the shot stays good. Vanilla
rolls a 10-in-256 chance to keep firing regardless, then breaks off to the
see state once the target is dead or out of sight. -/
def aSpidRefire (g : GameState) (i : Nat) : GameState := Id.run do
  let mut g := g.aFaceTarget i
  let (roll, g') := g.rand
  g := g'
  if roll < 10 then return g
  let m := g.mobjs[i]!
  -- break off when the target is dead *or* out of sight (vanilla checks both)
  if g.enemyAlive m && g.seesEnemy m then return g
  return g.setMobj i (m.setState (m.info.seeState.getD m.state))

/-- Chaingunner and SS: one bullet of a held burst. Same dice as the
zombieman's shot, over the shotgun's report. -/
def aCPosAttack (g : GameState) (i : Nat) : GameState :=
  zombieAttack g i Sfx.shotgun

/-- `A_CPosRefire`: as the spider's, but it lets go four times as readily.
Vanilla checks sight as well as life (`!P_CheckSight` in `A_CPosRefire`) —
without it a chaingunner holds its burst on a target that has stepped behind
a wall and never stops firing. -/
def aCPosRefire (g : GameState) (i : Nat) : GameState := Id.run do
  let mut g := g.aFaceTarget i
  let (roll, g') := g.rand
  g := g'
  if roll < 40 then return g
  let m := g.mobjs[i]!
  if g.enemyAlive m && g.seesEnemy m then return g
  return g.setMobj i (m.setState (m.info.seeState.getD m.state))

/-- `FATSPREAD` = ANG90/8 = 11.25°. The three mancubus attack frames each
loose two `MT_FATSHOT`s at these offsets from the aim, together an
asymmetric fan (vanilla `A_FatAttack1/2/3`), not one pair thrown thrice. -/
private def fatSpread : Float := 0.19635

private def fatShots (g : GameState) (i : Nat) (offsets : List Float) :
    GameState := Id.run do
  let mut g := g.aFaceTarget i
  let m := g.mobjs[i]!
  let some (tx, ty, tz) := g.enemyPos m | return g
  let base := Float.atan2 (ty - m.y) (tx - m.x)
  let d := m.distanceTo tx ty
  for off in offsets do
    let (jit, g') := g.blurJitter m
    g := g'
    let a := base + off + jit
    g := g.spawnMissile m (m.x + d * Float.cos a) (m.y + d * Float.sin a) tz
      .fatShot
  return g

-- Vanilla's exact offsets: `A_FatAttack1` throws one dead ahead and one at
-- +FATSPREAD, `A_FatAttack2` one ahead and one at −2·FATSPREAD,
-- `A_FatAttack3` the tight ±FATSPREAD/2 pair.
def aFatAttack (g : GameState) (i : Nat) : GameState :=
  g.fatShots i [0, fatSpread]
def aFatAttack2 (g : GameState) (i : Nat) : GameState :=
  g.fatShots i [0, -2 * fatSpread]
def aFatAttack3 (g : GameState) (i : Nat) : GameState :=
  g.fatShots i [-fatSpread / 2, fatSpread / 2]

/-- Arachnotron: one plasma bolt. -/
def aBspiAttack (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  match g.enemyPos m with
  | none => g
  | some (tx, ty, tz) =>
    let (jit, g) := g.blurJitter m
    g.spawnMissile m tx ty tz .arachPlasma (angleJitter := jit)

/-- Revenant: the swing, then the punch, then the rocket. -/
def aSkelWhoosh (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  g.playSound Sfx.skeSwing m.x m.y

def aSkelFist (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  if g.inMeleeRange m then
    let g := g.playSound Sfx.skePunch m.x m.y
    let (dmg, g) := g.randDice 1 10
    g.damageEnemy m (dmg * 6)
  else g

def aSkelMissile (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  -- the quarry died mid-wind-up: no rocket at all (vanilla's health gate),
  -- and certainly not one homing on the player instead
  match g.enemyPos m with
  | none => g
  | some (tx, ty, tz) =>
    let g := g.playSound Sfx.skeAttack m.x m.y
    let (jit, g) := g.blurJitter m
    -- the tracer homes on whatever the revenant was aiming at (0 = player)
    g.spawnMissile m tx ty tz .revenantMissile (angleJitter := jit)
      (target := m.target)

/-- Lost soul: scream and hurl itself at `SKULLSPEED` (vanilla 20/tic);
flight is resolved per tic in `moveSkull` until it hits something. The climb
rate is the height gap over the flight time, and vanilla clamps that time to
at least one tic (`if (dist < 1) dist = 1` *after* dividing by SKULLSPEED),
so a point-blank dive cannot rocket vertically. -/
def aSkullAttack (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  -- vanilla aims the dive at `dest->z + height/2`; `enemyPos` hands back the
  -- player's *eye* (z + 41), a hair above the chest, so rebase the player
  -- case. A gone target means no dive at all — the soul just stands.
  let aim := match enemyTarget g m with
    | .player => some (g.player.x, g.player.y, g.player.z + Player.height / 2)
    | .mobj j => let t := g.mobjs[j]!; some (t.x, t.y, t.z + t.info.height / 2)
    | .gone => none
  match aim with
  | none => g
  | some (tx, ty, tz) =>
    let g := g.playSound Sfx.sklAttack m.x m.y
    let speed := Speeds.skullCharge
    let time := max 1.0 (m.distanceTo tx ty / speed)
    g.setMobj i { m with
      charging := true
      momX := speed * Float.cos m.angle
      momY := speed * Float.sin m.angle
      momZ := (tz - m.z) / time }

/-- Vanilla's cap on the pain elemental: it counts the lost souls already
in the level — corpses mid-burst included, as vanilla counts every
`MT_SKULL` thinker — and stays quiet past twenty. Without this nothing
bounds the population — every soul it spits is another shooter's worth of
pressure and they compound. -/
private def maxLostSouls : Nat := 20

/-- Vanilla `A_PainShootSkull`: spit a lost soul out in front. The step out
is `4 + 3·(radius + skullradius)/2` (≈74 for the elemental), at z + 8. A
soul born where it cannot stand — inside a wall or another body — is not
left clipping: vanilla spawns it and immediately deals it 10000, so it
bursts on the spot, and we do the same. A soul that does fit takes the
elemental's own target and charges it at once (`A_SkullAttack`). -/
private def shootSkull (g : GameState) (m : Mobj) (angle : Float) : GameState :=
  -- counted, not collected: `filter |>.size` built a throwaway array of
  -- every soul on the map to ask how many there were
  let count := g.mobjs.countP fun s => s.kind == .lostSoul && !s.removed
  if count > maxLostSouls then g else
  let skull := ActorInfo.ofKind .lostSoul
  let d := 4 + 3 * (m.info.radius + skull.radius) / 2
  let x := m.x + d * Float.cos angle
  let y := m.y + d * Float.sin angle
  let z := m.z + 8
  let (g, i) := g.spawn .lostSoul x y angle
  let g := { g with mobjs := g.mobjs.modify i fun s =>
      { s with z, awake := true, shooterUid := m.uid, target := m.target } }
  let blocked :=
    (g.level.checkBody x y z skull.radius skull.height Player.maxStep).isNone
      || g.mobjBlocked g.mobjs[i]!.uid x y skull.radius
  if blocked then g.damageMobj i 10000
  else g.aSkullAttack i

/-- Pain elemental: one soul on the attack… -/
def aPainAttack (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  shootSkull g m m.angle

/-- …and three more when it bursts, at right angles to its facing. -/
def aPainDie (g : GameState) (i : Nat) : GameState := Id.run do
  let mut g := g
  let m := g.mobjs[i]!
  for turn in [1.5707963, 3.1415927, 4.7123890] do
    g := shootSkull g m (m.angle + turn)
  return g

/-- `A_VileChase`: an ordinary chase that also raises any corpse it walks
over. Vanilla plays each monster's death animation backwards out of a
dedicated `raisestate`; there is no such chain in these tables, so the
body walks its own death frames in reverse instead — right effect, same
frames. -/
def aVileChase (g : GameState) (i : Nat) : GameState := Id.run do
  let mut g := g
  let m := g.mobjs[i]!
  -- corpses within reach only, through the grid — vanilla walks the blockmap
  -- cells around the vile for exactly this
  for j in g.mobjsNear m.x m.y (m.info.radius + 32) do
    let c := g.mobjs[j]!
    if !c.corpse || c.raising || !c.info.countKill || c.removed then continue
    if c.info.deathState.isNone then continue
    -- vanilla `PIT_VileCheck`'s first two gates: the kind must have a
    -- raisestate at all (fourteen monsters do — see `ActorInfo.raisable`)…
    if !c.info.raisable then continue
    -- …and the corpse must be lying still: its death chain finished, at
    -- rest on the final forever frame (`tics == -1` in vanilla; DILL's
    -- negative-tics rest is the same mark)
    if c.tics ≥ 0 then continue
    -- `PIT_VileCheck` reach is a per-axis *box* test (vanilla measures from
    -- the vile's attempted step; the +32 covers that lead)
    let reach := m.info.radius + c.info.radius + 32
    if Float.abs (c.x - m.x) > reach || Float.abs (c.y - m.y) > reach then continue
    -- vanilla `PIT_VileCheck`: no rise where the living body would not fit —
    -- the corpse's spot must hold its full standing radius and height, clear
    -- of walls and of everything solid, or it would revive inside a wall or
    -- another monster
    if (g.level.checkBody c.x c.y c.z c.info.radius c.info.height
          Player.maxStep (isMonster := true)).isNone then continue
    if g.mobjBlocked c.uid c.x c.y c.info.radius then continue
    -- begin the rise: it walks its death frames backwards (see `tickMobjs`),
    -- health restored, still non-solid until it is fully on its feet
    let raised := { c with raising := true, health := c.info.health, tics := 5 }
    g := g.setMobj j raised
    g := g.playSound Sfx.slop c.x c.y
    -- the vile stops mid-stride, faces the corpse, and stands over it
    -- through its heal frames (vanilla S_VILE_HEAL1–3) instead of walking on
    if let some heal := m.info.healState then
      let m := g.mobjs[i]!
      g := g.setMobj i { (m.setState heal) with angle := angleTo m c.x c.y }
    -- it is alive again, so it no longer counts as killed
    return { g with kills := if g.kills > 0 then g.kills - 1 else 0 }
  return g.aChase i

/-- `A_VileTarget`: plant the flame where the target stands — no target,
no flame. -/
def aVileTarget (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  match g.enemyPos m with
  | none => g
  | some (tx, ty, _) =>
    let (g, j) := g.spawn .vileFire tx ty m.angle
    { g with mobjs := g.mobjs.modify j ({ · with shooterUid := m.uid }) }

/-- `A_Fire`: the flame clings to the victim, 24 units out along the way
they face, and stops following the instant the vile loses sight of them.
That freeze is the visible cue that the lock is broken — the flame stays
behind while you round the corner. -/
def aFire (g : GameState) (i : Nat) : GameState := Id.run do
  let f := g.mobjs[i]!
  -- the flame belongs to the vile that planted it (`fog->target` in vanilla)
  let owner := (g.mobjIdx? f.shooterUid).filter (!g.mobjs[·]!.removed)
  let some vi := owner | return g
  let v := g.mobjs[vi]!
  if !g.seesEnemy v then return g
  -- the flame clings 24 out along the *victim's* facing, at their feet
  -- (`seesEnemy` above is false at a gone target, so this only sees the
  -- player or a live monster)
  let (tx, ty, tz, ta) := match enemyTarget g v with
    | .mobj j => let t := g.mobjs[j]!; (t.x, t.y, t.z, t.angle)
    | _ => (g.player.x, g.player.y, g.player.z, g.player.angle)
  return g.setMobj i { f with
    x := tx + 24.0 * Float.cos ta, y := ty + 24.0 * Float.sin ta, z := tz }

/-- `A_VileAttack`: the flame goes off — a solid hit, a blast, and the
victim flung into the air. Nothing happens at all unless the vile can still
see its quarry when the flame lands. -/
def aVileAttack (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  -- Vanilla bails here on `!P_CheckSight`, before the sound and before any
  -- damage: break the vile's line of sight during its long wind-up and the
  -- attack simply fizzles. Ducking behind a pillar is the whole counterplay.
  -- A victim that *died* mid-wind-up fizzles the flame the same way
  -- (`seesEnemy` is false at a gone target) — it must never land on the
  -- player instead.
  if !g.seesEnemy m then g else
  match g.enemyPos m with
  | none => g   -- unreachable past the sight check; spelled out for totality
  | some (tx, ty, tz) =>
    let g := g.playSound Sfx.barExp m.x m.y
    -- resolve the victim *before* the damage lands: vanilla flings the target
    -- even when the 20 points kill it (the corpse pops into the air)
    let victim := enemyTarget g m
    let g := g.damageEnemy m 20
    -- Vanilla *sets* `target->momz = 1000/mass` — an assignment, on any
    -- target: the player's signature launch (mass 100 → 10/tic), and monsters
    -- too — light lost souls fly, a mass-1000 mancubus barely hops.
    let g := match victim with
      | .mobj j =>
        let t := g.mobjs[j]!
        g.setMobj j { t with momZ := 1000.0 / t.info.mass }
      | .player =>
        if !g.status.dead then
          { g with player := { g.player with momZ := 1000.0 / 100.0 } }
        else g
      | .gone => g
    -- The blast goes off where the flame stands, which vanilla parks 24 units
    -- back from the target along the vile's own facing — not at the target's
    -- feet. It is the vile's blast, but `damageMobj` refuses to make anything
    -- turn on a vile, so it scorches the crowd without seeding a grudge.
    g.radiusDamage (tx - 24.0 * Float.cos m.angle)
      (ty - 24.0 * Float.sin m.angle) tz 70 (sourceUid := m.uid)

/-- Fly the charging skull one tic: it stops (and bites) on any impact. -/
def moveSkull (g : GameState) (i : Nat) : GameState := Id.run do
  let m := g.mobjs[i]!
  -- Ending the dive — a slam or a wall — zeroes the momentum and drops the
  -- soul back to its *spawn* state (vanilla `PIT_CheckThing` and
  -- `P_XYMovement` both `P_SetMobjState (…, spawnstate)`); it stays awake,
  -- so `A_Look` re-acquires on the next thought.
  let stop := fun (g : GameState) =>
    let s := g.mobjs[i]!
    g.setMobj i { (s.setState s.info.spawnState) with
      charging := false, momX := 0, momY := 0, momZ := 0 }
  let nx := m.x + m.momX
  let ny := m.y + m.momY
  -- Other bodies: vanilla's slam fires on anything solid *or* shootable —
  -- a lamp stops the dive too, it just takes no damage (`P_DamageMobj`
  -- returns on a non-shootable target). The slam is (P_Random()%8+1)×3.
  for j in g.mobjsNear nx ny m.info.radius do
    let t := g.mobjs[j]!
    if t.removed || t.uid == m.uid || !(t.solid || t.shootable) then continue
    let reach := t.info.radius + m.info.radius
    if Float.abs (t.x - nx) < reach && Float.abs (t.y - ny) < reach then
      if !t.shootable then return stop g
      let (n, sides) := m.info.damageDice
      let (dmg, g') := g.randDice n sides
      return stop (g'.damageMobj j (dmg * m.info.damageMult) (sourceUid := m.uid)
        (inflictor := some (m.x, m.y)))
  -- the player
  if !g.status.dead then
    let p := g.player
    let reach := Player.radius + m.info.radius
    if Float.abs (p.x - nx) < reach && Float.abs (p.y - ny) < reach then
      let (n, sides) := m.info.damageDice
      let (dmg, g') := g.randDice n sides
      return stop (g'.damagePlayer (dmg * m.info.damageMult) (inflictor := some (m.x, m.y)))
  -- walls and ledges: the charge flies through `P_TryMove`, whose 24-unit
  -- step check fails against a taller ledge — the dive ends there rather
  -- than popping up on top of the step
  match g.level.checkBody nx ny m.z m.info.radius m.info.height
      Player.maxStep (isMonster := true) with
  | none => return stop g
  | some floorZ =>
    -- vanilla `P_ZMovement` reflects a skull-flying soul off the floor and
    -- the ceiling (`mo->momz = -mo->momz`) instead of ending the dive
    let sec := g.level.sectors[g.level.sectorAt nx ny]!
    let lo := floorZ
    let hi := sec.ceilH - m.info.height
    let zRaw := m.z + m.momZ
    let (nz, momZ) :=
      if zRaw < lo then (lo, -m.momZ)
      else if zRaw > hi then (max lo hi, -m.momZ)
      else (zRaw, m.momZ)
    let g := g.setMobj i { m with x := nx, y := ny, z := nz, momZ }
    -- A charging skull flies through `P_TryMove` in vanilla, so its path
    -- works line specials like anything else's — `MF_SKULLFLY` is not
    -- `MF_MISSILE`, and `EV_Teleport` only refuses the latter.
    return g.crossSpecialsMobj i m.x m.y

/-- Demon: all bite. -/
def aSargAttack (g : GameState) (i : Nat) : GameState :=
  let g := g.aFaceTarget i
  let m := g.mobjs[i]!
  -- Bite the *enemy* (a feuding monster too), not always the player.
  -- Vanilla `A_SargAttack` is silent — the sgtatk bark already played from
  -- `A_Chase` on entering the melee state, and plays nowhere else.
  if g.inMeleeRange m then
    let (dmg, g) := g.randDice 1 10
    g.damageEnemy m (dmg * 4)
  else g

/-! ## The Icon of Sin (MAP30) -/

/-- The weighted roulette a spawn cube spins for its monster, straight from
vanilla `A_SpawnFly`'s `P_Random` ladder. -/
private def spawnMonsterFor (r : Nat) : ActorKind :=
  if r < 50 then .imp
  else if r < 90 then .demon
  else if r < 120 then .spectre
  else if r < 130 then .painElemental
  else if r < 160 then .cacodemon
  else if r < 162 then .archVile
  else if r < 172 then .revenant
  else if r < 192 then .arachnotron   -- MT_BABY
  else if r < 222 then .mancubus     -- MT_FATSO
  else if r < 246 then .hellKnight
  else .baron

/-- `A_BrainAwake`: the spitter's opening roar as MAP30 begins (vanilla plays
`sfx_bossit` and gathers the target spots; we gather them per-spit instead).
All the Icon's voice lines have a NULL origin in vanilla — full volume
everywhere — so DILL sounds them at the player, where attenuation is zero. -/
def aBrainAwake (g : GameState) (_i : Nat) : GameState :=
  g.playSound Sfx.bosSight g.player.x g.player.y

/-- `A_BrainSpit`: the spitter flings a cube at a random target spot, timed
to arrive after `dist/speed` tics. -/
def aBrainSpit (g : GameState) (i : Nat) : GameState := Id.run do
  let m := g.mobjs[i]!
  -- Vanilla flips an `easy` flag every cadence and, on the two easiest skills,
  -- spits only when it comes up set — halving the spawn rate. The spitter has
  -- no AI, so we keep the flag in its otherwise-unused `moveDir`.
  let easy := 1 - m.moveDir
  let mut g := g.setMobj i { m with moveDir := easy }
  if g.skill ≤ 2 && easy == 0 then return g
  -- The target spots, gathered in map (spawn) order and cycled through
  -- round-robin — vanilla's `braintargeton`, parked in `moveCount`.
  let mut targets : Array Nat := #[]
  for j in [0:g.mobjs.size] do
    if g.mobjs[j]!.kind == .iconTarget && !g.mobjs[j]!.removed then
      targets := targets.push j
  if targets.isEmpty then return g
  let m := g.mobjs[i]!
  let t := g.mobjs[targets[m.moveCount % targets.size]!]!
  let d := max 1.0 (m.distanceTo t.x t.y)
  let speed := 10.0   -- vanilla MT_SPAWNSHOT speed
  let a := Float.atan2 (t.y - m.y) (t.x - m.x)
  g := g.setMobj i { m with moveCount := m.moveCount + 1 }
  let (g'', ci) := g.spawn .spawnCube m.x m.y a
  g := g''
  -- full volume in vanilla (NULL origin): sound it at the player
  g := g.playSound Sfx.bosSpit g.player.x g.player.y
  return g.setMobj ci { g.mobjs[ci]! with
    momX := speed * Float.cos a, momY := speed * Float.sin a,
    momZ := (t.z - m.z) / d * speed,
    moveCount := (ifloor (d / speed)).toNat + 1 }

/-- `A_BrainScream`: the brain's death throes — a row of ~64 explosions
marching the full width of the wall (at random heights), and the boss death
roar. Each puff goes on to seed the rest of the cascade. -/
def aBrainScream (g : GameState) (i : Nat) : GameState := Id.run do
  let m := g.mobjs[i]!
  let mut g := g
  let mut x := m.x - 196.0
  while x < m.x + 320.0 do
    let (rz, g1) := g.rand
    let (rt, g2) := g1.rand
    let (g3, ei) := g2.spawn .brainExplosion x (m.y - 320.0) 0.0
    g := g3
    g := g.setMobj ei { g.mobjs[ei]! with
      z := 128.0 + Float.ofNat rz * 2.0
      tics := max 1 (g.mobjs[ei]!.tics - Int.ofNat (rt &&& 7)) }
    x := x + 8.0
  -- full volume in vanilla (NULL origin): sound it at the player
  return g.playSound Sfx.bosDeath g.player.x g.player.y

/-- `A_BrainExplode`: each puff in the cascade births the next, a short hop
away and at a fresh random height, so the wall keeps erupting for the whole
death sequence (until `A_BrainDie` ends the level). -/
def aBrainExplode (g : GameState) (i : Nat) : GameState := Id.run do
  let m := g.mobjs[i]!
  let (r1, g1) := g.rand
  let (r2, g2) := g1.rand
  let (rz, g3) := g2.rand
  let (rt, g4) := g3.rand
  let x := m.x + (Float.ofNat r1 - Float.ofNat r2) / 32.0
  let (g5, ei) := g4.spawn .brainExplosion x m.y 0.0
  return g5.setMobj ei { g5.mobjs[ei]! with
    z := 128.0 + Float.ofNat rz * 2.0
    tics := max 1 (g5.mobjs[ei]!.tics - Int.ofNat (rt &&& 7)) }

/-- `A_BrainDie`: the brain is destroyed — the level, and Doom II, is won. -/
def aBrainDie (g : GameState) (_i : Nat) : GameState :=
  { g with exited := true }

/-- Fly a spawn cube one tic; at journey's end it births a random monster
in a puff of teleport fog and is gone (`A_SpawnFly`). -/
def moveCube (g : GameState) (i : Nat) : GameState := Id.run do
  let m := g.mobjs[i]!
  if m.moveCount > 1 then
    -- the cube hums as it flies (vanilla `A_SpawnSound`, ~once per state loop)
    let g := if m.moveCount % 12 == 0 then g.playSound Sfx.bosCube m.x m.y else g
    let m' := { m with x := m.x + m.momX, y := m.y + m.momY, z := m.z + m.momZ, moveCount := m.moveCount - 1 }
    return g.setMobj i m'
  let (r, g') := g.rand
  let mut g := g'
  let (g2, nj) := g.spawn (spawnMonsterFor r) m.x m.y m.angle
  g := g2
  -- Vanilla wakes the newborn only if `P_LookForPlayers (allaround)` truly
  -- sees the player — straight into its see state, with no sight cry
  -- (`A_SpawnFly` bypasses `A_Look`). Unseen, it stands dormant until
  -- sight or noise wakes it the ordinary way.
  let nb := g.mobjs[nj]!
  if !g.status.dead && g.seesPlayer nb then
    g := g.setMobj nj { (nb.setState (nb.info.seeState.getD nb.state)) with
      awake := true }
  -- vanilla `P_TeleportMove` gibs whatever already stood in the landing spot
  let nb := g.mobjs[nj]!
  for j in [0:g.mobjs.size] do
    let o := g.mobjs[j]!
    if j != nj && o.shootable && !o.removed
        && max (Float.abs (o.x - nb.x)) (Float.abs (o.y - nb.y))
          < nb.info.radius + o.info.radius then
      g := g.damageMobj j 10000
  -- …the player included: standing on an Icon target pad is fatal in vanilla
  if !g.status.dead then
    let p := g.player
    if max (Float.abs (p.x - nb.x)) (Float.abs (p.y - nb.y))
        < nb.info.radius + Player.radius then
      g := g.damagePlayer 10000
  g := (g.spawn .teleFog m.x m.y 0).1
  g := g.playSound Sfx.teleport m.x m.y
  return g.setMobj i { m with removed := true }

/-- Dispatch a state's action. -/
def runAction (g : GameState) (i : Nat) : Option Action → GameState
  | none => g
  | some a =>
    match a with
    | .look       => g.aLook i
    | .chase      => g.aChase i
    | .faceTarget => g.aFaceTarget i
    | .posAttack  => g.aPosAttack i
    | .sposAttack => g.aSPosAttack i
    | .trooAttack => g.aTroopAttack i
    | .sargAttack => g.aSargAttack i
    | .headAttack => g.aHeadAttack i
    | .bruisAttack => g.aBruisAttack i
    | .skullAttack => g.aSkullAttack i
    | .cyberAttack => g.aCyberAttack i
    | .spidRefire => g.aSpidRefire i
    | .cposAttack => g.aCPosAttack i
    | .cposRefire => g.aCPosRefire i
    | .fatRaise   =>
      let g := g.aFaceTarget i
      let m := g.mobjs[i]!
      g.playSound Sfx.manAttack m.x m.y
    | .fatAttack  => g.aFatAttack i
    | .fatAttack2 => g.aFatAttack2 i
    | .fatAttack3 => g.aFatAttack3 i
    | .bspiAttack => g.aBspiAttack i
    | .skelWhoosh => g.aSkelWhoosh i
    | .skelFist   => g.aSkelFist i
    | .skelMissile => g.aSkelMissile i
    | .painAttack => g.aPainAttack i
    | .painDie    => g.aPainDie i
    | .brainAwake => g.aBrainAwake i
    | .brainSpit  => g.aBrainSpit i
    | .brainScream => g.aBrainScream i
    | .brainExplode => g.aBrainExplode i
    | .brainDie   => g.aBrainDie i
    | .spawnFly   => g   -- the cube's flight is driven by `moveCube`
    | .vileChase  => g.aVileChase i
    -- vanilla A_VileStart is the whoosh alone; the facing beat is the
    -- attack chain's own next state
    | .vileStart  =>
      let m := g.mobjs[i]!
      g.playSound Sfx.vilAttack m.x m.y
    | .vileTarget => g.aVileTarget i
    | .vileAttack => g.aVileAttack i
    | .fire       => g.aFire i
    -- vanilla A_StartFire / A_FireCrackle: A_Fire plus a sound cue — the
    -- flame igniting (DSFLAMST), then roaring (DSFLAME) on the crackle beats
    | .fireStart  =>
      let f := g.mobjs[i]!
      (g.playSound Sfx.flameStart f.x f.y).aFire i
    | .fireCrackle =>
      let f := g.mobjs[i]!
      (g.playSound Sfx.flame f.x f.y).aFire i
    | .bfgSpray   => g.bfgSpray g.mobjs[i]!
    -- Keen's tag-666 door opens from the boss-death path in `Combat`,
    -- the same as every other map-gated special
    | .keenDie    => g
    -- footfalls: a sound, then an ordinary chase step
    | .hoof =>
      let m := g.mobjs[i]!
      (g.playSound Sfx.hoof m.x m.y).aChase i
    | .metal =>
      let m := g.mobjs[i]!
      (g.playSound Sfx.metal m.x m.y).aChase i
    | .babyMetal =>
      let m := g.mobjs[i]!
      (g.playSound Sfx.bspWalk m.x m.y).aChase i
    | .explode    =>
      let m := g.mobjs[i]!
      -- a barrel remembers whoever shot it in `target` (set by `damageMobj`),
      -- so a monster-triggered blast seeds infighting against that monster
      g.radiusDamage m.x m.y m.z 128 (sourceUid := m.target)
    | .pain =>
      let m := g.mobjs[i]!
      match m.kind.painSfx with
      | some s =>
        -- the Icon's grunt (A_BrainPain) has a NULL origin in vanilla —
        -- full volume — so it sounds at the player, attenuation zero
        if m.kind == .iconBrain then g.playSound s g.player.x g.player.y
        else g.playSound s m.x m.y
      | none => g
    | .scream =>
      let m := g.mobjs[i]!
      match m.kind.deathSfx with
      | some s =>
        -- as with the sight cry, vanilla rolls among the variants
        let (roll, g) := g.rand
        -- and, as with their sight roars, the two bosses' death screams
        -- play at full volume (NULL origin): sound them at the player
        if m.kind == ActorKind.cyberdemon || m.kind == ActorKind.spiderMastermind then
          g.playSound (Sfx.varied s roll) g.player.x g.player.y
        else g.playSound (Sfx.varied s roll) m.x m.y
      | none => g
    | .xscream =>
      let m := g.mobjs[i]!
      g.playSound Sfx.slop m.x m.y
    | .fall => g  -- corpses are already soft

/-- Run — and clear — a freshly entered state's first-frame action.

Vanilla `P_SetMobjState` runs the action of every state it enters;
`Mobj.setState` cannot (the dispatcher is here, above it in the module
graph), so it raises `entryPending` and this settles the debt. `tickMobjs`
calls it as each mobj comes up for thought and again on every natural state
advance, so a state entered from anywhere — a hit's pain frame, an attack
wind-up, a death — still acts, at most one tic later than vanilla. -/
private def runEntry (g : GameState) (i : Nat) : GameState :=
  let m := g.mobjs[i]!
  if !m.entryPending || m.removed then g
  else
    let g := g.setMobj i { m with entryPending := false }
    g.runAction i g.mobjs[i]!.stateDef.action

/-- `A_Tracer`: the revenant's rocket curves toward its quarry. Vanilla steers
only once every four tics (`gametic & 3`) — the rocket flies straight between
adjustments — snapping its heading up to `TRACEANGLE` (~16.875°) toward the
target and nudging its climb rate by a flat 1/8 unit toward the aim slope. It
trails smoke and can be shaken off by cutting hard across its update beat. -/
def traceMissile (g : GameState) (i : Nat) : GameState := Id.run do
  let m := g.mobjs[i]!
  -- steer on every fourth tic only; otherwise leave the rocket on its course
  if g.tics &&& 3 != 0 then return g
  -- a puff of smoke trails one step back along its path (vanilla's MT_SMOKE is
  -- itself the PUFF sprite)
  let (g', si) := g.spawn .puff (m.x - m.momX) (m.y - m.momY) 0.0
  let g := g'.setMobj si { g'.mobjs[si]! with z := m.z }
  -- resolve the target: a live monster by uid, or the player at uid 0. The aim
  -- point sits 40 above its feet, exactly as vanilla (`dest->z + 40*FRACUNIT`).
  let tgt : Option (Float × Float × Float) :=
    if m.target == 0 then
      if g.status.dead then none
      else some (g.player.x, g.player.y, g.player.z + 40.0)
    else
      match g.mobjIdx? m.target with
      | some j =>
        let t := g.mobjs[j]!
        if !t.removed && t.shootable then some (t.x, t.y, t.z + 40.0) else none
      | none => none
  -- a dead or vanished target stops the homing: the rocket flies straight on
  let some (tx, ty, tz) := tgt | return g
  let speed := m.info.speed
  -- turn toward the target by up to TRACEANGLE, snapping onto it if within reach
  let cur := Float.atan2 m.momY m.momX
  let want := Float.atan2 (ty - m.y) (tx - m.x)
  let maxTurn := 0.2945243   -- 0x0c000000 in Doom's angle units = 16.875°
  let d := Float.atan2 (Float.sin (want - cur)) (Float.cos (want - cur))
  let turn := max (-maxTurn) (min maxTurn d)
  let a := cur + turn
  -- nudge the climb rate a flat 1/8 toward the slope `(aim.z - z) / (dist/speed)`
  let dist := max 1.0 (m.distanceTo tx ty / speed)
  let slope := (tz - m.z) / dist
  let momZ := if slope < m.momZ then m.momZ - 0.125 else m.momZ + 0.125
  return g.setMobj i { m with
    momX := speed * Float.cos a, momY := speed * Float.sin a, momZ }

/-- `P_NightmareRespawn`: a fallen monster's corpse reincarnates at its map
spot. It lies still for 12 seconds, then rolls a 4-in-256 chance every 32 tics;
on a hit it warps back — teleport fog and chime at both the corpse and the
spawn point — as a fresh, dormant monster. Only map-placed countable monsters
qualify, and only if there is room to stand. Skill 5 only. -/
def nightmareRespawn (g : GameState) (i : Nat) : GameState := Id.run do
  let m := g.mobjs[i]!
  if !m.corpse || !m.canRespawn || !m.info.countKill || m.removed then return g
  let mut g := g.setMobj i { m with respawnTic := m.respawnTic + 1 }
  -- 12 seconds down, then a 4/256 roll gated to every 32nd tic (vanilla)
  if m.respawnTic + 1 < 12 * 35 then return g
  if g.tics &&& 31 != 0 then return g
  let (roll, g') := g.rand
  g := g'
  if roll > 4 then return g
  -- Somewhere to stand? If not, try again another tic. Vanilla's
  -- `P_CheckPosition` covers both halves: no solid body on the spot, and
  -- the geometry must fit the standing monster — walls clear, and the
  -- floor-to-ceiling gap at least its height, so a corpse never reincarnates
  -- inside a closed door or under a lowered lift.
  if g.mobjBlocked m.uid m.spawnX m.spawnY m.info.radius then return g
  let spawnFloor := g.level.sectors[g.level.sectorAt m.spawnX m.spawnY]!.floorH
  if (g.level.checkBody m.spawnX m.spawnY spawnFloor m.info.radius m.info.height
        Player.maxStep (isMonster := true)).isNone then return g
  -- fog and chime where it fell and where it returns
  g := (g.spawn .teleFog m.x m.y 0).1
  g := g.playSound Sfx.teleport m.x m.y
  g := (g.spawn .teleFog m.spawnX m.spawnY 0).1
  g := g.playSound Sfx.teleport m.spawnX m.spawnY
  -- reborn on its feet, briefly dormant (vanilla reactiontime 18); killcount is
  -- left untouched, so re-killing on Nightmare can push kills past 100% as in
  -- vanilla. The corpse is retired.
  let (g'', j) := g.spawn m.kind m.spawnX m.spawnY m.spawnAngle
  g := g''
  g := { g with mobjs := g.mobjs.modify j fun n =>
    { n with spawnX := m.spawnX, spawnY := m.spawnY, spawnAngle := m.spawnAngle
             canRespawn := true, ambush := m.ambush, reactionTime := 18 } }
  return g.setMobj i { g.mobjs[i]! with removed := true }

/-- Advance every mobj one tic: fly missiles, count down states, run entry
actions. Mobjs spawned during the tic (drops, puffs) think next tic. -/
def tickMobjs (g : GameState) : GameState := Id.run do
  let mut g := g
  let count := g.mobjs.size
  for i in [0:count] do
    if g.mobjs[i]!.removed then continue
    -- a state entered since this mobj last thought (pain, an attack
    -- wind-up, a death) runs its first-frame action now — see `runEntry`
    g := g.runEntry i
    if g.mobjs[i]!.removed then continue
    -- Nightmare: reincarnate a monster's corpse at its map spot
    if g.skill == 5 then
      g := g.nightmareRespawn i
      if g.mobjs[i]!.removed then continue
    if g.mobjs[i]!.kind == .spawnCube then
      g := g.moveCube i
      if g.mobjs[i]!.removed then continue
    if g.mobjs[i]!.kind == .revenantMissile && !g.mobjs[i]!.corpse then
      g := g.traceMissile i
    if g.mobjs[i]!.info.missile && !g.mobjs[i]!.corpse then
      g := g.moveMissile i
    if g.mobjs[i]!.charging && !g.mobjs[i]!.corpse then
      g := g.moveSkull i
    -- Knockback slide (vanilla `P_XYMovement` on the thrust momentum): a thing
    -- shoved by a hit drifts, clipping on walls, and bleeds off through
    -- friction. Walking monsters carry no momentum, so this only moves what a
    -- blow set going — the live monster reeling, or the corpse skidding.
    -- Missiles, charging skulls and spawn cubes fly under their own logic above.
    let km := g.mobjs[i]!
    if !km.removed && !km.info.missile && !km.charging && km.kind != .spawnCube
        && (km.momX != 0 || km.momY != 0) then
      if Float.abs km.momX > Player.stopSpeed
          || Float.abs km.momY > Player.stopSpeed then
        let nx := km.x + km.momX
        let ny := km.y + km.momY
        -- a live monster shoved into a ledge stops at the vanilla 24-unit
        -- step (P_TryMove's rule); only a corpse keeps the permissive slide,
        -- so a body can still be blasted up onto a shelf
        let clear := (g.level.checkBody nx ny km.z km.info.radius km.info.height
              (maxStep := if km.corpse then 1.0e9 else Player.maxStep)
              (isMonster := !km.corpse)).isSome
            && (km.corpse || !g.mobjBlocked km.uid nx ny km.info.radius)
        if clear then
          let km' := { km with x := nx, y := ny, momX := km.momX * Player.friction, momY := km.momY * Player.friction }
          g := g.setMobj i km'
        else
          g := g.setMobj i { km with momX := 0, momY := 0 }
      else
        -- Below `STOPSPEED` the slide is over: snap the momentum to zero
        -- (the same stop-clamp `Player.move` applies), restoring the
        -- invariant that a resting thing carries none — a lingering
        -- sub-threshold residue would silently add onto the next knockback.
        g := g.setMobj i { km with momX := 0, momY := 0 }
    -- ground things track their floor: fall to it, and ride it up/down when
    -- a lift or crusher moves it (so corpses and items don't float or sink).
    -- Missiles fly, and live floaters hold their own altitude.
    let gm := g.mobjs[i]!
    let grounded := !gm.info.missile && !gm.info.ceilingHang && !gm.info.noGravity
      && (gm.corpse || !gm.info.flying)
    if grounded && !gm.removed then
      let floorH := g.level.sectors[g.level.sectorAt gm.x gm.y]!.floorH
      -- A thing resting on its floor *rides* it, so it stays glued to a
      -- lift or a lowering floor. A plane can drop `Speeds.maxPlane` in a
      -- tic while a fall only starts at `gravity`, so free-falling instead
      -- would leave the sprite hanging above a floor dropping out from
      -- under it. Anything genuinely airborne falls under accelerating
      -- gravity, as the player does.
      if gm.momZ == 0 && gm.z - floorH ≤ Speeds.maxPlane then
        g := g.setMobj i { gm with z := floorH }
      else if gm.z > floorH then
        let momZ := gm.momZ - Player.gravity
        let z := max floorH (gm.z + momZ)
        g := g.setMobj i { gm with z, momZ := if z == floorH then 0 else momZ }
      else if gm.momZ > 0 then
        -- an upward launch off the floor (`A_VileAttack` *sets* `momz` on a
        -- grounded victim): vanilla `P_ZMovement` moves first and weakens the
        -- climb by gravity after, so even a heavy fling leaves the ground
        let momZ := gm.momZ - Player.gravity
        g := g.setMobj i { gm with z := gm.z + gm.momZ, momZ }
      else
        g := g.setMobj i { gm with z := floorH, momZ := 0 }
    let m := g.mobjs[i]!
    if m.removed || m.tics < 0 then continue
    if m.tics > 1 then
      g := g.setMobj i { m with tics := m.tics - 1 }
    else if m.raising then
      -- climb the death chain backwards; at the top, it lives again. The
      -- rewind must not fire the death frames' actions (the scream, a pain
      -- elemental's burst of souls), so the entry flag is quashed.
      let ds := m.info.deathState.getD m.state
      if m.state > ds then
        g := g.setMobj i { (m.setState (m.state - 1)) with
          tics := 5, raising := true, entryPending := false }
      else
        let seen := m.info.seeState.getD m.info.spawnState
        g := g.setMobj i { (m.setState seen) with
          raising := false, corpse := false, awake := true }
    else
      match m.stateDef.next with
      | none => g := g.setMobj i { m with removed := true }
      | some s =>
        g := g.setMobj i (m.setState s)
        g := g.runEntry i
  -- let the teleport lock fade
  g := { g with teleFreeze := g.teleFreeze - 1 }
  -- Compact the dead-and-gone. Compaction shifts indices, so `uidIndex` and
  -- `mobjGrid` are rebuilt to match — but only when something actually went:
  -- the rebuild walks the whole roster inserting into a fresh `HashMap` and a
  -- fresh cell grid, and the great majority of tics remove nothing at all.
  -- Skipping it then is safe precisely because nothing moved: `spawn` and
  -- `setMobj` keep both indexes exact as they go, and it is only compaction
  -- that invalidates them (see the invariant on `MobjGrid`). The `filter` is
  -- skipped with it, since it would copy the array to no purpose.
  if !g.mobjs.any (·.removed) then return g
  return { g with mobjs := g.mobjs.filter (!·.removed) }.rebuildIndexes

end GameState
end Dill
