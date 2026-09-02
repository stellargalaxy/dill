import Dill.Maps
import Dill.Game.State
import Dill.Game.Specials
import Dill.Game.Enemy
import Dill.Game.Player
import Dill.Game.Lights

/-!
# The simulation step

`tick : Input → GameState → GameState`, Doom's `P_PlayerThink` +
`P_XYMovement` distilled: thrust from the input, friction, and movement
clipped against linedefs found through the blockmap.

Collision follows vanilla's shape: a move is allowed if every crossed
linedef leaves an opening tall enough to stand in and no step up is higher
than 24 units. A blocked move runs `P_SlideMove` — up to the wall, then the
remainder of the step projected along it — falling back, as vanilla does, to
stepping one axis at a time when no wall can be identified.
-/

namespace Dill

namespace Player

/-- Vanilla `P_CheckPosition`: can a player body occupy `(x, y)` given feet
at `z`? Returns the floor height to stand on, or `none` if blocked. -/
def checkPosition (lvl : Level) (x y z : Float) : Option Float := Id.run do
  let sec := lvl.sectors[lvl.sectorAt x y]!
  let mut floorZ := sec.floorH
  let mut ceilZ := sec.ceilH
  for i in (lvl.linesNear x y radius) do
    let line := lvl.linedefs[i]!
    if !lvl.boxCrossesLine line x y radius then
      continue
    match line.back with
    | none => return none  -- solid wall
    | some back =>
      if line.has Linedef.blocking then return none
      let f := lvl.sectors[lvl.sidedefs[line.front]!.sector]!
      let b := lvl.sectors[lvl.sidedefs[back]!.sector]!
      floorZ := max floorZ (max f.floorH b.floorH)
      ceilZ := min ceilZ (min f.ceilH b.ceilH)
  -- all three of vanilla's clearance tests, shared with `Level.checkBody`
  -- and with `slideHit` below so the three cannot drift apart
  if !bodyFits ceilZ floorZ z height maxStep then return none
  return some floorZ

/-- Try to put the player at `(nx, ny)`; `none` if something is in the way. -/
private def stepTo (lvl : Level) (blocked : Float → Float → Bool)
    (p : Player) (nx ny : Float) : Option Player :=
  if blocked nx ny then none else
  (checkPosition lvl nx ny p.z).map fun floorZ =>
    { p with x := nx, y := ny
             z := if p.z < floorZ then floorZ else p.z }

/-- Vanilla's `stairstep`, the fallback when the slide cannot find a wall to
run along: one axis at a time, y before x. Momentum is left alone — the
original never writes `momx`/`momy` here, so a blocked axis simply fails
again next tic while ground friction runs the speed down. -/
private def stairStep (lvl : Level) (blocked : Float → Float → Bool)
    (p : Player) : Player :=
  match stepTo lvl blocked p p.x (p.y + p.momY) with
  | some p' => p'
  | none =>
    match stepTo lvl blocked p (p.x + p.momX) p.y with
    | some p' => p'
    | none => p

/-- The nearest wall this step would run into, as a fraction along the move
plus that wall's unit direction. Vanilla `P_SlideMove` traces the three
*leading* corners of the body's box — the two trailing ones cannot meet
anything the leading ones did not. -/
private def slideHit (lvl : Level) (p : Player) :
    Option (Float × Float × Float) := Id.run do
  let leadX := if p.momX > 0 then p.x + radius else p.x - radius
  let trailX := if p.momX > 0 then p.x - radius else p.x + radius
  let leadY := if p.momY > 0 then p.y + radius else p.y - radius
  let trailY := if p.momY > 0 then p.y - radius else p.y + radius
  let mut best : Option (Float × Float × Float) := none
  for (cx, cy) in [(leadX, leadY), (trailX, leadY), (leadX, trailY)] do
    for li in lvl.linesAlong cx cy (cx + p.momX) (cy + p.momY) do
      let line := lvl.linedefs[li]!
      let v1 := lvl.vertexes[line.v1]!
      let v2 := lvl.vertexes[line.v2]!
      let ldx := v2.x - v1.x
      let ldy := v2.y - v1.y
      -- the direction is this tic's whole step, so `t` is a fraction of it
      let some t := lvl.rayHitsLine line cx cy p.momX p.momY | continue
      if t < 0 || t > 1 then continue
      -- `PTR_SlideTraverse`: does this line actually stop us?
      let stops := match line.back with
        | none =>
          -- a one-sided wall blocks only from its front
          ldx * (p.y - v1.y) - ldy * (p.x - v1.x) < 0
        | some back =>
          if line.has Linedef.blocking then true
          else
            let f := lvl.sectors[lvl.sidedefs[line.front]!.sector]!
            let b := lvl.sectors[lvl.sidedefs[back]!.sector]!
            let openTop := min f.ceilH b.ceilH
            let openBot := max f.floorH b.floorH
            !bodyFits openTop openBot p.z height maxStep
      if !stops then continue
      let len := Float.sqrt (ldx * ldx + ldy * ldy)
      if len == 0 then continue
      let closer : Bool := match best with
        | some (bt, _, _) => decide (t < bt)
        | none => true
      if closer then best := some (t, ldx / len, ldy / len)
  return best

/-- Vanilla `P_SlideMove`: run up to the wall in the way, then carry what is
left of the step *along* it instead of throwing the blocked axis away. That
is what lets you glide down an angled corridor at speed; stairstepping
alone sticks you on anything that is not square to the grid. Three attempts,
then vanilla gives up and stairsteps. -/
private def slideMove (lvl : Level) (blocked : Float → Float → Bool)
    (p : Player) : Player := Id.run do
  let mut p := p
  for _ in [0:3] do
    match stepTo lvl blocked p (p.x + p.momX) (p.y + p.momY) with
    | some p' => return p'
    | none =>
      let some (frac, dx, dy) := slideHit lvl p | return stairStep lvl blocked p
      -- edge up to the wall, keeping a hair clear of it
      let frac := max 0.0 (frac - 0.03)
      let advanced :=
        if frac ≤ 0.0 then p
        else (stepTo lvl blocked p (p.x + p.momX * frac)
                (p.y + p.momY * frac)).getD p
      -- project the remainder onto the wall (`P_HitSlideLine`)
      let rest := 1.0 - frac
      let proj := (p.momX * rest) * dx + (p.momY * rest) * dy
      p := { advanced with momX := proj * dx, momY := proj * dy }
      if Float.abs p.momX < stopSpeed && Float.abs p.momY < stopSpeed then
        return { p with momX := 0, momY := 0 }
  return stairStep lvl blocked p

/-- One tic of player physics. `blocked` reports solid mobjs in the way;
`noclip` walks through everything. -/
def move (input : Input) (lvl : Level) (blocked : Float → Float → Bool)
    (p : Player) (noclip : Bool := false) : Player := Id.run do
  let mut p := p
  -- Turning: keyboard at fixed speed, mouse by accumulated delta.
  let turnSpeed := if input.run then turnRun else turnWalk
  if input.turnLeft then p := { p with angle := p.angle + turnSpeed }
  if input.turnRight then p := { p with angle := p.angle - turnSpeed }
  if input.mouseDx != 0 then
    p := { p with angle := p.angle - Float.ofInt input.mouseDx * 0.005 }
  -- keep the angle wrapped to (−π, π]: under sustained turning it otherwise
  -- grows without bound, eroding trig precision and the save's 16.16 range
  p := { p with angle := wrapAngle p.angle }

  -- Thrust (only when standing on the ground, like vanilla).
  let onGround := p.z ≤ (checkPosition lvl p.x p.y p.z |>.getD p.z)
  if onGround then
    let fwd := (if input.forward then 1.0 else 0.0)
             - (if input.back then 1.0 else 0.0)
    let side := (if input.strafeRight then 1.0 else 0.0)
              - (if input.strafeLeft then 1.0 else 0.0)
    let thrust := if input.run then thrustRun else thrustWalk
    let sThrust := if input.run then strafeRun else strafeWalk
    p := { p with
      momX := p.momX + fwd * thrust * Float.cos p.angle
                     + side * sThrust * Float.sin p.angle
      momY := p.momY + fwd * thrust * Float.sin p.angle
                     - side * sThrust * Float.cos p.angle }

  -- Vanilla clamps momentum to `MAXMOVE` before moving with it, so even a
  -- point-blank blast cannot fling you further than 30 units in a tic.
  p := { p with momX := max (-maxMove) (min maxMove p.momX)
                momY := max (-maxMove) (min maxMove p.momY) }
  -- Horizontal move with collision, then friction.
  p := if noclip then { p with x := p.x + p.momX, y := p.y + p.momY }
       else slideMove lvl blocked p
  -- Friction is ground-only: `P_XYMovement` returns before applying it when
  -- the thing is off its floor, which is what lets a fall or a rocket jump
  -- carry its speed the whole way instead of bleeding out mid-air.
  if p.z ≤ (checkPosition lvl p.x p.y p.z |>.getD p.z) then
    p := { p with momX := p.momX * friction, momY := p.momY * friction }
    if Float.abs p.momX < stopSpeed && Float.abs p.momY < stopSpeed then
      p := { p with momX := 0, momY := 0 }

  -- Vertical: fall to (or stay on) the floor of the current position.
  let floorZ := match checkPosition lvl p.x p.y p.z with
    | some f => f
    | none => (lvl.sectors[lvl.sectorAt p.x p.y]!).floorH
  if p.z > floorZ then
    -- vanilla `P_ZMovement` starts a fall from rest at double gravity:
    -- `if (momz == 0) momz = -GRAVITY*2; else momz -= GRAVITY`
    let momZ := if p.momZ == 0 then -2 * gravity else p.momZ - gravity
    let z := max floorZ (p.z + momZ)
    p := { p with z, momZ := if z == floorZ then 0 else momZ }
  else
    p := { p with z := floorZ, momZ := 0 }
  return p

/-- Vanilla `P_CalcHeight`'s eye spring. A landing drives `eyeHeight` down
through `eyeDelta`; it climbs back to `viewHeight`, is never allowed below
half of it, and the spring stiffens by a quarter each tic. The walk bob
itself is not stored — it is a pure function of momentum and the clock, so
`GameState.viewZ` works it out when the frame is drawn. -/
def calcHeight (p : Player) : Player := Id.run do
  let mut p := p
  if p.eyeDelta != 0 || p.eyeHeight != viewHeight then
    p := { p with eyeHeight := p.eyeHeight + p.eyeDelta }
    if p.eyeHeight > viewHeight then
      p := { p with eyeHeight := viewHeight, eyeDelta := 0 }
    if p.eyeHeight < viewHeight / 2 then
      p := { p with eyeHeight := viewHeight / 2 }
      -- vanilla's `deltaviewheight = 1` is one *fixed-point* unit — 1/65536
      -- of a map unit, just enough to set the spring climbing, not a whole
      -- unit's worth of kick
      if p.eyeDelta ≤ 0 then p := { p with eyeDelta := 1.0 / 65536.0 }
    if p.eyeDelta != 0 then
      p := { p with eyeDelta := p.eyeDelta + 0.25 }
      -- vanilla's `if (!deltaviewheight) deltaviewheight = 1`: a spring that
      -- lands exactly on zero would stall there, leaving the eye crouched
      -- for good. One fixed-point unit is enough to get it climbing again.
      if p.eyeDelta == 0 then p := { p with eyeDelta := 1.0 / 65536.0 }
  return p

end Player

/-- Where an eye `rise` above feet at `z` may actually sit at `(x, y)`.

Vanilla keeps the camera 4 below the ceiling, and never lets that clamp push
it under the feet — a 24-tall crawlspace holds the eye just under its
ceiling rather than above it. Every camera in DILL goes through here: the
live view, the offline `view`/`bench`/`fly` renders, and the test probes.
They used to spell `floorH + 41` out separately, which is only right in a
room with headroom; in E1M4's 24-tall duct (floor 160, ceiling 184) it put
the eye at 201 — outside the sector, looking at nothing. -/
def Level.eyeZ (lvl : Level) (x y z rise : Float) : Float :=
  let ceil := (lvl.sectors[lvl.sectorAt x y]!).ceilH
  max z (min (ceil - 4.0) (z + rise))

/-- Where the camera sits this tic: the eye height plus vanilla's walk bob,
`(momx² + momy²) / 4` capped at `MAXBOB`, swung by a sine that comes round
every 20 tics. Clamped into the room by `Level.eyeZ`. -/
def GameState.viewZ (g : GameState) : Float :=
  let p := g.player
  -- Vanilla `P_CalcHeight` suspends the bob in the air: off the ground the
  -- view is plain `z + viewheight`, so a fall or a rocket jump flies with
  -- a steady eye. Same on-ground test `Player.move` thrusts by.
  if p.z > (Player.checkPosition g.level p.x p.y p.z |>.getD p.z) then
    g.level.eyeZ p.x p.y p.z p.eyeHeight
  else
    let speed2 := p.momX * p.momX + p.momY * p.momY
    let bobAmt := min Player.maxBob (speed2 / 4.0)
    -- `FINEANGLES/20 * leveltime`: one full swing every 20 tics
    let phase := Float.ofNat (g.tics % 20) * (6.28318530718 / 20.0)
    let bob := (bobAmt / 2.0) * Float.sin phase
    g.level.eyeZ p.x p.y p.z (p.eyeHeight + bob)

/-- Fresh state for a map, with light effects running; `carry` brings the
previous map's vitals and arsenal along. -/
def GameState.newGame (level : Level) (carry : Option PlayerStatus := none)
    (skill : Nat := 4) : GameState :=
  (GameState.start level carry skill).spawnLights

/-- E1M1 → … → E1M8 → done, MAP01 → … → MAP30 → done, and the secret-level
detours of both. The rules live in `Dill.Maps`; an unrecognized map name
ends the run, as it did when this matched `ExMy` directly. -/
def nextMapName (cur : String) (secret : Bool) : Option String := do
  let id ← MapId.parse cur
  return (← id.next secret).name

/-- Advance the world one tic (1/35 s): the player moves and acts, items
are scooped up, monsters think, movers glide, flashes fade. -/
def tick (input : Input) (g : GameState) : GameState :=
  let oldX := g.player.x
  let oldY := g.player.y
  let g :=
    if g.status.dead then g
    else
      let blocked := fun nx ny =>
        g.mobjBlocked 0 nx ny Player.radius (blockedByPlayer := false)
      -- Fresh off a teleporter the player holds still for a moment
      -- (vanilla `reactiontime`: `P_PlayerThink` skips `P_MovePlayer`).
      -- Only movement is suspended — Use is handled below, outside the
      -- guard, exactly as vanilla does it.
      let moveInput := if g.teleFreeze > 0 then ({} : Input) else input
      let fallSpeed := g.player.momZ
      let moved := g.player.move moveInput g.level blocked g.status.noclip
      -- Vanilla `P_ZMovement`: coming down harder than `GRAVITY*8` squats the
      -- view and knocks the wind out of you. The squat springs back through
      -- `eyeDelta` in `calcHeight` below.
      let hardLanding := fallSpeed < -8.0 && moved.momZ == 0
      let moved := if hardLanding
        then { moved with eyeDelta := fallSpeed / 8.0 } else moved
      let g := { g with player := moved.calcHeight }
      let g := if hardLanding
        then g.playSound Sfx.oof g.player.x g.player.y else g
      -- vanilla `P_TryMove` skips crossed-special processing entirely for
      -- an `MF_NOCLIP` mover, so noclipping over an exit line or into a
      -- teleporter does nothing (p_map.c)
      let g := if g.status.noclip then g else g.crossSpecials oldX oldY
      if input.use && !g.useHeld then g.useLines else g
  let g := { g with useHeld := input.use }
  let g := g.markSeen
  let g := g.tickWeapon input
  -- a hitscan shot this tic may have crossed a gun-triggered line
  let g := if g.firedShot
    then { g.shootSpecialLines g.player.x g.player.y g.player.angle
             g.firedRange
           with firedShot := false }
    else g
  let g := g.touchItems
  let g := g.tickMobjs
  -- monster hitscans get the same deferred pass, gated inside
  -- `shootSpecialLines` to the one special vanilla lets them fire (46)
  let g := if g.monsterShots.isEmpty then g
    else { g.monsterShots.foldl (fun g' (x, y, a, r) =>
             g'.shootSpecialLines x y a r (player := false)) g
           with monsterShots := #[] }
  -- Was the player resting on their floor before the sector planes moved?
  -- Vanilla snaps such a thing onto the new floor from inside
  -- `P_ChangeSector` → `P_ThingHeightClip` ("walking monsters rise and fall
  -- with the floor"), so a lift carries you rather than dropping away
  -- beneath you. That is also what keeps `onground` true while it travels:
  -- a floor descending at 4 units a tic outruns a fall that starts at 1, so
  -- without this you hang in the air the whole way down and `P_MovePlayer`
  -- refuses to thrust — you ride the lift unable to walk.
  let riding := !g.status.dead &&
    (match Player.checkPosition g.level g.player.x g.player.y g.player.z with
     | some f => Float.abs (g.player.z - f) < 0.001
     | none => false)
  let moverSectors := g.movers.map (·.sector)
  let g := g.stepMovers
  let g := g.glueRiders moverSectors
  let g := if !riding then g else
    match Player.checkPosition g.level g.player.x g.player.y g.player.z with
    | some f => { g with player := { g.player with z := f, momZ := 0 } }
    | none => g
  let g := g.stepScrollers
  let g := g.stepButtons
  let g := g.stepLights
  let g := g.damageFloor
  -- stepping into a secret sector scores it (and un-marks it, like vanilla)
  let g := Id.run do
    let s := g.level.sectorAt g.player.x g.player.y
    -- `P_PlayerInSpecialSector` wants both feet on the sector's own floor
    -- ("Falling, not all the way down yet?") — flying over a secret pit,
    -- or riding a lift above one, scores nothing
    if g.level.sectors[s]!.special == 9 && !g.status.dead
        && g.player.z == g.level.sectors[s]!.floorH then
      let sectors := g.level.sectors.modify s ({ · with special := 0 })
      return { g with level := { g.level with sectors }
                      secrets := g.secrets + 1
                      -- Vanilla says nothing here: `P_PlayerInSpecialSector`
                      -- bumps `secretcount`, clears the special, and leaves
                      -- you to find out at the intermission tally. Announcing
                      -- it is a source-port convention we follow on purpose —
                      -- a secret you never noticed finding may as well not
                      -- have been one.
                      message := "A SECRET IS REVEALED!" }
    return g
  let g := g.stepFace
  let st := g.status
  -- Every countdown below is a `Nat`, and `Nat` subtraction *truncates*: at 0
  -- it stays 0 rather than wrapping. That is load-bearing, not an oversight —
  -- it is what lets each timer be run down unconditionally, with no `if > 0`
  -- guard and no risk of a huge number appearing underneath one. Anything
  -- here that grows a signed type has to grow a guard with it.
  let g := { g with status := { st with
    damageCount := st.damageCount - 1
    bonusCount := st.bonusCount - 1
    invulnTics := st.invulnTics - 1
    invisTics := st.invisTics - 1
    radsuitTics := st.radsuitTics - 1
    gogglesTics := st.gogglesTics - 1
    -- the berserk wash counts up from 1 (0 = never taken) and fades itself
    berserkTics := if st.berserkTics == 0 then 0 else st.berserkTics + 1 } }
  { g with tics := g.tics + 1 }

end Dill
