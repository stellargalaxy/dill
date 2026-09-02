import Dill.Game.State
import Dill.Game.Sfx
import Dill.Game.Combat

/-!
# Line specials: doors, lifts, moving floors, the exit

A linedef's `special` makes it do something when used (spacebar) or walked
over. The effect is a *mover*: a sector whose ceiling or floor glides to a
target height over several tics, possibly waits, and possibly comes back.
Movers live in `GameState` and are advanced by `tick`.

Implemented: the specials Doom and Doom II actually use — doors by use,
walk-over and key (including the blazing variants and the key-locked
switches), lifts and perpetual platforms, the floor and ceiling families,
crushers, stair builders, the light specials, the teleports, and the exits.
One-shot (`W1`/`S1`) variants clear the linedef's special afterwards, like
vanilla.

Two vanilla rules about *who* trips a line live here. `activateLine` takes
the side the actor crossed from, because `EV_Teleport` refuses a crossing
from the back of the line — that is what lets you step off a landing pad
instead of being thrown straight back. And a non-player reaches only the
handful of specials `monsterCanCross` lists, which is how monsters use
teleporters (39/97, plus the monster-only 125/126) without being able to
work switches and doors meant for the player.
-/

namespace Dill

/-- Vanilla's "and change": adopt a flat and sector special. The plain
specials pass `none` and leave the sector's own alone. -/
def Level.applyChange (lvl : Level) (s : Nat) :
    Option (String × Nat) → Level
  | none => lvl
  | some (flat, spec) =>
    let sectors := lvl.sectors.modify s
      (fun sec => { sec with floorFlat := flat, special := spec })
    { lvl with sectors }

/-- Advance a mover one tic. Returns the updated level and the mover's
continuation (`none` when its motion is finished). `crawl` slows a crushing
plane's harmful stroke to ⅛ speed — vanilla `T_MoveCeiling` drops a
`crushAndRaise`/`lowerAndCrush` ceiling to `CEILSPEED/8` while its move
reports `crushed`; `stepMovers` passes it while a live body is squeezed. -/
def Mover.step (lvl : Level) (crawl : Bool := false) : Mover → Level × Option Mover
  | .door s top wait closing stay speed delay =>
    -- `delay` (the timed doors of sector specials 10/14) is run down by
    -- `stepMovers` before the door is ever stepped; it is threaded through
    -- untouched here so the invariant survives a direct call
    let sec := lvl.sectors[s]!
    if closing then
      let h := max sec.floorH (sec.ceilH - speed)
      (lvl.setCeil s h, if h == sec.floorH then none
                        else some (.door s top wait closing stay speed delay))
    else if sec.ceilH < top then
      let h := min top (sec.ceilH + speed)
      (lvl.setCeil s h, some (.door s top wait false stay speed delay))
    else if stay then
      (lvl, none)
    else if wait > 0 then
      (lvl, some (.door s top (wait - 1) false stay speed delay))
    else
      (lvl, some (.door s top 0 true stay speed delay))
  | .lift s low high wait rising speed stalled =>
    let sec := lvl.sectors[s]!
    if rising then
      let h := min high (sec.floorH + speed)
      (lvl.setFloor s h, if h == high then none
                         else some (.lift s low high wait true speed stalled))
    else if sec.floorH > low then
      let h := max low (sec.floorH - speed)
      (lvl.setFloor s h, some (.lift s low high wait false speed stalled))
    else if wait > 0 then
      (lvl, some (.lift s low high (wait - 1) false speed stalled))
    else
      (lvl, some (.lift s low high 0 true speed stalled))
  | .floorDown s target speed change =>
    let sec := lvl.sectors[s]!
    let h := max target (sec.floorH - speed)
    let lvl := lvl.setFloor s h
    if h == target then (lvl.applyChange s change, none)
    else (lvl, some (.floorDown s target speed change))
  | .floorUp s target speed change crush =>
    let sec := lvl.sectors[s]!
    let h := min target (sec.floorH + speed)
    let lvl := lvl.setFloor s h
    if h == target then (lvl.applyChange s change, none)
    else (lvl, some (.floorUp s target speed change crush))
  | .ceiling s target speed crush =>
    let sec := lvl.sectors[s]!
    -- only the descent crawls: rising off a victim never crushes
    let down := if crawl then speed / 8 else speed
    let h := if sec.ceilH > target then max target (sec.ceilH - down)
             else min target (sec.ceilH + speed)
    (lvl.setCeil s h,
     if h == target then none else some (.ceiling s target speed crush))
  | .closeOpen s top wait reopening =>
    -- vanilla `close30ThenOpen`: down, sit shut for `wait`, up, stay open
    let sec := lvl.sectors[s]!
    if reopening then
      let h := min top (sec.ceilH + Speeds.door)
      (lvl.setCeil s h, if h == top then none
                        else some (.closeOpen s top wait true))
    else if sec.ceilH > sec.floorH then
      let h := max sec.floorH (sec.ceilH - Speeds.door)
      (lvl.setCeil s h, some (.closeOpen s top wait false))
    else if wait > 0 then
      (lvl, some (.closeOpen s top (wait - 1) false))
    else
      (lvl, some (.closeOpen s top 0 true))
  | .crusher s top low down stalled =>
    -- never finishes on its own; a "stop crusher" line stalls it. Vanilla
    -- grinds at `CEILSPEED` — only `fastCrushAndRaise` doubles it, which is
    -- not modelled — dropping to `CEILSPEED/8` while crushing (`crawl`); the
    -- up stroke restores full speed, as `T_MoveCeiling`'s up case does.
    let sec := lvl.sectors[s]!
    if down then
      let sp := if crawl then Speeds.ceiling / 8 else Speeds.ceiling
      let h := max low (sec.ceilH - sp)
      (lvl.setCeil s h, some (.crusher s top low (h > low) stalled))
    else
      let h := min top (sec.ceilH + Speeds.ceiling)
      (lvl.setCeil s h, some (.crusher s top low (h == top) stalled))
  | .perpetual s low high wait rising stalled =>
    -- Vanilla `T_PlatRaise` (perpetualRaise) pauses `plat->wait` at BOTH
    -- ends, not just the bottom: `wait > 0` is the pause, wherever the plat
    -- sits, and `rising` is the direction it heads when the count lapses.
    -- Arriving at either bound parks it for the full wait, turned around.
    let sec := lvl.sectors[s]!
    if wait > 0 then
      (lvl, some (.perpetual s low high (wait - 1) rising stalled))
    else if rising then
      let h := min high (sec.floorH + Speeds.liftPerpetual)
      (lvl.setFloor s h, some (.perpetual s low high
        (if h == high then Speeds.liftWait else 0) (h < high) stalled))
    else
      let h := max low (sec.floorH - Speeds.liftPerpetual)
      -- still heading down until the floor actually lands on `low`; only
      -- the arrival turns it around (mirroring the rising branch's `h < high`)
      (lvl.setFloor s h, some (.perpetual s low high
        (if h == low then Speeds.liftWait else 0) (h == low) stalled))

namespace GameState

/-- Did a trigger actually do anything?

Vanilla's `EV_DoDoor`/`EV_DoPlat`/`EV_DoFloor`/… return 0 when every tagged
sector was already busy, or when no sector carries the tag at all, and
`P_UseSpecialLine` then leaves the line armed and its switch unflipped. Every
starter below reports success the same way — by leaving a different mover
list, whether it pushed one (a door opening), dropped one (a "stop crusher"
line), or rewrote one in place (a DR door reversing mid-motion) — so
comparing the lists catches all three without threading a success flag
through seventy call sites.

Without this an S1 switch pressed while its own door is still open burns
itself: the special is cleared and the switch flips, but no door was
started, and once the door closes the way is shut for good. -/
private def started (before after : GameState) : Bool :=
  before.movers != after.movers

/-- Start a mover unless its sector already has one. Floors and ceilings
announce themselves through `stepMovers`, which grinds `DSSTNMOV` while a
plane travels and thumps `DSPSTOP` when it arrives — vanilla's
`T_MoveFloor`. Starting the grind here instead would fire once and fall
silent, and a stair builder starting eight movers at once would loose eight
copies on the same tic and then build the whole staircase in silence. -/
private def addMover (g : GameState) (m : Mover) : GameState :=
  if g.movers.any (·.sector == m.sector) then g
  else { g with movers := g.movers.push m }

/-- Make a sound *at the moving sector*, so a door across the map is heard
faint and far, not at the player's ear. Falls back to the player for a
sector with no geometry to speak of. -/
private def soundAtSector (g : GameState) (sfx : Nat) (s : Nat) : GameState :=
  match g.level.sectorSoundPos s with
  | some (x, y) => g.playSound sfx x y
  | none => g.playSound sfx g.player.x g.player.y

/-- Open a door sector. `manual` marks vanilla's DR doors (used directly
by their back side): using one that's already moving *reverses* it.
Tagged doors (walk-over and switch types) instead ignore the trigger
while the sector is busy — vanilla never toggles those, which matters
when a doorway's own walk lines fire as you pass through it. -/
private def addDoor (g : GameState) (s : Nat) (stay : Bool)
    (manual : Bool := false) (speed : Float := Speeds.door) : GameState :=
  match g.movers.findIdx? (·.sector == s) with
  | some i =>
    if !manual then g else
    match g.movers[i]! with
    -- a reversal keeps the door's *own* pace, not the reversing line's: it
    -- is the same door still moving, and a blazing door caught mid-slam
    -- should slam back. The flip is *silent* — vanilla's `EV_VerticalDoor`
    -- specialdata path just rewrites `door->direction` and returns, so a
    -- repressed DR door makes no fresh noise (unlike a tag door bounced
    -- off someone's head, which `stepMovers` announces).
    -- `delay` rides through the flip: a timed door (sector special 10/14,
    -- still counting down) behind a DR line must keep its countdown, not
    -- have it reset to 0 by a press.
    | .door s' top _ closing stay' sp delay =>
      let flipped := Mover.door s' top Speeds.doorWait (!closing) stay' sp delay
      { g with movers := g.movers.set! i flipped }
    | _ => g  -- some other mover owns this sector; leave it alone
  | none =>
    let top := g.level.lowestNeighborCeil s - 4
    -- Vanilla `EV_DoDoor` voices a tag door only when it has somewhere to
    -- go (`if door->topheight != sec->ceilingheight`), so re-triggering a
    -- door that already stands open is quiet. A manual (DR) door always
    -- speaks — `EV_VerticalDoor` has no such guard.
    let g := if manual || g.level.sectors[s]!.ceilH != top
             then g.soundAtSector Sfx.doorOpen s else g
    g.addMover (.door s top Speeds.doorWait false stay speed)

/-- Start a crusher: the ceiling grinds between 8 above the floor and
where it sits now. The busy check comes *before* the start sound (here and
in the lift/close starters below): vanilla skips a busy sector silently,
so hammering a repeatable trigger while the mover runs must not emit a
fresh `DSPSTART`/`DSDORCLS` per press. A crusher parked in stasis by a
"stop crusher" line is *resumed* instead — vanilla `EV_DoCeiling` calls
`P_ActivateInStasisCeiling` first, and the old thinker picks up mid-stroke
with its original bounds while its sector still refuses a new one. -/
private def addCrusher (g : GameState) (s : Nat) : GameState :=
  match g.movers.findIdx? (·.sector == s) with
  | some i =>
    let m := g.movers[i]!
    if m.stalled && m.isCeiling then
      { g with movers := g.movers.set! i m.unstall }
    else g
  | none =>
    let sec := g.level.sectors[s]!
    let g := g.soundAtSector Sfx.platStart s
    g.addMover (.crusher s sec.ceilH (sec.floorH + 8) true)

private def addPerpetual (g : GameState) (s : Nat) : GameState :=
  match g.movers.findIdx? (·.sector == s) with
  | some i =>
    -- vanilla `EV_DoPlat` (perpetualRaise) first wakes every in-stasis plat
    -- on the tag (`P_ActivateInStasis`) — and that reaches a stopped
    -- ordinary lift too, since `EV_StopPlat` stalls any plat kind
    let m := g.movers[i]!
    if m.stalled && m.isPlat then
      { g with movers := g.movers.set! i m.unstall }
    else g
  | none =>
    let sec := g.level.sectors[s]!
    -- vanilla bounds (`EV_DoPlat`): lowest neighbouring floor up to the
    -- HIGHEST neighbouring floor, each clamped so the current floor stays
    -- inside the range; the opening direction is a coin flip off the
    -- random stream (`plat->status = P_Random()&1`, 0 = up).
    let low := g.level.lowestNeighborFloor s        -- seeded with own floor
    let high := max (g.level.highestNeighborFloor s) sec.floorH
    let (roll, g) := g.rand
    let g := g.soundAtSector Sfx.platStart s
    -- wait 0: vanilla starts it moving at once (`plat->status = P_Random()&1`
    -- with no opening count), and `wait > 0` now means "parked at an end"
    g.addMover (.perpetual s low high 0 (roll &&& 1 == 0))

/-- Stall the mover owning this sector, if it is one `kind` accepts — how
vanilla's "stop crusher" and "stop lift" lines work, since those movers
never end by themselves. The stop is *stasis*, not removal: `EV_StopPlat`
and `EV_CeilingCrushStop` null the thinker function but keep the struct,
so the matching start line later resumes it with its original bounds
(`Mover.stall`/`unstall`). A kind with no stasis form (a plain `.ceiling`
caught by a stop-crusher line) is dropped, as before.

`kind` is not decoration. Vanilla keeps its thinkers in separate lists and
`EV_CeilingCrushStop` walks `activeceilings` while `EV_StopPlat` walks
`activeplats`, so a stop-crusher line pointed at a sector running a lift
does nothing at all. Filtering on the sector alone let either line halt
whichever mover happened to own it. -/
private def stopMovers (g : GameState) (kind : Mover → Bool) (s : Nat) :
    GameState :=
  { g with movers := g.movers.filterMap fun m =>
      if m.sector != s || !kind m || m.stalled then some m
      else m.stall }

/-- Set every sector tagged by `line` to one light level. -/
private def lightsTo (g : GameState) (line : Linedef) (lit : Nat) :
    GameState :=
  let lvl := (g.level.sectorsTagged line.tag).foldl
    (fun l s => l.setLight s lit) g.level
  { g with level := lvl }

/-- Vanilla `EV_LightTurnOn(line, 0)` (specials 12/80): each tagged sector
takes its brightest *neighbour's* light — its own is never consulted. The
running `bright` is deliberately carried from one tagged sector to the
next, as vanilla's single local variable is: an earlier sector's brighter
neighbourhood spills into every later one. -/
private def lightsToBrightest (g : GameState) (line : Linedef) : GameState :=
  Id.run do
    let mut lvl := g.level
    let mut bright := 0
    for s in g.level.sectorsTagged line.tag do
      for n in lvl.neighbors s do
        bright := max bright lvl.sectors[n]!.light
      lvl := lvl.setLight s bright
    return { g with level := lvl }

/-- Vanilla `EV_TurnTagLightsOff` (special 104): each tagged sector dims to
the minimum of its *own* light and its neighbours' — computed per sector,
with no carry-over between them. -/
private def lightsToDimmest (g : GameState) (line : Linedef) : GameState :=
  Id.run do
    let mut lvl := g.level
    for s in g.level.sectorsTagged line.tag do
      let dim := (lvl.neighbors s).foldl
        (fun l n => min l lvl.sectors[n]!.light) lvl.sectors[s]!.light
      lvl := lvl.setLight s dim
    return { g with level := lvl }

/-- Vanilla `EV_StartLightStrobing` (special 17): every tagged sector gets a
slow strobe — `P_SpawnStrobeFlash(sec, SLOWDARK, 0)`: 35 dark tics, an
out-of-phase opening count off the random stream, and the strobe's usual
"all one brightness falls to 0" fallback. The busy check is vanilla's
`sec->specialdata`, which light thinkers never set — only a sector *mover*
blocks the spawn. -/
private def startTagStrobe (g : GameState) (tag : Nat) : GameState :=
  Id.run do
    let mut g := g
    for s in g.level.sectorsTagged tag do
      if g.movers.any (·.sector == s) then continue
      let maxL := g.level.sectors[s]!.light
      let minL := g.level.minNeighborLight s
      let minL := if minL == maxL then 0 else minL
      let (roll, g') := g.rand
      g := { g' with lights :=
        g'.lights.push (.strobe s minL maxL 35 ((roll &&& 7) + 1)) }
    return g

/-- Close a door sector: a door mover started on its downward leg, with no
wait and no return trip. -/
private def closeDoor (g : GameState) (s : Nat)
    (speed : Float := Speeds.door) : GameState :=
  if g.movers.any (·.sector == s) then g else
  let g := g.soundAtSector Sfx.doorClose s
  g.addMover (.door s g.level.sectors[s]!.ceilH 0 true true speed)

/-- Specials 16/76: close, sit shut 30 seconds, reopen (vanilla
`close30ThenOpen`). The reopening height is the ceiling it closes from. -/
private def close30Door (g : GameState) (s : Nat) : GameState :=
  if g.movers.any (·.sector == s) then g else
  let g := g.soundAtSector Sfx.doorClose s
  g.addMover (.closeOpen s g.level.sectors[s]!.ceilH (30 * 35) false)

private def addLift (g : GameState) (s : Nat)
    (speed : Float := Speeds.lift) : GameState :=
  if g.movers.any (·.sector == s) then g else
  let g := g.soundAtSector Sfx.platStart s
  g.addMover (.lift s (g.level.lowestNeighborFloor s)
    g.level.sectors[s]!.floorH Speeds.liftWait false speed)

/-- Vanilla `EV_DoDonut`. The figure is three sectors deep: the tagged
"hole", the "ring" wrapped around it, and whatever lies beyond the ring.
Both halves move, at half the usual floor speed, and both aim at the floor
*beyond* — the hole sinks to it and the ring rises to meet it, the ring
taking that outer floor's flat and shedding its own sector special. The
result is one flush surface; lowering only the hole leaves it sunk a step
too far, in the middle of a ring that never moved. -/
private def addDonut (g : GameState) (hole : Nat) : GameState := Id.run do
  let speed := Speeds.floorRaiseChange
  -- the ring is across the hole's first line; the outer floor is across one
  -- of the ring's other lines
  let ring := g.level.neighbors hole
  let some s2 := ring[0]? | return g
  let outer := (g.level.neighbors s2).filter (fun n => n != hole && n != s2)
  let some s3 := outer[0]? | return g
  -- Vanilla `EV_DoDonut` skips the whole figure when the hole is busy
  -- (`if (s1->specialdata) continue`). It never checks the ring, but
  -- `addMover` would silently refuse either half, and half a donut is
  -- broken geometry — the ring rising alone, or the hole sinking beside a
  -- ring that never moves. Start both or neither; returning `g` unchanged
  -- reads as a refusal to `started`, so the switch is neither flipped nor
  -- its one-shot special spent — the convention every sibling EV follows.
  if g.movers.any (fun m => m.sector == hole || m.sector == s2) then return g
  let dest := g.level.sectors[s3]!.floorH
  let g := g.addMover (.floorDown hole dest speed)
  g.addMover (.floorUp s2 dest speed
    (some (g.level.sectors[s3]!.floorFlat, 0)))

/-- Vanilla `EV_BuildStairs`: the tagged sector rises `stepSize`, then each
next sector reached through a two-sided line with the same floor texture
rises `stepSize` more, and so on up the case. Specials 7/8 are `build8` —
8-unit steps at `Speeds.stair` (`FLOORSPEED/4`) — while 100/127 are
`turbo16`: 16-unit steps at `Speeds.stairTurbo` (`FLOORSPEED*4`).

Each step consults the current sector's own line list — vanilla's
`sec->lines[]`, which `Level.sectorLines` indexes at load and which holds
those lines in linedef order, so the step still lands on the same line a
sweep of the whole map would have found first. Sweeping was quadratic: a
thirty-step staircase on a twenty-thousand-line map re-read every linedef
thirty times, all to find a handful of lines already indexed. -/
private def buildStairs (g : GameState) (s0 : Nat)
    (stepSize : Float := 8) (speed : Float := Speeds.stair) :
    GameState := Id.run do
  let lvl := g.level
  let tex := lvl.sectors[s0]!.floorFlat
  let mut g := g
  let mut height := lvl.sectors[s0]!.floorH + stepSize
  g := g.addMover (.floorUp s0 height speed)
  let mut current := s0
  let mut visited : Array Nat := #[s0]
  let mut extended := true
  while extended do
    extended := false
    for i in (lvl.sectorLines[current]?).getD #[] do
      let line := lvl.linedefs[i]!
      let some back := line.back | continue
      let front := lvl.sidedefs[line.front]!.sector
      let backSec := lvl.sidedefs[back]!.sector
      if front == current && backSec != current
          && !visited.contains backSec
          && lvl.sectors[backSec]!.floorFlat == tex then
        height := height + stepSize
        g := g.addMover (.floorUp backSec height speed)
        visited := visited.push backSec
        current := backSec
        extended := true
        -- the step is found; the rest of this sector's lines cannot add to
        -- it, and the next pass starts from `current` anyway
        break
  return g

/-- Vanilla's blazing door family (`VDOORSPEED * 4`): Doom II's 105–118,
plus the six blazing key-locked doors. The ordinary locked doors 32–34 are
*not* blazing, and neither is anything from Doom 1. -/
def isBlazingDoor (special : Nat) : Bool :=
  match special with
  | 105 | 106 | 107 | 108 | 109 | 110 => true   -- WR / W1 raise, open, close
  | 111 | 112 | 113 | 114 | 115 | 116 => true   -- S1 / SR raise, open, close
  | 117 | 118 => true                            -- DR / D1 manual
  | 99 | 133 | 134 | 135 | 136 | 137 => true     -- the locked blazing doors
  | _ => false

/-- Vanilla's blazing lift family (`PLATSPEED * 8`): specials 120–123, the
fast down-wait-up-stay plats. E4M1's rocket-launcher lift is one of these. -/
def isBlazingLift (special : Nat) : Bool :=
  match special with
  | 120 | 121 | 122 | 123 => true
  | _ => false

/-- Specials a non-player can set off by walking over one — vanilla's
`if (!thing->player)` filter at the top of `P_CrossSpecialLine`. A monster
works the teleports, one door and the two lifts; every other special ignores
it completely, which is why monsters cannot open most doors for themselves. -/
def monsterCanCross (special : Nat) : Bool :=
  match special with
  | 39 | 97 | 125 | 126 => true    -- the teleports
  | 4 => true                      -- W1 raise door
  | 10 | 88 => true                -- W1/WR lift
  | _ => false

/-- Whisk an actor to the tagged sector's teleport-destination thing
(type 14), with fog at both ends — vanilla `EV_Teleport`. `actor` is `none`
for the player, `some i` for a mobj. The caller has already checked which
side the line was crossed from. -/
private def teleportActor (g : GameState) (tag : Nat)
    (actor : Option Nat := none) : GameState := Id.run do
  -- vanilla refuses outright to teleport a missile
  if let some i := actor then
    if g.mobjs[i]!.removed || g.mobjs[i]!.info.missile then return g
  for t in g.level.things do
    if t.type != 14 then continue
    if g.level.sectors[g.level.sectorAt t.x t.y]!.tag != tag then continue
    let angle := Float.ofInt t.angle * 3.14159265358979 / 180
    let destZ := g.level.sectors[g.level.sectorAt t.x t.y]!.floorH
    let (uid, radius, oldX, oldY) := match actor with
      | none => (0, Player.radius, g.player.x, g.player.y)
      | some i =>
        let m := g.mobjs[i]!
        (m.uid, m.info.radius, m.x, m.y)
    -- Vanilla `PIT_StompThing`: the player telefrags whatever is standing
    -- on the pad, but "monsters don't stomp things except on boss level" —
    -- off MAP30 a blocked monster simply does not teleport, and stays put.
    let mayStomp := actor.isNone || MapId.parse g.level.name == some (.level 30)
    -- Vanilla `PIT_StompThing` judges overlap by the square "blockdist"
    -- test — |dx| and |dy| each against the summed radii — not the true
    -- distance, so a diagonal near-miss still counts as standing on the pad.
    let blockers := Id.run do
      let mut out := #[]
      for j in [0:g.mobjs.size] do
        let o := g.mobjs[j]!
        if o.removed || !o.shootable || o.uid == uid then continue
        let rsum := o.info.radius + radius
        if Float.abs (o.x - t.x) < rsum && Float.abs (o.y - t.y) < rsum then
          out := out.push j
      return out
    -- a monster is blocked by the player too; the player never blocks itself
    let playerBlocks := actor.isSome && !g.status.dead
      && Float.abs (g.player.x - t.x) < Player.radius + radius
      && Float.abs (g.player.y - t.y) < Player.radius + radius
    if (!blockers.isEmpty || playerBlocks) && !mayStomp then return g
    let mut g := g
    for j in blockers do
      g := g.damageMobj j 10000
    if playerBlocks then
      g := g.damagePlayer 10000
    g := match actor with
      | none =>
        -- vanilla `thing->reactiontime = 18`, players only: stand still
        { g with teleFreeze := 18, player := { g.player with
            x := t.x, y := t.y, z := destZ
            momX := 0, momY := 0, momZ := 0, angle } }
      | some i =>
        g.setMobj i { g.mobjs[i]! with
          x := t.x, y := t.y, z := destZ
          momX := 0, momY := 0, momZ := 0, angle }
    g := (g.spawn .teleFog oldX oldY 0).1
    -- the arrival fog stands 20 units out along the destination's facing
    g := (g.spawn .teleFog (t.x + 20.0 * Float.cos angle)
                           (t.y + 20.0 * Float.sin angle) 0).1
    g := g.playSound Sfx.teleport oldX oldY
    return g.playSound Sfx.teleport t.x t.y
  return g

/-- Fire a special line. `byUse` distinguishes the spacebar from walking
over the line; each special answers to exactly one of them. `side` is which
side of the line the actor was on *before* crossing it (0 = front, 1 =
back), as vanilla passes to `P_CrossSpecialLine`; only the teleports look at
it. Use and gunfire always count as the front.

`actor` says who set it off: `none` for the player, `some i` for the mobj at
that index. A monster reaches only the handful of specials
`monsterCanCross` allows, and the monster-only teleports (125/126) answer to
nothing else.

Returns `none` when the line *refused* — an unhandled special/trigger
combination, a monster on a player-only line, or a key lock without the
key — so a switch only flips (and clunks) on a genuine activation. A W1
walk-over whose EV refused still returns `some`: the line burns out
regardless (`oneShotW` below), and walk-overs have no switch to flip. -/
def activateLineOpt (g : GameState) (lineIdx : Nat) (byUse : Bool)
    (side : Nat := 0) (actor : Option Nat := none) : Option GameState :=
  let line := g.level.linedefs[lineIdx]!
  if actor.isSome && !monsterCanCross line.special then none else
  -- Vanilla's *blazing* families move at four times the door rate and twice
  -- the lift rate. Which specials are blazing is decided once, here, and
  -- passed to every door/lift starter below, so adding a special to a family
  -- is a one-line change rather than a hunt through thirty match arms.
  let dspeed := if isBlazingDoor line.special then Speeds.doorBlaze
                else Speeds.door
  let lspeed := if isBlazingLift line.special then Speeds.liftBlaze
                else Speeds.lift
  -- A refusal, not a silent no-op, when nothing moved: see `started`.
  let tagged := fun (g' : GameState) (f : GameState → Nat → GameState) =>
    let after := (g'.level.sectorsTagged line.tag).foldl f g'
    if started g' after then some after else none
  -- A used one-shot (S1) burns only on success: vanilla clears the special
  -- in `P_ChangeSwitchTexture(line, 0)`, which `P_UseSpecialLine` reaches
  -- only when the EV reported something started.
  let oneShot := fun (g' : Option GameState) =>
    g'.map fun g' => { g' with level := g'.level.clearSpecial lineIdx }
  -- A walked one-shot (W1) burns unconditionally: `P_CrossSpecialLine`
  -- clears `line->special` without looking at the EV's answer, so a W1
  -- crossed while every tagged sector is busy is spent anyway — dead,
  -- armed-but-unfired. The gun one-shots (G1, 24/47) share this fate via
  -- `P_ShootSpecialLine`'s unconditional `P_ChangeSwitchTexture(line, 0)`.
  let oneShotW := fun (g' : Option GameState) =>
    let g' := g'.getD g
    some { g' with level := g'.level.clearSpecial lineIdx }
  -- Vanilla's "and change" family copies a flat, and sometimes a sector
  -- special, from a model sector. For everything that *rises* the model is
  -- the triggering line's own front sector and the copy happens at once;
  -- `lowerAndChange` instead waits and takes them from whatever it settles
  -- beside. `changeNow` is the immediate form: `spec` is `some 0` for the
  -- plat variants that explicitly cancel damage, `none` to leave it be.
  let frontSector := g.level.sidedefs[line.front]!.sector
  let modelFlat := g.level.sectors[frontSector]!.floorFlat
  let modelSpecial := g.level.sectors[frontSector]!.special
  -- Vanilla applies the change inside the same per-sector success path as
  -- the mover, so a sector already busy keeps its own flat and special —
  -- the retexture must not happen on a trigger that is about to refuse.
  let changeNow := fun (g' : GameState) (s : Nat) (spec : Option Nat) =>
    if g'.movers.any (·.sector == s) then g' else
    let sectors := g'.level.sectors.modify s fun sec =>
      { sec with floorFlat := modelFlat, special := spec.getD sec.special }
    { g' with level := { g'.level with sectors } }
  -- the deferred form: the flat and special of the neighbour already sitting
  -- at the height this floor is heading for
  let changeOnArrival := fun (g' : GameState) (s : Nat) (dest : Float) =>
    Id.run do
      for n in g'.level.neighbors s do
        let sec := g'.level.sectors[n]!
        if sec.floorH == dest then return some (sec.floorFlat, sec.special)
      return none
  -- Vanilla `turboLower` stops 8 above the highest neighbouring floor — but
  -- only when it has somewhere to go: `if (floordestheight != floorheight)
  -- floordestheight += 8`, so a floor already level with its highest
  -- neighbour stays put instead of popping up 8.
  let turboLowerDest := fun (g' : GameState) (s : Nat) =>
    let dest := g'.level.highestNeighborFloor s
    if dest != g'.level.sectors[s]!.floorH then dest + 8 else dest
  -- Vanilla `raiseFloor` heads for the lowest surrounding ceiling, clamped
  -- to the sector's OWN ceiling (`if floordestheight > sec->ceilingheight`),
  -- so a room taller than its doorways still stops at its own lid;
  -- `raiseFloorCrush` subtracts its 8 *after* that clamp.
  let raiseFloorDest := fun (g' : GameState) (s : Nat) =>
    min (g'.level.lowestNeighborCeil s) g'.level.sectors[s]!.ceilH
  let backDoor := fun (stay : Bool) (reversible : Bool) =>
    match line.back with
    | some back =>
      let ok := match line.special with
        | 26 | 32 => g.status.blueKey
        | 27 | 34 => g.status.yellowKey
        | 28 | 33 => g.status.redKey
        | _ => true
      if ok then
        -- as for `tagged`: a D1 press against a sector some other mover
        -- already owns must refuse, or `oneShot` spends the line on nothing.
        -- A DR reversal rewrites the mover in place, which `started` sees.
        let after := g.addDoor g.level.sidedefs[back]!.sector stay
          (manual := reversible) (speed := dspeed)
        if started g after then some after else none
      else none  -- locked, and the key isn't carried: a refusal
    | none => none
  match line.special, byUse with
  | 1, true | 26, true | 27, true | 28, true =>
    -- DR: door in the sector behind the line (26–28 want a key). Vanilla's
    -- EV_VerticalDoor reverses only these types when caught mid-motion;
    -- other door specials below just no-op against a busy sector.
    backDoor false true
  | 31, true =>  -- D1: door that stays open, one-shot switch
    oneShot (backDoor true false)
  | 2, false =>  -- W1: open tagged doors, stay
    oneShotW (tagged g (fun g' s => g'.addDoor s true (speed := dspeed)))
  | 90, false => -- WR: tagged door, open-wait-close
    tagged g (fun g' s => g'.addDoor s false (speed := dspeed))
  | 63, true =>  -- SR: tagged door, open-wait-close
    tagged g (fun g' s => g'.addDoor s false (speed := dspeed))
  | 61, true =>  -- SR: tagged door, stays open
    tagged g (fun g' s => g'.addDoor s true (speed := dspeed))
  | 103, true => -- S1: tagged door, stays open
    oneShot (tagged g (fun g' s => g'.addDoor s true (speed := dspeed)))
  | 62, true =>  -- SR: lift
    tagged g (fun g' s => g'.addLift s (speed := lspeed))
  | 21, true =>  -- S1: lift
    oneShot (tagged g (fun g' s => g'.addLift s (speed := lspeed)))
  | 88, false => -- WR: lift
    tagged g (fun g' s => g'.addLift s (speed := lspeed))
  | 36, false => -- W1: floor lowers to 8 above the highest neighbor (turbo)
    oneShotW (tagged g fun g' s =>
      g'.addMover (.floorDown s (turboLowerDest g' s) Speeds.floorTurbo))
  | 23, true =>  -- S1: floor lowers to the lowest neighbor
    oneShot (tagged g fun g' s =>
      g'.addMover (.floorDown s (g'.level.lowestNeighborFloor s) Speeds.floor))
  | 82, false => -- WR: floor lowers to the lowest neighbor
    tagged g (fun g' s =>
      g'.addMover (.floorDown s (g'.level.lowestNeighborFloor s) Speeds.floor))
  | 18, true =>  -- S1: floor rises to the next higher neighbor
    oneShot (tagged g fun g' s =>
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s) Speeds.floor))
  | 69, true =>  -- SR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s) Speeds.floor))
  | 102, true => -- S1: floor lowers to the highest neighbor
    oneShot (tagged g fun g' s =>
      g'.addMover (.floorDown s (g'.level.highestNeighborFloor s) Speeds.floor))
  | 19, false => -- W1: the same, on walking over
    oneShotW (tagged g fun g' s =>
      g'.addMover (.floorDown s (g'.level.highestNeighborFloor s) Speeds.floor))
  | 83, false => -- WR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.floorDown s (g'.level.highestNeighborFloor s) Speeds.floor))
  -- Doom II leans heavily on the *blazing* door and lift variants, which
  -- Doom 1 episode 1 never uses. Mechanically they are the doors and lifts
  -- above at four times the door rate and twice the lift rate, which
  -- `dspeed`/`lspeed` above carry into the same movers. Without these, 43%
  -- of Doom II's special linedefs — including MAP02's very first door — do
  -- nothing at all.
  | 117, true => -- DR: manual door, fast (MAP02's first door)
    backDoor false true
  | 118, true => -- D1: manual door, fast, stays open
    oneShot (backDoor true false)
  -- locked one-shot doors; `backDoor` does the key check, as for 26–28
  | 32, true | 33, true | 34, true =>
    oneShot (backDoor true false)
  -- Doom II's key-locked *switches*. Unlike 32–34, which open the sector
  -- behind the line, these open tagged sectors — MAP02's red bars are
  -- sixteen lines of 135 pointing at tag 7.
  | 99, true | 133, true | 134, true | 135, true | 136, true | 137, true =>
    let hasKey := match line.special with
      | 99 | 133 => g.status.blueKey
      | 134 | 135 => g.status.redKey
      | _ => g.status.yellowKey
    if !hasKey then none
    else
      let opened := tagged g (fun g' s => g'.addDoor s true (speed := dspeed))
      -- the odd numbers are S1 (one-shot); the even ones SR (repeatable)
      if line.special == 133 || line.special == 135 || line.special == 137
      then oneShot opened else opened
  | 111, true => -- S1: tagged door open-wait-close, fast
    oneShot (tagged g (fun g' s => g'.addDoor s false (speed := dspeed)))
  | 112, true => -- S1: tagged door, stays open, fast
    oneShot (tagged g (fun g' s => g'.addDoor s true (speed := dspeed)))
  | 114, true => -- SR: tagged door open-wait-close, fast
    tagged g (fun g' s => g'.addDoor s false (speed := dspeed))
  | 115, true => -- SR: tagged door, stays open, fast
    tagged g (fun g' s => g'.addDoor s true (speed := dspeed))
  | 108, false => -- W1: tagged door open-wait-close, fast
    oneShotW (tagged g (fun g' s => g'.addDoor s false (speed := dspeed)))
  | 109, false => -- W1: tagged door, stays open, fast
    oneShotW (tagged g (fun g' s => g'.addDoor s true (speed := dspeed)))
  | 105, false => -- WR: tagged door open-wait-close, fast
    tagged g (fun g' s => g'.addDoor s false (speed := dspeed))
  | 106, false => -- WR: tagged door, stays open, fast
    tagged g (fun g' s => g'.addDoor s true (speed := dspeed))
  -- fast lifts
  | 123, true => -- SR: lift, fast
    tagged g (fun g' s => g'.addLift s (speed := lspeed))
  | 122, true => -- S1: lift, fast
    oneShot (tagged g (fun g' s => g'.addLift s (speed := lspeed)))
  | 120, false => -- WR: lift, fast
    tagged g (fun g' s => g'.addLift s (speed := lspeed))
  | 121, false => -- W1: lift, fast
    oneShotW (tagged g (fun g' s => g'.addLift s (speed := lspeed)))
  -- The rest of the floor family. Vanilla spells out one linedef type per
  -- (trigger class × target height), so these are the same two movers over
  -- and over: what differs is where the floor is heading and whether the
  -- line is one-shot. The `tx` variants also retexture the sector, which
  -- is not modelled — the motion is the part that gates progress.
  | 38, false =>              -- W1: floor lowers to the lowest neighbour
    oneShotW (tagged g fun g' s =>
      g'.addMover (.floorDown s (g'.level.lowestNeighborFloor s) Speeds.floor))
  -- `lowerAndChange`: the floor drops and, on arrival, takes the flat and
  -- sector special of whatever neighbour it has come down to sit beside.
  | 37, false =>              -- W1: lower to lowest and change
    oneShotW (tagged g fun g' s =>
      let dest := g'.level.lowestNeighborFloor s
      g'.addMover (.floorDown s dest Speeds.floor (changeOnArrival g' s dest)))
  | 84, false =>              -- WR: the same, repeatable
    tagged g (fun g' s =>
      let dest := g'.level.lowestNeighborFloor s
      g'.addMover (.floorDown s dest Speeds.floor (changeOnArrival g' s dest)))
  | 60, true =>               -- SR: the same, on a switch
    tagged g (fun g' s =>
      g'.addMover (.floorDown s (g'.level.lowestNeighborFloor s) Speeds.floor))
  | 45, true =>               -- SR: floor lowers to the highest neighbour
    tagged g (fun g' s =>
      g'.addMover (.floorDown s (g'.level.highestNeighborFloor s) Speeds.floor))
  | 70, true =>               -- SR: floor lowers to 8 above the highest
    tagged g (fun g' s =>
      g'.addMover (.floorDown s (turboLowerDest g' s) Speeds.floorTurbo))
  | 71, true =>               -- S1: the same, one-shot
    oneShot (tagged g fun g' s =>
      g'.addMover (.floorDown s (turboLowerDest g' s) Speeds.floorTurbo))
  | 98, false =>              -- WR: the same, on walking over
    tagged g (fun g' s =>
      g'.addMover (.floorDown s (turboLowerDest g' s) Speeds.floorTurbo))
  -- `raiseToNearestAndChange` (20/22/47/68/95): the flat is copied from the
  -- triggering line's front sector at once, and the sector special is
  -- cleared outright — vanilla's "NO MORE DAMAGE, IF APPLICABLE". These are
  -- *plats*, not floors, and creep at `PLATSPEED/2` (`floorRaiseChange`).
  | 20, true =>               -- S1: raise to next higher and change
    oneShot (tagged g fun g' s =>
      let g' := changeNow g' s (some 0)
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s)
        Speeds.floorRaiseChange))
  | 68, true =>               -- SR: the same, repeatable
    tagged g (fun g' s =>
      let g' := changeNow g' s (some 0)
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s)
        Speeds.floorRaiseChange))
  | 22, false =>              -- W1: the same
    oneShotW (tagged g fun g' s =>
      let g' := changeNow g' s (some 0)
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s)
        Speeds.floorRaiseChange))
  | 95, false =>              -- WR: the same, repeatable
    tagged g (fun g' s =>
      let g' := changeNow g' s (some 0)
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s)
        Speeds.floorRaiseChange))
  | 119, false =>             -- W1: raise to next higher, no change
    oneShotW (tagged g fun g' s =>
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s) Speeds.floor))
  | 128, false =>             -- WR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s) Speeds.floor))
  | 5, false =>               -- W1: floor rises to the lowest ceiling
    oneShotW (tagged g fun g' s =>
      g'.addMover (.floorUp s (raiseFloorDest g' s) Speeds.floor))
  | 91, false =>              -- WR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.floorUp s (raiseFloorDest g' s) Speeds.floor))
  | 101, true =>              -- S1: the same, on a switch
    oneShot (tagged g fun g' s =>
      g'.addMover (.floorUp s (raiseFloorDest g' s) Speeds.floor))
  | 64, true =>               -- SR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.floorUp s (raiseFloorDest g' s) Speeds.floor))
  | 58, false =>              -- W1: floor rises 24
    oneShotW (tagged g fun g' s =>
      g'.addMover (.floorUp s (g'.level.sectors[s]!.floorH + 24) Speeds.floor))
  | 92, false =>              -- WR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.floorUp s (g'.level.sectors[s]!.floorH + 24) Speeds.floor))
  -- `raiseFloor24AndChange` (59/93) copies the flat *and* the special
  | 59, false =>              -- W1: rise 24 and change
    oneShotW (tagged g fun g' s =>
      let g' := changeNow g' s (some modelSpecial)
      g'.addMover (.floorUp s (g'.level.sectors[s]!.floorH + 24) Speeds.floor))
  | 93, false =>              -- WR: the same, repeatable
    tagged g (fun g' s =>
      let g' := changeNow g' s (some modelSpecial)
      g'.addMover (.floorUp s (g'.level.sectors[s]!.floorH + 24) Speeds.floor))
  -- the plat `raiseAndChange` switches (14/15/66/67) copy only the flat,
  -- and creep at the plat family's `PLATSPEED/2`
  | 15, true =>               -- S1: rise 24 and change
    oneShot (tagged g fun g' s =>
      let g' := changeNow g' s none
      g'.addMover (.floorUp s (g'.level.sectors[s]!.floorH + 24)
        Speeds.floorRaiseChange))
  | 66, true =>               -- SR: the same, repeatable
    tagged g (fun g' s =>
      let g' := changeNow g' s none
      g'.addMover (.floorUp s (g'.level.sectors[s]!.floorH + 24)
        Speeds.floorRaiseChange))
  | 67, true =>               -- SR: rise 32 and change
    tagged g (fun g' s =>
      let g' := changeNow g' s none
      g'.addMover (.floorUp s (g'.level.sectors[s]!.floorH + 32)
        Speeds.floorRaiseChange))
  | 140, true =>              -- S1: floor rises 512
    oneShot (tagged g fun g' s =>
      g'.addMover (.floorUp s (g'.level.sectors[s]!.floorH + 512) Speeds.floor))
  -- Doors that close rather than open. A door mover already handles the
  -- downward leg, so these start one mid-close with no wait and no return.
  | 3, false =>               -- W1: close
    oneShotW (tagged g fun g' s => g'.closeDoor s (speed := dspeed))
  | 75, false =>              -- WR: close
    tagged g (fun g' s => g'.closeDoor s (speed := dspeed))
  | 50, true | 113, true =>   -- S1: close (113 blazing)
    oneShot (tagged g fun g' s => g'.closeDoor s (speed := dspeed))
  | 42, true | 116, true =>   -- SR: close (116 blazing)
    tagged g (fun g' s => g'.closeDoor s (speed := dspeed))
  | 110, false =>             -- W1: close, blazing
    oneShotW (tagged g fun g' s => g'.closeDoor s (speed := dspeed))
  | 86, false =>              -- WR: open and stay
    tagged g (fun g' s => g'.addDoor s true (speed := dspeed))
  -- Ceilings and crushers. A crusher runs until a "stop" line parks it in
  -- stasis, so `stopMovers` is the counterpart to `addMover` here.
  | 40, false =>              -- W1: ceiling raises to the highest neighbour
    oneShotW (tagged g fun g' s =>
      g'.addMover (.ceiling s (g'.level.highestNeighborCeil s) Speeds.ceiling))
  | 41, true =>               -- S1: ceiling lowers to the floor
    oneShot (tagged g fun g' s =>
      g'.addMover (.ceiling s g'.level.sectors[s]!.floorH Speeds.ceiling))
  | 43, true =>               -- SR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.ceiling s g'.level.sectors[s]!.floorH Speeds.ceiling))
  -- 44/72 are vanilla `lowerAndCrush`, not a plain lower: the ceiling heads
  -- for 8 above the floor with `crush` set, grinding anyone beneath it
  -- rather than holding (`EV_DoCeiling(line, lowerAndCrush)`)
  | 44, false =>              -- W1: crushing ceiling lowers to 8 above the floor
    oneShotW (tagged g fun g' s =>
      g'.addMover (.ceiling s (g'.level.sectors[s]!.floorH + 8) Speeds.ceiling
        (crush := true)))
  | 72, false =>              -- WR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.ceiling s (g'.level.sectors[s]!.floorH + 8) Speeds.ceiling
        (crush := true)))
  | 6, false | 25, false =>   -- W1: crusher (6 is the fast one)
    oneShotW (tagged g fun g' s => g'.addCrusher s)
  | 73, false | 77, false =>  -- WR: crusher
    tagged g (fun g' s => g'.addCrusher s)
  | 49, true =>               -- S1: crusher
    oneShot (tagged g fun g' s => g'.addCrusher s)
  | 57, false =>              -- W1: stop the crusher (into stasis)
    oneShotW (tagged g fun g' s => g'.stopMovers Mover.isCeiling s)
  | 74, false =>              -- WR: the same, repeatable
    tagged g (fun g' s => g'.stopMovers Mover.isCeiling s)
  -- Perpetual lifts, and the lines that stop them.
  | 53, false =>              -- W1: perpetual lift
    oneShotW (tagged g fun g' s => g'.addPerpetual s)
  | 87, false =>              -- WR: the same, repeatable
    tagged g (fun g' s => g'.addPerpetual s)
  | 54, false =>              -- W1: stop the lift (into stasis)
    oneShotW (tagged g fun g' s => g'.stopMovers Mover.isPlat s)
  | 89, false =>              -- WR: the same, repeatable
    tagged g (fun g' s => g'.stopMovers Mover.isPlat s)
  -- Monster-only teleports: the player walks over these and nothing at all
  -- happens — vanilla keeps even the one-shot's burn-out inside the
  -- `if (!thing->player)` guard, so the line stays live for the monsters.
  | 125, false =>            -- W1, monsters only
    match actor with
    | none => none
    | some _ =>
      oneShotW (some (if side == 1 then g else g.teleportActor line.tag actor))
  | 126, false =>            -- WR, monsters only
    match actor with
    | none => none
    | some _ => some (if side == 1 then g else g.teleportActor line.tag actor)
  | 124, false =>             -- W1: secret exit
    some { g with exited := true, secretExit := true }
  | 127, true =>              -- S1: stairs (turbo16: 16-unit steps, fast)
    oneShot (tagged g fun g' s =>
      g'.buildStairs s (stepSize := 16) (speed := Speeds.stairTurbo))
  -- The gun-triggered family (G1/GR) matches `true` because that is what
  -- `shootSpecialLines` passes — and *only* it reaches them: `answersToUse`
  -- deliberately omits 24/46/47 so the spacebar never selects one, and the
  -- `false` column keeps walk-over (`crossSpecials`) off them, as vanilla's
  -- `P_CrossSpecialLine` has no case for any of the three.
  | 24, true =>               -- G1: shoot to raise the floor to the lowest
    -- ceiling (E2M4's shootable wall) — vanilla `raiseFloor`, own-ceiling
    -- clamp included. G1 burns even on refusal: `P_ShootSpecialLine` runs
    -- `P_ChangeSwitchTexture(line, 0)` without looking at the EV's answer.
    oneShotW (tagged g fun g' s =>
      g'.addMover (.floorUp s (raiseFloorDest g' s) Speeds.floor))
  | 46, true =>               -- GR: shoot to open a door and keep it open.
    -- Repeatable by type, but stray bullets keep crossing the line forever
    -- after the door has opened: once every tagged sector is already at its
    -- open height (or busy moving), re-firing is a *refusal* — no mover, no
    -- door sound, no switch flip — instead of a door that squeaks at every
    -- gunshot in the room.
    let openable := (g.level.sectorsTagged line.tag).filter fun s =>
      g.level.sectors[s]!.ceilH < g.level.lowestNeighborCeil s - 4
        && !g.movers.any (·.sector == s)
    if openable.isEmpty then none
    else some (openable.foldl (fun g' s => g'.addDoor s true (speed := dspeed)) g)
  | 47, true =>               -- G1: shoot to raise to next higher and change
    -- (a `raiseToNearestAndChange` plat, so `PLATSPEED/2` like 20/22/68/95);
    -- burns unconditionally, as 24 does
    oneShotW (tagged g fun g' s =>
      let g' := changeNow g' s (some 0)
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s)
        Speeds.floorRaiseChange))
  | 107, false =>             -- WR: tagged door closes, fast
    tagged g (fun g' s => g'.closeDoor s (speed := dspeed))
  | 16, false =>              -- W1: door closes, waits 30 s, opens again
    oneShotW (tagged g fun g' s => g'.close30Door s)
  | 130, false =>             -- W1: floor rises to the next higher, fast
    oneShotW (tagged g fun g' s =>
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s) Speeds.floorTurbo))
  | 129, false =>             -- WR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s) Speeds.floorTurbo))
  | 4, false =>               -- W1: tagged door, open-wait-close
    oneShotW (tagged g fun g' s => g'.addDoor s false (speed := dspeed))
  | 10, false =>              -- W1: lift
    oneShotW (tagged g fun g' s => g'.addLift s (speed := lspeed))
  | 100, false =>             -- W1: stairs (turbo16: 16-unit steps, fast)
    oneShotW (tagged g fun g' s =>
      g'.buildStairs s (stepSize := 16) (speed := Speeds.stairTurbo))
  | 131, true =>              -- S1: floor rises to the next higher (turbo)
    oneShot (tagged g fun g' s =>
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s) Speeds.floorTurbo))
  | 132, true =>              -- SR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.floorUp s (g'.level.nextHigherNeighborFloor s) Speeds.floorTurbo))
  -- `raiseFloorCrush`: up to 8 below the lowest ceiling (after the
  -- own-ceiling clamp), and marked `crush` — a body in the way is ground
  -- for 10 every fourth tic while the floor keeps rising, instead of
  -- holding the floor as the ordinary raises do
  | 56, false =>              -- W1: crushing floor
    oneShotW (tagged g fun g' s =>
      g'.addMover (.floorUp s (raiseFloorDest g' s - 8) Speeds.floor
        (crush := true)))
  | 94, false =>              -- WR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.floorUp s (raiseFloorDest g' s - 8) Speeds.floor
        (crush := true)))
  | 55, true =>               -- S1: crushing floor, on a switch
    oneShot (tagged g fun g' s =>
      g'.addMover (.floorUp s (raiseFloorDest g' s - 8) Speeds.floor
        (crush := true)))
  | 65, true =>               -- SR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.floorUp s (raiseFloorDest g' s - 8) Speeds.floor
        (crush := true)))
  -- `raiseToTexture`: vanilla raises the floor by the height of the
  -- shortest *lower texture* on the sector's two-sided lines — it walks
  -- `sec->lines`, reads `textureheight[bottomtexture]`, and takes the
  -- minimum. The sim never learns texture pixel heights (they live with
  -- the renderer's assets), so DILL substitutes vanilla's most common
  -- outcome, one 64-unit step — a deliberate approximation.
  | 30, false =>              -- W1: raise by the shortest lower texture
    oneShotW (tagged g fun g' s =>
      g'.addMover (.floorUp s (g'.level.sectors[s]!.floorH + 64) Speeds.floor))
  | 96, false =>              -- WR: the same, repeatable
    tagged g (fun g' s =>
      g'.addMover (.floorUp s (g'.level.sectors[s]!.floorH + 64) Speeds.floor))
  | 76, false =>              -- WR: door closes, waits 30 s, reopens — the
    -- repeatable twin of 16 (vanilla routes both to `close30ThenOpen`);
    -- E1M6's corridor doors are this type
    tagged g (fun g' s => g'.close30Door s)
  | 141, false =>             -- W1: crusher. Vanilla's is the *silent* one;
    -- the hush is not modelled, so it grinds at normal volume here
    oneShotW (tagged g fun g' s => g'.addCrusher s)
  | 29, true =>               -- S1: tagged door, open-wait-close
    oneShot (tagged g fun g' s => g'.addDoor s false (speed := dspeed))
  | 14, true =>               -- S1: rise 32 and change (flat only)
    oneShot (tagged g fun g' s =>
      let g' := changeNow g' s none
      g'.addMover (.floorUp s (g'.level.sectors[s]!.floorH + 32)
        Speeds.floorRaiseChange))
  | 9, true =>                -- S1: donut
    oneShot (tagged g fun g' s => g'.addDonut s)
  | 138, true =>              -- SR: lights to full
    some (lightsTo g line 255)
  | 139, true =>              -- SR: lights out
    some (lightsTo g line 35)
  -- Lights. `35` (out) was already here; these are the rest of the family.
  | 13, false =>              -- W1: lights to full
    oneShotW (some (lightsTo g line 255))
  | 81, false =>              -- WR: the same, repeatable
    some (lightsTo g line 255)
  | 12, false =>              -- W1: lights to the brightest neighbour
    oneShotW (some (lightsToBrightest g line))
  | 80, false =>              -- WR: the same, repeatable
    some (lightsToBrightest g line)
  | 79, false =>              -- WR: lights out
    some (lightsTo g line 35)
  | 104, false =>             -- W1: each sector to its dimmest neighbour
    oneShotW (some (lightsToDimmest g line))
  | 35, false => -- W1: lights out (to 35)
    oneShotW (some (lightsTo g line 35))
  | 17, false => -- W1: start the tagged sectors strobing (SLOWDARK)
    oneShotW (some (g.startTagStrobe line.tag))
  | 7, true =>   -- S1: build stairs
    oneShot (tagged g (fun g' s => g'.buildStairs s))
  | 8, false =>  -- W1: build stairs
    oneShotW (tagged g (fun g' s => g'.buildStairs s))
  -- Teleports, and the one special that cares which way you crossed.
  -- Vanilla `EV_Teleport` bails on `side == 1` — "don't teleport if hit
  -- back of line, so you can get out of teleporter". Without it every
  -- destination pad throws you straight back the moment you step off it.
  | 39, false => -- W1: teleport
    oneShotW (some (if side == 1 then g else g.teleportActor line.tag actor))
  | 97, false => -- WR: teleport
    some (if side == 1 then g else g.teleportActor line.tag actor)
  -- The exit switches make no sound here: `flipSwitch` runs right after a
  -- successful use and plays their single heavy `sfx_swtchx` clunk
  -- (vanilla's exit-switch case in `P_ChangeSwitchTexture`); a second
  -- sound from this arm was a double clunk.
  | 11, true =>  -- S1: exit switch
    some { g with exited := true }
  | 52, false => -- W1: walk-over exit
    some { g with exited := true }
  | 51, true =>  -- S1: secret exit
    some { g with exited := true, secretExit := true }
  | _, _ => none

/-- `activateLineOpt`, ignoring whether the line refused — for walk-over
triggers, where a refusal simply means nothing happens. -/
def activateLine (g : GameState) (lineIdx : Nat) (byUse : Bool)
    (side : Nat := 0) (actor : Option Nat := none) : GameState :=
  (g.activateLineOpt lineIdx byUse side actor).getD g

/-- Which side of `line` the point `(x, y)` lies on: 0 = front (to the right
of `v1→v2`), 1 = back. Vanilla `P_PointOnLineSide`. -/
private def pointOnLineSide (lvl : Level) (line : Linedef) (x y : Float) :
    Nat :=
  let p1 := lvl.vertexes[line.v1]!
  let p2 := lvl.vertexes[line.v2]!
  if (p2.x - p1.x) * (y - p1.y) - (p2.y - p1.y) * (x - p1.x) < 0 then 0 else 1

/-- Do the segments `1→2` and `3→4` properly cross? -/
private def segsCross (x1 y1 x2 y2 x3 y3 x4 y4 : Float) : Bool :=
  let d1 := (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
  let d2 := (x2 - x1) * (y4 - y1) - (y2 - y1) * (x4 - x1)
  let d3 := (x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)
  let d4 := (x4 - x3) * (y2 - y3) - (y4 - y3) * (x2 - x3)
  d1 * d2 < 0 && d3 * d4 < 0

/-- Fire specials on lines crossed by a segment of player movement
(vanilla `P_CrossSpecialLine`). -/
def crossSpecials (g : GameState) (oldX oldY : Float) : GameState := Id.run do
  let p := g.player
  if oldX == p.x && oldY == p.y then return g
  let mut g := g
  -- `linesNear` may hand back an index twice; a line must not fire its
  -- special twice for one step (as `shootSpecialLines` dedups its tracer)
  let mut seen : Array Nat := #[]
  for i in (g.level.linesNear ((oldX + p.x) / 2) ((oldY + p.y) / 2)
      (Float.abs (p.x - oldX) + Float.abs (p.y - oldY) + 1)) do
    if seen.contains i then continue
    seen := seen.push i
    let line := g.level.linedefs[i]!
    if line.special != 0 then
      let p1 := g.level.vertexes[line.v1]!
      let p2 := g.level.vertexes[line.v2]!
      -- the endpoint is the player's *current* position, re-read each
      -- activation: vanilla re-tests every spechit line against the thing
      -- as it now stands, so a teleport taken on an earlier line stops the
      -- rest of the doorway's lines from firing back at the departure pad
      if segsCross oldX oldY g.player.x g.player.y p1.x p1.y p2.x p2.y then
        -- vanilla passes the side the actor was on *before* the step
        g := g.activateLine i (byUse := false)
          (side := pointOnLineSide g.level line oldX oldY)
  return g

/-- The monster half of `crossSpecials`: fire the specials a mobj crossed
on its way from `(oldX, oldY)` to where it stands now. Vanilla runs the same
`spechit` walk for every thing that moves, filtered to the handful of lines
`monsterCanCross` lets a non-player work — which is how monsters use
teleporters. -/
def crossSpecialsMobj (g : GameState) (i : Nat) (oldX oldY : Float) :
    GameState := Id.run do
  let m := g.mobjs[i]!
  if m.removed then return g
  if oldX == m.x && oldY == m.y then return g
  let mut g := g
  -- deduped, and re-testing against the mobj's current position, exactly
  -- as `crossSpecials` above: a teleporter taken on one line must not let
  -- the pad's other lines fire on the same step
  let mut seen : Array Nat := #[]
  for li in (g.level.linesNear ((oldX + m.x) / 2) ((oldY + m.y) / 2)
      (Float.abs (m.x - oldX) + Float.abs (m.y - oldY) + 1)) do
    if seen.contains li then continue
    seen := seen.push li
    let line := g.level.linedefs[li]!
    if line.special == 0 || !monsterCanCross line.special then continue
    let p1 := g.level.vertexes[line.v1]!
    let p2 := g.level.vertexes[line.v2]!
    let mNow := g.mobjs[i]!
    if segsCross oldX oldY mNow.x mNow.y p1.x p1.y p2.x p2.y then
      g := g.activateLine li (byUse := false)
        (side := pointOnLineSide g.level line oldX oldY) (actor := some i)
  return g

/-- Specials that answer to the spacebar; walk-over-only lines must not
swallow a use press aimed at the door behind them. -/
def answersToUse (special : Nat) : Bool :=
  match special with
  | 1 | 26 | 27 | 28 | 31 | 61 | 63 | 103 | 62 | 21 | 23 | 7 | 11 | 51 => true
  -- (24/46/47 are gun-triggered and deliberately absent: `shootSpecialLines`
  -- activates them from the bullet's tracer, and listing them here would
  -- also let the spacebar fire a line that only answers to a shot)
  -- floor movers on switches: E3M1's tag-9 switch is 18, MAP01's
  -- north-west one is 102, and both were unreachable without these
  | 18 | 69 | 102 => true
  -- Doom II's blazing doors and lifts, and the one-shot locked doors
  | 117 | 118 | 32 | 33 | 34 => true
  | 111 | 112 | 114 | 115 | 122 | 123 => true
  -- key-locked switches, and the rest of the floor/door switch families
  | 99 | 133 | 134 | 135 | 136 | 137 => true
  | 20 | 68 | 101 | 64 | 15 | 66 | 67 | 140 | 60 | 45 | 70 | 71 => true
  -- the crushing-floor raises and the turbo raises (MAP29's bridge switches)
  | 55 | 65 | 131 | 132 => true
  | 50 | 113 | 42 | 116 => true
  -- ceilings, crushers, turbo stairs and the switch lights
  | 41 | 43 | 49 | 127 | 138 | 139 => true
  | 9 | 14 | 29 => true
  | _ => false
  -- INVARIANT: every special `activateLine` handles with `byUse = true`
  -- must appear above, or pressing Use will never even select the line —
  -- a silent no-op rather than a visible failure. `Tests.lean` checks it.

/-- Press a switch: flip its `SW1`/`SW2` texture and, unless it's an exit,
schedule it to pop back out after a second (vanilla `BUTTONTIME`). -/
def flipSwitch (g : GameState) (lineIdx : Nat) : GameState := Id.run do
  let line := g.level.linedefs[lineIdx]!
  let sd := line.front
  let side := g.level.sidedefs[sd]!
  -- the clunk comes from the switch itself, not the player's ear (vanilla
  -- plays it at `buttonlist->soundorg`, the line's front sector)
  let p1 := g.level.vertexes[line.v1]!
  let p2 := g.level.vertexes[line.v2]!
  let sx := (p1.x + p2.x) / 2
  let sy := (p1.y + p2.y) / 2
  -- vanilla checks top, then middle, then bottom for a switch texture
  for (slot, name) in [(0, side.upper), (1, side.middle), (2, side.lower)] do
    if let some twin := Level.switchTwin name then
      let mut g := { g with level := g.level.setSideTex sd slot twin }
      -- the exit switches clunk vanilla's heavier `sfx_swtchx`, once, from
      -- here alone; everything else gets the ordinary `sfx_swtchn`
      let sfx := if line.special == 11 || line.special == 51 then Sfx.swtchx
                 else Sfx.switchOn
      g := g.playSound sfx sx sy
      -- A switch only pops back out if it can be used again. `activateLine`
      -- ran first and cleared a one-shot line's special to 0, so a still-set
      -- special marks a repeatable switch: those rebound after a second, the
      -- rest stay flipped for good (one-shot switches and the exits).
      --
      -- One already rebounding queues nothing further — vanilla's
      -- `P_StartButton` refuses a second timer for a switch it is already
      -- holding. The texture still flips (back to its unpressed face, since
      -- the first press left the pressed one on), which is what vanilla
      -- does too; only the timer is skipped. Queueing a second entry
      -- instead records the *pressed* texture as the one to restore, and
      -- the later of the two rebounds then wins — leaving a repeatable
      -- switch stuck showing itself pressed for the rest of the map. A
      -- pair of taps on an SR light switch (138/139, which always succeeds
      -- and so always reaches here) inside one second is enough to do it.
      let rebounding := g.buttons.any fun (s, sl, _, _) => s == sd && sl == slot
      if line.special != 0 && line.special != 11
          && line.special != 51 && line.special != 52 && !rebounding then
        g := { g with buttons := g.buttons.push (sd, slot, name, 35) }
      return g
  return g

/-- Reveal, for the automap, every linedef bordering the sector the player
now stands in — so walking into a room draws its outline. -/
def markSeen (g : GameState) : GameState := Id.run do
  if g.seen.size != g.level.linedefs.size then return g  -- untracked
  let ps := g.level.sectorAt g.player.x g.player.y
  -- the sector's own line list, not a sweep of the map: this runs every tic
  let some lines := g.level.sectorLines[ps]? | return g
  let mut seen := g.seen
  for i in lines do
    seen := seen.set! i true
  return { g with seen }

/-- Tick the pressed switches; each pops back to its original texture when
its timer runs out — with a fresh `sfx_swtchn` clunk from the switch's own
sector, as vanilla's button countdown in `P_UpdateSpecials` replays it. -/
def stepButtons (g : GameState) : GameState := Id.run do
  let mut g := g
  let mut kept := #[]
  for (sd, slot, name, t) in g.buttons do
    if t ≤ 1 then
      g := { g with level := g.level.setSideTex sd slot name }
      -- vanilla's soundorg for a button is its line's front sector
      g := g.soundAtSector Sfx.switchOn g.level.sidedefs[sd]!.sector
    else
      kept := kept.push (sd, slot, name, t - 1)
  return { g with buttons := kept }

/-- Gun-triggered lines: a hitscan shot that crosses a `46` (GR open door)
or `47` (G1 raise floor) line fires it. Vanilla's `PTR_ShootTraverse` along
the bullet's tracer: the crossings are walked in distance order, every
special line reached is activated, and the walk ends at the first line the
shot cannot pass — a one-sided wall *or* a shut opening (a closed door).
`range` is the attack's own reach (a fist swing trips nothing beyond
arm's length; bullets trace vanilla's `MISSILERANGE`). `(x, y)` is the
shooter, and `player` whether that is the player: vanilla's
`P_ShootSpecialLine` lets any other shooter fire exactly one special —
46, its lone `ok = 1` case — so a zombieman's stray shot opens a
shoot-door but never trips 24 or 47. -/
def shootSpecialLines (g : GameState) (x y angle range : Float)
    (player : Bool := true) : GameState := Id.run do
  let dx := Float.cos angle
  let dy := Float.sin angle
  -- every line crossing along the tracer: distance, index, and whether the
  -- shot carries on past it. Only the lines in the blockmap cells the tracer
  -- actually crosses are considered, as every other ray walk here does
  -- (`shotCrossings`, `checkSight`, `slideHit`); sweeping the whole map cost
  -- the shot a pass over every linedef on it. `linesAlong` may hand back an
  -- index twice, so they are deduped — a line must not fire its special, or
  -- push its switch onto the rebound list, twice for one bullet.
  let mut crossings : Array (Float × Nat × Bool) := #[]
  let mut seen : Array Nat := #[]
  for i in g.level.linesAlong x y (x + dx * range) (y + dy * range) do
    if seen.contains i then continue
    seen := seen.push i
    let line := g.level.linedefs[i]!
    let some t := g.level.rayHitsLine line x y dx dy | continue
    if t ≤ 0 || t ≥ range then continue
    let passable := match line.back with
      | none => false
      | some back =>
        let f := g.level.sectors[g.level.sidedefs[line.front]!.sector]!
        let b := g.level.sectors[g.level.sidedefs[back]!.sector]!
        min f.ceilH b.ceilH - max f.floorH b.floorH > 0
    crossings := crossings.push (t, i, passable)
  let sorted := crossings.qsort (·.1 < ·.1)
  let mut g := g
  for (_, i, passable) in sorted do
    let line := g.level.linedefs[i]!
    if line.special == 46
        || (player && (line.special == 24 || line.special == 47)) then
      match g.activateLineOpt i (byUse := true) with
      | some g' => g := g'.flipSwitch i
      | none => pure ()
    if !passable then break
  return g

/-- The taunt vanilla prints for a locked line pressed without its key —
`p_doors.c`'s `PD_*` strings: the manual locked doors (26–28/32–34, via
`EV_VerticalDoor`) say "…TO OPEN THIS DOOR", the key-locked switches
(99/133–137, via `EV_DoLockedDoor`) "…TO ACTIVATE THIS OBJECT". `none`
when the special is no lock, or the key is carried — the caller consults
this only on a refusal, alongside the oof. -/
def lockedMessage (g : GameState) (special : Nat) : Option String :=
  let need := fun (has : Bool) (color deed : String) =>
    if has then none else some s!"YOU NEED A {color} KEY TO {deed}"
  match special with
  | 26 | 32 => need g.status.blueKey "BLUE" "OPEN THIS DOOR"
  | 27 | 34 => need g.status.yellowKey "YELLOW" "OPEN THIS DOOR"
  | 28 | 33 => need g.status.redKey "RED" "OPEN THIS DOOR"
  | 99 | 133 => need g.status.blueKey "BLUE" "ACTIVATE THIS OBJECT"
  | 134 | 135 => need g.status.redKey "RED" "ACTIVATE THIS OBJECT"
  | 136 | 137 => need g.status.yellowKey "YELLOW" "ACTIVATE THIS OBJECT"
  | _ => none

/-- The spacebar: trace 64 units from the eyes along the view angle and
fire the nearest *use-answering* special line — one press, one action,
and never through a wall (vanilla `P_UseLines`). A wall here is a one-sided
line *or* a two-sided one whose opening is shut (a closed door's track), as
vanilla's `PTR_UseTraverse` checks `openrange ≤ 0`. A press that reaches
only a wall, or a locked line without the key, grunts (vanilla's
`sfx_noway`/`sfx_oof`) instead of flipping the switch. -/
def useLines (g : GameState) : GameState := Id.run do
  let p := g.player
  let dx := Float.cos p.angle
  let dy := Float.sin p.angle
  let lineSectors := fun (line : Linedef) =>
    match line.back with
    | some back => #[g.level.sidedefs[line.front]!.sector,
                     g.level.sidedefs[back]!.sector]
    | none => #[g.level.sidedefs[line.front]!.sector]
  -- pass 1: the nearest use-answering line, pressed from its *front* side —
  -- vanilla `P_UseSpecialLine` refuses `if (side) return false` outright,
  -- so a switch is never worked from behind its own wall. One deliberate,
  -- narrow deviation: the manual-door family stays pressable from its back
  -- side too. Those lines operate the very sector behind them, and E1M7's
  -- first door carries its only use face on the *far* side of the slab —
  -- under the strict refusal it could never be opened from the east.
  let backsideOk := fun (special : Nat) =>
    match special with
    | 1 | 26 | 27 | 28 | 31 | 32 | 33 | 34 | 117 | 118 => true
    | _ => false
  let mut bestT := 1.0e30
  let mut bestLine : Option Nat := none
  for i in (g.level.linesNear p.x p.y 64) do
    let line := g.level.linedefs[i]!
    let some t := g.level.rayHitsLine line p.x p.y dx dy | continue
    if t ≤ 0 || t > 64 then continue
    if pointOnLineSide g.level line p.x p.y == 1 && !backsideOk line.special then
      continue
    if answersToUse line.special && t < bestT then
      bestT := t
      bestLine := some i
  -- pass 2: the nearest wall in the way. Vanilla `PTR_UseTraverse` blocks
  -- only on a *plain* line (`if (!ld->special)`) that is one-sided or whose
  -- opening is shut (`openrange ≤ 0`) — a closed door's track. Lines with a
  -- walk-over special stay transparent (see `answersToUse`), and a shut
  -- line bordering the sector the use line *operates on* — its back sector,
  -- the closed door slab itself (E1M7's far-face door) — must not block the
  -- press aimed at that door. Only the back sector is exempt: exempting the
  -- front (room) sector too made every shut line bordering the player's own
  -- room transparent, and switches fired through closed geometry all over
  -- the IWADs.
  let bestSecs : Array Nat := match bestLine with
    | some i =>
      match (g.level.linedefs[i]!).back with
      | some back => #[g.level.sidedefs[back]!.sector]
      | none => #[]
    | none => #[]
  let mut wallT := 1.0e30
  for i in (g.level.linesNear p.x p.y 64) do
    let line := g.level.linedefs[i]!
    if line.special != 0 then continue
    let some t := g.level.rayHitsLine line p.x p.y dx dy | continue
    if t ≤ 0 || t > 64 then continue
    if (lineSectors line).any (bestSecs.contains ·) then continue
    let shut := match line.back with
      | none => true
      | some back =>
        let f := g.level.sectors[g.level.sidedefs[line.front]!.sector]!
        let b := g.level.sectors[g.level.sidedefs[back]!.sector]!
        min f.ceilH b.ceilH - max f.floorH b.floorH ≤ 0
    if shut && t < wallT then wallT := t
  match bestLine with
  | some i =>
    if bestT ≥ wallT then return g.playSound Sfx.oof p.x p.y
    match g.activateLineOpt i (byUse := true) with
    | some g' => return g'.flipSwitch i
    | none =>
      -- a locked line names its missing key on the status bar (vanilla's
      -- `PD_*` messages) as well as grunting
      let g := match g.lockedMessage (g.level.linedefs[i]!).special with
        | some msg => { g with message := msg }
        | none => g
      return g.playSound Sfx.oof p.x p.y
  | none =>
    -- nothing usable, but a wall within reach still answers with the grunt
    if wallT < 1.0e30 then return g.playSound Sfx.oof p.x p.y
    return g

/-- The tallest head in sector `s`: the player (their box may straddle the
doorway, so the corners count too) and any solid mobj standing there.
`none` when the sector is empty. -/
private def occupantTop (level : Level) (g : GameState) (s : Nat) :
    Option Float := Id.run do
  let mut top : Option Float := none
  let raise := fun (t : Option Float) (v : Float) =>
    some (match t with | none => v | some t => max t v)
  -- a body of radius `r` at (x,y) overlaps the door sector if its centre or
  -- any corner of its box is inside it (bodies straddle the narrow track)
  let inSector := fun (x y r : Float) => Id.run do
    if level.sectorAt x y == s then return true
    for (dx, dy) in [(r, r), (-r, r), (r, -r), (-r, -r)] do
      if level.sectorAt (x + dx) (y + dy) == s then return true
    return false
  if !g.status.dead && inSector g.player.x g.player.y Player.radius then
    top := raise top (g.player.z + Player.height)
  -- Ask the mobj grid about the door's own patch of floor rather than
  -- walking the roster: this runs for every moving door, lift and
  -- close-30 on every tic, and `inSector` costs up to five BSP descents
  -- per body. A sector with no lines can name no place to ask about, so
  -- it falls back to the full scan — a door that misses a head is worse
  -- than a slow one.
  let bounds := level.sectorBounds s
  let candidates := match bounds with
    | none => Array.range g.mobjs.size
    | some b =>
      g.mobjsNear ((b.left + b.right) / 2) ((b.bottom + b.top) / 2)
        (max ((b.right - b.left) / 2) ((b.top - b.bottom) / 2))
  for i in candidates do
    let m := g.mobjs[i]!
    if m.removed || !m.solid then continue
    -- a box-overlap reject before paying for the descents
    if let some b := bounds then
      if m.x + m.info.radius < b.left || m.x - m.info.radius > b.right
          || m.y + m.info.radius < b.bottom || m.y - m.info.radius > b.top then
        continue
    if inSector m.x m.y m.info.radius then
      top := raise top (m.z + m.info.height)
  return top

/-- Snap whatever stands on a floor that just moved onto its new height.
Vanilla clips riders inside the plane move itself (`T_MovePlane` →
`P_ChangeSector` → `P_ThingHeightClip`), so a thing never hangs a tic
behind its floor; DILL's per-mobj glue in `tickMobjs` runs *before* the
movers step, which alone would leave every rider hovering one mover-step
above a descending platform on each rendered frame. Called right after
`stepMovers` with the sectors that were in motion. -/
def glueRiders (g : GameState) (sectors : Array Nat) : GameState := Id.run do
  let mut g := g
  for s in sectors do
    let candidates := match g.level.sectorBounds s with
      | none => Array.range g.mobjs.size
      | some b =>
        g.mobjsNear ((b.left + b.right) / 2) ((b.bottom + b.top) / 2)
          (max ((b.right - b.left) / 2) ((b.top - b.bottom) / 2))
    let floorH := g.level.sectors[s]!.floorH
    for i in candidates do
      let m := g.mobjs[i]!
      if m.removed || m.momZ != 0 then continue
      let grounded := !m.info.missile && !m.info.ceilingHang
        && !m.info.noGravity && (m.corpse || !m.info.flying)
      if !grounded then continue
      if g.level.sectorAt m.x m.y != s then continue
      if m.z != floorH && Float.abs (m.z - floorH) ≤ Speeds.maxPlane then
        g := g.setMobj i { m with z := floorH }
  return g

/-- Would raising sector `s`'s floor to `next` squeeze a body against a
ceiling? Vanilla judges fit in `P_ThingHeightClip` via `P_CheckPosition`:
a body overlapping the sector rides the rising floor, and must still fit
under the *minimum* ceiling of every sector its clipping box touches —
not the moving sector's own. The difference is exactly MAP05's lift edge:
a player straddling the platform and the low lip strip next door is
crushed against the strip's 160 ceiling long before the lift's own 288
could matter; testing only the lift's ceiling never fires, the floor
sails up, and the player is left wedged where nothing fits and no move is
legal. The floor height is compared, not the occupant's current `z` — the
riding snap stops tracking once the body no longer fits, so `z` lags. -/
private def raiseWouldSqueeze (level : Level) (g : GameState) (s : Nat)
    (next : Float) : Bool := Id.run do
  let corners := fun (r : Float) => [(r, r), (-r, r), (r, -r), (-r, -r)]
  let minCeil := fun (x y r : Float) => Id.run do
    let mut c := level.sectors[level.sectorAt x y]!.ceilH
    for (dx, dy) in corners r do
      c := min c level.sectors[level.sectorAt (x + dx) (y + dy)]!.ceilH
    return c
  -- as `occupantTop`: a body overlaps the sector if its centre or any box
  -- corner is inside it
  let inSector := fun (x y r : Float) => Id.run do
    if level.sectorAt x y == s then return true
    for (dx, dy) in corners r do
      if level.sectorAt (x + dx) (y + dy) == s then return true
    return false
  if !g.status.dead && inSector g.player.x g.player.y Player.radius then
    if next + Player.height > minCeil g.player.x g.player.y Player.radius then
      return true
  let bounds := level.sectorBounds s
  let candidates := match bounds with
    | none => Array.range g.mobjs.size
    | some b =>
      g.mobjsNear ((b.left + b.right) / 2) ((b.bottom + b.top) / 2)
        (max ((b.right - b.left) / 2) ((b.top - b.bottom) / 2))
  for i in candidates do
    let m := g.mobjs[i]!
    if m.removed || !m.solid then continue
    if let some b := bounds then
      if m.x + m.info.radius < b.left || m.x - m.info.radius > b.right
          || m.y + m.info.radius < b.bottom || m.y - m.info.radius > b.top then
        continue
    if inSector m.x m.y m.info.radius
        && next + m.info.height > minCeil m.x m.y m.info.radius then
      return true
  return false

/-- A crushing plane grinds whatever no longer fits in sector `s` —
vanilla's 10 damage every fourth tic (`P_ChangeSector` with `crunch`
set), to the player and to anything solid standing there. The squeeze is
judged by the sector's own gap (`ceilH - floorH` against the body's
height), which serves a descending crusher ceiling and a rising
`raiseFloorCrush` floor alike. -/
private def crushOccupants (g : GameState) (s : Nat) : GameState := Id.run do
  if g.tics % 4 != 0 then return g
  let mut g := g
  let sec := g.level.sectors[s]!
  let gap := sec.ceilH - sec.floorH
  -- as `occupantTop` and `raiseWouldSqueeze` judge occupancy: a body
  -- overlaps the sector if its centre or any clipping-box corner is inside
  -- it — vanilla's `P_ChangeSector` walks the blockbox, so a straddler
  -- half on the crushed sector is ground like anyone standing square in it
  let corners := fun (r : Float) => [(r, r), (-r, r), (r, -r), (-r, -r)]
  let inSector := fun (x y r : Float) => Id.run do
    if g.level.sectorAt x y == s then return true
    for (dx, dy) in corners r do
      if g.level.sectorAt (x + dx) (y + dy) == s then return true
    return false
  if !g.status.dead && gap < Player.height
      && inSector g.player.x g.player.y Player.radius then
    g := g.damagePlayer 10
  -- as `occupantTop`: the grid over the crusher's own bounds, not the roster
  let candidates := match g.level.sectorBounds s with
    | none => Array.range g.mobjs.size
    | some b =>
      g.mobjsNear ((b.left + b.right) / 2) ((b.bottom + b.top) / 2)
        (max ((b.right - b.left) / 2) ((b.top - b.bottom) / 2))
  for i in candidates do
    let m := g.mobjs[i]!
    if m.removed || !m.shootable then continue
    if gap < m.info.height && inSector m.x m.y m.info.radius then
      -- a crusher has no attacker behind it (vanilla passes a null source),
      -- so being ground does not rouse a sleeping monster
      g := g.damageMobj i 10 (wakes := false)
  return g

/-- Is a live body squeezed in sector `s` right now — precisely the bodies
`crushOccupants` would damage? Drives the crusher's contact-crawl: vanilla
`T_MoveCeiling` slows to `CEILSPEED/8` while its down-move reports
`crushed`. Judged from the current gap rather than a sticky flag, so a
crusher speeds back up the tic its victim is ground away, where vanilla
stays slow until the up stroke — a divergence noted in the audit ledger. -/
private def crushContact (level : Level) (g : GameState) (s : Nat) : Bool := Id.run do
  let sec := level.sectors[s]!
  let gap := sec.ceilH - sec.floorH
  let corners := fun (r : Float) => [(r, r), (-r, r), (r, -r), (-r, -r)]
  let inSector := fun (x y r : Float) => Id.run do
    if level.sectorAt x y == s then return true
    for (dx, dy) in corners r do
      if level.sectorAt (x + dx) (y + dy) == s then return true
    return false
  if !g.status.dead && gap < Player.height
      && inSector g.player.x g.player.y Player.radius then
    return true
  let candidates := match level.sectorBounds s with
    | none => Array.range g.mobjs.size
    | some b =>
      g.mobjsNear ((b.left + b.right) / 2) ((b.bottom + b.top) / 2)
        (max ((b.right - b.left) / 2) ((b.top - b.bottom) / 2))
  for i in candidates do
    let m := g.mobjs[i]!
    if m.removed || !m.shootable then continue
    if gap < m.info.height && inSector m.x m.y m.info.radius then
      return true
  return false

/-- Scrolling wall textures (special `48`). Not a trigger at all — the
sidedef's x offset simply creeps every tic, which the wall renderer
already reads, so the scroll costs nothing but the offset. -/
def stepScrollers (g : GameState) : GameState := Id.run do
  if g.level.scrollLines.isEmpty then return g
  let mut lvl := g.level
  -- the scrolling lines were indexed at load; the `special` re-check keeps
  -- this honest if a save ever restored a different set
  for i in g.level.scrollLines do
    let line := g.level.linedefs[i]!
    if line.special == 48 then
      let sides := lvl.sidedefs.modify line.front
        (fun sd => { sd with xOffset := sd.xOffset + 1 })
      lvl := { lvl with sidedefs := sides }
  return { g with level := lvl }

/-- Advance every active mover one tic, with its clunks and hums. A door
about to touch someone's head goes back up (and waits to try again) —
except a close-only door, which just holds and keeps pressing, and a
non-crush ceiling, which holds at the blocking height; a lift that would
press someone into the ceiling heads back down. Only the crushers — the
grinding ceilings and the `raiseFloorCrush` floors — do harm. -/
def stepMovers (g : GameState) : GameState := Id.run do
  let mut g := g
  let mut level := g.level
  let mut movers := #[]
  -- Where the movers being stepped end. `crushOccupants` below runs
  -- `damageMobj`, which can start a mover of its own — a boss ground to
  -- death drops the tag-666 floors — by pushing onto `g.movers` behind this
  -- loop's back. `movers` is rebuilt from the ones actually stepped, so
  -- without carrying the tail across, that trigger would fire and then
  -- vanish on the same tic.
  let stepping := g.movers.size
  -- Vanilla grinds `DSSTNMOV` on a `leveltime & 7` beat for every plane in
  -- motion. One grind per beat is enough here and keeps a stair builder — up
  -- to eight movers at once — from flooding all eight mixer channels with
  -- the same sample and drowning everything else out.
  let mut grinding : Option Nat := none   -- a grinding mover's sector
  for m0 in g.movers do
    -- A mover in stasis (a stopped plat or crusher) is carried unchanged:
    -- vanilla nulls its thinker function, so it neither moves nor sounds
    -- until the matching start line wakes it.
    if m0.stalled then
      movers := movers.push m0
      continue
    -- A timed door (sector specials 10/14) sits inert while its countdown
    -- runs — vanilla parks the thinker on `topcountdown`. It announces
    -- itself the tic it finally moves, as `T_VerticalDoor` does when the
    -- count expires.
    if let .door s top wait closing stay sp (delay + 1) := m0 then
      if delay == 0 then
        g := g.soundAtSector (if closing then Sfx.doorClose else Sfx.doorOpen) s
      movers := movers.push (.door s top wait closing stay sp delay)
      continue
    -- A blocked *close-only* door (`closeDoor` starts these: closing with
    -- `stay` set, since it has no return trip) does not reverse — vanilla
    -- `vld_close`, "DO NOT GO BACK UP". It holds where it is, still closing,
    -- and presses on as soon as the occupant moves. Only that combination
    -- of flags arises from `closeDoor`, so it identifies the family.
    let held : Bool := match m0 with
      | .door s _ _ true true sp _ =>
        match occupantTop level g s with
        | some occ => level.sectors[s]!.ceilH - sp < occ
        | none => false
      -- A rising floor that would squeeze a body holds at its last position
      -- and retries, as vanilla `T_MoveFloor` does for the non-crushing
      -- floor types (it restores `lastpos` and keeps the thinker) — except
      -- a `raiseFloorCrush` floor, which keeps grinding upward and deals
      -- its damage below instead.
      | .floorUp s target sp _ crush =>
        !crush
          && raiseWouldSqueeze level g s (min target (level.sectors[s]!.floorH + sp))
      -- A non-crush ceiling heading down (specials 40/41/43) holds against
      -- a body that no longer fits: vanilla `T_MovePlane` restores
      -- `lastpos` and reports `crushed`, and `T_MoveCeiling` keeps the
      -- thinker and its direction — so the ceiling presses on the moment
      -- the body leaves. A `lowerAndCrush` ceiling (44/72) keeps descending
      -- and deals its damage below instead, like `raiseFloorCrush`.
      -- Upward ceilings touch nobody.
      | .ceiling s target sp crush =>
        !crush
          && level.sectors[s]!.ceilH > target
          && (match occupantTop level g s with
              | some occ => max target (level.sectors[s]!.ceilH - sp) < occ
              | none => false)
      | _ => false
    if held then
      movers := movers.push m0
      continue
    let m := match m0 with
      | .door s top _ true stay sp _ =>
        match occupantTop level g s with
        -- how far it would drop this tic is its own speed, not the default:
        -- a blazing door covers four times as much and must start back up
        -- from four times as high
        | some occ =>
          if level.sectors[s]!.ceilH - sp < occ then
            Mover.door s top Speeds.doorWait false stay sp  -- back up
          else m0
        | none => m0
      | .closeOpen s top wait false =>
        -- about to touch a head: go back up — and, as vanilla's
        -- `close30ThenOpen` does, stay open once it gets there
        match occupantTop level g s with
        | some occ =>
          if level.sectors[s]!.ceilH - Speeds.door < occ then
            Mover.closeOpen s top wait true
          else m0
        | none => m0
      -- Vanilla `T_PlatRaise`: a rising platform that would squeeze a body
      -- is not a crusher — it reverses and heads back down, then tries again
      -- until the body steps off. The retry is paced by resetting the
      -- bottom wait to the full `plat->count = plat->wait`, so a straddled
      -- platform bobs once every three seconds, not every other tic.
      | .lift s low high _ true sp _ =>
        if raiseWouldSqueeze level g s (min high (level.sectors[s]!.floorH + sp)) then
          Mover.lift s low high Speeds.liftWait false sp  -- back down
        else m0
      -- Only a plat actually rising (`wait == 0`) can squeeze — one parked
      -- at an end moves nothing this tic. The prediction uses its own
      -- crawl, `liftPerpetual` (1/tic), not the ordinary lift's 4/tic,
      -- which reversed it up to 3 units before a body was touched. Vanilla
      -- heads straight back down (`plat->status = down`) with no pause;
      -- the full bottom wait is restored on arrival by `Mover.step`.
      | .perpetual s low high 0 true _ =>
        if raiseWouldSqueeze level g s
            (min high (level.sectors[s]!.floorH + Speeds.liftPerpetual)) then
          Mover.perpetual s low high 0 false
        else m0
      | _ => m0
    -- a crushing plane crawls at ⅛ while it is actually grinding someone
    -- (`crushContact`); only the harmful stroke — a crusher heading down, a
    -- crushing ceiling still above its target — ever slows
    let crawl := match m with
      | .crusher s _ _ true _ => crushContact level g s
      | .ceiling s target _ true =>
        level.sectors[s]!.ceilH > target && crushContact level g s
      | _ => false
    let (level', m') := m.step level crawl
    level := level'
    match m0, m with
    | .door _ _ _ true _ _ _, .door _ _ _ false _ _ _ =>
      g := g.soundAtSector Sfx.doorOpen m.sector
    | .closeOpen _ _ _ false, .closeOpen _ _ _ true =>
      g := g.soundAtSector Sfx.doorOpen m.sector
    -- a platform bounced off a body announces the reversal (vanilla plays
    -- `pstart` on every direction change)
    | .lift _ _ _ _ true _ _, .lift _ _ _ _ false _ _ =>
      g := g.soundAtSector Sfx.platStart m.sector
    | .perpetual _ _ _ _ true _, .perpetual _ _ _ _ false _ =>
      g := g.soundAtSector Sfx.platStart m.sector
    | _, _ => pure ()
    match m, m' with
    | .door _ _ _ false _ _ _, some (.door _ _ _ true _ _ _) =>
      g := g.soundAtSector Sfx.doorClose m.sector
    | .closeOpen _ _ _ false, some (.closeOpen _ _ _ true) =>
      -- the 30 seconds are up: the door heads back open
      g := g.soundAtSector Sfx.doorOpen m.sector
    | .lift _ _ _ _ false _ _, some (.lift _ _ _ _ true _ _) =>
      g := g.soundAtSector Sfx.platStart m.sector
    | .lift .., none =>
      g := g.soundAtSector Sfx.platStop m.sector
    -- a perpetual plat's routine turnarounds sound too: `T_PlatRaise`
    -- thumps `pstop` as it parks at either end (wait 0 → full wait), and
    -- clunks `pstart` the tic its wait lapses and it sets off again
    | .perpetual _ _ _ 0 _ _, some (.perpetual _ _ _ (_+1) _ _) =>
      g := g.soundAtSector Sfx.platStop m.sector
    | .perpetual _ _ _ 1 _ _, some (.perpetual _ _ _ 0 _ _) =>
      g := g.soundAtSector Sfx.platStart m.sector
    -- a floor or ceiling that has arrived thumps, as `T_MoveFloor` does
    | .floorUp .., none | .floorDown .., none | .ceiling .., none =>
      g := g.soundAtSector Sfx.platStop m.sector
    | _, _ => pure ()
    -- and grinds while it is still travelling
    match m' with
    | some (.floorUp ..) | some (.floorDown ..) | some (.ceiling ..)
    | some (.crusher ..) => grinding := some m.sector
    | _ => pure ()
    -- a crusher damages whatever is caught under it as it grinds down, and
    -- a crushing floor (`raiseFloorCrush`) whatever it squeezes as it rises
    if let .crusher cs .. := m then
      g := { g with level }        -- crushOccupants reads current heights
      g := g.crushOccupants cs
      level := g.level
    if let .floorUp fs _ _ _ true := m then
      g := { g with level }
      g := g.crushOccupants fs
      level := g.level
    -- and a crushing ceiling (`lowerAndCrush`, 44/72) likewise as it descends
    if let .ceiling cs _ _ true := m then
      g := { g with level }
      g := g.crushOccupants cs
      level := g.level
    if let some m' := m' then movers := movers.push m'
  -- anything a crush started this tic (see `stepping`) joins the survivors
  movers := movers ++ g.movers.extract stepping g.movers.size
  if let some gs := grinding then
    if g.tics % 8 == 0 then
      g := g.soundAtSector Sfx.stoneMove gs
  return { g with level, movers }

end GameState
end Dill
