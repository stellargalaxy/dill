import Std.Data.HashMap
import Dill.Level
import Dill.Shell
import Dill.Game.Random
import Dill.Game.Mobj

/-!
# Game state

Everything that changes as the game runs, advanced by the pure `tick`
function at 35 Hz. The `Level` lives *inside* the state because doors and
lifts change sector heights.

Doom's movement constants translate from fixed-point per-tic values; the
originals are noted alongside.
-/

namespace Dill

/-- Fold an angle into (−π, π]. Doom's BAM angles wrap by construction;
DILL's `Float` angles have to be folded by hand wherever they are compared
or accumulated, or sustained turning grows them without bound and erodes
trig precision. One definition, so every fold uses the same 2π. -/
def wrapAngle (x : Float) : Float :=
  x - (x / 6.28318530718).round * 6.28318530718

/-- The player: feet position, velocity, and facing. The camera sits
`viewHeight` above the feet. -/
structure Player where
  x     : Float
  y     : Float
  z     : Float
  momX  : Float := 0
  momY  : Float := 0
  momZ  : Float := 0
  angle : Float
  /-- Vanilla `player->viewheight`: normally `Player.viewHeight`, but a hard
  landing squats it and it springs back over the next moment. -/
  eyeHeight : Float := 41
  /-- Vanilla `player->deltaviewheight`, the spring driving `eyeHeight`. -/
  eyeDelta  : Float := 0
  deriving Repr, Inhabited

namespace Player

/-- Collision radius (vanilla 16). -/
def radius : Float := 16
/-- Body height (vanilla 56): the opening a player needs. -/
def height : Float := 56
/-- Eyes above feet (vanilla 41). -/
def viewHeight : Float := 41
/-- Highest step climbable without jumping (vanilla 24). -/
def maxStep : Float := 24

/-- Walk/run thrust per tic (vanilla `forwardmove` 25, 50 × 2048 fixed). -/
def thrustWalk : Float := 25.0 / 32.0
def thrustRun  : Float := 50.0 / 32.0
/-- Strafe thrust per tic (vanilla `sidemove` 24, 40 × 2048 fixed). -/
def strafeWalk : Float := 24.0 / 32.0
def strafeRun  : Float := 40.0 / 32.0
/-- Keyboard turn per tic in radians (vanilla `angleturn` 640, 1280 BAM<<16). -/
def turnWalk : Float := 640.0 / 65536.0 * 2.0 * 3.14159265358979
def turnRun  : Float := 1280.0 / 65536.0 * 2.0 * 3.14159265358979
/-- Ground drag per tic (vanilla `FRICTION` 0xE800/0x10000). -/
def friction : Float := 0.90625
/-- Below this speed momentum snaps to zero (vanilla `STOPSPEED`). -/
def stopSpeed : Float := 0.0625
/-- The most a thing may travel in one tic (vanilla `MAXMOVE`). -/
def maxMove : Float := 30
/-- Gravity per tic (vanilla `GRAVITY` 1.0). -/
def gravity : Float := 1.0
/-- The most the walk bob can swing the eye (vanilla `MAXBOB`). -/
def maxBob : Float := 16.0

/-- Where a shot leaves the player: vanilla's `shootz`,
`z + (height >> 1) + 8` — 36 above the feet for a 56-tall marine, a little
below the 41 the camera sits at. Every player hitscan and missile aim
traces from here. -/
def shootZ (p : Player) : Float := p.z + height / 2 + 8

/-- A swing's reach (vanilla `MELEERANGE`). `A_Saw` reaches one further —
vanilla's `+1` is really one fixed-point epsilon, kept as a whole unit
here. -/
def meleeRange : Float := 64
/-- A hitscan's reach (vanilla `MISSILERANGE`). -/
def missileRange : Float := 2048
/-- The weapon sprite's travel per tic while lowering or rising (vanilla
`LOWERSPEED`/`RAISESPEED`). -/
def weaponShift : Float := 6
/-- Weapon sprite fully up and ready (vanilla `WEAPONTOP`). -/
def weaponTop : Float := 32
/-- Weapon sprite fully lowered offscreen (vanilla `WEAPONBOTTOM`). -/
def weaponBottom : Float := 128

end Player

/-! Vanilla speeds (units/tic) and waits (tics), named after the constants
they come from: `VDOORSPEED`, `PLATSPEED`, `FLOORSPEED`, `CEILSPEED`. -/
namespace Speeds
def door : Float := 2
def doorWait : Nat := 150
/-- `VDOORSPEED * 4`: the *blazing* door family (Doom II's specials 105–118
and the blazing locked doors), which slams at four times the ordinary rate.
Doom II leans on these heavily, and running them at `door` speed makes half
its doors feel sluggish. -/
def doorBlaze : Float := 8
/-- `PLATSPEED * 4`, the down-wait-up-stay lift every map uses. -/
def lift : Float := 4
/-- `PLATSPEED` alone: the perpetual up-down platform (special 53) crawls at
a quarter of the ordinary lift's pace in vanilla (`EV_DoPlat`,
`perpetualRaise`). -/
def liftPerpetual : Float := 1
/-- `PLATSPEED * 8`: the blazing lift (specials 120–123). Besides looking
wrong at half speed, a slower lift stays busy longer — and a busy lift
silently refuses to be re-triggered, so the sluggishness is felt as
unresponsiveness too. -/
def liftBlaze : Float := 8
def liftWait : Nat := 105
/-- `FLOORSPEED`. An ordinary floor crawls: a 64-unit move is nearly two
seconds, not the half-second it takes at the lift's pace. -/
def floor : Float := 1
/-- `FLOORSPEED * 4`, for the specials vanilla marks turbo. -/
def floorTurbo : Float := 4
/-- `CEILSPEED`, for moving ceilings and for crushers. -/
def ceiling : Float := 1
def stair : Float := 0.25
/-- `EV_BuildStairs`' `turbo16` rate, `FLOORSPEED*4`: specials 100/127 build
16-unit steps at sixteen times the ordinary stair crawl. -/
def stairTurbo : Float := 4
/-- `PLATSPEED / 2`: the raise-and-change plat family creeps at half the
perpetual platform's crawl. -/
def floorRaiseChange : Float := 0.5
/-- `SKULLSPEED`: the lost soul's charge (its walking `speed` is 8). -/
def skullCharge : Float := 20
/-- `FLOATSPEED`: a floater's vertical drift per tic — homing toward its
enemy's level, and ducking toward an opening on a height-blocked move. -/
def floatDrift : Float := 4
/-- The fastest any plane travels in a tic — the blazing lift's 8. Anything
resting on a plane that moved is snapped to it rather than left hanging, and
this is how far it may have to reach: at 4, a monster on a blazing lift
un-glued on the first tic of the descent and free-fell behind it, floating
in the opening while the platform ran away below. -/
def maxPlane : Float := 8
end Speeds

/-- A sector surface in motion (door, lift, or falling floor); advanced one
tic at a time by `Mover.step` in `Dill.Game.Specials`. -/
inductive Mover where
  /-- A door's ceiling: rising to `top`, waiting `wait` tics, then closing
  (unless `stay`). `speed` is `Speeds.door`, or `Speeds.doorBlaze` for the
  blazing family; it travels with the mover so a save reloads at the same
  pace the door was opened at. `delay` holds the door inert for that many
  tics before its first move — vanilla's `topcountdown`, which the timed
  sector specials 10 (close in 30 s) and 14 (raise in 5 min) park a door
  thinker on at level load. -/
  | door (sector : Nat) (top : Float) (wait : Nat) (closing stay : Bool)
      (speed : Float := Speeds.door) (delay : Nat := 0)
  /-- A lift's floor: sinking to `low`, waiting, returning to `high`.
  `speed` as for `door`: `Speeds.lift`, or `Speeds.liftBlaze` when blazing.
  `stalled` is vanilla's *stasis* (`EV_StopPlat`): the thinker keeps all its
  state but does nothing until a perpetual-platform line on the tag wakes
  it (`P_ActivateInStasis`). -/
  | lift (sector : Nat) (low high : Float) (wait : Nat) (rising : Bool)
      (speed : Float := Speeds.lift) (stalled : Bool := false)
  /-- A floor gliding down to `target` at `speed`, one way. `changeTo` is
  vanilla's deferred "and change": the flat and sector special to adopt once
  the floor arrives, taken from whatever it comes to rest beside. -/
  | floorDown (sector : Nat) (target : Float) (speed : Float)
      (changeTo : Option (String × Nat) := none)
  /-- A floor rising to `target` at `speed`. Stair steps creep up at
  `Speeds.stair`; the "raise floor" specials move at `Speeds.floor`, and
  inheriting the stair speed makes them look broken rather than slow.
  `changeTo` is as above — the donut's ring uses it. `crush` marks
  vanilla's `raiseFloorCrush` (specials 55/56/65/94): instead of holding
  against a body that no longer fits, the floor keeps rising and grinds
  it, like a crusher. -/
  | floorUp (sector : Nat) (target : Float) (speed : Float)
      (changeTo : Option (String × Nat) := none) (crush : Bool := false)
  /-- A ceiling gliding to `target` at `speed`, one way. `crush` marks
  vanilla's `lowerAndCrush` (specials 44/72): instead of holding against a
  body that no longer fits, the ceiling keeps descending and grinds it,
  where the plain lowers (40/41/43) hold. -/
  | ceiling (sector : Nat) (target : Float) (speed : Float)
      (crush : Bool := false)
  /-- Vanilla `close30ThenOpen` (special 16): the door closes, sits shut for
  `wait` tics (30 seconds), then reopens to `top` and stays open. -/
  | closeOpen (sector : Nat) (top : Float) (wait : Nat) (reopening : Bool)
  /-- A crusher: the ceiling grinds down to `low`, back up to `top`, and
  round again forever. `stalled` is stasis, as for `lift`: a "stop crusher"
  line (`EV_CeilingCrushStop`) parks it in place, and a crusher-starting
  line on the tag resumes it mid-stroke (`P_ActivateInStasisCeiling`). -/
  | crusher (sector : Nat) (top : Float) (low : Float) (down : Bool)
      (stalled : Bool := false)
  /-- A lift that never settles: down, wait, up, wait, repeating. `stalled`
  as for `lift`. -/
  | perpetual (sector : Nat) (low : Float) (high : Float) (wait : Nat)
      (rising : Bool) (stalled : Bool := false)
  -- `BEq` is what lets `activateLineOpt` tell a real activation from a
  -- no-op: a trigger that started, stopped, or reversed something leaves a
  -- different mover list, one that found every tagged sector busy leaves an
  -- identical one. See `started` in `Dill.Game.Specials`.
  deriving Repr, Inhabited, BEq

def Mover.sector : Mover → Nat
  | .door s .. => s
  | .lift s .. => s
  | .floorDown s .. => s
  | .floorUp s .. => s
  | .ceiling s .. => s
  | .closeOpen s .. => s
  | .crusher s .. => s
  | .perpetual s .. => s

/-- Is this one of vanilla's `ceiling_t` thinkers — the ones a "stop crusher"
line (specials 57/74) reaches through `activeceilings`? Doors are `vldoor_t`
and are deliberately not among them. -/
def Mover.isCeiling : Mover → Bool
  | .ceiling .. | .crusher .. => true
  | _ => false

/-- Is this one of vanilla's `plat_t` thinkers — what a "stop lift" line
(specials 54/89) reaches through `activeplats`? -/
def Mover.isPlat : Mover → Bool
  | .lift .. | .perpetual .. => true
  | _ => false

/-- Is this mover parked in stasis? `stepMovers` carries a stalled mover
unchanged; only the matching start special wakes it. -/
def Mover.stalled : Mover → Bool
  | .lift _ _ _ _ _ _ st | .perpetual _ _ _ _ _ st | .crusher _ _ _ _ st => st
  | _ => false

/-- This mover put into stasis, state intact — vanilla's `EV_StopPlat` /
`EV_CeilingCrushStop`, which null the thinker function but keep the struct
so reactivation resumes with the original bounds. `none` for the kinds
vanilla removes outright instead (a plain `.ceiling` has no stasis form:
its stop line just drops it). -/
def Mover.stall : Mover → Option Mover
  | .lift s lo hi w r sp _ => some (.lift s lo hi w r sp true)
  | .perpetual s lo hi w r _ => some (.perpetual s lo hi w r true)
  | .crusher s t lo d _ => some (.crusher s t lo d true)
  | _ => none

/-- This mover woken from stasis, everything else preserved — vanilla's
`P_ActivateInStasis` / `P_ActivateInStasisCeiling` restoring the old
status and thinker function. -/
def Mover.unstall : Mover → Mover
  | .lift s lo hi w r sp _ => .lift s lo hi w r sp false
  | .perpetual s lo hi w r _ => .perpetual s lo hi w r false
  | .crusher s t lo d _ => .crusher s t lo d false
  | m => m

/-- Spatial index over `GameState.mobjs`: the thing-side counterpart of the
WAD's linedef `Blockmap`, standing in for vanilla's `blocklinks` and
`P_BlockThingsIterator`. Without it every "is anything near here" question
walks all of `mobjs`, which makes each monster's move cost the whole roster
and the tic cost the square of it.

Cells hold *array indices* into `mobjs` — stable within a tic, since a
removal only sets `removed` — and the grid borrows the level blockmap's
origin and 128-unit cell size so the two indexes line up.

Derived data, never saved: `spawn` inserts, `setMobj` moves, and
`rebuildIndexes` rebuilds it whenever `mobjs` is compacted. A grid that was
never built costs only speed — `mobjsNear` then offers every mobj, which is
conservative in the safe direction.

A grid left *stale* is a different matter, and the one invariant to keep:
every index it holds must still name a live slot in `mobjs`. Shrink `mobjs`
behind it and the next query hands back an index past the end, where
`mobjs[i]!` answers with a default mobj — a phantom body at the origin that
collision tests then believe in. So `mobjs` may only be touched by `spawn`,
`setMobj`, or a compaction followed by `rebuildIndexes`; anything else that
replaces the array must rebuild. `mobjGridTests` holds this down. -/
structure MobjGrid where
  originX : Float
  originY : Float
  cols    : Nat
  rows    : Nat
  /-- Row-major: mobj indices in cell `(col, row)` at `row * cols + col`.
  Each mobj sits in exactly one cell, so a query never repeats an index. -/
  cells   : Array (Array Nat)
  /-- The largest `info.radius` indexed so far. A cell holds a mobj by its
  *centre*, so a query has to reach this far past its own radius to be sure
  of catching a wide body whose edge pokes in. Only ever grows, which is the
  conservative direction. -/
  maxRadius : Float
  deriving Inhabited

namespace MobjGrid

/-- An empty grid over `lvl`'s blockmap geometry. A degenerate blockmap
collapses to a single cell, which makes every query return everything —
slow, but still correct, and it keeps the empty case out of the query. -/
def empty (lvl : Level) : MobjGrid :=
  let cols := max 1 lvl.blockmap.cols
  let rows := max 1 lvl.blockmap.rows
  { originX := lvl.blockmap.originX, originY := lvl.blockmap.originY
    cols, rows, cells := Array.replicate (cols * rows) #[], maxRadius := 0 }

/-- Clamp a cell coordinate into the grid. Points outside the blockmap —
a noclipping player, a mobj at the map edge — land in the nearest edge cell
rather than dropping out of the index. Clamping is monotone, which is the
whole soundness argument: if a query's box would have contained a mobj's
true cell, it still contains the clamped one. -/
@[inline] private def clampCell (v origin : Float) (n : Nat) : Nat :=
  min (n - 1) (max 0 (ifloor ((v - origin) / 128.0))).toNat

/-- The cell a world point falls in. -/
@[inline] def cellOf (gr : MobjGrid) (x y : Float) : Nat :=
  clampCell y gr.originY gr.rows * gr.cols + clampCell x gr.originX gr.cols

/-- Index mobj `i`, sitting at `(x, y)` with radius `r`. -/
def insert (gr : MobjGrid) (i : Nat) (x y r : Float) : MobjGrid :=
  if gr.cells.isEmpty then gr
  else { gr with cells := gr.cells.modify (gr.cellOf x y) (·.push i)
                 maxRadius := max gr.maxRadius r }

/-- Move mobj `i` from `(ox, oy)` to `(nx, ny)`. A step that stays inside one
cell touches nothing, which is the overwhelmingly common case: a cell is 128
units across and the fastest thing in the game covers 40 in a tic. -/
def move (gr : MobjGrid) (i : Nat) (ox oy nx ny : Float) : MobjGrid :=
  if gr.cells.isEmpty then gr else
  let src := gr.cellOf ox oy
  let dst := gr.cellOf nx ny
  if src == dst then gr
  else { gr with cells :=
    (gr.cells.modify src (·.filter (· != i))).modify dst (·.push i) }

/-- Every mobj index in the cells a query of `radius` around `(x, y)` could
touch. Conservative: the caller re-tests exactly.

Assumes a grid with cells — go through `GameState.mobjsNear`, which is what
decides between this and the full-roster fallback. Guarding here instead
would have to answer "nothing is near", which is the one wrong answer a
spatial index must never give. -/
def near (gr : MobjGrid) (x y radius : Float) : Array Nat := Id.run do
  -- reach past our own radius by the widest body indexed, since cells hold
  -- centres and a Spider Mastermind's edge is 128 units from its own
  let r := radius + gr.maxRadius
  let x1 := clampCell (x - r) gr.originX gr.cols
  let x2 := clampCell (x + r) gr.originX gr.cols
  let y1 := clampCell (y - r) gr.originY gr.rows
  let y2 := clampCell (y + r) gr.originY gr.rows
  let mut out := #[]
  for cy in [y1 : y2 + 1] do
    for cx in [x1 : x2 + 1] do
      out := out ++ gr.cells[cy * gr.cols + cx]!
  return out

end MobjGrid

/-- One animated sector light (see `Dill.Game.Lights`). -/
inductive LightFx where
  /-- Special 1: mostly-on light with random dropouts. -/
  | blink (sector minL maxL count : Nat)
  /-- Specials 2/3/12/13: hard on/off strobe (bright 5 tics). -/
  | strobe (sector minL maxL darkTime count : Nat)
  /-- Special 8: light glides up and down. -/
  | glow (sector minL maxL : Nat) (up : Bool)
  /-- Special 17: firelight jitter. -/
  | flicker (sector minL maxL count : Nat)
  deriving Repr, Inhabited

/-- Doom's nine weapons, in number-key order — the two melee arms share
slot 1 and the two shotguns share slot 3, as vanilla pairs them. -/
inductive Weapon where
  | fist | chainsaw | pistol | shotgun | superShotgun | chaingun
  | rocket | plasma | bfg
  deriving Repr, DecidableEq, Inhabited

/-- The four ammo pools. -/
inductive Ammo where
  | bullets | shells | rockets | cells
  deriving Repr, DecidableEq, Inhabited

/-- Everything about the player that isn't position: vitals, arsenal, and
the weapon sprite's place in its state machine. -/
structure PlayerStatus where
  health   : Int := 100
  armor    : Int := 0
  /-- Which jacket those points belong to: 0 none, 1 green (soaks a third of
  each blow), 2 blue (soaks half). Vanilla `player->armortype`; the type is
  what decides absorption, and it is cleared when the points run out. -/
  armorType : Nat := 0
  bullets  : Nat := 50
  shells   : Nat := 0
  rockets  : Nat := 0
  cells    : Nat := 0
  ownsSuperShotgun : Bool := false
  ownsShotgun  : Bool := false
  ownsChaingun : Bool := false
  ownsChainsaw : Bool := false
  ownsRocket   : Bool := false
  ownsPlasma   : Bool := false
  ownsBfg      : Bool := false
  /-- Backpack doubles every ammo capacity. -/
  backpack     : Bool := false
  blueKey      : Bool := false
  yellowKey    : Bool := false
  redKey       : Bool := false
  weapon   : Weapon := .pistol
  /-- Weapon the player is switching to: the current one lowers, then this
  becomes `weapon` and rises. `none` = no switch in progress. -/
  pending  : Option Weapon := none
  /-- Vertical position of the weapon sprite while switching: 32 = up and
  ready, 128 = fully lowered offscreen (vanilla `WEAPONTOP`/`BOTTOM`). -/
  weaponY  : Float := Player.weaponTop
  /-- `none` = weapon at ready; otherwise index into the attack sequence. -/
  attack   : Option Nat := none
  /-- Tics left in the current weapon-sprite state. -/
  psprTics : Nat := 0
  /-- Attack button held through a full sequence (widens bullet spread). -/
  refiring : Bool := false
  /-- `dilldqd` and `dillclip`. -/
  god    : Bool := false
  noclip : Bool := false
  /-- Powerup timers (tics remaining) and flags. -/
  invulnTics  : Nat := 0
  invisTics   : Nat := 0
  radsuitTics : Nat := 0
  gogglesTics : Nat := 0
  berserk     : Bool := false
  /-- Tics since the berserk pack was taken; drives its fading red wash. -/
  berserkTics : Nat := 0
  ownsMap     : Bool := false
  /-- Red pain flash and gold pickup flash countdowns. -/
  damageCount : Nat := 0
  bonusCount  : Nat := 0
  dead     : Bool := false
  deriving Repr, Inhabited

/-- The whole simulation. -/
structure GameState where
  level   : Level
  player  : Player
  status  : PlayerStatus := {}
  mobjs   : Array Mobj := #[]
  nextUid : Nat := 1
  /-- `uid → mobjs` index, so uid lookups (a monster's target, a missile's
  shooter) don't rescan the whole array. Derived data, never saved:
  extended by `spawn` and rebuilt whenever `mobjs` is compacted (the end of
  `tickMobjs`, and on load). Within a tic indices are stable — removals only
  set `removed` — so the map stays exact between rebuilds. -/
  uidIndex : Std.HashMap Nat Nat := {}
  /-- Spatial index over `mobjs` for "what is near here" queries. Derived
  data with the same lifecycle as `uidIndex`, and likewise never saved; see
  `MobjGrid`. Left empty, every query simply falls back to a full scan. -/
  mobjGrid : MobjGrid := default
  rng     : Rng := {}
  /-- Per-sector "heard gunfire" flags — vanilla's lingering `soundtarget`.
  Each player shot floods the alert out from their sector through open
  two-sided lines (crossing at most one sound-blocking line) and marks every
  sector reached; a non-ambush monster in a marked sector wakes. Accumulated,
  never cleared. Empty until the first shot builds it (size = sector count). -/
  alerted : Array Bool := #[]
  /-- Set the tic the player fires a hitscan weapon, so `tick` can check
  the shot against gun-triggered lines (specials 46/47). Transient. -/
  firedShot : Bool := false
  /-- How far this tic's hitscan travels, in map units: the gun-line check
  must not reach past the attack itself — a fist swing (64) can only trip a
  shoot-switch at arm's length, while a bullet traces vanilla's
  `MISSILERANGE` (2048). Transient, meaningful only while `firedShot`. -/
  firedRange : Float := 2048
  /-- Monster hitscans fired this tic, as (x, y, angle, range) — `tick`
  runs each through the gun-line check like the player's `firedShot`,
  though a non-player shooter can only fire special 46 (vanilla
  `P_ShootSpecialLine`'s lone `ok = 1` case). Transient, like `firedShot`. -/
  monsterShots : Array (Float × Float × Float × Float) := #[]
  /-- Tics the player stays put after arriving through a teleporter —
  vanilla sets the player's `reactiontime` to 18 ("don't move for a bit")
  and `P_PlayerThink` skips `P_MovePlayer` while it runs down. Use still
  works, as it does in vanilla. What stops a two-way pair ping-ponging is
  not this but the back-side check in `activateLine`. -/
  teleFreeze : Nat := 0
  movers  : Array Mover := #[]
  lights  : Array LightFx := #[]
  /-- Pressed switches waiting to pop back out: (sidedef, texture slot
  0=upper/1=middle/2=lower, the SW1/SW2 name to restore, tics left). -/
  buttons : Array (Nat × Nat × String × Nat) := #[]
  /-- Which linedefs the player has walked past, for the automap. Empty
  means "not yet tracked" (e.g. a freshly loaded save), treated as revealed. -/
  seen    : Array Bool := #[]
  /-- Sound events this tic: (sfx index, world x, world y). The shell
  drains and plays them with distance attenuation. -/
  sounds  : Array (Nat × Float × Float) := #[]
  /-- Something for the HUD to say — vanilla's `player->message`. The
  simulation writes it, `Ui.step` drains it onto the status bar, and the
  last writer in a tic wins, as in vanilla. Transient, so it is never
  saved. Empty means "nothing to report". -/
  message : String := ""
  /-- The exit was triggered; `secretExit` picks the ExM9 detour. -/
  exited  : Bool := false
  secretExit : Bool := false
  /-- Intermission tallies: monsters felled, items scooped, secrets found. -/
  kills       : Nat := 0
  killTotal   : Nat := 0
  items       : Nat := 0
  itemTotal   : Nat := 0
  secrets     : Nat := 0
  secretTotal : Nat := 0
  /-- Status-bar face animation: current idle look direction (0/1/2) and
  tics until it re-rolls, driven by its own RNG so the cosmetic wobble
  never perturbs the gameplay stream (vanilla keeps these on separate
  random indices). Not saved. -/
  faceLook    : Nat := 0
  faceTics    : Nat := 0
  faceRng     : Rng := { seed := 0x2f6e2b1 }
  /-- Spacebar state last tic, so "use" fires once per press. -/
  useHeld : Bool := false
  /-- Chosen difficulty 1–5 (ITYTD…Nightmare), carried between maps so a new
  level spawns the right roster of things. -/
  skill   : Nat := 4
  tics    : Nat := 0

namespace GameState

/-- Fresh-spawn a mobj into the world. -/
def spawn (g : GameState) (kind : ActorKind) (x y angle : Float) :
    GameState × Nat :=
  -- Nightmare runs the fast actor tables (vanilla `G_InitNew`)
  let m := spawnMobj g.nextUid kind g.level x y angle (fast := g.skill == 5)
  let i := g.mobjs.size
  ({ g with mobjs := g.mobjs.push m, nextUid := g.nextUid + 1
            uidIndex := g.uidIndex.insert m.uid i
            mobjGrid := g.mobjGrid.insert i m.x m.y m.info.radius },
   i)

/-- Rebuild both derived indexes over `mobjs` from scratch. Called wherever
array indices stop meaning what they meant: after a compaction, and on load.
Both indexes have exactly this lifecycle, so they are rebuilt together. -/
def rebuildIndexes (g : GameState) : GameState := Id.run do
  let mut idx : Std.HashMap Nat Nat := {}
  let mut grid := MobjGrid.empty g.level
  for i in [0:g.mobjs.size] do
    let m := g.mobjs[i]!
    idx := idx.insert m.uid i
    grid := grid.insert i m.x m.y m.info.radius
  return { g with uidIndex := idx, mobjGrid := grid }

/-- Mobj indices that could lie within `radius` of `(x, y)`: the grid's
candidates, or — when no grid has been built — every mobj. Conservative
either way, so the caller must still test exactly; never repeats an index. -/
def mobjsNear (g : GameState) (x y radius : Float) : Array Nat :=
  if g.mobjGrid.cells.isEmpty then Array.range g.mobjs.size
  else g.mobjGrid.near x y radius

/-- The array index of the mobj with this uid, `none` if it is gone (or
`uid` is 0, the player). Point lookups only, so it is deterministic. -/
def mobjIdx? (g : GameState) (uid : Nat) : Option Nat :=
  match g.uidIndex[uid]? with
  | some i =>
    -- guard against a stale entry (should not happen; cheap insurance)
    if i < g.mobjs.size && g.mobjs[i]!.uid == uid then some i else none
  | none => none

/-- Make a sound at a place. `sfx` indexes `Sfx.lumps`. -/
def playSound (g : GameState) (sfx : Nat) (x y : Float) : GameState :=
  { g with sounds := g.sounds.push (sfx, x, y) }

/-- Start a map: player at the start thing, every single-player thing for the
chosen `skill` spawned as a mobj; `carry` keeps a previous map's vitals and
arsenal. Skill picks the thing-flag bit: 1–2 easy, 3 medium, 4–5 hard. -/
def start (level : Level) (carry : Option PlayerStatus := none)
    (skill : Nat := 4) : GameState := Id.run do
  let player := match level.playerStart with
    | some t =>
      let sec := level.sectors[level.sectorAt t.x t.y]!
      { x := t.x, y := t.y, z := sec.floorH
        angle := Float.ofInt t.angle * 3.14159265358979 / 180 : Player }
    | none => { x := 0, y := 0, z := 0, angle := 0 }
  let status : PlayerStatus := match carry with
    -- Vitals and arsenal cross the exit; everything else vanilla's
    -- `G_PlayerFinishLevel` strips — the keycards, every power (berserk's
    -- whole-level fist included), the automap, and the palette counters —
    -- starts over on the new map.
    | some st => { st with attack := none, pending := none, psprTics := 0
                           weaponY := 32, refiring := false, damageCount := 0
                           bonusCount := 0, dead := false
                           blueKey := false, yellowKey := false, redKey := false
                           invulnTics := 0, invisTics := 0, radsuitTics := 0
                           gogglesTics := 0, berserk := false, berserkTics := 0
                           ownsMap := false }
    | none => {}
  let mut g : GameState := { level, player, status, skill
                             mobjGrid := MobjGrid.empty level
                             seen := Array.replicate level.linedefs.size false }
  -- thing-flag bit for this skill: bit0 easy (1–2), bit1 medium (3), bit2 hard
  let skillBit : UInt16 := if skill ≤ 2 then 1 else if skill == 3 then 2 else 4
  for t in level.things do
    if t.flags &&& skillBit == 0 || t.flags &&& 16 != 0 then continue
    let some kind := ActorKind.ofThingType t.type | continue
    let angle := Float.ofInt t.angle * 3.14159265358979 / 180
    let (g', i) := g.spawn kind t.x t.y angle
    g := g'
    -- remember the map spot so a countable monster can reincarnate here on
    -- Nightmare (vanilla `mobj->spawnpoint`)
    g := { g with mobjs := g.mobjs.modify i fun m =>
      { m with spawnX := t.x, spawnY := t.y, spawnAngle := angle
               canRespawn := m.info.countKill } }
    if t.flags &&& 8 != 0 then
      g := { g with mobjs := g.mobjs.modify i ({ · with ambush := true }) }
  return { g with
    killTotal := (g.mobjs.filter (·.info.countKill)).size
    -- vanilla MF_COUNTITEM: only the artifacts figure in the item tally
    itemTotal := (g.mobjs.filter (·.kind.countItem)).size
    secretTotal := (level.sectors.filter (·.special == 9)).size }

end GameState
end Dill
