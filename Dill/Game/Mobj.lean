import Dill.Level
import Dill.Game.Info

/-!
# Map objects

A mobj is a live thing in the world: a monster, a pickup, a barrel, a
fireball, a puff of smoke. Each carries its `ActorInfo` (shared, not
copied — Lean structures are persistent), its position and momentum, and
where it is in its state table.

Mobjs live in `GameState.mobjs` and are addressed by array index within a
tic; across tics they are identified by `uid` (the array is compacted when
things are removed).
-/

namespace Dill

structure Mobj where
  uid    : Nat
  kind   : ActorKind
  info   : ActorInfo
  x      : Float
  y      : Float
  z      : Float
  angle  : Float
  momX   : Float := 0
  momY   : Float := 0
  momZ   : Float := 0
  health : Int
  /-- Index into `info.states`. -/
  state  : Nat
  /-- Tics left in this state; negative = forever. -/
  tics   : Int
  /-- AI: current move direction (0–7, 45° steps) and tics before rechoosing. -/
  moveDir   : Nat := 0
  moveCount : Nat := 0
  /-- Chasing the player (woken up). -/
  awake  : Bool := false
  /-- Deaf ambusher: wakes on sight only, not on noise. -/
  ambush : Bool := false
  /-- A *missile* attack just happened (vanilla `MF_JUSTATTACKED`, set only
  by the ranged branch of `A_Chase`); forces a new move before attacking
  again — except on Nightmare, where clearing it costs the `A_Chase` call
  but skips the repositioning. -/
  justAttacked : Bool := false
  /-- What this monster hunts: a mobj `uid`, or 0 for the player.
  Set to an attacker's uid when hurt by another monster (infighting). -/
  target : Nat := 0
  /-- Vanilla `reactiontime`: counts down in `A_Chase`; while positive the
  monster holds its ranged fire. Set from `info` on spawn, zeroed when hurt. -/
  reactionTime : Nat := 0
  /-- Vanilla `threshold`: while positive the monster stays fixed on its
  current enemy and won't be pulled onto a new attacker. Set to 100 when a
  cross-species hit retargets it; counts down in `A_Chase`. -/
  threshold : Nat := 0
  /-- Vanilla `MF_JUSTHIT`: the enemy just wounded us — attack straight back
  on the next `A_Chase`, ignoring range. Cleared when that attack fires. -/
  justHit : Bool := false
  /-- Dead (or exploding): stops blocking and being shootable. -/
  corpse : Bool := false
  /-- A lost soul mid-flight toward its target. -/
  charging : Bool := false
  /-- An arch-vile is reviving this corpse: it walks its death frames in
  reverse, back onto its feet, before returning to life. -/
  raising : Bool := false
  /-- Nightmare respawn: the map spot this monster was placed at, how it faced,
  and how long its corpse has lain there. Only monsters placed from the map
  carry a spawn point (`canRespawn`); those born mid-game — Icon cubes, a pain
  elemental's lost souls — never reincarnate. -/
  spawnX : Float := 0
  spawnY : Float := 0
  spawnAngle : Float := 0
  canRespawn : Bool := false
  respawnTic : Nat := 0
  /-- Fireball shooters don't collide with their own missile. -/
  shooterUid : Nat := 0
  /-- Dropped by a dying monster rather than placed on the map: it carries
  half the ammo of a map pickup and never counts toward the item tally
  (vanilla `MF_DROPPED`). Its own field because `1` is also a perfectly
  ordinary `shooterUid` — uids start there. -/
  dropped : Bool := false
  /-- The player launched this missile: it hits monsters, spares the player. -/
  fromPlayer : Bool := false
  /-- A state was entered via `setState` and its first-frame action has not
  run yet. Vanilla `P_SetMobjState` runs the entered state's action on the
  spot; the action dispatcher lives in `Dill.Game.Enemy`, above this module,
  so `setState` raises this flag instead and `tickMobjs` runs (and clears)
  the action as the mobj next comes up for thought — at most one tic later
  than vanilla. -/
  entryPending : Bool := false
  removed : Bool := false
  deriving Inhabited

namespace Mobj

def stateDef (m : Mobj) : StateDef := m.info.states[m.state]!

/-- Sprite family for the current state (deaths can override it). -/
def sprite (m : Mobj) : String := m.stateDef.spriteOverride.getD m.info.sprite

def solid (m : Mobj) : Bool := m.info.solid && !m.corpse
def shootable (m : Mobj) : Bool := m.info.shootable && !m.corpse && m.health > 0

/-- Enter a state. The state's own first-frame action still has to run
(vanilla `P_SetMobjState` calls it); actions need `GameState`, so this only
marks it due — `entryPending` — for `tickMobjs` to dispatch. -/
def setState (m : Mobj) (s : Nat) : Mobj :=
  { m with state := s, tics := m.info.states[s]!.tics, entryPending := true }

def distanceTo (m : Mobj) (x y : Float) : Float :=
  Float.sqrt ((m.x - x) ^ 2 + (m.y - y) ^ 2)

end Mobj

/-- Doom's eight compass move directions, east = 0, counterclockwise. -/
def dirAngle (dir : Nat) : Float := Float.ofNat dir * (3.14159265358979 / 4)

/-- Build a live mobj. `fast` selects vanilla's Nightmare actor tables (see
`ActorInfo.fast`); the info travels on the mobj, so a thing spawned under
those rules keeps them for its whole life. -/
def spawnMobj (uid : Nat) (kind : ActorKind) (lvl : Level)
    (x y angle : Float) (fast : Bool := false) : Mobj :=
  let info := if fast then ActorInfo.fast kind (ActorInfo.ofKind kind)
              else ActorInfo.ofKind kind
  let sec := lvl.sectors[lvl.sectorAt x y]!
  -- ceiling-hung things dangle down from the ceiling; the rest sit on the floor
  let z := if info.ceilingHang then sec.ceilH - info.height else sec.floorH
  { uid, kind, info, x, y, z, angle
    health := info.health
    -- Vanilla `P_SpawnMobj` seeds `reactiontime` only below Nightmare
    -- (`if (gameskill != sk_nightmare)` in p_mobj.c) — there a woken monster
    -- may answer with a ranged attack at once. `fast` is set exactly on
    -- DILL's Nightmare (skill 5), so it carries the skill here.
    reactionTime := if fast then 0 else info.reactionTime
    state := info.spawnState
    tics := info.states[info.spawnState]!.tics }

end Dill
