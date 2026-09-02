/-!
# Actor definitions

A distilled `info.c`: every kind of map object — monster, pickup,
decoration, projectile, effect — is an `ActorInfo`: physique, behavior
numbers, and a state table. A state is a sprite frame shown for some tics,
with an optional action (the AI/attack hooks run by the mobj thinker).

The whole retail bestiary is defined — Doom's former humans through the
Cyberdemon and Spider Mastermind, Doom II's additions up to the arch-vile
and the Icon of Sin — along with every projectile, the effects (puffs,
blood, fog), and the item world. Adding an actor is adding a table entry.
-/

namespace Dill

/-- Behavior hooks, dispatched by `Dill.Game.Enemy`/`Combat`. -/
inductive Action where
  | look        -- scan for the player; wake to the see state
  | chase       -- stalk the target, occasionally attack
  | faceTarget
  | posAttack   -- zombieman: one bullet
  | sposAttack  -- shotgun guy: three bullets
  | trooAttack  -- imp: scratch in melee, else fireball
  | sargAttack  -- demon: bite
  | headAttack  -- cacodemon: bite in melee, else fireball
  | bruisAttack -- baron: claw in melee, else green fireball
  | skullAttack -- lost soul: hurl itself at the player
  | cyberAttack -- cyberdemon: a rocket, no melee
  | spidRefire  -- spider mastermind: hold the trigger while the shot is good
  | cposAttack  -- chaingunner / SS: one bullet of a held burst
  | cposRefire  -- as `spidRefire`, but the chaingunner lets go sooner
  | fatRaise    -- mancubus: rear back to fire (face + the DSMANATK grunt)
  | fatAttack   -- mancubus: first fireball pair (A_FatAttack1)
  | fatAttack2  -- mancubus: second pair, swung the other way
  | fatAttack3  -- mancubus: third pair, the narrow centre
  | bspiAttack  -- arachnotron: a plasma bolt
  | skelWhoosh  -- revenant: the swing before the punch (sound only)
  | skelFist    -- revenant: the punch itself
  | skelMissile -- revenant: its rocket
  | painAttack  -- pain elemental: spit out a lost soul
  | painDie     -- pain elemental: burst into three more on death
  | brainAwake  -- icon of sin: the spitter's opening roar as the level begins
  | brainSpit   -- icon of sin: fling a spawn cube at a target spot
  | spawnFly    -- the cube in flight: at the target, birth a monster
  | brainScream -- the brain's death: a wall-wide cascade of explosions + roar
  | brainExplode -- one explosion in the cascade spawns the next
  | brainDie    -- the brain is destroyed: the level (and the game) is won
  | vileChase   -- arch-vile: chase, but raise any corpse it walks past
  | vileStart   -- arch-vile: the DSVILATK whoosh as it begins its attack
  | vileTarget  -- arch-vile: plant the flame on the target
  | vileAttack  -- arch-vile: set it off
  | fire        -- the flame itself: cling to the victim while the vile sees them
  | fireStart   -- the flame's first frame: the DSFLAMST ignition, then cling
  | fireCrackle -- the flame's two crackle frames: the DSFLAME roar, then cling
  | keenDie     -- commander keen: the last one opens the tag-666 doors
  | hoof        -- the cyberdemon's hoof-fall (sound only, then chase)
  | metal       -- the heavy tread of both bosses (sound only, then chase)
  | babyMetal   -- the arachnotron's lighter tread (DSBSPWLK, then chase)
  | bfgSpray    -- the BFG burst's third frame: rake the room (A_BFGSpray)
  | pain
  | scream      -- (sound-only in vanilla; kept for fidelity of the tables)
  | xscream     -- the wet "slop" of a body bursting apart (extreme death)
  | fall        -- become non-solid: corpses are walkable
  | explode     -- barrel: 128 radius damage
  deriving Repr, DecidableEq, Inhabited

/-- One entry of an actor's film strip. -/
structure StateDef where
  frame  : Char
  /-- Tics to stay; negative = forever. -/
  tics   : Int
  action : Option Action := none
  /-- Next state index; `none` = remove the mobj. -/
  next   : Option Nat := none
  /-- Drawn at full brightness (muzzle flames, fireballs). -/
  bright : Bool := false
  /-- Some deaths change sprite family (barrel → BEXP explosion). -/
  spriteOverride : Option String := none
  deriving Repr, Inhabited

/-- Everything fixed about a kind of thing. -/
structure ActorInfo where
  sprite     : String            -- 4-char sprite family
  states     : Array StateDef
  spawnState : Nat := 0
  seeState    : Option Nat := none
  painState   : Option Nat := none
  missileState : Option Nat := none  -- ranged attack entry
  meleeState  : Option Nat := none
  deathState  : Option Nat := none
  /-- Extreme-death (gib) entry, used when overkill damage bursts the body. -/
  xdeathState : Option Nat := none
  /-- The state a monster stands in while raising a corpse — vanilla
  hardcodes `S_VILE_HEAL1` in `A_VileChase`; only the arch-vile has one. -/
  healState : Option Nat := none
  health     : Int := 1000
  speed      : Float := 0
  radius     : Float := 20
  height     : Float := 16
  painChance : Nat := 0
  /-- Vanilla `reactiontime`: tics a freshly spawned or woken monster holds
  fire before it will loose a ranged attack (`P_CheckMissileRange`). Zeroed
  the instant it is hurt. Every Doom monster uses 8. -/
  reactionTime : Nat := 8
  /-- Vanilla `mass`: how hard a hit shoves it. Knockback thrust from damage
  is inversely proportional to this — light souls fly, heavy bosses barely
  budge. Default 100, as most monsters and the player. -/
  mass       : Float := 100
  /-- Blocks movement. -/
  solid      : Bool := false
  /-- Can be damaged. -/
  shootable  : Bool := false
  /-- Touching it picks it up. -/
  pickup     : Bool := false
  /-- A flying projectile: explodes on impact. -/
  missile    : Bool := false
  /-- Sparks puffs instead of blood when shot (barrels). -/
  noBlood    : Bool := false
  /-- Floats: ignores steps, drop-offs, and gravity (cacodemon, lost soul). -/
  flying     : Bool := false
  /-- Hangs in the air where it spawns — never falls to the floor. Bullet
  puffs, blood, fog and flames (vanilla `MF_NOGRAVITY`); a wall-hit puff
  must stay at the impact point, not rain down. -/
  noGravity  : Bool := false
  /-- Missile damage: `n` dice of `sides`, times `damageMult`. -/
  damageDice : Nat × Nat := (0, 0)
  damageMult : Nat := 1
  /-- Explosion radius on impact (rockets); 0 = none. -/
  blastRadius : Nat := 0
  /-- Drawn as a dark shade (spectres). -/
  shadow     : Bool := false
  /-- Counts toward the kill percentage (lost souls don't, per vanilla). -/
  countKill  : Bool := false
  /-- Hangs from the ceiling instead of standing on the floor
  (vanilla `MF_SPAWNCEILING`: the hanging bodies of the hell maps). -/
  ceilingHang : Bool := false
  /-- An arch-vile can resurrect this monster's corpse — vanilla's
  `raisestate != S_NULL`, true for exactly the fourteen kinds info.c gives a
  raise chain. DILL revives by rewinding the death chain instead of playing
  a dedicated one, so eligibility has to be recorded explicitly; without it
  the vile would raise things vanilla never can (a cyberdemon, a keen). -/
  raisable : Bool := false
  deriving Inhabited

/-- The bestiary and item world, indexed by `ActorKind`. -/
inductive ActorKind where
  | zombieman | shotgunGuy | imp | demon | spectre
  | cacodemon | lostSoul | baron | cyberdemon | spiderMastermind
  | chaingunner | wolfSS | hellKnight | mancubus | arachnotron | revenant
  | painElemental | archVile | commanderKeen | vileFire
  | iconBrain | iconSpit | iconTarget | spawnCube | brainExplosion
  | barrel | impBall | cacoBall | baronBall | puff | blood | teleFog
  | rocket | plasmaBall | bfgBall | bfgPuff
  | fatShot | arachPlasma | revenantMissile
  | item (spriteFamily : String) (frames : String)   -- animated pickup
  | scenery (spriteFamily : String) (frames : String) (solid : Bool)
  deriving Repr, DecidableEq, Inhabited

namespace ActorInfo

/-- A looping animation over `frames`, `tics` each. -/
private def loopStates (frames : String) (tics : Int) (bright : Bool := false) :
    Array StateDef := Id.run do
  let n := frames.length
  let mut out := #[]
  for i in [0:n] do
    out := out.push { frame := frames.toList[i]!, tics
                      next := some ((i + 1) % n), bright }
  return out

/-- Build a chain starting at index `base`; `none` next = remove. -/
private def chain (base : Nat) (steps : Array (Char × Int × Option Action))
    (last : Option Nat) (bright : Bool := false)
    (spriteOverride : Option String := none) : Array StateDef := Id.run do
  let mut out := #[]
  for i in [0:steps.size] do
    let (frame, tics, action) := steps[i]!
    let next := if i + 1 < steps.size then some (base + i + 1) else last
    out := out.push { frame, tics, action, next, bright, spriteOverride }
  return out

/-- Mark the states at `idxs` (indices local to the given array) fullbright —
for chains where vanilla lights only some frames (a muzzle flame mid-burst,
the mancubus's firing frames). -/
private def litAt (states : Array StateDef) (idxs : List Nat) : Array StateDef :=
  idxs.foldl (fun s i => s.modify i ({ · with bright := true })) states

/-- The shared shape of the humanoid monsters: look 0–1, chase 2–9,
attack 10–12, pain 13–14, death 15+. -/
private def monster (sprite : String) (health : Int) (speed radius : Float)
    (painChance : Nat) (chaseTics : Int) (painFrame : Char)
    (attack : Array (Char × Int × Option Action))
    (death : Array (Char × Int × Option Action))
    (xdeath : Array (Char × Int × Option Action) := #[])
    (height : Float := 56) (painTics : Int := 3) : ActorInfo :=
  let look := chain 0 #[('A', 10, some .look), ('B', 10, some .look)] (some 0)
  let chase := chain 2 #[
    ('A', chaseTics, some .chase), ('A', chaseTics, some .chase),
    ('B', chaseTics, some .chase), ('B', chaseTics, some .chase),
    ('C', chaseTics, some .chase), ('C', chaseTics, some .chase),
    ('D', chaseTics, some .chase), ('D', chaseTics, some .chase)] (some 2)
  let attackStates := chain 10 attack (some 2)
  let pain := chain (10 + attack.size)
    #[(painFrame, painTics, none), (painFrame, painTics, some .pain)] (some 2)
  let deathBase := 10 + attack.size + 2
  let deathStates := chain deathBase death none
  let xdeathBase := deathBase + death.size
  let xdeathStates := if xdeath.isEmpty then #[] else chain xdeathBase xdeath none
  { sprite
    states := look ++ chase ++ attackStates ++ pain ++ deathStates ++ xdeathStates
    seeState := some 2
    missileState := some 10
    painState := some (10 + attack.size)
    deathState := some deathBase
    xdeathState := if xdeath.isEmpty then none else some xdeathBase
    health, speed, radius, height, painChance
    solid := true, shootable := true, countKill := true }

def zombieman : ActorInfo :=
  { monster "POSS" 20 8 20 200 4 'G'
      #[('E', 10, some .faceTarget), ('F', 8, some .posAttack), ('E', 8, none)]
      #[('H', 5, none), ('I', 5, some .scream), ('J', 5, some .fall),
        ('K', 5, none), ('L', -1, none)]
      (xdeath := #[('M', 5, none), ('N', 5, some .xscream), ('O', 5, some .fall),
        ('P', 5, none), ('Q', 5, none), ('R', 5, none), ('S', 5, none),
        ('T', 5, none), ('U', -1, none)])
    with raisable := true }

def shotgunGuy : ActorInfo :=
  let base := monster "SPOS" 30 8 20 170 3 'G'
    #[('E', 10, some .faceTarget), ('F', 10, some .sposAttack), ('E', 10, none)]
    #[('H', 5, none), ('I', 5, some .scream), ('J', 5, some .fall),
      ('K', 5, none), ('L', -1, none)]
    (xdeath := #[('M', 5, none), ('N', 5, some .xscream), ('O', 5, some .fall),
      ('P', 5, none), ('Q', 5, none), ('R', 5, none), ('S', 5, none),
      ('T', 5, none), ('U', -1, none)])
  -- vanilla S_SPOS_ATK2 — the frame that fires — is fullbright
  { base with states := litAt base.states [11], raisable := true }

def imp : ActorInfo :=
  { monster "TROO" 60 8 20 200 3 'H'
      #[('E', 8, some .faceTarget), ('F', 8, some .faceTarget),
        ('G', 6, some .trooAttack)]
      #[('I', 8, none), ('J', 8, some .scream), ('K', 6, none),
        ('L', 6, some .fall), ('M', -1, none)]
      -- vanilla S_TROO_XDIE: the slop on the second frame, the corpse
      -- softening (A_Fall) only on the fourth
      (xdeath := #[('N', 5, none), ('O', 5, some .xscream), ('P', 5, none),
        ('Q', 5, some .fall), ('R', 5, none), ('S', 5, none), ('T', 5, none),
        ('U', -1, none)])
      (painTics := 2)
    with meleeState := some 10, raisable := true }

def demon : ActorInfo :=
  { monster "SARG" 150 10 30 180 2 'H'
      #[('E', 8, some .faceTarget), ('F', 8, some .faceTarget),
        ('G', 8, some .sargAttack)]
      #[('I', 8, none), ('J', 8, some .scream), ('K', 4, none),
        ('L', 4, some .fall), ('M', 4, none), ('N', -1, none)]
      (painTics := 2)
    with meleeState := some 10, missileState := none, mass := 400
         raisable := true }

def spectre : ActorInfo :=
  { demon with shadow := true }

/-- The cacodemon idles and drifts on a single frame each (vanilla gives it
one STND and one RUN state) and bites or spits with the same three-frame
wind-up. It has **no meleestate** in vanilla: `A_HeadAttack` decides bite
vs fireball by range itself, and the missing melee entry is what makes
`P_CheckMissileRange` treat it as "no melee attack, so fire more". -/
def cacodemon : ActorInfo :=
  let look := chain 0 #[('A', 10, some .look)] (some 0)
  let run := chain 1 #[('A', 3, some .chase)] (some 1)
  -- only S_HEAD_ATK3 — the mouth-flash frame that fires — is fullbright
  let attack := litAt (chain 2 #[('B', 5, some .faceTarget),
    ('C', 5, some .faceTarget), ('D', 5, some .headAttack)] (some 1)) [2]
  let pain := chain 5 #[('E', 3, none), ('E', 3, some .pain),
    ('F', 6, none)] (some 1)
  let death := chain 8 #[('G', 8, none), ('H', 8, some .scream), ('I', 8, none),
    ('J', 8, none), ('K', 8, some .fall), ('L', -1, none)] none
  { sprite := "HEAD"
    states := look ++ run ++ attack ++ pain ++ death
    seeState := some 1, missileState := some 2
    painState := some 5, deathState := some 8
    health := 400, speed := 8, radius := 31, height := 56
    painChance := 128, solid := true, shootable := true, flying := true
    mass := 400, countKill := true, raisable := true }

/-- The lost soul is its own weapon: it screams and rams. -/
def lostSoul : ActorInfo :=
  let look := chain 0 #[('A', 10, some .look), ('B', 10, some .look)]
    (some 0) (bright := true)
  let run := chain 2 #[('A', 6, some .chase), ('B', 6, some .chase)]
    (some 2) (bright := true)
  -- Vanilla S_SKULL_ATK1–4: face, launch, then a C/D loop the soul sits in
  -- for the whole flight (states 6 ↔ 7). Ending the chain back at the run
  -- states instead put `A_Chase` in charge mid-dive — the soul steered and
  -- could even launch a second attack while already flying. The loop is
  -- broken by the slam itself, which drops it to its spawn state.
  let attack := chain 4 #[('C', 10, some .faceTarget),
    ('D', 4, some .skullAttack), ('C', 4, none), ('D', 4, none)]
    (some 6) (bright := true)
  let pain := chain 8 #[('E', 3, none), ('E', 3, some .pain)]
    (some 2) (bright := true)
  -- vanilla S_SKULL_DIE: the burst frames F–I glow (A_Fall rides I); the
  -- J/K embers gutter out unlit
  let death := chain 10 #[('F', 6, none), ('G', 6, some .scream), ('H', 6, none),
    ('I', 6, some .fall)] (some 14) (bright := true)
    ++ chain 14 #[('J', 6, none), ('K', 6, none)] none
  { sprite := "SKUL"
    states := look ++ run ++ attack ++ pain ++ death
    seeState := some 2, missileState := some 4
    painState := some 8, deathState := some 10
    health := 100, speed := 8, radius := 16, height := 56
    painChance := 256, solid := true, shootable := true, flying := true
    -- vanilla flags SOLID|SHOOTABLE|FLOAT|NOGRAVITY: without NOGRAVITY the
    -- dying soul sank to the floor mid-burst instead of exploding in place
    noGravity := true
    -- vanilla `damage` 3: a slam deals (P_Random()%8 + 1) * 3 — one d8
    -- tripled, not 3d8
    mass := 50, damageDice := (1, 8), damageMult := 3 }

def baron : ActorInfo :=
  monster "BOSS" 1000 8 24 50 3 'H'
    #[('E', 8, some .faceTarget), ('F', 8, some .faceTarget),
      ('G', 8, some .bruisAttack)]
    #[('I', 8, none), ('J', 8, some .scream), ('K', 8, none),
      ('L', 8, some .fall), ('M', 8, none), ('N', 8, none), ('O', -1, none)]
    (height := 64) (painTics := 2)
  |> fun b => { b with meleeState := some 10, mass := 1000, raisable := true }

/-- The Cyberdemon: no melee at all, just three rockets on a slow cadence.
Its walk cycle is the standard eight, but two of the steps land a footfall
sound, so the table is written out rather than built by `monster`. -/
def cyberdemon : ActorInfo :=
  let look := chain 0 #[('A', 10, some .look), ('B', 10, some .look)] (some 0)
  let run := chain 2 #[
    ('A', 3, some .hoof),  ('A', 3, some .chase),
    ('B', 3, some .chase), ('B', 3, some .chase),
    ('C', 3, some .chase), ('C', 3, some .chase),
    ('D', 3, some .metal), ('D', 3, some .chase)] (some 2)
  let attack := chain 10 #[
    ('E', 6, some .faceTarget),  ('F', 12, some .cyberAttack),
    ('E', 12, some .faceTarget), ('F', 12, some .cyberAttack),
    ('E', 12, some .faceTarget), ('F', 12, some .cyberAttack)] (some 2)
  let pain := chain 16 #[('G', 10, some .pain)] (some 2)
  let death := chain 17 #[
    ('H', 10, none), ('I', 10, some .scream), ('J', 10, none),
    ('K', 10, none), ('L', 10, none), ('M', 10, some .fall),
    ('N', 10, none), ('O', 10, none), ('P', 30, none),
    ('P', -1, none)] none
  { sprite := "CYBR"
    states := look ++ run ++ attack ++ pain ++ death
    seeState := some 2, missileState := some 10
    painState := some 16, deathState := some 17
    health := 4000, speed := 16, radius := 40, height := 110
    painChance := 20, mass := 1000, solid := true, shootable := true
    countKill := true }

/-- The Spider Mastermind: a chaingun on legs. It walks twelve frames with
three footfalls, and its attack loops back on itself through `spidRefire`
for as long as the shot stays good. -/
def spiderMastermind : ActorInfo :=
  let look := chain 0 #[('A', 10, some .look), ('B', 10, some .look)] (some 0)
  let run := chain 2 #[
    ('A', 3, some .metal), ('A', 3, some .chase),
    ('B', 3, some .chase), ('B', 3, some .chase),
    ('C', 3, some .metal), ('C', 3, some .chase),
    ('D', 3, some .chase), ('D', 3, some .chase),
    ('E', 3, some .metal), ('E', 3, some .chase),
    ('F', 3, some .chase), ('F', 3, some .chase)] (some 2)
  -- the refire step jumps back to the first shot, not to the wind-up;
  -- tics are vanilla S_SPID_ATK1–4: 20, 4, 4, 1 — the bursts land far
  -- faster than the long aim suggests
  let attack := chain 14 #[
    ('A', 20, some .faceTarget), ('G', 4, some .sposAttack),
    ('H', 4, some .sposAttack), ('H', 1, some .spidRefire)]
    (some 15) (bright := true)
  let pain := chain 18 #[('I', 3, none), ('I', 3, some .pain)] (some 2)
  let death := chain 20 #[
    ('J', 20, some .scream), ('K', 10, some .fall), ('L', 10, none),
    ('M', 10, none), ('N', 10, none), ('O', 10, none), ('P', 10, none),
    ('Q', 10, none), ('R', 10, none), ('S', 30, none),
    ('S', -1, none)] none
  { sprite := "SPID"
    states := look ++ run ++ attack ++ pain ++ death
    seeState := some 2, missileState := some 14
    painState := some 18, deathState := some 20
    health := 3000, speed := 12, radius := 128, height := 100
    painChance := 40, mass := 1000, solid := true, shootable := true
    countKill := true }

/-! ## Doom II's bestiary

The Hell Knight is a lighter Baron sharing its film strip exactly, so it is
a record update. The rest are written out: each has a walk cycle or attack
loop that the `monster` shape does not fit. -/

def hellKnight : ActorInfo :=
  { baron with sprite := "BOS2", health := 500 }

/-- The chaingunner holds its burst through `cposRefire`, which loops back
to the first shot rather than to the wind-up. -/
def chaingunner : ActorInfo :=
  let look := chain 0 #[('A', 10, some .look), ('B', 10, some .look)] (some 0)
  let run := chain 2 #[
    ('A', 3, some .chase), ('A', 3, some .chase),
    ('B', 3, some .chase), ('B', 3, some .chase),
    ('C', 3, some .chase), ('C', 3, some .chase),
    ('D', 3, some .chase), ('D', 3, some .chase)] (some 2)
  let atkFace := chain 10 #[('E', 10, some .faceTarget)] (some 11)
  -- the two shots glow; the 1-tic refire beat between bursts does not
  let atkFire := chain 11 #[
    ('F', 4, some .cposAttack), ('E', 4, some .cposAttack)]
      (some 13) (bright := true)
    ++ chain 13 #[('F', 1, some .cposRefire)] (some 11)
  let pain := chain 14 #[('G', 3, none), ('G', 3, some .pain)] (some 2)
  let death := chain 16 #[
    ('H', 5, none), ('I', 5, some .scream), ('J', 5, some .fall),
    ('K', 5, none), ('L', 5, none), ('M', 5, none), ('N', -1, none)] none
  -- vanilla S_CPOS_XDIE1–6: a six-frame gib strip, O through T
  let xdeath := chain 23 #[
    ('O', 5, none), ('P', 5, some .xscream), ('Q', 5, some .fall),
    ('R', 5, none), ('S', 5, none), ('T', -1, none)] none
  { sprite := "CPOS"
    states := look ++ run ++ atkFace ++ atkFire ++ pain ++ death ++ xdeath
    seeState := some 2, missileState := some 10, painState := some 14
    deathState := some 16, xdeathState := some 23
    health := 70, speed := 8, radius := 20, height := 56
    painChance := 170, solid := true, shootable := true, countKill := true
    raisable := true }

/-- The Wolfenstein SS: the same chaingun, a longer wind-up. -/
def wolfSS : ActorInfo :=
  let look := chain 0 #[('A', 10, some .look), ('B', 10, some .look)] (some 0)
  let run := chain 2 #[
    ('A', 3, some .chase), ('A', 3, some .chase),
    ('B', 3, some .chase), ('B', 3, some .chase),
    ('C', 3, some .chase), ('C', 3, some .chase),
    ('D', 3, some .chase), ('D', 3, some .chase)] (some 2)
  let atkFace := chain 10 #[('E', 10, some .faceTarget)] (some 11)
  -- Only the two `G` frames — the ones that actually loose a round — are
  -- fullbright in vanilla (`32774`); the `F` frames between them are plain,
  -- so the SS flickers on each shot rather than glowing throughout.
  let atkFire :=
    chain 11 #[('F', 10, some .faceTarget)] (some 12)
    ++ chain 12 #[('G', 4, some .cposAttack)] (some 13) (bright := true)
    ++ chain 13 #[('F', 6, some .faceTarget)] (some 14)
    ++ chain 14 #[('G', 4, some .cposAttack)] (some 15) (bright := true)
    ++ chain 15 #[('F', 1, some .cposRefire)] (some 11)
  let pain := chain 16 #[('H', 3, none), ('H', 3, some .pain)] (some 2)
  let death := chain 18 #[
    ('I', 5, none), ('J', 5, some .scream), ('K', 5, some .fall),
    ('L', 5, none), ('M', -1, none)] none
  -- vanilla S_SSWV_XDIE1–9: a silent first frame, then the slop, then the
  -- corpse softens — the same cadence as the other former humans
  let xdeath := chain 23 #[
    ('N', 5, none), ('O', 5, some .xscream), ('P', 5, some .fall),
    ('Q', 5, none), ('R', 5, none), ('S', 5, none), ('T', 5, none),
    ('U', 5, none), ('V', -1, none)] none
  { sprite := "SSWV"
    states := look ++ run ++ atkFace ++ atkFire ++ pain ++ death ++ xdeath
    seeState := some 2, missileState := some 10, painState := some 16
    deathState := some 18, xdeathState := some 23
    health := 50, speed := 8, radius := 20, height := 56
    painChance := 170, solid := true, shootable := true, countKill := true
    raisable := true }

/-- The mancubus throws three volleys, facing between each. -/
def mancubus : ActorInfo :=
  let look := chain 0 #[('A', 15, some .look), ('B', 15, some .look)] (some 0)
  let run := chain 2 #[
    ('A', 4, some .chase), ('A', 4, some .chase),
    ('B', 4, some .chase), ('B', 4, some .chase),
    ('C', 4, some .chase), ('C', 4, some .chase),
    ('D', 4, some .chase), ('D', 4, some .chase),
    ('E', 4, some .chase), ('E', 4, some .chase),
    ('F', 4, some .chase), ('F', 4, some .chase)] (some 2)
  -- vanilla S_FATT_ATK1–10: each fullbright H volley is followed by a
  -- *pair* of 5-tic A_FaceTarget beats — the recoil frame I, then the
  -- raised frame G — so the mancubus tracks you between shots
  let attack := litAt (chain 14 #[
    ('G', 20, some .fatRaise),  ('H', 10, some .fatAttack),
    ('I', 5, some .faceTarget), ('G', 5, some .faceTarget),
    ('H', 10, some .fatAttack2),
    ('I', 5, some .faceTarget), ('G', 5, some .faceTarget),
    ('H', 10, some .fatAttack3),
    ('I', 5, some .faceTarget), ('G', 5, some .faceTarget)] (some 2))
    [1, 4, 7]
  -- pain rides frame J; death runs K–T (vanilla skips nothing: K opens,
  -- the scream lands on L, the fall on M)
  let pain := chain 24 #[('J', 3, none), ('J', 3, some .pain)] (some 2)
  let death := chain 26 #[
    ('K', 6, none), ('L', 6, some .scream), ('M', 6, some .fall),
    ('N', 6, none), ('O', 6, none), ('P', 6, none), ('Q', 6, none),
    ('R', 6, none), ('S', 6, none), ('T', -1, none)] none
  { sprite := "FATT"
    states := look ++ run ++ attack ++ pain ++ death
    seeState := some 2, missileState := some 14, painState := some 24
    deathState := some 26
    health := 600, speed := 8, radius := 48, height := 64
    painChance := 80, mass := 1000, solid := true, shootable := true
    countKill := true, raisable := true }

/-- The arachnotron: plasma on a refire loop, and a mechanical tread. -/
def arachnotron : ActorInfo :=
  let look := chain 0 #[('A', 10, some .look), ('B', 10, some .look)] (some 0)
  -- vanilla S_BSPI_SIGHT: on waking (and whenever `A_SpidRefire` breaks off)
  -- it stands stock-still for 20 tics before the walk cycle starts
  let sight := chain 2 #[('A', 20, none)] (some 3)
  let run := chain 3 #[
    ('A', 3, some .babyMetal), ('A', 3, some .chase),
    ('B', 3, some .chase), ('B', 3, some .chase),
    ('C', 3, some .chase), ('C', 3, some .chase),
    ('D', 3, some .babyMetal), ('D', 3, some .chase),
    ('E', 3, some .chase), ('E', 3, some .chase),
    ('F', 3, some .chase), ('F', 3, some .chase)] (some 3)
  let attack := chain 15 #[
    ('A', 20, some .faceTarget), ('G', 4, some .bspiAttack),
    ('H', 4, none), ('H', 1, some .spidRefire)] (some 16) (bright := true)
  -- pain rides frame I, death J–P (info.c skips H entirely); vanilla's
  -- A_Scream sits on the very first death frame
  let pain := chain 19 #[('I', 3, none), ('I', 3, some .pain)] (some 3)
  let death := chain 21 #[
    ('J', 20, some .scream), ('K', 7, some .fall),
    ('L', 7, none), ('M', 7, none), ('N', 7, none), ('O', 7, none),
    ('P', -1, none)] none
  { sprite := "BSPI"
    states := look ++ sight ++ run ++ attack ++ pain ++ death
    seeState := some 2, missileState := some 15, painState := some 19
    deathState := some 21
    health := 500, speed := 12, radius := 64, height := 64
    painChance := 128, mass := 600, solid := true, shootable := true
    countKill := true, raisable := true }

/-- The revenant: a punch up close, a rocket at range. -/
def revenant : ActorInfo :=
  let look := chain 0 #[('A', 10, some .look), ('B', 10, some .look)] (some 0)
  let run := chain 2 #[
    ('A', 2, some .chase), ('A', 2, some .chase),
    ('B', 2, some .chase), ('B', 2, some .chase),
    ('C', 2, some .chase), ('C', 2, some .chase),
    ('D', 2, some .chase), ('D', 2, some .chase),
    ('E', 2, some .chase), ('E', 2, some .chase),
    ('F', 2, some .chase), ('F', 2, some .chase)] (some 2)
  -- vanilla S_SKEL_FIST1: a 0-tic A_FaceTarget beat opens the punch
  let melee := chain 14 #[
    ('G', 0, some .faceTarget), ('G', 6, some .skelWhoosh),
    ('H', 6, some .faceTarget), ('I', 6, some .skelFist)] (some 2)
  -- vanilla S_SKEL_MISS1/2: two *fullbright* J beats (a 0-tic and a 10-tic
  -- A_FaceTarget — the shoulder launchers flare before the shot); the K
  -- launch frames are unlit
  let missile := chain 18 #[
    ('J', 0, some .faceTarget), ('J', 10, some .faceTarget)]
      (some 20) (bright := true)
    ++ chain 20 #[('K', 10, some .skelMissile),
      ('K', 10, some .faceTarget)] (some 2)
  let pain := chain 22 #[('L', 5, none), ('L', 5, some .pain)] (some 2)
  let death := chain 24 #[
    ('L', 7, none), ('M', 7, none), ('N', 7, some .scream),
    ('O', 7, some .fall), ('P', 7, none), ('Q', -1, none)] none
  { sprite := "SKEL"
    states := look ++ run ++ melee ++ missile ++ pain ++ death
    seeState := some 2, meleeState := some 14, missileState := some 18
    painState := some 22, deathState := some 24
    health := 300, speed := 10, radius := 20, height := 56
    -- vanilla MT_UNDEAD mass 500: bony, but it barely rocks to a shot
    painChance := 100, mass := 500, solid := true, shootable := true
    countKill := true, raisable := true }

/-- The pain elemental drifts like a cacodemon and fights by proxy: it
spits a lost soul, and bursts into three more when killed. -/
def painElemental : ActorInfo :=
  let look := chain 0 #[('A', 10, some .look)] (some 0)
  let run := chain 1 #[
    ('A', 3, some .chase), ('A', 3, some .chase),
    ('B', 3, some .chase), ('B', 3, some .chase),
    ('C', 3, some .chase), ('C', 3, some .chase)] (some 1)
  -- the two F frames — the mouth agape, spitting — are fullbright
  let attack := chain 7 #[
    ('D', 5, some .faceTarget), ('E', 5, some .faceTarget)] (some 9)
    ++ chain 9 #[('F', 5, some .faceTarget), ('F', 0, some .painAttack)]
      (some 1) (bright := true)
  let pain := chain 11 #[('G', 6, none), ('G', 6, some .pain)] (some 1)
  -- vanilla S_PAIN_DIE1–6: the scream rides the second frame (I), the burst
  -- of souls the fifth (L)
  let death := chain 13 #[
    ('H', 8, none), ('I', 8, some .scream), ('J', 8, none),
    ('K', 8, none), ('L', 8, some .painDie), ('M', 8, none)] none true
  { sprite := "PAIN"
    states := look ++ run ++ attack ++ pain ++ death
    seeState := some 1, missileState := some 7
    painState := some 11, deathState := some 13
    health := 400, speed := 8, radius := 31, height := 56
    painChance := 128, mass := 400, solid := true, shootable := true
    flying := true, countKill := true, raisable := true }

/-- The flame an arch-vile plants under its target: harmless in itself, the
damage lands in `vileAttack`. Every frame runs `A_Fire`, which is what makes
it chase the victim across the floor instead of sitting where it was
planted — and stop dead when the vile loses sight of them. The frame walk is
vanilla S_FIRE1–30: sixty tics of A/B flicker growing through C/D/E/F to the
G/H roar, each rung revisited on the way up. The first frame carries
vanilla's `A_StartFire` (the DSFLAMST ignition) and S_FIRE5/S_FIRE19 its two
`A_FireCrackle`s (the DSFLAME roar) — each is `A_Fire` plus the sound cue. -/
def vileFire : ActorInfo :=
  { sprite := "FIRE", noGravity := true
    states := chain 0 #[
      ('A', 2, some .fireStart), ('B', 2, some .fire), ('A', 2, some .fire),
      ('B', 2, some .fire), ('C', 2, some .fireCrackle), ('B', 2, some .fire),
      ('C', 2, some .fire), ('B', 2, some .fire), ('C', 2, some .fire),
      ('D', 2, some .fire), ('C', 2, some .fire), ('D', 2, some .fire),
      ('C', 2, some .fire), ('D', 2, some .fire), ('E', 2, some .fire),
      ('D', 2, some .fire), ('E', 2, some .fire), ('D', 2, some .fire),
      ('E', 2, some .fireCrackle), ('F', 2, some .fire), ('E', 2, some .fire),
      ('F', 2, some .fire), ('E', 2, some .fire), ('F', 2, some .fire),
      ('G', 2, some .fire), ('H', 2, some .fire), ('G', 2, some .fire),
      ('H', 2, some .fire), ('G', 2, some .fire), ('H', 2, some .fire)]
      none true
    radius := 20, height := 16 }

/-- The arch-vile: raises the dead as it walks, and burns what it sees. -/
def archVile : ActorInfo :=
  let look := chain 0 #[('A', 10, some .look), ('B', 10, some .look)] (some 0)
  let run := chain 2 #[
    ('A', 2, some .vileChase), ('A', 2, some .vileChase),
    ('B', 2, some .vileChase), ('B', 2, some .vileChase),
    ('C', 2, some .vileChase), ('C', 2, some .vileChase),
    ('D', 2, some .vileChase), ('D', 2, some .vileChase),
    ('E', 2, some .vileChase), ('E', 2, some .vileChase),
    ('F', 2, some .vileChase), ('F', 2, some .vileChase)] (some 2)
  -- vanilla S_VILE_ATK1–11: a zero-tic A_VileStart (the DSVILATK whoosh),
  -- ten tics facing, the flame planted, six more 8-tic facing beats, the
  -- blast, and a 20-tic follow-through
  let attack := chain 14 #[
    ('G', 0, some .vileStart),  ('G', 10, some .faceTarget),
    ('H', 8, some .vileTarget),
    ('I', 8, some .faceTarget), ('J', 8, some .faceTarget),
    ('K', 8, some .faceTarget), ('L', 8, some .faceTarget),
    ('M', 8, some .faceTarget), ('N', 8, some .faceTarget),
    ('O', 8, some .vileAttack), ('P', 20, none)] (some 2) (bright := true)
  let pain := chain 25 #[('Q', 5, none), ('Q', 5, some .pain)] (some 2)
  -- vanilla S_VILE_DIE: 7-tic frames through W, then X and Y quicken to 5
  let death := chain 27 #[
    ('Q', 7, none), ('R', 7, some .scream), ('S', 7, some .fall),
    ('T', 7, none), ('U', 7, none), ('V', 7, none), ('W', 7, none),
    ('X', 5, none), ('Y', 5, none), ('Z', -1, none)] none
  -- vanilla S_VILE_HEAL1–3, entered by `A_VileChase` while a corpse rises:
  -- ten bright tics each on the bracket frames past 'Z' — Doom's frame
  -- letters simply continue up the ASCII table ('[', '\', ']')
  let heal := chain 37 #[('[', 10, none), ('\\', 10, none),
    (']', 10, none)] (some 2) (bright := true)
  { sprite := "VILE"
    states := look ++ run ++ attack ++ pain ++ death ++ heal
    seeState := some 2, missileState := some 14
    painState := some 25, deathState := some 27, healState := some 37
    health := 700, speed := 15, radius := 20, height := 56
    painChance := 10, mass := 500, solid := true, shootable := true
    countKill := true }

/-- Commander Keen: hangs from a rope (vanilla MF_SPAWNCEILING|MF_NOGRAVITY),
dies, and opens the tag-666 doors (MAP32). The death scream comes early, on
frame C — the long dangle that follows is silent. -/
def commanderKeen : ActorInfo :=
  let idle := chain 0 #[('A', -1, none)] (some 0)
  let pain := chain 1 #[('M', 4, none), ('M', 8, some .pain)] (some 0)
  let death := chain 3 #[
    ('A', 6, none), ('B', 6, none), ('C', 6, some .scream), ('D', 6, none),
    ('E', 6, none), ('F', 6, none), ('G', 6, none), ('H', 6, none),
    ('I', 6, none), ('J', 6, none), ('K', 6, some .keenDie),
    ('L', -1, none)] none
  { sprite := "KEEN"
    states := idle ++ pain ++ death
    painState := some 1, deathState := some 3
    health := 100, radius := 16, height := 72
    painChance := 256, mass := 10000000, solid := true, shootable := true
    noGravity := true, ceilingHang := true, countKill := true }

/-! ## The Icon of Sin (MAP30)

The wall-mounted brain you rocket to death, the hidden box that spits
monster-spawning cubes, the target spots the cubes fly to, and the cube
itself. -/

/-- The brain behind the wall: shootable, and its death ends the game. -/
def iconBrain : ActorInfo :=
  { sprite := "BBRN"
    states := #[{ frame := 'A', tics := -1 }]              -- 0: idle forever
      ++ chain 1 #[('B', 36, some .pain)] (some 0)         -- 1: pain, back to idle
      -- death (vanilla S_BRAIN_DIE1–4): the scream seeds the wall-wide
      -- cascade at once, then ~120 tics of throes before `brainDie` ends
      -- the level
      ++ chain 2 #[('A', 100, some .brainScream),
                   ('A', 10, none), ('A', 10, none),
                   ('A', -1, some .brainDie)] none
    painState := some 1, deathState := some 2
    health := 250, painChance := 255, radius := 16, height := 16
    -- vanilla mass 10000000: a rocket barely rocks it
    mass := 10000000, solid := true, shootable := true }

/-- The spitter: unseen, it wakes with a roar, then flings a cube at a target
on a slow cadence. The roar rides the second state because the spawn state's
action never runs (as for every actor). Like vanilla's boss shooter it is
hidden — `TNT1` is the null sprite (no lump), so the renderer never draws it. -/
def iconSpit : ActorInfo :=
  { sprite := "TNT1", height := 32
    states := #[
      { frame := 'A', tics := 10, next := some 1 },
      { frame := 'A', tics := 181, action := some .brainAwake, next := some 2 },
      -- vanilla S_BRAINEYE1: one cube every 150 tics, not the 181 of the
      -- opening roar
      { frame := 'A', tics := 150, action := some .brainSpit, next := some 2 }] }

/-- A target spot the cubes aim for (`MT_BOSSTARGET`): an invisible marker.
Vanilla spawns it in `S_NULL`; here the null sprite `TNT1` keeps it off-screen
(otherwise all 13 of MAP30's target spots would stand around as SS troopers). -/
def iconTarget : ActorInfo :=
  { sprite := "TNT1", height := 32, states := #[{ frame := 'A', tics := -1 }] }

/-- The flying spawn cube: drifts to its target, then births a monster.
Moved and detonated by `moveCube` in the simulation, not the missile path
(it must not explode on contact — it delivers a monster). -/
def spawnCube : ActorInfo :=
  { sprite := "BOSF"
    states := loopStates "ABCD" 3 (bright := true)
    -- `noGravity`: `moveCube` flies it on its own momentum; without this the
    -- grounded-tracking pass in `tickMobjs` would bleed gravity into the very
    -- `momZ` the cube steers by, and it would sag out of the sky mid-flight
    noGravity := true
    speed := 10, radius := 6, height := 32 }

/-- One puff of the Icon's death throes: a rocket-burst sprite that hangs in
the air, and whose second frame spawns the next puff (`A_BrainExplode`), so a
single `A_BrainScream` seeds a self-sustaining cascade across the wall. -/
def brainExplosion : ActorInfo :=
  -- vanilla spawns an MT_ROCKET and forces it into S_BRAINEXPLODE1, so the
  -- physique is the rocket's; only the state chain is its own
  { sprite := "MISL", noGravity := true, radius := 11, height := 8
    -- vanilla S_BRAINEXPLODE3: the *last* frame seeds the next puff
    states := chain 0 #[('B', 10, none), ('C', 10, none),
                        ('D', 10, some .brainExplode)] none (bright := true) }

/-- Barrel explosion frames come from the BEXP sprite. -/
def barrelDeathSprite : String := "BEXP"

def barrel : ActorInfo :=
  { sprite := "BAR1"
    states := loopStates "AB" 6
      ++ chain 2 #[('A', 5, none), ('B', 5, some .scream), ('C', 5, none),
                   ('D', 10, some .explode), ('E', 10, none)] none true
           (spriteOverride := some barrelDeathSprite)
    deathState := some 2
    health := 20, radius := 10, height := 42
    solid := true, shootable := true, noBlood := true }

private def fireball (sprite : String) (speed : Float) (dmg : Nat) : ActorInfo :=
  -- vanilla missile damage is the mobj's `damage` times one d8, uniform;
  -- every vanilla missile carries MF_NOGRAVITY with MF_MISSILE
  { sprite
    states := loopStates "AB" 4 (bright := true)
      ++ chain 2 #[('C', 6, none), ('D', 6, none), ('E', 6, none)] none true
    deathState := some 2
    speed, radius := 6, height := 8
    missile := true, noGravity := true, damageDice := (1, 8), damageMult := dmg }

/-- The mancubus fireball flies on MANF's two frames but bursts as a rocket
explosion — vanilla S_FATSHOTX1–3 are MISL frames, on the rocket's 8/6/4
cadence. -/
def fatShot : ActorInfo :=
  { sprite := "MANF"
    states := loopStates "AB" 4 (bright := true)
      ++ chain 2 #[('B', 8, none), ('C', 6, none), ('D', 4, none)] none true
           (spriteOverride := some "MISL")
    deathState := some 2
    speed := 20, radius := 6, height := 8
    missile := true, noGravity := true, damageDice := (1, 8), damageMult := 8 }

/-- The arachnotron's plasma and the revenant's rocket both burst into a
*different* sprite family, so their death chains override the sprite the
way the barrel's does. -/
def arachPlasma : ActorInfo :=
  { sprite := "APLS"
    states := loopStates "AB" 5 (bright := true)
      ++ chain 2 #[('A', 5, none), ('B', 5, none), ('C', 5, none),
                   ('D', 5, none), ('E', 5, none)] none true
           (spriteOverride := some "APBX")
    deathState := some 2
    speed := 25, radius := 13, height := 8
    missile := true, noGravity := true, damageDice := (1, 8), damageMult := 5 }

/-- The revenant's homing rocket. It steers toward its quarry through
`A_Tracer` (`GameState.traceMissile`, dispatched from `tickMobjs`), which is
what lets it be shaken off by cutting hard across its update beat. -/
def revenantMissile : ActorInfo :=
  { sprite := "FATB"
    states := loopStates "AB" 2 (bright := true)
      ++ chain 2 #[('A', 8, none), ('B', 6, none), ('C', 4, none)] none true
           (spriteOverride := some "FBXP")
    deathState := some 2
    speed := 10, radius := 11, height := 8
    missile := true, noGravity := true, damageDice := (1, 8), damageMult := 10 }

def impBall : ActorInfo := fireball "BAL1" 10 3
def cacoBall : ActorInfo := fireball "BAL2" 10 5
def baronBall : ActorInfo := fireball "BAL7" 15 8

/-- The player's rocket: flies straight, then bursts with radius damage. -/
def rocket : ActorInfo :=
  { sprite := "MISL"
    states := loopStates "A" 1 (bright := true)
      ++ chain 1 #[('B', 8, none), ('C', 6, none), ('D', 4, none)] none true
    deathState := some 1
    speed := 20, radius := 11, height := 8
    missile := true, noGravity := true
    damageDice := (1, 8), damageMult := 20, blastRadius := 128 }

/-- Plasma bolt: fast, cheap, direct hit. -/
def plasmaBall : ActorInfo :=
  { sprite := "PLSS"
    states := loopStates "AB" 6 (bright := true)
      ++ chain 2 #[('A', 4, none), ('B', 4, none), ('C', 4, none),
                   ('D', 4, none), ('E', 4, none)] none true
           (spriteOverride := some "PLSE")
    deathState := some 2
    speed := 25, radius := 13, height := 8
    missile := true, noGravity := true, damageDice := (1, 8), damageMult := 5 }

/-- The green flare the BFG's spray leaves on each thing it catches
(vanilla `MT_EXTRABFG`). Purely visual — the damage is dealt by
`bfgSpray` — but without it the spray is invisible and the weapon reads
as though it only landed its direct hit. -/
def bfgPuff : ActorInfo :=
  { sprite := "BFE2", noGravity := true
    states := chain 0 #[('A', 8, none), ('B', 8, none), ('C', 8, none),
                        ('D', 8, none)] none true
    radius := 20, height := 16 }

/-- The BFG ball: slow, then detonates and sprays the room. The burst runs
all six BFE1 frames, and — as in vanilla (S_BFGLAND3) — the spray rakes the
room only on the *third*, 16 tics after impact. That delay is the weapon's
counterplay: break the spray's line of sight while the ball is bursting and
the tracers fizzle. -/
def bfgBall : ActorInfo :=
  { sprite := "BFS1"
    states := loopStates "AB" 4 (bright := true)
      ++ chain 2 #[('A', 8, none), ('B', 8, none), ('C', 8, some .bfgSpray),
                   ('D', 8, none), ('E', 8, none), ('F', 8, none)] none true
           (spriteOverride := some "BFE1")
    deathState := some 2
    speed := 25, radius := 13, height := 8
    missile := true, noGravity := true
    damageDice := (1, 8), damageMult := 100 }

/-- The green haze of a teleporter firing. -/
def teleFog : ActorInfo :=
  { sprite := "TFOG", noGravity := true
    states := chain 0 #[('A', 6, none), ('B', 6, none), ('A', 6, none),
      ('B', 6, none), ('C', 6, none), ('D', 6, none), ('E', 6, none),
      ('F', 6, none), ('G', 6, none), ('H', 6, none), ('I', 6, none),
      ('J', 6, none)] none (bright := true) }

def puff : ActorInfo :=
  -- only the spark frame glows (vanilla S_PUFF1); the smoke B–D is unlit
  { sprite := "PUFF", noGravity := true
    states := litAt (chain 0 #[('A', 4, none), ('B', 4, none), ('C', 4, none),
                               ('D', 4, none)] none) [0] }

def blood : ActorInfo :=
  -- blood has weight (no `MF_NOGRAVITY`): it spurts and falls, unlike the puff
  { sprite := "BLUD"
    states := chain 0 #[('C', 8, none), ('B', 8, none), ('A', 8, none)] none }

/-- Vanilla's "fast monsters", switched on for Nightmare in `G_InitNew`.

It is a far smaller change than the name suggests: nothing gains a higher
`speed` stat. The demon and spectre have their tics halved across one
contiguous run of states — vanilla's `for (i = S_SARG_RUN1; i <= S_SARG_PAIN2;
i++)`, which sweeps up the run frames, the **attack** frames and the pain
frames alike — so they shamble, lunge and flinch at double rate. And the
three slow fireballs (the imp's and cacodemon's at 10, the baron's and
knight's at 15) all fly at 20. Nothing else is touched. -/
def fast (kind : ActorKind) (info : ActorInfo) : ActorInfo :=
  match kind with
  | .demon | .spectre =>
    -- The `monster` layout puts look at 0–1, the run states at `seeState`,
    -- then the attack states, then the two pain states — so `seeState`
    -- through `painState + 1` is exactly vanilla's RUN1…PAIN2 span.
    match info.seeState, info.painState with
    | some runFrom, some painFrom =>
      let states := Id.run do
        let mut out := info.states
        for i in [runFrom : min out.size (painFrom + 2)] do
          let s := out[i]!
          out := out.set! i
            { s with tics := if s.tics > 1 then s.tics / 2 else s.tics }
        return out
      { info with states }
    | _, _ => info
  | .impBall | .cacoBall | .baronBall => { info with speed := 20 }
  | _ => info

def ofKind : ActorKind → ActorInfo
  | .zombieman => zombieman
  | .shotgunGuy => shotgunGuy
  | .imp => imp
  | .demon => demon
  | .spectre => spectre
  | .cacodemon => cacodemon
  | .lostSoul => lostSoul
  | .baron => baron
  | .cyberdemon => cyberdemon
  | .spiderMastermind => spiderMastermind
  | .chaingunner => chaingunner
  | .wolfSS => wolfSS
  | .hellKnight => hellKnight
  | .mancubus => mancubus
  | .arachnotron => arachnotron
  | .revenant => revenant
  | .painElemental => painElemental
  | .archVile => archVile
  | .commanderKeen => commanderKeen
  | .vileFire => vileFire
  | .iconBrain => iconBrain
  | .iconSpit => iconSpit
  | .iconTarget => iconTarget
  | .spawnCube => spawnCube
  | .brainExplosion => brainExplosion
  | .fatShot => fatShot
  | .arachPlasma => arachPlasma
  | .revenantMissile => revenantMissile
  | .barrel => barrel
  | .impBall => impBall
  | .cacoBall => cacoBall
  | .baronBall => baronBall
  | .rocket => rocket
  | .plasmaBall => plasmaBall
  | .bfgBall => bfgBall
  | .bfgPuff => bfgPuff
  | .teleFog => teleFog
  | .puff => puff
  | .blood => blood
  | .item sprite frames =>
    { sprite, states := loopStates frames 6, pickup := true }
  | .scenery sprite frames solid =>
    -- the hanging gore and torsos (GOR*, HDB*) dangle from the ceiling;
    -- everything else stands on the floor
    let hang := sprite.startsWith "GOR" || sprite.startsWith "HDB"
    { sprite, states := loopStates frames 6, solid, ceilingHang := hang
      radius := 16, height := if hang then 64 else 54 }

end ActorInfo

/-- Vanilla `MF_COUNTITEM`: only the artifacts count toward the intermission
item percentage — the bonuses, the spheres and the powerups. Health kits,
armour, ammo, weapons, keys and the radiation suit are picked up but never
tallied. -/
def ActorKind.countItem : ActorKind → Bool
  | .item f _ =>
    f == "BON1" || f == "BON2" || f == "SOUL" || f == "MEGA"
      || f == "PINV" || f == "PSTR" || f == "PINS" || f == "PMAP"
      || f == "PVIS"
  | _ => false

/-- Map thing type → actor kind, for everything we spawn. -/
def ActorKind.ofThingType : Nat → Option ActorKind
  -- monsters
  | 3004 => some .zombieman
  | 9    => some .shotgunGuy
  | 3001 => some .imp
  | 3002 => some .demon
  | 58   => some .spectre
  | 3005 => some .cacodemon
  | 3006 => some .lostSoul
  | 3003 => some .baron
  | 16   => some .cyberdemon
  | 7    => some .spiderMastermind
  -- Doom II
  | 65   => some .chaingunner
  | 66   => some .revenant
  | 67   => some .mancubus
  | 68   => some .arachnotron
  | 69   => some .hellKnight
  | 84   => some .wolfSS
  | 71   => some .painElemental
  | 64   => some .archVile
  | 72   => some .commanderKeen
  | 88   => some .iconBrain
  | 89   => some .iconSpit
  | 87   => some .iconTarget
  | 82   => some (.item "SGN2" "A")   -- super shotgun
  | 2035 => some .barrel
  -- weapons
  | 2001 => some (.item "SHOT" "A") | 2002 => some (.item "MGUN" "A")
  | 2003 => some (.item "LAUN" "A") | 2004 => some (.item "PLAS" "A")
  | 2005 => some (.item "CSAW" "A") | 2006 => some (.item "BFUG" "A")
  -- ammo
  | 2007 => some (.item "CLIP" "A") | 2008 => some (.item "SHEL" "A")
  | 2048 => some (.item "AMMO" "A") | 2049 => some (.item "SBOX" "A")
  | 2010 => some (.item "ROCK" "A") | 2046 => some (.item "BROK" "A")
  | 2047 => some (.item "CELL" "A") | 17   => some (.item "CELP" "A")
  | 8    => some (.item "BPAK" "A")
  | 2011 => some (.item "STIM" "A") | 2012 => some (.item "MEDI" "A")
  | 2014 => some (.item "BON1" "ABCDCB") | 2015 => some (.item "BON2" "ABCDCB")
  | 2018 => some (.item "ARM1" "AB") | 2019 => some (.item "ARM2" "AB")
  | 2013 => some (.item "SOUL" "ABCDCB")
  -- timed powerups and specials
  | 2022 => some (.item "PINV" "ABCD")   -- invulnerability
  | 2023 => some (.item "PSTR" "A")      -- berserk pack
  | 2024 => some (.item "PINS" "ABCD")   -- blur / partial invisibility
  | 2025 => some (.item "SUIT" "A")      -- radiation suit
  | 2026 => some (.item "PMAP" "ABCDCB") -- computer area map
  | 2045 => some (.item "PVIS" "AB")     -- light-amplification goggles
  | 83   => some (.item "MEGA" "ABCD")   -- megasphere
  | 5    => some (.item "BKEY" "AB") | 6 => some (.item "YKEY" "AB")
  | 13   => some (.item "RKEY" "AB")
  -- the skull keys, equivalent to the cards of the same colour
  | 40   => some (.item "BSKU" "AB") | 39 => some (.item "YSKU" "AB")
  | 38   => some (.item "RSKU" "AB")
  -- dead things left as scenery
  | 18 => some (.scenery "POSS" "L" false)
  | 19 => some (.scenery "SPOS" "L" false)
  | 20 => some (.scenery "TROO" "M" false)
  | 21 => some (.scenery "SARG" "N" false)
  | 22 => some (.scenery "HEAD" "L" false)
  | 23 => some (.scenery "SKUL" "K" false)
  -- Doom II's gibs, lamps and hanging torsos
  | 79 => some (.scenery "POB1" "A" false)
  | 80 => some (.scenery "POB2" "A" false)
  | 81 => some (.scenery "BRS1" "A" false)
  | 85 => some (.scenery "TLMP" "ABCD" true)
  | 86 => some (.scenery "TLP2" "ABCD" true)
  | 73 => some (.scenery "HDB1" "A" true)
  | 74 => some (.scenery "HDB2" "A" true)
  | 75 => some (.scenery "HDB3" "A" true)
  | 76 => some (.scenery "HDB4" "A" true)
  | 77 => some (.scenery "HDB5" "A" true)
  | 78 => some (.scenery "HDB6" "A" true)
  -- decorations (E1-relevant)
  | 10 | 12 => some (.scenery "PLAY" "W" false)
  | 15 => some (.scenery "PLAY" "N" false)
  | 24 => some (.scenery "POL5" "A" false)
  | 34 => some (.scenery "CAND" "A" false)
  | 35 => some (.scenery "CBRA" "A" true)
  | 2028 => some (.scenery "COLU" "A" true)
  | 30 => some (.scenery "COL1" "A" true) | 31 => some (.scenery "COL2" "A" true)
  | 32 => some (.scenery "COL3" "A" true) | 33 => some (.scenery "COL4" "A" true)
  | 36 => some (.scenery "COL5" "AB" true) | 37 => some (.scenery "COL6" "A" true)
  | 41 => some (.scenery "CEYE" "ABCB" true)
  | 42 => some (.scenery "FSKU" "ABC" true)
  | 43 => some (.scenery "TRE1" "A" true) | 44 => some (.scenery "TBLU" "ABCD" true)
  | 45 => some (.scenery "TGRN" "ABCD" true) | 46 => some (.scenery "TRED" "ABCD" true)
  | 47 => some (.scenery "SMIT" "A" true) | 48 => some (.scenery "ELEC" "A" true)
  | 54 => some (.scenery "TRE2" "A" true)
  | 55 => some (.scenery "SMBT" "ABCD" true) | 56 => some (.scenery "SMGT" "ABCD" true)
  | 57 => some (.scenery "SMRT" "ABCD" true)
  | 70 => some (.scenery "FCAN" "ABC" true)
  -- gore on poles, the furniture of episodes 2-4
  | 25 => some (.scenery "POL1" "A" true)
  | 26 => some (.scenery "POL6" "AB" true)
  | 27 => some (.scenery "POL4" "A" true)
  | 28 => some (.scenery "POL2" "A" true)
  | 29 => some (.scenery "POL3" "AB" true)
  -- Hanging victims. Vanilla hangs these from the ceiling
  -- (MF_SPAWNCEILING); `ActorInfo.ofKind` gives every `GOR*`/`HDB*` family
  -- `ceilingHang`, so they dangle rather than stand. Each has a blocking and
  -- a non-blocking doomednum, and that distinction is honored too.
  | 49 => some (.scenery "GOR1" "ABCB" true)
  | 50 => some (.scenery "GOR2" "A" true)
  | 51 => some (.scenery "GOR3" "A" true)
  | 52 => some (.scenery "GOR4" "A" true)
  | 53 => some (.scenery "GOR5" "A" true)
  | 59 => some (.scenery "GOR2" "A" false)
  | 60 => some (.scenery "GOR4" "A" false)
  | 61 => some (.scenery "GOR3" "A" false)
  | 62 => some (.scenery "GOR5" "A" false)
  | 63 => some (.scenery "GOR1" "ABCB" false)
  | _ => none

end Dill
