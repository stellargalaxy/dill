import Dill
import VanillaData
import TestSupport

/-!
# Vanilla-fidelity golden tests

Diffs DILL's tables — `ActorInfo.ofKind`, `Weapon.attack`, `animGroups`,
`switchTwin`, `Speeds` — against `VanillaData`, the mechanical transcription
of linuxdoom-1.10's info.c / d_items.c / p_spec.c / p_switch.c. Pure data on
both sides: no WAD is opened.

Every tolerated divergence lives in the single `intentional` allowlist
below, each entry with its justification; anything else fails the run.

One nuance stays inline: `A_Saw` reaches `MELEERANGE + 1`, and vanilla's
`+1` is one fixed-point *epsilon* (1/65536 unit), written here as
`Player.meleeRange + 1` — a whole map unit, a documented approximation.
-/

open Dill

-- `VanillaData` derives no `Inhabited` (it never indexes itself); the `[i]!`
-- walks below need one.
instance : Inhabited VanillaData.S := ⟨⟨"", 'A', 0, "", false⟩⟩
instance : Inhabited VanillaData.Chain := ⟨⟨"", #[], .halt⟩⟩

/-! ## The allowlist -/

/-- Every divergence from vanilla this suite tolerates. A divergence not
described here is a failure. -/
structure Intentional where
  /-- (dillKind or `"*"`, vanilla action name, the action DILL's table
  carries instead). -/
  actionSwaps : List (String × String × Option Action)
  /-- Kinds whose vanilla `raisestate` chain DILL replaces by rewinding the
  death chain frame-by-frame (`tickMobjs`' `raising` walk): same frames,
  reversed in place, so no separate chain exists to diff. -/
  raiseRewinds : List String
  /-- Actors whose whole chain topology diverges by design; each gets a
  bespoke check in `main` instead of the generic walker. -/
  customActors : List String
  /-- (dillKind, sound slot) pairs where DILL plays vanilla's sound from a
  different place than the mobjinfo table (or knowingly not at all). -/
  soundSwaps : List (String × String)
  /-- (dillKind, flag) pairs where DILL's flag model folds vanilla's. -/
  flagSwaps : List (String × String)
  /-- (dillKind, field) pairs for numeric mobjinfo fields DILL knowingly
  leaves unmodeled for that actor. -/
  fieldSwaps : List (String × String)
  /-- Weapons whose attack sequence DILL restructures; the recorded
  (frame, tics, fire, refire) sequence is asserted verbatim, plus the
  total-tics equality with vanilla. -/
  weaponReshapes : List (String × Array (Char × Nat × Bool × Bool))
  /-- Weapons whose multi-frame vanilla flash DILL folds to the recorded
  frame list (the `GunState.flash` overlay is one frame per gun state). -/
  flashFolds : List (String × List Char)
  /-- Vanilla weapon-state actions realized by engine machinery rather than
  a table mark (see the comments at the definition). -/
  weaponActionFolds : List String
  /-- DILL's `.item`/`.scenery` kinds carry only a frame string: per-frame
  tics and bright bits are folded to a uniform 6-tic unlit loop.
  TODO: extend the item/scenery model with tics + bright if the flicker
  cadence of torches (4-tic in vanilla) ever matters. -/
  thingAnimFold : Bool
  /-- Scenery radius/height folded to 16 × (54 | 64-hanging); vanilla varies
  (e.g. the big tree's radius 32, hanging heights 52–88). Items are still
  checked strictly. TODO as above if collision fidelity ever matters. -/
  sceneryPhysiqueFold : Bool
  /-- Doomednums whose vanilla spawn chain *removes* the thing (thing 23,
  the dead lost soul, deletes itself after 6 tics — invisible in vanilla).
  DILL keeps the decoration visible instead. -/
  thingChainNotes : List Nat
  /-- SWATER1–4: named by vanilla's animdefs but absent from both retail
  IWADs; `P_InitPicAnims` skips the entry, and DILL's `animName` passes
  unknown names through untouched, so omitting the group is equivalent. -/
  swaterSkipped : Bool
  /-- `P_InitSwitchList`'s shareware/registered/commercial gates are not
  modeled: DILL's prefix rule flips any SW1/SW2 pair. A WAD lacking the
  texture simply never shows it, so the superset is harmless. -/
  episodeGatesUnmodeled : Bool

def intentional : Intentional := {
  actionSwaps := [
    -- A_BossDeath is dispatched from the kill path in `Dill.Game.Combat`
    -- (map-gated specials), not as a state action.
    ("*", "A_BossDeath", none),
    -- The rocket's blast is `blastRadius := 128`, applied on impact by the
    -- missile-explosion path, not by a death-frame action.
    ("rocket", "A_Explode", none),
    -- A_BrainPain is "play DSBOSPN"; DILL's generic `pain` action plays the
    -- brain's `painSfx`, which is DSBOSPN — same sound, shared mechanism.
    ("iconBrain", "A_BrainPain", some .pain),
    -- Homing is a simulation-side step (`traceMissile`, on vanilla's own
    -- gametic&3 beat), not a state action.
    ("revenantMissile", "A_Tracer", none),
    -- The cube's flight, hum and hatch are driven by `moveCube`; it must
    -- not ride the generic missile path (it delivers a monster).
    ("spawnCube", "A_SpawnSound", none),
    ("spawnCube", "A_SpawnFly", none)]
  raiseRewinds := [
    "zombieman", "shotgunGuy", "imp", "demon", "spectre", "cacodemon",
    "baron", "chaingunner", "wolfSS", "hellKnight", "mancubus",
    "arachnotron", "revenant", "painElemental"]
  customActors := ["iconSpit", "iconTarget"]
  soundSwaps := [
    -- the spit cue is played by `aBrainSpit` at launch (same DSBOSPIT).
    ("spawnCube", "seeSound"),
    -- the cube never detonates as a missile (moveCube removes it), so its
    -- vanilla deathsound (DSFIRXPL) has nowhere to play.
    ("spawnCube", "deathSound"),
    -- the launch cue (DSSKEATK) is played by `aSkelMissile`, exactly where
    -- vanilla's A_SkelMissile-spawned seesound comes from.
    ("revenantMissile", "seeSound"),
    -- vanilla physique is MT_ROCKET's, sounds included; the cascade puff is
    -- spawned straight into its display chain and never launches/explodes.
    ("brainExplosion", "seeSound"),
    ("brainExplosion", "deathSound"),
    -- vanilla's brain deathsound (DSBOSDTH) is unused there too — the death
    -- chain has no A_Scream; both engines roar via A_BrainScream instead.
    ("iconBrain", "deathSound")]
  flagSwaps := [
    -- DILL folds MF_NOGRAVITY into `flying` for live floaters; vanilla's
    -- P_KillMobj strips NOGRAVITY from every dying non-skull, so only the
    -- lost soul (whose corpse keeps it) carries explicit `noGravity`.
    ("cacodemon", "noGravity"),
    ("painElemental", "noGravity"),
    -- see the A_SpawnFly note: the cube is deliberately not a missile.
    ("spawnCube", "missile"),
    -- rocket physique on a thing that never flies (see soundSwaps).
    ("brainExplosion", "missile")]
  fieldSwaps := [
    -- contact damage of things that never deal it through the missile path
    ("spawnCube", "damage"),        -- delivered by moveCube, no impacts
    ("brainExplosion", "damage"),   -- inherited rocket damage, never flies
    ("brainExplosion", "speed")]    -- inherited rocket speed, never flies
  weaponReshapes := [
    -- Vanilla's trailing 0-tic A_ReFire (S_SAW3) is folded onto the second
    -- cut; `tickWeapon` resolves a refire mark the instant it is reached,
    -- so the timing is identical.
    ("chainsaw", #[('A', 4, true, false), ('B', 4, true, true)]),
    -- MISG has only frames A/B, so vanilla runs a *separate* 4-frame MISF
    -- flash psprite over an 8+12-tic gun hold. DILL's one-overlay-per-state
    -- model steps the gun through six B states carrying the flash frames
    -- A→D; same 20-tic total, rocket away at t = 8 exactly as vanilla's
    -- S_MISSILE2 entry action.
    ("missile launcher", #[('B', 3, false, false), ('B', 4, false, false),
                           ('B', 1, false, false), ('B', 7, true, false),
                           ('B', 5, false, false), ('B', 0, false, true)])]
  flashFolds := [
    -- one overlay per gun state: the shot state carries the first (and
    -- brightest) flash frame; the short second frame is dropped.
    ("shotgun", ['A']),
    ("super shotgun", ['I']),
    ("bfg 9000", ['A']),
    -- vanilla's second chaingun shot indexes one past the single CHGF
    -- flash state and lands on S_LIGHTDONE — an off-by-one that shows *no*
    -- flash. DILL shows CHGF B, the frame the WAD ships for exactly this.
    ("chaingun", ['A', 'B'])]
  weaponActionFolds := [
    -- lighting: DILL has no sector-light psprite hooks; muzzle light is
    -- the fullbright flash overlay itself
    "A_Light0", "A_Light1", "A_Light2",
    -- the flash psprite start: DILL's per-state `flash` mark
    "A_GunFlash",
    -- played by `tickWeapon`/`fireWeapon` at the trigger pull, 30 tics
    -- before the ball leaves, exactly as vanilla times it
    "A_BFGsound",
    -- super-shotgun reload choreography (sounds + ammo re-check); the
    -- frames and tics are kept, the DSDBOPN/DSDBLOAD/DSDBCLS cues are not.
    -- TODO: add the three lumps to the Sfx catalog if wanted.
    "A_CheckReload", "A_OpenShotgun2", "A_LoadShotgun2", "A_CloseShotgun2"]
  thingAnimFold := true
  sceneryPhysiqueFold := true
  thingChainNotes := [23]
  swaterSkipped := true
  episodeGatesUnmodeled := true }

/-! ## Shared lookup tables -/

/-- Vanilla action name → the DILL action a faithful table carries. -/
def mapAction : String → Option (Option Action)
  | "" => some none
  | "A_Look" => some (some .look)
  | "A_Chase" => some (some .chase)
  | "A_FaceTarget" => some (some .faceTarget)
  | "A_PosAttack" => some (some .posAttack)
  | "A_SPosAttack" => some (some .sposAttack)
  | "A_TroopAttack" => some (some .trooAttack)
  | "A_SargAttack" => some (some .sargAttack)
  | "A_HeadAttack" => some (some .headAttack)
  | "A_BruisAttack" => some (some .bruisAttack)
  | "A_SkullAttack" => some (some .skullAttack)
  | "A_CyberAttack" => some (some .cyberAttack)
  | "A_SpidRefire" => some (some .spidRefire)
  | "A_CPosAttack" => some (some .cposAttack)
  | "A_CPosRefire" => some (some .cposRefire)
  | "A_StartFire" => some (some .fireStart)
  | "A_FireCrackle" => some (some .fireCrackle)
  | "A_FatRaise" => some (some .fatRaise)
  | "A_FatAttack1" => some (some .fatAttack)
  | "A_FatAttack2" => some (some .fatAttack2)
  | "A_FatAttack3" => some (some .fatAttack3)
  | "A_BspiAttack" => some (some .bspiAttack)
  | "A_SkelWhoosh" => some (some .skelWhoosh)
  | "A_SkelFist" => some (some .skelFist)
  | "A_SkelMissile" => some (some .skelMissile)
  | "A_PainAttack" => some (some .painAttack)
  | "A_PainDie" => some (some .painDie)
  | "A_BrainAwake" => some (some .brainAwake)
  | "A_BrainSpit" => some (some .brainSpit)
  | "A_BrainScream" => some (some .brainScream)
  | "A_BrainExplode" => some (some .brainExplode)
  | "A_BrainDie" => some (some .brainDie)
  | "A_VileChase" => some (some .vileChase)
  | "A_VileStart" => some (some .vileStart)
  | "A_VileTarget" => some (some .vileTarget)
  | "A_VileAttack" => some (some .vileAttack)
  | "A_Fire" => some (some .fire)
  | "A_BFGSpray" => some (some .bfgSpray)
  | "A_KeenDie" => some (some .keenDie)
  | "A_Hoof" => some (some .hoof)
  | "A_Metal" => some (some .metal)
  | "A_BabyMetal" => some (some .babyMetal)
  | "A_Pain" => some (some .pain)
  | "A_Scream" => some (some .scream)
  | "A_XScream" => some (some .xscream)
  | "A_Fall" => some (some .fall)
  | "A_Explode" => some (some .explode)
  | _ => none

/-- The expected DILL action for one vanilla state, allowlist applied. -/
def expectedAction (kind : String) (van : String) : Option (Option Action) :=
  match intentional.actionSwaps.find? fun e =>
      (e.1 == kind || e.1 == "*") && e.2.1 == van with
  | some e => some e.2.2
  | none => mapAction van

/-- Vanilla chain entry → DILL `ActorInfo` entry index. -/
def entryIdx (info : ActorInfo) : String → Option Nat
  | "spawn" => some info.spawnState
  | "see" => info.seeState
  | "pain" => info.painState
  | "melee" => info.meleeState
  | "missile" => info.missileState
  | "death" => info.deathState
  | "xdeath" => info.xdeathState
  | "heal" => info.healState
  | _ => none

/-- One `ActorKind` per `VanillaData.mobjs` row, in the same order. -/
def kindTable : List (ActorKind × VanillaData.Mobj) :=
  ([.zombieman, .shotgunGuy, .imp, .demon, .spectre,
    .cacodemon, .lostSoul, .baron, .cyberdemon, .spiderMastermind,
    .chaingunner, .wolfSS, .hellKnight, .mancubus, .arachnotron, .revenant,
    .painElemental, .archVile, .commanderKeen, .vileFire,
    .iconBrain, .iconSpit, .iconTarget, .spawnCube, .brainExplosion,
    .barrel, .impBall, .cacoBall, .baronBall, .puff, .blood, .teleFog,
    .rocket, .plasmaBall, .bfgBall, .bfgPuff,
    .fatShot, .arachPlasma, .revenantMissile] : List ActorKind).zip
    VanillaData.mobjs.toList

/-- DS lump name (vanilla lowercase spelling) of a DILL sound; `""` = none. -/
def sfxName : Option Sfx → String
  | some s => (Sfx.lumps[s]!).toLower
  | none => ""

/-- Where DILL plays the sound vanilla reads from `info->attacksound`.
Vanilla itself plays it from inside the attack actions, so DILL keeps it in
the action code (`Dill.Game.Enemy`); this table records which sound that is
so the diff still covers the slot. -/
def dillAttackSfx : ActorKind → Option Sfx
  | .zombieman => some Sfx.pistol         -- aPosAttack
  | .demon | .spectre => some Sfx.sargAtk -- aSargAttack
  | .spiderMastermind => some Sfx.shotgun -- aSPosAttack
  | .lostSoul => some Sfx.sklAttack       -- aSkullAttack
  | _ => none

/-! ## mobjinfo comparisons -/

def fieldErrors (name : String) (info : ActorInfo) (v : VanillaData.Mobj) :
    List String := Id.run do
  let allowed := fun (f : String) => intentional.fieldSwaps.contains (name, f)
  let mut errs : List String := []
  let chk := fun (errs : List String) (f : String) (ok : Bool) (msg : String) =>
    if ok || allowed f then errs else errs ++ [s!"{name}: {f} {msg}"]
  errs := chk errs "health" (info.health == v.health)
    s!"{info.health} ≠ {v.health}"
  errs := chk errs "speed" (info.speed == v.speed)
    s!"{info.speed} ≠ {v.speed}"
  errs := chk errs "radius" (info.radius == v.radius)
    s!"{info.radius} ≠ {v.radius}"
  errs := chk errs "height" (info.height == v.height)
    s!"{info.height} ≠ {v.height}"
  errs := chk errs "mass" (info.mass == Float.ofInt v.mass)
    s!"{info.mass} ≠ {v.mass}"
  errs := chk errs "painChance" (info.painChance == v.painChance)
    s!"{info.painChance} ≠ {v.painChance}"
  errs := chk errs "reactionTime" (info.reactionTime == v.reactionTime)
    s!"{info.reactionTime} ≠ {v.reactionTime}"
  -- vanilla missile damage is (P_Random()%8 + 1) * damage: one d8 scaled
  if v.damage > 0 then
    errs := chk errs "damage"
      (info.damageDice == (1, 8) && info.damageMult == v.damage)
      s!"dice {info.damageDice} × {info.damageMult} ≠ 1d8 × {v.damage}"
  return errs

def flagErrors (name : String) (info : ActorInfo) (v : VanillaData.Mobj) :
    List String := Id.run do
  let allowed := fun (f : String) => intentional.flagSwaps.contains (name, f)
  let mut errs : List String := []
  let pairs : List (String × Bool × Bool) := [
    ("solid", info.solid, v.solid), ("shootable", info.shootable, v.shootable),
    ("pickup/MF_SPECIAL", info.pickup, v.special),
    ("missile", info.missile, v.missile), ("noBlood", info.noBlood, v.noBlood),
    ("flying/MF_FLOAT", info.flying, v.float),
    ("noGravity", info.noGravity, v.noGravity),
    ("shadow", info.shadow, v.shadow), ("countKill", info.countKill, v.countKill),
    ("ceilingHang/MF_SPAWNCEILING", info.ceilingHang, v.spawnCeiling)]
  for (f, d, van) in pairs do
    unless d == van || allowed f do
      errs := errs ++ [s!"{name}: {f} = {d}, vanilla {van}"]
  return errs

def soundErrors (name : String) (k : ActorKind) (v : VanillaData.Mobj) :
    List String := Id.run do
  let allowed := fun (f : String) => intentional.soundSwaps.contains (name, f)
  let mut errs : List String := []
  -- monsters cry their seesound from A_Look; missiles from the launch
  let dillSee := k.sightSfx.orElse fun _ => k.launchSfx
  let pairs : List (String × String × String) := [
    ("seeSound", sfxName dillSee, v.seeSound),
    ("attackSound", sfxName (dillAttackSfx k), v.attackSound),
    ("painSound", sfxName k.painSfx, v.painSound),
    ("deathSound", sfxName k.deathSfx, v.deathSound),
    ("activeSound", sfxName k.activeSfx, v.activeSound)]
  for (f, d, van) in pairs do
    unless d == van || allowed f do
      errs := errs ++ [s!"{name}: {f} = \"{d}\", vanilla \"{van}\""]
  return errs

/-- Walk DILL's states from the entry that corresponds to one vanilla chain,
comparing step-by-step, then the terminator. -/
def walkChain (name : String) (info : ActorInfo) (c : VanillaData.Chain) :
    List String := Id.run do
  let mut errs : List String := []
  let tag := s!"{name}/{c.entry}"
  let some start := entryIdx info c.entry
    | return [s!"{tag}: DILL has no such entry point"]
  if c.states.isEmpty then
    -- vanilla aliases this entry into another chain (imp missile → melee)
    match c.ending with
    | .loops tgt off =>
      match entryIdx info tgt with
      | some t =>
        unless start == t + off do
          errs := errs ++ [s!"{tag}: aliased entry {start} ≠ {tgt}+{off}"]
      | none => errs := errs ++ [s!"{tag}: alias target {tgt} missing"]
    | _ => errs := errs ++ [s!"{tag}: empty chain with a non-loop ending"]
    return errs
  let mut cur := start
  for i in [0:c.states.size] do
    let vs := c.states[i]!
    if cur ≥ info.states.size then
      return errs ++ [s!"{tag} state {i}: index {cur} out of range"]
    let ds := info.states[cur]!
    let sprite := ds.spriteOverride.getD info.sprite
    unless sprite == vs.sprite do
      errs := errs ++ [s!"{tag} state {i}: sprite {sprite} ≠ {vs.sprite}"]
    unless ds.frame == vs.frame do
      errs := errs ++ [s!"{tag} state {i}: frame {ds.frame} ≠ {vs.frame}"]
    unless ds.tics == vs.tics do
      errs := errs ++ [s!"{tag} state {i}: tics {ds.tics} ≠ {vs.tics}"]
    unless ds.bright == vs.bright do
      errs := errs ++ [s!"{tag} state {i}: bright {ds.bright} ≠ {vs.bright}"]
    match expectedAction name vs.action with
    | none => errs := errs ++ [s!"{tag} state {i}: unmapped action {vs.action}"]
    | some want =>
      unless ds.action == want do
        errs := errs ++
          [s!"{tag} state {i}: action {repr ds.action} ≠ {repr want} ({vs.action})"]
    if i + 1 < c.states.size then
      match ds.next with
      | some nxt => cur := nxt
      | none => return errs ++ [s!"{tag}: chain ends after state {i}, vanilla has {c.states.size}"]
    else
      -- terminator / loop topology
      match c.ending with
      | .halt =>
        unless ds.tics < 0 do
          errs := errs ++ [s!"{tag}: should hold its last frame forever (tics -1)"]
      | .remove =>
        unless ds.tics ≥ 0 && ds.next.isNone do
          errs := errs ++ [s!"{tag}: should remove after its last frame"]
      | .loops tgt off =>
        match entryIdx info tgt with
        | some t =>
          unless ds.next == some (t + off) do
            errs := errs ++ [s!"{tag}: loops to {repr ds.next} ≠ {tgt}+{off} ({t + off})"]
        | none => errs := errs ++ [s!"{tag}: loop target {tgt} missing in DILL"]
  return errs

def chainErrors (name : String) (info : ActorInfo) (v : VanillaData.Mobj) :
    List String := Id.run do
  let mut errs : List String := []
  for c in v.chains do
    if c.entry == "raise" then
      -- DILL resurrects by rewinding the death chain (see `raiseRewinds`)
      unless intentional.raiseRewinds.contains name do
        errs := errs ++ [s!"{name}: vanilla raise chain with no allowlisted rewind"]
      unless info.deathState.isSome do
        errs := errs ++ [s!"{name}: raise rewind has no death chain to rewind"]
    else
      errs := errs ++ walkChain name info c
  -- entries DILL has that vanilla lacks would be inventions: refuse them
  let dillEntries : List (String × Option Nat) := [
    ("see", info.seeState), ("pain", info.painState),
    ("melee", info.meleeState), ("missile", info.missileState),
    ("death", info.deathState), ("xdeath", info.xdeathState),
    ("heal", info.healState)]
  for (e, idx) in dillEntries do
    if idx.isSome && !(v.chains.any (·.entry == e)) then
      errs := errs ++ [s!"{name}: DILL has a {e} entry vanilla lacks"]
  return errs

/-! ## Weapon comparisons -/

def weaponTable : List (Weapon × VanillaData.WeaponDef) :=
  ([.fist, .pistol, .shotgun, .chaingun, .rocket,
    .plasma, .bfg, .chainsaw, .superShotgun] : List Weapon).zip
    VanillaData.weaponinfo.toList

def ammoName : Option Ammo → String
  | none => "am_noammo"
  | some .bullets => "am_clip"
  | some .shells => "am_shell"
  | some .rockets => "am_misl"
  | some .cells => "am_cell"

/-- The vanilla actions that pull the trigger (DILL's `fire` mark). -/
def fireActions : List String :=
  ["A_Punch", "A_Saw", "A_FirePistol", "A_FireShotgun", "A_FireShotgun2",
   "A_FireCGun", "A_FireMissile", "A_FirePlasma", "A_FireBFG"]

def weaponAtkErrors (w : Weapon) (vw : VanillaData.WeaponDef) :
    List String := Id.run do
  let mut errs : List String := []
  let some atk := vw.chains.find? (·.entry == "atk")
    | return [s!"{vw.name}: vanilla atk chain missing"]
  match intentional.weaponReshapes.find? (·.1 == vw.name) with
  | some (_, expect) =>
    let actual := w.attack.map fun g => (g.frame, g.tics, g.fire, g.refire)
    unless actual == expect do
      errs := errs ++ [s!"{vw.name}: reshaped attack drifted from its recorded form"]
    let vTot := atk.states.foldl (fun a s => a + s.tics) 0
    let dTot := w.attack.foldl (fun a g => a + Int.ofNat g.tics) 0
    unless vTot == dTot do
      errs := errs ++ [s!"{vw.name}: total attack tics {dTot} ≠ vanilla {vTot}"]
  | none =>
    if w.attack.size != atk.states.size then
      return [s!"{vw.name}: {w.attack.size} attack states ≠ vanilla {atk.states.size}"]
    for i in [0:atk.states.size] do
      let vs := atk.states[i]!
      let gs := w.attack[i]!
      unless vs.sprite == w.sprite do
        errs := errs ++ [s!"{vw.name} state {i}: sprite {w.sprite} ≠ {vs.sprite}"]
      unless gs.frame == vs.frame do
        errs := errs ++ [s!"{vw.name} state {i}: frame {gs.frame} ≠ {vs.frame}"]
      unless Int.ofNat gs.tics == vs.tics do
        errs := errs ++ [s!"{vw.name} state {i}: tics {gs.tics} ≠ {vs.tics}"]
      unless gs.fire == fireActions.contains vs.action do
        errs := errs ++ [s!"{vw.name} state {i}: fire mark ≠ {vs.action}"]
      unless gs.refire == (vs.action == "A_ReFire") do
        errs := errs ++ [s!"{vw.name} state {i}: refire mark ≠ {vs.action}"]
      unless vs.action == "" || vs.action == "A_ReFire"
          || fireActions.contains vs.action
          || intentional.weaponActionFolds.contains vs.action do
        errs := errs ++ [s!"{vw.name} state {i}: unallowlisted action {vs.action}"]
  return errs

def weaponFlashErrors (w : Weapon) (vw : VanillaData.WeaponDef) :
    List String := Id.run do
  let dillFlash := w.attack.toList.filterMap (·.flash)
  let some fc := vw.chains.find? (·.entry == "flash")
    | return if dillFlash.isEmpty then []
      else [s!"{vw.name}: DILL flashes but vanilla has no flash chain"]
  let mut errs : List String := []
  -- the shared S_LIGHTDONE terminator rides the SHTG sprite: drop it
  let fs := fc.states[0]!.sprite
  let vFrames := (fc.states.filter (·.sprite == fs)).toList.map (·.frame)
  unless w.flashSprite == fs do
    errs := errs ++ [s!"{vw.name}: flash sprite {w.flashSprite} ≠ {fs}"]
  let expect := match intentional.flashFolds.find? (·.1 == vw.name) with
    | some (_, e) => e
    | none => vFrames
  unless dillFlash == expect do
    errs := errs ++ [s!"{vw.name}: flash frames {dillFlash} ≠ {expect}"]
  return errs

def weaponMetaErrors (w : Weapon) (vw : VanillaData.WeaponDef) :
    List String := Id.run do
  let mut errs : List String := []
  unless ammoName w.ammoType == vw.ammo do
    errs := errs ++ [s!"{vw.name}: ammo {ammoName w.ammoType} ≠ {vw.ammo}"]
  -- ammo per pull lives in code in vanilla: BFGCELLS 40 (p_pspr.c) and the
  -- super shotgun's 2 shells (A_FireShotgun2); everything else spends 1
  let cost := match vw.name with
    | "bfg 9000" => 40 | "super shotgun" => 2 | _ => 1
  unless w.ammoCost == cost do
    errs := errs ++ [s!"{vw.name}: ammoCost {w.ammoCost} ≠ {cost}"]
  if let some atk := vw.chains.find? (·.entry == "atk") then
    unless w.sprite == atk.states[0]!.sprite do
      errs := errs ++ [s!"{vw.name}: sprite {w.sprite} ≠ {atk.states[0]!.sprite}"]
  -- ready/idle: DILL folds the 1-tic up/down/ready loops into the weaponY
  -- machine; the frame shown while idle must still match the ready chain
  if let some ready := vw.chains.find? (·.entry == "ready") then
    if vw.name == "chainsaw" then
      -- the C/D alternation at 4 tics each *is* the saw running
      let ok := (List.range 8).all fun t =>
        Weapon.idleFrame w t == (if t % 8 < 4 then 'C' else 'D')
      unless ok do
        errs := errs ++ ["chainsaw: idleFrame does not run the 4-tic C/D cycle"]
    else
      unless Weapon.idleFrame w 0 == ready.states[0]!.frame do
        errs := errs ++ [s!"{vw.name}: idle frame ≠ {ready.states[0]!.frame}"]
  return errs

/-! ## Animation expansion -/

/-- Split a trailing decimal suffix: `"SLIME09"` → `("SLIME", 9, 2)`. -/
def suffixNum (s : String) : Option (String × Nat × Nat) :=
  let chars := s.toList
  let digits := (chars.reverse.takeWhile (·.isDigit)).reverse
  if digits.isEmpty then none
  else some (String.ofList (chars.take (chars.length - digits.length)),
             (String.ofList digits).toNat!, digits.length)

/-- Expand a vanilla animdefs start..end pair into its frame list. The two
letter-suffixed wall cycles are irregular directory runs and are spelled
out; everything else is a numeric run at the start name's digit width. -/
def expandAnim (start stop : String) : Option (Array String) :=
  if start == "FIREWALA" && stop == "FIREWALL" then
    some #["FIREWALA", "FIREWALB", "FIREWALL"]
  else if start == "FIRELAV3" && stop == "FIRELAVA" then
    some #["FIRELAV3", "FIRELAVA"]
  else match suffixNum start, suffixNum stop with
  | some (p1, n1, w), some (p2, n2, _) =>
    if p1 == p2 && n1 ≤ n2 then
      some <| (Array.range (n2 - n1 + 1)).map fun i =>
        let raw := toString (n1 + i)
        p1 ++ String.ofList (List.replicate (w - raw.length) '0') ++ raw
    else none
  | _, _ => none

/-! ## The run -/

def reportErrs (errs : List String) : IO Unit := do
  for e in errs do IO.eprintln s!"    ✗ {e}"

def main : IO UInt32 := do
  let mut r : TestRun := {}

  IO.println "vanilla fidelity: mobjinfo + states (info.c)"
  r ← check r "kind table aligns with VanillaData row order"
    (kindTable.length == VanillaData.mobjs.size
      && kindTable.all fun (k, v) => s!"{repr k}".endsWith ("." ++ v.dillKind))
  for (k, v) in kindTable do
    let info := ActorInfo.ofKind k
    let name := v.dillKind
    let physErrs := fieldErrors name info v
    reportErrs physErrs
    r ← check r s!"{name}: physique" physErrs.isEmpty
    let flErrs := flagErrors name info v
    reportErrs flErrs
    r ← check r s!"{name}: flags" flErrs.isEmpty
    let sndErrs := soundErrors name k v
    reportErrs sndErrs
    r ← check r s!"{name}: sounds" sndErrs.isEmpty
    if intentional.customActors.contains name then
      continue   -- chain topology checked bespoke below
    let chErrs := chainErrors name info v
    reportErrs chErrs
    r ← check r s!"{name}: state chains" chErrs.isEmpty

  -- The Icon of Sin's hidden pair: vanilla hides the spitter with
  -- MF_NOSECTOR on an SSWV sprite and spawns the targets in S_NULL; DILL
  -- has neither mechanism and hides both behind the null sprite TNT1.
  let spit := ActorInfo.ofKind .iconSpit
  r ← check r "iconSpit: wake roar then a cube every 150 tics (TNT1 stand-in)"
    (spit.sprite == "TNT1" && spit.states.size == 3
      && spit.states[1]!.tics == 181 && spit.states[1]!.action == some .brainAwake
      && spit.states[1]!.next == some 2
      && spit.states[2]!.tics == 150 && spit.states[2]!.action == some .brainSpit
      && spit.states[2]!.next == some 2)
  let tgt := ActorInfo.ofKind .iconTarget
  r ← check r "iconTarget: an invisible marker (vanilla spawns it in S_NULL)"
    (tgt.sprite == "TNT1" && tgt.states.size == 1 && tgt.states[0]!.tics < 0)

  IO.println "vanilla fidelity: weaponinfo (d_items.c / p_pspr.c)"
  for (w, vw) in weaponTable do
    let atkErrs := weaponAtkErrors w vw
    reportErrs atkErrs
    r ← check r s!"{vw.name}: attack sequence" atkErrs.isEmpty
    let flErrs := weaponFlashErrors w vw
    reportErrs flErrs
    r ← check r s!"{vw.name}: muzzle flash" flErrs.isEmpty
    let mErrs := weaponMetaErrors w vw
    reportErrs mErrs
    r ← check r s!"{vw.name}: sprite, ammo, idle" mErrs.isEmpty

  IO.println "vanilla fidelity: item and scenery things"
  for td in VanillaData.things do
    let mut errs : List String := []
    match ActorKind.ofThingType td.doomednum with
    | none => errs := [s!"doomednum {td.doomednum} unmapped in ofThingType"]
    | some k =>
      let info := ActorInfo.ofKind k
      let vFrames := String.ofList (td.chain.states.toList.map (·.frame))
      let vSprite := td.chain.states[0]!.sprite
      match k with
      | .item f frames =>
        unless f == vSprite && frames == vFrames do
          errs := errs ++ [s!"{td.doomednum}: {f} \"{frames}\" ≠ {vSprite} \"{vFrames}\""]
        unless td.flags.contains "MF_SPECIAL" do
          errs := errs ++ [s!"{td.doomednum}: DILL pickup but vanilla not MF_SPECIAL"]
        unless info.radius == td.radius && info.height == td.height do
          errs := errs ++ [s!"{td.doomednum}: {info.radius}×{info.height} ≠ {td.radius}×{td.height}"]
        unless k.countItem == td.flags.contains "MF_COUNTITEM" do
          errs := errs ++ [s!"{td.doomednum}: countItem ≠ MF_COUNTITEM"]
      | .scenery f frames solid =>
        unless f == vSprite && frames == vFrames do
          errs := errs ++ [s!"{td.doomednum}: {f} \"{frames}\" ≠ {vSprite} \"{vFrames}\""]
        unless solid == td.flags.contains "MF_SOLID" do
          errs := errs ++ [s!"{td.doomednum}: solid ≠ MF_SOLID"]
        unless info.ceilingHang == td.flags.contains "MF_SPAWNCEILING" do
          errs := errs ++ [s!"{td.doomednum}: ceilingHang ≠ MF_SPAWNCEILING"]
        unless td.flags.contains "MF_SPECIAL" == false do
          errs := errs ++ [s!"{td.doomednum}: vanilla MF_SPECIAL but DILL scenery"]
        unless intentional.sceneryPhysiqueFold
            || (info.radius == td.radius && info.height == td.height) do
          errs := errs ++ [s!"{td.doomednum}: {info.radius}×{info.height} ≠ {td.radius}×{td.height}"]
      | _ => errs := errs ++ [s!"{td.doomednum}: maps to a fixed actor, not item/scenery"]
      -- chain shape: vanilla single-frame halts and looping strips both
      -- become DILL frame-string loops; a vanilla chain that *removes* its
      -- thing is a real divergence unless noted (thing 23 deletes itself)
      if td.chain.ending == .remove
          && !intentional.thingChainNotes.contains td.doomednum then
        errs := errs ++ [s!"{td.doomednum}: vanilla chain removes the thing"]
      unless intentional.thingAnimFold do
        for vs in td.chain.states do
          unless (vs.tics == 6 || vs.tics == -1) && !vs.bright do
            errs := errs ++ [s!"{td.doomednum}: tics/bright not the folded 6-tic loop"]
    reportErrs errs
    r ← check r s!"thing {td.doomednum} ({td.mt})" errs.isEmpty

  IO.println "vanilla fidelity: animations (p_spec.c animdefs)"
  let mut matched := 0
  for (_, start, stop) in VanillaData.animdefs do
    if start == "SWATER1" then
      -- named by animdefs, absent from both retail IWADs (see allowlist)
      r ← check r "SWATER1..SWATER4 skipped like P_InitPicAnims would"
        intentional.swaterSkipped
    else
      match expandAnim start stop with
      | none => r ← check r s!"anim {start}..{stop} expands" false
      | some frames =>
        let hit := Assets.animGroups.contains frames
        unless hit do
          IO.eprintln s!"    ✗ animGroups lacks {frames}"
        if hit then matched := matched + 1
        r ← check r s!"anim {start}..{stop}" hit
  r ← check r "no animGroups beyond vanilla's table"
    (Assets.animGroups.size == matched)

  IO.println "vanilla fidelity: switches (p_switch.c alphSwitchList)"
  let mut swErrs : List String := []
  for (sw1, sw2, _) in VanillaData.alphSwitchList do
    unless Level.switchTwin sw1 == some sw2
        && Level.switchTwin sw2 == some sw1 do
      swErrs := swErrs ++ [s!"{sw1} ↔ {sw2}"]
  reportErrs swErrs
  -- episode gates deliberately unmodeled (see allowlist): the prefix rule
  -- is a harmless superset of every gamemode's table slice
  r ← check r
    s!"all {VanillaData.alphSwitchList.size} switch pairs flip via the SW1/SW2 prefix rule"
    (swErrs.isEmpty && intentional.episodeGatesUnmodeled)

  IO.println "vanilla fidelity: movement constants (p_spec.h / p_local.h)"
  r ← check r "door, lift, floor, ceiling and stair speeds match"
    (Speeds.door == VanillaData.Consts.vdoorSpeed
      && Speeds.doorWait == VanillaData.Consts.vdoorWait
      && Speeds.doorBlaze == VanillaData.Consts.vdoorBlazeSpeed
      && Speeds.lift == VanillaData.Consts.platSpeedDWUS
      && Speeds.liftBlaze == VanillaData.Consts.platSpeedBlaze
      && Speeds.liftPerpetual == VanillaData.Consts.platSpeed
      && Speeds.liftWait == VanillaData.Consts.platWaitTics
      && Speeds.floor == VanillaData.Consts.floorSpeed
      && Speeds.floorTurbo == VanillaData.Consts.floorSpeedTurbo
      && Speeds.ceiling == VanillaData.Consts.ceilSpeed
      && Speeds.stair == VanillaData.Consts.stairSpeedBuild8
      && Speeds.stairTurbo == VanillaData.Consts.stairSpeedTurbo16)
  r ← check r "GRAVITY and MAXMOVE match"
    (Player.gravity == VanillaData.Consts.gravity
      && Player.maxMove == VanillaData.Consts.maxMove)
  r ← check r "combat ranges, weapon travel, skull charge and float drift match"
    (Player.meleeRange == VanillaData.Consts.meleeRange
      && Player.missileRange == VanillaData.Consts.missileRange
      && Player.weaponShift == VanillaData.Consts.weaponLowerRaiseSpeed
      && Player.weaponTop == VanillaData.Consts.weaponTop
      && Player.weaponBottom == VanillaData.Consts.weaponBottom
      && Speeds.skullCharge == VanillaData.Consts.skullSpeed
      && Speeds.floatDrift == VanillaData.Consts.floatSpeed
      && Speeds.floorRaiseChange == VanillaData.Consts.platSpeedRaiseChange)

  if r.failures == 0 then
    IO.println "all vanilla-fidelity tests passed"
    return 0
  else
    IO.println s!"{r.failures} vanilla-fidelity test(s) failed"
    return 1
