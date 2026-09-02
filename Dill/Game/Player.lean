import Dill.Game.Combat

/-!
# The player's hands

Weapon state machines (the sprite bobbing at the bottom of the screen),
trigger logic, and item pickups. Timings, spreads, and damage dice follow
vanilla's tables; the state machines are flattened to one attack sequence
per weapon with `fire`/`flash`/`refire` marks.
-/

namespace Dill

/-- One step of a weapon's attack animation. -/
structure GunState where
  frame  : Char
  tics   : Nat
  /-- Pull the trigger on entry (consume ammo, do the damage). -/
  fire   : Bool := false
  /-- Muzzle-flash frame overlaid at full brightness. -/
  flash  : Option Char := none
  /-- Holding the button here loops the sequence. -/
  refire : Bool := false
  /-- A sound cue played on entering this step — the super shotgun's
  break-load-snap reload choreography (vanilla `A_OpenShotgun2` and kin). -/
  sfx    : Option Sfx := none
  deriving Repr, Inhabited

namespace Weapon

def sprite : Weapon → String
  | .fist => "PUNG" | .chainsaw => "SAWG" | .pistol => "PISG"
  | .shotgun => "SHTG" | .superShotgun => "SHT2"
  | .chaingun => "CHGG" | .rocket => "MISG"
  | .plasma => "PLSG" | .bfg => "BFGG"

def flashSprite : Weapon → String
  | .fist => "PUNG" | .chainsaw => "SAWG" | .pistol => "PISF"
  | .shotgun => "SHTF" | .superShotgun => "SHT2"
  | .chaingun => "CHGF" | .rocket => "MISF"
  | .plasma => "PLSF" | .bfg => "BFGF"

/-- Attack sequences from vanilla's weapon states. -/
def attack : Weapon → Array GunState
  | .fist =>
    #[⟨'B', 4, false, none, false, none⟩, ⟨'C', 4, true, none, false, none⟩,
      ⟨'D', 5, false, none, false, none⟩, ⟨'C', 4, false, none, false, none⟩,
      ⟨'B', 5, false, none, true, none⟩]
  | .chainsaw =>
    #[⟨'A', 4, true, none, false, none⟩, ⟨'B', 4, true, none, true, none⟩]
  | .pistol =>
    #[⟨'A', 4, false, none, false, none⟩, ⟨'B', 6, true, some 'A', false, none⟩,
      ⟨'C', 4, false, none, false, none⟩, ⟨'B', 5, false, none, true, none⟩]
  | .shotgun =>
    -- SHTG has only A–D; vanilla pumps A→B→C→D→C→B→A, then holds a final
    -- 7-tic A (S_SGUN9, the A_ReFire beat) before the next shell
    #[⟨'A', 3, false, none, false, none⟩, ⟨'A', 7, true, some 'A', false, none⟩,
      ⟨'B', 5, false, none, false, none⟩, ⟨'C', 5, false, none, false, none⟩,
      ⟨'D', 4, false, none, false, none⟩, ⟨'C', 5, false, none, false, none⟩,
      ⟨'B', 5, false, none, false, none⟩, ⟨'A', 3, false, none, false, none⟩,
      ⟨'A', 7, false, none, true, none⟩]
  | .superShotgun =>
    -- The flash is SHT2's own I/J frames, not the gun frame. The D/F/H
    -- beats carry vanilla's A_OpenShotgun2 / A_LoadShotgun2 /
    -- A_CloseShotgun2 sound cues: the gun breaks open, loads, snaps shut.
    #[⟨'A', 3, false, none, false, none⟩, ⟨'A', 7, true, some 'I', false, none⟩,
      ⟨'B', 7, false, none, false, none⟩, ⟨'C', 7, false, none, false, none⟩,
      ⟨'D', 7, false, none, false, some Sfx.dbOpn⟩,
      ⟨'E', 7, false, none, false, none⟩,
      ⟨'F', 7, false, none, false, some Sfx.dbLoad⟩,
      ⟨'G', 6, false, none, false, none⟩,
      ⟨'H', 6, false, none, false, some Sfx.dbCls⟩,
      ⟨'A', 5, false, none, true, none⟩]
  | .chaingun =>
    -- the refire beat is vanilla's 0-tic S_CHAIN3: on release the gun
    -- drops straight back to ready
    #[⟨'A', 4, true, some 'A', false, none⟩, ⟨'B', 4, true, some 'B', false, none⟩,
      ⟨'B', 0, false, none, true, none⟩]
  | .rocket =>
    -- Vanilla S_MISSILE1–3: MISG holds its one recoil frame B for 8 tics
    -- (A_GunFlash on entry starts the separate 4-frame MISF flash psprite)
    -- + 12 tics (A_FireMissile on entry — the rocket leaves at t=8) + a
    -- 0-tic A_ReFire. The one-overlay-per-state model steps the B hold
    -- through the MISF frames instead: A and B at their vanilla 3/4 tics,
    -- C cut to the 1 tic left before the shot, D held to the flash's t=15
    -- end. Same 20-tic total, rocket away at t=8.
    #[⟨'B', 3, false, some 'A', false, none⟩,
      ⟨'B', 4, false, some 'B', false, none⟩,
      ⟨'B', 1, false, some 'C', false, none⟩,
      ⟨'B', 7, true,  some 'D', false, none⟩,
      ⟨'B', 5, false, none, false, none⟩,
      ⟨'B', 0, false, none, true, none⟩]
  | .plasma =>
    #[⟨'A', 3, true, some 'A', false, none⟩, ⟨'B', 20, false, none, true, none⟩]
  | .bfg =>
    #[⟨'A', 20, false, none, false, none⟩, ⟨'B', 10, false, none, false, none⟩,
      ⟨'B', 10, true, some 'A', false, none⟩, ⟨'B', 20, false, none, true, none⟩]

/-- The frame shown when the weapon is up but idle. Most guns just sit on
`A`, but the chainsaw idles on C/D — that alternation *is* the saw running,
and holding it on one frame is why it looks dead in the hands. -/
def idleFrame : Weapon → Nat → Char
  | .chainsaw, tics => if tics / 4 % 2 == 0 then 'C' else 'D'
  | _, _ => 'A'

def ofNumber : Nat → Option Weapon
  | 1 => some .fist | 2 => some .pistol | 3 => some .shotgun
  | 4 => some .chaingun | 5 => some .rocket | 6 => some .plasma
  | 7 => some .bfg | _ => none

/-- Which ammo pool a weapon draws from (`none` = melee). -/
def ammoType : Weapon → Option Ammo
  | .fist | .chainsaw => none
  | .pistol | .chaingun => some .bullets
  | .shotgun | .superShotgun => some .shells
  | .rocket => some .rockets
  | .plasma | .bfg => some .cells

/-- Rounds a trigger pull spends (the BFG eats 40 cells). -/
def ammoCost : Weapon → Nat
  | .bfg => 40 | .superShotgun => 2 | _ => 1

end Weapon

namespace PlayerStatus

def ammoCount (st : PlayerStatus) : Ammo → Nat
  | .bullets => st.bullets
  | .shells => st.shells
  | .rockets => st.rockets
  | .cells => st.cells

/-- Is there ammo for a trigger pull of this weapon? -/
def hasAmmoFor (st : PlayerStatus) (w : Weapon) : Bool :=
  match w.ammoType with
  | none => true
  | some a => st.ammoCount a ≥ w.ammoCost

def owns (st : PlayerStatus) : Weapon → Bool
  | .fist | .pistol => true
  | .chainsaw => st.ownsChainsaw
  | .shotgun => st.ownsShotgun
  | .superShotgun => st.ownsSuperShotgun
  | .chaingun => st.ownsChaingun
  | .rocket => st.ownsRocket
  | .plasma => st.ownsPlasma
  | .bfg => st.ownsBfg

/-- Vanilla `P_CheckAmmo`'s fallback ladder: what to reach for when the gun
in hand runs dry. The order is the one hardcoded there, and so are the two
odd thresholds — the super shotgun wants *more than* two shells and the BFG
more than forty cells, though a shot costs exactly that. Every rung either
has the ammo for a shot or needs none, so this never picks the empty weapon
it is replacing. -/
def bestWhenDry (st : PlayerStatus) : Weapon :=
  if st.ownsPlasma && st.cells > 0 then .plasma
  else if st.ownsSuperShotgun && st.shells > 2 then .superShotgun
  else if st.ownsChaingun && st.bullets > 0 then .chaingun
  else if st.ownsShotgun && st.shells > 0 then .shotgun
  else if st.bullets > 0 then .pistol
  else if st.ownsChainsaw then .chainsaw
  else if st.ownsRocket && st.rockets > 0 then .rocket
  else if st.ownsBfg && st.cells > 40 then .bfg
  else .fist

/-- `P_CheckAmmo` when the answer is "no": drop out of the attack and queue
the fallback, which sets the empty gun lowering on the next tic. -/
def dryFallback (st : PlayerStatus) : PlayerStatus :=
  let next := bestWhenDry st
  { st with attack := none, refiring := false
            pending := if next == st.weapon then st.pending else some next }

/-- Spend a weapon's ammo. -/
def spend (st : PlayerStatus) (w : Weapon) : PlayerStatus :=
  match w.ammoType with
  | none => st
  | some .bullets => { st with bullets := st.bullets - w.ammoCost }
  | some .shells => { st with shells := st.shells - w.ammoCost }
  | some .rockets => { st with rockets := st.rockets - w.ammoCost }
  | some .cells => { st with cells := st.cells - w.ammoCost }

end PlayerStatus

namespace GameState

/-- Where a shot leaves the player — `Player.shootZ`, with the ground
position alongside for the trace calls. -/
private def eye (g : GameState) : Float × Float × Float :=
  (g.player.x, g.player.y, g.player.shootZ)

/-- The vertical aim for one pull of the trigger, shared by every pellet
that pull looses (vanilla calls `P_BulletSlope` once per shot). The *aim*
reaches only 1024 (vanilla `16*64*FRACUNIT`) though the shot then flies the
full `MISSILERANGE` — past 1024 the volley simply goes out level. -/
private def playerBulletSlope (g : GameState) : Float :=
  let (ex, ey, ez) := g.eye
  bulletSlope g ex ey ez g.player.angle 1024 true

/-- One player bullet. `slope` is the volley's shared vertical aim;
`spreadScale` widens the horizontal scatter (the super shotgun's is twice
the usual), and `vSpread` is the extra vertical scatter only it has. -/
private def gunShot (g : GameState) (accurate : Bool) (damageMult : Nat)
    (slope : Float) (spreadScale : Float := 1.0) (vSpread : Bool := false) :
    GameState :=
  let (ex, ey, ez) := g.eye
  let (spread, g) := if accurate then (0, g) else g.randDiff
  let (vJit, g) := if vSpread then g.randDiff else (0, g)
  let (dmg, g) := g.randDice 1 3
  -- vanilla A_FireShotgun2 adds ((P_Random()-P_Random())<<5)/65536 to the
  -- slope: each diff step is 32/65536 ≈ 0.000488 of slope
  g.lineAttack ex ey ez
    (g.player.angle + Float.ofInt spread * 0.000383 * spreadScale)
    Player.missileRange
    (dmg * damageMult) true
    (slope := some (slope + Float.ofInt vJit * (32.0 / 65536.0)))

/-- The nearest shootable thing within `range` in a modest cone ahead of the
player — the chainsaw's bite check (vanilla `P_AimLineAttack` for `A_Saw`). -/
private def meleeTargetIdx (g : GameState) (a range : Float) : Option Nat := Id.run do
  let p := g.player
  let mut best : Option (Nat × Float) := none
  for i in g.mobjsNear p.x p.y range do
    let m := g.mobjs[i]!
    if m.removed || !m.shootable then continue
    let d := m.distanceTo p.x p.y
    if d ≥ range + m.info.radius then continue
    let rel := Float.atan2 (m.y - p.y) (m.x - p.x) - a
    let rel := Float.abs (wrapAngle rel)
    if rel > 0.35 then continue
    match best with
    | some (_, bd) => if d < bd then best := some (i, d)
    | none => best := some (i, d)
  return best.map (·.1)

/-- Pull the trigger: consume ammo, make noise, do the harm. -/
private def fireWeapon (g : GameState) : GameState := Id.run do
  let st := g.status
  let sfx := match st.weapon with
    | .fist => Sfx.punch
    | .chainsaw => Sfx.sawFull
    | .shotgun => Sfx.shotgun
    | .superShotgun => Sfx.superShotgun
    | .rocket => Sfx.rocket
    | .plasma => Sfx.plasma
    | .bfg => Sfx.bfg
    | _ => Sfx.pistol   -- pistol and chaingun share the pistol report
  -- A hitscan shot's tracer can trip a gun-triggered line (46/47) — `tick`
  -- checks that, since the special dispatch lives a layer up from here.
  let hitscan := match st.weapon with
    | .rocket | .plasma | .bfg => false
    | _ => true
  -- Every weapon raises the alert and floods it through the map's geometry:
  -- `P_FireWeapon` calls `P_NoiseAlert` unconditionally, so swinging the
  -- fist or the chainsaw wakes the room just as a gunshot does.
  -- melee "hitscans" only reach as far as the swing (vanilla runs the
  -- punch's `P_LineAttack` traverse over `MELEERANGE`, not `MISSILERANGE`;
  -- `A_Saw` uses `MELEERANGE + 1` = 65)
  let range : Float := match st.weapon with
    | .fist => Player.meleeRange
    | .chainsaw => Player.meleeRange + 1
    | _ => Player.missileRange
  let mut g := { g with firedShot := hitscan, firedRange := range }
  g := g.alertSound
  -- the chainsaw picks its bite-or-air sound after the swing (below), and
  -- the fist's punch plays only on a hit (vanilla A_Punch — see its arm);
  -- the BFG roared at the trigger pull (vanilla A_BFGsound), 30 tics before
  -- the ball leaves, so nothing plays here
  if st.weapon != .chainsaw && st.weapon != .bfg && st.weapon != .fist then
    g := g.playSound sfx g.player.x g.player.y
  g := { g with status := st.spend st.weapon }
  match st.weapon with
  | .fist =>
    let (spread, g') := g.randDiff
    let (dmg, g'') := g'.randDice 1 10
    let (ex, ey, ez) := g''.eye
    -- a berserk pack multiplies fist damage tenfold
    let mult := if st.berserk then 20 else 2
    let a := g''.player.angle + Float.ofInt spread * 0.000383
    -- Vanilla A_Punch: the punch sound and the snap to face the victim
    -- happen only on a hit (`linetarget`) — a swing at air is silent and
    -- leaves the aim alone. The victim is found the way the chainsaw's
    -- tug finds its bite.
    let victim := (meleeTargetIdx g'' a Player.meleeRange).map fun ti =>
      let t := g''.mobjs[ti]!; (t.x, t.y)
    let g3 := g''.lineAttack ex ey ez a Player.meleeRange (dmg * mult) true
    match victim with
    | none => return g3
    | some (tx, ty) =>
      let g3 := g3.playSound Sfx.punch g3.player.x g3.player.y
      -- unlike the saw's gentle tug, the punch snaps square onto the target
      return { g3 with player := { g3.player with
        angle := Float.atan2 (ty - g3.player.y) (tx - g3.player.x) } }
  | .chainsaw =>
    let (spread, g') := g.randDiff
    let (dmg, g'') := g'.randDice 1 10
    let (ex, ey, ez) := g''.eye
    let a := g''.player.angle + Float.ofInt spread * 0.000383
    -- vanilla A_Saw: a bite plays sawhit and tugs the aim onto the target;
    -- cutting air plays sawful. Its reach is MELEERANGE + 1 = 65, one unit
    -- past the fist — not a longer blade.
    let tugTo := (meleeTargetIdx g'' a (Player.meleeRange + 1)).map (fun ti =>
      let t := g''.mobjs[ti]!; (t.x, t.y))
    let g3 := g''.lineAttack ex ey ez a (Player.meleeRange + 1) (dmg * 2) true
    let g3 := g3.playSound (if tugTo.isSome then Sfx.sawHit else Sfx.sawFull)
                g3.player.x g3.player.y
    match tugTo with
    | none => return g3
    | some (tx, ty) =>
      let target := Float.atan2 (ty - g3.player.y) (tx - g3.player.x)
      let d := target - g3.player.angle
      let d := wrapAngle d
      let step := max (-0.078) (min 0.078 d)   -- tug ≤ ~4.5° toward the bite
      let p := { g3.player with angle := g3.player.angle + step }
      return { g3 with player := p }
  -- Every hitscan weapon settles its vertical aim once, up front, and every
  -- pellet of the volley then flies along it — vanilla's `P_BulletSlope`
  -- before the `P_GunShot` loop. Re-aiming per pellet would let each one
  -- lock onto a different target.
  | .pistol =>
    return g.gunShot (accurate := !st.refiring) 5 g.playerBulletSlope
  | .chaingun =>
    return g.gunShot (accurate := !st.refiring) 5 g.playerBulletSlope
  | .shotgun =>
    let slope := g.playerBulletSlope
    for _ in [0:7] do
      g := g.gunShot (accurate := false) 5 slope
    return g
  | .superShotgun =>
    -- Twenty pellets from both barrels: twice the horizontal scatter of any
    -- other gun, and the only one that scatters vertically too.
    let slope := g.playerBulletSlope
    for _ in [0:20] do
      g := g.gunShot (accurate := false) 5 slope
        (spreadScale := 2.0) (vSpread := true)
    return g
  | .rocket => return g.spawnPlayerMissile .rocket
  | .plasma => return g.spawnPlayerMissile .plasmaBall
  | .bfg => return g.spawnPlayerMissile .bfgBall

/-- The weapon-sprite state machine, one tic. -/
def tickWeapon (g : GameState) (input : Input) : GameState := Id.run do
  let mut g := g
  let mut st := g.status
  if st.dead then return g
  -- Weapon switch (vanilla LOWERSPEED/RAISESPEED; WEAPONTOP/WEAPONBOTTOM).
  -- Lowering the old gun and raising the new one both suspend firing.
  if st.pending.isSome && st.attack.isNone then
    let y := st.weaponY + Player.weaponShift
    if y ≥ Player.weaponBottom then
      let neww := st.pending.get!
      -- Vanilla A_WeaponReady: the rocket launcher and the BFG never
      -- auto-fire — a trigger held across the switch (`attackdown`) must be
      -- released and pressed afresh. The latch rides `refiring`, which is
      -- otherwise never set with the weapon at ready; the ready branch
      -- below honors and clears it.
      st := { st with weapon := neww, pending := none
                      weaponY := Player.weaponBottom
                      refiring := input.fire
                        && (neww == .rocket || neww == .bfg) }
      g := { g with status := st }
      -- the chainsaw revs as it comes up (vanilla sfx_sawup in P_BringUpWeapon)
      if neww == .chainsaw then
        g := g.playSound Sfx.sawUp g.player.x g.player.y
      return g
    st := { st with weaponY := y }
    return { g with status := st }
  if st.weaponY > Player.weaponTop && st.pending.isNone then
    st := { st with weaponY := max Player.weaponTop (st.weaponY - Player.weaponShift) }
    g := { g with status := st }
    if st.weaponY > Player.weaponTop then return g   -- still rising; no firing yet
  -- Queue a switch: vanilla accepts `pendingweapon` at any time — mid-attack
  -- included — and it takes effect at the next A_ReFire / A_WeaponReady
  -- beat. The lowering itself still waits for the attack to resolve (the
  -- pending block above only runs with `attack` clear).
  if st.pending.isNone && st.weaponY == 32 then
    if let some n := input.weapon then
      -- slot 1 toggles fist ↔ chainsaw, and slot 3 shotgun ↔ super shotgun
      -- (vanilla shares each pair on one key)
      let wanted := if n == 1 && st.ownsChainsaw
        then (if st.weapon == .fist then Weapon.chainsaw else .fist)
        else if n == 3 && st.ownsSuperShotgun
        then (if st.weapon == .shotgun then Weapon.superShotgun else .shotgun)
        else Weapon.ofNumber n |>.getD st.weapon
      if st.owns wanted && wanted != st.weapon then
        st := { st with pending := some wanted
                        refiring := st.attack.isSome && st.refiring }
        g := { g with status := st }
        -- from an idle weapon this tic is spent on the queueing; a
        -- mid-attack queue lets the sequence keep stepping below
        if st.attack.isNone then return g
  match st.attack with
  | none =>
    -- ready: a fresh trigger pull starts the attack. Vanilla `P_FireWeapon`
    -- runs `P_CheckAmmo` first, so pulling on an empty gun does not click —
    -- it reaches for the next weapon down the ladder instead.
    if input.fire then
      -- vanilla A_WeaponReady: the rocket launcher and the BFG do not
      -- auto-fire — the `attackdown` latch (here on `refiring`, set at
      -- switch completion) holds them until the trigger is released
      if st.refiring && (st.weapon == .rocket || st.weapon == .bfg) then
        return g
      if !st.hasAmmoFor st.weapon then
        return { g with status := st.dryFallback }
      st := { st with attack := some 0
                      psprTics := (st.weapon.attack[0]!).tics
                      refiring := false }
      g := { g with status := st }
      -- vanilla A_BFGsound: the BFG roars on the first attack state, at the
      -- pull itself — the ball only leaves 30 tics later
      if st.weapon == .bfg then
        g := g.playSound Sfx.bfg g.player.x g.player.y
      if let some s := (st.weapon.attack[0]!).sfx then
        g := g.playSound s g.player.x g.player.y
      if (st.weapon.attack[0]!).fire then
        g := g.fireWeapon
      return g
    -- trigger up at ready: the attackdown latch releases (as vanilla
    -- clears `attackdown` here), so the next press fires
    if st.refiring then
      st := { st with refiring := false }
      g := { g with status := st }
    -- idle: the chainsaw keeps its running putter (vanilla sfx_sawidl on the
    -- S_SAW ready frame, ~every 8 tics)
    if st.weapon == .chainsaw && st.weaponY == 32 && g.tics % 8 == 0 then
      g := g.playSound Sfx.sawIdle g.player.x g.player.y
    return g
  | some idx =>
    let seq := st.weapon.attack
    if st.psprTics > 1 then
      return { g with status := { st with psprTics := st.psprTics - 1 } }
    if idx + 1 < seq.size then
      let nextStep := seq[idx + 1]!
      -- A refire step (vanilla A_ReFire) is resolved the instant it is
      -- reached: with the trigger held we restart the attack now, skipping
      -- the step's tics — so a long refire frame (the plasma's 20-tic B)
      -- becomes the *release* cooldown, not a delay between shots.
      -- vanilla `A_ReFire` ends in `P_CheckAmmo` either way, so reaching the
      -- refire point dry queues the fallback instead of looping the attack
      if nextStep.refire && !st.hasAmmoFor st.weapon then
        return { g with status := st.dryFallback }
      -- vanilla A_ReFire: a queued weapon change goes through instead of
      -- looping — the held trigger does not trap you on the old gun
      if nextStep.refire && input.fire && st.pending.isNone
          && st.hasAmmoFor st.weapon then
        st := { st with attack := some 0, psprTics := (seq[0]!).tics
                        refiring := true }
        g := { g with status := st }
        -- a held BFG trigger loops the sequence from its A_BFGsound state
        if st.weapon == .bfg then
          g := g.playSound Sfx.bfg g.player.x g.player.y
        if let some s := (seq[0]!).sfx then
          g := g.playSound s g.player.x g.player.y
        if (seq[0]!).fire then g := g.fireWeapon
        return g
      st := { st with attack := some (idx + 1), psprTics := nextStep.tics }
      g := { g with status := st }
      -- a step's sound cue plays on entry (the super shotgun's reload)
      if let some s := nextStep.sfx then
        g := g.playSound s g.player.x g.player.y
      if nextStep.fire then
        if st.hasAmmoFor st.weapon then g := g.fireWeapon
        else
          -- dry mid-sequence: `P_CheckAmmo` again, so the empty gun goes
          -- away rather than finishing its animation on nothing
          g := { g with status := st.dryFallback }
      return g
    return { g with status := { st with attack := none, refiring := false } }

/-! ## Pickups -/

private def capped (v add cap : Int) : Int := min cap (v + add)

/-- Ammo capacities, doubled once the backpack is carried. -/
private def ammoMax (st : PlayerStatus) : Ammo → Nat
  | .bullets => if st.backpack then 400 else 200
  | .shells => if st.backpack then 100 else 50
  | .rockets => if st.backpack then 100 else 50
  | .cells => if st.backpack then 600 else 300

/-- Vanilla `P_GiveAmmo`: nothing at a full pool (`none` — the item is
left in the world); otherwise add and cap. And when the pool was at *zero*,
reach for a better weapon with vanilla's hardcoded preferences: fresh
bullets pull a fist-holder onto the chaingun (else the pistol); shells pull
the fist or pistol onto the shotgun; cells the fist or pistol onto the
plasma rifle; rockets only ever pull the fist onto the launcher. Any other
weapon in hand is left alone — "player was lower on purpose". -/
private def giveAmmo (st : PlayerStatus) (a : Ammo) (n : Nat) :
    Option PlayerStatus :=
  if st.ammoCount a ≥ ammoMax st a then none
  else
    let old := st.ammoCount a
    let v := min (ammoMax st a) (old + n)
    let st := match a with
      | .bullets => { st with bullets := v }
      | .shells => { st with shells := v }
      | .rockets => { st with rockets := v }
      | .cells => { st with cells := v }
    if old != 0 then some st
    else
      let pick : Option Weapon := match a with
        | .bullets =>
          if st.weapon == .fist then
            some (if st.ownsChaingun then .chaingun else .pistol)
          else none
        | .shells =>
          if (st.weapon == .fist || st.weapon == .pistol) && st.ownsShotgun
          then some .shotgun else none
        | .cells =>
          if (st.weapon == .fist || st.weapon == .pistol) && st.ownsPlasma
          then some .plasma else none
        | .rockets =>
          if st.weapon == .fist && st.ownsRocket then some .rocket else none
      some (match pick with
        | some w => { st with pending := some w }
        | none => st)

/-- Vanilla `P_GiveWeapon`: a new weapon is marked owned (by the caller),
gains its clips and queues itself; an *owned* one is only worth its ammo —
and with that pool already full nothing is gained, so it answers `none` and
the weapon stays on the floor. -/
private def giveWeapon (st : PlayerStatus) (w : Weapon) (owned : Bool)
    (ammo : Ammo) (n : Nat) : Option PlayerStatus :=
  let gaveAmmo := giveAmmo st ammo n
  if owned then gaveAmmo
  else
    -- Switching to a newly-picked weapon is *deferred* through `pending`,
    -- not applied to `weapon` on the spot: an instant swap mid-attack would
    -- leave `attack` indexing the old weapon's longer sequence and panic
    -- the HUD. (This overwrites any switch `giveAmmo` queued, as vanilla's
    -- ordering does.)
    some { gaveAmmo.getD st with pending := some w }

/-- Apply one touched item; `none` = leave it (nothing gained).

`double` is vanilla's trainer bonus: `P_GiveAmmo` shifts every haul left by
one on I'm Too Young To Die and on Nightmare ("you'll need it in
nightmare"). It applies to bare ammo, to the clips that come with a weapon,
and to the backpack alike, and lands before the capacity cap. -/
private def applyItem (st : PlayerStatus) (family : String) (dropped : Bool)
    (double : Bool := false) : Option PlayerStatus :=
  let health := fun (n : Int) (cap : Int) =>
    if st.health ≥ cap then none
    else some { st with health := capped st.health n cap }
  let amt := fun (n : Nat) => if double then n * 2 else n
  match family with
  | "CLIP" => giveAmmo st .bullets (amt (if dropped then 5 else 10))
  | "AMMO" => giveAmmo st .bullets (amt 50)
  | "SHEL" => giveAmmo st .shells (amt 4)
  | "SBOX" => giveAmmo st .shells (amt 20)
  | "ROCK" => giveAmmo st .rockets (amt 1)
  | "BROK" => giveAmmo st .rockets (amt 5)
  | "CELL" => giveAmmo st .cells (amt 20)
  | "CELP" => giveAmmo st .cells (amt 100)
  | "BPAK" =>
    -- backpack: raise the caps, then one clip's worth of each — each haul
    -- through `giveAmmo`, as vanilla's four `P_GiveAmmo` calls, so a pool
    -- filled from zero still auto-raises the matching weapon
    let st := { st with backpack := true }
    let st := (giveAmmo st .bullets (amt 10)).getD st
    let st := (giveAmmo st .shells (amt 4)).getD st
    let st := (giveAmmo st .rockets (amt 1)).getD st
    some ((giveAmmo st .cells (amt 20)).getD st)
  | "STIM" => health 10 100
  | "MEDI" => health 25 100
  | "BON1" => some { st with health := capped st.health 1 200 }
  | "SOUL" => some { st with health := capped st.health 100 200 }
  -- Armour bonuses stack past 100 and, with no jacket on, count as green
  -- (vanilla `if (!player->armortype) player->armortype = 1`). The two
  -- jackets set both the points and the type they absorb at.
  | "BON2" => some { st with armor := capped st.armor 1 200
                             armorType := if st.armorType == 0 then 1 else st.armorType }
  | "ARM1" =>
    if st.armor ≥ 100 then none else some { st with armor := 100, armorType := 1 }
  | "ARM2" =>
    if st.armor ≥ 200 then none else some { st with armor := 200, armorType := 2 }
  | "SHOT" =>
    (giveWeapon st .shotgun st.ownsShotgun .shells
      (amt (if dropped then 4 else 8))).map ({ · with ownsShotgun := true })
  | "SGN2" =>
    (giveWeapon st .superShotgun st.ownsSuperShotgun .shells (amt 8)).map
      ({ · with ownsSuperShotgun := true })
  | "MGUN" =>
    (giveWeapon st .chaingun st.ownsChaingun .bullets
      (amt (if dropped then 10 else 20))).map ({ · with ownsChaingun := true })
  | "CSAW" =>
    -- the chainsaw brings no ammo, so owned again means nothing gained:
    -- vanilla P_GiveWeapon returns false and the saw stays on the floor.
    -- A new saw defers the switch (see `giveWeapon`) so a mid-attack
    -- pickup can't crash.
    if st.ownsChainsaw then none
    else some { st with ownsChainsaw := true, pending := some Weapon.chainsaw }
  | "LAUN" =>
    (giveWeapon st .rocket st.ownsRocket .rockets (amt 2)).map
      ({ · with ownsRocket := true })
  | "PLAS" =>
    (giveWeapon st .plasma st.ownsPlasma .cells (amt 40)).map
      ({ · with ownsPlasma := true })
  | "BFUG" =>
    (giveWeapon st .bfg st.ownsBfg .cells (amt 40)).map
      ({ · with ownsBfg := true })
  -- Doom has two sets of keys, cards and skulls, and a locked door accepts
  -- either colour-match. Only the cards were here, so any map gated on a
  -- skull key could not be finished.
  | "BKEY" | "BSKU" => some { st with blueKey := true }
  | "YKEY" | "YSKU" => some { st with yellowKey := true }
  | "RKEY" | "RSKU" => some { st with redKey := true }
  -- timed powerups (vanilla durations in tics: 35 = one second)
  | "PINV" => some { st with invulnTics := 30 * 35 }
  | "PINS" => some { st with invisTics := 60 * 35 }
  | "SUIT" => some { st with radsuitTics := 60 * 35 }
  | "PVIS" => some { st with gogglesTics := 120 * 35 }
  | "PMAP" => some { st with ownsMap := true }
  -- berserk: full heal, mark berserk, punch out with the fist (deferred) —
  -- but vanilla only queues the fist when it isn't already in hand
  | "PSTR" => some { st with berserk := true, berserkTics := 1
                             pending := if st.weapon == .fist then st.pending
                                        else some .fist
                             health := max st.health 100 }
  -- the megasphere is 200 health and a *blue* jacket (vanilla P_GiveArmor 2)
  | "MEGA" => some { st with health := 200, armor := 200, armorType := 2 }
  | _ => none

/-- Scoop up everything the player is standing on. -/
def touchItems (g : GameState) : GameState := Id.run do
  if g.status.dead then return g
  let mut g := g
  -- only what is standing near the player, not the map's whole roster
  -- (the reach covers the widest item's radius, since the touch test below
  -- is against the sum of radii)
  for i in g.mobjsNear g.player.x g.player.y (Player.radius + 32) do
    let m := g.mobjs[i]!
    if m.removed || !m.info.pickup then continue
    -- Vanilla's touch is PIT_CheckThing's axis-aligned *box* against the
    -- sum of radii — not a circle — so corner grabs reach further than a
    -- Euclidean test would allow.
    let rsum := m.info.radius + Player.radius
    if Float.abs (m.x - g.player.x) ≥ rsum
        || Float.abs (m.y - g.player.y) ≥ rsum then continue
    -- Vanilla P_TouchSpecialThing's vertical window: the item must lie
    -- within (-8, height] of the player's feet — no scooping a bonus off a
    -- ledge overhead, or from a pit more than 8 below
    let delta := m.z - g.player.z
    if delta > Player.height || delta < -8 then continue
    let family := match m.kind with
      | .item f _ => f
      | _ => ""
    -- ITYTD and Nightmare both hand out double ammo (vanilla `P_GiveAmmo`)
    let double := g.skill == 1 || g.skill == 5
    match applyItem g.status family m.dropped double with
    | none => continue
    | some st =>
      -- one short gold pulse; capped so grabbing a cluster (a backpack in a
      -- pile of ammo) doesn't leave the screen washed yellow for a second.
      -- Only the artifacts (`countItem`, vanilla MF_COUNTITEM) move the
      -- intermission tally — and a monster's dropped clip never does.
      let counted := m.kind.countItem && !m.dropped
      g := { g with status := { st with bonusCount := min 8 (st.bonusCount + 4) }
                    items := if counted then g.items + 1 else g.items }
      let sfx := if ["SHOT", "SGN2", "MGUN", "CSAW", "LAUN", "PLAS", "BFUG"].contains family
        then Sfx.weapUp
        -- vanilla marks the eight power items with the sfx_getpow swell
        else if ["SOUL", "MEGA", "PINV", "PSTR", "PINS", "SUIT", "PMAP",
                 "PVIS"].contains family
        then Sfx.getPow else Sfx.itemUp
      g := g.playSound sfx g.player.x g.player.y
      g := g.setMobj i { m with removed := true }
  return g

/-! ## The status-bar face -/

/-- Re-roll the idle look direction on a short random timer, so a resting
face keeps glancing about (vanilla `ST_updateFaceWidget`). -/
def stepFace (g : GameState) : GameState :=
  if g.faceTics > 0 then { g with faceTics := g.faceTics - 1 }
  else
    let (r, faceRng) := g.faceRng.next
    { g with faceLook := r % 3, faceTics := 15 + r % 17, faceRng }

/-- Which face graphic to show: dead, god, hurt (ouch / turned), grinning
after a pickup, or an idle glance — each in the current health bracket. -/
def faceLump (g : GameState) : String :=
  let st := g.status
  let h := max 0 (min 100 st.health)
  let bracket := min 4 ((100 - h) * 5 / 101)
  if st.dead then "STFDEAD0"
  else if st.god then "STFGOD0"
  else if st.damageCount ≥ 20 then s!"STFOUCH{bracket}"
  else if st.damageCount > 0 then
    if g.faceLook == 2 then s!"STFTL{bracket}0" else s!"STFTR{bracket}0"
  else if st.bonusCount > 0 then s!"STFEVL{bracket}"
  else s!"STFST{bracket}{g.faceLook}"

end GameState
end Dill
