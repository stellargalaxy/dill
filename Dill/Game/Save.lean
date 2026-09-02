import Dill.Game.Lights

/-!
# Saved games

A save is a readable text snapshot of everything `tick` evolves: the map
name, the player, every sector's moving parts, active movers, and all
mobjs. Loading re-reads the map from the WAD and lays the snapshot over
it. Floats travel as 16.16 fixed-point integers, so positions round-trip
to within 1/65536 of a unit.

Deliberately *not* saved, and what that costs:

- light-thinker phase (`lights`) — rebuilt by `spawnLights` against the
  pristine WAD light levels, so an effect's bright/dark range survives a
  save taken mid-strobe; only the phase resets, which is invisible
- pressed-switch timers (`buttons`) and flipped sidedef textures — a
  deliberate cosmetic omission: a repeatable switch mid-rebound reloads
  visually unpressed, and a spent one-shot switch reloads showing its
  unpressed SW1 face. The *specials* still round-trip exactly — a fired
  one-shot line is written nowhere and the loader clears every special
  before restoring the listed ones, so it stays fired and cannot re-arm.
- scrolling-wall x offsets (special 48) — the scroll restarts (cosmetic)
- the automap trail (`seen`) — a loaded game shows the map revealed
- `teleFreeze` (an 18-tic window), the transient per-tic flags
  (`firedShot`, `monsterShots`, `exited`, `sounds`, `message`), and the
  status-bar face state

Saved since version 8: a door mover's `delay` (the timed doors of sector
specials 10/14), a lift/perpetual/crusher's `stalled` (stasis, from the
stop-plat and stop-crusher lines), and a rising floor's `crush` flag
(`raiseFloorCrush`). Each rides a column appended to its mover line; an
older save reads as delay 0, not stalled, not crushing.

Saved since version 7: a mobj's pending first-frame action (`entryPending`,
bit 256 of its flag word). The `time` line also dropped its third column —
a noise counter nothing ever read; older saves still load, the stray
column skipped.

Saved since version 6: a mobj's `dropped` flag, in bit 128 of its flag word.
It used to ride on `shooterUid == 1`, which is also an ordinary uid. A
version-5 save has the bit clear, so a monster's dropped clip left lying in
one reloads as a map pickup — full ammo, and counted on the item tally.

Saved since version 5: each door's and lift's speed, so a blazing door
reloads slamming rather than crawling (`Speeds.doorBlaze`/`liftBlaze`). A
save without the column is read as an ordinary-speed mover.

Saved since version 4: each sector's floor flat, because the "and change"
floors (`changeNow`/`applyChange`, the donut ring) rewrite it alongside the
sector special — which *was* saved (version 3) while the flat silently
reverted to the WAD's on load.
-/

namespace Dill.Save

private def fx (v : Float) : Int :=
  ifloor (v * 65536.0 + 0.5)

private def unfx (i : Int) : Float :=
  Float.ofInt i / 65536.0

private def encodeKind : ActorKind → String
  | .zombieman => "zombie" | .shotgunGuy => "sarge" | .imp => "imp"
  | .demon => "demon" | .spectre => "spectre" | .cacodemon => "caco"
  | .lostSoul => "soul" | .baron => "baron" | .barrel => "barrel"
  | .cyberdemon => "cyber" | .spiderMastermind => "spider"
  | .chaingunner => "cpos" | .wolfSS => "sswv" | .hellKnight => "knight"
  | .mancubus => "fatso" | .arachnotron => "baby" | .revenant => "skel"
  | .fatShot => "fatshot" | .arachPlasma => "arachplaz"
  | .revenantMissile => "tracer"
  | .painElemental => "pain" | .archVile => "vile"
  | .commanderKeen => "keen" | .vileFire => "vilefire"
  | .iconBrain => "brain" | .iconSpit => "spit"
  | .iconTarget => "target" | .spawnCube => "cube"
  | .brainExplosion => "brainboom"
  | .impBall => "impball" | .cacoBall => "cacoball"
  | .baronBall => "baronball" | .puff => "puff" | .blood => "blood"
  | .teleFog => "fog"
  | .rocket => "rocket" | .plasmaBall => "plasmaball" | .bfgBall => "bfgball"
  | .bfgPuff => "bfgpuff"
  | .item f fr => s!"item:{f}:{fr}"
  | .scenery f fr sol => s!"scen:{f}:{fr}:{if sol then "1" else "0"}"

private def decodeKind (s : String) : Option ActorKind :=
  match s.splitOn ":" with
  | ["item", f, fr] => some (.item f fr)
  | ["scen", f, fr, sol] => some (.scenery f fr (sol == "1"))
  | ["zombie"] => some .zombieman | ["sarge"] => some .shotgunGuy
  | ["imp"] => some .imp | ["demon"] => some .demon
  | ["spectre"] => some .spectre | ["caco"] => some .cacodemon
  | ["soul"] => some .lostSoul | ["baron"] => some .baron
  | ["cyber"] => some .cyberdemon | ["spider"] => some .spiderMastermind
  | ["cpos"] => some .chaingunner | ["sswv"] => some .wolfSS
  | ["knight"] => some .hellKnight | ["fatso"] => some .mancubus
  | ["baby"] => some .arachnotron | ["skel"] => some .revenant
  | ["fatshot"] => some .fatShot | ["arachplaz"] => some .arachPlasma
  | ["tracer"] => some .revenantMissile
  | ["pain"] => some .painElemental | ["vile"] => some .archVile
  | ["keen"] => some .commanderKeen | ["vilefire"] => some .vileFire
  | ["brain"] => some .iconBrain | ["spit"] => some .iconSpit
  | ["target"] => some .iconTarget | ["cube"] => some .spawnCube
  | ["brainboom"] => some .brainExplosion
  | ["barrel"] => some .barrel | ["impball"] => some .impBall
  | ["cacoball"] => some .cacoBall | ["baronball"] => some .baronBall
  | ["puff"] => some .puff | ["blood"] => some .blood
  | ["fog"] => some .teleFog
  | ["rocket"] => some .rocket | ["plasmaball"] => some .plasmaBall
  | ["bfgball"] => some .bfgBall | ["bfgpuff"] => some .bfgPuff
  | _ => none

private def weaponIdx : Weapon → Nat
  | .fist => 0 | .pistol => 1 | .shotgun => 2 | .chaingun => 3
  | .chainsaw => 4 | .rocket => 5 | .plasma => 6 | .bfg => 7
  | .superShotgun => 8

private def weaponOf : Nat → Weapon
  | 2 => .shotgun | 3 => .chaingun | 4 => .chainsaw
  | 5 => .rocket | 6 => .plasma | 7 => .bfg | 8 => .superShotgun
  | 1 => .pistol | _ => .fist

private def b (v : Bool) : String := if v then "1" else "0"

/-- A deferred "and change" as two space-free tokens; `-` means none. -/
private def chg : Option (String × Nat) → String
  | none => "- 0"
  | some (flat, spec) => s!"{if flat == "" then "-" else flat} {spec}"

private def unchg (flat : String) (spec : String) : Option (String × Nat) :=
  if flat == "-" then none else some (flat, (spec.toNat?).getD 0)

private def packMobjFlags (m : Mobj) : Nat :=
  (if m.awake then 1 else 0) + (if m.ambush then 2 else 0)
    + (if m.justAttacked then 4 else 0) + (if m.corpse then 8 else 0)
    + (if m.charging then 16 else 0) + (if m.raising then 32 else 0)
    + (if m.justHit then 64 else 0) + (if m.dropped then 128 else 0)
    + (if m.entryPending then 256 else 0)

/-- Serialize the whole game.

Fields added since version 1 are *appended* to their line, and the reader
defaults anything a shorter line omits — so a version-1 save still loads.
`skill` gets a line of its own rather than extending `time`, which the
reader destructures exactly. -/
def saveGame (g : GameState) : String := Id.run do
  let mut out := #["dillsave 9", s!"map {g.level.name}",
    s!"time {g.tics} {g.rng.seed.toNat} {g.nextUid}",
    s!"skill {g.skill}",
    s!"stats {g.kills} {g.killTotal} {g.items} {g.itemTotal} \
      {g.secrets} {g.secretTotal}"]
  let p := g.player
  out := out.push s!"player {fx p.x} {fx p.y} {fx p.z} \
    {fx p.momX} {fx p.momY} {fx p.momZ} {fx p.angle} \
    {fx p.eyeHeight} {fx p.eyeDelta}"
  let st := g.status
  out := out.push s!"status {st.health} {st.armor} {st.bullets} {st.shells} \
    {b st.ownsShotgun} {b st.ownsChaingun} {b st.blueKey} {b st.yellowKey} \
    {b st.redKey} {weaponIdx st.weapon} {b st.dead} {b st.god} {b st.noclip} \
    {st.rockets} {st.cells} {b st.ownsChainsaw} {b st.ownsRocket} \
    {b st.ownsPlasma} {b st.ownsBfg} {b st.backpack} \
    {b st.ownsSuperShotgun} {st.invulnTics} {st.invisTics} {st.radsuitTics} \
    {st.gogglesTics} {b st.berserk} {st.berserkTics} {b st.ownsMap} \
    {st.armorType}"
  for i in [0:g.level.sectors.size] do
    let s := g.level.sectors[i]!
    -- the fifth column (sector special) was appended in version 3, the
    -- sixth (floor flat) in version 4: they are what a found secret (9 → 0)
    -- and the "and change" floors mutate. Flat names carry no spaces; `-`
    -- stands in for the (never-seen) empty name.
    out := out.push s!"sec {i} {fx s.floorH} {fx s.ceilH} {s.light} \
      {s.special} {if s.floorFlat == "" then "-" else s.floorFlat}"
  for i in [0:g.level.linedefs.size] do
    let sp := g.level.linedefs[i]!.special
    if sp != 0 then
      out := out.push s!"special {i} {sp}"
  -- the per-sector gunfire alert (vanilla's lingering `soundtarget`), as a
  -- 0/1 string; absent from older saves, where monsters just re-alert
  if !g.alerted.isEmpty then
    out := out.push s!"alerted \
      {String.ofList (g.alerted.toList.map fun b => if b then '1' else '0')}"
  for m in g.movers do
    out := out.push (match m with
      -- the trailing speed column was appended in version 5 (the blazing
      -- door/lift family), version 8 appended one more to most kinds —
      -- the door's `delay`, the plat and crusher `stalled` stasis flag,
      -- and the rising floor's `crush` — and version 9 the lowering
      -- ceiling's `crush`
      | .door s top wait closing stay speed delay =>
        s!"mover door {s} {fx top} {wait} {b closing} {b stay} {fx speed} \
          {delay}"
      | .lift s low high wait rising speed st =>
        s!"mover lift {s} {fx low} {fx high} {wait} {b rising} {fx speed} \
          {b st}"
      | .floorDown s t sp ch => s!"mover fdown {s} {fx t} {fx sp} {chg ch}"
      | .ceiling s t sp cr => s!"mover ceil {s} {fx t} {fx sp} {b cr}"
      | .closeOpen s top wait re => s!"mover c30 {s} {fx top} {wait} {b re}"
      | .crusher s top low d st =>
        s!"mover crush {s} {fx top} {fx low} {b d} {b st}"
      | .perpetual s lo hi w r st =>
        s!"mover perp {s} {fx lo} {fx hi} {w} {b r} {b st}"
      | .floorUp s t sp ch cr =>
        s!"mover fup {s} {fx t} {fx sp} {chg ch} {b cr}")
  for m in g.mobjs do
    if !m.removed then
      out := out.push s!"mobj {encodeKind m.kind} {m.uid} \
        {fx m.x} {fx m.y} {fx m.z} {fx m.angle} \
        {fx m.momX} {fx m.momY} {fx m.momZ} {m.health} {m.state} {m.tics} \
        {m.moveDir} {m.moveCount} {packMobjFlags m} {m.shooterUid} \
        {m.target} {m.threshold} {m.reactionTime} {b m.fromPlayer} \
        {b m.canRespawn} {fx m.spawnX} {fx m.spawnY} {fx m.spawnAngle} \
        {m.respawnTic}"
  out := out.push "end"
  return String.intercalate "\n" out.toList

private def ints (ws : List String) : Option (List Int) :=
  ws.mapM (·.toInt?)

/-- Rebuild a game from a save, against the same WAD. -/
def loadGame (wad : Wad) (text : String) : Except String GameState := do
  let lines := text.splitOn "\n"
  -- The header first: every version of the writer has opened with
  -- `dillsave <n>`, so a file without one is not a save at all, and a
  -- version above ours comes from a newer build whose lines this loader
  -- would half-read. The strict line match below would eventually trip on
  -- one of them, but "too new" is the readable truth.
  match ((lines.head?.getD "").splitOn " ").filter (· != "") with
  | ["dillsave", v] =>
    match v.toNat? with
    | some n =>
      if n > 9 then
        throw s!"save is version {n}, newer than this build reads (9)"
    | none => throw s!"bad save version {v}"
  | _ => throw "not a dill save (no `dillsave <version>` header)"
  let mut mapName := ""
  -- first pass: the map, so the level exists before we lay state over it
  for line in lines do
    if let ["map", name] := line.splitOn " " then
      mapName := name
  if mapName == "" then throw "save has no map line"
  let lvl0 ← Level.load wad mapName
  -- one-shot specials that fired are absent from the save: clear them all,
  -- then restore the listed ones
  let mut level := { lvl0 with
    linedefs := lvl0.linedefs.map ({ · with special := 0 }) }
  let mut player : Player := default
  let mut status : PlayerStatus := {}
  let mut tics := 0
  let mut seed : UInt32 := 1
  let mut nextUid := 1
  let mut movers : Array Mover := #[]
  let mut mobjs : Array Mobj := #[]
  let mut stats : Array Nat := #[0, 0, 0, 0, 0, 0]
  let mut skill := 4
  let mut alerted : Array Bool := #[]
  -- The `end` sentinel closes every save; a file cut off before it is
  -- missing state — trailing mobjs and movers at least, possibly the
  -- player — and must not load as if nothing were wrong.
  let mut sawEnd := false
  for line in lines do
    let ws := (line.splitOn " ").filter (· != "")
    match ws with
    | [] => pure ()               -- a blank line (a trailing newline, say)
    | ["dillsave", _] => pure ()  -- the header, already checked above
    | ["map", _] => pure ()       -- consumed by the first pass
    | ["end"] => sawEnd := true
    | "time" :: rest =>
      match ints rest with
      | some [t, s, u] =>
        tics := t.toNat; seed := UInt32.ofNat s.toNat; nextUid := u.toNat
      | some [t, s, _, u] =>
        -- pre-version-7 saves carried a third column, a noise counter
        -- nothing ever read; skipped
        tics := t.toNat; seed := UInt32.ofNat s.toNat; nextUid := u.toNat
      | _ => throw "bad time line"
    | ["skill", s] =>
      -- absent in a version-1 save — the initial 4 (Ultra-Violence) is the
      -- old default — but a line that is present must parse
      let some sk := s.toNat? | throw "bad skill line"
      skill := sk
    | "stats" :: rest =>
      -- every writer version has emitted all six tallies
      let some vals := ints rest | throw "bad stats line"
      let [k, kt, i, it, sc, st] := vals | throw "bad stats line"
      stats := #[k.toNat, kt.toNat, i.toNat, it.toNat, sc.toNat, st.toNat]
    | "player" :: rest =>
      let some vals := ints rest | throw "bad player line"
      let [x, y, z, mx, my, mz, a] := vals.take 7 | throw "bad player line"
      -- the eye spring was appended later; an older save just stands upright
      let ext := vals.drop 7
      player := { x := unfx x, y := unfx y, z := unfx z
                  momX := unfx mx, momY := unfx my, momZ := unfx mz
                  angle := unfx a
                  eyeHeight := unfx (ext[0]?.getD (fx Player.viewHeight))
                  eyeDelta := unfx (ext[1]?.getD 0) }
    | "status" :: rest =>
      let some (h :: ar :: bu :: sh :: os :: oc :: bk :: yk :: rk :: w
          :: dd :: extra) := ints rest
        | throw "bad status line"
      if w < 0 || w > 8 then throw s!"status weapon index {w} out of range"
      status := { health := h, armor := ar
                  bullets := bu.toNat, shells := sh.toNat
                  ownsShotgun := os == 1, ownsChaingun := oc == 1
                  blueKey := bk == 1, yellowKey := yk == 1, redKey := rk == 1
                  weapon := weaponOf w.toNat, dead := dd == 1
                  god := extra[0]? == some 1
                  noclip := extra[1]? == some 1
                  rockets := (extra[2]?.getD 0).toNat
                  cells := (extra[3]?.getD 0).toNat
                  ownsChainsaw := extra[4]? == some 1
                  ownsRocket := extra[5]? == some 1
                  ownsPlasma := extra[6]? == some 1
                  ownsBfg := extra[7]? == some 1
                  backpack := extra[8]? == some 1
                  -- appended after version 1; absent means "off"
                  ownsSuperShotgun := extra[9]? == some 1
                  invulnTics := (extra[10]?.getD 0).toNat
                  invisTics := (extra[11]?.getD 0).toNat
                  radsuitTics := (extra[12]?.getD 0).toNat
                  gogglesTics := (extra[13]?.getD 0).toNat
                  berserk := extra[14]? == some 1
                  berserkTics := (extra[15]?.getD 0).toNat
                  ownsMap := extra[16]? == some 1
                  -- Saves from before armour had a type carry only points.
                  -- Read them the way they behaved: over 100 can only have
                  -- come from a blue jacket, anything else counts as green.
                  armorType := match extra[17]? with
                    | some t => t.toNat
                    | none => if ar > 100 then 2 else if ar > 0 then 1 else 0 }
    | "sec" :: rest =>
      -- 4 numeric columns through version 2; version 3 appended the sector
      -- special, version 4 the floor flat (a name, so it is split off
      -- before the numeric parse)
      let some (i :: f :: c :: l :: restv) := ints (rest.take 5)
        | throw "bad sec line"
      if i < 0 || i.toNat ≥ level.sectors.size then
        throw s!"sec index {i} out of range ({level.sectors.size} sectors)"
      let flat := (rest.drop 5).head?
      let sectors := level.sectors.modify i.toNat fun s =>
        { s with floorH := unfx f, ceilH := unfx c, light := l.toNat
                 special := match restv with
                   | sp :: _ => sp.toNat
                   | [] => s.special
                 floorFlat := match flat with
                   | some fl => if fl == "-" then s.floorFlat else fl
                   | none => s.floorFlat }
      level := { level with sectors }
    | "special" :: rest =>
      let some [i, sp] := ints rest | throw "bad special line"
      if i < 0 || i.toNat ≥ level.linedefs.size then
        throw s!"special line index {i} out of range \
          ({level.linedefs.size} linedefs)"
      let linedefs := level.linedefs.modify i.toNat fun l =>
        { l with special := sp.toNat }
      level := { level with linedefs }
    | ["alerted", bits] =>
      -- appended in version 3; ignored (and re-flooded on the next shot)
      -- if the sector count does not line up
      if bits.length == level.sectors.size then
        alerted := (bits.toList.map (· == '1')).toArray
    | ["mover", "door", s, top, wait, closing, stay, speed, delay] =>
      -- version 8 appended the timed-door `delay` countdown
      let some [s, top, wait, c, st, sp, d] :=
        ints [s, top, wait, closing, stay, speed, delay]
        | throw "bad door mover"
      movers := movers.push (.door s.toNat (unfx top) wait.toNat
        (c == 1) (st == 1) (unfx sp) d.toNat)
    | ["mover", "door", s, top, wait, closing, stay, speed] =>
      let some [s, top, wait, c, st, sp] :=
        ints [s, top, wait, closing, stay, speed] | throw "bad door mover"
      movers := movers.push (.door s.toNat (unfx top) wait.toNat
        (c == 1) (st == 1) (unfx sp))
    | ["mover", "door", s, top, wait, closing, stay] =>
      -- pre-version-5 save: no speed column, so it was an ordinary door
      let some [s, top, wait, c, st] := ints [s, top, wait, closing, stay]
        | throw "bad door mover"
      movers := movers.push (.door s.toNat (unfx top) wait.toNat
        (c == 1) (st == 1) Speeds.door)
    | ["mover", "lift", s, low, high, wait, rising, speed, stalled] =>
      -- version 8 appended the stasis flag
      let some [s, lo, hi, w, r, sp, st] :=
        ints [s, low, high, wait, rising, speed, stalled]
        | throw "bad lift mover"
      movers := movers.push
        (.lift s.toNat (unfx lo) (unfx hi) w.toNat (r == 1) (unfx sp)
          (st == 1))
    | ["mover", "lift", s, low, high, wait, rising, speed] =>
      let some [s, lo, hi, w, r, sp] :=
        ints [s, low, high, wait, rising, speed] | throw "bad lift mover"
      movers := movers.push
        (.lift s.toNat (unfx lo) (unfx hi) w.toNat (r == 1) (unfx sp))
    | ["mover", "lift", s, low, high, wait, rising] =>
      -- pre-version-5 save: as above
      let some [s, lo, hi, w, r] := ints [s, low, high, wait, rising]
        | throw "bad lift mover"
      movers := movers.push
        (.lift s.toNat (unfx lo) (unfx hi) w.toNat (r == 1) Speeds.lift)
    | ["mover", "fdown", s, t, sp, cf, cs] =>
      let some [s, t, sp] := ints [s, t, sp] | throw "bad floor mover"
      movers := movers.push (.floorDown s.toNat (unfx t) (unfx sp) (unchg cf cs))
    | ["mover", "fdown", s, t, sp] =>
      let some [s, t, sp] := ints [s, t, sp] | throw "bad floor mover"
      movers := movers.push (.floorDown s.toNat (unfx t) (unfx sp))
    | ["mover", "fdown", s, t] =>
      -- a save from before floors carried their own speed
      let some [s, t] := ints [s, t] | throw "bad floor mover"
      movers := movers.push (.floorDown s.toNat (unfx t) Speeds.floor)
    | ["mover", "fup", s, t, sp, cf, cs, crush] =>
      -- version 8 appended the `raiseFloorCrush` flag
      let some [s, t, sp, cr] := ints [s, t, sp, crush]
        | throw "bad floor mover"
      movers := movers.push
        (.floorUp s.toNat (unfx t) (unfx sp) (unchg cf cs) (cr == 1))
    | ["mover", "fup", s, t, sp, cf, cs] =>
      let some [s, t, sp] := ints [s, t, sp] | throw "bad floor mover"
      movers := movers.push (.floorUp s.toNat (unfx t) (unfx sp) (unchg cf cs))
    | ["mover", "fup", s, t, sp] =>
      let some [s, t, sp] := ints [s, t, sp] | throw "bad floor mover"
      movers := movers.push (.floorUp s.toNat (unfx t) (unfx sp))
    | ["mover", "fup", s, t] =>
      -- The first format wrote every one-way plane as a bare
      -- `mover <kind> <sector> <target>` — the era `fdown` and `ceil` keep
      -- their two-column arms for ("before floors carried their own
      -- speed"). Rising floors existed then too (the raise-floor specials
      -- and the stair builders are as old as the movers), so `fup` gets
      -- the same arm and the same default pace; without it a legacy rising
      -- floor would now be refused as an unrecognized line.
      let some [s, t] := ints [s, t] | throw "bad floor mover"
      movers := movers.push (.floorUp s.toNat (unfx t) Speeds.floor)
    | ["mover", "ceil", s, t, sp, cr] =>
      -- version 9 appended the 44/72 `lowerAndCrush` flag
      let some [s, t, sp, cr] := ints [s, t, sp, cr] | throw "bad ceiling mover"
      movers := movers.push (.ceiling s.toNat (unfx t) (unfx sp) (cr == 1))
    | ["mover", "ceil", s, t, sp] =>
      let some [s, t, sp] := ints [s, t, sp] | throw "bad ceiling mover"
      movers := movers.push (.ceiling s.toNat (unfx t) (unfx sp))
    | ["mover", "ceil", s, t] =>
      -- likewise for ceilings
      let some [s, t] := ints [s, t] | throw "bad ceiling mover"
      movers := movers.push (.ceiling s.toNat (unfx t) Speeds.ceiling)
    | ["mover", "c30", s, top, wait, re] =>
      -- special 16's close-wait-reopen door (saved from version 3 on)
      let some [s, top, w, re] := ints [s, top, wait, re]
        | throw "bad close30 mover"
      movers := movers.push (.closeOpen s.toNat (unfx top) w.toNat (re == 1))
    | ["mover", "crush", s, top, low, d, stalled] =>
      -- version 8 appended the stasis flag
      let some [s, top, low, d, st] := ints [s, top, low, d, stalled]
        | throw "bad crusher mover"
      movers := movers.push
        (.crusher s.toNat (unfx top) (unfx low) (d == 1) (st == 1))
    | ["mover", "crush", s, top, low, d] =>
      let some [s, top, low, d] := ints [s, top, low, d]
        | throw "bad crusher mover"
      movers := movers.push (.crusher s.toNat (unfx top) (unfx low) (d == 1))
    | ["mover", "perp", s, lo, hi, w, r, stalled] =>
      -- version 8 appended the stasis flag
      let some [s, lo, hi, w, r, st] := ints [s, lo, hi, w, r, stalled]
        | throw "bad perpetual mover"
      movers := movers.push
        (.perpetual s.toNat (unfx lo) (unfx hi) w.toNat (r == 1) (st == 1))
    | ["mover", "perp", s, lo, hi, w, r] =>
      let some [s, lo, hi, w, r] := ints [s, lo, hi, w, r]
        | throw "bad perpetual mover"
      movers := movers.push
        (.perpetual s.toNat (unfx lo) (unfx hi) w.toNat (r == 1))
    | "mobj" :: kindStr :: rest =>
      let some kind := decodeKind kindStr | throw s!"bad mobj kind {kindStr}"
      let some vals := ints rest | throw "bad mobj line"
      -- version 1 wrote the first fifteen columns; anything past them was
      -- appended later and defaults when a shorter (older) line omits it
      let [uid, x, y, z, a, mx, my, mz, h, st, ti, md, mc, fl, sh] :=
        vals.take 15 | throw "bad mobj line"
      let ext := vals.drop 15
      let info := ActorInfo.ofKind kind
      -- Semantic checks: these indices feed panicking `[...]!` lookups at
      -- tick and render time, so a stale save (state tables change between
      -- versions) must fail here, as a readable error, not there.
      if st < 0 || st.toNat ≥ info.states.size then
        throw s!"mobj state {st} out of range for {kindStr} \
          ({info.states.size} states)"
      if md < 0 || md.toNat ≥ 8 then
        throw s!"mobj moveDir {md} out of range for {kindStr} (0–7)"
      -- `spawn` mints uids from 1 up: a `target` of 0 means "the player"
      -- (see `EnemyTarget`), so a body claiming uid 0 would be untargetable
      if uid ≤ 0 then
        throw s!"mobj uid {uid} out of range (0 is the player-target sentinel)"
      mobjs := mobjs.push
        { uid := uid.toNat, kind, info
          x := unfx x, y := unfx y, z := unfx z, angle := unfx a
          momX := unfx mx, momY := unfx my, momZ := unfx mz
          health := h, state := st.toNat, tics := ti
          moveDir := md.toNat, moveCount := mc.toNat
          awake := fl.toNat &&& 1 != 0, ambush := fl.toNat &&& 2 != 0
          justAttacked := fl.toNat &&& 4 != 0, corpse := fl.toNat &&& 8 != 0
          charging := fl.toNat &&& 16 != 0, raising := fl.toNat &&& 32 != 0
          justHit := fl.toNat &&& 64 != 0, dropped := fl.toNat &&& 128 != 0
          entryPending := fl.toNat &&& 256 != 0
          shooterUid := sh.toNat
          target := (ext[0]?.getD 0).toNat
          threshold := (ext[1]?.getD 0).toNat
          reactionTime := (ext[2]?.getD 0).toNat
          -- without this a player's in-flight rocket reloads as hostile
          fromPlayer := ext[3]? == some 1
          canRespawn := ext[4]? == some 1
          spawnX := unfx (ext[5]?.getD 0), spawnY := unfx (ext[6]?.getD 0)
          spawnAngle := unfx (ext[7]?.getD 0)
          respawnTic := (ext[8]?.getD 0).toNat }
    | _ =>
      -- Every line kind any writer version (≤ 8) ever produced has an arm
      -- above, the short legacy forms included — so an unmatched line can
      -- only be corruption, most likely a save truncated mid-line, whose
      -- half-written mover a silent skip would simply drop.
      throw s!"unrecognized save line: {line}"
  if !sawEnd then
    throw "save truncated: the `end` sentinel never arrived"
  -- Nightmare's fast actor tables travel on the mobj (`spawnMobj` applies
  -- them at skill 5), but the lines above rebuilt every mobj from the
  -- ordinary `ActorInfo.ofKind` — reapply them, now that the whole file
  -- (skill included, wherever its line fell) has been read.
  if skill == 5 then
    mobjs := mobjs.map fun m => { m with info := ActorInfo.fast m.kind m.info }
  -- every mover steps a sector it names with `[...]!`: check them all here.
  -- One mover per sector is an invariant `addMover`/`addDoor` maintain
  -- (vanilla's `sec->specialdata`), and two loaded movers naming the same
  -- sector would fight over its heights every tic. The writer serializes a
  -- state that keeps the invariant, so a file breaking it is corrupt and
  -- refused like any other bad index — not silently thinned to the first.
  let mut ownedSectors : Array Nat := #[]
  for m in movers do
    if m.sector ≥ level.sectors.size then
      throw s!"mover sector {m.sector} out of range \
        ({level.sectors.size} sectors)"
    if ownedSectors.contains m.sector then
      throw s!"two movers claim sector {m.sector}"
    ownedSectors := ownedSectors.push m.sector
  -- A mobj's uid is likewise its identity: `target` and `shooterUid` resolve
  -- through `uidIndex`, keyed by uid, so two bodies sharing one would
  -- silently alias every reference onto whichever `rebuildIndexes` kept.
  -- The writer serializes live mobjs whose uids `spawn` minted uniquely; a
  -- file breaking that is corrupt and refused like a duplicate mover.
  let mut seenUids : Std.HashMap Nat Unit := {}
  for m in mobjs do
    if seenUids.contains m.uid then
      throw s!"two mobjs claim uid {m.uid}"
    seenUids := seenUids.insert m.uid ()
  -- `spawn` hands out `nextUid` and increments it, and `uidIndex` is keyed by
  -- uid — so a `nextUid` that does not clear every uid in the file would mint
  -- duplicates, and `mobjIdx?` would then resolve a monster's target, a
  -- missile's shooter or a revenant tracer's quarry onto the wrong body. The
  -- file is the only thing that could get this wrong, and raising the counter
  -- is always safe, so repair it rather than refusing a save that is
  -- otherwise perfectly loadable.
  nextUid := mobjs.foldl (fun n m => max n (m.uid + 1)) nextUid
  let g : GameState :=
    { level, player, status, mobjs, movers, tics, nextUid, skill
      alerted
      rng := { seed }
      kills := stats[0]!, killTotal := stats[1]!
      items := stats[2]!, itemTotal := stats[3]!
      secrets := stats[4]!, secretTotal := stats[5]! }
  let g := g.rebuildIndexes
  -- Light thinkers are rebuilt, not saved — but against the *pristine* WAD
  -- level (`lvl0`), never the saved sector lights: `spawnLights` reads each
  -- sector's current light as the effect's bright ceiling, and a strobe
  -- saved mid-dark-phase would reload with its range clamped to the dark
  -- value, ratcheting the map darker on every save/load cycle. Spawn-time
  -- lights are exactly the WAD's, so this reproduces the original ranges.
  -- `spawnLights` also rolls each blinking light's opening phase, which
  -- would leave the restored game on a different point in the random stream
  -- than the one it was saved from — and a loaded game has to replay
  -- identically. Put the saved seed back afterwards.
  let pristine := { g with level := lvl0 }.spawnLights
  return { g with lights := pristine.lights, rng := { seed } }

end Dill.Save
