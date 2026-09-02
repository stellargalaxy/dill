import Dill.Game.Info

/-!
# Sound effect catalog

An `Sfx` is an index into `Sfx.lumps` (the `DS*` DMX lumps of the WAD).
The core emits `(sfx, x, y)` events into `GameState.sounds`; the shell
plays them with distance attenuation. Which actor makes which noise
follows vanilla's `sfxinfo`.
-/

namespace Dill

abbrev Sfx := Nat

namespace Sfx

/-- Lump names, in index order. -/
def lumps : Array String := #[
  "DSPISTOL", "DSSHOTGN", "DSPUNCH",  "DSCLAW",   "DSSGTATK",
  "DSFIRSHT", "DSFIRXPL", "DSBAREXP", "DSITEMUP", "DSWPNUP",
  "DSDOROPN", "DSDORCLS", "DSPSTART", "DSPSTOP",  "DSSWTCHN",
  "DSPLPAIN", "DSPLDETH", "DSPOPAIN", "DSDMPAIN", "DSPOSIT1",
  "DSBGSIT1", "DSSGTSIT", "DSPODTH1", "DSPODTH2", "DSBGDTH1",
  "DSSGTDTH", "DSOOF",  "DSTELEPT", "DSCACSIT", "DSCACDTH",
  "DSBRSSIT", "DSBRSDTH", "DSSKLATK", "DSPLASMA", "DSRLAUNC",
  "DSBFG",    "DSSLOP",   "DSHOOF",   "DSMETAL",  "DSCYBSIT",
  "DSCYBDTH", "DSSPISIT", "DSSPIDTH",
  -- Doom II's additions; absent from a Doom 1 IWAD, where `loadSounds`
  -- simply skips them and those monsters never appear anyway.
  "DSKNTSIT", "DSKNTDTH", "DSMANSIT", "DSMANDTH", "DSMNPAIN",
  "DSBSPSIT", "DSBSPDTH", "DSBSPWLK", "DSSKESIT", "DSSKEDTH",
  "DSSKEATK", "DSSKESWG", "DSSKEPCH", "DSSSSIT",   "DSSSDTH",
  "DSPOSIT2", "DSPOSIT3", "DSPODTH3", "DSDSHTGN",
  "DSSAWUP",  "DSSAWIDL", "DSSAWFUL", "DSSAWHIT", "DSSTNMOV",
  "DSBOSPN",
  -- pain elemental, arch-vile, the mancubus/keen attack cues, and every
  -- monster's periodic "active" growl (`activesound`)
  "DSPESIT",  "DSPEPAIN", "DSPEDTH",
  "DSVILSIT", "DSVIPAIN", "DSVILDTH", "DSVILATK", "DSVILACT",
  "DSMANATK", "DSKEENPN", "DSKEENDT",
  "DSPOSACT", "DSBGACT",  "DSDMACT",  "DSBSPACT", "DSSKEACT",
  -- the Icon of Sin's own voice: it wakes, spits a cube, the cube hums in
  -- flight, and it dies. Doom II only; skipped on a Doom 1 IWAD.
  "DSBOSSIT", "DSBOSPIT", "DSBOSCUB", "DSBOSDTH",
  -- The imp's second sight and death grunts. Vanilla rolls between the two
  -- of each (and among the three former-human variants above), which is what
  -- keeps a room full of the same monster from speaking in unison.
  "DSBGSIT2", "DSBGDTH2",
  -- the BFG ball's burst (vanilla MT_BFG deathsound `sfx_rxplod`)
  "DSRXPLOD",
  -- powerup pickups swell (`sfx_getpow`) where ordinary bonuses blip
  "DSGETPOW",
  -- the exit switch's own heavier clunk (`sfx_swtchx`)
  "DSSWTCHX",
  -- the super shotgun's break-load-snap reload (Doom II lumps; the loader
  -- skips them on a Doom 1 IWAD, as it does every absent lump here)
  "DSDBOPN", "DSDBLOAD", "DSDBCLS",
  -- the arch-vile's flame igniting and roaring (Doom II)
  "DSFLAMST", "DSFLAME",
  -- the over-50-overkill death scream (`sfx_pdiehi`, commercial IWADs)
  "DSPDIEHI"]

def pistol   : Sfx := 0
def shotgun  : Sfx := 1
def punch    : Sfx := 2
def claw     : Sfx := 3
def sargAtk  : Sfx := 4
def fireShot : Sfx := 5
def fireExpl : Sfx := 6
def barExp   : Sfx := 7
def itemUp   : Sfx := 8
def weapUp   : Sfx := 9
def doorOpen : Sfx := 10
def doorClose : Sfx := 11
def platStart : Sfx := 12
def platStop : Sfx := 13
def switchOn : Sfx := 14
def plPain   : Sfx := 15
def plDeath  : Sfx := 16
def poPain   : Sfx := 17
def dmPain   : Sfx := 18
def poSight  : Sfx := 19
def bgSight  : Sfx := 20
def sgtSight : Sfx := 21
def poDeath1 : Sfx := 22
def poDeath2 : Sfx := 23
def bgDeath  : Sfx := 24
def sgtDeath : Sfx := 25
def oof      : Sfx := 26
def teleport : Sfx := 27
def cacSight : Sfx := 28
def cacDeath : Sfx := 29
def brsSight : Sfx := 30
def brsDeath : Sfx := 31
def sklAttack : Sfx := 32
def plasma   : Sfx := 33
def rocket   : Sfx := 34
def bfg      : Sfx := 35
/-- The wet burst when a body is gibbed (extreme death). -/
def slop     : Sfx := 36
/-- The episode bosses: the Cyberdemon's hoof-fall and the heavy tread both
it and the Spider Mastermind put down as they walk. -/
def hoof     : Sfx := 37
def metal    : Sfx := 38
def cybSight : Sfx := 39
def cybDeath : Sfx := 40
def spiSight : Sfx := 41
def spiDeath : Sfx := 42
/-- Doom II's bestiary and the super shotgun. -/
def kntSight : Sfx := 43
def kntDeath : Sfx := 44
def manSight : Sfx := 45
def manDeath : Sfx := 46
def manPain  : Sfx := 47
def bspSight : Sfx := 48
def bspDeath : Sfx := 49
def bspWalk  : Sfx := 50
def skeSight : Sfx := 51
def skeDeath : Sfx := 52
def skeAttack : Sfx := 53
def skeSwing : Sfx := 54
def skePunch : Sfx := 55
def ssSight  : Sfx := 56
def ssDeath  : Sfx := 57
def poSight2 : Sfx := 58
def poSight3 : Sfx := 59
def poDeath3 : Sfx := 60
def superShotgun : Sfx := 61
/-- The chainsaw: revving up, idling, cutting air, and biting. -/
def sawUp   : Sfx := 62
def sawIdle : Sfx := 63
def sawFull : Sfx := 64
def sawHit  : Sfx := 65
/-- The grind a floor or ceiling makes while it travels. -/
def stoneMove : Sfx := 66
/-- The Icon of Sin's brain grunting when rocketed (`A_BrainPain`). -/
def bosPain : Sfx := 67
/-- The pain elemental: it sights, flinches, and bursts. -/
def peSight : Sfx := 68
def pePain  : Sfx := 69
def peDeath : Sfx := 70
/-- The arch-vile: sight, pain, death, the whoosh as it aims its flame, and
its own guttural idle. -/
def vilSight  : Sfx := 71
def vilPain   : Sfx := 72
def vilDeath  : Sfx := 73
def vilAttack : Sfx := 74
def vilActive : Sfx := 75
/-- The mancubus rearing back to fire (`A_FatRaise`). -/
def manAttack : Sfx := 76
/-- Commander Keen's pain and death (MAP32). -/
def keenPain  : Sfx := 77
def keenDeath : Sfx := 78
/-- The periodic idle mutters monsters make while hunting (`activesound`):
the former humans, the imp, the fleshy horde, the arachnotron, the revenant. -/
def posActive : Sfx := 79
def bgActive  : Sfx := 80
def dmActive  : Sfx := 81
def bspActive : Sfx := 82
def skeActive : Sfx := 83
/-- The Icon of Sin (MAP30): the boss brain waking (`A_BrainAwake`), the
spitter flinging a cube (`A_BrainSpit`), the cube humming in flight, and the
brain's death roar (`A_BrainScream`). All play at full volume in vanilla. -/
def bosSight : Sfx := 84
def bosSpit  : Sfx := 85
def bosCube  : Sfx := 86
def bosDeath : Sfx := 87
/-- The imp's alternates, rolled against `bgSight`/`bgDeath`. -/
def bgSight2 : Sfx := 88
def bgDeath2 : Sfx := 89
/-- The BFG ball detonating (vanilla `sfx_rxplod`). -/
def rxplod : Sfx := 90
/-- A powerup's swell (vanilla `sfx_getpow`). -/
def getPow : Sfx := 91
/-- The exit switch's heavier clunk (vanilla `sfx_swtchx`). -/
def swtchx : Sfx := 92
/-- The super shotgun breaking open, loading, snapping shut. -/
def dbOpn  : Sfx := 93
def dbLoad : Sfx := 94
def dbCls  : Sfx := 95
/-- The arch-vile's flame igniting, then roaring (vanilla `sfx_flamst`/`sfx_flame`). -/
def flameStart : Sfx := 96
def flame      : Sfx := 97
/-- The overkill death scream (vanilla `sfx_pdiehi`, health below -50). -/
def plDeathHi  : Sfx := 98

/-- Vanilla `A_Look` and `A_Scream` do not play a monster's catalogued sight
or death cry directly — they roll among its variants first: any of the three
former-human grunts, or either of the imp's two. Everything else speaks with
its single voice. -/
def varied (s : Sfx) (roll : Nat) : Sfx :=
  if s == poSight || s == poSight2 || s == poSight3 then
    #[poSight, poSight2, poSight3][roll % 3]!
  else if s == poDeath1 || s == poDeath2 || s == poDeath3 then
    #[poDeath1, poDeath2, poDeath3][roll % 3]!
  else if s == bgSight then (if roll % 2 == 0 then bgSight else bgSight2)
  else if s == bgDeath then (if roll % 2 == 0 then bgDeath else bgDeath2)
  else s

end Sfx

namespace ActorKind

def sightSfx : ActorKind → Option Sfx
  | .zombieman => some Sfx.poSight
  -- vanilla seeds the shotgun guy (and the chaingunner) with DSPOSIT2;
  -- `Sfx.varied` still rolls among all three former-human grunts
  | .shotgunGuy => some Sfx.poSight2
  | .imp => some Sfx.bgSight
  | .demon | .spectre => some Sfx.sgtSight
  | .cacodemon => some Sfx.cacSight
  | .baron => some Sfx.brsSight
  | .cyberdemon => some Sfx.cybSight
  | .spiderMastermind => some Sfx.spiSight
  | .hellKnight => some Sfx.kntSight
  | .mancubus => some Sfx.manSight
  | .arachnotron => some Sfx.bspSight
  | .revenant => some Sfx.skeSight
  | .chaingunner => some Sfx.poSight2
  | .wolfSS => some Sfx.ssSight
  | .painElemental => some Sfx.peSight
  | .archVile => some Sfx.vilSight
  | _ => none

def painSfx : ActorKind → Option Sfx
  | .zombieman | .shotgunGuy | .imp => some Sfx.poPain
  | .demon | .spectre | .cacodemon | .lostSoul | .baron => some Sfx.dmPain
  | .cyberdemon | .spiderMastermind => some Sfx.dmPain
  | .mancubus => some Sfx.manPain
  | .hellKnight | .arachnotron => some Sfx.dmPain
  -- the revenant grunts like the humans (MT_UNDEAD painsound sfx_popain)
  | .chaingunner | .wolfSS | .revenant => some Sfx.poPain
  | .iconBrain => some Sfx.bosPain
  | .painElemental => some Sfx.pePain
  | .archVile => some Sfx.vilPain
  | .commanderKeen => some Sfx.keenPain
  | _ => none

/-- Played by the `scream` action in each death sequence — and, for the
missiles, by `explodeMissile` on impact (vanilla plays the mobj's
`deathsound` in both paths): rockets and the revenant's tracer burst with
DSBAREXP, the fireball family with DSFIRXPL, the BFG ball with DSRXPLOD. -/
def deathSfx : ActorKind → Option Sfx
  | .rocket | .revenantMissile => some Sfx.barExp
  | .impBall | .cacoBall | .baronBall | .fatShot
  | .plasmaBall | .arachPlasma => some Sfx.fireExpl
  | .bfgBall => some Sfx.rxplod
  | .zombieman => some Sfx.poDeath1
  | .shotgunGuy => some Sfx.poDeath2
  | .imp => some Sfx.bgDeath
  | .demon | .spectre => some Sfx.sgtDeath
  | .cacodemon => some Sfx.cacDeath
  | .baron => some Sfx.brsDeath
  | .lostSoul => some Sfx.fireExpl
  | .barrel => some Sfx.barExp
  | .cyberdemon => some Sfx.cybDeath
  | .spiderMastermind => some Sfx.spiDeath
  | .hellKnight => some Sfx.kntDeath
  | .mancubus => some Sfx.manDeath
  | .arachnotron => some Sfx.bspDeath
  | .revenant => some Sfx.skeDeath
  -- vanilla seeds MT_CHAINGUY with DSPODTH2 (DSPODTH3 only comes up on
  -- `Sfx.varied`'s roll, as for the other former humans)
  | .chaingunner => some Sfx.poDeath2
  | .wolfSS => some Sfx.ssDeath
  | .painElemental => some Sfx.peDeath
  | .archVile => some Sfx.vilDeath
  | .commanderKeen => some Sfx.keenDeath
  | _ => none

/-- The idle mutter a monster makes at random while hunting (vanilla
`activesound`, rolled at 3/256 in `A_Chase`). Keen is silent; the lost soul
mutters `DSDMACT` like the rest of the fleshy horde (info.c `MT_SKULL`). -/
def activeSfx : ActorKind → Option Sfx
  | .zombieman | .shotgunGuy | .chaingunner | .wolfSS | .mancubus =>
    some Sfx.posActive
  | .imp => some Sfx.bgActive
  | .demon | .spectre | .cacodemon | .baron | .hellKnight | .lostSoul
  | .cyberdemon | .spiderMastermind | .painElemental => some Sfx.dmActive
  | .arachnotron => some Sfx.bspActive
  | .revenant => some Sfx.skeActive
  | .archVile => some Sfx.vilActive
  | _ => none

/-- The sound a projectile makes leaving the barrel (vanilla plays the missile
mobj's `seesound` in `P_SpawnMissile`). The revenant's tracer is silent — its
launch cue comes from `A_SkelMissile` instead. -/
def launchSfx : ActorKind → Option Sfx
  | .impBall | .cacoBall | .baronBall | .fatShot => some Sfx.fireShot
  | .rocket => some Sfx.rocket
  | .arachPlasma | .plasmaBall => some Sfx.plasma
  | _ => none

end ActorKind
end Dill
