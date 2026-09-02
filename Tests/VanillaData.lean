/-!
# VanillaData — vanilla Doom reference values for DILL fidelity diffing

Golden data transcribed **mechanically** from the id Software source
(github.com/id-Software/DOOM, linuxdoom-1.10) — info.c (`states[]`,
`mobjinfo[]`), d_items.c (`weaponinfo[]`), p_spec.c (`animdefs[]`),
p_switch.c (`alphSwitchList[]`), sounds.c (`S_sfx[]`), and the constants of
p_spec.h / p_local.h / p_pspr.c. Curled copies of those files sit in the
scratchpad directory this file was generated from; a generator script
(`gen.py`) parsed them — no value here was typed from memory.

This file deliberately does **not** import Dill: it uses only primitive
types, so it compiles standalone and the later diff against
`Dill.Game.Info` / `Dill.Game.Player` / `Dill.Assets` can be an ordinary
Lean (or eyeball) comparison without entangling the two representations.

## Representation of state chains

Vanilla's `states[]` is one flat array; DILL keeps a local array per actor
with `spawn`/`see`/... entry indices. To diff without baking DILL's layout
into the vanilla data, each actor here carries one `Chain` per vanilla
entry point (`spawnstate`, `seestate`, `painstate`, `meleestate`,
`missilestate`, `deathstate`, `xdeathstate`, `raisestate`), walked through
`nextstate` until it
- reaches a `tics = -1` state (`Ending.halt` — the mobj holds that frame
  forever; vanilla still stores a nominal nextstate but never advances),
- reaches `S_NULL` (`Ending.remove` — the mobj is removed),
- re-enters a state already recorded for this actor
  (`Ending.loops entry offset` — e.g. a pain chain falling back into the
  see chain at offset 0, or a spawn chain looping to its own start).
Chains are walked in the entry order above, so shared states land in the
earliest chain that reaches them (e.g. the cacodemon's `meleestate` and
`missilestate` are the same vanilla state: `melee` carries the states and
`missile` is `⟨[], loops "melee" 0⟩`).

Each state records the **sprite family of that state** (vanilla states
carry their sprite; DILL uses a per-actor sprite plus `spriteOverride` —
death chains that change family, e.g. barrel → BEXP, show up here as a
different sprite string mid-chain), the frame letter (`frame & 0x7fff`
past `'A'`; the arch-vile heal frames run past `'Z'` into `'['`, `'\'`,
`']'`), the tics, the vanilla action name (`""` for NULL), and the
FF_FULLBRIGHT bit (`frame & 0x8000`).

`misc1`/`misc2` are 0 for every vanilla state and are dropped.
-/

namespace VanillaData

/-- One vanilla `state_t`: sprite family, frame letter, tics, action
(`""` = NULL), fullbright flag. -/
structure S where
  sprite : String
  frame  : Char
  tics   : Int
  action : String
  bright : Bool
  deriving Repr, DecidableEq

/-- How a walked chain of `nextstate`s ends (see the module comment). -/
inductive Ending where
  | remove
  | halt
  | loops (entry : String) (offset : Nat)
  deriving Repr, DecidableEq

/-- The chain of states from one mobjinfo entry point. -/
structure Chain where
  entry  : String
  states : Array S
  ending : Ending
  deriving Repr

/-- One vanilla `mobjinfo_t` entry, restricted to the fields DILL models
plus provenance. Units: `radius`/`height` in map units (`N*FRACUNIT`
divided out); `speed` is map units per `P_Move` step for walkers and map
units per tic for missiles (vanilla writes the latter `N*FRACUNIT`; the
comment on each converted value quotes the original expression).
`painChance` is out of 256. `damage` is the raw vanilla missile damage
multiplier (a hit deals `((P_Random()%8)+1) * damage`). Sounds are DS lump
names in vanilla's lowercase spelling (`"dsposit1"`); `""` = `sfx_None`.
Flags: the named booleans are the ones DILL models (its field names:
`special` = DILL `pickup`, `float` = DILL `flying`, `spawnCeiling` = DILL
`ceilingHang`); any remaining vanilla flags land verbatim in
`otherFlags` so nothing is silently dropped. -/
structure Mobj where
  mt : String
  dillKind : String
  doomednum : Int
  health : Int
  speed : Float
  radius : Float
  height : Float
  mass : Int
  painChance : Nat
  damage : Nat
  reactionTime : Nat
  seeSound : String := ""
  attackSound : String := ""
  painSound : String := ""
  deathSound : String := ""
  activeSound : String := ""
  solid : Bool := false
  shootable : Bool := false
  special : Bool := false
  missile : Bool := false
  noBlood : Bool := false
  float : Bool := false
  noGravity : Bool := false
  shadow : Bool := false
  countKill : Bool := false
  countItem : Bool := false
  spawnCeiling : Bool := false
  dropOff : Bool := false
  noSector : Bool := false
  noBlockmap : Bool := false
  otherFlags : Array String := #[]
  chains : Array Chain
  deriving Repr

/-! ## mobjinfo[] + states[] — info.c

One `Mobj` per DILL `ActorKind`, in `ActorKind` declaration order. -/

def zombieman : Mobj := {
  mt := "MT_POSSESSED", dillKind := "zombieman"
  doomednum := 3004
  health := 20
  speed := 8
  radius := 20, height := 56, mass := 100
  painChance := 200, damage := 0, reactionTime := 8
  seeSound := "dsposit1", attackSound := "dspistol", painSound := "dspopain", deathSound := "dspodth1", activeSound := "dsposact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"POSS", 'A', 10, "A_Look", false⟩,   -- S_POSS_STND
        ⟨"POSS", 'B', 10, "A_Look", false⟩   -- S_POSS_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"POSS", 'A', 4, "A_Chase", false⟩,   -- S_POSS_RUN1
        ⟨"POSS", 'A', 4, "A_Chase", false⟩,   -- S_POSS_RUN2
        ⟨"POSS", 'B', 4, "A_Chase", false⟩,   -- S_POSS_RUN3
        ⟨"POSS", 'B', 4, "A_Chase", false⟩,   -- S_POSS_RUN4
        ⟨"POSS", 'C', 4, "A_Chase", false⟩,   -- S_POSS_RUN5
        ⟨"POSS", 'C', 4, "A_Chase", false⟩,   -- S_POSS_RUN6
        ⟨"POSS", 'D', 4, "A_Chase", false⟩,   -- S_POSS_RUN7
        ⟨"POSS", 'D', 4, "A_Chase", false⟩   -- S_POSS_RUN8
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"POSS", 'G', 3, "", false⟩,   -- S_POSS_PAIN
        ⟨"POSS", 'G', 3, "A_Pain", false⟩   -- S_POSS_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"POSS", 'E', 10, "A_FaceTarget", false⟩,   -- S_POSS_ATK1
        ⟨"POSS", 'F', 8, "A_PosAttack", false⟩,   -- S_POSS_ATK2
        ⟨"POSS", 'E', 8, "", false⟩   -- S_POSS_ATK3
      ],
      ending := .loops "see" 0 },
    { entry := "death",
      states := #[
        ⟨"POSS", 'H', 5, "", false⟩,   -- S_POSS_DIE1
        ⟨"POSS", 'I', 5, "A_Scream", false⟩,   -- S_POSS_DIE2
        ⟨"POSS", 'J', 5, "A_Fall", false⟩,   -- S_POSS_DIE3
        ⟨"POSS", 'K', 5, "", false⟩,   -- S_POSS_DIE4
        ⟨"POSS", 'L', -1, "", false⟩   -- S_POSS_DIE5
      ],
      ending := .halt },
    { entry := "xdeath",
      states := #[
        ⟨"POSS", 'M', 5, "", false⟩,   -- S_POSS_XDIE1
        ⟨"POSS", 'N', 5, "A_XScream", false⟩,   -- S_POSS_XDIE2
        ⟨"POSS", 'O', 5, "A_Fall", false⟩,   -- S_POSS_XDIE3
        ⟨"POSS", 'P', 5, "", false⟩,   -- S_POSS_XDIE4
        ⟨"POSS", 'Q', 5, "", false⟩,   -- S_POSS_XDIE5
        ⟨"POSS", 'R', 5, "", false⟩,   -- S_POSS_XDIE6
        ⟨"POSS", 'S', 5, "", false⟩,   -- S_POSS_XDIE7
        ⟨"POSS", 'T', 5, "", false⟩,   -- S_POSS_XDIE8
        ⟨"POSS", 'U', -1, "", false⟩   -- S_POSS_XDIE9
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"POSS", 'K', 5, "", false⟩,   -- S_POSS_RAISE1
        ⟨"POSS", 'J', 5, "", false⟩,   -- S_POSS_RAISE2
        ⟨"POSS", 'I', 5, "", false⟩,   -- S_POSS_RAISE3
        ⟨"POSS", 'H', 5, "", false⟩   -- S_POSS_RAISE4
      ],
      ending := .loops "see" 0 }
  ] }

def shotgunGuy : Mobj := {
  mt := "MT_SHOTGUY", dillKind := "shotgunGuy"
  doomednum := 9
  health := 30
  speed := 8
  radius := 20, height := 56, mass := 100
  painChance := 170, damage := 0, reactionTime := 8
  seeSound := "dsposit2", painSound := "dspopain", deathSound := "dspodth2", activeSound := "dsposact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"SPOS", 'A', 10, "A_Look", false⟩,   -- S_SPOS_STND
        ⟨"SPOS", 'B', 10, "A_Look", false⟩   -- S_SPOS_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"SPOS", 'A', 3, "A_Chase", false⟩,   -- S_SPOS_RUN1
        ⟨"SPOS", 'A', 3, "A_Chase", false⟩,   -- S_SPOS_RUN2
        ⟨"SPOS", 'B', 3, "A_Chase", false⟩,   -- S_SPOS_RUN3
        ⟨"SPOS", 'B', 3, "A_Chase", false⟩,   -- S_SPOS_RUN4
        ⟨"SPOS", 'C', 3, "A_Chase", false⟩,   -- S_SPOS_RUN5
        ⟨"SPOS", 'C', 3, "A_Chase", false⟩,   -- S_SPOS_RUN6
        ⟨"SPOS", 'D', 3, "A_Chase", false⟩,   -- S_SPOS_RUN7
        ⟨"SPOS", 'D', 3, "A_Chase", false⟩   -- S_SPOS_RUN8
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"SPOS", 'G', 3, "", false⟩,   -- S_SPOS_PAIN
        ⟨"SPOS", 'G', 3, "A_Pain", false⟩   -- S_SPOS_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"SPOS", 'E', 10, "A_FaceTarget", false⟩,   -- S_SPOS_ATK1
        ⟨"SPOS", 'F', 10, "A_SPosAttack", true⟩,   -- S_SPOS_ATK2
        ⟨"SPOS", 'E', 10, "", false⟩   -- S_SPOS_ATK3
      ],
      ending := .loops "see" 0 },
    { entry := "death",
      states := #[
        ⟨"SPOS", 'H', 5, "", false⟩,   -- S_SPOS_DIE1
        ⟨"SPOS", 'I', 5, "A_Scream", false⟩,   -- S_SPOS_DIE2
        ⟨"SPOS", 'J', 5, "A_Fall", false⟩,   -- S_SPOS_DIE3
        ⟨"SPOS", 'K', 5, "", false⟩,   -- S_SPOS_DIE4
        ⟨"SPOS", 'L', -1, "", false⟩   -- S_SPOS_DIE5
      ],
      ending := .halt },
    { entry := "xdeath",
      states := #[
        ⟨"SPOS", 'M', 5, "", false⟩,   -- S_SPOS_XDIE1
        ⟨"SPOS", 'N', 5, "A_XScream", false⟩,   -- S_SPOS_XDIE2
        ⟨"SPOS", 'O', 5, "A_Fall", false⟩,   -- S_SPOS_XDIE3
        ⟨"SPOS", 'P', 5, "", false⟩,   -- S_SPOS_XDIE4
        ⟨"SPOS", 'Q', 5, "", false⟩,   -- S_SPOS_XDIE5
        ⟨"SPOS", 'R', 5, "", false⟩,   -- S_SPOS_XDIE6
        ⟨"SPOS", 'S', 5, "", false⟩,   -- S_SPOS_XDIE7
        ⟨"SPOS", 'T', 5, "", false⟩,   -- S_SPOS_XDIE8
        ⟨"SPOS", 'U', -1, "", false⟩   -- S_SPOS_XDIE9
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"SPOS", 'L', 5, "", false⟩,   -- S_SPOS_RAISE1
        ⟨"SPOS", 'K', 5, "", false⟩,   -- S_SPOS_RAISE2
        ⟨"SPOS", 'J', 5, "", false⟩,   -- S_SPOS_RAISE3
        ⟨"SPOS", 'I', 5, "", false⟩,   -- S_SPOS_RAISE4
        ⟨"SPOS", 'H', 5, "", false⟩   -- S_SPOS_RAISE5
      ],
      ending := .loops "see" 0 }
  ] }

/- Vanilla source, info.c mobjinfo[] (verbatim):
{                // MT_TROOP
        3001,                // doomednum
        S_TROO_STND,                // spawnstate
        60,                // spawnhealth
        S_TROO_RUN1,                // seestate
        sfx_bgsit1,                // seesound
        8,                // reactiontime
        0,                // attacksound
        S_TROO_PAIN,                // painstate
        200,                // painchance
        sfx_popain,                // painsound
        S_TROO_ATK1,                // meleestate
        S_TROO_ATK1,                // missilestate
        S_TROO_DIE1,                // deathstate
        S_TROO_XDIE1,                // xdeathstate
        sfx_bgdth1,                // deathsound
        8,                // speed
        20*FRACUNIT,                // radius
        56*FRACUNIT,                // height
        100,                // mass
        0,                // damage
        sfx_bgact,                // activesound
        MF_SOLID|MF_SHOOTABLE|MF_COUNTKILL,                // flags
        S_TROO_RAISE1                // raisestate
    },
-/
def imp : Mobj := {
  mt := "MT_TROOP", dillKind := "imp"
  doomednum := 3001
  health := 60
  speed := 8
  radius := 20, height := 56, mass := 100
  painChance := 200, damage := 0, reactionTime := 8
  seeSound := "dsbgsit1", painSound := "dspopain", deathSound := "dsbgdth1", activeSound := "dsbgact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"TROO", 'A', 10, "A_Look", false⟩,   -- S_TROO_STND
        ⟨"TROO", 'B', 10, "A_Look", false⟩   -- S_TROO_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"TROO", 'A', 3, "A_Chase", false⟩,   -- S_TROO_RUN1
        ⟨"TROO", 'A', 3, "A_Chase", false⟩,   -- S_TROO_RUN2
        ⟨"TROO", 'B', 3, "A_Chase", false⟩,   -- S_TROO_RUN3
        ⟨"TROO", 'B', 3, "A_Chase", false⟩,   -- S_TROO_RUN4
        ⟨"TROO", 'C', 3, "A_Chase", false⟩,   -- S_TROO_RUN5
        ⟨"TROO", 'C', 3, "A_Chase", false⟩,   -- S_TROO_RUN6
        ⟨"TROO", 'D', 3, "A_Chase", false⟩,   -- S_TROO_RUN7
        ⟨"TROO", 'D', 3, "A_Chase", false⟩   -- S_TROO_RUN8
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"TROO", 'H', 2, "", false⟩,   -- S_TROO_PAIN
        ⟨"TROO", 'H', 2, "A_Pain", false⟩   -- S_TROO_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "melee",
      states := #[
        ⟨"TROO", 'E', 8, "A_FaceTarget", false⟩,   -- S_TROO_ATK1
        ⟨"TROO", 'F', 8, "A_FaceTarget", false⟩,   -- S_TROO_ATK2
        ⟨"TROO", 'G', 6, "A_TroopAttack", false⟩   -- S_TROO_ATK3
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
      ],
      ending := .loops "melee" 0 },
    { entry := "death",
      states := #[
        ⟨"TROO", 'I', 8, "", false⟩,   -- S_TROO_DIE1
        ⟨"TROO", 'J', 8, "A_Scream", false⟩,   -- S_TROO_DIE2
        ⟨"TROO", 'K', 6, "", false⟩,   -- S_TROO_DIE3
        ⟨"TROO", 'L', 6, "A_Fall", false⟩,   -- S_TROO_DIE4
        ⟨"TROO", 'M', -1, "", false⟩   -- S_TROO_DIE5
      ],
      ending := .halt },
    { entry := "xdeath",
      states := #[
        ⟨"TROO", 'N', 5, "", false⟩,   -- S_TROO_XDIE1
        ⟨"TROO", 'O', 5, "A_XScream", false⟩,   -- S_TROO_XDIE2
        ⟨"TROO", 'P', 5, "", false⟩,   -- S_TROO_XDIE3
        ⟨"TROO", 'Q', 5, "A_Fall", false⟩,   -- S_TROO_XDIE4
        ⟨"TROO", 'R', 5, "", false⟩,   -- S_TROO_XDIE5
        ⟨"TROO", 'S', 5, "", false⟩,   -- S_TROO_XDIE6
        ⟨"TROO", 'T', 5, "", false⟩,   -- S_TROO_XDIE7
        ⟨"TROO", 'U', -1, "", false⟩   -- S_TROO_XDIE8
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"TROO", 'M', 8, "", false⟩,   -- S_TROO_RAISE1
        ⟨"TROO", 'L', 8, "", false⟩,   -- S_TROO_RAISE2
        ⟨"TROO", 'K', 6, "", false⟩,   -- S_TROO_RAISE3
        ⟨"TROO", 'J', 6, "", false⟩,   -- S_TROO_RAISE4
        ⟨"TROO", 'I', 6, "", false⟩   -- S_TROO_RAISE5
      ],
      ending := .loops "see" 0 }
  ] }

def demon : Mobj := {
  mt := "MT_SERGEANT", dillKind := "demon"
  doomednum := 3002
  health := 150
  speed := 10
  radius := 30, height := 56, mass := 400
  painChance := 180, damage := 0, reactionTime := 8
  seeSound := "dssgtsit", attackSound := "dssgtatk", painSound := "dsdmpain", deathSound := "dssgtdth", activeSound := "dsdmact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"SARG", 'A', 10, "A_Look", false⟩,   -- S_SARG_STND
        ⟨"SARG", 'B', 10, "A_Look", false⟩   -- S_SARG_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"SARG", 'A', 2, "A_Chase", false⟩,   -- S_SARG_RUN1
        ⟨"SARG", 'A', 2, "A_Chase", false⟩,   -- S_SARG_RUN2
        ⟨"SARG", 'B', 2, "A_Chase", false⟩,   -- S_SARG_RUN3
        ⟨"SARG", 'B', 2, "A_Chase", false⟩,   -- S_SARG_RUN4
        ⟨"SARG", 'C', 2, "A_Chase", false⟩,   -- S_SARG_RUN5
        ⟨"SARG", 'C', 2, "A_Chase", false⟩,   -- S_SARG_RUN6
        ⟨"SARG", 'D', 2, "A_Chase", false⟩,   -- S_SARG_RUN7
        ⟨"SARG", 'D', 2, "A_Chase", false⟩   -- S_SARG_RUN8
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"SARG", 'H', 2, "", false⟩,   -- S_SARG_PAIN
        ⟨"SARG", 'H', 2, "A_Pain", false⟩   -- S_SARG_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "melee",
      states := #[
        ⟨"SARG", 'E', 8, "A_FaceTarget", false⟩,   -- S_SARG_ATK1
        ⟨"SARG", 'F', 8, "A_FaceTarget", false⟩,   -- S_SARG_ATK2
        ⟨"SARG", 'G', 8, "A_SargAttack", false⟩   -- S_SARG_ATK3
      ],
      ending := .loops "see" 0 },
    { entry := "death",
      states := #[
        ⟨"SARG", 'I', 8, "", false⟩,   -- S_SARG_DIE1
        ⟨"SARG", 'J', 8, "A_Scream", false⟩,   -- S_SARG_DIE2
        ⟨"SARG", 'K', 4, "", false⟩,   -- S_SARG_DIE3
        ⟨"SARG", 'L', 4, "A_Fall", false⟩,   -- S_SARG_DIE4
        ⟨"SARG", 'M', 4, "", false⟩,   -- S_SARG_DIE5
        ⟨"SARG", 'N', -1, "", false⟩   -- S_SARG_DIE6
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"SARG", 'N', 5, "", false⟩,   -- S_SARG_RAISE1
        ⟨"SARG", 'M', 5, "", false⟩,   -- S_SARG_RAISE2
        ⟨"SARG", 'L', 5, "", false⟩,   -- S_SARG_RAISE3
        ⟨"SARG", 'K', 5, "", false⟩,   -- S_SARG_RAISE4
        ⟨"SARG", 'J', 5, "", false⟩,   -- S_SARG_RAISE5
        ⟨"SARG", 'I', 5, "", false⟩   -- S_SARG_RAISE6
      ],
      ending := .loops "see" 0 }
  ] }

def spectre : Mobj := {
  mt := "MT_SHADOWS", dillKind := "spectre"
  doomednum := 58
  health := 150
  speed := 10
  radius := 30, height := 56, mass := 400
  painChance := 180, damage := 0, reactionTime := 8
  seeSound := "dssgtsit", attackSound := "dssgtatk", painSound := "dsdmpain", deathSound := "dssgtdth", activeSound := "dsdmact"
  solid := true, shootable := true, shadow := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"SARG", 'A', 10, "A_Look", false⟩,   -- S_SARG_STND
        ⟨"SARG", 'B', 10, "A_Look", false⟩   -- S_SARG_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"SARG", 'A', 2, "A_Chase", false⟩,   -- S_SARG_RUN1
        ⟨"SARG", 'A', 2, "A_Chase", false⟩,   -- S_SARG_RUN2
        ⟨"SARG", 'B', 2, "A_Chase", false⟩,   -- S_SARG_RUN3
        ⟨"SARG", 'B', 2, "A_Chase", false⟩,   -- S_SARG_RUN4
        ⟨"SARG", 'C', 2, "A_Chase", false⟩,   -- S_SARG_RUN5
        ⟨"SARG", 'C', 2, "A_Chase", false⟩,   -- S_SARG_RUN6
        ⟨"SARG", 'D', 2, "A_Chase", false⟩,   -- S_SARG_RUN7
        ⟨"SARG", 'D', 2, "A_Chase", false⟩   -- S_SARG_RUN8
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"SARG", 'H', 2, "", false⟩,   -- S_SARG_PAIN
        ⟨"SARG", 'H', 2, "A_Pain", false⟩   -- S_SARG_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "melee",
      states := #[
        ⟨"SARG", 'E', 8, "A_FaceTarget", false⟩,   -- S_SARG_ATK1
        ⟨"SARG", 'F', 8, "A_FaceTarget", false⟩,   -- S_SARG_ATK2
        ⟨"SARG", 'G', 8, "A_SargAttack", false⟩   -- S_SARG_ATK3
      ],
      ending := .loops "see" 0 },
    { entry := "death",
      states := #[
        ⟨"SARG", 'I', 8, "", false⟩,   -- S_SARG_DIE1
        ⟨"SARG", 'J', 8, "A_Scream", false⟩,   -- S_SARG_DIE2
        ⟨"SARG", 'K', 4, "", false⟩,   -- S_SARG_DIE3
        ⟨"SARG", 'L', 4, "A_Fall", false⟩,   -- S_SARG_DIE4
        ⟨"SARG", 'M', 4, "", false⟩,   -- S_SARG_DIE5
        ⟨"SARG", 'N', -1, "", false⟩   -- S_SARG_DIE6
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"SARG", 'N', 5, "", false⟩,   -- S_SARG_RAISE1
        ⟨"SARG", 'M', 5, "", false⟩,   -- S_SARG_RAISE2
        ⟨"SARG", 'L', 5, "", false⟩,   -- S_SARG_RAISE3
        ⟨"SARG", 'K', 5, "", false⟩,   -- S_SARG_RAISE4
        ⟨"SARG", 'J', 5, "", false⟩,   -- S_SARG_RAISE5
        ⟨"SARG", 'I', 5, "", false⟩   -- S_SARG_RAISE6
      ],
      ending := .loops "see" 0 }
  ] }

/- Vanilla source, info.c mobjinfo[] (verbatim):
{                // MT_HEAD
        3005,                // doomednum
        S_HEAD_STND,                // spawnstate
        400,                // spawnhealth
        S_HEAD_RUN1,                // seestate
        sfx_cacsit,                // seesound
        8,                // reactiontime
        0,                // attacksound
        S_HEAD_PAIN,                // painstate
        128,                // painchance
        sfx_dmpain,                // painsound
        0,                // meleestate
        S_HEAD_ATK1,                // missilestate
        S_HEAD_DIE1,                // deathstate
        S_NULL,                // xdeathstate
        sfx_cacdth,                // deathsound
        8,                // speed
        31*FRACUNIT,                // radius
        56*FRACUNIT,                // height
        400,                // mass
        0,                // damage
        sfx_dmact,                // activesound
        MF_SOLID|MF_SHOOTABLE|MF_FLOAT|MF_NOGRAVITY|MF_COUNTKILL,                // flags
        S_HEAD_RAISE1                // raisestate
    },
-/
def cacodemon : Mobj := {
  mt := "MT_HEAD", dillKind := "cacodemon"
  doomednum := 3005
  health := 400
  speed := 8
  radius := 31, height := 56, mass := 400
  painChance := 128, damage := 0, reactionTime := 8
  seeSound := "dscacsit", painSound := "dsdmpain", deathSound := "dscacdth", activeSound := "dsdmact"
  solid := true, shootable := true, float := true, noGravity := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"HEAD", 'A', 10, "A_Look", false⟩   -- S_HEAD_STND
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"HEAD", 'A', 3, "A_Chase", false⟩   -- S_HEAD_RUN1
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"HEAD", 'E', 3, "", false⟩,   -- S_HEAD_PAIN
        ⟨"HEAD", 'E', 3, "A_Pain", false⟩,   -- S_HEAD_PAIN2
        ⟨"HEAD", 'F', 6, "", false⟩   -- S_HEAD_PAIN3
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"HEAD", 'B', 5, "A_FaceTarget", false⟩,   -- S_HEAD_ATK1
        ⟨"HEAD", 'C', 5, "A_FaceTarget", false⟩,   -- S_HEAD_ATK2
        ⟨"HEAD", 'D', 5, "A_HeadAttack", true⟩   -- S_HEAD_ATK3
      ],
      ending := .loops "see" 0 },
    { entry := "death",
      states := #[
        ⟨"HEAD", 'G', 8, "", false⟩,   -- S_HEAD_DIE1
        ⟨"HEAD", 'H', 8, "A_Scream", false⟩,   -- S_HEAD_DIE2
        ⟨"HEAD", 'I', 8, "", false⟩,   -- S_HEAD_DIE3
        ⟨"HEAD", 'J', 8, "", false⟩,   -- S_HEAD_DIE4
        ⟨"HEAD", 'K', 8, "A_Fall", false⟩,   -- S_HEAD_DIE5
        ⟨"HEAD", 'L', -1, "", false⟩   -- S_HEAD_DIE6
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"HEAD", 'L', 8, "", false⟩,   -- S_HEAD_RAISE1
        ⟨"HEAD", 'K', 8, "", false⟩,   -- S_HEAD_RAISE2
        ⟨"HEAD", 'J', 8, "", false⟩,   -- S_HEAD_RAISE3
        ⟨"HEAD", 'I', 8, "", false⟩,   -- S_HEAD_RAISE4
        ⟨"HEAD", 'H', 8, "", false⟩,   -- S_HEAD_RAISE5
        ⟨"HEAD", 'G', 8, "", false⟩   -- S_HEAD_RAISE6
      ],
      ending := .loops "see" 0 }
  ] }

def lostSoul : Mobj := {
  mt := "MT_SKULL", dillKind := "lostSoul"
  doomednum := 3006
  health := 100
  speed := 8
  radius := 16, height := 56, mass := 50
  painChance := 256, damage := 3, reactionTime := 8
  attackSound := "dssklatk", painSound := "dsdmpain", deathSound := "dsfirxpl", activeSound := "dsdmact"
  solid := true, shootable := true, float := true, noGravity := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"SKUL", 'A', 10, "A_Look", true⟩,   -- S_SKULL_STND
        ⟨"SKUL", 'B', 10, "A_Look", true⟩   -- S_SKULL_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"SKUL", 'A', 6, "A_Chase", true⟩,   -- S_SKULL_RUN1
        ⟨"SKUL", 'B', 6, "A_Chase", true⟩   -- S_SKULL_RUN2
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"SKUL", 'E', 3, "", true⟩,   -- S_SKULL_PAIN
        ⟨"SKUL", 'E', 3, "A_Pain", true⟩   -- S_SKULL_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"SKUL", 'C', 10, "A_FaceTarget", true⟩,   -- S_SKULL_ATK1
        ⟨"SKUL", 'D', 4, "A_SkullAttack", true⟩,   -- S_SKULL_ATK2
        ⟨"SKUL", 'C', 4, "", true⟩,   -- S_SKULL_ATK3
        ⟨"SKUL", 'D', 4, "", true⟩   -- S_SKULL_ATK4
      ],
      ending := .loops "missile" 2 },
    { entry := "death",
      states := #[
        ⟨"SKUL", 'F', 6, "", true⟩,   -- S_SKULL_DIE1
        ⟨"SKUL", 'G', 6, "A_Scream", true⟩,   -- S_SKULL_DIE2
        ⟨"SKUL", 'H', 6, "", true⟩,   -- S_SKULL_DIE3
        ⟨"SKUL", 'I', 6, "A_Fall", true⟩,   -- S_SKULL_DIE4
        ⟨"SKUL", 'J', 6, "", false⟩,   -- S_SKULL_DIE5
        ⟨"SKUL", 'K', 6, "", false⟩   -- S_SKULL_DIE6
      ],
      ending := .remove }
  ] }

def baron : Mobj := {
  mt := "MT_BRUISER", dillKind := "baron"
  doomednum := 3003
  health := 1000
  speed := 8
  radius := 24, height := 64, mass := 1000
  painChance := 50, damage := 0, reactionTime := 8
  seeSound := "dsbrssit", painSound := "dsdmpain", deathSound := "dsbrsdth", activeSound := "dsdmact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BOSS", 'A', 10, "A_Look", false⟩,   -- S_BOSS_STND
        ⟨"BOSS", 'B', 10, "A_Look", false⟩   -- S_BOSS_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"BOSS", 'A', 3, "A_Chase", false⟩,   -- S_BOSS_RUN1
        ⟨"BOSS", 'A', 3, "A_Chase", false⟩,   -- S_BOSS_RUN2
        ⟨"BOSS", 'B', 3, "A_Chase", false⟩,   -- S_BOSS_RUN3
        ⟨"BOSS", 'B', 3, "A_Chase", false⟩,   -- S_BOSS_RUN4
        ⟨"BOSS", 'C', 3, "A_Chase", false⟩,   -- S_BOSS_RUN5
        ⟨"BOSS", 'C', 3, "A_Chase", false⟩,   -- S_BOSS_RUN6
        ⟨"BOSS", 'D', 3, "A_Chase", false⟩,   -- S_BOSS_RUN7
        ⟨"BOSS", 'D', 3, "A_Chase", false⟩   -- S_BOSS_RUN8
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"BOSS", 'H', 2, "", false⟩,   -- S_BOSS_PAIN
        ⟨"BOSS", 'H', 2, "A_Pain", false⟩   -- S_BOSS_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "melee",
      states := #[
        ⟨"BOSS", 'E', 8, "A_FaceTarget", false⟩,   -- S_BOSS_ATK1
        ⟨"BOSS", 'F', 8, "A_FaceTarget", false⟩,   -- S_BOSS_ATK2
        ⟨"BOSS", 'G', 8, "A_BruisAttack", false⟩   -- S_BOSS_ATK3
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
      ],
      ending := .loops "melee" 0 },
    { entry := "death",
      states := #[
        ⟨"BOSS", 'I', 8, "", false⟩,   -- S_BOSS_DIE1
        ⟨"BOSS", 'J', 8, "A_Scream", false⟩,   -- S_BOSS_DIE2
        ⟨"BOSS", 'K', 8, "", false⟩,   -- S_BOSS_DIE3
        ⟨"BOSS", 'L', 8, "A_Fall", false⟩,   -- S_BOSS_DIE4
        ⟨"BOSS", 'M', 8, "", false⟩,   -- S_BOSS_DIE5
        ⟨"BOSS", 'N', 8, "", false⟩,   -- S_BOSS_DIE6
        ⟨"BOSS", 'O', -1, "A_BossDeath", false⟩   -- S_BOSS_DIE7
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"BOSS", 'O', 8, "", false⟩,   -- S_BOSS_RAISE1
        ⟨"BOSS", 'N', 8, "", false⟩,   -- S_BOSS_RAISE2
        ⟨"BOSS", 'M', 8, "", false⟩,   -- S_BOSS_RAISE3
        ⟨"BOSS", 'L', 8, "", false⟩,   -- S_BOSS_RAISE4
        ⟨"BOSS", 'K', 8, "", false⟩,   -- S_BOSS_RAISE5
        ⟨"BOSS", 'J', 8, "", false⟩,   -- S_BOSS_RAISE6
        ⟨"BOSS", 'I', 8, "", false⟩   -- S_BOSS_RAISE7
      ],
      ending := .loops "see" 0 }
  ] }

def cyberdemon : Mobj := {
  mt := "MT_CYBORG", dillKind := "cyberdemon"
  doomednum := 16
  health := 4000
  speed := 16
  radius := 40, height := 110, mass := 1000
  painChance := 20, damage := 0, reactionTime := 8
  seeSound := "dscybsit", painSound := "dsdmpain", deathSound := "dscybdth", activeSound := "dsdmact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"CYBR", 'A', 10, "A_Look", false⟩,   -- S_CYBER_STND
        ⟨"CYBR", 'B', 10, "A_Look", false⟩   -- S_CYBER_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"CYBR", 'A', 3, "A_Hoof", false⟩,   -- S_CYBER_RUN1
        ⟨"CYBR", 'A', 3, "A_Chase", false⟩,   -- S_CYBER_RUN2
        ⟨"CYBR", 'B', 3, "A_Chase", false⟩,   -- S_CYBER_RUN3
        ⟨"CYBR", 'B', 3, "A_Chase", false⟩,   -- S_CYBER_RUN4
        ⟨"CYBR", 'C', 3, "A_Chase", false⟩,   -- S_CYBER_RUN5
        ⟨"CYBR", 'C', 3, "A_Chase", false⟩,   -- S_CYBER_RUN6
        ⟨"CYBR", 'D', 3, "A_Metal", false⟩,   -- S_CYBER_RUN7
        ⟨"CYBR", 'D', 3, "A_Chase", false⟩   -- S_CYBER_RUN8
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"CYBR", 'G', 10, "A_Pain", false⟩   -- S_CYBER_PAIN
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"CYBR", 'E', 6, "A_FaceTarget", false⟩,   -- S_CYBER_ATK1
        ⟨"CYBR", 'F', 12, "A_CyberAttack", false⟩,   -- S_CYBER_ATK2
        ⟨"CYBR", 'E', 12, "A_FaceTarget", false⟩,   -- S_CYBER_ATK3
        ⟨"CYBR", 'F', 12, "A_CyberAttack", false⟩,   -- S_CYBER_ATK4
        ⟨"CYBR", 'E', 12, "A_FaceTarget", false⟩,   -- S_CYBER_ATK5
        ⟨"CYBR", 'F', 12, "A_CyberAttack", false⟩   -- S_CYBER_ATK6
      ],
      ending := .loops "see" 0 },
    { entry := "death",
      states := #[
        ⟨"CYBR", 'H', 10, "", false⟩,   -- S_CYBER_DIE1
        ⟨"CYBR", 'I', 10, "A_Scream", false⟩,   -- S_CYBER_DIE2
        ⟨"CYBR", 'J', 10, "", false⟩,   -- S_CYBER_DIE3
        ⟨"CYBR", 'K', 10, "", false⟩,   -- S_CYBER_DIE4
        ⟨"CYBR", 'L', 10, "", false⟩,   -- S_CYBER_DIE5
        ⟨"CYBR", 'M', 10, "A_Fall", false⟩,   -- S_CYBER_DIE6
        ⟨"CYBR", 'N', 10, "", false⟩,   -- S_CYBER_DIE7
        ⟨"CYBR", 'O', 10, "", false⟩,   -- S_CYBER_DIE8
        ⟨"CYBR", 'P', 30, "", false⟩,   -- S_CYBER_DIE9
        ⟨"CYBR", 'P', -1, "A_BossDeath", false⟩   -- S_CYBER_DIE10
      ],
      ending := .halt }
  ] }

/- Vanilla source, info.c mobjinfo[] (verbatim):
{                // MT_SPIDER
        7,                // doomednum
        S_SPID_STND,                // spawnstate
        3000,                // spawnhealth
        S_SPID_RUN1,                // seestate
        sfx_spisit,                // seesound
        8,                // reactiontime
        sfx_shotgn,                // attacksound
        S_SPID_PAIN,                // painstate
        40,                // painchance
        sfx_dmpain,                // painsound
        0,                // meleestate
        S_SPID_ATK1,                // missilestate
        S_SPID_DIE1,                // deathstate
        S_NULL,                // xdeathstate
        sfx_spidth,                // deathsound
        12,                // speed
        128*FRACUNIT,                // radius
        100*FRACUNIT,                // height
        1000,                // mass
        0,                // damage
        sfx_dmact,                // activesound
        MF_SOLID|MF_SHOOTABLE|MF_COUNTKILL,                // flags
        S_NULL                // raisestate
    },
-/
def spiderMastermind : Mobj := {
  mt := "MT_SPIDER", dillKind := "spiderMastermind"
  doomednum := 7
  health := 3000
  speed := 12
  radius := 128, height := 100, mass := 1000
  painChance := 40, damage := 0, reactionTime := 8
  seeSound := "dsspisit", attackSound := "dsshotgn", painSound := "dsdmpain", deathSound := "dsspidth", activeSound := "dsdmact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"SPID", 'A', 10, "A_Look", false⟩,   -- S_SPID_STND
        ⟨"SPID", 'B', 10, "A_Look", false⟩   -- S_SPID_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"SPID", 'A', 3, "A_Metal", false⟩,   -- S_SPID_RUN1
        ⟨"SPID", 'A', 3, "A_Chase", false⟩,   -- S_SPID_RUN2
        ⟨"SPID", 'B', 3, "A_Chase", false⟩,   -- S_SPID_RUN3
        ⟨"SPID", 'B', 3, "A_Chase", false⟩,   -- S_SPID_RUN4
        ⟨"SPID", 'C', 3, "A_Metal", false⟩,   -- S_SPID_RUN5
        ⟨"SPID", 'C', 3, "A_Chase", false⟩,   -- S_SPID_RUN6
        ⟨"SPID", 'D', 3, "A_Chase", false⟩,   -- S_SPID_RUN7
        ⟨"SPID", 'D', 3, "A_Chase", false⟩,   -- S_SPID_RUN8
        ⟨"SPID", 'E', 3, "A_Metal", false⟩,   -- S_SPID_RUN9
        ⟨"SPID", 'E', 3, "A_Chase", false⟩,   -- S_SPID_RUN10
        ⟨"SPID", 'F', 3, "A_Chase", false⟩,   -- S_SPID_RUN11
        ⟨"SPID", 'F', 3, "A_Chase", false⟩   -- S_SPID_RUN12
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"SPID", 'I', 3, "", false⟩,   -- S_SPID_PAIN
        ⟨"SPID", 'I', 3, "A_Pain", false⟩   -- S_SPID_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"SPID", 'A', 20, "A_FaceTarget", true⟩,   -- S_SPID_ATK1
        ⟨"SPID", 'G', 4, "A_SPosAttack", true⟩,   -- S_SPID_ATK2
        ⟨"SPID", 'H', 4, "A_SPosAttack", true⟩,   -- S_SPID_ATK3
        ⟨"SPID", 'H', 1, "A_SpidRefire", true⟩   -- S_SPID_ATK4
      ],
      ending := .loops "missile" 1 },
    { entry := "death",
      states := #[
        ⟨"SPID", 'J', 20, "A_Scream", false⟩,   -- S_SPID_DIE1
        ⟨"SPID", 'K', 10, "A_Fall", false⟩,   -- S_SPID_DIE2
        ⟨"SPID", 'L', 10, "", false⟩,   -- S_SPID_DIE3
        ⟨"SPID", 'M', 10, "", false⟩,   -- S_SPID_DIE4
        ⟨"SPID", 'N', 10, "", false⟩,   -- S_SPID_DIE5
        ⟨"SPID", 'O', 10, "", false⟩,   -- S_SPID_DIE6
        ⟨"SPID", 'P', 10, "", false⟩,   -- S_SPID_DIE7
        ⟨"SPID", 'Q', 10, "", false⟩,   -- S_SPID_DIE8
        ⟨"SPID", 'R', 10, "", false⟩,   -- S_SPID_DIE9
        ⟨"SPID", 'S', 30, "", false⟩,   -- S_SPID_DIE10
        ⟨"SPID", 'S', -1, "A_BossDeath", false⟩   -- S_SPID_DIE11
      ],
      ending := .halt }
  ] }

def chaingunner : Mobj := {
  mt := "MT_CHAINGUY", dillKind := "chaingunner"
  doomednum := 65
  health := 70
  speed := 8
  radius := 20, height := 56, mass := 100
  painChance := 170, damage := 0, reactionTime := 8
  seeSound := "dsposit2", painSound := "dspopain", deathSound := "dspodth2", activeSound := "dsposact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"CPOS", 'A', 10, "A_Look", false⟩,   -- S_CPOS_STND
        ⟨"CPOS", 'B', 10, "A_Look", false⟩   -- S_CPOS_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"CPOS", 'A', 3, "A_Chase", false⟩,   -- S_CPOS_RUN1
        ⟨"CPOS", 'A', 3, "A_Chase", false⟩,   -- S_CPOS_RUN2
        ⟨"CPOS", 'B', 3, "A_Chase", false⟩,   -- S_CPOS_RUN3
        ⟨"CPOS", 'B', 3, "A_Chase", false⟩,   -- S_CPOS_RUN4
        ⟨"CPOS", 'C', 3, "A_Chase", false⟩,   -- S_CPOS_RUN5
        ⟨"CPOS", 'C', 3, "A_Chase", false⟩,   -- S_CPOS_RUN6
        ⟨"CPOS", 'D', 3, "A_Chase", false⟩,   -- S_CPOS_RUN7
        ⟨"CPOS", 'D', 3, "A_Chase", false⟩   -- S_CPOS_RUN8
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"CPOS", 'G', 3, "", false⟩,   -- S_CPOS_PAIN
        ⟨"CPOS", 'G', 3, "A_Pain", false⟩   -- S_CPOS_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"CPOS", 'E', 10, "A_FaceTarget", false⟩,   -- S_CPOS_ATK1
        ⟨"CPOS", 'F', 4, "A_CPosAttack", true⟩,   -- S_CPOS_ATK2
        ⟨"CPOS", 'E', 4, "A_CPosAttack", true⟩,   -- S_CPOS_ATK3
        ⟨"CPOS", 'F', 1, "A_CPosRefire", false⟩   -- S_CPOS_ATK4
      ],
      ending := .loops "missile" 1 },
    { entry := "death",
      states := #[
        ⟨"CPOS", 'H', 5, "", false⟩,   -- S_CPOS_DIE1
        ⟨"CPOS", 'I', 5, "A_Scream", false⟩,   -- S_CPOS_DIE2
        ⟨"CPOS", 'J', 5, "A_Fall", false⟩,   -- S_CPOS_DIE3
        ⟨"CPOS", 'K', 5, "", false⟩,   -- S_CPOS_DIE4
        ⟨"CPOS", 'L', 5, "", false⟩,   -- S_CPOS_DIE5
        ⟨"CPOS", 'M', 5, "", false⟩,   -- S_CPOS_DIE6
        ⟨"CPOS", 'N', -1, "", false⟩   -- S_CPOS_DIE7
      ],
      ending := .halt },
    { entry := "xdeath",
      states := #[
        ⟨"CPOS", 'O', 5, "", false⟩,   -- S_CPOS_XDIE1
        ⟨"CPOS", 'P', 5, "A_XScream", false⟩,   -- S_CPOS_XDIE2
        ⟨"CPOS", 'Q', 5, "A_Fall", false⟩,   -- S_CPOS_XDIE3
        ⟨"CPOS", 'R', 5, "", false⟩,   -- S_CPOS_XDIE4
        ⟨"CPOS", 'S', 5, "", false⟩,   -- S_CPOS_XDIE5
        ⟨"CPOS", 'T', -1, "", false⟩   -- S_CPOS_XDIE6
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"CPOS", 'N', 5, "", false⟩,   -- S_CPOS_RAISE1
        ⟨"CPOS", 'M', 5, "", false⟩,   -- S_CPOS_RAISE2
        ⟨"CPOS", 'L', 5, "", false⟩,   -- S_CPOS_RAISE3
        ⟨"CPOS", 'K', 5, "", false⟩,   -- S_CPOS_RAISE4
        ⟨"CPOS", 'J', 5, "", false⟩,   -- S_CPOS_RAISE5
        ⟨"CPOS", 'I', 5, "", false⟩,   -- S_CPOS_RAISE6
        ⟨"CPOS", 'H', 5, "", false⟩   -- S_CPOS_RAISE7
      ],
      ending := .loops "see" 0 }
  ] }

def wolfSS : Mobj := {
  mt := "MT_WOLFSS", dillKind := "wolfSS"
  doomednum := 84
  health := 50
  speed := 8
  radius := 20, height := 56, mass := 100
  painChance := 170, damage := 0, reactionTime := 8
  seeSound := "dssssit", painSound := "dspopain", deathSound := "dsssdth", activeSound := "dsposact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"SSWV", 'A', 10, "A_Look", false⟩,   -- S_SSWV_STND
        ⟨"SSWV", 'B', 10, "A_Look", false⟩   -- S_SSWV_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"SSWV", 'A', 3, "A_Chase", false⟩,   -- S_SSWV_RUN1
        ⟨"SSWV", 'A', 3, "A_Chase", false⟩,   -- S_SSWV_RUN2
        ⟨"SSWV", 'B', 3, "A_Chase", false⟩,   -- S_SSWV_RUN3
        ⟨"SSWV", 'B', 3, "A_Chase", false⟩,   -- S_SSWV_RUN4
        ⟨"SSWV", 'C', 3, "A_Chase", false⟩,   -- S_SSWV_RUN5
        ⟨"SSWV", 'C', 3, "A_Chase", false⟩,   -- S_SSWV_RUN6
        ⟨"SSWV", 'D', 3, "A_Chase", false⟩,   -- S_SSWV_RUN7
        ⟨"SSWV", 'D', 3, "A_Chase", false⟩   -- S_SSWV_RUN8
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"SSWV", 'H', 3, "", false⟩,   -- S_SSWV_PAIN
        ⟨"SSWV", 'H', 3, "A_Pain", false⟩   -- S_SSWV_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"SSWV", 'E', 10, "A_FaceTarget", false⟩,   -- S_SSWV_ATK1
        ⟨"SSWV", 'F', 10, "A_FaceTarget", false⟩,   -- S_SSWV_ATK2
        ⟨"SSWV", 'G', 4, "A_CPosAttack", true⟩,   -- S_SSWV_ATK3
        ⟨"SSWV", 'F', 6, "A_FaceTarget", false⟩,   -- S_SSWV_ATK4
        ⟨"SSWV", 'G', 4, "A_CPosAttack", true⟩,   -- S_SSWV_ATK5
        ⟨"SSWV", 'F', 1, "A_CPosRefire", false⟩   -- S_SSWV_ATK6
      ],
      ending := .loops "missile" 1 },
    { entry := "death",
      states := #[
        ⟨"SSWV", 'I', 5, "", false⟩,   -- S_SSWV_DIE1
        ⟨"SSWV", 'J', 5, "A_Scream", false⟩,   -- S_SSWV_DIE2
        ⟨"SSWV", 'K', 5, "A_Fall", false⟩,   -- S_SSWV_DIE3
        ⟨"SSWV", 'L', 5, "", false⟩,   -- S_SSWV_DIE4
        ⟨"SSWV", 'M', -1, "", false⟩   -- S_SSWV_DIE5
      ],
      ending := .halt },
    { entry := "xdeath",
      states := #[
        ⟨"SSWV", 'N', 5, "", false⟩,   -- S_SSWV_XDIE1
        ⟨"SSWV", 'O', 5, "A_XScream", false⟩,   -- S_SSWV_XDIE2
        ⟨"SSWV", 'P', 5, "A_Fall", false⟩,   -- S_SSWV_XDIE3
        ⟨"SSWV", 'Q', 5, "", false⟩,   -- S_SSWV_XDIE4
        ⟨"SSWV", 'R', 5, "", false⟩,   -- S_SSWV_XDIE5
        ⟨"SSWV", 'S', 5, "", false⟩,   -- S_SSWV_XDIE6
        ⟨"SSWV", 'T', 5, "", false⟩,   -- S_SSWV_XDIE7
        ⟨"SSWV", 'U', 5, "", false⟩,   -- S_SSWV_XDIE8
        ⟨"SSWV", 'V', -1, "", false⟩   -- S_SSWV_XDIE9
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"SSWV", 'M', 5, "", false⟩,   -- S_SSWV_RAISE1
        ⟨"SSWV", 'L', 5, "", false⟩,   -- S_SSWV_RAISE2
        ⟨"SSWV", 'K', 5, "", false⟩,   -- S_SSWV_RAISE3
        ⟨"SSWV", 'J', 5, "", false⟩,   -- S_SSWV_RAISE4
        ⟨"SSWV", 'I', 5, "", false⟩   -- S_SSWV_RAISE5
      ],
      ending := .loops "see" 0 }
  ] }

def hellKnight : Mobj := {
  mt := "MT_KNIGHT", dillKind := "hellKnight"
  doomednum := 69
  health := 500
  speed := 8
  radius := 24, height := 64, mass := 1000
  painChance := 50, damage := 0, reactionTime := 8
  seeSound := "dskntsit", painSound := "dsdmpain", deathSound := "dskntdth", activeSound := "dsdmact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BOS2", 'A', 10, "A_Look", false⟩,   -- S_BOS2_STND
        ⟨"BOS2", 'B', 10, "A_Look", false⟩   -- S_BOS2_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"BOS2", 'A', 3, "A_Chase", false⟩,   -- S_BOS2_RUN1
        ⟨"BOS2", 'A', 3, "A_Chase", false⟩,   -- S_BOS2_RUN2
        ⟨"BOS2", 'B', 3, "A_Chase", false⟩,   -- S_BOS2_RUN3
        ⟨"BOS2", 'B', 3, "A_Chase", false⟩,   -- S_BOS2_RUN4
        ⟨"BOS2", 'C', 3, "A_Chase", false⟩,   -- S_BOS2_RUN5
        ⟨"BOS2", 'C', 3, "A_Chase", false⟩,   -- S_BOS2_RUN6
        ⟨"BOS2", 'D', 3, "A_Chase", false⟩,   -- S_BOS2_RUN7
        ⟨"BOS2", 'D', 3, "A_Chase", false⟩   -- S_BOS2_RUN8
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"BOS2", 'H', 2, "", false⟩,   -- S_BOS2_PAIN
        ⟨"BOS2", 'H', 2, "A_Pain", false⟩   -- S_BOS2_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "melee",
      states := #[
        ⟨"BOS2", 'E', 8, "A_FaceTarget", false⟩,   -- S_BOS2_ATK1
        ⟨"BOS2", 'F', 8, "A_FaceTarget", false⟩,   -- S_BOS2_ATK2
        ⟨"BOS2", 'G', 8, "A_BruisAttack", false⟩   -- S_BOS2_ATK3
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
      ],
      ending := .loops "melee" 0 },
    { entry := "death",
      states := #[
        ⟨"BOS2", 'I', 8, "", false⟩,   -- S_BOS2_DIE1
        ⟨"BOS2", 'J', 8, "A_Scream", false⟩,   -- S_BOS2_DIE2
        ⟨"BOS2", 'K', 8, "", false⟩,   -- S_BOS2_DIE3
        ⟨"BOS2", 'L', 8, "A_Fall", false⟩,   -- S_BOS2_DIE4
        ⟨"BOS2", 'M', 8, "", false⟩,   -- S_BOS2_DIE5
        ⟨"BOS2", 'N', 8, "", false⟩,   -- S_BOS2_DIE6
        ⟨"BOS2", 'O', -1, "", false⟩   -- S_BOS2_DIE7
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"BOS2", 'O', 8, "", false⟩,   -- S_BOS2_RAISE1
        ⟨"BOS2", 'N', 8, "", false⟩,   -- S_BOS2_RAISE2
        ⟨"BOS2", 'M', 8, "", false⟩,   -- S_BOS2_RAISE3
        ⟨"BOS2", 'L', 8, "", false⟩,   -- S_BOS2_RAISE4
        ⟨"BOS2", 'K', 8, "", false⟩,   -- S_BOS2_RAISE5
        ⟨"BOS2", 'J', 8, "", false⟩,   -- S_BOS2_RAISE6
        ⟨"BOS2", 'I', 8, "", false⟩   -- S_BOS2_RAISE7
      ],
      ending := .loops "see" 0 }
  ] }

def mancubus : Mobj := {
  mt := "MT_FATSO", dillKind := "mancubus"
  doomednum := 67
  health := 600
  speed := 8
  radius := 48, height := 64, mass := 1000
  painChance := 80, damage := 0, reactionTime := 8
  seeSound := "dsmansit", painSound := "dsmnpain", deathSound := "dsmandth", activeSound := "dsposact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"FATT", 'A', 15, "A_Look", false⟩,   -- S_FATT_STND
        ⟨"FATT", 'B', 15, "A_Look", false⟩   -- S_FATT_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"FATT", 'A', 4, "A_Chase", false⟩,   -- S_FATT_RUN1
        ⟨"FATT", 'A', 4, "A_Chase", false⟩,   -- S_FATT_RUN2
        ⟨"FATT", 'B', 4, "A_Chase", false⟩,   -- S_FATT_RUN3
        ⟨"FATT", 'B', 4, "A_Chase", false⟩,   -- S_FATT_RUN4
        ⟨"FATT", 'C', 4, "A_Chase", false⟩,   -- S_FATT_RUN5
        ⟨"FATT", 'C', 4, "A_Chase", false⟩,   -- S_FATT_RUN6
        ⟨"FATT", 'D', 4, "A_Chase", false⟩,   -- S_FATT_RUN7
        ⟨"FATT", 'D', 4, "A_Chase", false⟩,   -- S_FATT_RUN8
        ⟨"FATT", 'E', 4, "A_Chase", false⟩,   -- S_FATT_RUN9
        ⟨"FATT", 'E', 4, "A_Chase", false⟩,   -- S_FATT_RUN10
        ⟨"FATT", 'F', 4, "A_Chase", false⟩,   -- S_FATT_RUN11
        ⟨"FATT", 'F', 4, "A_Chase", false⟩   -- S_FATT_RUN12
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"FATT", 'J', 3, "", false⟩,   -- S_FATT_PAIN
        ⟨"FATT", 'J', 3, "A_Pain", false⟩   -- S_FATT_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"FATT", 'G', 20, "A_FatRaise", false⟩,   -- S_FATT_ATK1
        ⟨"FATT", 'H', 10, "A_FatAttack1", true⟩,   -- S_FATT_ATK2
        ⟨"FATT", 'I', 5, "A_FaceTarget", false⟩,   -- S_FATT_ATK3
        ⟨"FATT", 'G', 5, "A_FaceTarget", false⟩,   -- S_FATT_ATK4
        ⟨"FATT", 'H', 10, "A_FatAttack2", true⟩,   -- S_FATT_ATK5
        ⟨"FATT", 'I', 5, "A_FaceTarget", false⟩,   -- S_FATT_ATK6
        ⟨"FATT", 'G', 5, "A_FaceTarget", false⟩,   -- S_FATT_ATK7
        ⟨"FATT", 'H', 10, "A_FatAttack3", true⟩,   -- S_FATT_ATK8
        ⟨"FATT", 'I', 5, "A_FaceTarget", false⟩,   -- S_FATT_ATK9
        ⟨"FATT", 'G', 5, "A_FaceTarget", false⟩   -- S_FATT_ATK10
      ],
      ending := .loops "see" 0 },
    { entry := "death",
      states := #[
        ⟨"FATT", 'K', 6, "", false⟩,   -- S_FATT_DIE1
        ⟨"FATT", 'L', 6, "A_Scream", false⟩,   -- S_FATT_DIE2
        ⟨"FATT", 'M', 6, "A_Fall", false⟩,   -- S_FATT_DIE3
        ⟨"FATT", 'N', 6, "", false⟩,   -- S_FATT_DIE4
        ⟨"FATT", 'O', 6, "", false⟩,   -- S_FATT_DIE5
        ⟨"FATT", 'P', 6, "", false⟩,   -- S_FATT_DIE6
        ⟨"FATT", 'Q', 6, "", false⟩,   -- S_FATT_DIE7
        ⟨"FATT", 'R', 6, "", false⟩,   -- S_FATT_DIE8
        ⟨"FATT", 'S', 6, "", false⟩,   -- S_FATT_DIE9
        ⟨"FATT", 'T', -1, "A_BossDeath", false⟩   -- S_FATT_DIE10
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"FATT", 'R', 5, "", false⟩,   -- S_FATT_RAISE1
        ⟨"FATT", 'Q', 5, "", false⟩,   -- S_FATT_RAISE2
        ⟨"FATT", 'P', 5, "", false⟩,   -- S_FATT_RAISE3
        ⟨"FATT", 'O', 5, "", false⟩,   -- S_FATT_RAISE4
        ⟨"FATT", 'N', 5, "", false⟩,   -- S_FATT_RAISE5
        ⟨"FATT", 'M', 5, "", false⟩,   -- S_FATT_RAISE6
        ⟨"FATT", 'L', 5, "", false⟩,   -- S_FATT_RAISE7
        ⟨"FATT", 'K', 5, "", false⟩   -- S_FATT_RAISE8
      ],
      ending := .loops "see" 0 }
  ] }

def arachnotron : Mobj := {
  mt := "MT_BABY", dillKind := "arachnotron"
  doomednum := 68
  health := 500
  speed := 12
  radius := 64, height := 64, mass := 600
  painChance := 128, damage := 0, reactionTime := 8
  seeSound := "dsbspsit", painSound := "dsdmpain", deathSound := "dsbspdth", activeSound := "dsbspact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BSPI", 'A', 10, "A_Look", false⟩,   -- S_BSPI_STND
        ⟨"BSPI", 'B', 10, "A_Look", false⟩   -- S_BSPI_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"BSPI", 'A', 20, "", false⟩,   -- S_BSPI_SIGHT
        ⟨"BSPI", 'A', 3, "A_BabyMetal", false⟩,   -- S_BSPI_RUN1
        ⟨"BSPI", 'A', 3, "A_Chase", false⟩,   -- S_BSPI_RUN2
        ⟨"BSPI", 'B', 3, "A_Chase", false⟩,   -- S_BSPI_RUN3
        ⟨"BSPI", 'B', 3, "A_Chase", false⟩,   -- S_BSPI_RUN4
        ⟨"BSPI", 'C', 3, "A_Chase", false⟩,   -- S_BSPI_RUN5
        ⟨"BSPI", 'C', 3, "A_Chase", false⟩,   -- S_BSPI_RUN6
        ⟨"BSPI", 'D', 3, "A_BabyMetal", false⟩,   -- S_BSPI_RUN7
        ⟨"BSPI", 'D', 3, "A_Chase", false⟩,   -- S_BSPI_RUN8
        ⟨"BSPI", 'E', 3, "A_Chase", false⟩,   -- S_BSPI_RUN9
        ⟨"BSPI", 'E', 3, "A_Chase", false⟩,   -- S_BSPI_RUN10
        ⟨"BSPI", 'F', 3, "A_Chase", false⟩,   -- S_BSPI_RUN11
        ⟨"BSPI", 'F', 3, "A_Chase", false⟩   -- S_BSPI_RUN12
      ],
      ending := .loops "see" 1 },
    { entry := "pain",
      states := #[
        ⟨"BSPI", 'I', 3, "", false⟩,   -- S_BSPI_PAIN
        ⟨"BSPI", 'I', 3, "A_Pain", false⟩   -- S_BSPI_PAIN2
      ],
      ending := .loops "see" 1 },
    { entry := "missile",
      states := #[
        ⟨"BSPI", 'A', 20, "A_FaceTarget", true⟩,   -- S_BSPI_ATK1
        ⟨"BSPI", 'G', 4, "A_BspiAttack", true⟩,   -- S_BSPI_ATK2
        ⟨"BSPI", 'H', 4, "", true⟩,   -- S_BSPI_ATK3
        ⟨"BSPI", 'H', 1, "A_SpidRefire", true⟩   -- S_BSPI_ATK4
      ],
      ending := .loops "missile" 1 },
    { entry := "death",
      states := #[
        ⟨"BSPI", 'J', 20, "A_Scream", false⟩,   -- S_BSPI_DIE1
        ⟨"BSPI", 'K', 7, "A_Fall", false⟩,   -- S_BSPI_DIE2
        ⟨"BSPI", 'L', 7, "", false⟩,   -- S_BSPI_DIE3
        ⟨"BSPI", 'M', 7, "", false⟩,   -- S_BSPI_DIE4
        ⟨"BSPI", 'N', 7, "", false⟩,   -- S_BSPI_DIE5
        ⟨"BSPI", 'O', 7, "", false⟩,   -- S_BSPI_DIE6
        ⟨"BSPI", 'P', -1, "A_BossDeath", false⟩   -- S_BSPI_DIE7
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"BSPI", 'P', 5, "", false⟩,   -- S_BSPI_RAISE1
        ⟨"BSPI", 'O', 5, "", false⟩,   -- S_BSPI_RAISE2
        ⟨"BSPI", 'N', 5, "", false⟩,   -- S_BSPI_RAISE3
        ⟨"BSPI", 'M', 5, "", false⟩,   -- S_BSPI_RAISE4
        ⟨"BSPI", 'L', 5, "", false⟩,   -- S_BSPI_RAISE5
        ⟨"BSPI", 'K', 5, "", false⟩,   -- S_BSPI_RAISE6
        ⟨"BSPI", 'J', 5, "", false⟩   -- S_BSPI_RAISE7
      ],
      ending := .loops "see" 1 }
  ] }

/- Vanilla source, info.c mobjinfo[] (verbatim):
{                // MT_UNDEAD
        66,                // doomednum
        S_SKEL_STND,                // spawnstate
        300,                // spawnhealth
        S_SKEL_RUN1,                // seestate
        sfx_skesit,                // seesound
        8,                // reactiontime
        0,                // attacksound
        S_SKEL_PAIN,                // painstate
        100,                // painchance
        sfx_popain,                // painsound
        S_SKEL_FIST1,                // meleestate
        S_SKEL_MISS1,                // missilestate
        S_SKEL_DIE1,                // deathstate
        S_NULL,                // xdeathstate
        sfx_skedth,                // deathsound
        10,                // speed
        20*FRACUNIT,                // radius
        56*FRACUNIT,                // height
        500,                // mass
        0,                // damage
        sfx_skeact,                // activesound
        MF_SOLID|MF_SHOOTABLE|MF_COUNTKILL,                // flags
        S_SKEL_RAISE1                // raisestate
    },
-/
def revenant : Mobj := {
  mt := "MT_UNDEAD", dillKind := "revenant"
  doomednum := 66
  health := 300
  speed := 10
  radius := 20, height := 56, mass := 500
  painChance := 100, damage := 0, reactionTime := 8
  seeSound := "dsskesit", painSound := "dspopain", deathSound := "dsskedth", activeSound := "dsskeact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"SKEL", 'A', 10, "A_Look", false⟩,   -- S_SKEL_STND
        ⟨"SKEL", 'B', 10, "A_Look", false⟩   -- S_SKEL_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"SKEL", 'A', 2, "A_Chase", false⟩,   -- S_SKEL_RUN1
        ⟨"SKEL", 'A', 2, "A_Chase", false⟩,   -- S_SKEL_RUN2
        ⟨"SKEL", 'B', 2, "A_Chase", false⟩,   -- S_SKEL_RUN3
        ⟨"SKEL", 'B', 2, "A_Chase", false⟩,   -- S_SKEL_RUN4
        ⟨"SKEL", 'C', 2, "A_Chase", false⟩,   -- S_SKEL_RUN5
        ⟨"SKEL", 'C', 2, "A_Chase", false⟩,   -- S_SKEL_RUN6
        ⟨"SKEL", 'D', 2, "A_Chase", false⟩,   -- S_SKEL_RUN7
        ⟨"SKEL", 'D', 2, "A_Chase", false⟩,   -- S_SKEL_RUN8
        ⟨"SKEL", 'E', 2, "A_Chase", false⟩,   -- S_SKEL_RUN9
        ⟨"SKEL", 'E', 2, "A_Chase", false⟩,   -- S_SKEL_RUN10
        ⟨"SKEL", 'F', 2, "A_Chase", false⟩,   -- S_SKEL_RUN11
        ⟨"SKEL", 'F', 2, "A_Chase", false⟩   -- S_SKEL_RUN12
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"SKEL", 'L', 5, "", false⟩,   -- S_SKEL_PAIN
        ⟨"SKEL", 'L', 5, "A_Pain", false⟩   -- S_SKEL_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "melee",
      states := #[
        ⟨"SKEL", 'G', 0, "A_FaceTarget", false⟩,   -- S_SKEL_FIST1
        ⟨"SKEL", 'G', 6, "A_SkelWhoosh", false⟩,   -- S_SKEL_FIST2
        ⟨"SKEL", 'H', 6, "A_FaceTarget", false⟩,   -- S_SKEL_FIST3
        ⟨"SKEL", 'I', 6, "A_SkelFist", false⟩   -- S_SKEL_FIST4
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"SKEL", 'J', 0, "A_FaceTarget", true⟩,   -- S_SKEL_MISS1
        ⟨"SKEL", 'J', 10, "A_FaceTarget", true⟩,   -- S_SKEL_MISS2
        ⟨"SKEL", 'K', 10, "A_SkelMissile", false⟩,   -- S_SKEL_MISS3
        ⟨"SKEL", 'K', 10, "A_FaceTarget", false⟩   -- S_SKEL_MISS4
      ],
      ending := .loops "see" 0 },
    { entry := "death",
      states := #[
        ⟨"SKEL", 'L', 7, "", false⟩,   -- S_SKEL_DIE1
        ⟨"SKEL", 'M', 7, "", false⟩,   -- S_SKEL_DIE2
        ⟨"SKEL", 'N', 7, "A_Scream", false⟩,   -- S_SKEL_DIE3
        ⟨"SKEL", 'O', 7, "A_Fall", false⟩,   -- S_SKEL_DIE4
        ⟨"SKEL", 'P', 7, "", false⟩,   -- S_SKEL_DIE5
        ⟨"SKEL", 'Q', -1, "", false⟩   -- S_SKEL_DIE6
      ],
      ending := .halt },
    { entry := "raise",
      states := #[
        ⟨"SKEL", 'Q', 5, "", false⟩,   -- S_SKEL_RAISE1
        ⟨"SKEL", 'P', 5, "", false⟩,   -- S_SKEL_RAISE2
        ⟨"SKEL", 'O', 5, "", false⟩,   -- S_SKEL_RAISE3
        ⟨"SKEL", 'N', 5, "", false⟩,   -- S_SKEL_RAISE4
        ⟨"SKEL", 'M', 5, "", false⟩,   -- S_SKEL_RAISE5
        ⟨"SKEL", 'L', 5, "", false⟩   -- S_SKEL_RAISE6
      ],
      ending := .loops "see" 0 }
  ] }

def painElemental : Mobj := {
  mt := "MT_PAIN", dillKind := "painElemental"
  doomednum := 71
  health := 400
  speed := 8
  radius := 31, height := 56, mass := 400
  painChance := 128, damage := 0, reactionTime := 8
  seeSound := "dspesit", painSound := "dspepain", deathSound := "dspedth", activeSound := "dsdmact"
  solid := true, shootable := true, float := true, noGravity := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"PAIN", 'A', 10, "A_Look", false⟩   -- S_PAIN_STND
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"PAIN", 'A', 3, "A_Chase", false⟩,   -- S_PAIN_RUN1
        ⟨"PAIN", 'A', 3, "A_Chase", false⟩,   -- S_PAIN_RUN2
        ⟨"PAIN", 'B', 3, "A_Chase", false⟩,   -- S_PAIN_RUN3
        ⟨"PAIN", 'B', 3, "A_Chase", false⟩,   -- S_PAIN_RUN4
        ⟨"PAIN", 'C', 3, "A_Chase", false⟩,   -- S_PAIN_RUN5
        ⟨"PAIN", 'C', 3, "A_Chase", false⟩   -- S_PAIN_RUN6
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"PAIN", 'G', 6, "", false⟩,   -- S_PAIN_PAIN
        ⟨"PAIN", 'G', 6, "A_Pain", false⟩   -- S_PAIN_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"PAIN", 'D', 5, "A_FaceTarget", false⟩,   -- S_PAIN_ATK1
        ⟨"PAIN", 'E', 5, "A_FaceTarget", false⟩,   -- S_PAIN_ATK2
        ⟨"PAIN", 'F', 5, "A_FaceTarget", true⟩,   -- S_PAIN_ATK3
        ⟨"PAIN", 'F', 0, "A_PainAttack", true⟩   -- S_PAIN_ATK4
      ],
      ending := .loops "see" 0 },
    { entry := "death",
      states := #[
        ⟨"PAIN", 'H', 8, "", true⟩,   -- S_PAIN_DIE1
        ⟨"PAIN", 'I', 8, "A_Scream", true⟩,   -- S_PAIN_DIE2
        ⟨"PAIN", 'J', 8, "", true⟩,   -- S_PAIN_DIE3
        ⟨"PAIN", 'K', 8, "", true⟩,   -- S_PAIN_DIE4
        ⟨"PAIN", 'L', 8, "A_PainDie", true⟩,   -- S_PAIN_DIE5
        ⟨"PAIN", 'M', 8, "", true⟩   -- S_PAIN_DIE6
      ],
      ending := .remove },
    { entry := "raise",
      states := #[
        ⟨"PAIN", 'M', 8, "", false⟩,   -- S_PAIN_RAISE1
        ⟨"PAIN", 'L', 8, "", false⟩,   -- S_PAIN_RAISE2
        ⟨"PAIN", 'K', 8, "", false⟩,   -- S_PAIN_RAISE3
        ⟨"PAIN", 'J', 8, "", false⟩,   -- S_PAIN_RAISE4
        ⟨"PAIN", 'I', 8, "", false⟩,   -- S_PAIN_RAISE5
        ⟨"PAIN", 'H', 8, "", false⟩   -- S_PAIN_RAISE6
      ],
      ending := .loops "see" 0 }
  ] }

def archVile : Mobj := {
  mt := "MT_VILE", dillKind := "archVile"
  doomednum := 64
  health := 700
  speed := 15
  radius := 20, height := 56, mass := 500
  painChance := 10, damage := 0, reactionTime := 8
  seeSound := "dsvilsit", painSound := "dsvipain", deathSound := "dsvildth", activeSound := "dsvilact"
  solid := true, shootable := true, countKill := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"VILE", 'A', 10, "A_Look", false⟩,   -- S_VILE_STND
        ⟨"VILE", 'B', 10, "A_Look", false⟩   -- S_VILE_STND2
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"VILE", 'A', 2, "A_VileChase", false⟩,   -- S_VILE_RUN1
        ⟨"VILE", 'A', 2, "A_VileChase", false⟩,   -- S_VILE_RUN2
        ⟨"VILE", 'B', 2, "A_VileChase", false⟩,   -- S_VILE_RUN3
        ⟨"VILE", 'B', 2, "A_VileChase", false⟩,   -- S_VILE_RUN4
        ⟨"VILE", 'C', 2, "A_VileChase", false⟩,   -- S_VILE_RUN5
        ⟨"VILE", 'C', 2, "A_VileChase", false⟩,   -- S_VILE_RUN6
        ⟨"VILE", 'D', 2, "A_VileChase", false⟩,   -- S_VILE_RUN7
        ⟨"VILE", 'D', 2, "A_VileChase", false⟩,   -- S_VILE_RUN8
        ⟨"VILE", 'E', 2, "A_VileChase", false⟩,   -- S_VILE_RUN9
        ⟨"VILE", 'E', 2, "A_VileChase", false⟩,   -- S_VILE_RUN10
        ⟨"VILE", 'F', 2, "A_VileChase", false⟩,   -- S_VILE_RUN11
        ⟨"VILE", 'F', 2, "A_VileChase", false⟩   -- S_VILE_RUN12
      ],
      ending := .loops "see" 0 },
    { entry := "pain",
      states := #[
        ⟨"VILE", 'Q', 5, "", false⟩,   -- S_VILE_PAIN
        ⟨"VILE", 'Q', 5, "A_Pain", false⟩   -- S_VILE_PAIN2
      ],
      ending := .loops "see" 0 },
    { entry := "missile",
      states := #[
        ⟨"VILE", 'G', 0, "A_VileStart", true⟩,   -- S_VILE_ATK1
        ⟨"VILE", 'G', 10, "A_FaceTarget", true⟩,   -- S_VILE_ATK2
        ⟨"VILE", 'H', 8, "A_VileTarget", true⟩,   -- S_VILE_ATK3
        ⟨"VILE", 'I', 8, "A_FaceTarget", true⟩,   -- S_VILE_ATK4
        ⟨"VILE", 'J', 8, "A_FaceTarget", true⟩,   -- S_VILE_ATK5
        ⟨"VILE", 'K', 8, "A_FaceTarget", true⟩,   -- S_VILE_ATK6
        ⟨"VILE", 'L', 8, "A_FaceTarget", true⟩,   -- S_VILE_ATK7
        ⟨"VILE", 'M', 8, "A_FaceTarget", true⟩,   -- S_VILE_ATK8
        ⟨"VILE", 'N', 8, "A_FaceTarget", true⟩,   -- S_VILE_ATK9
        ⟨"VILE", 'O', 8, "A_VileAttack", true⟩,   -- S_VILE_ATK10
        ⟨"VILE", 'P', 20, "", true⟩   -- S_VILE_ATK11
      ],
      ending := .loops "see" 0 },
    { entry := "death",
      states := #[
        ⟨"VILE", 'Q', 7, "", false⟩,   -- S_VILE_DIE1
        ⟨"VILE", 'R', 7, "A_Scream", false⟩,   -- S_VILE_DIE2
        ⟨"VILE", 'S', 7, "A_Fall", false⟩,   -- S_VILE_DIE3
        ⟨"VILE", 'T', 7, "", false⟩,   -- S_VILE_DIE4
        ⟨"VILE", 'U', 7, "", false⟩,   -- S_VILE_DIE5
        ⟨"VILE", 'V', 7, "", false⟩,   -- S_VILE_DIE6
        ⟨"VILE", 'W', 7, "", false⟩,   -- S_VILE_DIE7
        ⟨"VILE", 'X', 5, "", false⟩,   -- S_VILE_DIE8
        ⟨"VILE", 'Y', 5, "", false⟩,   -- S_VILE_DIE9
        ⟨"VILE", 'Z', -1, "", false⟩   -- S_VILE_DIE10
      ],
      ending := .halt },
    { entry := "heal",
      states := #[
        ⟨"VILE", '[', 10, "", true⟩,   -- S_VILE_HEAL1
        ⟨"VILE", '\\', 10, "", true⟩,   -- S_VILE_HEAL2
        ⟨"VILE", ']', 10, "", true⟩   -- S_VILE_HEAL3
      ],
      ending := .loops "see" 0 }
  ] }

def commanderKeen : Mobj := {
  mt := "MT_KEEN", dillKind := "commanderKeen"
  doomednum := 72
  health := 100
  speed := 0
  radius := 16, height := 72, mass := 10000000
  painChance := 256, damage := 0, reactionTime := 8
  painSound := "dskeenpn", deathSound := "dskeendt"
  solid := true, shootable := true, noGravity := true, countKill := true, spawnCeiling := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"KEEN", 'A', -1, "", false⟩   -- S_KEENSTND
      ],
      ending := .halt },
    { entry := "pain",
      states := #[
        ⟨"KEEN", 'M', 4, "", false⟩,   -- S_KEENPAIN
        ⟨"KEEN", 'M', 8, "A_Pain", false⟩   -- S_KEENPAIN2
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"KEEN", 'A', 6, "", false⟩,   -- S_COMMKEEN
        ⟨"KEEN", 'B', 6, "", false⟩,   -- S_COMMKEEN2
        ⟨"KEEN", 'C', 6, "A_Scream", false⟩,   -- S_COMMKEEN3
        ⟨"KEEN", 'D', 6, "", false⟩,   -- S_COMMKEEN4
        ⟨"KEEN", 'E', 6, "", false⟩,   -- S_COMMKEEN5
        ⟨"KEEN", 'F', 6, "", false⟩,   -- S_COMMKEEN6
        ⟨"KEEN", 'G', 6, "", false⟩,   -- S_COMMKEEN7
        ⟨"KEEN", 'H', 6, "", false⟩,   -- S_COMMKEEN8
        ⟨"KEEN", 'I', 6, "", false⟩,   -- S_COMMKEEN9
        ⟨"KEEN", 'J', 6, "", false⟩,   -- S_COMMKEEN10
        ⟨"KEEN", 'K', 6, "A_KeenDie", false⟩,   -- S_COMMKEEN11
        ⟨"KEEN", 'L', -1, "", false⟩   -- S_COMMKEEN12
      ],
      ending := .halt }
  ] }

def vileFire : Mobj := {
  mt := "MT_FIRE", dillKind := "vileFire"
  doomednum := -1
  health := 1000
  speed := 0
  radius := 20, height := 16, mass := 100
  painChance := 0, damage := 0, reactionTime := 8
  noGravity := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"FIRE", 'A', 2, "A_StartFire", true⟩,   -- S_FIRE1
        ⟨"FIRE", 'B', 2, "A_Fire", true⟩,   -- S_FIRE2
        ⟨"FIRE", 'A', 2, "A_Fire", true⟩,   -- S_FIRE3
        ⟨"FIRE", 'B', 2, "A_Fire", true⟩,   -- S_FIRE4
        ⟨"FIRE", 'C', 2, "A_FireCrackle", true⟩,   -- S_FIRE5
        ⟨"FIRE", 'B', 2, "A_Fire", true⟩,   -- S_FIRE6
        ⟨"FIRE", 'C', 2, "A_Fire", true⟩,   -- S_FIRE7
        ⟨"FIRE", 'B', 2, "A_Fire", true⟩,   -- S_FIRE8
        ⟨"FIRE", 'C', 2, "A_Fire", true⟩,   -- S_FIRE9
        ⟨"FIRE", 'D', 2, "A_Fire", true⟩,   -- S_FIRE10
        ⟨"FIRE", 'C', 2, "A_Fire", true⟩,   -- S_FIRE11
        ⟨"FIRE", 'D', 2, "A_Fire", true⟩,   -- S_FIRE12
        ⟨"FIRE", 'C', 2, "A_Fire", true⟩,   -- S_FIRE13
        ⟨"FIRE", 'D', 2, "A_Fire", true⟩,   -- S_FIRE14
        ⟨"FIRE", 'E', 2, "A_Fire", true⟩,   -- S_FIRE15
        ⟨"FIRE", 'D', 2, "A_Fire", true⟩,   -- S_FIRE16
        ⟨"FIRE", 'E', 2, "A_Fire", true⟩,   -- S_FIRE17
        ⟨"FIRE", 'D', 2, "A_Fire", true⟩,   -- S_FIRE18
        ⟨"FIRE", 'E', 2, "A_FireCrackle", true⟩,   -- S_FIRE19
        ⟨"FIRE", 'F', 2, "A_Fire", true⟩,   -- S_FIRE20
        ⟨"FIRE", 'E', 2, "A_Fire", true⟩,   -- S_FIRE21
        ⟨"FIRE", 'F', 2, "A_Fire", true⟩,   -- S_FIRE22
        ⟨"FIRE", 'E', 2, "A_Fire", true⟩,   -- S_FIRE23
        ⟨"FIRE", 'F', 2, "A_Fire", true⟩,   -- S_FIRE24
        ⟨"FIRE", 'G', 2, "A_Fire", true⟩,   -- S_FIRE25
        ⟨"FIRE", 'H', 2, "A_Fire", true⟩,   -- S_FIRE26
        ⟨"FIRE", 'G', 2, "A_Fire", true⟩,   -- S_FIRE27
        ⟨"FIRE", 'H', 2, "A_Fire", true⟩,   -- S_FIRE28
        ⟨"FIRE", 'G', 2, "A_Fire", true⟩,   -- S_FIRE29
        ⟨"FIRE", 'H', 2, "A_Fire", true⟩   -- S_FIRE30
      ],
      ending := .remove }
  ] }

def iconBrain : Mobj := {
  mt := "MT_BOSSBRAIN", dillKind := "iconBrain"
  doomednum := 88
  health := 250
  speed := 0
  radius := 16, height := 16, mass := 10000000
  painChance := 255, damage := 0, reactionTime := 8
  painSound := "dsbospn", deathSound := "dsbosdth"
  solid := true, shootable := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BBRN", 'A', -1, "", false⟩   -- S_BRAIN
      ],
      ending := .halt },
    { entry := "pain",
      states := #[
        ⟨"BBRN", 'B', 36, "A_BrainPain", false⟩   -- S_BRAIN_PAIN
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"BBRN", 'A', 100, "A_BrainScream", false⟩,   -- S_BRAIN_DIE1
        ⟨"BBRN", 'A', 10, "", false⟩,   -- S_BRAIN_DIE2
        ⟨"BBRN", 'A', 10, "", false⟩,   -- S_BRAIN_DIE3
        ⟨"BBRN", 'A', -1, "A_BrainDie", false⟩   -- S_BRAIN_DIE4
      ],
      ending := .halt }
  ] }

def iconSpit : Mobj := {
  mt := "MT_BOSSSPIT", dillKind := "iconSpit"
  doomednum := 89
  health := 1000
  speed := 0
  radius := 20, height := 32, mass := 100
  painChance := 0, damage := 0, reactionTime := 8
  noSector := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"SSWV", 'A', 10, "A_Look", false⟩   -- S_BRAINEYE
      ],
      ending := .loops "spawn" 0 },
    { entry := "see",
      states := #[
        ⟨"SSWV", 'A', 181, "A_BrainAwake", false⟩,   -- S_BRAINEYESEE
        ⟨"SSWV", 'A', 150, "A_BrainSpit", false⟩   -- S_BRAINEYE1
      ],
      ending := .loops "see" 1 }
  ] }

def iconTarget : Mobj := {
  mt := "MT_BOSSTARGET", dillKind := "iconTarget"
  doomednum := 87
  health := 1000
  speed := 0
  radius := 20, height := 32, mass := 100
  painChance := 0, damage := 0, reactionTime := 8
  noSector := true, noBlockmap := true
  chains := #[

  ] }

def spawnCube : Mobj := {
  mt := "MT_SPAWNSHOT", dillKind := "spawnCube"
  doomednum := -1
  health := 1000
  speed := 10 -- map units/tic (info.c: 10*FRACUNIT)
  radius := 6, height := 32, mass := 100
  painChance := 0, damage := 3, reactionTime := 8
  seeSound := "dsbospit", deathSound := "dsfirxpl"
  missile := true, noGravity := true, dropOff := true, noBlockmap := true
  otherFlags := #["MF_NOCLIP"]
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BOSF", 'A', 3, "A_SpawnSound", true⟩,   -- S_SPAWN1
        ⟨"BOSF", 'B', 3, "A_SpawnFly", true⟩,   -- S_SPAWN2
        ⟨"BOSF", 'C', 3, "A_SpawnFly", true⟩,   -- S_SPAWN3
        ⟨"BOSF", 'D', 3, "A_SpawnFly", true⟩   -- S_SPAWN4
      ],
      ending := .loops "spawn" 0 }
  ] }

def barrel : Mobj := {
  mt := "MT_BARREL", dillKind := "barrel"
  doomednum := 2035
  health := 20
  speed := 0
  radius := 10, height := 42, mass := 100
  painChance := 0, damage := 0, reactionTime := 8
  deathSound := "dsbarexp"
  solid := true, shootable := true, noBlood := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BAR1", 'A', 6, "", false⟩,   -- S_BAR1
        ⟨"BAR1", 'B', 6, "", false⟩   -- S_BAR2
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"BEXP", 'A', 5, "", true⟩,   -- S_BEXP
        ⟨"BEXP", 'B', 5, "A_Scream", true⟩,   -- S_BEXP2
        ⟨"BEXP", 'C', 5, "", true⟩,   -- S_BEXP3
        ⟨"BEXP", 'D', 10, "A_Explode", true⟩,   -- S_BEXP4
        ⟨"BEXP", 'E', 10, "", true⟩   -- S_BEXP5
      ],
      ending := .remove }
  ] }

def impBall : Mobj := {
  mt := "MT_TROOPSHOT", dillKind := "impBall"
  doomednum := -1
  health := 1000
  speed := 10 -- map units/tic (info.c: 10*FRACUNIT)
  radius := 6, height := 8, mass := 100
  painChance := 0, damage := 3, reactionTime := 8
  seeSound := "dsfirsht", deathSound := "dsfirxpl"
  missile := true, noGravity := true, dropOff := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BAL1", 'A', 4, "", true⟩,   -- S_TBALL1
        ⟨"BAL1", 'B', 4, "", true⟩   -- S_TBALL2
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"BAL1", 'C', 6, "", true⟩,   -- S_TBALLX1
        ⟨"BAL1", 'D', 6, "", true⟩,   -- S_TBALLX2
        ⟨"BAL1", 'E', 6, "", true⟩   -- S_TBALLX3
      ],
      ending := .remove }
  ] }

def cacoBall : Mobj := {
  mt := "MT_HEADSHOT", dillKind := "cacoBall"
  doomednum := -1
  health := 1000
  speed := 10 -- map units/tic (info.c: 10*FRACUNIT)
  radius := 6, height := 8, mass := 100
  painChance := 0, damage := 5, reactionTime := 8
  seeSound := "dsfirsht", deathSound := "dsfirxpl"
  missile := true, noGravity := true, dropOff := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BAL2", 'A', 4, "", true⟩,   -- S_RBALL1
        ⟨"BAL2", 'B', 4, "", true⟩   -- S_RBALL2
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"BAL2", 'C', 6, "", true⟩,   -- S_RBALLX1
        ⟨"BAL2", 'D', 6, "", true⟩,   -- S_RBALLX2
        ⟨"BAL2", 'E', 6, "", true⟩   -- S_RBALLX3
      ],
      ending := .remove }
  ] }

def baronBall : Mobj := {
  mt := "MT_BRUISERSHOT", dillKind := "baronBall"
  doomednum := -1
  health := 1000
  speed := 15 -- map units/tic (info.c: 15*FRACUNIT)
  radius := 6, height := 8, mass := 100
  painChance := 0, damage := 8, reactionTime := 8
  seeSound := "dsfirsht", deathSound := "dsfirxpl"
  missile := true, noGravity := true, dropOff := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BAL7", 'A', 4, "", true⟩,   -- S_BRBALL1
        ⟨"BAL7", 'B', 4, "", true⟩   -- S_BRBALL2
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"BAL7", 'C', 6, "", true⟩,   -- S_BRBALLX1
        ⟨"BAL7", 'D', 6, "", true⟩,   -- S_BRBALLX2
        ⟨"BAL7", 'E', 6, "", true⟩   -- S_BRBALLX3
      ],
      ending := .remove }
  ] }

def puff : Mobj := {
  mt := "MT_PUFF", dillKind := "puff"
  doomednum := -1
  health := 1000
  speed := 0
  radius := 20, height := 16, mass := 100
  painChance := 0, damage := 0, reactionTime := 8
  noGravity := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"PUFF", 'A', 4, "", true⟩,   -- S_PUFF1
        ⟨"PUFF", 'B', 4, "", false⟩,   -- S_PUFF2
        ⟨"PUFF", 'C', 4, "", false⟩,   -- S_PUFF3
        ⟨"PUFF", 'D', 4, "", false⟩   -- S_PUFF4
      ],
      ending := .remove }
  ] }

def blood : Mobj := {
  mt := "MT_BLOOD", dillKind := "blood"
  doomednum := -1
  health := 1000
  speed := 0
  radius := 20, height := 16, mass := 100
  painChance := 0, damage := 0, reactionTime := 8
  noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BLUD", 'C', 8, "", false⟩,   -- S_BLOOD1
        ⟨"BLUD", 'B', 8, "", false⟩,   -- S_BLOOD2
        ⟨"BLUD", 'A', 8, "", false⟩   -- S_BLOOD3
      ],
      ending := .remove }
  ] }

def teleFog : Mobj := {
  mt := "MT_TFOG", dillKind := "teleFog"
  doomednum := -1
  health := 1000
  speed := 0
  radius := 20, height := 16, mass := 100
  painChance := 0, damage := 0, reactionTime := 8
  noGravity := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"TFOG", 'A', 6, "", true⟩,   -- S_TFOG
        ⟨"TFOG", 'B', 6, "", true⟩,   -- S_TFOG01
        ⟨"TFOG", 'A', 6, "", true⟩,   -- S_TFOG02
        ⟨"TFOG", 'B', 6, "", true⟩,   -- S_TFOG2
        ⟨"TFOG", 'C', 6, "", true⟩,   -- S_TFOG3
        ⟨"TFOG", 'D', 6, "", true⟩,   -- S_TFOG4
        ⟨"TFOG", 'E', 6, "", true⟩,   -- S_TFOG5
        ⟨"TFOG", 'F', 6, "", true⟩,   -- S_TFOG6
        ⟨"TFOG", 'G', 6, "", true⟩,   -- S_TFOG7
        ⟨"TFOG", 'H', 6, "", true⟩,   -- S_TFOG8
        ⟨"TFOG", 'I', 6, "", true⟩,   -- S_TFOG9
        ⟨"TFOG", 'J', 6, "", true⟩   -- S_TFOG10
      ],
      ending := .remove }
  ] }

def rocket : Mobj := {
  mt := "MT_ROCKET", dillKind := "rocket"
  doomednum := -1
  health := 1000
  speed := 20 -- map units/tic (info.c: 20*FRACUNIT)
  radius := 11, height := 8, mass := 100
  painChance := 0, damage := 20, reactionTime := 8
  seeSound := "dsrlaunc", deathSound := "dsbarexp"
  missile := true, noGravity := true, dropOff := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"MISL", 'A', 1, "", true⟩   -- S_ROCKET
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"MISL", 'B', 8, "A_Explode", true⟩,   -- S_EXPLODE1
        ⟨"MISL", 'C', 6, "", true⟩,   -- S_EXPLODE2
        ⟨"MISL", 'D', 4, "", true⟩   -- S_EXPLODE3
      ],
      ending := .remove }
  ] }

def plasmaBall : Mobj := {
  mt := "MT_PLASMA", dillKind := "plasmaBall"
  doomednum := -1
  health := 1000
  speed := 25 -- map units/tic (info.c: 25*FRACUNIT)
  radius := 13, height := 8, mass := 100
  painChance := 0, damage := 5, reactionTime := 8
  seeSound := "dsplasma", deathSound := "dsfirxpl"
  missile := true, noGravity := true, dropOff := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"PLSS", 'A', 6, "", true⟩,   -- S_PLASBALL
        ⟨"PLSS", 'B', 6, "", true⟩   -- S_PLASBALL2
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"PLSE", 'A', 4, "", true⟩,   -- S_PLASEXP
        ⟨"PLSE", 'B', 4, "", true⟩,   -- S_PLASEXP2
        ⟨"PLSE", 'C', 4, "", true⟩,   -- S_PLASEXP3
        ⟨"PLSE", 'D', 4, "", true⟩,   -- S_PLASEXP4
        ⟨"PLSE", 'E', 4, "", true⟩   -- S_PLASEXP5
      ],
      ending := .remove }
  ] }

def bfgBall : Mobj := {
  mt := "MT_BFG", dillKind := "bfgBall"
  doomednum := -1
  health := 1000
  speed := 25 -- map units/tic (info.c: 25*FRACUNIT)
  radius := 13, height := 8, mass := 100
  painChance := 0, damage := 100, reactionTime := 8
  deathSound := "dsrxplod"
  missile := true, noGravity := true, dropOff := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BFS1", 'A', 4, "", true⟩,   -- S_BFGSHOT
        ⟨"BFS1", 'B', 4, "", true⟩   -- S_BFGSHOT2
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"BFE1", 'A', 8, "", true⟩,   -- S_BFGLAND
        ⟨"BFE1", 'B', 8, "", true⟩,   -- S_BFGLAND2
        ⟨"BFE1", 'C', 8, "A_BFGSpray", true⟩,   -- S_BFGLAND3
        ⟨"BFE1", 'D', 8, "", true⟩,   -- S_BFGLAND4
        ⟨"BFE1", 'E', 8, "", true⟩,   -- S_BFGLAND5
        ⟨"BFE1", 'F', 8, "", true⟩   -- S_BFGLAND6
      ],
      ending := .remove }
  ] }

def bfgPuff : Mobj := {
  mt := "MT_EXTRABFG", dillKind := "bfgPuff"
  doomednum := -1
  health := 1000
  speed := 0
  radius := 20, height := 16, mass := 100
  painChance := 0, damage := 0, reactionTime := 8
  noGravity := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"BFE2", 'A', 8, "", true⟩,   -- S_BFGEXP
        ⟨"BFE2", 'B', 8, "", true⟩,   -- S_BFGEXP2
        ⟨"BFE2", 'C', 8, "", true⟩,   -- S_BFGEXP3
        ⟨"BFE2", 'D', 8, "", true⟩   -- S_BFGEXP4
      ],
      ending := .remove }
  ] }

def fatShot : Mobj := {
  mt := "MT_FATSHOT", dillKind := "fatShot"
  doomednum := -1
  health := 1000
  speed := 20 -- map units/tic (info.c: 20*FRACUNIT)
  radius := 6, height := 8, mass := 100
  painChance := 0, damage := 8, reactionTime := 8
  seeSound := "dsfirsht", deathSound := "dsfirxpl"
  missile := true, noGravity := true, dropOff := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"MANF", 'A', 4, "", true⟩,   -- S_FATSHOT1
        ⟨"MANF", 'B', 4, "", true⟩   -- S_FATSHOT2
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"MISL", 'B', 8, "", true⟩,   -- S_FATSHOTX1
        ⟨"MISL", 'C', 6, "", true⟩,   -- S_FATSHOTX2
        ⟨"MISL", 'D', 4, "", true⟩   -- S_FATSHOTX3
      ],
      ending := .remove }
  ] }

def arachPlasma : Mobj := {
  mt := "MT_ARACHPLAZ", dillKind := "arachPlasma"
  doomednum := -1
  health := 1000
  speed := 25 -- map units/tic (info.c: 25*FRACUNIT)
  radius := 13, height := 8, mass := 100
  painChance := 0, damage := 5, reactionTime := 8
  seeSound := "dsplasma", deathSound := "dsfirxpl"
  missile := true, noGravity := true, dropOff := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"APLS", 'A', 5, "", true⟩,   -- S_ARACH_PLAZ
        ⟨"APLS", 'B', 5, "", true⟩   -- S_ARACH_PLAZ2
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"APBX", 'A', 5, "", true⟩,   -- S_ARACH_PLEX
        ⟨"APBX", 'B', 5, "", true⟩,   -- S_ARACH_PLEX2
        ⟨"APBX", 'C', 5, "", true⟩,   -- S_ARACH_PLEX3
        ⟨"APBX", 'D', 5, "", true⟩,   -- S_ARACH_PLEX4
        ⟨"APBX", 'E', 5, "", true⟩   -- S_ARACH_PLEX5
      ],
      ending := .remove }
  ] }

def revenantMissile : Mobj := {
  mt := "MT_TRACER", dillKind := "revenantMissile"
  doomednum := -1
  health := 1000
  speed := 10 -- map units/tic (info.c: 10*FRACUNIT)
  radius := 11, height := 8, mass := 100
  painChance := 0, damage := 10, reactionTime := 8
  seeSound := "dsskeatk", deathSound := "dsbarexp"
  missile := true, noGravity := true, dropOff := true, noBlockmap := true
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"FATB", 'A', 2, "A_Tracer", true⟩,   -- S_TRACER
        ⟨"FATB", 'B', 2, "A_Tracer", true⟩   -- S_TRACER2
      ],
      ending := .loops "spawn" 0 },
    { entry := "death",
      states := #[
        ⟨"FBXP", 'A', 8, "", true⟩,   -- S_TRACEEXP1
        ⟨"FBXP", 'B', 6, "", true⟩,   -- S_TRACEEXP2
        ⟨"FBXP", 'C', 4, "", true⟩   -- S_TRACEEXP3
      ],
      ending := .remove }
  ] }

/-- DILL's `brainExplosion` has no vanilla mobjinfo entry: vanilla spawns an `MT_ROCKET`
and forces it into `S_BRAINEXPLODE1` (`A_BrainScream`/`A_BrainExplode` in p_enemy.c),
so its physique fields are `MT_ROCKET`'s. Only the state chain is distinct: -/
def brainExplosion : Mobj := { rocket with
  mt := "MT_ROCKET (S_BRAINEXPLODE1)", dillKind := "brainExplosion"
  chains := #[
    { entry := "spawn",
      states := #[
        ⟨"MISL", 'B', 10, "", true⟩,   -- S_BRAINEXPLODE1
        ⟨"MISL", 'C', 10, "", true⟩,   -- S_BRAINEXPLODE2
        ⟨"MISL", 'D', 10, "A_BrainExplode", true⟩   -- S_BRAINEXPLODE3
      ],
      ending := .remove }
  ] }

/-- Every fixed-`ActorKind` actor, in DILL `ActorKind` declaration order
(the parameterized `.item`/`.scenery` families are in `things` below). -/
def mobjs : Array Mobj := #[
  zombieman, shotgunGuy, imp, demon, spectre,
  cacodemon, lostSoul, baron, cyberdemon, spiderMastermind,
  chaingunner, wolfSS, hellKnight, mancubus, arachnotron, revenant,
  painElemental, archVile, commanderKeen, vileFire,
  iconBrain, iconSpit, iconTarget, spawnCube, brainExplosion,
  barrel, impBall, cacoBall, baronBall, puff, blood, teleFog,
  rocket, plasmaBall, bfgBall, bfgPuff,
  fatShot, arachPlasma, revenantMissile]

/-! ## weaponinfo[] — d_items.c

Chains walked from `upstate`/`downstate`/`readystate`/`atkstate`/
`flashstate`. The up/down/ready states loop on themselves (1-tic
`A_Raise`/`A_Lower`/`A_WeaponReady` beats); an attack chain that returns
to the ready state ends `loops "ready" 0`. Flash chains run through the
shared `S_LIGHTDONE` terminator (SHTG frame E, 0 tics, `A_Light0`,
then removed), recorded inline at the end of each flash chain. -/

/-- Ammo-type names are vanilla's `ammotype_t` constants verbatim. -/
structure WeaponDef where
  name : String
  ammo : String
  chains : Array Chain
  deriving Repr

def fist : WeaponDef := {
  name := "fist", ammo := "am_noammo"
  chains := #[
    { entry := "up",
      states := #[
        ⟨"PUNG", 'A', 1, "A_Raise", false⟩   -- S_PUNCHUP
      ],
      ending := .loops "up" 0 },
    { entry := "down",
      states := #[
        ⟨"PUNG", 'A', 1, "A_Lower", false⟩   -- S_PUNCHDOWN
      ],
      ending := .loops "down" 0 },
    { entry := "ready",
      states := #[
        ⟨"PUNG", 'A', 1, "A_WeaponReady", false⟩   -- S_PUNCH
      ],
      ending := .loops "ready" 0 },
    { entry := "atk",
      states := #[
        ⟨"PUNG", 'B', 4, "", false⟩,   -- S_PUNCH1
        ⟨"PUNG", 'C', 4, "A_Punch", false⟩,   -- S_PUNCH2
        ⟨"PUNG", 'D', 5, "", false⟩,   -- S_PUNCH3
        ⟨"PUNG", 'C', 4, "", false⟩,   -- S_PUNCH4
        ⟨"PUNG", 'B', 5, "A_ReFire", false⟩   -- S_PUNCH5
      ],
      ending := .loops "ready" 0 }
  ] }

def pistol : WeaponDef := {
  name := "pistol", ammo := "am_clip"
  chains := #[
    { entry := "up",
      states := #[
        ⟨"PISG", 'A', 1, "A_Raise", false⟩   -- S_PISTOLUP
      ],
      ending := .loops "up" 0 },
    { entry := "down",
      states := #[
        ⟨"PISG", 'A', 1, "A_Lower", false⟩   -- S_PISTOLDOWN
      ],
      ending := .loops "down" 0 },
    { entry := "ready",
      states := #[
        ⟨"PISG", 'A', 1, "A_WeaponReady", false⟩   -- S_PISTOL
      ],
      ending := .loops "ready" 0 },
    { entry := "atk",
      states := #[
        ⟨"PISG", 'A', 4, "", false⟩,   -- S_PISTOL1
        ⟨"PISG", 'B', 6, "A_FirePistol", false⟩,   -- S_PISTOL2
        ⟨"PISG", 'C', 4, "", false⟩,   -- S_PISTOL3
        ⟨"PISG", 'B', 5, "A_ReFire", false⟩   -- S_PISTOL4
      ],
      ending := .loops "ready" 0 },
    { entry := "flash",
      states := #[
        ⟨"PISF", 'A', 7, "A_Light1", true⟩,   -- S_PISTOLFLASH
        ⟨"SHTG", 'E', 0, "A_Light0", false⟩   -- S_LIGHTDONE
      ],
      ending := .remove }
  ] }

def shotgun : WeaponDef := {
  name := "shotgun", ammo := "am_shell"
  chains := #[
    { entry := "up",
      states := #[
        ⟨"SHTG", 'A', 1, "A_Raise", false⟩   -- S_SGUNUP
      ],
      ending := .loops "up" 0 },
    { entry := "down",
      states := #[
        ⟨"SHTG", 'A', 1, "A_Lower", false⟩   -- S_SGUNDOWN
      ],
      ending := .loops "down" 0 },
    { entry := "ready",
      states := #[
        ⟨"SHTG", 'A', 1, "A_WeaponReady", false⟩   -- S_SGUN
      ],
      ending := .loops "ready" 0 },
    { entry := "atk",
      states := #[
        ⟨"SHTG", 'A', 3, "", false⟩,   -- S_SGUN1
        ⟨"SHTG", 'A', 7, "A_FireShotgun", false⟩,   -- S_SGUN2
        ⟨"SHTG", 'B', 5, "", false⟩,   -- S_SGUN3
        ⟨"SHTG", 'C', 5, "", false⟩,   -- S_SGUN4
        ⟨"SHTG", 'D', 4, "", false⟩,   -- S_SGUN5
        ⟨"SHTG", 'C', 5, "", false⟩,   -- S_SGUN6
        ⟨"SHTG", 'B', 5, "", false⟩,   -- S_SGUN7
        ⟨"SHTG", 'A', 3, "", false⟩,   -- S_SGUN8
        ⟨"SHTG", 'A', 7, "A_ReFire", false⟩   -- S_SGUN9
      ],
      ending := .loops "ready" 0 },
    { entry := "flash",
      states := #[
        ⟨"SHTF", 'A', 4, "A_Light1", true⟩,   -- S_SGUNFLASH1
        ⟨"SHTF", 'B', 3, "A_Light2", true⟩,   -- S_SGUNFLASH2
        ⟨"SHTG", 'E', 0, "A_Light0", false⟩   -- S_LIGHTDONE
      ],
      ending := .remove }
  ] }

def chaingun : WeaponDef := {
  name := "chaingun", ammo := "am_clip"
  chains := #[
    { entry := "up",
      states := #[
        ⟨"CHGG", 'A', 1, "A_Raise", false⟩   -- S_CHAINUP
      ],
      ending := .loops "up" 0 },
    { entry := "down",
      states := #[
        ⟨"CHGG", 'A', 1, "A_Lower", false⟩   -- S_CHAINDOWN
      ],
      ending := .loops "down" 0 },
    { entry := "ready",
      states := #[
        ⟨"CHGG", 'A', 1, "A_WeaponReady", false⟩   -- S_CHAIN
      ],
      ending := .loops "ready" 0 },
    { entry := "atk",
      states := #[
        ⟨"CHGG", 'A', 4, "A_FireCGun", false⟩,   -- S_CHAIN1
        ⟨"CHGG", 'B', 4, "A_FireCGun", false⟩,   -- S_CHAIN2
        ⟨"CHGG", 'B', 0, "A_ReFire", false⟩   -- S_CHAIN3
      ],
      ending := .loops "ready" 0 },
    { entry := "flash",
      states := #[
        ⟨"CHGF", 'A', 5, "A_Light1", true⟩,   -- S_CHAINFLASH1
        ⟨"SHTG", 'E', 0, "A_Light0", false⟩   -- S_LIGHTDONE
      ],
      ending := .remove }
  ] }

def rocketLauncher : WeaponDef := {
  name := "missile launcher", ammo := "am_misl"
  chains := #[
    { entry := "up",
      states := #[
        ⟨"MISG", 'A', 1, "A_Raise", false⟩   -- S_MISSILEUP
      ],
      ending := .loops "up" 0 },
    { entry := "down",
      states := #[
        ⟨"MISG", 'A', 1, "A_Lower", false⟩   -- S_MISSILEDOWN
      ],
      ending := .loops "down" 0 },
    { entry := "ready",
      states := #[
        ⟨"MISG", 'A', 1, "A_WeaponReady", false⟩   -- S_MISSILE
      ],
      ending := .loops "ready" 0 },
    { entry := "atk",
      states := #[
        ⟨"MISG", 'B', 8, "A_GunFlash", false⟩,   -- S_MISSILE1
        ⟨"MISG", 'B', 12, "A_FireMissile", false⟩,   -- S_MISSILE2
        ⟨"MISG", 'B', 0, "A_ReFire", false⟩   -- S_MISSILE3
      ],
      ending := .loops "ready" 0 },
    { entry := "flash",
      states := #[
        ⟨"MISF", 'A', 3, "A_Light1", true⟩,   -- S_MISSILEFLASH1
        ⟨"MISF", 'B', 4, "", true⟩,   -- S_MISSILEFLASH2
        ⟨"MISF", 'C', 4, "A_Light2", true⟩,   -- S_MISSILEFLASH3
        ⟨"MISF", 'D', 4, "A_Light2", true⟩,   -- S_MISSILEFLASH4
        ⟨"SHTG", 'E', 0, "A_Light0", false⟩   -- S_LIGHTDONE
      ],
      ending := .remove }
  ] }

def plasmaRifle : WeaponDef := {
  name := "plasma rifle", ammo := "am_cell"
  chains := #[
    { entry := "up",
      states := #[
        ⟨"PLSG", 'A', 1, "A_Raise", false⟩   -- S_PLASMAUP
      ],
      ending := .loops "up" 0 },
    { entry := "down",
      states := #[
        ⟨"PLSG", 'A', 1, "A_Lower", false⟩   -- S_PLASMADOWN
      ],
      ending := .loops "down" 0 },
    { entry := "ready",
      states := #[
        ⟨"PLSG", 'A', 1, "A_WeaponReady", false⟩   -- S_PLASMA
      ],
      ending := .loops "ready" 0 },
    { entry := "atk",
      states := #[
        ⟨"PLSG", 'A', 3, "A_FirePlasma", false⟩,   -- S_PLASMA1
        ⟨"PLSG", 'B', 20, "A_ReFire", false⟩   -- S_PLASMA2
      ],
      ending := .loops "ready" 0 },
    { entry := "flash",
      states := #[
        ⟨"PLSF", 'A', 4, "A_Light1", true⟩,   -- S_PLASMAFLASH1
        ⟨"SHTG", 'E', 0, "A_Light0", false⟩   -- S_LIGHTDONE
      ],
      ending := .remove }
  ] }

def bfg9000 : WeaponDef := {
  name := "bfg 9000", ammo := "am_cell"
  chains := #[
    { entry := "up",
      states := #[
        ⟨"BFGG", 'A', 1, "A_Raise", false⟩   -- S_BFGUP
      ],
      ending := .loops "up" 0 },
    { entry := "down",
      states := #[
        ⟨"BFGG", 'A', 1, "A_Lower", false⟩   -- S_BFGDOWN
      ],
      ending := .loops "down" 0 },
    { entry := "ready",
      states := #[
        ⟨"BFGG", 'A', 1, "A_WeaponReady", false⟩   -- S_BFG
      ],
      ending := .loops "ready" 0 },
    { entry := "atk",
      states := #[
        ⟨"BFGG", 'A', 20, "A_BFGsound", false⟩,   -- S_BFG1
        ⟨"BFGG", 'B', 10, "A_GunFlash", false⟩,   -- S_BFG2
        ⟨"BFGG", 'B', 10, "A_FireBFG", false⟩,   -- S_BFG3
        ⟨"BFGG", 'B', 20, "A_ReFire", false⟩   -- S_BFG4
      ],
      ending := .loops "ready" 0 },
    { entry := "flash",
      states := #[
        ⟨"BFGF", 'A', 11, "A_Light1", true⟩,   -- S_BFGFLASH1
        ⟨"BFGF", 'B', 6, "A_Light2", true⟩,   -- S_BFGFLASH2
        ⟨"SHTG", 'E', 0, "A_Light0", false⟩   -- S_LIGHTDONE
      ],
      ending := .remove }
  ] }

def chainsaw : WeaponDef := {
  name := "chainsaw", ammo := "am_noammo"
  chains := #[
    { entry := "up",
      states := #[
        ⟨"SAWG", 'C', 1, "A_Raise", false⟩   -- S_SAWUP
      ],
      ending := .loops "up" 0 },
    { entry := "down",
      states := #[
        ⟨"SAWG", 'C', 1, "A_Lower", false⟩   -- S_SAWDOWN
      ],
      ending := .loops "down" 0 },
    { entry := "ready",
      states := #[
        ⟨"SAWG", 'C', 4, "A_WeaponReady", false⟩,   -- S_SAW
        ⟨"SAWG", 'D', 4, "A_WeaponReady", false⟩   -- S_SAWB
      ],
      ending := .loops "ready" 0 },
    { entry := "atk",
      states := #[
        ⟨"SAWG", 'A', 4, "A_Saw", false⟩,   -- S_SAW1
        ⟨"SAWG", 'B', 4, "A_Saw", false⟩,   -- S_SAW2
        ⟨"SAWG", 'B', 0, "A_ReFire", false⟩   -- S_SAW3
      ],
      ending := .loops "ready" 0 }
  ] }

/- Vanilla source, info.c states[] for the super-shotgun attack (verbatim):
    {SPR_SHT2,0,3,{NULL},S_DSGUN2,0,0},	// S_DSGUN1
    {SPR_SHT2,0,7,{A_FireShotgun2},S_DSGUN3,0,0},	// S_DSGUN2
    {SPR_SHT2,1,7,{NULL},S_DSGUN4,0,0},	// S_DSGUN3
    {SPR_SHT2,2,7,{A_CheckReload},S_DSGUN5,0,0},	// S_DSGUN4
    {SPR_SHT2,3,7,{A_OpenShotgun2},S_DSGUN6,0,0},	// S_DSGUN5
    {SPR_SHT2,4,7,{NULL},S_DSGUN7,0,0},	// S_DSGUN6
    {SPR_SHT2,5,7,{A_LoadShotgun2},S_DSGUN8,0,0},	// S_DSGUN7
    {SPR_SHT2,6,6,{NULL},S_DSGUN9,0,0},	// S_DSGUN8
    {SPR_SHT2,7,6,{A_CloseShotgun2},S_DSGUN10,0,0},	// S_DSGUN9
    {SPR_SHT2,0,5,{A_ReFire},S_DSGUN,0,0},	// S_DSGUN10
   flash:
    {SPR_SHT2,32776,5,{A_Light1},S_DSGUNFLASH2,0,0},	// S_DSGUNFLASH1
    {SPR_SHT2,32777,4,{A_Light2},S_LIGHTDONE,0,0},	// S_DSGUNFLASH2
-/
def superShotgun : WeaponDef := {
  name := "super shotgun", ammo := "am_shell"
  chains := #[
    { entry := "up",
      states := #[
        ⟨"SHT2", 'A', 1, "A_Raise", false⟩   -- S_DSGUNUP
      ],
      ending := .loops "up" 0 },
    { entry := "down",
      states := #[
        ⟨"SHT2", 'A', 1, "A_Lower", false⟩   -- S_DSGUNDOWN
      ],
      ending := .loops "down" 0 },
    { entry := "ready",
      states := #[
        ⟨"SHT2", 'A', 1, "A_WeaponReady", false⟩   -- S_DSGUN
      ],
      ending := .loops "ready" 0 },
    { entry := "atk",
      states := #[
        ⟨"SHT2", 'A', 3, "", false⟩,   -- S_DSGUN1
        ⟨"SHT2", 'A', 7, "A_FireShotgun2", false⟩,   -- S_DSGUN2
        ⟨"SHT2", 'B', 7, "", false⟩,   -- S_DSGUN3
        ⟨"SHT2", 'C', 7, "A_CheckReload", false⟩,   -- S_DSGUN4
        ⟨"SHT2", 'D', 7, "A_OpenShotgun2", false⟩,   -- S_DSGUN5
        ⟨"SHT2", 'E', 7, "", false⟩,   -- S_DSGUN6
        ⟨"SHT2", 'F', 7, "A_LoadShotgun2", false⟩,   -- S_DSGUN7
        ⟨"SHT2", 'G', 6, "", false⟩,   -- S_DSGUN8
        ⟨"SHT2", 'H', 6, "A_CloseShotgun2", false⟩,   -- S_DSGUN9
        ⟨"SHT2", 'A', 5, "A_ReFire", false⟩   -- S_DSGUN10
      ],
      ending := .loops "ready" 0 },
    { entry := "flash",
      states := #[
        ⟨"SHT2", 'I', 5, "A_Light1", true⟩,   -- S_DSGUNFLASH1
        ⟨"SHT2", 'J', 4, "A_Light2", true⟩,   -- S_DSGUNFLASH2
        ⟨"SHTG", 'E', 0, "A_Light0", false⟩   -- S_LIGHTDONE
      ],
      ending := .remove }
  ] }

/-- The nine weapons, in vanilla `weapontype_t` order. -/
def weaponinfo : Array WeaponDef := #[
  fist, pistol, shotgun, chaingun, rocketLauncher,
  plasmaRifle, bfg9000, chainsaw, superShotgun]

/-! ## Item and scenery things

DILL's `.item`/`.scenery` `ActorKind`s are parameterized by sprite family
and frame string; vanilla gives each doomednum its own `mobjinfo` entry.
One `ThingDef` per doomednum that `ActorKind.ofThingType` maps to
`.item`/`.scenery`, with the spawn chain walked from `states[]` (this is
where per-frame tics and bright bits live — e.g. the soulsphere's six
frames are 6-tic and fullbright) and the raw vanilla flag list. -/

structure ThingDef where
  doomednum : Nat
  mt : String
  radius : Float
  height : Float
  flags : Array String
  chain : Chain
  deriving Repr

def things : Array ThingDef := #[
  { doomednum := 82, mt := "MT_SUPERSHOTGUN", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SGN2", 'A', -1, "", false⟩   -- S_SHOT2
        ],
        ending := .halt } },
  { doomednum := 2001, mt := "MT_SHOTGUN", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SHOT", 'A', -1, "", false⟩   -- S_SHOT
        ],
        ending := .halt } },
  { doomednum := 2002, mt := "MT_CHAINGUN", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"MGUN", 'A', -1, "", false⟩   -- S_MGUN
        ],
        ending := .halt } },
  { doomednum := 2003, mt := "MT_MISC27", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"LAUN", 'A', -1, "", false⟩   -- S_LAUN
        ],
        ending := .halt } },
  { doomednum := 2004, mt := "MT_MISC28", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"PLAS", 'A', -1, "", false⟩   -- S_PLAS
        ],
        ending := .halt } },
  { doomednum := 2005, mt := "MT_MISC26", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"CSAW", 'A', -1, "", false⟩   -- S_CSAW
        ],
        ending := .halt } },
  { doomednum := 2006, mt := "MT_MISC25", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"BFUG", 'A', -1, "", false⟩   -- S_BFUG
        ],
        ending := .halt } },
  { doomednum := 2007, mt := "MT_CLIP", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"CLIP", 'A', -1, "", false⟩   -- S_CLIP
        ],
        ending := .halt } },
  { doomednum := 2008, mt := "MT_MISC22", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SHEL", 'A', -1, "", false⟩   -- S_SHEL
        ],
        ending := .halt } },
  { doomednum := 2048, mt := "MT_MISC17", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"AMMO", 'A', -1, "", false⟩   -- S_AMMO
        ],
        ending := .halt } },
  { doomednum := 2049, mt := "MT_MISC23", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SBOX", 'A', -1, "", false⟩   -- S_SBOX
        ],
        ending := .halt } },
  { doomednum := 2010, mt := "MT_MISC18", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"ROCK", 'A', -1, "", false⟩   -- S_ROCK
        ],
        ending := .halt } },
  { doomednum := 2046, mt := "MT_MISC19", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"BROK", 'A', -1, "", false⟩   -- S_BROK
        ],
        ending := .halt } },
  { doomednum := 2047, mt := "MT_MISC20", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"CELL", 'A', -1, "", false⟩   -- S_CELL
        ],
        ending := .halt } },
  { doomednum := 17, mt := "MT_MISC21", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"CELP", 'A', -1, "", false⟩   -- S_CELP
        ],
        ending := .halt } },
  { doomednum := 8, mt := "MT_MISC24", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"BPAK", 'A', -1, "", false⟩   -- S_BPAK
        ],
        ending := .halt } },
  { doomednum := 2011, mt := "MT_MISC10", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"STIM", 'A', -1, "", false⟩   -- S_STIM
        ],
        ending := .halt } },
  { doomednum := 2012, mt := "MT_MISC11", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"MEDI", 'A', -1, "", false⟩   -- S_MEDI
        ],
        ending := .halt } },
  { doomednum := 2014, mt := "MT_MISC2", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_COUNTITEM"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"BON1", 'A', 6, "", false⟩,   -- S_BON1
          ⟨"BON1", 'B', 6, "", false⟩,   -- S_BON1A
          ⟨"BON1", 'C', 6, "", false⟩,   -- S_BON1B
          ⟨"BON1", 'D', 6, "", false⟩,   -- S_BON1C
          ⟨"BON1", 'C', 6, "", false⟩,   -- S_BON1D
          ⟨"BON1", 'B', 6, "", false⟩   -- S_BON1E
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 2015, mt := "MT_MISC3", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_COUNTITEM"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"BON2", 'A', 6, "", false⟩,   -- S_BON2
          ⟨"BON2", 'B', 6, "", false⟩,   -- S_BON2A
          ⟨"BON2", 'C', 6, "", false⟩,   -- S_BON2B
          ⟨"BON2", 'D', 6, "", false⟩,   -- S_BON2C
          ⟨"BON2", 'C', 6, "", false⟩,   -- S_BON2D
          ⟨"BON2", 'B', 6, "", false⟩   -- S_BON2E
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 2018, mt := "MT_MISC0", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"ARM1", 'A', 6, "", false⟩,   -- S_ARM1
          ⟨"ARM1", 'B', 7, "", true⟩   -- S_ARM1A
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 2019, mt := "MT_MISC1", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"ARM2", 'A', 6, "", false⟩,   -- S_ARM2
          ⟨"ARM2", 'B', 6, "", true⟩   -- S_ARM2A
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 2013, mt := "MT_MISC12", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_COUNTITEM"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SOUL", 'A', 6, "", true⟩,   -- S_SOUL
          ⟨"SOUL", 'B', 6, "", true⟩,   -- S_SOUL2
          ⟨"SOUL", 'C', 6, "", true⟩,   -- S_SOUL3
          ⟨"SOUL", 'D', 6, "", true⟩,   -- S_SOUL4
          ⟨"SOUL", 'C', 6, "", true⟩,   -- S_SOUL5
          ⟨"SOUL", 'B', 6, "", true⟩   -- S_SOUL6
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 2022, mt := "MT_INV", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_COUNTITEM"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"PINV", 'A', 6, "", true⟩,   -- S_PINV
          ⟨"PINV", 'B', 6, "", true⟩,   -- S_PINV2
          ⟨"PINV", 'C', 6, "", true⟩,   -- S_PINV3
          ⟨"PINV", 'D', 6, "", true⟩   -- S_PINV4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 2023, mt := "MT_MISC13", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_COUNTITEM"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"PSTR", 'A', -1, "", true⟩   -- S_PSTR
        ],
        ending := .halt } },
  { doomednum := 2024, mt := "MT_INS", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_COUNTITEM"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"PINS", 'A', 6, "", true⟩,   -- S_PINS
          ⟨"PINS", 'B', 6, "", true⟩,   -- S_PINS2
          ⟨"PINS", 'C', 6, "", true⟩,   -- S_PINS3
          ⟨"PINS", 'D', 6, "", true⟩   -- S_PINS4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 2025, mt := "MT_MISC14", radius := 20, height := 16,
    flags := #["MF_SPECIAL"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SUIT", 'A', -1, "", true⟩   -- S_SUIT
        ],
        ending := .halt } },
  { doomednum := 2026, mt := "MT_MISC15", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_COUNTITEM"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"PMAP", 'A', 6, "", true⟩,   -- S_PMAP
          ⟨"PMAP", 'B', 6, "", true⟩,   -- S_PMAP2
          ⟨"PMAP", 'C', 6, "", true⟩,   -- S_PMAP3
          ⟨"PMAP", 'D', 6, "", true⟩,   -- S_PMAP4
          ⟨"PMAP", 'C', 6, "", true⟩,   -- S_PMAP5
          ⟨"PMAP", 'B', 6, "", true⟩   -- S_PMAP6
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 2045, mt := "MT_MISC16", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_COUNTITEM"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"PVIS", 'A', 6, "", true⟩,   -- S_PVIS
          ⟨"PVIS", 'B', 6, "", false⟩   -- S_PVIS2
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 83, mt := "MT_MEGA", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_COUNTITEM"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"MEGA", 'A', 6, "", true⟩,   -- S_MEGA
          ⟨"MEGA", 'B', 6, "", true⟩,   -- S_MEGA2
          ⟨"MEGA", 'C', 6, "", true⟩,   -- S_MEGA3
          ⟨"MEGA", 'D', 6, "", true⟩   -- S_MEGA4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 5, mt := "MT_MISC4", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_NOTDMATCH"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"BKEY", 'A', 10, "", false⟩,   -- S_BKEY
          ⟨"BKEY", 'B', 10, "", true⟩   -- S_BKEY2
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 6, mt := "MT_MISC6", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_NOTDMATCH"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"YKEY", 'A', 10, "", false⟩,   -- S_YKEY
          ⟨"YKEY", 'B', 10, "", true⟩   -- S_YKEY2
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 13, mt := "MT_MISC5", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_NOTDMATCH"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"RKEY", 'A', 10, "", false⟩,   -- S_RKEY
          ⟨"RKEY", 'B', 10, "", true⟩   -- S_RKEY2
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 40, mt := "MT_MISC9", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_NOTDMATCH"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"BSKU", 'A', 10, "", false⟩,   -- S_BSKULL
          ⟨"BSKU", 'B', 10, "", true⟩   -- S_BSKULL2
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 39, mt := "MT_MISC7", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_NOTDMATCH"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"YSKU", 'A', 10, "", false⟩,   -- S_YSKULL
          ⟨"YSKU", 'B', 10, "", true⟩   -- S_YSKULL2
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 38, mt := "MT_MISC8", radius := 20, height := 16,
    flags := #["MF_SPECIAL", "MF_NOTDMATCH"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"RSKU", 'A', 10, "", false⟩,   -- S_RSKULL
          ⟨"RSKU", 'B', 10, "", true⟩   -- S_RSKULL2
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 18, mt := "MT_MISC63", radius := 20, height := 16,
    flags := #[],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"POSS", 'L', -1, "", false⟩   -- S_POSS_DIE5
        ],
        ending := .halt } },
  { doomednum := 19, mt := "MT_MISC67", radius := 20, height := 16,
    flags := #[],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SPOS", 'L', -1, "", false⟩   -- S_SPOS_DIE5
        ],
        ending := .halt } },
  { doomednum := 20, mt := "MT_MISC66", radius := 20, height := 16,
    flags := #[],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"TROO", 'M', -1, "", false⟩   -- S_TROO_DIE5
        ],
        ending := .halt } },
  { doomednum := 21, mt := "MT_MISC64", radius := 20, height := 16,
    flags := #[],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SARG", 'N', -1, "", false⟩   -- S_SARG_DIE6
        ],
        ending := .halt } },
  { doomednum := 22, mt := "MT_MISC61", radius := 20, height := 16,
    flags := #[],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"HEAD", 'L', -1, "", false⟩   -- S_HEAD_DIE6
        ],
        ending := .halt } },
  { doomednum := 23, mt := "MT_MISC65", radius := 20, height := 16,
    flags := #[],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SKUL", 'K', 6, "", false⟩   -- S_SKULL_DIE6
        ],
        ending := .remove } },
  { doomednum := 79, mt := "MT_MISC84", radius := 20, height := 16,
    flags := #["MF_NOBLOCKMAP"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"POB1", 'A', -1, "", false⟩   -- S_COLONGIBS
        ],
        ending := .halt } },
  { doomednum := 80, mt := "MT_MISC85", radius := 20, height := 16,
    flags := #["MF_NOBLOCKMAP"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"POB2", 'A', -1, "", false⟩   -- S_SMALLPOOL
        ],
        ending := .halt } },
  { doomednum := 81, mt := "MT_MISC86", radius := 20, height := 16,
    flags := #["MF_NOBLOCKMAP"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"BRS1", 'A', -1, "", false⟩   -- S_BRAINSTEM
        ],
        ending := .halt } },
  { doomednum := 85, mt := "MT_MISC29", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"TLMP", 'A', 4, "", true⟩,   -- S_TECHLAMP
          ⟨"TLMP", 'B', 4, "", true⟩,   -- S_TECHLAMP2
          ⟨"TLMP", 'C', 4, "", true⟩,   -- S_TECHLAMP3
          ⟨"TLMP", 'D', 4, "", true⟩   -- S_TECHLAMP4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 86, mt := "MT_MISC30", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"TLP2", 'A', 4, "", true⟩,   -- S_TECH2LAMP
          ⟨"TLP2", 'B', 4, "", true⟩,   -- S_TECH2LAMP2
          ⟨"TLP2", 'C', 4, "", true⟩,   -- S_TECH2LAMP3
          ⟨"TLP2", 'D', 4, "", true⟩   -- S_TECH2LAMP4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 73, mt := "MT_MISC78", radius := 16, height := 88,
    flags := #["MF_SOLID", "MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"HDB1", 'A', -1, "", false⟩   -- S_HANGNOGUTS
        ],
        ending := .halt } },
  { doomednum := 74, mt := "MT_MISC79", radius := 16, height := 88,
    flags := #["MF_SOLID", "MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"HDB2", 'A', -1, "", false⟩   -- S_HANGBNOBRAIN
        ],
        ending := .halt } },
  { doomednum := 75, mt := "MT_MISC80", radius := 16, height := 64,
    flags := #["MF_SOLID", "MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"HDB3", 'A', -1, "", false⟩   -- S_HANGTLOOKDN
        ],
        ending := .halt } },
  { doomednum := 76, mt := "MT_MISC81", radius := 16, height := 64,
    flags := #["MF_SOLID", "MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"HDB4", 'A', -1, "", false⟩   -- S_HANGTSKULL
        ],
        ending := .halt } },
  { doomednum := 77, mt := "MT_MISC82", radius := 16, height := 64,
    flags := #["MF_SOLID", "MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"HDB5", 'A', -1, "", false⟩   -- S_HANGTLOOKUP
        ],
        ending := .halt } },
  { doomednum := 78, mt := "MT_MISC83", radius := 16, height := 64,
    flags := #["MF_SOLID", "MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"HDB6", 'A', -1, "", false⟩   -- S_HANGTNOBRAIN
        ],
        ending := .halt } },
  { doomednum := 10, mt := "MT_MISC68", radius := 20, height := 16,
    flags := #[],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"PLAY", 'W', -1, "", false⟩   -- S_PLAY_XDIE9
        ],
        ending := .halt } },
  { doomednum := 12, mt := "MT_MISC69", radius := 20, height := 16,
    flags := #[],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"PLAY", 'W', -1, "", false⟩   -- S_PLAY_XDIE9
        ],
        ending := .halt } },
  { doomednum := 15, mt := "MT_MISC62", radius := 20, height := 16,
    flags := #[],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"PLAY", 'N', -1, "", false⟩   -- S_PLAY_DIE7
        ],
        ending := .halt } },
  { doomednum := 24, mt := "MT_MISC71", radius := 20, height := 16,
    flags := #[],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"POL5", 'A', -1, "", false⟩   -- S_GIBS
        ],
        ending := .halt } },
  { doomednum := 34, mt := "MT_MISC49", radius := 20, height := 16,
    flags := #[],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"CAND", 'A', -1, "", true⟩   -- S_CANDLESTIK
        ],
        ending := .halt } },
  { doomednum := 35, mt := "MT_MISC50", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"CBRA", 'A', -1, "", true⟩   -- S_CANDELABRA
        ],
        ending := .halt } },
  { doomednum := 2028, mt := "MT_MISC31", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"COLU", 'A', -1, "", true⟩   -- S_COLU
        ],
        ending := .halt } },
  { doomednum := 30, mt := "MT_MISC32", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"COL1", 'A', -1, "", false⟩   -- S_TALLGRNCOL
        ],
        ending := .halt } },
  { doomednum := 31, mt := "MT_MISC33", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"COL2", 'A', -1, "", false⟩   -- S_SHRTGRNCOL
        ],
        ending := .halt } },
  { doomednum := 32, mt := "MT_MISC34", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"COL3", 'A', -1, "", false⟩   -- S_TALLREDCOL
        ],
        ending := .halt } },
  { doomednum := 33, mt := "MT_MISC35", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"COL4", 'A', -1, "", false⟩   -- S_SHRTREDCOL
        ],
        ending := .halt } },
  { doomednum := 36, mt := "MT_MISC37", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"COL5", 'A', 14, "", false⟩,   -- S_HEARTCOL
          ⟨"COL5", 'B', 14, "", false⟩   -- S_HEARTCOL2
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 37, mt := "MT_MISC36", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"COL6", 'A', -1, "", false⟩   -- S_SKULLCOL
        ],
        ending := .halt } },
  { doomednum := 41, mt := "MT_MISC38", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"CEYE", 'A', 6, "", true⟩,   -- S_EVILEYE
          ⟨"CEYE", 'B', 6, "", true⟩,   -- S_EVILEYE2
          ⟨"CEYE", 'C', 6, "", true⟩,   -- S_EVILEYE3
          ⟨"CEYE", 'B', 6, "", true⟩   -- S_EVILEYE4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 42, mt := "MT_MISC39", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"FSKU", 'A', 6, "", true⟩,   -- S_FLOATSKULL
          ⟨"FSKU", 'B', 6, "", true⟩,   -- S_FLOATSKULL2
          ⟨"FSKU", 'C', 6, "", true⟩   -- S_FLOATSKULL3
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 43, mt := "MT_MISC40", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"TRE1", 'A', -1, "", false⟩   -- S_TORCHTREE
        ],
        ending := .halt } },
  { doomednum := 44, mt := "MT_MISC41", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"TBLU", 'A', 4, "", true⟩,   -- S_BLUETORCH
          ⟨"TBLU", 'B', 4, "", true⟩,   -- S_BLUETORCH2
          ⟨"TBLU", 'C', 4, "", true⟩,   -- S_BLUETORCH3
          ⟨"TBLU", 'D', 4, "", true⟩   -- S_BLUETORCH4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 45, mt := "MT_MISC42", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"TGRN", 'A', 4, "", true⟩,   -- S_GREENTORCH
          ⟨"TGRN", 'B', 4, "", true⟩,   -- S_GREENTORCH2
          ⟨"TGRN", 'C', 4, "", true⟩,   -- S_GREENTORCH3
          ⟨"TGRN", 'D', 4, "", true⟩   -- S_GREENTORCH4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 46, mt := "MT_MISC43", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"TRED", 'A', 4, "", true⟩,   -- S_REDTORCH
          ⟨"TRED", 'B', 4, "", true⟩,   -- S_REDTORCH2
          ⟨"TRED", 'C', 4, "", true⟩,   -- S_REDTORCH3
          ⟨"TRED", 'D', 4, "", true⟩   -- S_REDTORCH4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 47, mt := "MT_MISC47", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SMIT", 'A', -1, "", false⟩   -- S_STALAGTITE
        ],
        ending := .halt } },
  { doomednum := 48, mt := "MT_MISC48", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"ELEC", 'A', -1, "", false⟩   -- S_TECHPILLAR
        ],
        ending := .halt } },
  { doomednum := 54, mt := "MT_MISC76", radius := 32, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"TRE2", 'A', -1, "", false⟩   -- S_BIGTREE
        ],
        ending := .halt } },
  { doomednum := 55, mt := "MT_MISC44", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SMBT", 'A', 4, "", true⟩,   -- S_BTORCHSHRT
          ⟨"SMBT", 'B', 4, "", true⟩,   -- S_BTORCHSHRT2
          ⟨"SMBT", 'C', 4, "", true⟩,   -- S_BTORCHSHRT3
          ⟨"SMBT", 'D', 4, "", true⟩   -- S_BTORCHSHRT4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 56, mt := "MT_MISC45", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SMGT", 'A', 4, "", true⟩,   -- S_GTORCHSHRT
          ⟨"SMGT", 'B', 4, "", true⟩,   -- S_GTORCHSHRT2
          ⟨"SMGT", 'C', 4, "", true⟩,   -- S_GTORCHSHRT3
          ⟨"SMGT", 'D', 4, "", true⟩   -- S_GTORCHSHRT4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 57, mt := "MT_MISC46", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"SMRT", 'A', 4, "", true⟩,   -- S_RTORCHSHRT
          ⟨"SMRT", 'B', 4, "", true⟩,   -- S_RTORCHSHRT2
          ⟨"SMRT", 'C', 4, "", true⟩,   -- S_RTORCHSHRT3
          ⟨"SMRT", 'D', 4, "", true⟩   -- S_RTORCHSHRT4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 70, mt := "MT_MISC77", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"FCAN", 'A', 4, "", true⟩,   -- S_BBAR1
          ⟨"FCAN", 'B', 4, "", true⟩,   -- S_BBAR2
          ⟨"FCAN", 'C', 4, "", true⟩   -- S_BBAR3
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 25, mt := "MT_MISC74", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"POL1", 'A', -1, "", false⟩   -- S_DEADSTICK
        ],
        ending := .halt } },
  { doomednum := 26, mt := "MT_MISC75", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"POL6", 'A', 6, "", false⟩,   -- S_LIVESTICK
          ⟨"POL6", 'B', 8, "", false⟩   -- S_LIVESTICK2
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 27, mt := "MT_MISC72", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"POL4", 'A', -1, "", false⟩   -- S_HEADONASTICK
        ],
        ending := .halt } },
  { doomednum := 28, mt := "MT_MISC70", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"POL2", 'A', -1, "", false⟩   -- S_HEADSONSTICK
        ],
        ending := .halt } },
  { doomednum := 29, mt := "MT_MISC73", radius := 16, height := 16,
    flags := #["MF_SOLID"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"POL3", 'A', 6, "", true⟩,   -- S_HEADCANDLES
          ⟨"POL3", 'B', 6, "", true⟩   -- S_HEADCANDLES2
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 49, mt := "MT_MISC51", radius := 16, height := 68,
    flags := #["MF_SOLID", "MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"GOR1", 'A', 10, "", false⟩,   -- S_BLOODYTWITCH
          ⟨"GOR1", 'B', 15, "", false⟩,   -- S_BLOODYTWITCH2
          ⟨"GOR1", 'C', 8, "", false⟩,   -- S_BLOODYTWITCH3
          ⟨"GOR1", 'B', 6, "", false⟩   -- S_BLOODYTWITCH4
        ],
        ending := .loops "spawn" 0 } },
  { doomednum := 50, mt := "MT_MISC52", radius := 16, height := 84,
    flags := #["MF_SOLID", "MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"GOR2", 'A', -1, "", false⟩   -- S_MEAT2
        ],
        ending := .halt } },
  { doomednum := 51, mt := "MT_MISC53", radius := 16, height := 84,
    flags := #["MF_SOLID", "MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"GOR3", 'A', -1, "", false⟩   -- S_MEAT3
        ],
        ending := .halt } },
  { doomednum := 52, mt := "MT_MISC54", radius := 16, height := 68,
    flags := #["MF_SOLID", "MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"GOR4", 'A', -1, "", false⟩   -- S_MEAT4
        ],
        ending := .halt } },
  { doomednum := 53, mt := "MT_MISC55", radius := 16, height := 52,
    flags := #["MF_SOLID", "MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"GOR5", 'A', -1, "", false⟩   -- S_MEAT5
        ],
        ending := .halt } },
  { doomednum := 59, mt := "MT_MISC56", radius := 20, height := 84,
    flags := #["MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"GOR2", 'A', -1, "", false⟩   -- S_MEAT2
        ],
        ending := .halt } },
  { doomednum := 60, mt := "MT_MISC57", radius := 20, height := 68,
    flags := #["MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"GOR4", 'A', -1, "", false⟩   -- S_MEAT4
        ],
        ending := .halt } },
  { doomednum := 61, mt := "MT_MISC58", radius := 20, height := 52,
    flags := #["MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"GOR3", 'A', -1, "", false⟩   -- S_MEAT3
        ],
        ending := .halt } },
  { doomednum := 62, mt := "MT_MISC59", radius := 20, height := 52,
    flags := #["MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"GOR5", 'A', -1, "", false⟩   -- S_MEAT5
        ],
        ending := .halt } },
  { doomednum := 63, mt := "MT_MISC60", radius := 20, height := 68,
    flags := #["MF_SPAWNCEILING", "MF_NOGRAVITY"],
    chain :=
      { entry := "spawn",
        states := #[
          ⟨"GOR1", 'A', 10, "", false⟩,   -- S_BLOODYTWITCH
          ⟨"GOR1", 'B', 15, "", false⟩,   -- S_BLOODYTWITCH2
          ⟨"GOR1", 'C', 8, "", false⟩,   -- S_BLOODYTWITCH3
          ⟨"GOR1", 'B', 6, "", false⟩   -- S_BLOODYTWITCH4
        ],
        ending := .loops "spawn" 0 } }
]

/-! ## animdefs[] — p_spec.c

All vanilla entries, in table order: (istexture, startname, endname).
The table has 22 data rows; its 23rd row is the `{-1}` terminator.
Vanilla stores `{istexture, ENDNAME, STARTNAME, speed}`; this table is
(istexture, start, end) with the 8-tic speed shared by every entry.
The animation runs from `startname` to `endname` in WAD directory order. -/

def animdefs : Array (Bool × String × String) := #[
  (false, "NUKAGE1", "NUKAGE3"),
  (false, "FWATER1", "FWATER4"),
  (false, "SWATER1", "SWATER4"),
  (false, "LAVA1", "LAVA4"),
  (false, "BLOOD1", "BLOOD3"),
  (false, "RROCK05", "RROCK08"),
  (false, "SLIME01", "SLIME04"),
  (false, "SLIME05", "SLIME08"),
  (false, "SLIME09", "SLIME12"),
  (true, "BLODGR1", "BLODGR4"),
  (true, "SLADRIP1", "SLADRIP3"),
  (true, "BLODRIP1", "BLODRIP4"),
  (true, "FIREWALA", "FIREWALL"),
  (true, "GSTFONT1", "GSTFONT3"),
  (true, "FIRELAV3", "FIRELAVA"),
  (true, "FIREMAG1", "FIREMAG3"),
  (true, "FIREBLU1", "FIREBLU2"),
  (true, "ROCKRED1", "ROCKRED3"),
  (true, "BFALL1", "BFALL4"),
  (true, "SFALL1", "SFALL4"),
  (true, "WFALL1", "WFALL4"),
  (true, "DBRAIN1", "DBRAIN4")
]

/-! ## alphSwitchList[] — p_switch.c

(SW1 name, SW2 name, episode gate): 1 = shareware, 2 = registered,
3 = commercial (Doom II). `P_InitSwitchList` admits entries with
`episode <= episode` where episode is 1/2/3 by gamemode. -/

def alphSwitchList : Array (String × String × Nat) := #[
  ("SW1BRCOM", "SW2BRCOM", 1),
  ("SW1BRN1", "SW2BRN1", 1),
  ("SW1BRN2", "SW2BRN2", 1),
  ("SW1BRNGN", "SW2BRNGN", 1),
  ("SW1BROWN", "SW2BROWN", 1),
  ("SW1COMM", "SW2COMM", 1),
  ("SW1COMP", "SW2COMP", 1),
  ("SW1DIRT", "SW2DIRT", 1),
  ("SW1EXIT", "SW2EXIT", 1),
  ("SW1GRAY", "SW2GRAY", 1),
  ("SW1GRAY1", "SW2GRAY1", 1),
  ("SW1METAL", "SW2METAL", 1),
  ("SW1PIPE", "SW2PIPE", 1),
  ("SW1SLAD", "SW2SLAD", 1),
  ("SW1STARG", "SW2STARG", 1),
  ("SW1STON1", "SW2STON1", 1),
  ("SW1STON2", "SW2STON2", 1),
  ("SW1STONE", "SW2STONE", 1),
  ("SW1STRTN", "SW2STRTN", 1),
  ("SW1BLUE", "SW2BLUE", 2),
  ("SW1CMT", "SW2CMT", 2),
  ("SW1GARG", "SW2GARG", 2),
  ("SW1GSTON", "SW2GSTON", 2),
  ("SW1HOT", "SW2HOT", 2),
  ("SW1LION", "SW2LION", 2),
  ("SW1SATYR", "SW2SATYR", 2),
  ("SW1SKIN", "SW2SKIN", 2),
  ("SW1VINE", "SW2VINE", 2),
  ("SW1WOOD", "SW2WOOD", 2),
  ("SW1PANEL", "SW2PANEL", 3),
  ("SW1ROCK", "SW2ROCK", 3),
  ("SW1MET2", "SW2MET2", 3),
  ("SW1WDMET", "SW2WDMET", 3),
  ("SW1BRIK", "SW2BRIK", 3),
  ("SW1MOD1", "SW2MOD1", 3),
  ("SW1ZIM", "SW2ZIM", 3),
  ("SW1STON6", "SW2STON6", 3),
  ("SW1TEK", "SW2TEK", 3),
  ("SW1MARB", "SW2MARB", 3),
  ("SW1SKULL", "SW2SKULL", 3)
]

/-! ## Movement and mover constants

Sources: p_spec.h, p_local.h, p_pspr.c, p_plats.c, p_doors.c, p_floor.c.
Conversion rule: vanilla fixed-point (`FRACUNIT = 1<<16`) divided by
65536 → map units; per-tic rates stay per-tic. Verbatim quotes:

  p_spec.h:   #define PLATWAIT   3            (seconds; used as 35*PLATWAIT tics)
  p_spec.h:   #define PLATSPEED  FRACUNIT
  p_spec.h:   #define VDOORSPEED FRACUNIT*2
  p_spec.h:   #define VDOORWAIT  150
  p_spec.h:   #define CEILSPEED  FRACUNIT
  p_spec.h:   #define FLOORSPEED FRACUNIT
  p_local.h:  #define FLOATSPEED (FRACUNIT*4)
  p_local.h:  #define GRAVITY    FRACUNIT
  p_local.h:  #define MAXMOVE    (30*FRACUNIT)
  p_local.h:  #define MELEERANGE (64*FRACUNIT)
  p_local.h:  #define MISSILERANGE (32*64*FRACUNIT)
  p_pspr.c:   #define LOWERSPEED FRACUNIT*6
  p_pspr.c:   #define RAISESPEED FRACUNIT*6
  p_pspr.c:   #define WEAPONBOTTOM 128*FRACUNIT
  p_pspr.c:   #define WEAPONTOP    32*FRACUNIT
  p_plats.c (EV_DoPlat): raiseAndChange/raiseToNearestAndChange PLATSPEED/2;
    downWaitUpStay PLATSPEED*4, wait 35*PLATWAIT; blazeDWUS PLATSPEED*8,
    wait 35*PLATWAIT; perpetualRaise PLATSPEED, wait 35*PLATWAIT
  p_doors.c (EV_DoDoor): blazeRaise/blazeOpen/blazeClose door->speed = VDOORSPEED*4
  p_floor.c (EV_BuildStairs): build8 speed = FLOORSPEED/4;
    turbo16 speed = FLOORSPEED*4
  p_floor.c (EV_DoFloor, raiseFloorTurbo): floor->speed = FLOORSPEED*4
-/

namespace Consts
/-- `VDOORSPEED` = FRACUNIT*2 → 2 map units/tic. -/
def vdoorSpeed : Float := 2
/-- `VDOORWAIT` = 150 tics. -/
def vdoorWait : Nat := 150
/-- Blazing doors: `VDOORSPEED*4` → 8 units/tic (p_doors.c). -/
def vdoorBlazeSpeed : Float := 8
/-- `PLATSPEED` = FRACUNIT → 1 unit/tic (perpetualRaise lifts). -/
def platSpeed : Float := 1
/-- `PLATSPEED*4` → 4 units/tic: downWaitUpStay, the common lift. -/
def platSpeedDWUS : Float := 4
/-- `PLATSPEED*8` → 8 units/tic: blazeDWUS (specials 120–123). -/
def platSpeedBlaze : Float := 8
/-- `PLATSPEED/2` → 0.5 units/tic: the raiseAndChange plat family. -/
def platSpeedRaiseChange : Float := 0.5
/-- `35*PLATWAIT` = 105 tics (PLATWAIT is 3 seconds). -/
def platWaitTics : Nat := 105
/-- `FLOORSPEED` = FRACUNIT → 1 unit/tic. -/
def floorSpeed : Float := 1
/-- `FLOORSPEED*4` → 4 units/tic (raiseFloorTurbo, and turbo16 stairs). -/
def floorSpeedTurbo : Float := 4
/-- `CEILSPEED` = FRACUNIT → 1 unit/tic. -/
def ceilSpeed : Float := 1
/-- Stairs, specials 7/8 (`build8`): `FLOORSPEED/4` → 0.25 units/tic. -/
def stairSpeedBuild8 : Float := 0.25
/-- Stairs, specials 100/127 (`turbo16`): `FLOORSPEED*4` → 4 units/tic. -/
def stairSpeedTurbo16 : Float := 4
/-- `MELEERANGE` = 64*FRACUNIT → 64 map units. `P_CheckMeleeRange` tests
`dist >= MELEERANGE - 20*FRACUNIT + target radius`; `A_Saw` reaches
`MELEERANGE + 1` (one *fixed-point epsilon* past 64 in vanilla — the +1 is
1/65536 unit, not one map unit). -/
def meleeRange : Float := 64
/-- `MISSILERANGE` = 32*64*FRACUNIT → 2048 map units. -/
def missileRange : Float := 2048
/-- `FLOATSPEED` = FRACUNIT*4 → 4 units/tic (floaters homing vertically). -/
def floatSpeed : Float := 4
/-- `GRAVITY` = FRACUNIT → 1 unit/tic². -/
def gravity : Float := 1
/-- `MAXMOVE` = 30*FRACUNIT → 30 units/tic. -/
def maxMove : Float := 30
/-- `LOWERSPEED`/`RAISESPEED` = FRACUNIT*6 → 6 psprite units/tic;
weapon sits at `WEAPONTOP` 32, hides at `WEAPONBOTTOM` 128. -/
def weaponLowerRaiseSpeed : Float := 6
def weaponTop : Float := 32
def weaponBottom : Float := 128
/-- `A_SkullAttack`'s `SKULLSPEED` (p_enemy.c: 20*FRACUNIT) → 20 units/tic —
the lost soul's *charge* speed; its mobjinfo speed is 8. -/
def skullSpeed : Float := 20
end Consts

/-! ## Model-mismatch notes

Vanilla content in this file that DILL's model cannot represent directly,
so a diff should treat these knowingly rather than as transcription noise. -/

/-- Vanilla features with no DILL-side slot. -/
def unmodeled : Array String := #[
  "raise chains: vanilla gives raisestates to MT_POSSESSED, MT_SHOTGUY, " ++
    "MT_TROOP, MT_SERGEANT/MT_SHADOWS, MT_CHAINGUY, MT_WOLFSS, MT_FATSO, " ++
    "MT_KNIGHT, MT_BRUISER, MT_UNDEAD, MT_BABY, MT_HEAD, MT_PAIN " ++
    "(A_VileChase resurrection targets); DILL ActorInfo has no raise " ++
    "entry (only the arch-vile's healState). Recorded here as the " ++
    "'raise' chains anyway.",
  "actors DILL does not define at all: MT_PLAYER (DILL's player is not a " ++
    "mobj), MT_SMOKE (the puffs A_Tracer drops behind the revenant " ++
    "missile), MT_SPAWNFIRE (the teleport flame A_SpawnFly leaves when a " ++
    "spawn cube delivers), MT_IFOG (deathmatch item-respawn fog), " ++
    "MT_TELEPORTMAN (the teleport-destination marker; DILL reads the " ++
    "thing directly). None appear in `mobjs`.",
  "actions DILL folds together: vanilla's vile-fire states run " ++
    "A_StartFire and A_FireCrackle (sound cues) on specific frames where " ++
    "DILL runs plain `fire` on every frame; recorded verbatim in the " ++
    "chains. A_PlayerScream, A_XScream on MT_PLAYER only. A_Tracer is a " ++
    "state action in vanilla (on the S_TRACER loop) but a simulation-side " ++
    "step in DILL (`traceMissile`); the revenantMissile chain here " ++
    "records A_Tracer on the states that carry it.",
  "A_BrainPain: vanilla's iconBrain pain state runs A_BrainPain (sound); " ++
    "DILL uses its generic pain action. Recorded verbatim.",
  "sfxinfo_t priority/singularity/link fields (sounds.c) are not " ++
    "modeled by DILL's flat Sfx list and are not recorded here.",
  "state misc1/misc2: zero for every vanilla state; dropped.",
  "SWATER1..SWATER4: vanilla animdefs entry 3 names flats absent from " ++
    "both retail IWADs (P_InitPicAnims skips missing entries); DILL's " ++
    "animGroups omits the group entirely. It IS recorded in `animdefs`.",
  "alphSwitchList: DILL derives SW1<->SW2 pairs by name prefix " ++
    "(Level.switchTwin) instead of a table; the vanilla table (with its " ++
    "episode gates, which DILL does not model) is recorded above.",
  "weapon ammo-per-shot lives in code in vanilla (BFGCELLS 40 in " ++
    "p_pspr.c, 2 shells in A_FireShotgun2), not in weaponinfo; DILL " ++
    "models it as Weapon.ammoCost. Not part of `weaponinfo` here.",
  "MT_SKULL flies at SKULLSPEED (20*FRACUNIT) only while charging " ++
    "(A_SkullAttack); its mobjinfo speed 8 governs A_Chase drifting. " ++
    "See Consts.skullSpeed."]

end VanillaData
