/-!
# Map identity

Doom names its maps two ways: `ExMy` (Doom, Ultimate Doom, and the episode
PWADs that follow their layout) and `MAPnn` (Doom II, Plutonia, TNT). Four
things are derived from that name — what comes next, which sky hangs over
it, which music plays, and which graphics the tally screen shows — and each
rule differs between the two schemes.

This module is the one place that knows those rules, so widening DILL to a
new WAD family means extending a table here rather than editing the renderer,
the simulation, and the shell in parallel. It is deliberately free of any
dependency on the rest of DILL so both `Render` and `Game` can use it.
-/

namespace Dill

/-- Which naming scheme a map belongs to, with its coordinates decoded. -/
inductive MapId where
  /-- `ExMy`: episode `e`, map `m`, both 1-based. -/
  | episode (e m : Nat)
  /-- `MAPnn`: level `n`, 1-based. -/
  | level (n : Nat)
  deriving Repr, DecidableEq, Inhabited

namespace MapId

private def digit (c : Char) : Option Nat :=
  if c.isDigit then some (c.toNat - '0'.toNat) else none

/-- Decode a map lump name. Unrecognized names give `none`, which every
caller treats the way the old string code did: fall back to a default. -/
def parse (name : String) : Option MapId :=
  match name.toList with
  | ['E', e, 'M', m] => do return .episode (← digit e) (← digit m)
  | ['M', 'A', 'P', a, b] => do return .level ((← digit a) * 10 + (← digit b))
  | _ => none

/-- The lump name this identity refers to. -/
def name : MapId → String
  | .episode e m => s!"E{e}M{m}"
  | .level n => s!"MAP{if n < 10 then "0" else ""}{n}"

/-- Which map an episode's secret level returns you to (vanilla
`G_DoCompleted`, whose `wminfo.next` is 0-based): E1M9 → E1M4, E2M9 → E2M6,
E3M9 → E3M7, E4M9 → E4M3. An episode with no entry keeps the episode-1
target. -/
def secretReturn : Nat → Nat
  | 1 => 4
  | 2 => 6
  | 3 => 7
  | 4 => 3
  | _ => 4

/-- Where a finished map leads, or `none` at the end of the run.

Doom II detours through the Wolfenstein maps: MAP15 → MAP31 → MAP32, with a
normal exit from either landing on MAP16. -/
def next (id : MapId) (secret : Bool) : Option MapId :=
  match id with
  | .episode e m =>
    if secret then some (.episode e 9)
    else if m == 9 then some (.episode e (secretReturn e))
    else if m == 8 then none
    else some (.episode e (m + 1))
  | .level n =>
    let normal : Option MapId :=
      if n == 31 || n == 32 then some (.level 16)
      else if n == 30 then none
      else some (.level (n + 1))
    if secret then
      if n == 15 then some (.level 31)
      else if n == 31 then some (.level 32)
      -- A secret exit anywhere else is undefined in vanilla (only maps 15
      -- and 31 have one to define); nonconforming PWADs that place one
      -- expect a normal exit, so take the normal successor rather than
      -- end the run.
      else normal
    else normal

/-- The sky texture. Episodes take their digit (`SKY1`…); Doom II switches
sky by act, at MAP12 and MAP21. -/
def sky : MapId → String
  | .episode e _ => s!"SKY{e}"
  | .level n => if n < 12 then "SKY1" else if n < 21 then "SKY2" else "SKY3"

/-- Doom II's music, indexed by level number. Doom 1 needs no table: its
lumps are `D_` + the map name. Every name here was checked against the
lump directory of a retail `doom2.wad`. -/
private def doom2Music : Array String :=
  #["D_RUNNIN", "D_STALKS", "D_COUNTD", "D_BETWEE", "D_DOOM", "D_THE_DA",
    "D_SHAWN", "D_DDTBLU", "D_IN_CIT", "D_DEAD", "D_STLKS2", "D_THEDA2",
    "D_DOOM2", "D_DDTBL2", "D_RUNNI2", "D_DEAD2", "D_STLKS3", "D_ROMERO",
    "D_SHAWN2", "D_MESSAG", "D_COUNT2", "D_DDTBL3", "D_AMPIE", "D_THEDA3",
    "D_ADRIAN", "D_MESSG2", "D_ROMER2", "D_TENSE", "D_SHAWN3", "D_OPENIN",
    "D_EVIL", "D_ULTIMA"]

/-- Ultimate Doom's fourth episode ("Thy Flesh Consumed") ships no music of
its own: every map borrows an episode 1–3 track. This is vanilla `S_Start`'s
`spmus` table, indexed by map number. -/
private def episode4Music : Array String :=
  #["D_E3M4", "D_E3M2", "D_E3M3", "D_E1M5", "D_E2M7",
    "D_E2M4", "D_E2M6", "D_E2M5", "D_E1M9"]

/-- The music lump for a map. A level number outside 1–32 has no entry;
callers already treat a missing music lump as silence. -/
def music : MapId → String
  -- the `m == 0` guard keeps Nat's saturating `m - 1` from silently
  -- selecting E4M1's entry for a degenerate E4M0
  | .episode 4 m => if m == 0 then "D_E4M0"
                    else episode4Music[m - 1]?.getD s!"D_E4M{m}"
  | .episode e m => s!"D_E{e}M{m}"
  -- same guard for MAP00: saturating `n - 1` would play MAP01's D_RUNNIN
  | .level 0 => ""
  | .level n => doom2Music[n - 1]?.getD ""

/-- The tally screen's "<name> FINISHED" graphic: `WILV{episode}{map}` for
Doom 1, `CWILV{nn}` for Doom II, both 0-based within their range. -/
def nameGraphic : MapId → String
  -- degenerate 0 coordinates (reachable via the warp cheat) would saturate
  -- `- 1` to 0 and show E1M1's graphic; name a lump that cannot exist instead,
  -- which callers already treat as "no graphic"
  | .episode 0 _ | .episode _ 0 => ""
  | .level 0 => ""
  | .episode e m => s!"WILV{e - 1}{m - 1}"
  | .level n =>
    let i := n - 1
    s!"CWILV{if i < 10 then "0" else ""}{i}"

/-- Resolve the two digits of a warp cheat against the map you are standing
on. `dillclev`'s digits mean episode-and-map under `ExMy` naming and a single
two-digit level number under `MAPnn`, so `dillclev07` reaches E0M7 on Doom
and MAP07 on Doom II — and MAP10 through MAP32 are reachable at all only
because of this. -/
def warpTarget (here : MapId) (a b : Nat) : MapId :=
  match here with
  | .episode .. => .episode a b
  | .level .. => .level (a * 10 + b)

/-- Doom's par times in seconds, straight from `g_game.c`'s `pars` and
`cpars`. Episode 4 has none — the original table only ever had three rows,
and Ultimate Doom read off the end of it — so it reports 0 and the tally
simply leaves the line out. -/
private def episodePars : Array (Array Nat) := #[
  #[0, 30, 75, 120, 90, 165, 180, 180, 30, 165],
  #[0, 90, 90, 90, 120, 90, 360, 240, 30, 170],
  #[0, 90, 45, 90, 150, 90, 90, 165, 30, 135]]

private def levelPars : Array Nat := #[
   30,  90, 120, 120,  90, 150, 120, 120, 270,  90,   --  1–10
  210, 150, 150, 150, 210, 150, 420, 150, 210, 150,   -- 11–20
  240, 150, 180, 150, 150, 300, 330, 420, 300, 180,   -- 21–30
  120,  30]                                           -- 31–32

/-- The par time in seconds, or 0 where the game defines none. -/
def parTime : MapId → Nat
  | .episode 0 _ => 0  -- saturating `e - 1` would read episode 1's row
  | .episode e m => ((episodePars[e - 1]?).bind (·[m]?)).getD 0
  | .level 0 => 0
  | .level n => (levelPars[n - 1]?).getD 0

/-- The backdrop behind the tally screen. Doom 1 has `WIMAP0`–`WIMAP2` for
episodes 1–3; Ultimate Doom shipped no `WIMAP3`, so vanilla `WI_loadData`
special-cases episode 4 (`epsd == 3`) to Doom II's `INTERPIC`. -/
def intermissionBack : MapId → String
  -- degenerate episode 0 (reachable via the warp cheat) would saturate
  -- `e - 1` to 0 and show episode 1's backdrop; name no lump instead,
  -- which the tally screen already treats as "no backdrop"
  | .episode 0 _ => ""
  | .episode 4 _ => "INTERPIC"
  | .episode e _ => s!"WIMAP{min (e - 1) 2}"
  | .level _ => "INTERPIC"

end MapId
end Dill
