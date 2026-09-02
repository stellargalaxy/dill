import Dill.Maps
import Dill.Game.State

/-!
# Cheat codes

The classics, typed blind during play: `dilldqd` (god mode), `dillkfa`
(everything), `dillfa` (everything but keys), `dillclip` / `dillspispopd`
(walk through walls), `dillmypos`, `dillddt` (reveal the whole automap), and
`dillclev`⟨e⟩⟨m⟩ to warp to a map — the two digits read as episode-and-map
on Doom and as a level number on Doom II, following the map you are
currently on.

`scan` watches the tail of the typed-character buffer; `apply` performs
the state change and returns the message to flash on screen. Warping
needs the WAD, so `.warp` is carried back to the shell to execute.
-/

namespace Dill

inductive Cheat where
  | god | kfa | fa | noclip | mypos
  /-- `dillddt`: reveal the whole automap. Deliberately not part of
  `dillkfa` — vanilla's arsenal cheat never granted the map either, and this
  is the separate one that does. -/
  | fullMap
  /-- The two typed digits; what map they name depends on the WAD's
  naming scheme, so they are resolved against the current level. -/
  | warp (a b : Nat)
  deriving Repr, DecidableEq, Inhabited

namespace Cheat

/-- Does the typed buffer end in a cheat? -/
def scan (buf : String) : Option Cheat := Id.run do
  if buf.endsWith "dilldqd" then return some .god
  if buf.endsWith "dillkfa" then return some .kfa
  if buf.endsWith "dillfa" then return some .fa
  if buf.endsWith "dillclip" || buf.endsWith "dillspispopd" then
    return some .noclip
  if buf.endsWith "dillmypos" then return some .mypos
  if buf.endsWith "dillddt" then return some .fullMap
  let chars := buf.toList
  if chars.length ≥ 10 then
    let tail := chars.drop (chars.length - 10)
    match tail with
    | ['d', 'i', 'l', 'l', 'c', 'l', 'e', 'v', e, m] =>
      if e.isDigit && m.isDigit then
        return some (.warp (e.toNat - '0'.toNat) (m.toNat - '0'.toNat))
    | _ => pure ()
  return none

/-- All weapons, full ammo (backpack-aware, matching `ammoMax`), and full
armor — everything `dillkfa`/`dillfa` hand out short of the keys.

`hasSuperShotgun` says whether the loaded WAD actually carries one (see
`Assets.hasSuperShotgun`). On Doom 1 it does not, and this is the only route
by which a player there could come to own one: granting it anyway left `3`
toggling onto a gun with no sprites and no firing sound — invisible hands
that still killed things. -/
def fullArsenal (st : PlayerStatus) (hasSuperShotgun : Bool) : PlayerStatus :=
  { st with
    -- 200 points of blue, the jacket vanilla's arsenal cheat hands out
    armor := 200
    armorType := 2
    bullets := if st.backpack then 400 else 200
    shells  := if st.backpack then 100 else 50
    rockets := if st.backpack then 100 else 50
    cells   := if st.backpack then 600 else 300
    ownsShotgun := true
    ownsSuperShotgun := st.ownsSuperShotgun || hasSuperShotgun
    ownsChaingun := true, ownsChainsaw := true
    ownsRocket := true, ownsPlasma := true, ownsBfg := true }

/-- Perform a cheat; returns the new state and the flash message.
`.warp` only announces itself — the shell loads the map. -/
def apply (g : GameState) (hasSuperShotgun : Bool) : Cheat → GameState × String
  | .god =>
    let st := g.status
    let on := !st.god
    let health := if on then max st.health 100 else st.health
    let st := { st with god := on, health := health }
    ({ g with status := st }
    , if on then "DEGREELESSNESS MODE ON" else "DEGREELESSNESS MODE OFF")
  | .kfa =>
    let st := fullArsenal g.status hasSuperShotgun
    ({ g with status := { st with
        blueKey := true, yellowKey := true, redKey := true } }
    , "VERY HAPPY AMMO ADDED")
  | .fa =>
    ({ g with status := fullArsenal g.status hasSuperShotgun }
    , "AMMO ADDED")
  | .noclip =>
    let on := !g.status.noclip
    ({ g with status := { g.status with noclip := on } }
    , if on then "NO CLIPPING MODE ON" else "NO CLIPPING MODE OFF")
  | .mypos =>
    (g, s!"X {ifloor g.player.x} Y {ifloor g.player.y} \
        A {ifloor (g.player.angle * 57.29578)}")
  | .fullMap =>
    let on := !g.status.ownsMap
    ({ g with status := { g.status with ownsMap := on } }
    , if on then "FULL MAP" else "MAP OFF")
  | .warp a b =>
    let target := match MapId.parse g.level.name with
      | some here => (here.warpTarget a b).name
      | none => s!"E{a}M{b}"
    (g, s!"CHANGING LEVEL TO {target}")

end Cheat

def GameState.applyCheat (g : GameState) (c : Cheat)
    (hasSuperShotgun : Bool) : GameState × String :=
  Cheat.apply g hasSuperShotgun c

end Dill
