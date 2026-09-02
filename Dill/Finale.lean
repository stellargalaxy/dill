import Dill.Maps
import Dill.Game.Sfx

/-!
# The finale

Doom's `f_finale.c`: what the game says to you when an episode ends, and
what it shows you afterwards.

Three things live here, and vanilla runs them as one little state machine
hung off `gamestate == GS_FINALE`:

* a **text screen** — a page of story typed out a character at a time over a
  tiled flat, with the victory music under it. Doom II shows one between
  each pair of acts; Doom shows one at the end of every episode.
* an **end picture** for Doom, which differs per episode: a credits card, a
  victory painting, the scrolling bunny, or `ENDPIC`.
* the **cast call** for Doom II, the roll of every monster in the game
  taking a bow and being shot.

The text is the one part of Doom that lives in the *source* rather than the
WAD — `d_englsh.h`, compiled into the executable — so it is reproduced here
rather than read from a lump. A PWAD cannot override it in vanilla either.
-/

namespace Dill

namespace Finale

/-- Vanilla `TEXTSPEED`: tics per character revealed. -/
def textSpeed : Nat := 3
/-- Vanilla `TEXTWAIT`: tics the finished page holds before it moves on. -/
def textWait : Nat := 250
/-- Vanilla's `finalecount > 50` gate on Doom II — the earliest a keypress
is allowed to skip the page, so the button that ended the level does not
also skip the story it leads to. -/
def skipAfter : Nat := 50

/-! ## The text

Line breaks are vanilla's own. `F_TextWrite` starts a new line on `\n` and
does no wrapping of its own, so where these break is where Doom breaks
them. -/

def e1Text : String :=
  "ONCE YOU BEAT THE BIG BADASSES AND\n\
   CLEAN OUT THE MOON BASE YOU'RE SUPPOSED\n\
   TO WIN, AREN'T YOU? AREN'T YOU? WHERE'S\n\
   YOUR RED CARPET WELCOME?\n\n\
   WHAT? YOU DIDN'T EXPECT SOMEONE TO\n\
   COME OUT OF THE MOON BASE AND SHOOT\n\
   YOU? WELL, HERE'S THE TICKET TO YOUR\n\
   FUTURE...\n\n\
   YOU'RE ON YOUR WAY TO THE SHORES OF HELL."

def e2Text : String :=
  "YOU'VE DONE IT! THE HIDEOUS CYBER-\n\
   DEMON LORD THAT RULED THE LOST DEIMOS\n\
   MOON BASE HAS BEEN slain AND YOU\n\
   ARE TRIUMPHANT! BUT ... WHERE ARE\n\
   YOU? YOU CLAMBER TO THE EDGE OF THE\n\
   MOON AND LOOK DOWN TO SEE THE AWFUL\n\
   TRUTH.\n\n\
   DEIMOS FLOATS ABOVE HELL ITSELF!\n\
   YOU'VE NEVER HEARD OF ANYONE ESCAPING\n\
   FROM HELL, BUT YOU'LL MAKE THE BASTARDS\n\
   SORRY THEY EVER HEARD OF YOU! QUICKLY,\n\
   YOU RAPPEL DOWN TO THE SURFACE OF\n\
   HELL.\n\n\
   NOW, IT'S ON TO THE FINAL CHAPTER OF\n\
   DOOM! -- INFERNO."

def e3Text : String :=
  "THE LOATHSOME SPIDERDEMON THAT\n\
   MASTERMINDED THE INVASION OF THE MOON\n\
   BASES AND CAUSED SO MUCH DEATH HAS HAD\n\
   ITS ASS KICKED FOR ALL TIME.\n\n\
   A HIDDEN DOORWAY OPENS AND YOU ENTER.\n\
   YOU'VE PROVEN TOO TOUGH FOR HELL TO\n\
   CONTAIN, AND NOW HELL AT LAST PLAYS\n\
   FAIR -- FOR YOU EMERGE FROM THE DOORWAY\n\
   TO SEE THE GREEN FIELDS OF EARTH!\n\
   HOME AT LAST.\n\n\
   YOU WONDER WHAT'S HAPPENING ON EARTH\n\
   WHILE YOU WERE BATTLING EVIL UNLEASHED.\n\
   IT'S GOOD THAT NO HELL-SPAWN COULD\n\
   HAVE MADE IT THROUGH THAT DOORWAY WITH\n\
   YOU ..."

def e4Text : String :=
  "THE SPIDER MASTERMIND MUST HAVE SENT FORTH\n\
   ITS LEGIONS OF HELLSPAWN BEFORE YOU\n\
   ENTERED ITS FORTRESS, FOR YOU SEE\n\
   THAT ALL AROUND YOU IS THE SLAIN\n\
   ARMY THAT SIEGED THE GATE OF HELL.\n\n\
   YOU LOOK UPON THE TWISTED CORPSES OF\n\
   THE DEMONS AND FEEL A DEEP\n\
   SATISFACTION. THEY WERE NO MATCH FOR\n\
   YOUR FURY, AND YOU HAVE AVENGED THE\n\
   DEAD.\n\n\
   NOW YOU LOOK ACROSS THE DESOLATE PLAIN,\n\
   AND THINK OF THE FIGHT AHEAD."

def c1Text : String :=
  "YOU HAVE ENTERED DEEPLY INTO THE INFESTED\n\
   STARPORT. BUT SOMETHING IS WRONG. THE\n\
   MONSTERS HAVE BROUGHT THEIR OWN REALITY\n\
   WITH THEM, AND THE STARPORT'S TECHNOLOGY\n\
   IS BEING SUBVERTED BY THEIR PRESENCE.\n\n\
   AHEAD, YOU SEE AN OUTPOST OF HELL, A\n\
   FORTIFIED ZONE. IF YOU CAN GET PAST IT,\n\
   YOU CAN PENETRATE INTO THE HAUNTED HEART\n\
   OF THE STARBASE AND FIND THE CONTROLLING\n\
   SWITCH WHICH HOLDS EARTH'S POPULATION\n\
   HOSTAGE."

def c2Text : String :=
  "YOU HAVE WON! YOUR VICTORY HAS ENABLED\n\
   HUMANKIND TO EVACUATE EARTH AND ESCAPE\n\
   THE NIGHTMARE.  NOW YOU ARE THE ONLY\n\
   HUMAN LEFT ON THE FACE OF THE PLANET.\n\
   CANNIBAL MUTATIONS, CARNIVOROUS ALIENS,\n\
   AND EVIL SPIRITS ARE YOUR ONLY NEIGHBORS.\n\
   YOU SIT BACK AND WAIT FOR DEATH, CONTENT\n\
   THAT YOU HAVE SAVED YOUR SPECIES.\n\n\
   BUT THEN, EARTH CONTROL BEAMS DOWN A\n\
   MESSAGE FROM SPACE: \"SENSORS HAVE LOCATED\n\
   THE SOURCE OF THE ALIEN INVASION. IF YOU\n\
   GO THERE, YOU MAY BE ABLE TO BLOCK THEIR\n\
   ENTRY.  THE ALIEN BASE IS IN THE HEART OF\n\
   YOUR OWN HOME CITY, NOT FAR FROM THE\n\
   STARPORT.\" SLOWLY AND PAINFULLY YOU GET\n\
   UP AND RETURN TO THE FRAY."

def c3Text : String :=
  "YOU ARE AT THE CORRUPT HEART OF THE CITY,\n\
   SURROUNDED BY THE CORPSES OF YOUR ENEMIES.\n\
   YOU SEE NO WAY TO DESTROY THE CREATURES'\n\
   ENTRYWAY ON THIS SIDE, SO YOU CLENCH YOUR\n\
   TEETH AND PLUNGE THROUGH IT.\n\n\
   THERE MUST BE A WAY TO CLOSE IT ON THE\n\
   OTHER SIDE. WHAT DO YOU CARE IF YOU'VE\n\
   GOT TO GO THROUGH HELL TO GET TO IT?"

def c4Text : String :=
  "THE HORRENDOUS VISAGE OF THE BIGGEST\n\
   DEMON YOU'VE EVER SEEN CRUMBLES BEFORE\n\
   YOU, AFTER YOU PUMP YOUR ROCKETS INTO\n\
   HIS EXPOSED BRAIN. THE MONSTER SHRIVELS\n\
   UP AND DIES, ITS THRASHING LIMBS\n\
   DEVASTATING UNTOLD MILES OF HELL'S\n\
   SURFACE.\n\n\
   YOU'VE DONE IT. THE INVASION IS OVER.\n\
   EARTH IS SAVED. HELL IS A WRECK. YOU\n\
   WONDER WHERE BAD FOLKS WILL GO WHEN THEY\n\
   DIE, NOW. WIPING THE SWEAT FROM YOUR\n\
   FOREHEAD YOU BEGIN THE LONG TREK BACK\n\
   HOME. REBUILDING EARTH OUGHT TO BE A\n\
   LOT MORE FUN THAN RUINING IT WAS.\n"

def c5Text : String :=
  "CONGRATULATIONS, YOU'VE FOUND THE SECRET\n\
   LEVEL! LOOKS LIKE IT'S BEEN BUILT BY\n\
   HUMANS, RATHER THAN DEMONS. YOU WONDER\n\
   WHO THE INMATES OF THIS CORNER OF HELL\n\
   WILL BE."

def c6Text : String :=
  "CONGRATULATIONS, YOU'VE FOUND THE\n\
   SUPER SECRET LEVEL!  YOU'D BETTER\n\
   BLAZE THROUGH THIS ONE!"

/-- What plays when an episode or act ends: the text, the flat tiled behind
it, and the music. `none` where the game has nothing to say — most maps
simply lead to the next one.

Doom shows a page at the end of every episode; Doom II shows one after the
last map of each act (6, 11, 20, 30) and on the way into each of its two
secret levels (15 → 31, 31 → 32). The flats are vanilla's own, and each is
an ordinary floor texture out of the IWAD. -/
structure Page where
  text  : String
  flat  : String
  music : String
  deriving Repr, Inhabited

/-- The page shown on *leaving* `id`, if any. `secret` says the exit taken
was the secret one, which is what distinguishes MAP15's two endings. -/
def pageFor (id : MapId) (secret : Bool) : Option Page :=
  let doom := fun (t f : String) => some { text := t, flat := f, music := "D_VICTOR" }
  let doom2 := fun (t f : String) => some { text := t, flat := f, music := "D_READ_M" }
  match id with
  | .episode 1 8 => doom e1Text "FLOOR4_8"
  | .episode 2 8 => doom e2Text "SFLR6_1"
  | .episode 3 8 => doom e3Text "MFLR8_4"
  | .episode 4 8 => doom e4Text "MFLR8_3"
  | .episode .. => none
  | .level 6  => doom2 c1Text "SLIME16"
  | .level 11 => doom2 c2Text "RROCK14"
  | .level 20 => doom2 c3Text "RROCK07"
  | .level 30 => doom2 c4Text "RROCK17"
  -- MAP15 and MAP31 only speak when you take their *secret* exit; the
  -- ordinary way out of either just leads on to the next map
  | .level 15 => if secret then doom2 c5Text "RROCK13" else none
  | .level 31 => if secret then doom2 c6Text "RROCK19" else none
  | .level _ => none

/-- How long a page runs before it moves on by itself: vanilla's
`strlen(finaletext) * TEXTSPEED + TEXTWAIT`. -/
def pageTics (p : Page) : Nat := p.text.length * textSpeed + textWait

/-- How many characters of the page are showing after `count` tics. Vanilla
`F_TextWrite` starts the reveal ten tics in — `(finalecount - 10) / TEXTSPEED`
— so the flat is bare for a moment before the first letter lands. -/
def shownChars (count : Nat) : Nat := (count - 10) / textSpeed

/-- What Doom shows *after* the text, per episode — vanilla `F_Drawer`'s
second stage. `CREDIT` is the retail card and `HELP2` the one the earlier
releases used; the caller tries them in order and takes whichever the IWAD
actually has. Episode 3 has no picture at all: it gets the bunny. -/
inductive EndPic where
  /-- A still, named by the first lump present of those listed. -/
  | still (lumps : List String)
  /-- Episode 3's scrolling bunny, and then `THE END`. -/
  | bunny
  deriving Repr, Inhabited

/-- The end picture for an episode, or `none` where there is none to show
(Doom II ends on the cast call instead). -/
def endPic : MapId → Option EndPic
  | .episode 1 _ => some (.still ["CREDIT", "HELP2"])
  | .episode 2 _ => some (.still ["VICTORY2"])
  | .episode 3 _ => some .bunny
  | .episode 4 _ => some (.still ["ENDPIC"])
  | _ => none

/-! ## The cast call

Doom II's curtain call: every monster in the game walks on, takes a swing at
you, and — when you pull the trigger — dies, in `castorder`'s order, ending
with the marine himself. Vanilla `F_CastTicker` walks the *real* state
tables, which is why each one moves and sounds exactly as it does in play,
and this does the same: the members below are the ordinary `ActorInfo`s. -/

/-- The marine has no `mobjinfo` entry in DILL — he is the player, not a
thing — so his corner of `info.c` is spelled out here: `S_PLAY_RUN1`–`4`,
the two attack frames, and the seven-frame death. -/
def heroInfo : ActorInfo :=
  { sprite := "PLAY"
    states := #[
      -- run (S_PLAY_RUN1…4), looping
      { frame := 'A', tics := 4, next := some 1 },
      { frame := 'B', tics := 4, next := some 2 },
      { frame := 'C', tics := 4, next := some 3 },
      { frame := 'D', tics := 4, next := some 0 },
      -- attack (S_PLAY_ATK1/2); the second is the muzzle flash
      { frame := 'E', tics := 12, next := some 5 },
      { frame := 'F', tics := 6, next := some 0, bright := true },
      -- death (S_PLAY_DIE1…7), ending on a frame it holds forever
      { frame := 'H', tics := 10, next := some 7 },
      { frame := 'I', tics := 10, next := some 8 },
      { frame := 'J', tics := 10, next := some 9 },
      { frame := 'K', tics := 10, next := some 10 },
      { frame := 'L', tics := 10, next := some 11 },
      { frame := 'M', tics := 10, next := some 12 },
      { frame := 'N', tics := -1, next := none }]
    seeState := some 0
    missileState := some 4
    deathState := some 6 }

/-- One entry of vanilla's `castorder`: what it is called, and what it is.
`none` is the marine, who gets `heroInfo` above. -/
structure CastMember where
  name : String
  kind : Option ActorKind
  deriving Repr, Inhabited

/-- Vanilla's `castorder`, in order, names from `d_englsh.h`. -/
def castOrder : Array CastMember := #[
  { name := "ZOMBIEMAN", kind := some .zombieman },
  { name := "SHOTGUN GUY", kind := some .shotgunGuy },
  { name := "HEAVY WEAPON DUDE", kind := some .chaingunner },
  { name := "IMP", kind := some .imp },
  { name := "DEMON", kind := some .demon },
  { name := "LOST SOUL", kind := some .lostSoul },
  { name := "CACODEMON", kind := some .cacodemon },
  { name := "HELL KNIGHT", kind := some .hellKnight },
  { name := "BARON OF HELL", kind := some .baron },
  { name := "ARACHNOTRON", kind := some .arachnotron },
  { name := "PAIN ELEMENTAL", kind := some .painElemental },
  { name := "REVENANT", kind := some .revenant },
  { name := "MANCUBUS", kind := some .mancubus },
  { name := "ARCH-VILE", kind := some .archVile },
  { name := "THE SPIDER MASTERMIND", kind := some .spiderMastermind },
  { name := "THE CYBERDEMON", kind := some .cyberdemon },
  { name := "OUR HERO", kind := none }]

/-- The `ActorInfo` a cast member performs from. -/
def castInfo (m : CastMember) : ActorInfo :=
  match m.kind with
  | some k => ActorInfo.ofKind k
  | none => heroInfo

/-- Where the cast call stands: who is on, which of their states is up, and
vanilla's three little flags — `castframes` counting the walk cycle toward
its attack, `castattacking`, and `castonmelee` alternating so a monster with
both attacks shows each in turn. -/
structure Cast where
  index     : Nat := 0
  state     : Nat := 0
  tics      : Int := 0
  frames    : Nat := 0
  attacking : Bool := false
  onMelee   : Bool := false
  dying     : Bool := false
  deriving Repr, Inhabited

/-- Put a cast member on: their walk state, and their sight cry. -/
def Cast.enter (index : Nat) : Cast × Option Sfx :=
  let m := castOrder[index]!
  let info := castInfo m
  let st := info.seeState.getD 0
  ({ index, state := st, tics := info.states[st]!.tics, frames := 0 },
   m.kind.bind ActorKind.sightSfx)

/-- The trigger, during the cast: whoever is on stage dies where they stand,
with their death cry — vanilla `F_CastResponder`. A member already dying
takes no notice. -/
def Cast.shoot (c : Cast) : Cast × Option Sfx :=
  if c.dying then (c, none) else
  let m := castOrder[c.index]!
  let info := castInfo m
  match info.deathState with
  | none => (c, none)
  | some st =>
    ({ c with state := st, tics := info.states[st]!.tics
              dying := true, attacking := false, frames := 0 },
     m.kind.bind ActorKind.deathSfx)

/-- One tic of `F_CastTicker`. Returns the new state and anything to be
heard.

The shape is vanilla's: the state's tics run down; when they do, either the
chain advances or — at its end, or once a corpse has settled — the next
member walks on. Twelve frames into the walk the member attacks, alternating
melee and missile, and twenty-four frames in (or the moment the chain leads
back to the walk) it stops attacking and paces again. -/
def Cast.step (c : Cast) : Cast × Option Sfx := Id.run do
  let mut c := c
  if c.tics > 1 then
    return ({ c with tics := c.tics - 1 }, none)
  let m := castOrder[c.index]!
  let info := castInfo m
  let seeSt := info.seeState.getD 0
  let cur := info.states[c.state]!
  -- the chain has run out — a settled corpse, or a strip with no next: the
  -- next member walks on
  if cur.tics < 0 || cur.next.isNone then
    let next := if c.index + 1 ≥ castOrder.size then 0 else c.index + 1
    return Cast.enter next
  -- otherwise step along it
  let st := cur.next.getD seeSt
  c := { c with state := st, frames := c.frames + 1 }
  let mut cue : Option Sfx := none
  -- vanilla's "sound hacks": the attack frames speak as they land. DILL's
  -- tables carry each actor's launch cry, which is the same sound on the
  -- same frame.
  if c.attacking && st == (info.missileState.getD 9999) then
    cue := m.kind.bind ActorKind.launchSfx
  -- twelve frames of pacing, then a swing
  if c.frames == 12 then
    let melee := info.meleeState
    let missile := info.missileState
    let pick := if c.onMelee then melee.orElse (fun _ => missile)
                else missile.orElse (fun _ => melee)
    match pick with
    | some st' =>
      c := { c with state := st', attacking := true, onMelee := !c.onMelee }
      cue := (if c.onMelee then m.kind.bind ActorKind.launchSfx else cue)
    | none => c := { c with onMelee := !c.onMelee }
  -- and back to pacing when the swing is done
  if c.attacking && (c.frames ≥ 24 || c.state == seeSt) then
    c := { c with attacking := false, frames := 0, state := seeSt }
  let t := info.states[c.state]!.tics
  return ({ c with tics := if t < 0 then 15 else t }, cue)

/-- The sprite family and frame the cast is showing, as `spriteRots` keys
them (`TROOD`, say). Vanilla `F_CastDrawer` takes rotation 0 of it — the
cast always faces you, whichever way the monster would be pointing. -/
def Cast.sprite (c : Cast) : String :=
  let info := castInfo castOrder[c.index]!
  let st := info.states[c.state]!
  (st.spriteOverride.getD info.sprite).push st.frame

def Cast.name (c : Cast) : String := castOrder[c.index]!.name

end Finale
end Dill
