import Dill.Game.Tick
import Dill.Game.Save
import Dill.Game.Cheats
import Dill.Render.Frame
import Dill.Render.Hud
import Dill.Render.Bsp
import Dill.Render.Wipe

/-!
# The user interface, as pure state

The menus, the HUD assembly, and the composition of a frame. None of it
touches the OS, so it lives in the core beside the simulation rather than in
the shell: `Ui.step` is to the menus what `tick` is to the game, and both are
testable by folding a list of `Input`s.

`Main.lean` keeps only what genuinely needs IO — reading a save off disk,
starting the music, putting the frame on screen — and drives these.
-/

namespace Dill

/-- Project live mobjs into what the renderer needs. -/
def renderMobjs (g : GameState) : Array Render.RenderMobj :=
  g.mobjs.filterMap fun m =>
    if m.removed then none else some
      { x := m.x, y := m.y, z := m.z, angle := m.angle
        sprite := m.sprite, frame := m.stateDef.frame
        bright := m.stateDef.bright, shadow := m.info.shadow
        radius := m.info.radius }

/-- Doom's `fixedcolormap`: invulnerability paints the world through the
inverse map (row 32), the light-amp goggles hold it fully bright (row 0).
Invulnerability wins when both are active. -/
def fixedColormap (st : PlayerStatus) : Option Nat :=
  if st.invulnTics > 0 then some 32
  else if st.gogglesTics > 0 then some 0
  else none

/-- Assemble the HUD: weapon sprite (bobbing at ready), flash, vitals. -/
def hudInfo (g : GameState) (paused : Bool := false)
    (message : String := "") : Render.HudInfo :=
  let st := g.status
  -- `[·]?` not `[·]!`: never panic the renderer on a stray attack index
  let step := st.attack.bind (st.weapon.attack[·]?)
  let frame := match step with
    | some s => s.frame
    | none => Weapon.idleFrame st.weapon g.tics
  let (sx, sy) :=
    if st.weaponY != 32 || st.pending.isSome then
      (1.0, st.weaponY)          -- sliding down/up mid-switch: no bob
    else match step with
    | some _ => (1.0, 32.0)
    | none =>
      let speed := Float.sqrt (g.player.momX^2 + g.player.momY^2)
      let bob := min 16.0 (speed * speed / 4)
      let t := Float.ofNat g.tics * 0.098
      (1.0 + bob * Float.cos t, 32.0 + bob * Float.abs (Float.sin t))
  let sec := g.level.sectors[g.level.sectorAt g.player.x g.player.y]!
  { health := st.health
    armor := st.armor
    ammo := st.weapon.ammoType.map st.ammoCount
    -- the dead drop their gun out of view
    weapon := if st.dead then none
      else some (st.weapon.sprite.push frame |>.push '0', sx, sy)
    flash := if st.dead then none else step.bind fun s =>
      s.flash.map fun c => st.weapon.flashSprite.push c |>.push '0'
    light := sec.light
    fixedColormap := fixedColormap st
    -- partial invisibility fuzzes your own weapon (vanilla): solid while the
    -- powerup is strong, blinking off in its last ~3.6 s as it wears out
    weaponFuzz := if st.invisTics > 128 || st.invisTics &&& 8 != 0
      then some g.tics else none
    keys := (st.blueKey, st.yellowKey, st.redKey)
    paused := paused
    message := message
    face := some g.faceLump }

/-- Vanilla's screen tint: pain reds beat bonus golds; death holds a
steady red so the state is unmistakable.

The two `1 +` / `9 +` bases are vanilla `ST_doPaletteStuff`'s `STARTREDPALS`
and `STARTBONUSPALS`: the count picks *how far into* the run of tints to go
(`(cnt+7)>>3`, clamped to the run's length) and the base says where the run
begins. Folding the base into the clamp instead — as this did — lands every
tint one step lighter than vanilla's, so a single pickup showed the palest
gold rather than the second. -/
def flashPalette (st : PlayerStatus) : Nat :=
  -- `NUMREDPALS` is 8, so the offset within the run clamps at 7
  let pain := if st.damageCount > 0
    then 1 + min 7 ((st.damageCount + 7) / 8) else 0
  let pain := if st.dead then max pain 5 else pain
  -- the berserk pack washes the screen red on pickup and fades over ~20 s
  -- (vanilla `bzc = 12 - powers>>6`), subtle at its peak
  let berserk := if st.berserk && st.berserkTics < 768
    then 1 + min 7 ((12 - st.berserkTics / 64 + 7) / 8) else 0
  let red := max pain berserk
  if red > 0 then red
  -- `NUMBONUSPALS` is 4: four golds starting at palette 9
  else if st.bonusCount > 0 then 9 + min 3 ((st.bonusCount + 7) / 8)
  -- the radiation suit washes the screen green (PLAYPAL row 13); in the last
  -- four seconds it blinks, warning the suit is about to expire (vanilla)
  else if st.radsuitTics > 0
      && (st.radsuitTics > 4 * 35 || st.radsuitTics &&& 8 != 0) then 13
  else 0


/-- Menu / UI mode: the game only ticks while `.playing`. -/
inductive UiMode where
  | title | menu | saveMenu | loadMenu | episodeMenu | skillMenu | playing
  | intermission
  deriving BEq

/-- The five difficulties, in menu order (index + 1 = skill number). -/
def skillItems : Array String :=
  #["I'M TOO YOUNG TO DIE", "HEY NOT TOO ROUGH", "HURT ME PLENTY",
    "ULTRA-VIOLENCE", "NIGHTMARE!"]

/-- The four Ultimate Doom episodes, indexed by the `ExM1` digit. Which of
them a given IWAD actually holds is discovered at load; the shareware WAD
carries only the first. -/
def episodeNames : Array String :=
  #["KNEE-DEEP IN THE DEAD", "THE SHORES OF HELL", "INFERNO",
    "THY FLESH CONSUMED", "SIGIL", "SIGIL II"]

/-- Episodes past the six that have names get a generic label rather than
being hidden — a PWAD is free to add an E7. -/
def episodeLabel (e : Nat) : String :=
  episodeNames[e - 1]?.getD s!"EPISODE {e}"


def mainMenuItems : Array String := #["NEW GAME", "SAVE GAME", "LOAD GAME", "QUIT"]

/-- How many save slots there are. The menus size themselves from this and
`Main.readSlots` fills exactly this many, so the number lives in one place
rather than being spelled out at each. -/
def saveSlots : Nat := 4

/-- A blank palette frame — the backdrop for the title and the tally. -/
def blankFrame : ByteArray :=
  ByteArray.mk (Array.replicate (Render.screenW * Render.screenH) 0)

/-- What a run of the game is fixed on: the data files, the decoded assets,
and the choices made on the command line. Settled before the loop starts and
never changed by it, so `Ui.step` and `composeFrame` can take it by value. -/
structure Session where
  wad      : Wad
  assets   : Assets
  /-- Episode numbers this IWAD actually holds, and their menu labels. -/
  episodes     : Array Nat
  episodeItems : Array String
  /-- Offer the episode picker: more than one episode, and no map named on
  the command line (an explicit map is already a choice). -/
  pickEpisode  : Bool
  /-- `--fit`: keep the letterbox in gameplay too. -/
  fit      : Bool

/-- Everything the menus own. Pure state, stepped once per frame by
`Ui.step` — the same shape as `GameState` and `tick`, and testable for the
same reason. -/
structure Ui where
  mode          : UiMode := .title
  sel           : Nat := 0
  /-- Save/load slot labels, filled by the `readSlots` effect. -/
  slots         : Array String := #[]
  /-- Last frame's input, for edge detection. -/
  prev          : Input := {}
  inGame        : Bool := false
  showMap       : Bool := false
  skill         : Nat := 4
  /-- The map `NEW GAME` starts on: the CLI's, or the picker's choice. -/
  startMap      : String
  message       : String := ""
  messageFrames : Nat := 0
  cheatBuf      : String := ""
  /-- Frames since start, driving the blinking skull cursor. -/
  frames        : Nat := 0
  wi            : Render.WiView := default

/-- What a UI step asks the shell to do. `Ui.step` is pure, so everything
that touches a file, the music, or the clock leaves as one of these and the
game loop performs it. Keeping the list small is the point: it is the entire
IO surface of the menus. -/
inductive UiEffect where
  /-- Start a fresh game on `map` at `skill`. -/
  | newGame (map : String) (skill : Nat)
  /-- Carry the current arsenal into `map` (the intermission's Continue). -/
  | nextMap (map : String)
  /-- Warp to `map` at a pistol start (the `dillclev` cheat). -/
  | warp (map : String)
  /-- The episode is over: back to the title screen. -/
  | toTitle
  | saveSlot (slot : Nat)
  | loadSlot (slot : Nat)
  /-- Refresh the four slot labels before a save/load menu is shown. -/
  | readSlots
  | quit
  deriving Repr, BEq

/-- The tally screen's contents, from the game that just ended. Pure. -/
def intermissionView (g : GameState) : Render.WiView :=
  -- a zero total is clamped to 1, as vanilla `WI_initVariables` does with
  -- `maxkills`/`maxitems`/`maxsecret` — so a map with nothing to find
  -- tallies 0%, not 100%
  let pct := fun (n total : Nat) => n * 100 / max total 1
  let finished := MapId.parse g.level.name
  let entering := (nextMapName g.level.name g.secretExit).bind MapId.parse
  { backPic := (finished.map MapId.intermissionBack).getD "WIMAP0"
    finishedPic := (finished.map MapId.nameGraphic).getD ""
    enteringPic := entering.map MapId.nameGraphic
    killPct := pct g.kills g.killTotal
    itemPct := pct g.items g.itemTotal
    secretPct := pct g.secrets g.secretTotal
    -- the sim runs at Doom's 35 Hz, so tics divide straight down
    levelTime := g.tics / 35
    parTime := (finished.map MapId.parTime).getD 0 }

/-- Compose the frame the player sees, as 426×200 palette indices: the world
(when there is one), with the tally or a menu laid over it.

Pure — a function of the session, the UI, and the game — which means a frame
can be composed and compared byte for byte in a test, without a window. The
palette flash and the automap overlay are applied at present time, not here,
because both depend on how the frame is being put on screen. -/
def composeFrame (sess : Session) (ui : Ui) (g : GameState) : ByteArray :=
  -- the dead sink to the floor; the living get vanilla's bobbing eye
  let eyeZ := if g.status.dead then g.player.z + 8.0 else g.viewZ
  let view : Render.View :=
    { x := g.player.x, y := g.player.y
      height := eyeZ, angle := g.player.angle
      fixedColormap := fixedColormap g.status, tics := g.tics }
  let world :=
    if ui.mode == .title || ui.mode == .intermission || !ui.inGame then
      blankFrame
    else
      Render.renderPalette g.level sess.assets view (renderMobjs g)
        (some (hudInfo g false
          (if ui.messageFrames > 0 then ui.message else "")))
  let skullAlt := ui.frames / 8 % 2 == 1
  let listMenu := fun (header : String) (items : Array String) =>
    some ({ titleScreen := !ui.inGame, header, items, selected := ui.sel
            skullAlt
            -- a failed load leaves the player in this menu, and from the
            -- title screen there is no HUD line to say so
            message := if ui.messageFrames > 0 then ui.message else ""
          } : Render.MenuView)
  let menuView : Option Render.MenuView := match ui.mode with
    | .title => some { titleScreen := true }
    | .menu => some { titleScreen := !ui.inGame, logo := true
                      items := mainMenuItems
                      itemLumps := #["M_NGAME", "M_SAVEG", "M_LOADG", "M_QUITG"]
                      selected := ui.sel, skullAlt }
    | .saveMenu => listMenu "SAVE GAME" ui.slots
    | .loadMenu => listMenu "LOAD GAME" ui.slots
    | .episodeMenu => listMenu "CHOOSE EPISODE" sess.episodeItems
    | .skillMenu => listMenu "CHOOSE SKILL" skillItems
    | .playing | .intermission => none
  match menuView with
  | some mv => Render.withFrame world (Render.drawMenu sess.assets mv)
  | none =>
    if ui.mode == .intermission then
      Render.withFrame world (Render.drawIntermission sess.assets ui.wi)
    else world

/-- One frame of menu logic: the `tick` of the UI.

Pure, and deliberately so — the menus were the one part of the game that
could not be tested without opening a window. `typed` carries the letters
and digits the shell collected since the last frame, so cheat scanning is in
here too rather than tangled into the loop. Returns the new UI state, the
game state (a cheat may have altered it), and what the shell must go and do.

The game itself is *not* stepped here; the loop does that on its own 35 Hz
accumulator, and only while `mode` is `.playing`. -/
def Ui.step (sess : Session) (input : Input) (typed : String)
    (ui : Ui) (g : GameState) : Ui × GameState × Array UiEffect := Id.run do
  let mut ui := { ui with frames := ui.frames + 1 }
  let mut g := g
  let mut fx : Array UiEffect := #[]
  let pressed := fun (get : Input → Bool) => get input && !(get ui.prev)
  let confirm := pressed (·.enter) || pressed (·.use)
  let up := pressed (·.forward)
  let down := pressed (·.back)
  let esc := pressed (·.pause)
  -- move the highlight within a menu of `n` items
  let cycle := fun (n : Nat) (sel : Nat) =>
    if n == 0 then 0
    else if up then (sel + n - 1) % n
    else if down then (sel + 1) % n
    else sel

  match ui.mode with
  | .title =>
    if confirm || esc || pressed (·.fire) then
      ui := { ui with mode := .menu, sel := 0 }
  | .menu =>
    ui := { ui with sel := cycle mainMenuItems.size ui.sel }
    if esc then
      ui := { ui with mode := if ui.inGame then .playing else .title }
    else if confirm then
      match ui.sel with
      | 0 =>
        if sess.pickEpisode then ui := { ui with sel := 0, mode := .episodeMenu }
        -- default the highlight to Ultra-Violence
        else ui := { ui with sel := 3, mode := .skillMenu }
      | 1 =>
        if ui.inGame then
          fx := fx.push .readSlots
          ui := { ui with sel := 0, mode := .saveMenu }
      | 2 =>
        fx := fx.push .readSlots
        ui := { ui with sel := 0, mode := .loadMenu }
      | _ => fx := fx.push .quit
  | .saveMenu =>
    ui := { ui with sel := cycle saveSlots ui.sel }
    if esc then ui := { ui with mode := .menu }
    else if confirm then
      fx := fx.push (.saveSlot ui.sel)
      ui := { ui with mode := .playing }
  | .loadMenu =>
    ui := { ui with sel := cycle saveSlots ui.sel }
    if esc then ui := { ui with mode := .menu }
    else if confirm then
      -- the loop sets `.playing` only if the slot actually loads
      fx := fx.push (.loadSlot ui.sel)
  | .episodeMenu =>
    ui := { ui with sel := cycle sess.episodeItems.size ui.sel }
    if esc then ui := { ui with mode := .menu }
    else if confirm then
      ui := { ui with
        startMap := s!"E{sess.episodes[ui.sel]?.getD 1}M1"
        sel := 3, mode := .skillMenu }
  | .skillMenu =>
    ui := { ui with sel := cycle skillItems.size ui.sel }
    if esc then
      ui := { ui with sel := 0
                      mode := if sess.pickEpisode then .episodeMenu else .menu }
    else if confirm then
      let skill := ui.sel + 1
      fx := fx.push (.newGame ui.startMap skill)
      ui := { ui with skill, showMap := false, mode := .playing }
  | .intermission =>
    if confirm then
      match nextMapName g.level.name g.secretExit with
      | none => fx := fx.push .toTitle
      | some next => fx := fx.push (.nextMap next)
  | .playing =>
    if esc then ui := { ui with sel := 0, mode := .menu }
    if pressed (·.map) then ui := { ui with showMap := !ui.showMap }
    -- anything the simulation wants said (vanilla's `player->message`),
    -- drained the way the loop drains `sounds`. A cheat typed this same
    -- frame overwrites it below, which is the right precedence: the cheat
    -- is what the player just asked for.
    if !g.message.isEmpty then
      ui := { ui with message := g.message, messageFrames := 100 }
      g := { g with message := "" }
    -- cheat codes accumulate from the letters and digits typed this frame
    if !typed.isEmpty then
      let buf := ui.cheatBuf ++ typed
      -- `.toString` is not redundant: `String.drop` yields a `String.Slice`
      ui := { ui with cheatBuf :=
        if buf.length > 16 then (buf.drop (buf.length - 16)).toString else buf }
    if let some cheat := Cheat.scan ui.cheatBuf then
      let (g', msg) := g.applyCheat cheat sess.assets.hasSuperShotgun
      g := g'
      ui := { ui with cheatBuf := "", message := msg, messageFrames := 100 }
      if let .warp a b := cheat then
        -- the digits mean ExMy or MAPnn depending on the WAD (see MapId)
        let name := match MapId.parse g.level.name with
          | some here => (here.warpTarget a b).name
          | none => s!"E{a}M{b}"
        fx := fx.push (.warp name)
    -- F5 / F9 quicksave and quickload, through slot 1
    if pressed (·.save) then fx := fx.push (.saveSlot 0)
    if pressed (·.load) then fx := fx.push (.loadSlot 0)

  -- `messageFrames` is a `Nat`, so the countdown truncates at 0 instead of
  -- wrapping and needs no guard of its own (as in `tick`'s timer block)
  ui := { ui with prev := input
                  messageFrames := ui.messageFrames - 1 }
  return (ui, g, fx)

end Dill
