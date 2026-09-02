import Dill

/-!
The imperative shell's entry point. All file and console IO lives here;
everything it calls in `Dill` is pure.
-/

open Dill

def usage : String :=
  "usage: dill <file.wad> [MAP] [--classic] [--fit]   play (MAP e.g. E1M1, MAP07)
       dill info  <file.wad>              print the WAD header and lump directory
       dill map   <file.wad> <MAP>        print a map's stats (e.g. E1M1)
       dill ppm   <file.wad> <NAME> <out> dump a texture/flat/sprite as a PPM image
       dill view  <file.wad> <MAP> <out> [x y deg]  render one frame to a PPM
       dill fly   <file.wad> [MAP]        free-fly camera, no physics
       dill bench <file.wad> <MAP> <n>    render n frames and report frames/sec
       dill music <file.wad> <MAP> <out>  dump a map's music as a MIDI file
       dill pattern [frames]              open the window with a test pattern

  any command also takes -file <file.wad> (repeatable) to layer PWADs
  --classic presents the original 4:3 picture; --fit letterboxes to keep
  the full 16:9 width rather than cropping to cover the display"

/-- Unwrap a pure parse result into IO. -/
def orDie (what : String) : Except String α → IO α
  | .ok a     => return a
  | .error e  => throw (IO.userError s!"{what}: {e}")

/-- Read and parse one WAD, turning read and parse failures alike into
errors that name the file, rather than surfacing a raw IO exception. -/
def loadOneWad (path : String) : IO Wad := do
  unless (← System.FilePath.pathExists path) do
    throw (IO.userError s!"cannot open {path}: no such file or directory")
  let bytes ← try
    IO.FS.readBinFile path
  catch e =>
    throw (IO.userError s!"cannot open {path}: {e}")
  match Wad.parse bytes with
  | .ok wad     => return wad
  | .error e    => throw (IO.userError s!"{path}: {e}")

/-- The base WAD with every `-file` PWAD merged over it, in order.

The base has to be an IWAD: a PWAD carries only what it replaces, so
loading one on its own fails later and obscurely on whatever shared lump
it happens to lack first (`PLAYPAL`, usually). Say so up front instead. -/
def loadWad (path : String) (pwads : List String := []) : IO Wad := do
  let mut wad ← loadOneWad path
  if wad.kind == .pwad then
    throw (IO.userError s!"{path} is a PWAD, not an IWAD — it holds only \
      the lumps it replaces. Load it over an IWAD instead:\n  \
      dill doom.wad -file {path}")
  for p in pwads do
    wad := wad.merge (← loadOneWad p)
  return wad

/-- Pull every `-file <wad>` pair out of the arguments, leaving the rest.

Pairs are taken left to right, so the only way a `-file` survives into the
leftovers is with nothing after it to consume — `args.contains "-file"` is
therefore an exact test for the dangling case, which `run` reports rather
than letting the bare flag go on to be read as a map name. -/
def splitPwads : List String → List String × List String
  | [] => ([], [])
  | "-file" :: w :: rest => let (ps, as) := splitPwads rest; (w :: ps, as)
  | a :: rest => let (ps, as) := splitPwads rest; (ps, a :: as)

/-- Right-align a number in a field of `width` characters. -/
def pad (n : Nat) (width : Nat) : String :=
  let s := toString n
  "".pushn ' ' (width - s.length) ++ s

def cmdInfo (path : String) (pwads : List String := []) : IO Unit := do
  let wad ← loadWad path pwads
  let kind := match wad.kind with
    | .iwad => "IWAD"
    | .pwad => "PWAD"
  IO.println s!"{path}: {kind}, {wad.lumps.size} lumps, {wad.bytes.size} bytes"
  IO.println ""
  IO.println "index   offset     size name"
  for k in [0:wad.lumps.size] do
    let l := wad.lumps[k]!
    IO.println s!"{pad k 5} {pad l.offset 8} {pad l.size 8} {l.name}"

def cmdMap (path mapName : String) (pwads : List String := []) : IO Unit := do
  let wad ← loadWad path pwads
  let lvl ← orDie mapName (Level.load wad mapName)
  IO.println s!"{mapName}: \
    {lvl.vertexes.size} vertexes, \
    {lvl.linedefs.size} linedefs, \
    {lvl.sidedefs.size} sidedefs, \
    {lvl.sectors.size} sectors"
  IO.println s!"BSP: {lvl.nodes.size} nodes, \
    {lvl.subsectors.size} subsectors, \
    {lvl.segs.size} segs"
  IO.println s!"{lvl.things.size} things, \
    blockmap {lvl.blockmap.cols}×{lvl.blockmap.rows}"
  if let some start := lvl.playerStart then
    let sec := lvl.sectors[lvl.sectorAt start.x start.y]!
    IO.println s!"player 1 start: ({start.x}, {start.y}) facing {start.angle}°, \
      in sector with floor {sec.floorH} ceiling {sec.ceilH} \
      light {sec.light} floor flat {sec.floorFlat}"

/-- Write palette-indexed pixels as a binary PPM for visual inspection. -/
def writePpm (path : String) (assets : Assets) (width height : Nat)
    (pixelAt : Nat → Nat → Option UInt8) : IO Unit := do
  let mut out := ByteArray.emptyWithCapacity (width * height * 3)
  for y in [0:height] do
    for x in [0:width] do
      match pixelAt x y with
      | some c =>
        let c := c.toNat * 3
        out := out.push (assets.palette.get! c)
        out := out.push (assets.palette.get! (c + 1))
        out := out.push (assets.palette.get! (c + 2))
      | none =>  -- transparent: magenta, so holes are obvious
        out := out.push 255; out := out.push 0; out := out.push 255
  let header := s!"P6\n{width} {height}\n255\n".toUTF8
  IO.FS.writeBinFile path (header ++ out)
  IO.println s!"wrote {path} ({width}×{height})"

def cmdPpm (path name out : String) (pwads : List String := []) : IO UInt32 := do
  let wad ← loadWad path pwads
  let assets ← orDie "assets" (Assets.load wad)
  if let some pic := assets.textures.get? name <|> assets.sprites.get? name then
    writePpm out assets pic.width pic.height fun x y =>
      if pic.opaqueAt x y then some (pic.get x y) else none
    return 0
  if let some flat := assets.flats.get? name then
    writePpm out assets 64 64 fun x y => some (flat.get! (y * 64 + x))
    return 0
  IO.eprintln s!"{name}: no texture, sprite, or flat with that name"
  return 1

/-- Dump a map's music as a Standard MIDI File, for inspection the way
`ppm` dumps a graphic. Takes the same `-file` PWADs as everything else. -/
def cmdMusic (path mapName out : String) (pwads : List String := []) :
    IO UInt32 := do
  let wad ← loadWad path pwads
  let candidates := match MapId.parse mapName with
    | some id => [id.music]
    | none =>
      if mapName == "INTRO" then ["D_INTRO", "D_DM2TTL"] else [s!"D_{mapName}"]
  let some lump := candidates.findSome? (wad.find? ·)
    | do IO.eprintln s!"{mapName}: no music lump ({", ".intercalate candidates})"
         return 1
  match Music.toMidi (wad.data lump) with
  | .error e => IO.eprintln s!"{lump.name}: {e}"; return 1
  | .ok midi =>
    IO.FS.writeBinFile out midi
    IO.println s!"wrote {out} ({lump.name}, {midi.size} bytes)"
    return 0

/-- Start a map's music: the `D_<map>` MUS lump, converted to MIDI for the
system synth. Missing or malformed music is just silence. -/
def playMusic (wad : Wad) (mapName : String) : IO Unit := do
  -- A map's music comes from `MapId`. Anything else is a direct name, but
  -- the title screen needs both spellings: Doom II has no `D_INTRO`, it
  -- calls its title track `D_DM2TTL`.
  let candidates := match MapId.parse mapName with
    | some id => [id.music]
    | none =>
      if mapName == "INTRO" then ["D_INTRO", "D_DM2TTL"] else [s!"D_{mapName}"]
  let some lump := candidates.findSome? (wad.find? ·) | return
  match Music.toMidi (wad.data lump) with
  | .ok midi => Shell.musicPlay midi
  | .error e => IO.eprintln s!"music {mapName}: {e}"

/-- Parse the DMX sound lumps (`DS*`) and hand them to the mixer. -/
def loadSounds (wad : Wad) : IO Unit := do
  for i in [0:Sfx.lumps.size] do
    if let some lump := wad.find? Sfx.lumps[i]! then
      let b := wad.data lump
      if b.size > 8 then
        let rate := (Bytes.u16le b 2).toUInt32
        let count := (Bytes.u32le b 4).toNat
        -- 8-byte header, then the samples with 16 pad bytes on each side
        let (start, len) := if count > 32 && b.size ≥ 8 + count
          then (24, count - 32) else (8, b.size - 8)
        Shell.soundLoad (UInt32.ofNat i) rate (b.extract start (start + len))

/-- The view from a map's player 1 start. The eye goes through
`Level.eyeZ`, so an offline render stands where the game would stand — a
bare `floorH + 41` puts the camera through the ceiling of anything shorter
than a room. -/
def startView (lvl : Level) : Render.View :=
  match lvl.playerStart with
  | some t =>
    let sec := lvl.sectors[lvl.sectorAt t.x t.y]!
    { x := t.x, y := t.y
      height := lvl.eyeZ t.x t.y sec.floorH Player.viewHeight
      angle := Float.ofInt t.angle * 3.14159265358979 / 180 }
  | none => { x := 0, y := 0, height := 41, angle := 0 }

/-- Render one frame from the player start (or a given camera) to a PPM. -/
def cmdView (path mapName out : String) (cam : Option (Float × Float × Float))
    (pwads : List String := [])
    : IO Unit := do
  let wad ← loadWad path pwads
  let lvl ← orDie mapName (Level.load wad mapName)
  let assets ← orDie "assets" (Assets.load wad)
  let view := match cam with
    | some (x, y, deg) =>
      let sec := lvl.sectors[lvl.sectorAt x y]!
      { x, y, height := lvl.eyeZ x y sec.floorH Player.viewHeight
        angle := deg * 3.14159265358979 / 180 : Render.View }
    | none => startView lvl
  let g := GameState.start lvl
  let frame := Render.renderPalette lvl assets view (renderMobjs g)
  writePpm out assets Render.screenW Render.screenH fun x y =>
    some (frame.get! (y * Render.screenW + x))

/-- Interactive free-fly camera (no physics yet): arrows/WASD + mouse. -/
def cmdFly (path mapName : String) (pwads : List String := []) : IO UInt32 := do
  let wad ← loadWad path pwads
  let lvl ← orDie mapName (Level.load wad mapName)
  let assets ← orDie "assets" (Assets.load wad)
  let mut view := startView lvl
  let staticMobjs := renderMobjs (GameState.start lvl)
  Shell.init 960 720 s!"dill — {mapName}"
  repeat
    let input := Input.decode (← Shell.poll)
    if input.quit then break
    let turn := (if input.turnLeft then 0.05 else 0)
              + (if input.turnRight then -0.05 else 0)
              - Float.ofInt input.mouseDx * 0.005
    let move := (if input.forward then 8.0 else 0)
              + (if input.back then -8.0 else 0)
    let strafe := (if input.strafeRight then 8.0 else 0)
                + (if input.strafeLeft then -8.0 else 0)
    view := { view with
      angle := view.angle + turn
      x := view.x + move * Float.cos view.angle + strafe * Float.sin view.angle
      y := view.y + move * Float.sin view.angle - strafe * Float.cos view.angle }
    let sec := lvl.sectors[lvl.sectorAt view.x view.y]!
    view := { view with
      height := lvl.eyeZ view.x view.y sec.floorH Player.viewHeight }
    Shell.present (Render.render lvl assets view staticMobjs)
  Shell.shutdown
  return 0

def slotFile (i : Nat) : String := s!"dill-save-{i + 1}.txt"

/-- "SLOT 1 - E1M3" or "SLOT 1 - EMPTY", from the save file's map line. -/
def slotLabel (i : Nat) : IO String := do
  match (← try (some <$> IO.FS.readFile (slotFile i)) catch _ => pure none) with
  | some text =>
    let mapName := (text.splitOn "\n").findSome? fun l =>
      match l.splitOn " " with
      | ["map", m] => some m
      | _ => none
    return s!"SLOT {i + 1} - {mapName.getD "SAVED"}"
  | none => return s!"SLOT {i + 1} - EMPTY"

/-- Doom's melt wipe between screens (`Dill/Render/Wipe.lean`): the columns
of `old` fall away over `new` until the new screen is all that is left.

Vanilla runs a wipe as a blocking animation, so this does too: the
simulation does not advance, and the only input that gets through is
closing the window. The melt itself steps on the same fixed 35 Hz clock as
the game — and with the same 10-tic catch-up clamp — so it takes the same
~1.2 s however fast frames come. Returns the advanced wipe dice and whether
the window was closed. -/
def runWipe (assets : Assets) (rng : Rng) (old new : ByteArray) (pal : Nat) :
    IO (Rng × Bool) := do
  let (w0, rng) := Render.Wipe.init rng
  let mut w := w0
  let mut tics := 0
  let start ← Shell.ticks
  let mut quit := false
  while !w.done do
    if (Input.decode (← Shell.poll)).quit then
      quit := true
      break
    -- tic 0 composes to the old screen exactly, so the melt begins on the
    -- very frame the player was already looking at
    Shell.present (Render.toRGBA assets (w.compose old new) pal)
    let target := ((← Shell.ticks) - start).toNat * 35 / 1000
    let capped := min target (tics + 10)
    while tics < capped && !w.done do
      w := w.step
      tics := tics + 1
  return (rng, quit)

/-- The game: simulate at Doom's fixed 35 Hz regardless of frame rate.

The loop is deliberately thin. Each frame it polls, hands the input to the
pure `Ui.step`, performs whatever effects that asked for, advances the
simulation on its own accumulator, composes a frame with the pure
`composeFrame`, and presents it. Everything that decides *what* happens is
in those two pure functions; what is left here is the IO. -/
def cmdPlay (path : String) (mapArg : Option String) (classic fit : Bool)
    (pwads : List String := []) : IO UInt32 := do
  let wad ← loadWad path pwads
  -- With no map named, start where this IWAD actually begins: Doom and its
  -- episode PWADs at E1M1, Doom II and Final Doom at MAP01.
  let firstMap := if (wad.find? "E1M1").isSome then "E1M1" else "MAP01"
  let mapName := mapArg.getD firstMap
  -- Which episodes this IWAD holds, by probing for each `ExM1` marker. The
  -- picker is offered only when there is a real choice to make, and only
  -- when no map was named on the command line — an explicit map is already
  -- a choice, so honor it rather than overriding it from the menu.
  let episodes := (Array.range 9).map (· + 1)
    |>.filter fun e => (wad.find? s!"E{e}M1").isSome
  let episodeItems := episodes.map episodeLabel
  let pickEpisode := mapArg.isNone && episodes.size > 1
  let mut assets ← orDie "assets" (Assets.load wad)
  let mut lvl ← orDie mapName (Level.load wad mapName)
  let mut g := GameState.newGame lvl
  Shell.init 960 720 s!"dill — {mapName}"
  Shell.setAspect (if classic then 1 else 0)
  loadSounds wad
  playMusic wad "INTRO"
  -- an optional overlay: overlay.png in the working directory, composited
  -- 1:1 at the display's native resolution over every frame (see the shell)
  if (← Shell.setOverlay "overlay.png") != 0 then
    IO.println "loaded overlay.png"
  -- an optional custom title graphic: dill_logo.png fills the whole
  -- widescreen logo/menu screen (imported into the 426×200 palette frame)
  if let some rgba ← Shell.decodePng "dill_logo.png" 426 200 then
    -- `ofRGBA` refuses a buffer the shell decoded short (or a zero size);
    -- a bad logo just means the stock title, not a crash
    if let some logo := Picture.ofRGBA assets.palette rgba 426 200 then
      assets := { assets with graphics := assets.graphics.insert "DILLLOGO" logo }
      IO.println "loaded dill_logo.png"
  -- everything fixed for the run; `assets` is settled by now
  let sess : Session :=
    { wad, assets, episodes, episodeItems, pickEpisode, fit }
  let mut ui : Ui := { startMap := mapName }

  -- The tic accumulator's anchor: the clock reading at the last realign,
  -- paired with the simulation tics counted by then. `Shell.ticks` is a
  -- 32-bit millisecond clock; the pair form matters because the obvious
  -- single-value anchor — a t0 extrapolated back to a virtual tic 0 as
  -- `now - g.tics * 1000 / 35` — stops fitting 32 bits once a session's
  -- tics exceed ~2^32 ms worth (~49.7 days), after which no realign could
  -- ever let `target` reach `g.tics` again and the simulation would freeze
  -- for good. Anchoring at the *current* position keeps every quantity
  -- small; the wrap of the clock itself is handled in the playing branch.
  let mut anchor : UInt32 × Nat := (← Shell.ticks, 0)
  -- Input the *simulation* consumes, held until a tic actually runs. A frame
  -- often owes no tic at all (60 Hz frames against a 35 Hz sim), and a tap
  -- landing on such a frame would otherwise be dropped on the floor. The
  -- mouse has always been accumulated this way; `use`, `fire`, and the
  -- weapon keys need it for the same reason.
  let mut pendingMouse : Int := 0
  let mut pendingUse := false
  -- Weapon presses are a FIFO, not a latch: the shell hands out one press
  -- per poll in the order typed (see its weapon queue), and several polls
  -- can elapse before a tic is owed, so a last-wins latch would lose the
  -- intermediate presses. Each tic consumes exactly one entry.
  let mut pendingWeapon : Array Nat := #[]
  -- Fire gets the same latch, but ORed into the live snapshot rather than
  -- replacing it: fire is a *held* input (a held trigger keeps shooting), so
  -- the latch only exists to keep a click shorter than one frame from
  -- vanishing when that frame owes no tic.
  let mut pendingFire := false
  -- the melt wipe: the palette frame last put on screen, a flag set by the
  -- screen changes that wipe, and the dice that scramble the column delays
  -- (kept out of `GameState.rng` so a wipe cannot perturb the simulation)
  let mut lastShown := blankFrame
  let mut wipeNext := false
  let mut wipeRng : Rng := {}
  -- the letterbox setting the last presented frame went out under. A melt
  -- has to keep it: the falling image was drawn at that scale, and switching
  -- mid-melt would resize it as it fell. Starts letterboxed, like the title
  -- screen the loop opens on.
  let mut lastFit : UInt32 := 1
  let realign := fun (g : GameState) (now : UInt32) => (now, g.tics)
  -- read the four slot labels off disk
  let readSlots : IO (Array String) := do
    let mut out := #[]
    for i in [0:saveSlots] do out := out.push (← slotLabel i)
    return out

  repeat
    let input := Input.decode (← Shell.poll)
    if input.quit then break
    -- letters and digits typed since the last frame, for the cheat scanner
    let mut typed := ""
    let mut q ← Shell.typed
    while q != 0 do
      typed := typed.push (Char.ofNat (q &&& 0xFF).toNat)
      q := q >>> 8

    let wasPlaying := ui.mode == .playing
    let (ui', g', effects) := ui.step sess input typed g
    ui := ui'
    g := g'

    let mut quitting := false
    for e in effects do
      match e with
      | .quit => quitting := true
      | .readSlots => ui := { ui with slots := ← readSlots }
      | .saveSlot i =>
        -- A save that cannot be written is a bad afternoon, not a crash: an
        -- uncaught exception here unwinds straight out of the loop, so the
        -- window never gets torn down and the run is lost along with the
        -- save. Report it on the HUD and carry on, as the load path does.
        try
          IO.FS.writeFile (slotFile i) (Save.saveGame g)
        catch e =>
          IO.eprintln s!"save: {e}"
          ui := { ui with message := "SAVE FAILED", messageFrames := 100 }
      | .loadSlot i =>
        match (← try (some <$> IO.FS.readFile (slotFile i))
                 catch _ => pure none) with
        | none => pure ()
        | some text =>
          match Save.loadGame wad text with
          | .error err =>
            -- as the `Level.load` arm below: say so on screen rather than
            -- only on a stderr the player is not looking at. The mode is
            -- left alone, so a failed load leaves the slot list up.
            IO.eprintln s!"load: {err}"
            ui := { ui with message := "LOAD FAILED", messageFrames := 100 }
          | .ok loaded =>
            -- Reload the map and its music too: `lvl` is what a death
            -- restarts into, so a load across maps would otherwise drop you
            -- back on the one you left. `Save.loadGame` has just decoded
            -- this very map out of this very WAD, so the second read cannot
            -- realistically fail — but it is read as a value rather than
            -- thrown through `orDie`, because an exception here would unwind
            -- clean out of the loop with the window still up and the run
            -- lost. That is the same hazard the save arm above guards
            -- against, and the state is only committed once both halves
            -- have succeeded, so a failure leaves the game in progress
            -- exactly as it was.
            match Level.load wad loaded.level.name with
            | .error err =>
              IO.eprintln s!"load: {loaded.level.name}: {err}"
              ui := { ui with message := "LOAD FAILED", messageFrames := 100 }
            | .ok next =>
              g := loaded
              lvl := next
              playMusic wad g.level.name
              anchor := realign g (← Shell.ticks)
              wipeNext := true
              ui := { ui with inGame := true, mode := .playing }
      -- The three ways a map begins are one operation — load it, start a
      -- game, cue its music, realign the clock, melt into it — differing
      -- only in what carries across and what a failure means.
      | .newGame .. | .nextMap .. | .warp .. =>
        -- What differs between them: what carries across, and whether a
        -- failure is the player's mistyped warp or a broken game. Every
        -- other effect is spelled out rather than swept into a wildcard, so
        -- a new one added to `UiEffect` fails to compile here instead of
        -- quietly becoming a warp to nowhere.
        let (map, carry, skill, warping) := match e with
          | .newGame map skill => (map, none, skill, false)
          | .nextMap map => (map, some g.status, g.skill, false)
          | .warp map => (map, none, g.skill, true)
          | .toTitle | .saveSlot _ | .loadSlot _ | .readSlots | .quit =>
            ("", none, g.skill, false)
        match Level.load wad map with
        | .error err =>
          -- a mistyped warp is the player's slip, not a broken game: say so
          -- on the HUD and stay put. Anything else has dropped us out of the
          -- level we were in, so fall back to the title.
          -- with its own `messageFrames`: leaning on the one the cheat's
          -- own "CHANGING LEVEL TO …" happened to set this same frame is a
          -- coupling that would rot the moment a warp came from anywhere else
          if warping then
            ui := { ui with message := s!"NO MAP {map}", messageFrames := 100 }
          else
            IO.eprintln s!"{map}: {err}"
            ui := { ui with mode := .title }
        | .ok next =>
          lvl := next
          g := GameState.newGame next carry skill
          playMusic wad map
          -- a fresh game starts at tic 0: realign the accumulator so the
          -- first frame doesn't burst-tick to the menu's wall-clock target
          anchor := realign g (← Shell.ticks)
          wipeNext := true
          ui := { ui with inGame := true, mode := .playing }
      | .toTitle =>
        playMusic wad "INTRO"
        ui := { ui with inGame := false, mode := .title }
    if quitting then break
    -- after the effects, since loading a slot is another way in
    let enteredPlay := ui.mode == .playing && !wasPlaying

    pendingMouse := pendingMouse + input.mouseDx
    pendingUse := pendingUse || input.use
    -- A weapon key merely *held* reports every poll (that persistence is
    -- what lets a switch queued mid-attack land when the gun comes free),
    -- so push only when the press differs from the newest entry: the hold
    -- re-queues once the FIFO drains instead of flooding it. The size cap
    -- mirrors the shell's 8-deep queue — past that the backlog is stale.
    if let some w := input.weapon then
      if pendingWeapon.back? != some w && pendingWeapon.size < 8 then
        pendingWeapon := pendingWeapon.push w
    pendingFire := pendingFire || input.fire
    -- Coming back into the game must not carry the keypress that got us
    -- here into the simulation: confirming a menu item with the spacebar
    -- would otherwise reach the first tic as a `use` and open whatever the
    -- player happens to be facing. The melt clears these for the
    -- transitions that wipe; this covers the ones that do not — the save
    -- menu, and Esc back into a game already in progress.
    if enteredPlay then
      pendingMouse := 0
      pendingUse := false
      pendingWeapon := #[]
      pendingFire := false
    let now ← Shell.ticks
    if ui.mode == .playing then
      -- Catch-up is capped at 10 tics (~286 ms of simulation) per frame. A
      -- 60 Hz frame owes ~3 tics, so the cap absorbs a dropped frame or two
      -- transparently; anything longer — a suspend, a debugger stop, a
      -- machine too slow to tick at 35 Hz — discards its debt by realigning
      -- the anchor instead of freezing to replay hours of simulation (the
      -- classic fixed-timestep spiral of death).
      let (t0, ticsAt0) := anchor
      let target := ticsAt0 + ((now - t0).toNat * 35) / 1000
      -- The 32-bit millisecond clock wraps at 2^32 ms (~49.7 days). The
      -- modular UInt32 subtraction absorbs a wrap of the clock itself, but
      -- one continuous play session never realigns, so after a whole wrap
      -- period the *elapsed* count wraps too and `target` lands behind
      -- `g.tics` — impossible otherwise, since tics never outrun the target
      -- that admitted them. Realign rather than freeze waiting for a target
      -- that could not catch up for another 49.7 days.
      if target < g.tics then
        anchor := realign g now
      else
        let capped := min target (g.tics + 10)
        let mut first := true
        while g.tics < capped do
          -- the held-until-consumed inputs go to the first tic of the
          -- frame — except weapon presses, which drain one per tic so each
          -- queued press gets a tic of its own, in the order typed
          let tickInput := if first then
              { input with mouseDx := pendingMouse, use := pendingUse
                           weapon := pendingWeapon[0]?
                           fire := input.fire || pendingFire }
            else { input with mouseDx := 0, weapon := pendingWeapon[0]? }
          g := tick tickInput g
          pendingWeapon := pendingWeapon.extract 1 pendingWeapon.size
          if first then
            pendingMouse := 0
            pendingUse := false
            pendingFire := false
          first := false
        if capped < target then
          anchor := realign g now
    else
      anchor := realign g now
      pendingMouse := 0
      pendingUse := false
      pendingWeapon := #[]
      pendingFire := false
    -- play this frame's sounds, distance-faded (silent past 1200 units)
    for (sfx, sx, sy) in g.sounds do
      let dist := Float.sqrt ((g.player.x - sx)^2 + (g.player.y - sy)^2)
      let gain := if dist ≤ 200 then 1.0
                  else max 0.0 (1.0 - (dist - 200) / 1000)
      if gain > 0 then
        Shell.soundPlay (UInt32.ofNat sfx) gain
    g := { g with sounds := #[] }
    -- the exit leads to the tally screen
    if ui.mode == .playing && g.exited then
      ui := { ui with wi := intermissionView g, mode := .intermission }
      g := { g with exited := false }
      playMusic wad "INTER"
      wipeNext := true
    -- death: sink the camera, hold the red, wait for use to restart
    if ui.mode == .playing && g.status.dead && input.use then
      g := GameState.newGame lvl none g.skill
      anchor := realign g (← Shell.ticks)
      wipeNext := true

    let frame := composeFrame sess ui g
    let pal := if ui.mode == .playing then flashPalette g.status else 0
    -- letterbox only the pre-game title/logo screen so the whole logo
    -- shows; once in a game (incl. the Esc menu over it) fill the screen.
    -- `--fit` keeps the letterbox on throughout, so the full 16:9 width
    -- survives on a display too narrow to cover without cropping it away.
    let wantFit : UInt32 := if ui.inGame && !fit then 0 else 1
    -- A screen change melts: the frame the player was last looking at falls
    -- away column by column over the first frame of the new screen. Only
    -- changes of *screen* wipe — a menu opening over the game does not,
    -- exactly as vanilla. The melt eats wall-clock time with the simulation
    -- frozen, so the accumulator is realigned after it, like every other
    -- transition here.
    if wipeNext then
      wipeNext := false
      -- Melt under the *outgoing* screen's letterbox. Leaving the title
      -- screen for a game changes it (the logo screen letterboxes so a
      -- full-width `dill_logo.png` survives; the game fills the display),
      -- and switching before the melt scaled the falling logo up as it
      -- fell — it read as a zoom. The new scale takes over below, once the
      -- old screen has dropped off the bottom entirely.
      Shell.setFit lastFit
      let (rng, quit) ← runWipe assets wipeRng lastShown frame pal
      wipeRng := rng
      if quit then break
      anchor := realign g (← Shell.ticks)
      -- a melt swallows about a second of wall clock; anything latched
      -- before it belonged to the screen that just fell away, and firing it
      -- on the new one would open a door — or loose a shot on the first tic
      -- of a new life — that the player never asked for
      pendingMouse := 0
      pendingUse := false
      pendingWeapon := #[]
      pendingFire := false
    Shell.setFit wantFit
    lastFit := wantFit
    lastShown := frame
    if ui.showMap && ui.mode == .playing then
      -- the automap draws over the live 3D view: antialiased linedefs,
      -- north up, with the world still visible underneath
      let mapView : Render.MapView :=
        { x := g.player.x, y := g.player.y, angle := g.player.angle
          revealAll := g.status.ownsMap }
      let world := Render.toRGBA assets frame pal
      let base := Render.automap g.level mapView g.seen (over := some world)
      -- Keep the *status* readout on top by baking it over a black palette
      -- frame and compositing that above the lines — but not the weapon.
      -- The weapon sprite is already in `frame`, so the map draws over it
      -- and the gun sits under the lines where it belongs; painting it again
      -- here would put the barrel back on top and hide a quarter of the map
      -- behind it. Health, armour, ammo and the face still go over the top,
      -- which is what you want to be able to read while the map is up.
      -- (`flash` is structurally gated on `weapon`, but say so explicitly.)
      let (hudFrame, hudMask) := Render.withFrameMask blankFrame
        (Render.drawHud assets { hudInfo g false
          (if ui.messageFrames > 0 then ui.message else "") with
            weapon := none, flash := none })
      Shell.present (Render.compositeHud assets base hudFrame hudMask)
    else
      Shell.present (Render.toRGBA assets frame pal)
  Shell.shutdown
  return 0


/-- Render-speed check: draw `n` frames sweeping the view angle. -/
def cmdBench (path mapName : String) (n : Nat) (pwads : List String := []) : IO UInt32 := do
  let wad ← loadWad path pwads
  let lvl ← orDie mapName (Level.load wad mapName)
  let assets ← orDie "assets" (Assets.load wad)
  let base := startView lvl
  let mobjs := renderMobjs (GameState.start lvl)
  let t0 ← IO.monoMsNow
  let mut sink : UInt64 := 0
  for i in [0:n] do
    let view := { base with angle := base.angle + Float.ofNat i * 0.02 }
    let frame := Render.render lvl assets view mobjs
    sink := sink + (frame.get! 0).toUInt64
  let t1 ← IO.monoMsNow
  IO.println s!"{n} frames in {t1 - t0} ms \
    ({(Float.ofNat (t1 - t0)) / Float.ofNat n} ms/frame, checksum {sink})"
  return 0

/-- Shell smoke test: animate a gradient until quit (or `frames` frames). -/
def cmdPattern (frames : Option Nat) : IO UInt32 := do
  Shell.init 960 720 "dill — test pattern"
  let mut t : Nat := 0
  repeat
    let input := Input.decode (← Shell.poll)
    if input.quit || frames.any (t / 2 ≥ ·) then break
    let mut frame := ByteArray.emptyWithCapacity (Render.screenW * Render.screenH * 4)
    for y in [0:Render.screenH] do
      for x in [0:Render.screenW] do
        frame := frame.push (UInt8.ofNat ((x + t) % 256))
        frame := frame.push (UInt8.ofNat ((y + t / 2) % 256))
        frame := frame.push (UInt8.ofNat ((x + y) % 256))
        frame := frame.push 255
    Shell.present frame
    t := t + 2
  Shell.shutdown
  return 0

/-- The recognized subcommand names. A first argument matching one of these
with the wrong arity is an arity mistake, not a WAD path. -/
def commandNames : List String :=
  ["info", "map", "ppm", "view", "fly", "bench", "music", "pattern"]

def run (rawArgs : List String) : IO UInt32 := do
  -- `-file a.wad -file b.wad` loads PWADs over the IWAD, Doom-style
  let (pwads, args) := splitPwads rawArgs
  -- a `-file` left over took no argument (see `splitPwads`); say so, rather
  -- than let the bare flag fall through and be read as a map name
  if args.contains "-file" then
    IO.eprintln "dill: -file needs a WAD path after it"
    IO.eprintln usage
    return 2
  match args with
  | ["info", path] =>
    cmdInfo path pwads
    return 0
  | ["pattern"] =>
    cmdPattern none
  | ["pattern", n] =>
    match n.toNat? with
    | some k => cmdPattern (some k)
    | none =>
      IO.eprintln s!"pattern: frame count '{n}' is not a number"
      IO.eprintln usage
      return 2
  | ["view", path, mapName, out] =>
    cmdView path mapName out none pwads
    return 0
  | ["view", path, mapName, out, x, y, deg] =>
    match x.toInt?, y.toInt?, deg.toInt? with
    | some x, some y, some d =>
      cmdView path mapName out (some (Float.ofInt x, Float.ofInt y, Float.ofInt d)) pwads
      return 0
    | _, _, _ =>
      IO.eprintln "view: x, y, and angle must be integers"
      return 2
  | ["bench", path, mapName, n] =>
    match n.toNat? with
    | some k => cmdBench path mapName k pwads
    | none =>
      IO.eprintln s!"bench: frame count '{n}' is not a number"
      IO.eprintln usage
      return 2
  | ["fly", path] =>
    cmdFly path "E1M1" pwads
  | ["fly", path, mapName] =>
    cmdFly path mapName pwads
  | ["map", path, mapName] =>
    cmdMap path mapName pwads
    return 0
  | ["ppm", path, name, out] =>
    cmdPpm path name out pwads
  | ["music", path, mapName, out] =>
    cmdMusic path mapName out pwads
  | args =>
    let classic := args.contains "--classic"
    let fit := args.contains "--fit"
    let flags := ["--classic", "--fit"]
    match args.filter (!flags.contains ·) with
    | path :: rest =>
      -- A known subcommand landing here got the wrong number of arguments;
      -- don't misread it as a WAD path.
      if commandNames.contains path then
        IO.eprintln s!"dill: wrong arguments for '{path}'"
        IO.eprintln usage
        return 2
      -- A first argument that names no file and doesn't even look like a
      -- WAD is far more likely a misspelled subcommand than a game file.
      if !(← System.FilePath.pathExists ⟨path⟩)
          && !(path.toLower.endsWith ".wad") then
        IO.eprintln s!"dill: unknown command or missing file '{path}'"
        IO.eprintln usage
        return 2
      match rest with
      | [] => cmdPlay path none classic fit pwads
      | [mapName] => cmdPlay path (some mapName) classic fit pwads
      | _ =>
        IO.eprintln usage
        return 2
    | [] =>
      IO.eprintln usage
      return 2

def main (rawArgs : List String) : IO UInt32 := do
  try
    run rawArgs
  catch e =>
    -- one friendly line naming the problem, not a raw uncaught exception
    IO.eprintln s!"dill: {e}"
    -- An exception from a windowed command unwinds past that command's own
    -- `Shell.shutdown`, so the window, the Vulkan device and the audio
    -- streams would otherwise outlive the error and be reclaimed only by
    -- process exit — on macOS that leaves a dead fullscreen window on
    -- screen while the message is printed behind it. `dill_shutdown` tears
    -- down only what was actually brought up and is safe to call twice, so
    -- the non-graphical commands (`info`, `map`, `ppm`, …) can go through
    -- it too rather than needing to know whether they opened anything.
    try Shell.shutdown catch _ => pure ()
    return 1
