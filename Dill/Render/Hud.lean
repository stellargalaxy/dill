import Dill.Maps
import Dill.Render.Frame

/-!
# The heads-up display

Drawn straight onto the palette frame after the world: the weapon sprite
at the bottom (with its muzzle flash), and the classic red status digits —
ammo, health, armor — in the corners, like the fullscreen HUDs of source
ports. No status-bar bezel; the whole 426×200 stays world. The vanilla
320-wide layout is centred in it by `hudX`.
-/

namespace Dill.Render

/-- What the game wants shown this frame. -/
structure HudInfo where
  health : Int
  armor  : Int
  ammo   : Option Nat        -- none for the fist
  /-- Weapon sprite lump and its vanilla psprite position (sx, sy). -/
  weapon : Option (String × Float × Float) := none
  /-- Muzzle-flash lump, drawn over the weapon at full brightness. -/
  flash  : Option String := none
  /-- Sector light at the player, so the gun dims in the dark. -/
  light  : Nat := 255
  /-- Screen-wide colormap override for the weapon (goggles/invuln). -/
  fixedColormap : Option Nat := none
  /-- Partial invisibility: draw the weapon (and flash) as fuzz, phased by
  this tic count. `none` = draw solid. Vanilla's `MF_SHADOW` psprite. -/
  weaponFuzz : Option Nat := none
  /-- Keys held: (blue, yellow, red) — drawn as STKEYS card icons. -/
  keys   : Bool × Bool × Bool := (false, false, false)
  paused : Bool := false
  /-- Transient message (cheats, pickups) at the top of the screen. -/
  message : String := ""
  /-- The marine's face graphic (`STF*`), shown left of the health. -/
  face   : Option String := none
  deriving Inhabited

/-- Blit a picture (with transparency) at scale 1, colormap row `cm`. When
`fuzz` is set the picture is a mask: each of its pixels shows the darkened
background a fuzz-table offset away (vanilla `R_DrawFuzzColumn`), so an
invisible weapon shimmers instead of showing its art. -/
def drawPic (assets : Assets) (pic : Picture) (x0 y0 : Int) (cm : Nat)
    (fuzz : Bool := false) (fuzzPhase : Nat := 0) : RenderM Unit := do
  let cmBase := cm * 256
  let mut fpos := (← get).fuzzPos
  let mut frame ← takeFrame
  -- coverage is recorded only when a caller seeded a buffer (see
  -- `DrawState.cover`); `track` keeps the ordinary path to one Bool test
  let mut cover ← takeCover
  let track := !cover.isEmpty
  for px in [0:pic.width] do
    let x := x0 + Int.ofNat px
    if 0 ≤ x && x < Int.ofNat screenW then
      for py in [0:pic.height] do
        let y := y0 + Int.ofNat py
        if 0 ≤ y && y < Int.ofNat screenH && pic.opaqueAt px py then
          if fuzz then
            frame := fuzzPixel assets frame x.toNat y.toNat (fpos + fuzzPhase)
            fpos := fpos + 1
          else
            let texel := (pic.get px py).toNat
            frame := frame.set! (y.toNat * screenW + x.toNat)
              (assets.colormap.get! (cmBase + texel))
          if track then
            cover := cover.set! (y.toNat * screenW + x.toNat) 1
  putFrame frame
  putCover cover
  if fuzz then
    let fposNow := fpos
    modify fun s => { s with fuzzPos := fposNow }

/-- Draw a psprite lump at vanilla's screen position: the lump's own
offsets place it relative to `(sx, sy)`. -/
private def drawPSprite (assets : Assets) (lump : String) (sx sy : Float)
    (cm : Nat) (fuzz : Bool := false) (fuzzPhase : Nat := 0) : RenderM Unit := do
  let some pic := assets.sprites.get? lump | return
  drawPic assets pic (hudX (ifloor (sx - Float.ofInt pic.leftOffset)))
    (ifloor (sy - Float.ofInt pic.topOffset)) cm fuzz fuzzPhase

/-- Right-aligned number in the big red status font, ending at `xRight`. -/
private def drawNumber (assets : Assets) (n : Nat) (xRight y : Int) :
    RenderM Unit := do
  let digits := if n == 0 then [0] else Id.run do
    let mut v := n
    let mut out : List Nat := []
    while v > 0 do
      out := v % 10 :: out
      v := v / 10
    return out
  let mut x := xRight
  for d in digits.reverse do
    let some pic := assets.graphics.get? s!"STTNUM{d}" | return
    x := x - Int.ofNat pic.width
    drawPic assets pic x y 0
  return

/-- Doom's small text font (`STCFN*`): uppercase-only, per-glyph widths. -/
def drawText (assets : Assets) (s : String) (x0 y : Int) : RenderM Unit := do
  let mut x := x0
  for c in s.toList do
    let code := c.toUpper.toNat
    if code == 32 then
      x := x + 4
    else if 33 ≤ code && code ≤ 95 then
      if let some pic := assets.graphics.get? s!"STCFN{(1000 + code).repr.drop 1}" then
        drawPic assets pic x y 0
        x := x + Int.ofNat pic.width + 1

/-- What a menu screen shows; drawn over the (frozen) game or the title. -/
structure MenuView where
  titleScreen : Bool := false
  logo        : Bool := false
  header      : String := ""
  items       : Array String := #[]
  /-- Optional big-graphic lump per item (vanilla's M_NGAME etc.). -/
  itemLumps   : Array String := #[]
  selected    : Nat := 0
  /-- Blink phase for the skull cursor. -/
  skullAlt    : Bool := false
  /-- A transient line under the list — how a failed save or load reports
  itself. The HUD's own message line is drawn as part of the world, which a
  menu reached from the title screen does not have. -/
  message     : String := ""
  deriving Inhabited

/-- Draw a menu: title backdrop and/or logo, items, skull cursor.
Vanilla's layout: the logo at (94, 2), items from y = 72. -/
def drawMenu (assets : Assets) (m : MenuView) : RenderM Unit := do
  if m.titleScreen then
    -- a custom dill_logo fills the whole widescreen frame; else the
    -- classic TITLEPIC sits in the 320-wide 4:3 band
    match assets.graphics.get? "DILLLOGO" with
    | some pic => drawPic assets pic 0 0 0
    | none =>
      if let some pic := assets.graphics.get? "TITLEPIC" then
        drawPic assets pic (hudX 0) 0 0
  -- the DOOM wordmark, unless a custom logo already carries its own title
  if m.logo && !assets.graphics.contains "DILLLOGO" then
    if let some pic := assets.graphics.get? "M_DOOM" then
      drawPic assets pic (hudX 94) 2 0
  if m.header != "" then
    drawText assets m.header (hudX 110) 60
  let mut y : Int := 72
  for i in [0:m.items.size] do
    let asGraphic := Id.run do
      let some lumpName := m.itemLumps[i]? | return none
      assets.graphics.get? lumpName
    match asGraphic with
    | some pic => drawPic assets pic (hudX 97) y 0
    | none => drawText assets m.items[i]! (hudX 97) (y + 2)
    if i == m.selected then
      let skull := if m.skullAlt then "M_SKULL2" else "M_SKULL1"
      if let some pic := assets.graphics.get? skull then
        drawPic assets pic (hudX 66) (y - 5) 0
    y := y + 16
  if m.message != "" then
    drawText assets m.message (hudX 66) (y + 8)

/-- What the tally screen between maps shows. -/
structure WiView where
  /-- Tally graphics, already resolved to lump names by `Dill.MapId` — the
  naming differs between Doom 1 and Doom II, and that belongs there. -/
  backPic     : String
  finishedPic : String
  enteringPic : Option String
  killPct     : Nat
  itemPct     : Nat
  secretPct   : Nat
  /-- How long the map took, in seconds, and the par it is measured against
  (0 where the game sets none, in which case the par line is left off). -/
  levelTime   : Nat := 0
  parTime     : Nat := 0
  deriving Inhabited

/-- The intermission: episode map backdrop, "<name> FINISHED", the
percentages, and what's next. -/
def drawIntermission (assets : Assets) (wi : WiView) : RenderM Unit := do
  if let some pic := assets.graphics.get? wi.backPic then
    -- Centre by the backdrop's own width. `hudX 0` would assume 320, but
    -- these vary: Doom's WIMAP* are 320 wide while the re-release ships a
    -- 560-wide INTERPIC, which that assumption shoves off to the right.
    drawPic assets pic ((Int.ofNat screenW - Int.ofNat pic.width) / 2) 0 0
  if let some pic := assets.graphics.get? wi.finishedPic then
    drawPic assets pic (hudX (160 - Int.ofNat pic.width / 2)) 6 0
  if let some pic := assets.graphics.get? "WIF" then
    drawPic assets pic (hudX (160 - Int.ofNat pic.width / 2)) 26 0
  drawText assets s!"KILLS   {wi.killPct}%" (hudX 108) 70
  drawText assets s!"ITEMS   {wi.itemPct}%" (hudX 108) 86
  drawText assets s!"SECRETS {wi.secretPct}%" (hudX 108) 102
  -- your time against the map's par, as vanilla shows below the tally
  let clock := fun (secs : Nat) =>
    let m := secs / 60
    let s := secs % 60
    s!"{m}:{if s < 10 then "0" else ""}{s}"
  drawText assets s!"TIME    {clock wi.levelTime}" (hudX 108) 118
  if wi.parTime > 0 then
    drawText assets s!"PAR     {clock wi.parTime}" (hudX 108) 128
  match wi.enteringPic with
  | some nextPic =>
    if let some pic := assets.graphics.get? "WIENTER" then
      drawPic assets pic (hudX (160 - Int.ofNat pic.width / 2)) 140 0
    if let some pic := assets.graphics.get? nextPic then
      drawPic assets pic (hudX (160 - Int.ofNat pic.width / 2)) 160 0
  | none =>
    drawText assets "EPISODE COMPLETE" (hudX 104) 150
  drawText assets "PRESS USE" (hudX 130) 184

/-- Run extra drawing over an already-rendered palette frame. -/
def withFrame (frame : ByteArray) (act : RenderM Unit) : ByteArray :=
  ((act.run { DrawState.init with frame }).2).frame

/-- Like `withFrame`, but also report which pixels the drawing actually
touched: returns the frame and a screen-sized coverage mask (1 = drawn).
This is what a compositor over the automap needs instead of treating some
palette index as transparent — genuinely drawn HUD texels use index 0 (the
black outlines of the STTNUM digits and STCFN glyphs), so only coverage can
tell "drawn black" from "never drawn". -/
def withFrameMask (frame : ByteArray) (act : RenderM Unit) :
    ByteArray × ByteArray :=
  let s := (act.run { DrawState.init with
    frame := frame
    cover := ByteArray.mk (Array.replicate (screenW * screenH) 0) }).2
  (s.frame, s.cover)

/-- Paint the whole HUD over the finished world frame. -/
def drawHud (assets : Assets) (hud : HudInfo) : RenderM Unit := do
  let cm := lightColormap hud.light 0.02 (fixed := hud.fixedColormap)
  let (fuzz, phase) := match hud.weaponFuzz with
    | some p => (true, p)
    | none => (false, 0)
  if let some (lump, sx, sy) := hud.weapon then
    drawPSprite assets lump sx sy cm fuzz phase
  if let some lump := hud.flash then
    if let some (_, sx, sy) := hud.weapon then
      -- the flash is fullbright (colormap row 0) — unless a fixed colormap
      -- is active: vanilla's `fixedcolormap` overrides every psprite, so
      -- invulnerability inverse-maps the flash along with the weapon
      drawPSprite assets lump sx sy (hud.fixedColormap.getD 0) fuzz phase
  -- the marine's face at bottom-left, then health% to its right,
  -- armor% beside that, ammo bottom-right
  let y : Int := 183
  if let some faceLump := hud.face then
    if let some pic := assets.graphics.get? faceLump then
      -- bottom-aligned so the taller "ouch"/"god" faces don't jump
      drawPic assets pic (hudX 8) (198 - Int.ofNat pic.height) 0
  drawNumber assets (max 0 hud.health).toNat (hudX 110) y
  if let some pct := assets.graphics.get? "STTPRCNT" then
    drawPic assets pct (hudX 110) y 0
  drawNumber assets (max 0 hud.armor).toNat (hudX 210) y
  if let some pct := assets.graphics.get? "STTPRCNT" then
    drawPic assets pct (hudX 210) y 0
  if let some ammo := hud.ammo then
    drawNumber assets ammo (hudX 310) y
  -- key cards stacked between armor and ammo (STKEYS0/1/2 = blue/yellow/red)
  let (blue, yellow, red) := hud.keys
  for (has, lump, ky) in [(blue, "STKEYS0", 172), (yellow, "STKEYS1", 181),
                          (red, "STKEYS2", 190)] do
    if has then
      if let some pic := assets.graphics.get? lump then
        drawPic assets pic (hudX 256) ky 0
  if hud.paused then
    if let some pic := assets.graphics.get? "M_PAUSE" then
      drawPic assets pic (hudX (160 - Int.ofNat pic.width / 2)) 20 0
  if hud.message != "" then
    drawText assets hud.message (hudX 2) 2

end Dill.Render
