import Std.Data.HashMap
import Dill.Wad

/-!
# Graphics assets

Doom draws everything through a 256-color palette (`PLAYPAL`) and dims light
by remapping colors through `COLORMAP` (map 0 = full bright, 31 = black).

Wall textures are *composited*: `TEXTURE1`/`TEXTURE2` describe how to paste
one or more *patches* (picture lumps, named by `PNAMES`) onto a canvas.
Patches store pixels in vertical *posts* with transparent gaps, which is also
exactly what sprites are. Flats (floors/ceilings) are simpler: raw 64×64
bytes between the `F_START`/`F_END` markers.

Everything decodes to `Picture`: column-major pixels plus an opacity mask,
because the renderer draws walls and sprites by column.
-/

namespace Dill

open Bytes

/-- A decoded image: palette indices, column-major (`pixels[x * height + y]`).

`mask` marks opaque texels; sprites and grate textures have holes. The
offsets position sprites relative to their world point. -/
structure Picture where
  width      : Nat
  height     : Nat
  leftOffset : Int
  topOffset  : Int
  pixels     : ByteArray
  mask       : ByteArray
  deriving Inhabited

namespace Picture

@[inline] def get (p : Picture) (x y : Nat) : UInt8 :=
  p.pixels.get! (x * p.height + y)

@[inline] def opaqueAt (p : Picture) (x y : Nat) : Bool :=
  p.mask.get! (x * p.height + y) != 0

end Picture

/-- The palette index whose RGB is nearest to `(r,g,b)` (least squares). -/
private def nearestPalette (palette : ByteArray) (r g b : Nat) : UInt8 :=
  Id.run do
    let mut best := 0
    let mut bestD := 1000000
    for i in [0:256] do
      -- Nat subtraction saturates at 0, so compute signed distance via Int
      let dr := Int.ofNat (palette.get! (i*3)).toNat - Int.ofNat r
      let dg := Int.ofNat (palette.get! (i*3+1)).toNat - Int.ofNat g
      let db := Int.ofNat (palette.get! (i*3+2)).toNat - Int.ofNat b
      let d := (dr*dr + dg*dg + db*db).toNat
      if d < bestD then
        bestD := d
        best := i
    return UInt8.ofNat best

/-- Build a `Picture` from an RGBA image by quantizing each pixel to the
nearest palette colour (pixels with alpha < 128 become transparent). Used
to import a custom PNG (e.g. a title logo) into the palettized renderer.
Images already in Doom's palette map exactly. -/
def Picture.ofRGBA (palette : ByteArray) (rgba : ByteArray) (w h : Nat) :
    Option Picture := Id.run do
  -- Validate like `Picture.parse` refuses zero-dimension pictures: the
  -- `get!`s below read `w*h*4` bytes of `rgba`, and a zero dimension makes
  -- `Picture.get`/`opaqueAt` index an empty pixel array — a panic waiting
  -- for the first draw. `none`, not a panic, for data that arrives from a
  -- file on the caller's disk.
  if w == 0 || h == 0 || rgba.size < w * h * 4 then return none
  let mut pixels := ByteArray.mk (Array.replicate (w * h) 0)
  let mut mask   := ByteArray.mk (Array.replicate (w * h) 0)
  for y in [0:h] do
    for x in [0:w] do
      let i := (y * w + x) * 4
      if rgba.get! (i+3) ≥ 128 then
        let idx := nearestPalette palette (rgba.get! i).toNat
          (rgba.get! (i+1)).toNat (rgba.get! (i+2)).toNat
        let j := x * h + y            -- column-major layout
        pixels := pixels.set! j idx
        mask   := mask.set! j 1
  return some { width := w, height := h, leftOffset := 0, topOffset := 0
                pixels, mask }

/-- Largest pixel count a picture or texture may claim: 16M texels (e.g.
8192×2048) is beyond any real asset, while a malformed header's 65535×65535
would otherwise demand two 4 GB allocations. -/
private def maxPictureArea : Nat := 1 <<< 24

/-- Decode a picture-format lump (sprites, patches, menu graphics). -/
def Picture.parse (b : ByteArray) : Except String Picture := do
  if b.size < 8 then throw "picture lump truncated"
  let width  := (u16le b 0).toNat
  let height := (u16le b 2).toNat
  let leftOffset := i16le b 4
  let topOffset  := i16le b 6
  if width * height > maxPictureArea then
    throw s!"picture claims unreasonable size {width}×{height}"
  -- A zero-dimension picture is nothing to draw, and `get!`/`opaqueAt`
  -- index `x * height + y` into its empty pixel array — a panic waiting
  -- for the first draw. Refuse it here, like `parseTextureLump` drops
  -- zero-dimension textures.
  if width == 0 || height == 0 then
    throw s!"picture claims zero size {width}×{height}"
  if b.size < 8 + width * 4 then throw "picture column table truncated"
  let mut pixels := ByteArray.mk (Array.replicate (width * height) 0)
  let mut mask   := ByteArray.mk (Array.replicate (width * height) 0)
  -- Malformed-input guard. The column table can point every column at the
  -- same long post list, making this walk O(width × lump size) on a
  -- crafted lump. Honest pictures are far smaller: distinct columns
  -- partition the lump (≤ `b.size` post bytes in all), and even a
  -- compressor that points identical columns at shared data (wadptr) can
  -- honestly consume no more than a full column each — non-overlapping
  -- posts hold ≤ `height` pixels plus 4 bytes of framing per post. Cap
  -- total consumption at the larger of the two and refuse anything beyond.
  let cap := max b.size (width * (3 * height + 4))
  let mut consumed := 0
  for x in [0:width] do
    -- Each column is a list of posts: (top, length, pad, bytes…, pad), 0xFF ends.
    let mut p := (u32le b (8 + x * 4)).toNat
    while p < b.size && b.get! p != 0xFF do
      -- the length byte must exist before it is read
      if p + 2 > b.size then throw "picture post truncated"
      let top := (b.get! p).toNat
      let len := (b.get! (p + 1)).toNat
      if p + 3 + len > b.size then throw "picture post truncated"
      consumed := consumed + 4 + len
      if consumed > cap then
        throw "picture posts consume more bytes than the lump holds"
      for dy in [0:len] do
        let y := top + dy
        if y < height then
          pixels := pixels.set! (x * height + y) (b.get! (p + 3 + dy))
          mask   := mask.set! (x * height + y) 1
      p := p + 4 + len
  return { width, height, leftOffset, topOffset, pixels, mask }

/-- One of a sprite's 8 view rotations: which lump, and whether mirrored
(the WAD stores `TROOA2A8` once and flips it for the other side). -/
structure SpriteRot where
  lump    : String
  flipped : Bool
  deriving Repr, Inhabited

/-- All decoded graphics, ready for the renderer. -/
structure Assets where
  /-- All 14 palettes of `PLAYPAL` (0 normal, 1–8 pain, 9–12 bonus, 13 suit). -/
  palettes : ByteArray
  /-- 33 light maps × 256 entries — the bound `load` enforces and all the
  renderer touches: rows 0–31 dim toward black, row 32 is the
  invulnerability inverse. (IWAD lumps carry a 34th, unused row.) -/
  colormap : ByteArray
  textures : Std.HashMap String Picture
  /-- Raw 64×64 row-major flats, keyed by lump name. -/
  flats    : Std.HashMap String ByteArray
  /-- Sprite lumps (`TROOA1`, …), keyed by lump name. -/
  sprites  : Std.HashMap String Picture
  /-- Sprite family+frame (`"TROOA"`) → its 8 rotations (all equal for
  rotation-less sprites). -/
  spriteRots : Std.HashMap String (Array SpriteRot)
  /-- Miscellaneous graphics by lump name (HUD digits, etc.). -/
  graphics : Std.HashMap String Picture

/-- Palette 0: 256 × RGB. -/
def Assets.palette (a : Assets) : ByteArray := a.palettes.extract 0 768

/-- Does this WAD carry the super shotgun at all?

Doom 1 does not: no `SHT2` weapon sprites, no `SGN2` pickup, no `DSDSHTGN`.
Nothing on a Doom 1 map places one either (thing 82 is a Doom II number), so
the only way to end up holding one there is the arsenal cheat — and what it
hands over is an invisible, silent gun that still fires. This is the gate on
that, asked of the assets rather than of the map name, because what decides
the question is whether the sprites are there, not what the level is called:
a Doom 1-style PWAD loaded over `doom2.wad` has them and should keep them. -/
def Assets.hasSuperShotgun (a : Assets) : Bool := a.sprites.contains "SHT2A0"

namespace Assets

/-- Doom's flat/texture animation groups present in the retail IWADs: each is
a cycle of frames the engine flips through (vanilla `P_InitPicAnims`). The
same machinery drives animated floors (nukage, water, lava, blood) and
animated walls (dripping slime and blood, fire, the falls). A group whose
frames a given IWAD lacks — Doom II carries almost none of Doom 1's animated
walls — is harmless dead weight: no sidedef on its maps names those
textures, so `animName` never matches, and even a PWAD naming a frame the
WAD lacks only makes the resolved name a texture-lookup miss, which every
draw site already treats as "no texture". -/
def animGroups : Array (Array String) := #[
  #["NUKAGE1", "NUKAGE2", "NUKAGE3"],
  #["FWATER1", "FWATER2", "FWATER3", "FWATER4"],
  #["LAVA1", "LAVA2", "LAVA3", "LAVA4"],
  #["BLOOD1", "BLOOD2", "BLOOD3"],
  -- Doom 1's animated walls: Knee-Deep's dripping slime, then the blood,
  -- fire, lava and rock cycles of the later episodes. Each cycle keeps the
  -- IWAD texture-directory order, which is what vanilla animates through
  -- (its animdefs entries name only the first and last frame).
  #["SLADRIP1", "SLADRIP2", "SLADRIP3"],
  #["BLODGR1", "BLODGR2", "BLODGR3", "BLODGR4"],
  #["BLODRIP1", "BLODRIP2", "BLODRIP3", "BLODRIP4"],
  #["FIREWALA", "FIREWALB", "FIREWALL"],
  #["GSTFONT1", "GSTFONT2", "GSTFONT3"],
  -- FIRELAV3 before FIRELAVA is not a typo: vanilla's entry runs from
  -- start FIRELAV3 to end FIRELAVA, and that is their directory order
  #["FIRELAV3", "FIRELAVA"],
  #["FIREMAG1", "FIREMAG2", "FIREMAG3"],
  #["FIREBLU1", "FIREBLU2"],
  #["ROCKRED1", "ROCKRED2", "ROCKRED3"],
  -- Doom II's flat animations (slime, dark rock) …
  #["RROCK05", "RROCK06", "RROCK07", "RROCK08"],
  #["SLIME01", "SLIME02", "SLIME03", "SLIME04"],
  #["SLIME05", "SLIME06", "SLIME07", "SLIME08"],
  #["SLIME09", "SLIME10", "SLIME11", "SLIME12"],
  -- …and its animated walls: blood, slime and water falls, and the Icon of
  -- Sin's brain wall
  #["BFALL1", "BFALL2", "BFALL3", "BFALL4"],
  #["SFALL1", "SFALL2", "SFALL3", "SFALL4"],
  #["WFALL1", "WFALL2", "WFALL3", "WFALL4"],
  #["DBRAIN1", "DBRAIN2", "DBRAIN3", "DBRAIN4"]]

/-- `animGroups` inverted: frame name → (group index, index within group),
built once at module init. `animName` runs for every drawn seg's three
textures and every visplane's flat, and almost always concludes "not
animated" — a linear scan of 21 groups of string compares per call was
most of that cost; one hash probe is not. -/
private def animLookup : Std.HashMap String (Nat × Nat) := Id.run do
  let mut m := ∅
  for gi in [0:animGroups.size] do
    let grp := animGroups[gi]!
    for i in [0:grp.size] do
      m := m.insert grp[i]! (gi, i)
  return m

/-- Resolve a flat or texture name to the frame showing at tic `tics`. Each
frame in a group is phase-shifted by its index, exactly like vanilla's
`texturetranslation` cycling (frames advance every 8 tics). Names that
aren't animated pass straight through. -/
def animName (name : String) (tics : Nat) : String :=
  match animLookup.get? name with
  | some (gi, i) =>
    let grp := animGroups[gi]!
    grp[(tics / 8 + i) % grp.size]!
  | none => name

/-- `PNAMES`: the global list of patch lump names that textures reference by
index. Some IWADs store these in lowercase; lump names are uppercase. -/
private def parsePnames (b : ByteArray) : Except String (Array String) := do
  if b.size < 4 then throw "PNAMES truncated"
  let count := (u32le b 0).toNat
  if b.size < 4 + count * 8 then throw "PNAMES name table truncated"
  let mut names := Array.mkEmpty count
  for k in [0:count] do
    names := names.push (name8 b (4 + k * 8)).toUpper
  return names

/-- Composite one texture from its patches onto a blank canvas. -/
private def composite (width height : Nat)
    (patches : Array (Int × Int × Picture)) : Picture := Id.run do
  let mut pixels := ByteArray.mk (Array.replicate (width * height) 0)
  let mut mask   := ByteArray.mk (Array.replicate (width * height) 0)
  for (originX, originY, patch) in patches do
    for px in [0:patch.width] do
      let x := originX + px
      if 0 ≤ x && x < width then
        for py in [0:patch.height] do
          let y := originY + py
          if 0 ≤ y && y < height && patch.opaqueAt px py then
            let i := x.toNat * height + y.toNat
            pixels := pixels.set! i (patch.get px py)
            mask   := mask.set! i 1
  return { width, height, leftOffset := 0, topOffset := 0, pixels, mask }

/-- Decode one `TEXTURE1`/`TEXTURE2` lump into `acc`. -/
private def parseTextureLump (pnames : Array String)
    (patchCache : Std.HashMap String Picture) (b : ByteArray)
    (acc : Std.HashMap String Picture) :
    Except String (Std.HashMap String Picture) := do
  if b.size < 4 then throw "texture lump truncated"
  let count := (u32le b 0).toNat
  -- the offset table must fit inside the lump (mirrors `parsePnames`);
  -- this also bounds `count`, which is otherwise a raw u32 from the file
  if b.size < 4 + count * 4 then throw "texture offset table truncated"
  let mut out := acc
  for k in [0:count] do
    let ofs := (u32le b (4 + k * 4)).toNat
    if ofs + 22 > b.size then throw "texture entry truncated"
    -- uppercased like every other name: sidedefs look textures up uppercase
    let texName := (name8 b ofs).toUpper
    let width   := (u16le b (ofs + 12)).toNat
    let height  := (u16le b (ofs + 14)).toNat
    if width * height > maxPictureArea then
      throw s!"texture {texName} claims unreasonable size {width}×{height}"
    let nPatches := (u16le b (ofs + 20)).toNat
    -- A zero-dimension texture is nothing to draw, and the column loops
    -- tile with `% tex.height` and index `tcol * tex.height` — both of
    -- which read off the end of an empty pixel array. Dropping it here
    -- makes the name a lookup miss, which every draw site already handles
    -- ("no texture on this wall"), instead of a panic mid-frame.
    if width == 0 || height == 0 then
      continue
    let mut patches := Array.mkEmpty nPatches
    for j in [0:nPatches] do
      let p := ofs + 22 + j * 10
      if p + 10 > b.size then throw s!"texture {texName}: patch list truncated"
      let originX := i16le b p
      let originY := i16le b (p + 2)
      let index   := (u16le b (p + 4)).toNat
      let some patchName := pnames[index]?
        | throw s!"texture {texName}: patch index {index} out of range"
      -- A missing patch is survivable: skip that piece rather than refuse
      -- to load the game. It happens whenever a PWAD replaces TEXTURE1
      -- wholesale and names patches its host IWAD lacks — loading a Doom 1
      -- PWAD over Doom II, say. The texture comes out incomplete, which is
      -- far better than not starting.
      if let some patch := patchCache.get? patchName then
        patches := patches.push (originX, originY, patch)
    out := out.insert texName (composite width height patches)
  return out

/-- `Picture.parse` for an *optional* lump — sprites, patches, HUD and menu
graphics, whose absence the call sites already skip. A zero-dimension
picture is refused by the parser (there is nothing to draw and its indexing
would panic), but for these lumps that refusal should degrade exactly like
the lump not being there, not refuse the whole WAD; genuinely corrupt data
(truncated posts) stays an error. -/
private def parseOptional? (b : ByteArray) : Except String (Option Picture) :=
  if b.size ≥ 8 && (u16le b 0 == 0 || u16le b 2 == 0) then pure none
  else some <$> Picture.parse b

/-- Decode every patch lump named by `PNAMES` once, up front.

Resolution scans the directory in order, later entries winning (so a PWAD's
replacement patch shadows the IWAD's, marked section or loose lump alike) —
but lumps inside the flat and sprite sections never count: those are raw
64×64 bytes and sprite pictures in *their* namespaces, and a name can recur
across namespaces meaning different things (Sunlust names both a patch and
a flat `TILE`; resolving the flat here would reject the whole WAD). -/
private def decodePatches (wad : Wad) (pnames : Array String) :
    Except String (Std.HashMap String Picture) := do
  let mut candidates : Std.HashMap String Lump := ∅
  let mut inFlats := false
  let mut inSprites := false
  for l in wad.lumps do
    match l.name with
    | "F_START" | "FF_START" => inFlats := true
    | "F_END" | "FF_END" => inFlats := false
    | "S_START" | "SS_START" => inSprites := true
    | "S_END" | "SS_END" => inSprites := false
    | _ =>
      if !inFlats && !inSprites && l.size > 0 then
        candidates := candidates.insert l.name l
  let mut cache : Std.HashMap String Picture := ∅
  for name in pnames do
    -- A few PNAMES entries are historical junk with no lump; skip them.
    if let some lump := candidates.get? name then
      if let some pic ← parseOptional? (wad.data lump) then
        cache := cache.insert name pic
  return cache

/-- Collect the lumps strictly between two marker lumps, across *every*
such section in the directory. A lone IWAD has exactly one, but merging a
PWAD appends its own `S_START`/`S_END` block — taking only the first section
would silently drop everything the PWAD adds. Later sections come last, so
callers inserting into a map get PWAD-over-IWAD shadowing for free.

Each end accepts *several* marker names, because PWADs conventionally use
doubled markers for the sections they add (`FF_START`/`FF_END` flats —
Sunlust — and `SS_START`/`SS_END` sprites), and either kind of end marker
may close either kind of start (DeuTeX emits `FF_START`…`F_END`). This is
the same namespace reading `decodePatches` does. -/
private def betweenMarkers (wad : Wad) (startMarks endMarks : Array String) :
    Except String (Array Lump) := do
  let mut out := #[]
  let mut inside := false
  let mut seen := false
  for l in wad.lumps do
    if startMarks.contains l.name then
      inside := true
      seen := true
    else if endMarks.contains l.name then
      inside := false
    else if inside then
      out := out.push l
  if !seen then
    throw s!"marker {startMarks[0]!.quote} not found"
  return out

/-- Load everything the renderer needs from an IWAD. -/
def load (wad : Wad) : Except String Assets := do
  let playpal ← wad.find "PLAYPAL"
  -- the renderer indexes palettes 0–13 and colormap rows 0–32 (invuln)
  -- without further checks, so a short lump must be refused here by name
  if playpal.size < 14 * 768 then
    throw s!"PLAYPAL: {playpal.size} bytes, need 14 palettes × 768"
  let colormap ← wad.find "COLORMAP"
  if colormap.size < 33 * 256 then
    throw s!"COLORMAP: {colormap.size} bytes, need 33 maps × 256"
  let pnames ← parsePnames (wad.data (← wad.find "PNAMES"))
  let patchCache ← decodePatches wad pnames

  let mut textures : Std.HashMap String Picture := ∅
  textures ← parseTextureLump pnames patchCache
    (wad.data (← wad.find "TEXTURE1")) textures
  if let some t2 := wad.find? "TEXTURE2" then
    textures ← parseTextureLump pnames patchCache (wad.data t2) textures

  let mut flats : Std.HashMap String ByteArray := ∅
  for lump in ← betweenMarkers wad #["F_START", "FF_START"] #["F_END", "FF_END"] do
    if lump.size == 64 * 64 then
      flats := flats.insert lump.name (wad.data lump)

  let mut sprites : Std.HashMap String Picture := ∅
  let mut spriteRots : Std.HashMap String (Array SpriteRot) := ∅
  -- Rotation `0` means "this one picture from every angle", so it fills the
  -- table — but only the slots no directional lump has already claimed. A
  -- WAD carrying both `TROOA0` and `TROOA1`–`A8` is malformed (vanilla
  -- refuses to load it at all); filling unconditionally silently threw the
  -- eight directions away whenever the `0` lump happened to come later in
  -- the directory, which is the worse of the two ways to be wrong. An
  -- untouched slot is the `default` one, whose lump name is empty.
  let put8 := fun (rots : Array SpriteRot) (rot : Nat) (r : SpriteRot) =>
    if rot == 0 then rots.map (fun old => if old.lump.isEmpty then r else old)
    else rots.set! (rot - 1) r
  for lump in ← betweenMarkers wad #["S_START", "SS_START"] #["S_END", "SS_END"] do
    if lump.size > 0 then
      let some pic ← parseOptional? (wad.data lump) | continue
      sprites := sprites.insert lump.name pic
      -- name = FFFF + frame + rotation [+ mirrored frame + rotation]
      -- A rotation char must be vanilla's '0'–'8'. Anything else — a
      -- 16-rotation PWAD sprite ('9'–'G') or a junk name in the sprite
      -- section — is skipped rather than indexing past the 8-slot table,
      -- and a sub-'0' char must not saturate to rotation 0 and silently
      -- clobber all eight slots.
      let rotOf := fun (c : Char) =>
        if '0' ≤ c && c ≤ '8' then some (c.toNat - '0'.toNat) else none
      let chars := lump.name.toList
      if chars.length ≥ 6 then
        let family := String.ofList (chars.take 4)
        if let some rot1 := rotOf chars[5]! then
          let key1 := family.push chars[4]!
          let old := spriteRots.get? key1 |>.getD (Array.replicate 8 default)
          spriteRots := spriteRots.insert key1
            (put8 old rot1 { lump := lump.name, flipped := false })
        if chars.length == 8 then
          if let some rot2 := rotOf chars[7]! then
            let key2 := family.push chars[6]!
            let old := spriteRots.get? key2 |>.getD (Array.replicate 8 default)
            spriteRots := spriteRots.insert key2
              (put8 old rot2 { lump := lump.name, flipped := true })

  -- HUD graphics: the big red status digits, the percent sign, and the
  -- three key-card icons the HUD draws for the keys you are carrying
  let mut graphics : Std.HashMap String Picture := ∅
  for name in #["STTNUM0", "STTNUM1", "STTNUM2", "STTNUM3", "STTNUM4",
                "STTNUM5", "STTNUM6", "STTNUM7", "STTNUM8", "STTNUM9",
                "STTPRCNT", "STTMINUS", "M_PAUSE", "TITLEPIC", "M_DOOM",
                "M_SKULL1", "M_SKULL2", "M_NGAME", "M_SAVEG", "M_LOADG",
                "M_QUITG", "WIF", "WIENTER",
                "STKEYS0", "STKEYS1", "STKEYS2"] do
    if let some lump := wad.find? name then
      if let some pic ← parseOptional? (wad.data lump) then
        graphics := graphics.insert name pic
  -- Intermission art. This has to cover every episode and both naming
  -- schemes, not just episode 1: `WIMAP{e}` backdrops and `WILV{e}{m}`
  -- names for Doom, `INTERPIC` and `CWILV{nn}` for Doom II. Whatever a
  -- given IWAD lacks is simply skipped.
  let mut interNames := #["INTERPIC", "WIMAP0", "WIMAP1", "WIMAP2"]
  for e in [0:4] do
    for m in [0:9] do
      interNames := interNames.push s!"WILV{e}{m}"
  for n in [0:32] do
    interNames := interNames.push s!"CWILV{if n < 10 then "0" else ""}{n}"
  for name in interNames do
    if let some lump := wad.find? name then
      if let some pic ← parseOptional? (wad.data lump) then
        graphics := graphics.insert name pic
  -- the small menu/HUD text font, one glyph per printable character
  for code in [33:96] do
    let name := s!"STCFN{(1000 + code).repr.drop 1}"
    if let some lump := wad.find? name then
      if let some pic ← parseOptional? (wad.data lump) then
        graphics := graphics.insert name pic
  -- the marine's status-bar face: 5 pain brackets × idle/turn/ouch/grin,
  -- plus the god and dead faces
  let mut faces : Array String := #["STFGOD0", "STFDEAD0"]
  for n in [0:5] do
    faces := faces ++ #[s!"STFST{n}0", s!"STFST{n}1", s!"STFST{n}2",
      s!"STFTL{n}0", s!"STFTR{n}0", s!"STFOUCH{n}", s!"STFEVL{n}", s!"STFKILL{n}"]
  for name in faces do
    if let some lump := wad.find? name then
      if let some pic ← parseOptional? (wad.data lump) then
        graphics := graphics.insert name pic

  return { palettes := wad.data playpal
           colormap := wad.data colormap
           textures, flats, sprites, spriteRots, graphics }

end Assets
end Dill
