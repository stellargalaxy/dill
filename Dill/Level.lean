import Dill.Wad

/-!
# Map geometry

A Doom map is a set of lumps following a marker lump (`E1M1`, …) in a fixed
order. The renderer walks the precomputed BSP tree (`NODES` → `SSECTORS` →
`SEGS`); the simulation collides against `LINEDEFS` via the `BLOCKMAP` spatial
index. All 16-bit map coordinates become `Float` here — see DESIGN.md.
-/

namespace Dill

open Bytes

/-- A 2D map point. Doom's unit is roughly an inch; the player is 56 tall. -/
structure Vertex where
  x : Float
  y : Float
  deriving Repr, Inhabited

/-- A wall or boundary between two sectors, as drawn by the level designer.

`front` is the right side when walking from `v1` to `v2`; one-sided lines
(solid walls) have no `back`. `special` and `tag` make the line *do*
something: door, lift, switch. -/
structure Linedef where
  v1      : Nat
  v2      : Nat
  flags   : UInt16
  special : Nat
  tag     : Nat
  front   : Nat
  back    : Option Nat
  deriving Repr, Inhabited

namespace Linedef
/-- Blocks players and monsters. -/
def blocking     : UInt16 := 0x0001
/-- Two-sided: has both a front and a back sector. -/
def twoSided     : UInt16 := 0x0004
/-- Sound-blocking (`ML_SOUNDBLOCK`): a gunshot's alert crosses at most one. -/
def soundBlock   : UInt16 := 0x0040
/-- Upper texture is drawn from the top down ("unpegged"). -/
def upperUnpegged : UInt16 := 0x0008
/-- Lower/middle texture is drawn from the bottom up. -/
def lowerUnpegged : UInt16 := 0x0010

def has (l : Linedef) (flag : UInt16) : Bool := l.flags &&& flag != 0
end Linedef

/-- One side of a linedef: texture names and their alignment offsets.

Texture name `"-"` means "no texture here". `upper` covers the step down to
a lower neighboring ceiling, `lower` the step up to a higher neighboring
floor, `middle` the main wall (or a see-through grate on two-sided lines). -/
structure Sidedef where
  xOffset : Float
  yOffset : Float
  upper   : String
  lower   : String
  middle  : String
  sector  : Nat
  deriving Repr, Inhabited

/-- A room: a polygon with one floor height, one ceiling height, one light
level. Doors and lifts are sectors whose heights move over time. -/
structure Sector where
  floorH    : Float
  ceilH     : Float
  floorFlat : String
  ceilFlat  : String
  light     : Nat
  special   : Nat
  tag       : Nat
  deriving Repr, Inhabited

/-- A fragment of a linedef, cut up by the BSP builder. Segs are what the
renderer actually draws. `backSide` says which side of its linedef this seg
runs along; `offset` is the distance from the linedef's start vertex to the
seg's, used to keep textures aligned across the cut. -/
structure Seg where
  v1       : Nat
  v2       : Nat
  linedef  : Nat
  backSide : Bool
  offset   : Float
  deriving Repr, Inhabited

/-- A convex region of one sector: a run of consecutive segs. -/
structure Subsector where
  first : Nat
  count : Nat
  deriving Repr, Inhabited

/-- A child of a BSP node: either another node or a leaf subsector. -/
inductive BspChild where
  | node      (index : Nat)
  | subsector (index : Nat)
  deriving Repr, Inhabited

/-- Axis-aligned bounding box (map coordinates, y grows north). -/
structure BBox where
  top    : Float
  bottom : Float
  left   : Float
  right  : Float
  deriving Repr, Inhabited

/-- A BSP node: a partition line splitting space into a right and left half,
with a bounding box around each child. The tree's root is the *last* node. -/
structure Node where
  x        : Float
  y        : Float
  dx       : Float
  dy       : Float
  rightBox : BBox
  leftBox  : BBox
  right    : BspChild
  left     : BspChild
  deriving Repr, Inhabited

/-- Is the point on the right side (child 0) of the node's partition line?

The `≥` ties toward the right child; vanilla `R_PointOnSide` ties toward
the *left* (side 1). Deliberately left as is: with `Float` coordinates an
exact tie is nearly unreachable, and a point on the partition line renders
correctly from either child. -/
def Node.pointOnRight (n : Node) (px py : Float) : Bool :=
  (px - n.x) * n.dy - (py - n.y) * n.dx ≥ 0

/-- `Float.floor` as an `Int` (exact for the coordinate ranges Doom uses). -/
@[inline] def ifloor (a : Float) : Int :=
  let f := a.floor
  if f < 0 then -Int.ofNat (-f).toUInt64.toNat else Int.ofNat f.toUInt64.toNat

/-- A map object placement: player start, monster, pickup, decoration. -/
structure Thing where
  x     : Float
  y     : Float
  angle : Int
  type  : Nat
  flags : UInt16
  deriving Repr, Inhabited

/-- Spatial index for collision: a 128×128-unit grid where each cell lists
the linedefs crossing it. -/
structure Blockmap where
  originX : Float
  originY : Float
  cols    : Nat
  rows    : Nat
  /-- Row-major: linedef indices in cell `(col, row)` at `row * cols + col`. -/
  blocks  : Array (Array Nat)
  deriving Repr, Inhabited

/-- A fully decoded map. -/
structure Level where
  name       : String
  vertexes   : Array Vertex
  linedefs   : Array Linedef
  sidedefs   : Array Sidedef
  sectors    : Array Sector
  segs       : Array Seg
  subsectors : Array Subsector
  nodes      : Array Node
  things     : Array Thing
  blockmap   : Blockmap
  /-- `REJECT`: one bit per ordered sector pair, set when the two certainly
  cannot see each other. Vanilla's `P_CheckSight` tests it before tracing
  anything. Empty when the lump is missing or the wrong size, which simply
  means "reject nothing" — the sight check then does its own work. -/
  reject     : ByteArray := ByteArray.empty
  /-- The linedefs touching each sector — vanilla's `sec->lines[]`, built once
  at load. Which lines bound a sector is fixed geometry, so anything that
  works outward from a sector (the automap reveal, the noise flood, the
  neighbour heights) walks a handful of lines instead of the whole map. -/
  sectorLines : Array (Array Nat) := #[]
  /-- Lines carrying special 48, the scrolling wall texture. Nothing ever
  grants or clears that special, so the set is settled at load; the stepper
  still re-checks each one, so a save that rewrote specials cannot go wrong. -/
  scrollLines : Array Nat := #[]

/-- Does a body `height` tall, with its feet at `z`, fit through an opening
running from `openBot` to `openTop`, given it can climb `maxStep`?

Vanilla `P_TryMove` asks three questions and this is all of them:

1. `openTop - openBot ≥ height` — the gap is tall enough at all;
2. `openTop - z ≥ height` — and tall enough *above where the body is now*,
   which is a different question whenever the body is off its floor (falling,
   or riding something down);
3. `openBot - z ≤ maxStep` — and the step up is climbable.

One definition because the second clause used to be in only one of the three
places that need it: `Player.slideHit` tested it, while `Player.checkPosition`
and `Level.checkBody` — the two functions that actually decide whether a body
may stand somewhere — did not. So the slide and the position test disagreed
about what was passable, and an airborne body could be moved into a space
whose ceiling was already below its head. -/
def bodyFits (openTop openBot z height maxStep : Float) : Bool :=
  openTop - openBot ≥ height
    && openTop - z ≥ height
    && openBot - z ≤ maxStep

namespace Level

/-- A map's lumps sit in the 10 directory slots after its marker (THINGS …
BLOCKMAP). Searching further would silently borrow the *next* map's lump of
the same name when this map lacks one. -/
private def mapWindow : Nat := 10

/-- Fetch a map lump by name relative to the map marker at `marker`, and
check it divides evenly into `recordSize`-byte records. -/
private def mapLump (wad : Wad) (marker : Nat) (name : String)
    (recordSize : Nat) : Except String (ByteArray × Nat) := do
  let some index := wad.indexOf? name marker
    | throw s!"map lump {name.quote} not found"
  if index > marker + mapWindow then
    throw s!"map lump {name.quote} not found before the next map's lumps"
  let lump := wad.lumps[index]!
  if lump.size % recordSize != 0 then
    throw s!"{name}: size {lump.size} is not a multiple of {recordSize}"
  return (wad.data lump, lump.size / recordSize)

/-- Parse `count` records of `size` bytes with `read`. -/
private def readRecords (bytes : ByteArray) (count size : Nat)
    (read : ByteArray → Nat → α) : Array α := Id.run do
  let mut out := Array.mkEmpty count
  for k in [0:count] do
    out := out.push (read bytes (k * size))
  return out

private def parseVertexes (b : ByteArray) (n : Nat) : Array Vertex :=
  readRecords b n 4 fun b o =>
    { x := f16le b o, y := f16le b (o + 2) }

private def parseLinedefs (b : ByteArray) (n : Nat) : Array Linedef :=
  readRecords b n 14 fun b o =>
    { v1      := (u16le b o).toNat
      v2      := (u16le b (o + 2)).toNat
      flags   := u16le b (o + 4)
      special := (u16le b (o + 6)).toNat
      tag     := (u16le b (o + 8)).toNat
      front   := (u16le b (o + 10)).toNat
      back    := match (u16le b (o + 12)).toNat with
                 | 0xFFFF => none
                 | s      => some s }

private def parseSidedefs (b : ByteArray) (n : Nat) : Array Sidedef :=
  readRecords b n 30 fun b o =>
    { xOffset := f16le b o
      yOffset := f16le b (o + 2)
      -- texture names are matched case-insensitively (vanilla uppercases
      -- them); some IWAD sidedefs store lowercase, e.g. E1M4's `doorstop`
      upper   := (name8 b (o + 4)).toUpper
      lower   := (name8 b (o + 12)).toUpper
      middle  := (name8 b (o + 20)).toUpper
      sector  := (u16le b (o + 28)).toNat }

private def parseSectors (b : ByteArray) (n : Nat) : Array Sector :=
  readRecords b n 26 fun b o =>
    { floorH    := f16le b o
      ceilH     := f16le b (o + 2)
      floorFlat := (name8 b (o + 4)).toUpper
      ceilFlat  := (name8 b (o + 12)).toUpper
      light     := ((i16le b (o + 20)).toNat? |>.getD 0)
      special   := (u16le b (o + 22)).toNat
      tag       := (u16le b (o + 24)).toNat }

private def parseSegs (b : ByteArray) (n : Nat) : Array Seg :=
  readRecords b n 12 fun b o =>
    { v1       := (u16le b o).toNat
      v2       := (u16le b (o + 2)).toNat
      linedef  := (u16le b (o + 6)).toNat
      backSide := u16le b (o + 8) != 0
      offset   := f16le b (o + 10) }

private def parseSubsectors (b : ByteArray) (n : Nat) : Array Subsector :=
  readRecords b n 4 fun b o =>
    { count := (u16le b o).toNat, first := (u16le b (o + 2)).toNat }

private def parseBBox (b : ByteArray) (o : Nat) : BBox :=
  { top    := f16le b o
    bottom := f16le b (o + 2)
    left   := f16le b (o + 4)
    right  := f16le b (o + 6) }

private def parseChild (raw : Nat) : BspChild :=
  if raw &&& 0x8000 != 0 then .subsector (raw &&& 0x7FFF) else .node raw

private def parseNodes (b : ByteArray) (n : Nat) : Array Node :=
  readRecords b n 28 fun b o =>
    { x        := f16le b o
      y        := f16le b (o + 2)
      dx       := f16le b (o + 4)
      dy       := f16le b (o + 6)
      rightBox := parseBBox b (o + 8)
      leftBox  := parseBBox b (o + 16)
      right    := parseChild (u16le b (o + 24)).toNat
      left     := parseChild (u16le b (o + 26)).toNat }

private def parseThings (b : ByteArray) (n : Nat) : Array Thing :=
  readRecords b n 10 fun b o =>
    { x     := f16le b o
      y     := f16le b (o + 2)
      angle := i16le b (o + 4)
      type  := (u16le b (o + 6)).toNat
      flags := u16le b (o + 8) }

-- (not `private`: the packed-lump tests parse crafted lumps directly)
def parseBlockmap (b : ByteArray) : Except String Blockmap := do
  if b.size < 8 then throw "BLOCKMAP: header truncated"
  let cols := (u16le b 4).toNat
  let rows := (u16le b 6).toNat
  -- The whole offset table must fit inside the lump. This also bounds
  -- `cols * rows`, so a malformed header can't demand billions of cells.
  if 8 + cols * rows * 2 > b.size then
    throw s!"BLOCKMAP: offset table ({cols}×{rows} cells) extends past \
             lump end ({b.size} bytes)"
  let mut blocks := Array.mkEmpty (cols * rows)
  -- Malformed-input guard. The offset table can point every cell at the
  -- same long list, making a naive walk O(cells × lump size) on a crafted
  -- lump. But aliasing is also legitimate: packing node builders (ZenNode,
  -- zdbsp) point every cell with identical contents at one stored list,
  -- and ZokumBSP's subset compression goes further, pointing a cell into
  -- the *middle* of another cell's list to share its tail — so neither
  -- summing each cell's length against the lump's own word count nor
  -- caching whole lists by their start offset admits every honest packed
  -- map (as `Picture.parse` notes for wadptr-shared columns). Instead,
  -- each lump word is freshly parsed at most once: `raw` remembers, for
  -- every word position already walked, which parsed list carries its
  -- suffix, so a walk reaching previously-parsed ground splices that known
  -- tail rather than re-reading it, and `mat` memoizes each offset's
  -- finished list so cells naming the same offset share one array. Fresh
  -- parsing is then bounded by the lump's word count (`consumed`, now an
  -- invariant as much as a guard), leaving the words *copied* out of
  -- shared lists as the one crafted-input lever: honest subset packing
  -- copies each shared tail about once, so a few lumps' worth is ample,
  -- and a lump built to alias every suffix of one giant list is refused
  -- before the copies can grow quadratic.
  let mut raw : Array (Option (Array Nat × Nat)) :=
    Array.replicate (b.size / 2) none
  let mut mat : Array (Option (Array Nat)) := Array.replicate (b.size / 2) none
  let mut consumed := 0
  let mut copied := 0
  let copyCap := 4 * b.size   -- in words: 8× the lump's own word count
  for cell in [0:cols * rows] do
    -- The offset table entries are in 16-bit words from the lump start.
    let listOfs := (u16le b (8 + cell * 2)).toNat * 2
    if listOfs + 2 > b.size then throw "BLOCKMAP: block offset out of range"
    if let some lines := mat[listOfs / 2]! then
      blocks := blocks.push lines
    else if let some (parent, drop) := raw[listOfs / 2]! then
      -- A tail of an already-parsed list. A fresh walk from here would
      -- apply the leading-0 rule below, so apply it to the raw suffix.
      let drop := if u16le b listOfs == 0 then drop + 1 else drop
      let lines := parent.extract drop parent.size
      copied := copied + lines.size
      if copied > copyCap then
        throw "BLOCKMAP: shared cell lists expand past the copy budget"
      mat := mat.set! (listOfs / 2) (some lines)
      blocks := blocks.push lines
    else
      -- Each list conventionally opens with a 0 word (vanilla's node builder
      -- wrote one; the engine never reads it) and ends with 0xFFFF. Skip the
      -- leading word only when it really is that 0 — some nonconforming
      -- tools omit it, and skipping unconditionally would then drop the
      -- cell's first linedef (the Boom-lineage reading).
      let mut lines : Array Nat := #[]
      let mut walked : Array Nat := #[]   -- word positions this walk read
      let mut spliced := false
      let mut p := if u16le b listOfs == 0 then listOfs + 2 else listOfs
      while p + 2 ≤ b.size && u16le b p != 0xFFFF && !spliced do
        match raw[p / 2]! with
        | some (parent, drop) =>
          -- Previously-parsed ground: splice its known tail and stop. No
          -- leading-0 rule here — mid-list, a 0 word is linedef 0, data.
          let tail := parent.extract drop parent.size
          copied := copied + tail.size
          if copied > copyCap then
            throw "BLOCKMAP: shared cell lists expand past the copy budget"
          lines := lines ++ tail
          spliced := true
        | none =>
          consumed := consumed + 1
          if consumed > b.size / 2 then
            throw "BLOCKMAP: cell lists consume more words than the lump holds"
          walked := walked.push p
          lines := lines.push (u16le b p).toNat
          p := p + 2
      for j in [0:walked.size] do
        raw := raw.set! (walked[j]! / 2) (some (lines, j))
      mat := mat.set! (listOfs / 2) (some lines)
      blocks := blocks.push lines
  return { originX := f16le b 0
           originY := f16le b 2
           cols, rows, blocks }

/-- Cross-check every index one lump stores into another, so a malformed map
fails here with a named error instead of panicking mid-render. The record
parsers only guarantee each lump's *size*; the indices inside are the WAD's
word and must be checked before `segFrontSector`, `subsectorAt`, and friends
trust them with `[...]!`. -/
private def validate (lvl : Level) : Except String Unit := do
  for i in [0:lvl.linedefs.size] do
    let l := lvl.linedefs[i]!
    if l.v1 ≥ lvl.vertexes.size || l.v2 ≥ lvl.vertexes.size then
      throw s!"LINEDEFS: linedef {i} references a vertex out of range"
    if l.front ≥ lvl.sidedefs.size then
      throw s!"LINEDEFS: linedef {i} front sidedef {l.front} out of range"
    if let some b := l.back then
      if b ≥ lvl.sidedefs.size then
        throw s!"LINEDEFS: linedef {i} back sidedef {b} out of range"
  for i in [0:lvl.sidedefs.size] do
    let s := lvl.sidedefs[i]!.sector
    if s ≥ lvl.sectors.size then
      throw s!"SIDEDEFS: sidedef {i} sector {s} out of range"
  for i in [0:lvl.segs.size] do
    let s := lvl.segs[i]!
    if s.v1 ≥ lvl.vertexes.size || s.v2 ≥ lvl.vertexes.size then
      throw s!"SEGS: seg {i} references a vertex out of range"
    if s.linedef ≥ lvl.linedefs.size then
      throw s!"SEGS: seg {i} linedef {s.linedef} out of range"
  if lvl.subsectors.isEmpty then
    throw "SSECTORS: map has no subsectors"
  for i in [0:lvl.subsectors.size] do
    let ss := lvl.subsectors[i]!
    if ss.count == 0 then
      throw s!"SSECTORS: subsector {i} has no segs"
    if ss.first + ss.count > lvl.segs.size then
      throw s!"SSECTORS: subsector {i} segs {ss.first}+{ss.count} out of range"
  for i in [0:lvl.nodes.size] do
    let n := lvl.nodes[i]!
    for c in [n.right, n.left] do
      match c with
      | .node j =>
        if j ≥ lvl.nodes.size then
          throw s!"NODES: node {i} child node {j} out of range"
      | .subsector j =>
        if j ≥ lvl.subsectors.size then
          throw s!"NODES: node {i} child subsector {j} out of range"
  for i in [0:lvl.blockmap.blocks.size] do
    for l in lvl.blockmap.blocks[i]! do
      if l ≥ lvl.linedefs.size then
        throw s!"BLOCKMAP: cell {i} references linedef {l} out of range"

/-- Decode the map that follows marker lump `name` (e.g. `"E1M1"`). -/
def load (wad : Wad) (name : String) : Except String Level := do
  -- the last marker wins, so a PWAD's replacement map shadows the IWAD's;
  -- the sub-lumps are then read forward from *that* marker
  let some marker := wad.lastIndexOf? name
    | throw s!"map {name.quote} not found"
  let (vertexBytes, nVertexes)     ← mapLump wad marker "VERTEXES" 4
  let (linedefBytes, nLinedefs)    ← mapLump wad marker "LINEDEFS" 14
  let (sidedefBytes, nSidedefs)    ← mapLump wad marker "SIDEDEFS" 30
  let (sectorBytes, nSectors)      ← mapLump wad marker "SECTORS" 26
  let (segBytes, nSegs)            ← mapLump wad marker "SEGS" 12
  let (subsectorBytes, nSubs)      ← mapLump wad marker "SSECTORS" 4
  let (nodeBytes, nNodes)          ← mapLump wad marker "NODES" 28
  let (thingBytes, nThings)        ← mapLump wad marker "THINGS" 10
  let (blockmapBytes, _)           ← mapLump wad marker "BLOCKMAP" 1
  let blockmap ← parseBlockmap blockmapBytes
  -- REJECT is advisory. Keep it only when it is the full matrix *and*
  -- actually rejects something: plenty of big maps (Plutonia MAP29 among
  -- them) ship an all-zero lump because building a real one is slow, and
  -- consulting that is pure overhead on every sight check.
  let needed := (nSectors * nSectors + 7) / 8
  let reject := match wad.indexOf? "REJECT" marker with
    | some idx =>
      if idx > marker + mapWindow then ByteArray.empty  -- next map's REJECT
      else
        let bytes := wad.data wad.lumps[idx]!
        -- scan only what is actually there: undersized REJECT lumps are
        -- common in the wild, and reading past `bytes.size` would panic
        let rejectsSomething := Id.run do
          for k in [0:min needed bytes.size] do
            if bytes[k]! != 0 then return true
          return false
        if needed > 0 && bytes.size ≥ needed && rejectsSomething then bytes
        else ByteArray.empty
    | none => ByteArray.empty
  -- Index the geometry once: which lines bound each sector, and which of
  -- them scroll. Both are fixed for the life of the map.
  let linedefs := parseLinedefs linedefBytes nLinedefs
  let sidedefs := parseSidedefs sidedefBytes nSidedefs
  let (sectorLines, scrollLines) := Id.run do
    let mut byS : Array (Array Nat) := Array.replicate nSectors #[]
    let mut scroll : Array Nat := #[]
    for i in [0:linedefs.size] do
      let l := linedefs[i]!
      if l.special == 48 then scroll := scroll.push i
      let mut touched : Array Nat := #[]
      if h : l.front < sidedefs.size then touched := touched.push sidedefs[l.front].sector
      if let some b := l.back then
        if h : b < sidedefs.size then
          let s := sidedefs[b].sector
          if !touched.contains s then touched := touched.push s
      for s in touched do
        -- `modify` keeps the inner array uniquely referenced, so the push
        -- stays in place; `set! s (byS[s]!.push i)` would copy it each time
        if s < byS.size then byS := byS.modify s (·.push i)
    return (byS, scroll)
  let lvl : Level :=
    { name, reject, linedefs, sidedefs, sectorLines, scrollLines
      vertexes   := parseVertexes vertexBytes nVertexes
      sectors    := parseSectors sectorBytes nSectors
      segs       := parseSegs segBytes nSegs
      subsectors := parseSubsectors subsectorBytes nSubs
      nodes      := parseNodes nodeBytes nNodes
      things     := parseThings thingBytes nThings
      blockmap }
  validate lvl
  return lvl

/-- Indices of the linedefs sitting in the blockmap cells a ray crosses,
from `(x1, y1)` to `(x2, y2)`.

This is vanilla's `P_PathTraverse`, which walks the blockmap along the ray
rather than considering every line on the map — the difference between
reading a dozen linedefs and all three thousand for a single bullet. The
walk is the standard grid traversal: step to whichever axis boundary the ray
meets next. May repeat indices; callers must be idempotent. -/
def linesAlong (lvl : Level) (x1 y1 x2 y2 : Float) : Array Nat := Id.run do
  let bm := lvl.blockmap
  if bm.cols == 0 || bm.rows == 0 then return #[]
  -- work in cell units, so a cell is one unit wide
  let fx1 := (x1 - bm.originX) / 128.0
  let fy1 := (y1 - bm.originY) / 128.0
  let fx2 := (x2 - bm.originX) / 128.0
  let fy2 := (y2 - bm.originY) / 128.0
  let mut cx := ifloor fx1
  let mut cy := ifloor fy1
  let endX := ifloor fx2
  let endY := ifloor fy2
  let dx := fx2 - fx1
  let dy := fy2 - fy1
  let stepX : Int := if dx > 0 then 1 else if dx < 0 then -1 else 0
  let stepY : Int := if dy > 0 then 1 else if dy < 0 then -1 else 0
  -- how far along the ray one whole cell of travel costs, per axis
  let deltaX := if dx == 0 then 1.0e30 else Float.abs (1.0 / dx)
  let deltaY := if dy == 0 then 1.0e30 else Float.abs (1.0 / dy)
  -- …and how far to the first boundary
  let mut nextX :=
    if dx == 0 then 1.0e30
    else if dx > 0 then ((Float.ofInt cx + 1.0) - fx1) / dx
    else (Float.ofInt cx - fx1) / dx
  let mut nextY :=
    if dy == 0 then 1.0e30
    else if dy > 0 then ((Float.ofInt cy + 1.0) - fy1) / dy
    else (Float.ofInt cy - fy1) / dy
  let mut out := #[]
  let gather := fun (acc : Array Nat) (gx gy : Int) =>
    if 0 ≤ gx && gx < Int.ofNat bm.cols && 0 ≤ gy && gy < Int.ofNat bm.rows then
      acc ++ bm.blocks[gy.toNat * bm.cols + gx.toNat]!
    else acc
  out := gather out cx cy
  -- every step moves one cell straight toward the end cell, so the cells'
  -- Manhattan distance bounds the walk exactly — computed from the actual
  -- endpoints because either may lie outside the grid (a hitscan overshoots
  -- the map edge; noclip walks off it), where `cols + rows` undercounts.
  -- +2 absorbs float ties on cell boundaries. Map coordinates live in
  -- ±32768, so real cells live in ±256; the 4096 ceiling only stops a
  -- degenerate (infinite) endpoint from turning the cap into a hang.
  let steps := min ((endX - cx).natAbs + (endY - cy).natAbs + 2) 4096
  for _ in [0 : steps] do
    if cx == endX && cy == endY then break
    if nextX < nextY then
      nextX := nextX + deltaX
      cx := cx + stepX
    else
      nextY := nextY + deltaY
      cy := cy + stepY
    out := gather out cx cy
  return out

/-- Does `REJECT` rule out any sight line between these two sectors?
Vanilla `P_CheckSight`'s "trivial rejection": bit `s1 * numsectors + s2` of
the matrix, set when the pair certainly cannot see each other. Always
`false` when the lump was missing or short. -/
@[inline] def rejects (lvl : Level) (s1 s2 : Nat) : Bool :=
  if lvl.reject.isEmpty then false
  else
    let pnum := s1 * lvl.sectors.size + s2
    let byte := pnum >>> 3
    if h : byte < lvl.reject.size then
      lvl.reject[byte] &&& (1 <<< UInt8.ofNat (pnum &&& 7)) != 0
    else false

/-- The sector a seg's front side faces. -/
def segFrontSector (lvl : Level) (s : Seg) : Nat :=
  let line := lvl.linedefs[s.linedef]!
  let side := if s.backSide then line.back.getD line.front else line.front
  lvl.sidedefs[side]!.sector

/-- The sector behind a seg, if its linedef is two-sided. -/
def segBackSector (lvl : Level) (s : Seg) : Option Nat := do
  let line := lvl.linedefs[s.linedef]!
  let side ← if s.backSide then some line.front else line.back
  return lvl.sidedefs[side]!.sector

/-- Walk the BSP to the subsector containing a point (the tree root is the
last node). -/
def subsectorAt (lvl : Level) (x y : Float) : Nat := Id.run do
  if lvl.nodes.isEmpty then return 0
  let mut child := BspChild.node (lvl.nodes.size - 1)
  -- a valid tree visits each node at most once, so `nodes.size` steps must
  -- reach a leaf; the cap keeps a cyclic (malformed) tree from hanging us
  for _ in [0:lvl.nodes.size] do
    match child with
    | .subsector i => return i
    | .node i =>
      let n := lvl.nodes[i]!
      child := if n.pointOnRight x y then n.right else n.left
  match child with
  | .subsector i => return i
  | .node _ => return 0

/-- Where the player begins.

Vanilla's `P_SpawnMapThing` calls `P_SpawnPlayer` for *every* type-1 thing it
meets, so a map that places more than one — PWADs do — starts you at the
last. That is the same "last wins" rule `Wad.find?` and `Level.load` follow
for lumps, and taking the first instead put the player somewhere else
entirely on those maps. One definition so the simulation, the offline `view`
camera, and `dill map` cannot disagree about it. -/
def playerStart (lvl : Level) : Option Thing :=
  lvl.things.foldl (fun best t => if t.type == 1 then some t else best) none

/-- The sector containing a point. -/
def sectorAt (lvl : Level) (x y : Float) : Nat :=
  let sub := lvl.subsectors[lvl.subsectorAt x y]!
  lvl.segFrontSector lvl.segs[sub.first]!

/-- Indices of linedefs whose blockmap cells overlap a square of `radius`
around a point. May contain duplicates; users must be idempotent.

The cell range is clamped to the grid and then walked directly. A body's
box spans one to four cells, so filtering the whole grid instead would cost
every collision test the map's entire blockmap — and collision runs for the
player and each monster several times a tic. -/
def linesNear (lvl : Level) (x y radius : Float) : Array Nat := Id.run do
  let bm := lvl.blockmap
  if bm.cols == 0 || bm.rows == 0 then return #[]
  let cell := fun (v origin : Float) => ifloor ((v - origin) / 128.0)
  -- `Int.toNat` clamps a negative cell (left of / below the grid) to 0
  let x1 := (cell (x - radius) bm.originX).toNat
  let y1 := (cell (y - radius) bm.originY).toNat
  let x2 := cell (x + radius) bm.originX
  let y2 := cell (y + radius) bm.originY
  -- entirely off the low side: no cell overlaps
  if x2 < 0 || y2 < 0 then return #[]
  let x2 := min (bm.cols - 1) x2.toNat
  let y2 := min (bm.rows - 1) y2.toNat
  -- entirely off the high side leaves `x1 > x2`, and the range is empty
  let mut out := #[]
  for cy in [y1 : y2 + 1] do
    for cx in [x1 : x2 + 1] do
      out := out ++ bm.blocks[cy * bm.cols + cx]!
  return out

/-! ## Ray and box against a linedef

The two geometric primitives every collision and trace in the game is built
from. Both were once copied into each caller; keeping one of each means the
player and the monsters cannot drift onto subtly different collision shapes,
and a sight line cannot come to disagree with the bullet fired along it. -/

/-- Does the axis-aligned box of half-width `radius` around `(x, y)` cross
`line`? Reject on the line's bounding box first, then straddle-test: the box
meets the infinite line exactly when its four corners do not all agree on
which side they are on.

Shared by the player's `Player.checkPosition` and the monsters' `checkBody`,
which is what makes them one collision shape rather than two. -/
def boxCrossesLine (lvl : Level) (line : Linedef) (x y radius : Float) : Bool :=
  let p1 := lvl.vertexes[line.v1]!
  let p2 := lvl.vertexes[line.v2]!
  if x + radius ≤ min p1.x p2.x || x - radius ≥ max p1.x p2.x then false
  else if y + radius ≤ min p1.y p2.y || y - radius ≥ max p1.y p2.y then false
  else
    let dx := p2.x - p1.x
    let dy := p2.y - p1.y
    let side := fun (cx cy : Float) => dx * (cy - p1.y) - dy * (cx - p1.x) > 0
    let a := side (x - radius) (y - radius)
    let b := side (x + radius) (y - radius)
    let c := side (x - radius) (y + radius)
    let d := side (x + radius) (y + radius)
    !(a == b && b == c && c == d)

/-- Where the ray from `(ox, oy)` along `(dx, dy)` meets `line`: `some t`,
the fraction along the ray, when it crosses the linedef *segment*; `none`
when the two are parallel or the crossing falls outside the linedef's extent.

`t` is measured in units of the direction vector, so a caller passing a unit
direction reads `t` as a distance while one passing a whole tic's movement
reads it as a fraction of that step. The guard on `t` itself belongs to the
caller, because it is the one thing that genuinely differs between them: a
sight line wants `0 < t < 1`, a bullet `0 < t < range`, a missile
`0 < t ≤ 1`, a wall slide `0 ≤ t ≤ 1`. -/
@[inline] def rayHitsLine (lvl : Level) (line : Linedef)
    (ox oy dx dy : Float) : Option Float :=
  let p1 := lvl.vertexes[line.v1]!
  let p2 := lvl.vertexes[line.v2]!
  let ldx := p2.x - p1.x
  let ldy := p2.y - p1.y
  let denom := dx * ldy - dy * ldx
  if denom == 0 then none
  else
    -- `u` locates the crossing along the linedef; outside 0…1 the ray passes
    -- the linedef's infinite line but misses the wall itself
    let u := ((p1.x - ox) * dy - (p1.y - oy) * dx) / denom
    if u < 0 || u > 1 then none
    else some (((p1.x - ox) * ldy - (p1.y - oy) * ldx) / denom)

/-! ## Sector neighbourhoods

Which sectors touch which, and the extreme heights and light levels among
them. Everything here is a question about the map's fixed geometry, so it
belongs beside the geometry rather than beside whatever asks — the line
specials that raise a floor "to the next higher neighbour", the light
thinkers that dim to the darkest one, and the boss deaths that drop the
tag-666 floors all ask the same questions, and they live in three modules
that cannot import each other. -/

/-- Sectors on the other side of a sector's two-sided lines. -/
def neighbors (lvl : Level) (s : Nat) : Array Nat := Id.run do
  -- only the lines that bound this sector (vanilla's `sec->lines[]`)
  let some lines := lvl.sectorLines[s]? | return #[]
  let mut out := #[]
  for i in lines do
    let line := lvl.linedefs[i]!
    let some back := line.back | continue
    let f := lvl.sidedefs[line.front]!.sector
    let b := lvl.sidedefs[back]!.sector
    if f == s && b != s then out := out.push b
    if b == s && f != s then out := out.push f
  return out

/-- Vanilla `P_FindLowestFloorSurrounding`, which starts from the sector's
own floor — so a sector with no neighbours stays put. -/
def lowestNeighborFloor (lvl : Level) (s : Nat) : Float :=
  (lvl.neighbors s).foldl (fun h n => min h lvl.sectors[n]!.floorH)
    lvl.sectors[s]!.floorH

def highestNeighborFloor (lvl : Level) (s : Nat) : Float :=
  (lvl.neighbors s).foldl (fun h n => max h lvl.sectors[n]!.floorH) (-32000.0)

/-- Vanilla `P_FindLowestCeilingSurrounding`. Note the seed: `MAXINT` there,
32000 here — *not* the sector's own ceiling. A door opens to 4 below this,
so a tagged sector with no neighbours is meant to fly open rather than not
move at all. -/
def lowestNeighborCeil (lvl : Level) (s : Nat) : Float :=
  (lvl.neighbors s).foldl (fun h n => min h lvl.sectors[n]!.ceilH) 32000.0

def highestNeighborCeil (lvl : Level) (s : Nat) : Float :=
  (lvl.neighbors s).foldl (fun h n => max h lvl.sectors[n]!.ceilH)
    lvl.sectors[s]!.ceilH

/-- The next floor *above* this one among its neighbours — the step a
"raise to next higher" switch climbs to. With no higher neighbour the
floor stays put, as in vanilla. -/
def nextHigherNeighborFloor (lvl : Level) (s : Nat) : Float :=
  let here := lvl.sectors[s]!.floorH
  (lvl.neighbors s).foldl (fun best n =>
    let h := lvl.sectors[n]!.floorH
    if h > here && (best == here || h < best) then h else best) here

/-- The darkest light level among a sector's neighbours (its own if alone). -/
def minNeighborLight (lvl : Level) (s : Nat) : Nat :=
  (lvl.neighbors s).foldl (fun l n => min l lvl.sectors[n]!.light)
    lvl.sectors[s]!.light

def sectorsTagged (lvl : Level) (tag : Nat) : Array Nat := Id.run do
  let mut out := #[]
  for i in [0 : lvl.sectors.size] do
    if lvl.sectors[i]!.tag == tag then out := out.push i
  return out

/-- A representative point of a sector for positional sounds — the average
of its lines' midpoints (vanilla keeps a `soundorg` at the sector's bbox
centre). `none` for a sector with no lines. -/
def sectorSoundPos (lvl : Level) (s : Nat) : Option (Float × Float) := Id.run do
  let some lines := lvl.sectorLines[s]? | return none
  if lines.isEmpty then return none
  let mut sx := 0.0
  let mut sy := 0.0
  for i in lines do
    let line := lvl.linedefs[i]!
    let p1 := lvl.vertexes[line.v1]!
    let p2 := lvl.vertexes[line.v2]!
    sx := sx + (p1.x + p2.x) / 2
    sy := sy + (p1.y + p2.y) / 2
  return some (sx / Float.ofNat lines.size, sy / Float.ofNat lines.size)

/-- The axis-aligned bounds of a sector, from the lines that bound it —
vanilla's `sector_t.blockbox`. `none` for a sector with no lines, which
callers must treat as "no idea", not as "empty".

Cheap: a door or lift sector is bounded by a handful of lines, and
`sectorLines` already has them indexed. It exists so the things-in-this-
sector questions (a door checking for heads, a crusher for victims) can ask
the mobj grid about a *place* instead of walking the whole roster. -/
def sectorBounds (lvl : Level) (s : Nat) : Option BBox := Id.run do
  let some lines := lvl.sectorLines[s]? | return none
  if lines.isEmpty then return none
  let mut b : BBox := { left := 1.0e30, right := -1.0e30
                        bottom := 1.0e30, top := -1.0e30 }
  for i in lines do
    let line := lvl.linedefs[i]!
    for v in [lvl.vertexes[line.v1]!, lvl.vertexes[line.v2]!] do
      b := { left := min b.left v.x, right := max b.right v.x
             bottom := min b.bottom v.y, top := max b.top v.y }
  return some b

/-! ## Editing a level in place

Doors, lifts and switches all mutate the map as the game runs. These are the
primitive writes they go through. -/

def setFloor (lvl : Level) (s : Nat) (h : Float) : Level :=
  { lvl with sectors := lvl.sectors.modify s ({ · with floorH := h }) }

def setCeil (lvl : Level) (s : Nat) (h : Float) : Level :=
  { lvl with sectors := lvl.sectors.modify s ({ · with ceilH := h }) }

def setLight (lvl : Level) (s : Nat) (light : Nat) : Level :=
  { lvl with sectors := lvl.sectors.modify s ({ · with light }) }

/-- One-shot specials clear themselves after firing. -/
def clearSpecial (lvl : Level) (line : Nat) : Level :=
  { lvl with linedefs := lvl.linedefs.modify line ({ · with special := 0 }) }

/-- Retexture one slot (0=upper, 1=middle, 2=lower) of a sidedef. -/
def setSideTex (lvl : Level) (sd slot : Nat) (name : String) : Level :=
  { lvl with sidedefs := lvl.sidedefs.modify sd fun s => match slot with
      | 0 => { s with upper := name }
      | 1 => { s with middle := name }
      | _ => { s with lower := name } }

/-- A switch texture's pressed/unpressed twin: `SW1*` ↔ `SW2*`. `none` for a
plain wall. -/
def switchTwin (name : String) : Option String :=
  if name.startsWith "SW1" then some ("SW2" ++ name.drop 3)
  else if name.startsWith "SW2" then some ("SW1" ++ name.drop 3)
  else none

/-! ## Sight, clearance, and sound

The three traversals the simulation runs over the map. They are geometry —
none of them knows what a monster or a gunshot is — so they sit here with
the rest of it rather than in whichever `Game` module first needed one. -/

/-- Line-of-sight from eye 1 to point 2: blocked by one-sided lines and by
two-sided lines whose opening doesn't contain the sight line where it
crosses. A simplification of `P_CheckSight` (no slope narrowing across
multiple openings), fine at room scale. -/
def checkSight (lvl : Level) (x1 y1 z1 x2 y2 z2 : Float) : Bool := Id.run do
  -- Vanilla's first move is the REJECT matrix: one precomputed bit per
  -- sector pair saying they certainly cannot see each other. Two cheap BSP
  -- descents buy a skip of the whole linedef walk, which is what keeps the
  -- sight checks in `A_Look` and the refire actions affordable.
  -- the `isEmpty` guard first: with no matrix loaded the two BSP descents
  -- below would be pure waste, and `&&` keeps them from running at all
  if !lvl.reject.isEmpty
      && lvl.rejects (lvl.sectorAt x1 y1) (lvl.sectorAt x2 y2) then return false
  let dx := x2 - x1
  let dy := y2 - y1
  -- vanilla traces sight through the blockmap too, not over every linedef
  for li in lvl.linesAlong x1 y1 x2 y2 do
    let line := lvl.linedefs[li]!
    -- `dx`/`dy` span the whole sight line, so `t` is a fraction of it
    let some t := lvl.rayHitsLine line x1 y1 dx dy | continue
    if t ≤ 0 || t ≥ 1 then continue
    match line.back with
    | none => return false
    | some back =>
      let f := lvl.sectors[lvl.sidedefs[line.front]!.sector]!
      let b := lvl.sectors[lvl.sidedefs[back]!.sector]!
      let sightZ := z1 + (z2 - z1) * t
      if sightZ ≤ max f.floorH b.floorH || sightZ ≥ min f.ceilH b.ceilH then
        return false
  return true

/-- Can a body (`radius` wide, `height` tall, feet at `z`) stand at
`(x, y)`? Walls only — mobj blocking is separate. Returns the floor to
stand on. `maxDrop` set forbids walking off ledges (monsters). -/
def checkBody (lvl : Level) (x y z radius height maxStep : Float)
    (maxDrop : Option Float := none) (isMonster : Bool := false) :
    Option Float := Id.run do
  let sec := lvl.sectors[lvl.sectorAt x y]!
  let mut floorZ := sec.floorH
  let mut lowestFloor := sec.floorH
  let mut ceilZ := sec.ceilH
  for i in (lvl.linesNear x y radius) do
    let line := lvl.linedefs[i]!
    if !lvl.boxCrossesLine line x y radius then continue
    match line.back with
    | none => return none
    | some back =>
      if line.has Linedef.blocking then return none
      if isMonster && line.flags &&& 0x0002 != 0 then return none
      let f := lvl.sectors[lvl.sidedefs[line.front]!.sector]!
      let b := lvl.sectors[lvl.sidedefs[back]!.sector]!
      floorZ := max floorZ (max f.floorH b.floorH)
      lowestFloor := min lowestFloor (min f.floorH b.floorH)
      ceilZ := min ceilZ (min f.ceilH b.ceilH)
  if !bodyFits ceilZ floorZ z height maxStep then return none
  if let some drop := maxDrop then
    if z - lowestFloor > drop then return none
  return some floorZ

/-- The opening a body's box would occupy at (x, y) — highest floor, lowest
ceiling — ignoring whether the body's current `z` fits it. `none` on a hard
block (one-sided or blocking line). This is what vanilla's `floatok` reads:
a flyer refused by `checkBody` only because its altitude is wrong can float
toward this opening and pass, which is how a cacodemon ducks under a
doorway lintel instead of jamming against it forever. -/
def openingNear (lvl : Level) (x y radius : Float) (isMonster : Bool := false) :
    Option (Float × Float) := Id.run do
  let sec := lvl.sectors[lvl.sectorAt x y]!
  let mut floorZ := sec.floorH
  let mut ceilZ := sec.ceilH
  for i in (lvl.linesNear x y radius) do
    let line := lvl.linedefs[i]!
    if !lvl.boxCrossesLine line x y radius then continue
    match line.back with
    | none => return none
    | some back =>
      if line.has Linedef.blocking then return none
      if isMonster && line.flags &&& 0x0002 != 0 then return none
      let f := lvl.sectors[lvl.sidedefs[line.front]!.sector]!
      let b := lvl.sectors[lvl.sidedefs[back]!.sector]!
      floorZ := max floorZ (max f.floorH b.floorH)
      ceilZ := min ceilZ (min f.ceilH b.ceilH)
  return some (floorZ, ceilZ)

/-- Vanilla `P_RecursiveSound`: flood a gunshot's alert out from sector
`start` through open two-sided lines, crossing at most one `ML_SOUNDBLOCK`
line, and mark every sector it reaches. Accumulates into `seed` (never
clears) so an alerted sector stays alerted — the lingering `soundtarget`. A
shut opening (a closed door) stops the sound, as it does in vanilla. -/
def soundFlood (lvl : Level) (start : Nat) (seed : Array Bool) :
    Array Bool := Id.run do
  let n := lvl.sectors.size
  if start ≥ n then return seed
  -- Search carrying how many soundblock lines have been crossed (0 or 1).
  -- Each sector's own line list is consulted as it is reached — vanilla's
  -- `P_RecursiveSound` over `sec->lines[]` — rather than building an
  -- adjacency for the entire map on every shot.
  let mut trav := Array.replicate n 9          -- min blocks to reach; 9 = unreached
  let mut alerted := if seed.size == n then seed else Array.replicate n false
  trav := trav.set! start 0
  let mut work : Array (Nat × Nat) := #[(start, 0)]
  -- a sector can enter the queue at most twice (once per soundblock count)
  for _ in [0 : 2 * n + 2] do
    if work.isEmpty then break
    let (sec, blocks) := work.back!
    work := work.pop
    alerted := alerted.set! sec true
    let some lines := lvl.sectorLines[sec]? | continue
    for li in lines do
      let line := lvl.linedefs[li]!
      let some back := line.back | continue
      let fs := lvl.sidedefs[line.front]!.sector
      let bs := lvl.sidedefs[back]!.sector
      if fs == bs then continue
      let f := lvl.sectors[fs]!
      let b := lvl.sectors[bs]!
      if min f.ceilH b.ceilH ≤ max f.floorH b.floorH then continue  -- shut
      let other := if fs == sec then bs else fs
      let sb := line.has Linedef.soundBlock
      if sb && blocks != 0 then continue       -- a second soundblock stops it
      let nb := if sb then blocks + 1 else blocks
      if nb < trav[other]! then
        trav := trav.set! other nb
        work := work.push (other, nb)
  return alerted

end Level
end Dill
