import Std.Data.HashMap
import Dill.Bytes

/-!
# WAD files

A WAD ("Where's All the Data") is Doom's archive format: a 12-byte header, a
blob of raw lump data, and a directory of `(offset, size, name)` entries at
the end. Names are not unique — maps reuse lump names like `THINGS`, and
sprite/flat sections are delimited by empty marker lumps (`S_START`…`S_END`),
so lookups sometimes need to start from a known index rather than the front.
-/

namespace Dill

/-- A directory entry: where a named chunk of data lives in the file.

`name` is uppercased at parse (vanilla's `W_AddFile` does the same with
`strupr`), so every lookup in DILL is by the uppercase name. -/
structure Lump where
  name   : String
  offset : Nat
  size   : Nat
  deriving Repr, Inhabited

/-- `IWAD` is a complete game ("Internal WAD"); `PWAD` is a patch on top of one. -/
inductive Wad.Kind where
  | iwad
  | pwad
  deriving Repr, DecidableEq, Inhabited

/-- A parsed WAD: the raw bytes plus the decoded lump directory.

`byName` maps each lump name to its *last* index in the directory — the one
`lastIndexOf?`/`find?` mean — so whole-directory lookups are O(1) instead of
a scan. It is maintained by `parse` and `merge`, the only two ways a `Wad`
is built; it deliberately has no default, so a hand-rolled literal is forced
to supply the index rather than silently getting an empty one that would
make every `find?` miss. -/
structure Wad where
  kind   : Wad.Kind
  bytes  : ByteArray
  lumps  : Array Lump
  byName : Std.HashMap String Nat

namespace Wad

/-- Decode the header and lump directory. Pure; fails with a descriptive
message rather than panicking on malformed input. -/
def parse (bytes : ByteArray) : Except String Wad := do
  if bytes.size < 12 then
    throw s!"file too small to be a WAD ({bytes.size} bytes)"
  let kind ← match Bytes.ascii bytes 0 4 with
    | "IWAD" => pure Kind.iwad
    | "PWAD" => pure Kind.pwad
    | magic  => throw s!"not a WAD file (magic {magic.quote})"
  let count  := (Bytes.u32le bytes 4).toNat
  let dirOfs := (Bytes.u32le bytes 8).toNat
  if dirOfs + count * 16 > bytes.size then
    throw s!"lump directory out of range (offset {dirOfs}, {count} entries, \
             file size {bytes.size})"
  let mut lumps := Array.mkEmpty count
  let mut byName : Std.HashMap String Nat := ∅
  for k in [0:count] do
    let entry := dirOfs + k * 16
    let lump : Lump :=
      { offset := (Bytes.u32le bytes entry).toNat
        size   := (Bytes.u32le bytes (entry + 4)).toNat
        -- vanilla `strupr`s every name on load; a few PWADs store lowercase
        name   := (Bytes.name8 bytes (entry + 8)).toUpper }
    if lump.size > 0 && lump.offset + lump.size > bytes.size then
      throw s!"lump {lump.name.quote} extends past end of file"
    lumps := lumps.push lump
    byName := byName.insert lump.name k   -- later entries win
  return { kind, bytes, lumps, byName }

/-- The raw bytes of a lump. -/
def data (w : Wad) (l : Lump) : ByteArray :=
  w.bytes.extract l.offset (l.offset + l.size)

/-- Index of the first lump named `name`, at or after `start`.

Map lumps (`THINGS`, `LINEDEFS`, …) only mean something relative to their
map marker, so map loading passes the marker's index as `start`.

A scan, not a `byName` lookup — that index only remembers each name's *last*
entry, which is the wrong answer for a name that recurs once per map. The
scan is bounded in practice by callers starting it at the marker and giving
up a handful of lumps later (`Level.mapWindow`). -/
def indexOf? (w : Wad) (name : String) (start : Nat := 0) : Option Nat := do
  for k in [start:w.lumps.size] do
    if w.lumps[k]!.name == name then
      return k
  none

/-- Index of the *last* lump named `name`.

Loading a PWAD appends its directory after the IWAD's, and a PWAD's copy of
a lump is meant to replace the one it shadows — so whole-directory lookups
resolve to the last match, not the first. In a lone IWAD nothing is
shadowed and this is the same lump either way. O(1) via `byName`. -/
def lastIndexOf? (w : Wad) (name : String) : Option Nat :=
  w.byName.get? name

/-- The effective lump named `name`: the last one in the directory. -/
def find? (w : Wad) (name : String) : Option Lump := do
  w.lumps[← w.lastIndexOf? name]?

/-- Load a PWAD over a base WAD, the way Doom's `-file` does: the patch's
bytes are appended and its lump offsets rebased, so the result is still one
flat `Wad` and every reader downstream is unchanged. Shadowing falls out of
`find?`/`lastIndexOf?` preferring the later entry; new lumps (a fresh
episode's maps, say) simply extend the directory. -/
def merge (base patch : Wad) : Wad :=
  let shift := base.bytes.size
  let moved := patch.lumps.map fun l => { l with offset := l.offset + shift }
  -- the patch's entries sit after the base's, so its last index for a name
  -- (rebased by the base's directory length) shadows the base's
  let byName := patch.byName.fold (init := base.byName) fun m name idx =>
    m.insert name (idx + base.lumps.size)
  { base with bytes := base.bytes ++ patch.bytes
              lumps := base.lumps ++ moved
              byName }

/-- Like `find?`, but a parse error naming the missing lump. -/
def find (w : Wad) (name : String) : Except String Lump :=
  match w.find? name with
  | some l => .ok l
  | none   => .error s!"required lump {name.quote} not found"

end Wad
end Dill
