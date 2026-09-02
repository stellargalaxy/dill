/-!
# Byte-level readers

Doom's WAD format is a stream of little-endian integers and 8-byte,
NUL-padded ASCII names. These helpers read them out of a `ByteArray`.

Callers are expected to bounds-check once per record (parsers validate that a
whole lump or directory lies inside the file), after which `get!` cannot fail.
-/

namespace Dill.Bytes

/-- Read a little-endian `UInt16` at byte offset `i`. -/
def u16le (b : ByteArray) (i : Nat) : UInt16 :=
  (b.get! i).toUInt16 ||| (b.get! (i + 1)).toUInt16 <<< 8

/-- Read a little-endian `UInt32` at byte offset `i`. -/
def u32le (b : ByteArray) (i : Nat) : UInt32 :=
  (b.get! i).toUInt32
    ||| (b.get! (i + 1)).toUInt32 <<< 8
    ||| (b.get! (i + 2)).toUInt32 <<< 16
    ||| (b.get! (i + 3)).toUInt32 <<< 24

/-- Read a little-endian signed 16-bit value at byte offset `i`.

Returned as `Int`: map coordinates and heights are small, and unbounded
integers keep the arithmetic downstream free of overflow concerns. -/
def i16le (b : ByteArray) (i : Nat) : Int :=
  let u := (u16le b i).toNat
  if u < 0x8000 then u else (u : Int) - 0x10000

/-- Read a little-endian signed 16-bit value as a `Float`. Map geometry is
stored as 16-bit integers but all our math is `Float`. -/
def f16le (b : ByteArray) (i : Nat) : Float :=
  Float.ofInt (i16le b i)

/-- Read `len` bytes at offset `i` as an ASCII string. -/
def ascii (b : ByteArray) (i len : Nat) : String := Id.run do
  let mut s := ""
  for j in [0:len] do
    s := s.push (Char.ofNat (b.get! (i + j)).toNat)
  return s

/-- Read one of Doom's 8-byte names at offset `i`: ASCII, NUL-padded. -/
def name8 (b : ByteArray) (i : Nat) : String := Id.run do
  let mut s := ""
  for j in [0:8] do
    let c := b.get! (i + j)
    if c == 0 then break
    s := s.push (Char.ofNat c.toNat)
  return s

end Dill.Bytes
