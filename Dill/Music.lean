import Dill.Bytes

/-!
# Music: MUS → MIDI

Doom's music lumps (`D_E1M1`, …) are MUS: a trimmed MIDI dialect running at
140 tics per second, with delays *after* events and a couple of packed
encodings. `musToMidi` converts a lump into a Standard MIDI File (format 0)
that any General MIDI synthesizer can play — the shell hands it to the
operating system's synth.

The classic mapping (`mus2mid`): MUS channel 15 is percussion (MIDI 9);
controller 0 is a program change; note-on volume is sticky per channel.
Timing is exact: 140 MIDI ticks per quarter note at one second per quarter
makes one tick = one MUS tic.
-/

namespace Dill.Music

open Dill.Bytes

/-- MUS controller number → MIDI controller number (index 1–9). -/
private def controllerMap : Array UInt8 :=
  #[0, 0, 1, 7, 10, 11, 91, 93, 64, 67]

/-- MUS channel → MIDI channel: percussion 15 → 9, and 9–14 shift up to
keep clear of it. -/
private def midiChannel (c : Nat) : UInt8 :=
  if c == 15 then 9 else if c ≥ 9 then UInt8.ofNat (c + 1) else UInt8.ofNat c

/-- Append a MIDI variable-length quantity. -/
private def pushVarLen (out : ByteArray) (v : Nat) : ByteArray := Id.run do
  let mut chunks : List UInt8 := [UInt8.ofNat (v % 128)]
  let mut v := v / 128
  while v > 0 do
    chunks := UInt8.ofNat (v % 128 ||| 128) :: chunks
    v := v / 128
  let mut out := out
  for c in chunks do
    out := out.push c
  return out

private def pushU32be (out : ByteArray) (v : Nat) : ByteArray :=
  (((out.push (UInt8.ofNat (v >>> 24 &&& 0xFF))).push
    (UInt8.ofNat (v >>> 16 &&& 0xFF))).push
    (UInt8.ofNat (v >>> 8 &&& 0xFF))).push (UInt8.ofNat (v &&& 0xFF))

/-- Convert a MUS lump to a format-0 Standard MIDI File. -/
def musToMidi (mus : ByteArray) : Except String ByteArray := do
  if mus.size < 16 then throw "MUS lump too small"
  if !(mus.get! 0 == 0x4D && mus.get! 1 == 0x55
      && mus.get! 2 == 0x53 && mus.get! 3 == 0x1A) then
    throw "not a MUS lump"
  let scoreLen := (u16le mus 4).toNat
  let scoreStart := (u16le mus 6).toNat
  let scoreEnd := min mus.size (scoreStart + scoreLen)

  -- Track body: tempo one second per quarter, then the converted score.
  let mut track := ByteArray.empty
  track := track ++ ⟨#[0x00, 0xFF, 0x51, 0x03, 0x0F, 0x42, 0x40]⟩
  let mut velocity : Array UInt8 := Array.replicate 16 100
  let mut delta := 0
  let mut p := scoreStart
  let mut done := false
  while !done && p < scoreEnd do
    let desc := mus.get! p
    p := p + 1
    let kind := (desc >>> 4 &&& 7).toNat
    let musChan := (desc &&& 15).toNat
    let ch := midiChannel musChan
    match kind with
    | 0 =>  -- release note
      if p ≥ scoreEnd then done := true
      else
        let note := mus.get! p &&& 0x7F
        p := p + 1
        track := pushVarLen track delta
        track := ((track.push (0x80 + ch)).push note).push 64
        delta := 0
    | 1 =>  -- play note, sticky volume
      if p ≥ scoreEnd then done := true
      else
        let b := mus.get! p
        p := p + 1
        if b &&& 0x80 != 0 then
          if p ≥ scoreEnd then done := true
          else
            velocity := velocity.set! musChan (min 127 (mus.get! p))
            p := p + 1
        if !done then
          track := pushVarLen track delta
          track := ((track.push (0x90 + ch)).push (b &&& 0x7F)).push
            velocity[musChan]!
          delta := 0
    | 2 =>  -- pitch wheel: 0–255, 128 centered → 14-bit MIDI bend
      if p ≥ scoreEnd then done := true
      else
        let v := (mus.get! p).toNat * 64
        p := p + 1
        track := pushVarLen track delta
        track := ((track.push (0xE0 + ch)).push
          (UInt8.ofNat (v &&& 0x7F))).push (UInt8.ofNat (v >>> 7))
        delta := 0
    | 3 =>  -- system event → MIDI channel-mode controller
      if p ≥ scoreEnd then done := true
      else
        let sys := (mus.get! p &&& 0x7F).toNat
        p := p + 1
        -- MUS defines system events 10–14 only; drop anything else (keeping
        -- its delay in `delta`) rather than emit a spurious all-notes-off —
        -- the same policy as unknown controllers below
        let cc? : Option UInt8 := match sys with
          | 10 => some 120 | 11 => some 123 | 12 => some 126
          | 13 => some 127 | 14 => some 121
          | _ => none
        if let some cc := cc? then
          track := pushVarLen track delta
          track := ((track.push (0xB0 + ch)).push cc).push 0
          delta := 0
    | 4 =>  -- controller; number 0 is a program change
      if p + 1 ≥ scoreEnd then done := true
      else
        let num := (mus.get! p &&& 0x7F).toNat
        let val := min 127 (mus.get! (p + 1))
        p := p + 2
        if num == 0 then
          track := pushVarLen track delta
          track := (track.push (0xC0 + ch)).push val
          delta := 0
        else if h : num < controllerMap.size then
          track := pushVarLen track delta
          track := ((track.push (0xB0 + ch)).push controllerMap[num]).push val
          delta := 0
        -- MUS defines controllers 0–9 only; drop anything else (keeping its
        -- delay accumulated in `delta`) rather than emit a random pedal event
    | 6 =>  -- score end
      done := true
    | 5 =>  -- end of measure: nothing
      pure ()
    | _ =>  -- unused event with one payload byte
      p := p + 1
    -- a set top bit on the event byte means a delay follows
    if !done && desc &&& 0x80 != 0 then
      let mut v := 0
      let mut more := true
      while more && p < scoreEnd do
        let b := mus.get! p
        p := p + 1
        v := v * 128 + (b &&& 0x7F).toNat
        more := b &&& 0x80 != 0
      delta := delta + v
  track := track ++ ⟨#[0x00, 0xFF, 0x2F, 0x00]⟩

  -- Assemble: format 0, one track, 140 ticks per (one-second) quarter.
  let mut out : ByteArray := ⟨#[0x4D, 0x54, 0x68, 0x64, 0, 0, 0, 6,
                               0, 0, 0, 1, 0, 140]⟩
  out := out ++ ⟨#[0x4D, 0x54, 0x72, 0x6B]⟩
  out := pushU32be out track.size
  return out ++ track

/-- Is this lump already a Standard MIDI File? -/
def isMidi (b : ByteArray) : Bool :=
  b.size ≥ 4 && b.get! 0 == 0x4D && b.get! 1 == 0x54
             && b.get! 2 == 0x68 && b.get! 3 == 0x64   -- "MThd"

/-- A music lump as playable MIDI, whichever way the WAD stores it.

The IWADs use MUS, but a PWAD may drop a Standard MIDI File straight into
`D_*` — SIGIL and SIGIL II both do — and those need passing through rather
than rejecting as malformed MUS. -/
def toMidi (b : ByteArray) : Except String ByteArray :=
  if isMidi b then .ok b else musToMidi b

end Dill.Music
