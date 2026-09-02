/-!
# The FFI boundary

Fourteen opaque functions implemented in `c/shell.c` (SDL3 window + Vulkan
present, audio, and PNG decode), plus the pure decoding of the input
snapshot they report.

This is the only module besides `Main` that mentions `IO`.
-/

namespace Dill.Shell

/-- Open the window and bring up Vulkan. Call once. -/
@[extern "dill_init"]
opaque init (width height : UInt32) (title : @&String) : IO Unit

/-- Present a 426×200 RGBA frame (340 800 bytes). How it lands on the
display is set by `setAspect` and `setFit`, not fixed here. -/
@[extern "dill_present"]
opaque present (frame : @&ByteArray) : IO Unit

/-- Pump window events; returns the packed input snapshot. -/
@[extern "dill_poll"]
opaque poll : IO UInt64

/-- Choose the presented aspect: classic 4:3 crop, or the full 16:9. -/
@[extern "dill_aspect"]
opaque setAspect (classic : UInt32) : IO Unit

/-- Letterbox the whole frame (so nothing is cropped) instead of the
edge-cropping cover fill. Set on menu/title screens, off in gameplay. -/
@[extern "dill_fit"]
opaque setFit (on : UInt32) : IO Unit

/-- Load a PNG overlay from `path`, decoded at its native pixel size and
composited 1:1 over every frame using its alpha channel — no scaling,
anchored at the top-left of the display. Returns whether it loaded. Call
after `init`. -/
@[extern "dill_set_overlay"]
opaque setOverlay (path : @&String) : IO UInt32

/-- Decode a PNG scaled to `w × h`, as `w*h*4` premultiplied-RGBA bytes.
`none` if the file is missing or unreadable. -/
@[extern "dill_decode_png"]
opaque decodePng (path : @&String) (w h : UInt32) : IO (Option ByteArray)

/-- Load a raw 8-bit mono sound clip into a mixer slot. -/
@[extern "dill_sound_load"]
opaque soundLoad (id : UInt32) (rate : UInt32) (data : @&ByteArray) : IO Unit

/-- Play a loaded clip at a gain (0–1). -/
@[extern "dill_sound_play"]
opaque soundPlay (id : UInt32) (gain : Float) : IO Unit

/-- Play a Standard MIDI File through the system synth, looping forever. -/
@[extern "dill_music_play"]
opaque musicPlay (midi : @&ByteArray) : IO Unit

/-- Silence the music. -/
@[extern "dill_music_stop"]
opaque musicStop : IO Unit

/-- Up to 8 letters/digits typed since the last call, packed low-first
(0 = none). Feeds the cheat-code scanner. -/
@[extern "dill_typed"]
opaque typed : IO UInt64

/-- Milliseconds since startup. -/
@[extern "dill_ticks"]
opaque ticks : IO UInt32

/-- Tear down Vulkan and the window. -/
@[extern "dill_shutdown"]
opaque shutdown : IO Unit

end Shell

/-- One sampled moment of player intent. The simulation consumes these at
35 Hz; it never sees SDL events. -/
structure Input where
  forward     : Bool := false
  back        : Bool := false
  strafeLeft  : Bool := false
  strafeRight : Bool := false
  turnLeft    : Bool := false
  turnRight   : Bool := false
  run         : Bool := false
  use         : Bool := false
  /-- Ctrl or left mouse button. -/
  fire        : Bool := false
  /-- Esc: pause toggle (edge-triggered by the game loop). -/
  pause       : Bool := false
  /-- F5 / F9: quicksave and quickload. -/
  save        : Bool := false
  load        : Bool := false
  /-- Return: confirm in menus. -/
  enter       : Bool := false
  /-- Tab: toggle the automap (edge-triggered by the game loop). -/
  map         : Bool := false
  /-- Number keys 1–7: fist/chainsaw, pistol, shotgun/super shotgun,
  chaingun, rocket, plasma, BFG. `none` = no switch. -/
  weapon      : Option Nat := none
  quit        : Bool := false
  /-- Mouse x movement since the last poll, in SDL pixels (+ = right). -/
  mouseDx     : Int := 0
  deriving Repr, Inhabited

namespace Input

/-- Unpack the shell's snapshot. Bit layout matches `c/shell.c`:
held-key bits low, quit at bit 15, signed 16-bit mouse dx in bits 48–63. -/
def decode (raw : UInt64) : Input :=
  let bit (n : Nat) : Bool := (raw >>> UInt64.ofNat n) &&& 1 == 1
  let dxRaw := (raw >>> 48).toNat
  { forward     := bit 0
    back        := bit 1
    strafeLeft  := bit 2
    strafeRight := bit 3
    turnLeft    := bit 4
    turnRight   := bit 5
    run         := bit 6
    use         := bit 7
    fire        := bit 8
    weapon      := if bit 9 then some 1 else if bit 10 then some 2
                   else if bit 11 then some 3 else if bit 12 then some 4
                   else if bit 20 then some 5 else if bit 21 then some 6
                   else if bit 22 then some 7 else none
    pause       := bit 13
    save        := bit 16
    load        := bit 17
    enter       := bit 19
    map         := bit 23
    quit        := bit 15
    mouseDx     := if dxRaw < 0x8000 then dxRaw else (dxRaw : Int) - 0x10000 }

end Input
end Dill
