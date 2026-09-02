import Dill

/-!
# Shared test support

The pieces both test binaries need: the pass/fail counter, the map-loading
helpers, and the renderer completeness probe (which `tests` runs over a
handful of maps and `wadtests` runs over every episode of every WAD).

`Tests.lean` is one very large function and the compiler charges for it, so
anything two suites share lives here rather than being duplicated — and the
heavy WAD sweep lives in its own module so editing it does not force a
recompile of the fast suite.
-/

open Dill

structure TestRun where
  failures : Nat := 0

def check (r : TestRun) (name : String) (cond : Bool) : IO TestRun := do
  if cond then
    IO.println s!"  ok    {name}"
    return r
  else
    IO.println s!"  FAIL  {name}"
    return { r with failures := r.failures + 1 }

/-- Clear the world of mobjs, rebuilding the derived indexes with it. Many
tests want an empty arena, but `mobjs := #[]` alone leaves `uidIndex` and
`mobjGrid` holding indices past the end of the now-empty array, and the next
`mobjsNear` hands those out — `mobjs[i]!` then panics and answers with a
default mobj, so a collision test silently reads a body that is not there. -/
def emptied (g : GameState) : GameState :=
  { g with mobjs := #[] }.rebuildIndexes

/-- Parse a WAD from the project root, or `none` when the file is not there.

Only `doom.wad` is required to run the suite; the rest of the IWADs are
whatever happens to be in the tree, so a test that wants one has to be able
to skip. A file that *is* present and fails to parse is a different matter
and says so — silence there would let a broken WAD read as a clean run,
which is the same distinction `loadMaps` draws below. -/
def loadWadIfPresent (path : String) : IO (Option Wad) := do
  unless ← System.FilePath.pathExists path do return none
  match Wad.parse (← IO.FS.readBinFile path) with
  | .ok wad => return some wad
  | .error e => IO.eprintln s!"  {path}: {e}"; return none

/-- Decode the maps a WAD actually carries, keeping the failures.

The sweeps below run a *speculative* list of names (`E1M1`…`E4M9`,
`MAP01`…`MAP32`) against whatever WAD is loaded, so a name the WAD simply
does not hold is not a failure — but a marker it *does* hold that fails to
decode is one, and every sweep used to write that case off as
`| .error _ => pure ()`. A map that stopped loading therefore made those
tests pass while reporting nothing; splitting "absent" from "broken" here is
what lets them fail instead. -/
def loadMaps (wad : Wad) (names : List String) :
    Array (String × Level) × Array (String × String) := Id.run do
  let mut ok := #[]
  let mut bad := #[]
  for name in names do
    if (wad.lastIndexOf? name).isNone then continue   -- not in this WAD
    match Level.load wad name with
    | .ok lvl => ok := ok.push (name, lvl)
    | .error e => bad := bad.push (name, e)
  return (ok, bad)

/-- Every map marker a WAD carries, in directory order and without repeats.
Taken from the lump names themselves rather than a hand-kept list, so an
episode nobody thought of — SIGIL's `E5M*`, SIGIL II's `E6M*` — is covered
the moment the file is present. -/
def mapMarkers (wad : Wad) : Array String := Id.run do
  let mut out := #[]
  for l in wad.lumps do
    if (MapId.parse l.name).isSome && !out.contains l.name then
      out := out.push l.name
  return out

/-- The `ExMy` and `MAPnn` names the sweeps speculate over. -/
def doomMapNames : List String :=
  (List.range 4).flatMap fun e => (List.range 9).map fun m => s!"E{e+1}M{m+1}"
def doom2MapNames : List String :=
  (List.range 32).map fun m => if m + 1 < 10 then s!"MAP0{m+1}" else s!"MAP{m+1}"

/-- Renderer completeness: every pixel of every probed frame is painted.

`only` restricts the sweep to named maps; empty means every `ExMy` the WAD
carries. `viewsPerMap` bounds the cost, since two full renders per view is
what makes a probe expensive. The fast suite spends its budget on three
maps known to exercise the sky-wall paths; `wadtests` spends a smaller
per-map budget across every episode instead. -/
def rendererCompletenessTests (r0 : TestRun) (wad : Wad)
    (only : Array String := #[]) (viewsPerMap : Nat := 5) : IO TestRun := do
  let mut r := r0
  IO.println "renderer completeness (no unpainted pixels):"
  match Assets.load wad with
  | .ok assets =>
    -- Every pixel of every frame must be painted by a wall, flat, or sky.
    -- Render from a spread of thing placements (guaranteed playable spots).
    -- A sentinel-filled frame exposes anything the passes miss — but a
    -- painted texel can legitimately *equal* any one sentinel (FIREBLU
    -- comes out of the colormap as palette index 254), so each view is
    -- rendered over two different sentinels and only a pixel left at its
    -- sentinel in *both* frames counts as a hole. E1M8 and E3M9 join E1M1
    -- for their solid sky barriers ("sky walls"), which once left
    -- sealed-but-unpainted bands above their lower textures.
    --
    -- Scope, and why it is not "every map of every WAD": this is not a
    -- property Doom itself satisfies. A closed portal whose facing sidedef
    -- carries no texture seals its columns and paints nothing into them —
    -- vanilla's Hall of Mirrors — and real maps have them (doom2 MAP28's
    -- linedef 547 is one: back ceiling 128 against front floor 128, with
    -- `upper`, `lower` and `middle` all "-"). Sweeping the Doom II lineage
    -- would be asserting DILL renders geometry the original could not.
    -- Every map of the *Doom 1* lineage does satisfy it, so the sweep runs
    -- over all of them — 36 maps and any `ExMy` a PWAD adds, where it used
    -- to run over three.
    let paintOver := fun (lvl : Level) (view : Render.View) (sen : UInt8) =>
      Id.run do
        let ctx : Render.Ctx := { level := lvl, assets, view }
        let init := { Render.DrawState.init with
          frame := ByteArray.mk
            (Array.replicate (Render.screenW * Render.screenH) sen) }
        let paint : Render.RenderM Unit := do
          let masked ← Render.renderChild ctx (lvl.nodes.size + 1)
            (BspChild.node (lvl.nodes.size - 1))
          Render.drawPlanes ctx
          for m in masked do Render.drawMasked ctx m
        return ((paint.run init).2).frame
    -- The eye goes through `Level.eyeZ`, the clamp the game itself uses.
    -- Spelling `floorH + 41` out here instead put the camera *above the
    -- ceiling* in anything shorter than a room — E1M4's 24-tall duct (floor
    -- 160, ceiling 184) reported a clean half-screen of "holes" from
    -- outside the map — so the probe reported bugs that only its own
    -- camera had.
    let holesAt := fun (lvl : Level) (x y angle : Float) => Id.run do
      let sec := lvl.sectors[lvl.sectorAt x y]!
      let view : Render.View :=
        { x, y, height := lvl.eyeZ x y sec.floorH Player.viewHeight, angle }
      let a := paintOver lvl view 254
      let b := paintOver lvl view 253
      let mut n := 0
      for i in [0 : Render.screenW * Render.screenH] do
        if a.get! i == 254 && b.get! i == 253 then n := n + 1
      return n
    -- Maps whose probed views legitimately show unpainted pixels. Each was
    -- traced to the linedef responsible, and all three are the same thing:
    -- a wall that *seals* its columns while carrying no texture to paint
    -- them with. `drawSeg` runs `sealZColumn` and `markSolid`, so nothing
    -- behind shows through and the band stays at the frame's background —
    -- which is exactly Doom's Hall of Mirrors. Vanilla fills it with
    -- whatever was in the framebuffer; DILL leaves it black, which is at
    -- least deterministic. Neither is a renderer fault.
    --   E1M2 — linedef 134, a portal sealed shut (back ceiling under front
    --     floor) with `upper`, `lower` and `middle` all "-".
    --   E1M4 — linedef 289, the same with only the upper textured: the band
    --     below it has no `lower` to draw.
    --   E4M8 — linedef 425, a *one-sided* wall whose `middle` is "-",
    --     covering a single row (112) of nine columns.
    --
    -- It is tempting to derive this from the z-buffer instead — a sealed
    -- band carries depth, so "unpainted but no depth" would need no list.
    -- Do not: the bug this probe exists to catch (a sky wall that failed to
    -- mark its ceiling plane, leaving a black band over its lower texture)
    -- is *also* sealed-and-depth-carrying, and that rule waves it through.
    -- Checked by reintroducing it: the z-buffer form passed, this one fails.
    let knownHoles : Array String := #["E1M2", "E1M4", "E4M8"]
    -- the named maps, or every `ExMy` this WAD carries
    let episodeMarkers := (mapMarkers wad).filter fun n =>
      if !only.isEmpty then only.contains n
      else match MapId.parse n with
        | some (.episode ..) => true
        | _ => false
    let (probeMaps, probeFails) := loadMaps wad episodeMarkers.toList
    r ← check r "the render probe's maps all decode" probeFails.isEmpty
    -- A fixed budget of views per map, strided across the thing list so
    -- they land in different rooms.
    let mut holeMaps : Array (String × Nat) := #[]
    for (mapName, lvl) in probeMaps do
      let stride := max 1 (lvl.things.size / viewsPerMap)
      let mut totalHoles := 0
      let mut k := 0
      let mut taken := 0
      for t in lvl.things do
        k := k + 1
        if k % stride != 0 || taken ≥ viewsPerMap then continue
        -- A spot the player could never occupy proves nothing. Teleport
        -- destination markers (type 14) sit in zero-height sectors —
        -- E2M1's is at floor = ceiling = 128 — and a camera sealed inside
        -- one sees the inside of a closed box: a whole blank frame, which
        -- the probe would report as 85 200 holes.
        let sec := lvl.sectors[lvl.sectorAt t.x t.y]!
        if sec.ceilH - sec.floorH < Player.height then continue
        taken := taken + 1
        totalHoles := totalHoles + holesAt lvl t.x t.y (Float.ofNat k)
      if totalHoles > 0 then holeMaps := holeMaps.push (mapName, totalHoles)
    let unexpected := holeMaps.filter fun (n, _) => !knownHoles.contains n
    unless holeMaps.isEmpty do
      IO.println s!"  (maps with unpainted pixels: {holeMaps.toList})"
    r ← check r
      s!"no unexpected map among {probeMaps.size} leaves pixels unpainted"
      unexpected.isEmpty
  | .error e =>
    IO.eprintln s!"  setup failed: {e}"
    r ← check r "assets load for the render probe" false
  return r
