import Dill
import TestSupport

/-!
Golden tests against the real `doom.wad` in the project root.
Run with `lake test`. Every group runs whatever the ones before it did, so a
failure does not mask the rest; the failures are counted as they go and the
binary exits nonzero at the end if any were seen.
-/

open Dill

/-- The two geometry indexes built at load. Split out of `main` because the
whole suite lives in one function and the compiler has a budget for it. -/
def geometryIndexTests (r0 : TestRun) (lvl : Level) : IO TestRun := do
  let mut r := r0
  -- The automap reveal, the noise flood and the neighbour heights all read
  -- the per-sector line list instead of sweeping the map, so a line missing
  -- from a sector's list would quietly cut that sector off.
  let wrong := Id.run do
    let mut bad := 0
    for s in [0 : lvl.sectors.size] do
      let indexed := lvl.sectorLines[s]!
      for i in [0 : lvl.linedefs.size] do
        let l := lvl.linedefs[i]!
        let touches := lvl.sidedefs[l.front]!.sector == s
          || (l.back.map (lvl.sidedefs[·]!.sector)) == some s
        if touches != indexed.contains i then bad := bad + 1
    return bad
  r ← check r "every sector's line list matches a full sweep" (wrong == 0)
  let scrollTruth := Id.run do
    let mut n := 0
    for i in [0 : lvl.linedefs.size] do
      if lvl.linedefs[i]!.special == 48 then n := n + 1
    return n
  r ← check r "the scroll index holds exactly the special-48 lines"
    (lvl.scrollLines.size == scrollTruth
      && lvl.scrollLines.all (fun i => lvl.linedefs[i]!.special == 48))
  -- The blockmap ray walk must be a *superset* of what a ray really crosses:
  -- miss one line and shots or sight lines pass through a wall.
  let pts := lvl.things.map (fun t => (t.x, t.y))
  let n := min 20 pts.size
  let (missed, visited, rays) := Id.run do
    let mut missed := 0
    let mut visited := 0
    let mut rays := 0
    for a in [0:n] do
      for b in [0:n] do
        if a == b then continue
        let (x1, y1) := pts[a]!
        let (x2, y2) := pts[b]!
        let dx := x2 - x1
        let dy := y2 - y1
        let walk := lvl.linesAlong x1 y1 x2 y2
        rays := rays + 1
        visited := visited + walk.size
        for i in [0:lvl.linedefs.size] do
          let l := lvl.linedefs[i]!
          let p1 := lvl.vertexes[l.v1]!
          let p2 := lvl.vertexes[l.v2]!
          let ldx := p2.x - p1.x
          let ldy := p2.y - p1.y
          let denom := dx * ldy - dy * ldx
          if denom == 0 then continue
          let t := ((p1.x - x1) * ldy - (p1.y - y1) * ldx) / denom
          let u := ((p1.x - x1) * dy - (p1.y - y1) * dx) / denom
          if t ≤ 0 || t ≥ 1 || u < 0 || u > 1 then continue
          if !walk.contains i then missed := missed + 1
    return (missed, visited, rays)
  r ← check r "the ray walk never misses a linedef the ray crosses" (missed == 0)
  r ← check r "…and visits far fewer lines than the whole map"
    (visited / rays * 4 < lvl.linedefs.size)
  return r

/-- A little-endian 32-bit value, for assembling synthetic WADs. -/
def le32 (v : Nat) : ByteArray :=
  ByteArray.mk #[UInt8.ofNat (v &&& 0xFF), UInt8.ofNat (v >>> 8 &&& 0xFF),
                 UInt8.ofNat (v >>> 16 &&& 0xFF), UInt8.ofNat (v >>> 24 &&& 0xFF)]

/-- The mobj grid (`MobjGrid`), the thing side of the blockmap. It stands in
for a full sweep of `mobjs`, so it gets the same test the sector-line list
and the blockmap ray walk get: compare the index against the sweep it
replaces. The contract is one-way — a query may hand back more than it
should, never less, because every caller re-tests exactly. Handing back
*less* is a monster walking through a body; handing back an index past the
end of `mobjs` is a phantom body at the origin. -/
def mobjGridTests (r0 : TestRun) (wad : Wad) : IO TestRun := do
  IO.println "the mobj grid (spatial index over mobjs):"
  let mut r := r0
  let .ok lvl := Level.load wad "E1M1"
    | check r "E1M1 loads for the grid tests" false
  let g0 := GameState.newGame lvl
  -- radii spanning what the game actually asks for: a body's own 16–30, a
  -- missile's path bound, a rocket blast's 128
  let radii : Array Float := #[0.0, 16.0, 30.0, 128.0]
  -- Probes: every thing's own spot (where bodies actually cluster), a coarse
  -- sweep of the map for the empty cells, and two points far outside the
  -- blockmap, where nothing but the cell clamping keeps the query honest.
  let probes : Array (Float × Float) := Id.run do
    let mut out := #[]
    for m in g0.mobjs do out := out.push (m.x, m.y)
    let minX := lvl.vertexes.foldl (fun a v => min a v.x) 1.0e9
    let maxX := lvl.vertexes.foldl (fun a v => max a v.x) (-1.0e9)
    let minY := lvl.vertexes.foldl (fun a v => min a v.y) 1.0e9
    let maxY := lvl.vertexes.foldl (fun a v => max a v.y) (-1.0e9)
    for i in [0:8] do
      for j in [0:8] do
        out := out.push (minX + (maxX - minX) * Float.ofNat i / 7.0,
                         minY + (maxY - minY) * Float.ofNat j / 7.0)
    return out.push (minX - 4000.0, minY - 4000.0)
             |>.push (maxX + 4000.0, maxY + 4000.0)
  -- How many live mobjs the full sweep finds within reach of `(x, y)` that
  -- the grid's candidate list left out. Zero is the whole invariant.
  let missed := fun (g : GameState) => Id.run do
    let mut bad := 0
    for (x, y) in probes do
      for radius in radii do
        let near := g.mobjsNear x y radius
        for i in [0:g.mobjs.size] do
          let m := g.mobjs[i]!
          if m.removed then continue
          if m.distanceTo x y < radius + m.info.radius && !near.contains i then
            bad := bad + 1
    return bad
  -- Every index the grid holds must name a live slot, exactly once: a stale
  -- index past the end is what `mobjs[i]!` turns into a phantom body.
  let malformed := fun (g : GameState) => Id.run do
    let mut seen : Array Nat := #[]
    let mut bad := 0
    for cell in g.mobjGrid.cells do
      for i in cell do
        if i ≥ g.mobjs.size || seen.contains i then bad := bad + 1
        seen := seen.push i
    return bad
  r ← check r "a query never misses a body the full sweep finds"
    (missed g0 == 0)
  r ← check r "every cell entry indexes a live mobj, exactly once"
    (malformed g0 == 0)
  -- `setMobj` is the only path a live body's position may take, and it is
  -- what keeps the grid exact. Shove every mobj a few cells over — 128 units
  -- is one cell, so these all cross — and the sweep must still agree.
  let shoved := Id.run do
    let mut g := g0
    for i in [0:g.mobjs.size] do
      let m := g.mobjs[i]!
      let d := Float.ofNat (i % 7) * 96.0 - 288.0
      g := g.setMobj i { m with x := m.x + d, y := m.y - d }
    return g
  r ← check r "…and still not after every body has been moved across cells"
    (missed shoved == 0 && malformed shoved == 0)
  -- Compaction renumbers `mobjs`, so the grid is rebuilt with it. Retire
  -- every third body and let a tic do the compacting.
  let compacted := Id.run do
    let mut g := shoved
    for i in [0:g.mobjs.size] do
      if i % 3 == 0 then g := g.setMobj i { g.mobjs[i]! with removed := true }
    return g.tickMobjs
  r ← check r "a compaction leaves no stale index behind"
    (compacted.mobjs.size < shoved.mobjs.size
      && missed compacted == 0 && malformed compacted == 0)
  -- The check has teeth. The mistake that really happened here — clearing
  -- `mobjs` and leaving the index behind — must show up as stale entries,
  -- or the four checks above are only asserting that nothing went wrong in
  -- a world where nothing could.
  r ← check r "…and a grid left behind by a cleared roster is caught"
    (malformed { g0 with mobjs := #[] } > 0)
  -- A `GameState` that never built a grid still plays: the query degrades to
  -- the full roster rather than to an empty answer.
  let ungridded := { g0 with mobjGrid := (default : MobjGrid) }
  r ← check r "a state with no grid falls back to the whole roster"
    (ungridded.mobjsNear 0 0 0 == Array.range ungridded.mobjs.size
      && missed ungridded == 0)
  return r

/-- Doom II's *blazing* door and lift families: which specials belong to
them, that the movers actually run at the faster pace, that a reversal keeps
the door's own pace rather than the default, and that a save carries the
speed across (and a pre-version-5 save without it still loads). -/
def blazingTests (r0 : TestRun) (wad : Wad) : IO TestRun := do
  IO.println "blazing doors and lifts (Doom II's fast movers):"
  let mut r := r0
  r ← check r "the blazing speeds are VDOORSPEED×4 and PLATSPEED×8"
    (Speeds.doorBlaze == Speeds.door * 4 && Speeds.doorBlaze == 8
      && Speeds.liftBlaze == Speeds.lift * 2 && Speeds.liftBlaze == 8)
  -- the families, straight off vanilla's linedef table: 105–118 plus the six
  -- key-locked blazing doors. 32–34 are locked but *ordinary* speed, and
  -- nothing from Doom 1 blazes at all.
  let blazeDoors := #[105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
                      116, 117, 118, 99, 133, 134, 135, 136, 137]
  let plainDoors := #[1, 2, 3, 4, 16, 26, 27, 28, 29, 31, 32, 33, 34, 42, 46,
                      50, 61, 63, 75, 76, 86, 90, 103]
  r ← check r "every blazing door special is in the family, and only those"
    (blazeDoors.all GameState.isBlazingDoor && !plainDoors.any GameState.isBlazingDoor)
  let blazeLifts := #[120, 121, 122, 123]
  r ← check r "…and the four blazing lifts, which are not doors"
    (blazeLifts.all GameState.isBlazingLift
      && !(#[10, 21, 62, 88] : Array Nat).any GameState.isBlazingLift
      && !blazeDoors.any GameState.isBlazingLift && !blazeLifts.any GameState.isBlazingDoor)
  let .ok lvl := Level.load wad "E1M1"
    | check r "E1M1 loads for the blazing tests" false
  -- One tic of a mover with nothing in its way: how far the plane travels is
  -- the speed and nothing else. E1M1's first door sector serves as geometry.
  let s := lvl.sectorAt 1500 (-2496)
  let opensBy := fun (speed : Float) =>
    let before := lvl.sectors[s]!.ceilH
    let (lvl', _) := (Mover.door s (before + 1000) 150 false false speed).step lvl
    lvl'.sectors[s]!.ceilH - before
  r ← check r "a blazing door covers four times an ordinary one in a tic"
    (Float.abs (opensBy Speeds.door - 2) < 0.001
      && Float.abs (opensBy Speeds.doorBlaze - 8) < 0.001)
  let sinksBy := fun (speed : Float) =>
    let before := lvl.sectors[s]!.floorH
    let (lvl', _) := (Mover.lift s (before - 1000) before 105 false speed).step lvl
    before - lvl'.sectors[s]!.floorH
  r ← check r "…and a blazing lift sinks twice as fast as an ordinary one"
    (Float.abs (sinksBy Speeds.lift - 4) < 0.001
      && Float.abs (sinksBy Speeds.liftBlaze - 8) < 0.001)
  -- The speed travels with the mover, so a save reloads a door at the pace it
  -- was opened at rather than at the default.
  let doorSpeed := fun (g : GameState) =>
    match g.movers[0]? with
    | some (Mover.door _ _ _ _ _ sp _) => sp
    | _ => (0 : Float)
  let lifted := { GameState.newGame lvl with movers :=
    #[.door s (lvl.sectors[s]!.ceilH + 64) 150 false false Speeds.doorBlaze,
      .lift (lvl.sectorAt 1056 (-3616)) 0 64 105 false Speeds.liftBlaze] }
  match Save.loadGame wad (Save.saveGame lifted) with
  | .error e =>
    IO.eprintln s!"  blazing save failed to load: {e}"
    r ← check r "a blazing door and lift round-trip through a save" false
  | .ok back =>
    r ← check r "a blazing door and lift round-trip through a save"
      (match back.movers[0]?, back.movers[1]? with
       | some (Mover.door _ _ _ _ _ d _), some (Mover.lift _ _ _ _ _ l _) =>
         d == Speeds.doorBlaze && l == Speeds.liftBlaze
       | _, _ => false)
  -- A version-4 save has no speed column at all (4718592 is 72.0 in 16.16):
  -- it must still load, at the ordinary pace.
  let v4 := (Save.saveGame (GameState.newGame lvl)).replace "\nend"
    s!"\nmover door {s} 4718592 150 0 0\nend"
  r ← check r "a pre-version-5 save loads its doors at the ordinary pace"
    (match Save.loadGame wad v4 with
     | .ok old => doorSpeed old == Speeds.door
     | .error _ => false)
  -- End to end on the real thing: MAP02's first door is a 117 (DR, blazing).
  -- Skipped quietly when doom2.wad is not present, as the MAP18 group is.
  if ← System.FilePath.pathExists "doom2.wad" then
    let bytes ← IO.FS.readBinFile "doom2.wad"
    match Wad.parse bytes >>= fun w => Level.load w "MAP02" with
    | .error e => IO.eprintln s!"  MAP02 failed to load: {e}"
    | .ok m2 =>
      match (List.range m2.linedefs.size).find?
          (fun i => m2.linedefs[i]!.special == 117) with
      | none => IO.eprintln "  MAP02 has no special-117 door; skipped"
      | some li =>
        let opened := (GameState.newGame m2).activateLine li (byUse := true)
        r ← check r "MAP02's manual door opens at the blazing speed"
          (opened.movers.size == 1 && doorSpeed opened == Speeds.doorBlaze)
        -- Reversing it mid-open must keep the door's *own* pace: it is the
        -- same door still moving, and a blazing door caught halfway should
        -- slam back rather than crawl.
        let mid := Id.run do
          let mut g := opened
          for _ in [0:3] do g := g.stepMovers
          return g
        let flipped := mid.activateLine li (byUse := true)
        r ← check r "…and still slams when reversed halfway"
          (doorSpeed flipped == Speeds.doorBlaze
            && (match flipped.movers[0]? with
                | some (Mover.door _ _ _ closing _ _ _) => closing
                | _ => false))
  return r

/-- Assemble a minimal PWAD from named lumps: header, lump data, directory. -/
def buildPwad (entries : Array (String × ByteArray)) : ByteArray := Id.run do
  let mut data := ByteArray.empty
  let mut dir := ByteArray.empty
  for (name, bytes) in entries do
    dir := dir ++ le32 (12 + data.size) ++ le32 bytes.size
    let mut n := ByteArray.empty
    for c in name.toList.take 8 do n := n.push (UInt8.ofNat c.toNat)
    while n.size < 8 do n := n.push 0
    dir := dir ++ n
    data := data ++ bytes
  let mut out := "PWAD".toUTF8
  out := out ++ le32 entries.size ++ le32 (12 + data.size)
  return out ++ data ++ dir

/-- Second-pass review fixes: doubled marker sections (`FF_`/`SS_`), the
uppercase name policy, and map-identity edge cases. Own function to stay
inside `main`'s budget. -/
def secondPassTests (r0 : TestRun) (wad : Wad) : IO TestRun := do
  let mut r := r0
  -- A PWAD marking its flats FF_START/FF_END (Sunlust's convention) must
  -- still get them loaded, and a lowercase directory name must resolve
  -- uppercase (vanilla's `W_AddFile` uppercases every name).
  let flat := ByteArray.mk (Array.replicate 4096 7)
  let pwad := buildPwad #[("FF_START", ByteArray.empty),
                          ("newflat", flat),
                          ("FF_END", ByteArray.empty)]
  match Wad.parse pwad with
  | .error e => IO.eprintln s!"  synthetic PWAD failed to parse: {e}"; return r
  | .ok patch =>
    r ← check r "a lowercase lump name is uppercased at parse"
      ((patch.find? "NEWFLAT").isSome && (patch.find? "newflat").isNone)
    match Assets.load (wad.merge patch) with
    | .error e => IO.eprintln s!"  merged assets failed to load: {e}"; return r
    | .ok assets =>
      r ← check r "a flat in an FF_START/FF_END section loads"
        (assets.flats.get? "NEWFLAT" |>.any fun b =>
          b.size == 4096 && b.get! 0 == 7)
      r ← check r "…without disturbing the F_START flats"
        (assets.flats.contains "FLOOR4_8")
  r ← check r "MAP00's music is no music, not MAP01's"
    (MapId.music (.level 0) == "" && MapId.music (.level 1) == "D_RUNNIN")
  r ← check r "Doom II's slime flat animates (vanilla animdefs)"
    (Assets.animName "SLIME01" 8 == "SLIME02"
      && Assets.animName "BFALL4" 8 == "BFALL1"
      && Assets.animName "RROCK07" 8 == "RROCK08")
  -- A post whose length byte would sit past the lump's end: the old reader
  -- fetched it before bounds-checking, panicking on the way to the error.
  r ← check r "a truncated picture post is an error, not a panic"
    (match Picture.parse (ByteArray.mk #[1,0, 1,0, 0,0, 0,0, 12,0,0,0, 5]) with
     | .error _ => true | .ok _ => false)
  return r

/-- Constants and rules taken straight from vanilla, kept in their own
function so `main` stays inside the compiler's budget. -/
def vanillaRuleTests (r0 : TestRun) (wad : Wad) : IO TestRun := do
  let mut r := r0
  r ← check r "mover speeds match VDOORSPEED/PLATSPEED/FLOORSPEED/CEILSPEED"
    (Speeds.door == 2 && Speeds.doorWait == 150
      && Speeds.lift == 4 && Speeds.liftWait == 105
      && Speeds.floor == 1 && Speeds.ceiling == 1
      && Speeds.floorTurbo == 4 && Speeds.stair == 0.25)
  r ← check r "momentum is clamped to MAXMOVE" (Player.maxMove == 30)
  r ← check r "par times match g_game.c's pars[] and cpars[]"
    ([30, 75, 120, 90, 165, 180, 180, 30, 165].zipIdx.all (fun (p, i) =>
        MapId.parTime (.episode 1 (i + 1)) == p)
      && MapId.parTime (.episode 3 2) == 45
      && MapId.parTime (.level 1) == 30 && MapId.parTime (.level 29) == 300
      && MapId.parTime (.level 32) == 30
      -- episode 4 had no row in the original table
      && MapId.parTime (.episode 4 1) == 0)
  -- E1M8's finale floor: cancels god, grinds you down without killing, exits
  match Level.load wad "E1M8" with
  | .error e => IO.eprintln s!"  E1M8: {e}"; return r
  | .ok lvl =>
    let some si := (List.range lvl.sectors.size).find?
      (fun i => lvl.sectors[i]!.special == 11) | return r
    let (cx, cy) := Id.run do
      let mut sx := 0.0
      let mut sy := 0.0
      let mut n := 0.0
      for l in lvl.linedefs do
        let f := lvl.sidedefs[l.front]!.sector
        let b := (l.back.map (lvl.sidedefs[·]!.sector)).getD f
        if f == si || b == si then
          let p1 := lvl.vertexes[l.v1]!
          let p2 := lvl.vertexes[l.v2]!
          sx := sx + p1.x + p2.x; sy := sy + p1.y + p2.y; n := n + 2.0
      return (sx / n, sy / n)
    let g0 := GameState.newGame lvl
    let pz := lvl.sectors[lvl.sectorAt cx cy]!.floorH
    let p := { g0.player with x := cx, y := cy, z := pz }
    let godly := { g0.status with god := true }
    let start := { (emptied g0) with player := p, status := godly }
    let ended := Id.run do
      let mut g := start
      for _ in [0:400] do g := tick {} g
      return g
    r ← check r "the E1M8 finale floor cancels god and exits the level"
      (!ended.status.god && ended.exited
        && ended.status.health > 0 && ended.status.health ≤ 10
        && !ended.status.dead)
    -- `P_CalcHeight`: the eye swings +/- MAXBOB/2 at a run and sits still
    -- otherwise, and a squat always springs back rather than stalling.
    -- E1M1's start has room to run; E1M8's faces a wall.
    let some runway := (Level.load wad "E1M1").toOption | return r
    let g1 := GameState.newGame runway
    let (lo, hi) := Id.run do
      let mut g := emptied g1
      let mut lo := 1.0e9
      let mut hi := -1.0e9
      for k in [0:40] do
        g := tick { forward := true, run := true } g
        if k ≥ 20 then
          let e := g.viewZ - g.player.z
          lo := min lo e
          hi := max hi e
      return (lo, hi)
    -- the swing depends on how much speed the room allows, but it must
    -- happen and must never exceed the cap of MAXBOB/2 either way
    r ← check r "the eye bobs while moving, within MAXBOB/2 either way"
      (hi - lo > 1.0 && hi - lo ≤ Player.maxBob + 0.01
        && lo ≥ Player.viewHeight - Player.maxBob / 2 - 0.01
        && hi ≤ Player.viewHeight + Player.maxBob / 2 + 0.01)
    let stood := Id.run do
      let mut g := emptied g1
      for _ in [0:40] do g := tick {} g
      return g.viewZ - g.player.z
    r ← check r "…and holds steady standing still" (stood == Player.viewHeight)
    let sprung := Id.run do
      let squat := { g1.player with eyeDelta := -1.5 }
      let mut g := { (emptied g1) with player := squat }
      let mut dipped := false
      for _ in [0:60] do
        g := tick {} g
        if g.player.eyeHeight < Player.viewHeight - 4 then dipped := true
      return (dipped, g.player.eyeHeight)
    r ← check r "a squatted eye springs all the way back, never stalling"
      (sprung.1 && sprung.2 == Player.viewHeight)
    return r

/-- The melt screen wipe (`Dill/Render/Wipe.lean`), in its own function
like the other sections. Needs no WAD: it composes two synthetic frames
whose pixels name their own source row, so a composed pixel says which
frame and which row of it the melt took that pixel from. -/
def wipeTests (r0 : TestRun) : IO TestRun := do
  let mut r := r0
  let w := Render.screenW
  let h := Render.screenH
  -- old row y is the byte y + 1 (200 rows fit in a byte); new is all zeroes
  let old := ByteArray.mk (Array.ofFn (n := w * h) fun i =>
    UInt8.ofNat (i.val / w + 1))
  let new := ByteArray.mk (Array.replicate (w * h) 0)
  let (w0, _) := Render.Wipe.init { seed := 0x51ee7 }
  r ← check r "melt columns are 2px wide and start in vanilla's 16-row band"
    (Render.wipeColW == 2 && Render.wipeCols == 213
      && w0.offsets.size == Render.wipeCols
      && w0.offsets.all (fun y => -15 ≤ y && y ≤ 0)
      -- `wipe_initMelt` walks one row at a time, so neighbours stay together
      && (List.range (Render.wipeCols - 1)).all fun c =>
           (w0.offsets[c + 1]! - w0.offsets[c]!).natAbs ≤ 1)

  -- The whole melt: a bounded number of tics, ending with every column off
  -- the bottom. Vanilla's worst case is 15 tics of delay, 5 of acceleration
  -- (0, 1, 3, 7, 15, 31) and 22 more at 8 rows a tic — about 1.2 s at 35 Hz.
  let (tics, wEnd) := Id.run do
    let mut w := w0
    let mut n := 0
    while !w.done && n < 500 do
      w := w.step
      n := n + 1
    return (n, w)
  r ← check r "the melt finishes in ~40 tics with every column past the bottom"
    (20 ≤ tics && tics ≤ 45 && wEnd.done
      && wEnd.offsets.all (· ≥ Int.ofNat h))

  -- Tic 0: every column is still waiting, so nothing of the new screen
  -- shows anywhere — vanilla's wipe opens on the old frame untouched.
  let allOld := Id.run do
    let f := w0.compose old new
    let mut ok := true
    for y in [0:h] do
      for x in [0:w] do
        if f.get! (y * w + x) != UInt8.ofNat (y + 1) then ok := false
    return ok
  r ← check r "at tic 0 nothing is revealed: the frame is the old one, as-is"
    (w0.offsets.all (· ≤ 0) && allOld)

  -- Mid-melt: each column is the new screen down to its offset and the old
  -- screen from its own row 0 below that, with the columns at different
  -- heights (the delays are randomized, so the tear is ragged).
  let wMid := Id.run do
    let mut w := w0
    for _ in [0:22] do w := w.step
    return w
  let shifts := wMid.offsets.map fun y => min h (max 0 y).toNat
  let arranged := Id.run do
    let f := wMid.compose old new
    let mut ok := true
    for c in [0 : Render.wipeCols] do
      let s := shifts[c]!
      for px in [0 : Render.wipeColW] do
        let x := c * Render.wipeColW + px
        for y in [0:h] do
          let want := if y < s then 0 else UInt8.ofNat (y - s + 1)
          if f.get! (y * w + x) != want then ok := false
    return ok
  let lo := shifts.foldl min h
  let hi := shifts.foldl max 0
  r ← check r "mid-melt each column is new above its offset and old below it"
    (arranged && 0 < lo && hi < h)
  r ← check r "…and the columns fall at ragged, randomized heights"
    (hi - lo ≥ 8)

  -- Determinism: the wipe is pure, so one seed gives one melt.
  let melt := fun (seed : UInt32) => Id.run do
    let (w0, _) := Render.Wipe.init { seed }
    let mut w := w0
    let mut frames : Array ByteArray := #[]
    for _ in [0:14] do
      frames := frames.push (w.compose old new)
      w := w.step
    return frames
  let a := melt 0x1ea4c0de
  let b := melt 0x1ea4c0de
  let c := melt 0x0d00d
  r ← check r "the same seed melts identically, a different seed differently"
    ((List.range 14).all (fun i => a[i]!.data == b[i]!.data)
      && !((List.range 14).all fun i => a[i]!.data == c[i]!.data))
  return r

/-- The menus, now that they are pure. `Ui.step` is to the UI what `tick` is
to the game, so it is tested the same way: fold inputs through it and assert
where you end up and what it asked the shell to do. None of this was
reachable before the UI was lifted out of the game loop. -/
def uiTests (r0 : TestRun) (wad : Wad) : IO TestRun := do
  IO.println "the menus and frame composition (pure UI):"
  let mut r := r0
  let .ok assets := Assets.load wad
    | check r "assets load for the UI tests" false
  let .ok lvl := Level.load wad "E1M1"
    | check r "E1M1 loads for the UI tests" false
  let g := GameState.newGame lvl
  let sess : Session :=
    { wad, assets, episodes := #[1, 2, 3, 4]
      episodeItems := #["E1", "E2", "E3", "E4"]
      pickEpisode := false, fit := false }
  let ui0 : Ui := { startMap := "E1M1" }
  -- press a key for one frame: the input arrives, then is released, so the
  -- next step sees a clean edge
  let press := fun (ui : Ui) (i : Input) =>
    let (ui, _, fx) := ui.step sess i "" g
    (ui, fx)
  let tap := fun (ui : Ui) (i : Input) =>
    let (ui, fx) := press ui i
    let (ui, _) := press ui {}
    (ui, fx)
  let enter : Input := { enter := true }
  let down : Input := { back := true }
  let esc : Input := { pause := true }

  let (ui1, _) := tap ui0 enter
  r ← check r "the title screen opens the menu on Enter" (ui1.mode == .menu)

  -- a held key must not scroll: the edge is what counts
  let (held, _) := press ui1 down
  let (held2, _) := press held down
  r ← check r "holding Down moves the highlight exactly once"
    (held.sel == 1 && held2.sel == 1)

  -- four Downs wrap a four-item menu back to the top
  let wrapped := Id.run do
    let mut u := ui1
    for _ in [0:4] do u := (tap u down).1
    return u
  r ← check r "the main menu wraps after its last item" (wrapped.sel == 0)

  -- NEW GAME with a single episode goes straight to the skill picker, with
  -- the highlight defaulted to Ultra-Violence
  let (skillUi, _) := tap ui1 enter
  r ← check r "NEW GAME picks skill, defaulting to Ultra-Violence"
    (skillUi.mode == .skillMenu && skillUi.sel == 3)
  let (started, fx) := tap skillUi enter
  r ← check r "confirming a skill asks for a new game at that skill"
    (started.mode == .playing && fx == #[.newGame "E1M1" 4])

  -- with several episodes on offer, the picker comes first and chooses the map
  let sess4 := { sess with pickEpisode := true }
  let tap4 := fun (ui : Ui) (i : Input) =>
    ((ui.step sess4 i "" g).1.step sess4 {} "" g).1
  let epUi := tap4 (tap4 ui0 enter) enter   -- title → menu → NEW GAME
  r ← check r "the episode picker appears when the IWAD has several"
    (epUi.mode == .episodeMenu)
  let chosen := tap4 (tap4 epUi down) enter -- highlight episode 2, confirm
  r ← check r "choosing episode 2 starts E2M1"
    (chosen.startMap == "E2M1" && chosen.mode == .skillMenu)

  -- QUIT is the fourth item
  let quitUi := Id.run do
    let mut u := ui1
    for _ in [0:3] do u := (tap u down).1
    return u
  let (_, qfx) := tap quitUi enter
  r ← check r "the QUIT item asks the loop to stop" (qfx == #[.quit])

  -- SAVE GAME is inert outside a game, and live inside one
  let saveUi := (tap ui1 down).1
  let (outOfGame, _) := tap saveUi enter
  let (inGame, sfx) := tap { saveUi with inGame := true } enter
  r ← check r "SAVE GAME does nothing before a game has started"
    (outOfGame.mode == .menu)
  r ← check r "…and opens the slot list once one has"
    (inGame.mode == .saveMenu && sfx == #[.readSlots])

  -- Esc backs out to wherever the player came from
  let (escOut, _) := tap ui1 esc
  let (escIn, _) := tap { ui1 with inGame := true } esc
  r ← check r "Esc leaves the menu for the title, or back into the game"
    (escOut.mode == .title && escIn.mode == .playing)

  -- cheats are scanned in the pure step, from the letters the shell typed
  let playing : Ui := { startMap := "E1M1", mode := .playing, inGame := true }
  let (_, godG, _) := playing.step sess {} "dilldqd" g
  r ← check r "typing the god cheat while playing turns it on" godG.status.god
  -- and it takes a fresh buffer, so the same letters do not re-fire
  let (afterCheat, _, _) := playing.step sess {} "dilldqd" g
  r ← check r "…and the cheat buffer is cleared behind it"
    (afterCheat.cheatBuf == "")
  let (_, _, warpFx) := playing.step sess {} "dillclev23" g
  r ← check r "a warp cheat asks the loop to load that map"
    (warpFx == #[.warp "E2M3"])

  -- F5/F9 reach the loop as slot-0 traffic
  let (_, _, quickSave) := playing.step sess { save := true } "" g
  let (_, _, quickLoad) := playing.step sess { load := true } "" g
  r ← check r "F5 and F9 quicksave and quickload through slot 1"
    (quickSave == #[.saveSlot 0] && quickLoad == #[.loadSlot 0])

  -- the tally screen advances to the next map, or ends the episode
  let atExit := { playing with mode := .intermission }
  let (_, _, nextFx) := (atExit.step sess enter "" g)
  r ← check r "the tally screen continues to the next map"
    (nextFx == #[.nextMap "E1M2"])

  -- end to end: mark the sector the player is standing in as secret (9) and
  -- step the simulation, the way walking into one does
  let here := lvl.sectorAt g.player.x g.player.y
  let secretLvl := { lvl with
    sectors := lvl.sectors.modify here ({ · with special := 9 }) }
  let found := tick {} { g with level := secretLvl }
  r ← check r "stepping into a secret scores it and says so"
    (found.secrets == g.secrets + 1
      && found.message == "A SECRET IS REVEALED!"
      && found.level.sectors[here]!.special == 0)
  -- and it is scored once: vanilla clears the special behind you
  r ← check r "…once only, the special having been cleared"
    ((tick {} found).secrets == found.secrets)

  -- the simulation's message channel reaches the status bar. Vanilla is
  -- silent on secrets; announcing them is a deliberate departure, so the
  -- wiring is worth pinning down.
  let secretG := { g with message := "A SECRET IS REVEALED!" }
  let (shown, drained, _) := playing.step sess {} "" secretG
  r ← check r "a message from the simulation reaches the HUD"
    (shown.message == "A SECRET IS REVEALED!" && shown.messageFrames > 0)
  r ← check r "…and is drained, so it shows once rather than sticking"
    (drained.message == "" && (playing.step sess {} "" drained).1.message == "")
  -- a cheat typed on the same frame wins, being what the player just asked for
  let (cheated, _, _) := playing.step sess {} "dilldqd" secretG
  r ← check r "a cheat typed the same frame takes precedence"
    (cheated.message != "A SECRET IS REVEALED!")

  -- a frame composes without a window, and identically twice over
  let title := composeFrame sess ui0 g
  let title2 := composeFrame sess ui0 g
  let world := composeFrame sess playing g
  r ← check r "a composed frame is the right size and deterministic"
    (title.size == Render.screenW * Render.screenH && title == title2)
  r ← check r "the title screen and the live view compose differently"
    (title != world)
  r ← check r "the title screen is actually painted, not left blank"
    (title != blankFrame)
  return r

-- The July-2026 vanilla-fidelity fix wave, pinned: the square (Chebyshev)
-- collision and splash metrics, monsters bidding doors open, the BFG spray's
-- concentration on big targets, the pickup rules, plat stasis, the timed-door
-- sector special, Doom II par times, Nightmare's zero reaction, noclip
-- skipping walk-over triggers, and `A_Chase`'s skill-gated fire cadence.
set_option maxHeartbeats 1000000 in
def fixWaveTests (r0 : TestRun) (wad : Wad) : IO TestRun := do
  IO.println "fix-wave regressions (square metrics, doors, BFG, pickups, plats):"
  let mut r := r0
  -- DOOM II PARS: g_game.c's cpars[] rows, in seconds. MAP15/17/21 are the
  -- three the par-table fix restored (they had drifted or read off the end).
  r ← check r "MAP15/17/21 par times are 210/420/240 seconds"
    (MapId.parTime (.level 15) == 210 && MapId.parTime (.level 17) == 420
      && MapId.parTime (.level 21) == 240)
  let .ok lvl := Level.load wad "E1M1"
    | check r "setup: E1M1 loads for the fix-wave tests" false
  let g0 := GameState.newGame lvl
  let base := emptied g0
  -- the open nukage courtyard the combat groups use, facing east
  let arena := { base with player := { base.player with
    x := -700, y := -3430, angle := 0
    z := lvl.sectors[lvl.sectorAt (-700) (-3430)]!.floorH } }

  -- CHEBYSHEV BLOCKING (`PIT_CheckThing`): Doom bodies are *squares* — two
  -- 16-radius bodies block while |dx| and |dy| are both under the summed 32.
  -- A diagonal offset of 23,23 is Euclidean 32.5 apart, which a circle test
  -- would wave through; the square metric blocks it. At 33,33 both axes
  -- clear the sum and the spot is free.
  let (soulG, _) := arena.spawn .lostSoul (-500) (-3430) 0
  r ← check r "square bodies: a dx=dy=23 diagonal blocks two 16-radius bodies"
    ((ActorInfo.ofKind .lostSoul).radius == 16
      && soulG.mobjBlocked 0 (-500 + 23) (-3430 + 23) 16 (blockedByPlayer := false))
  r ← check r "…while dx=dy=33 clears both axes and stands free"
    (!soulG.mobjBlocked 0 (-500 + 33) (-3430 + 33) 16 (blockedByPlayer := false))

  -- CHEBYSHEV SPLASH (`PIT_RadiusAttack`): blast falloff is the *square*
  -- distance max(|dx|,|dy|) minus the victim's radius, floored at 0. A
  -- baron (radius 24, 1000 hp) at (+40,+30) from a 128-point burst is
  -- Chebyshev 40 out: 128 − (40 − 24) = 112 lands exactly. The Euclidean
  -- 50 would have left only 102, so the equality pins the metric.
  let (blastG, bi) := arena.spawn .baron (-260) (-3400) 0
  let blasted := blastG.radiusDamage (-300) (-3430) blastG.mobjs[bi]!.z 128
  r ← check r "splash falls off by the square metric: exactly 112 lands"
    ((ActorInfo.ofKind .baron).radius == 24
      && blasted.mobjs[bi]!.health == 1000 - 112)

  -- OWNED WEAPON STAYS (`P_GiveWeapon`/`P_GiveAmmo`): to an owner a floor
  -- shotgun is only worth its shells, and with the pool already full
  -- nothing is gained — vanilla returns false and the item is NOT consumed.
  -- One shell down, the same pickup tops the pool back up and is taken.
  let (fullTaken, fullShells, spentTaken, spentShells) := Id.run do
    let g := { arena with status := { arena.status with
      ownsShotgun := true, shells := 50 } }
    let (g, ii) := g.spawn (.item "SHOT" "A") arena.player.x arena.player.y 0
    let after := g.touchItems
    let after49 := ({ g with status := { g.status with shells := 49 } }).touchItems
    return (after.mobjs[ii]!.removed, after.status.shells,
            after49.mobjs[ii]!.removed, after49.status.shells)
  r ← check r "a full-shells owner leaves the shotgun on the floor"
    (!fullTaken && fullShells == 50)
  r ← check r "…and one spent shell later it is consumed, topping back up"
    (spentTaken && spentShells == 50)

  -- PICKUP WINDOW (`P_TouchSpecialThing`): the item must lie within
  -- (−8, height] of the feet — 57 above the 56-tall marine is out, 40 is
  -- in — and the touch itself is the square blockdist (|dx| and |dy| each
  -- under the radii sum), so a diagonal inside the box but outside the old
  -- inscribed circle still grabs.
  let grabAt := fun (dx dy dz : Float) => Id.run do
    let (g, ii) := arena.spawn (.item "BON1" "A")
      (arena.player.x + dx) (arena.player.y + dy) 0
    let g := g.setMobj ii { g.mobjs[ii]! with z := arena.player.z + dz }
    return (g.touchItems.mobjs[ii]!).removed
  r ← check r "an item 57 above the feet is out of the pickup window"
    (!grabAt 0 0 57)
  r ← check r "…while 40 above is grabbed" (grabAt 0 0 40)
  let rsum := (ActorInfo.ofKind (.item "BON1" "A")).radius + Player.radius
  r ← check r "a diagonal inside the square box but outside the circle grabs"
    (rsum == 36 && Float.sqrt (34.0 ^ 2 + 34.0 ^ 2) > rsum && grabAt 34 34 0)

  -- NIGHTMARE REACTION (`P_SpawnMobj`): vanilla seeds `reactiontime` from
  -- the info table only below Nightmare — a skill-5 spawn carries 0 and may
  -- answer with a ranged attack the instant it wakes.
  let reactAt := fun (skill : Nat) =>
    let g := { base with skill }
    let (g', i) := g.spawn .zombieman (-500) (-3430) 0
    g'.mobjs[i]!.reactionTime
  r ← check r "skill 5 spawns with reactionTime 0, skill 4 with the table's 8"
    (reactAt 5 == 0 && reactAt 4 == 8
      && reactAt 4 == (ActorInfo.ofKind .zombieman).reactionTime)

  return r

-- The two scripted-combat halves of the fix-wave group: the BFG's spray
-- concentration and `A_Chase`'s skill-gated fire cadence. Both fold hundreds
-- of tics inside one definition, which is more than the elaborator will
-- carry alongside the checks above — hence the split, as elsewhere here.
set_option maxHeartbeats 1000000 in
def fixWaveCombatTests (r0 : TestRun) (wad : Wad) : IO TestRun := do
  let mut r := r0
  let .ok lvl := Level.load wad "E1M1"
    | check r "setup: E1M1 loads for the fix-wave combat tests" false
  let base := emptied (GameState.newGame lvl)
  let arena := { base with player := { base.player with
    x := -700, y := -3430, angle := 0
    z := lvl.sectors[lvl.sectorAt (-700) (-3430)]!.floorH } }

  -- BFG CONCENTRATION (`A_BFGSpray`): the ball's own hit is one 1d8×100
  -- roll, but the spray that follows — run by the burst's third BFE1 frame,
  -- 16 tics after landing — is forty independent 15d8 aim-rays fanned over
  -- 90°, and a big target fills many of them at once. A point-blank
  -- cyberdemon must soak far more from the spray alone than any single
  -- ray's 120 max. The two damage events land on different tics, so the
  -- health trace separates them.
  let bfgTotal := Id.run do
    let g := { arena with status := { arena.status with god := true } }
    let (g, ci) := g.spawn .cyberdemon (-600) (-3430) 3.14159265
    let before := g.mobjs[ci]!.health
    let mut g := g.spawnPlayerMissile .bfgBall
    for _ in [0:60] do
      g := tick {} g
    let after := ((g.mobjs.find? (·.kind == .cyberdemon)).map (·.health)).getD 0
    return before - after
  -- A ball hit tops out at 1d8×100 = 800 and one ray at 15d8 = 120, so
  -- anything past 920 can only be several rays landing on the same body.
  r ← check r "ball plus spray beat 920: many rays land on one wide target"
    (bfgTotal > 920)

  -- A_CHASE MISSILE GATE: below Nightmare a monster only *considers* a
  -- missile from a move boundary (movecount at 0) and must reposition after
  -- each one; skill 5 waives both. The same scripted hitscanner with the
  -- same clear line of fire therefore looses strictly fewer volleys at
  -- skill 3 over the same tics — counted as its pistol reports in the
  -- accumulated sound log, which a pure run never drains.
  let volleys := fun (skill : Nat) => Id.run do
    let g := { arena with skill := skill
                          status := { arena.status with health := 4000 } }
    let (g, zi) := g.spawn .zombieman (-450) (-3430) 3.14159265
    let mut g := { g with mobjs := g.mobjs.modify zi fun m =>
      { m with awake := true, state := m.info.seeState.getD m.state } }
    for _ in [0:300] do
      g := tick {} g
      -- pin the player: knockback would otherwise drift them out of the
      -- line of fire and change what is being measured
      g := { g with player := { g.player with
        x := -700, y := -3430, momX := 0, momY := 0 } }
    return (g.sounds.filter (·.1 == Sfx.pistol)).size
  let v3 := volleys 3
  let v5 := volleys 5
  r ← check r "a hitscanner fires strictly fewer volleys at skill 3 than at 5"
    (0 < v3 && v3 < v5)

  return r

-- The second half of the fix-wave group: it holds the plat-stasis,
-- timed-door, monster-door and noclip scenarios, split out because one
-- function holding all twelve outgrew the elaborator's budget — the same
-- pressure that split `main` into groups in the first place.
set_option maxHeartbeats 1000000 in
def fixWaveTests2 (r0 : TestRun) (wad : Wad) : IO TestRun := do
  let mut r := r0
  let .ok lvl := Level.load wad "E1M1"
    | check r "setup: E1M1 loads for the fix-wave tests" false
  let base := emptied (GameState.newGame lvl)

  -- PLAT STASIS (`EV_StopPlat` / `P_ActivateInStasis`): a "stop lift" line
  -- parks a perpetual plat in *stasis* — the mover survives, frozen, its
  -- sector still owned — and the matching perpetual line later resumes it
  -- with its original bounds. The stalled flag rides the v8 save format.
  let liftSec := (lvl.sectorsTagged 2)[0]!
  let mkSpecial := fun (g : GameState) (sp : Nat) =>
    { g with level := { g.level with
        linedefs := g.level.linedefs.modify 191 ({ · with special := sp, tag := 2 }) } }
  let platG := Id.run do
    let mut g := { base with movers := #[.perpetual liftSec (-48) 104 0 false] }
    for _ in [0:10] do g := g.stepMovers
    return g
  let h10 := platG.level.sectors[liftSec]!.floorH
  let stopped := (mkSpecial platG 89).activateLine 191 (byUse := false)
  let stalledOk := match stopped.movers[0]? with
    | some (Mover.perpetual s lo hi _ _ st) =>
      s == liftSec && lo == -48 && hi == 104 && st
    | _ => false
  let frozen := Id.run do
    let mut g := stopped
    for _ in [0:30] do g := g.stepMovers
    return g
  r ← check r "a stopped perpetual plat survives stalled, its height frozen"
    (h10 < 104 && stalledOk && frozen.movers.size == 1
      && frozen.level.sectors[liftSec]!.floorH == h10)
  let resumed := Id.run do
    let mut g := (mkSpecial frozen 87).activateLine 191 (byUse := false)
    for _ in [0:8] do g := g.stepMovers
    return g
  let resumedOk := match resumed.movers[0]? with
    | some (Mover.perpetual s lo hi _ _ st) =>
      s == liftSec && lo == -48 && hi == 104 && !st
    | _ => false
  r ← check r "…the perpetual line resumes it with the SAME bounds"
    (resumedOk && resumed.level.sectors[liftSec]!.floorH < h10)
  r ← check r "…and a save round-trips the stalled flag"
    (match Save.loadGame wad (Save.saveGame frozen) with
     | .ok back =>
       (match back.movers[0]? with
        | some (Mover.perpetual s lo hi _ _ st) =>
          s == liftSec && lo == -48 && hi == 104 && st
        | _ => false)
     | .error _ => false)

  -- TIMED DOOR (sector special 10, `P_SpawnDoorCloseIn30`): level load
  -- parks a close-only door thinker on a 30-second countdown (vanilla's
  -- `topcountdown`, 30·35 = 1050 tics). Nothing moves while it runs; when
  -- it lapses the ceiling shuts one-way and stays shut.
  let startSec := lvl.sectorAt 1056 (-3616)
  let timedLvl := { lvl with
    sectors := lvl.sectors.modify startSec ({ · with special := 10 }) }
  let timedG := Id.run do
    let g := emptied (GameState.newGame timedLvl)
    -- park the player elsewhere: the closing door must meet no head
    return { g with player := { g.player with
      x := 2960, y := -4768
      z := timedLvl.sectors[timedLvl.sectorAt 2960 (-4768)]!.floorH } }
  let spawnedDelayed := match timedG.movers.find? (·.sector == startSec) with
    | some (Mover.door _ _ _ closing stay _ d) => closing && stay && d == 30 * 35
    | _ => false
  let at1049 := Id.run do
    let mut g := timedG
    for _ in [0:1049] do g := g.stepMovers
    return g
  let at1100 := Id.run do
    let mut g := at1049
    for _ in [0:51] do g := g.stepMovers
    return g
  r ← check r "special 10 spawns a door parked on a 1050-tic countdown"
    spawnedDelayed
  r ← check r "…that has not budged through tic 1049"
    (at1049.level.sectors[startSec]!.ceilH == lvl.sectors[startSec]!.ceilH)
  r ← check r "…and by tic 1100 is closed for good, the mover retired"
    (at1100.level.sectors[startSec]!.ceilH == at1100.level.sectors[startSec]!.floorH
      && (at1100.movers.find? (·.sector == startSec)).isNone)

  -- MONSTERS OPEN DOORS (`P_Move`'s blocked-move door bid): a walker whose
  -- refused step touched a plain DR door line (special 1) puts it to use
  -- and waits for it, while an `ML_SECRET`-flagged line is refused outright
  -- in `P_UseSpecialLine`'s non-player gate, so secret doors never open for
  -- monsters. E3M1's sector-13 door (lines 113/125, both special 1) is the
  -- real thing; the secret half runs the same scene with the flag added.
  match Level.load wad "E3M1" with
  | .error e => IO.eprintln s!"  E3M1: {e}"; r ← check r "setup: E3M1 loads" false
  | .ok l3 =>
    let doorSec := l3.sidedefs[(l3.linedefs[125]!.back.getD 0)]!.sector
    let monsterOpens := fun (markSecret : Bool) => Id.run do
      let mark := fun (l : Linedef) => { l with flags := l.flags ||| 0x20 }
      let l3 := if markSecret
        then { l3 with linedefs := (l3.linedefs.modify 113 mark).modify 125 mark }
        else l3
      -- the player waits west of the door; a woken zombieman starts east
      let g := emptied (GameState.newGame l3)
      let g := { g with player := { g.player with
        x := 808, y := 480, angle := 0
        z := l3.sectors[l3.sectorAt 808 480]!.floorH } }
      let (g, mi) := g.spawn .zombieman 912 480 3.14159265
      let mut g := { g with mobjs := g.mobjs.modify mi fun m =>
        { m with awake := true, state := m.info.seeState.getD m.state } }
      let mut opened := false
      for _ in [0 : 6 * 35] do
        g := tick {} g
        if g.movers.any fun mv => match mv with
            | Mover.door s .. => s == doorSec
            | _ => false then
          opened := true
      return opened
    r ← check r "a woken walker bids E3M1's DR door open within seconds"
      (l3.linedefs[113]!.special == 1 && l3.linedefs[125]!.special == 1
        && monsterOpens false)
    r ← check r "…but never an ML_SECRET-flagged special-1 line"
      (!monsterOpens true)

  -- NOCLIP TRIGGERS (`P_TryMove`): vanilla runs the crossed-special walk
  -- only for a body without `MF_NOCLIP`, so the cheat sightsees without
  -- tripping the map — no doors thrown open, no teleporter grabbing you in
  -- passing. The line the player actually strides over here is E1M1's 37,
  -- armed as a plain W1 "open door and stay" on the map's own door tag, so
  -- a firing is unmistakable: one mover where there were none.
  let armed := { lvl with
    linedefs := lvl.linedefs.modify 37 fun ld =>
      { ld with special := 2, tag := 3 } }
  let stride := fun (noclip : Bool) =>
    let g0 := emptied (GameState.newGame armed)
    tick {} { g0 with
      status := { g0.status with noclip }
      player := { g0.player with
        x := 1056, y := -3418, momX := 0, momY := 30
        z := armed.sectors[armed.sectorAt 1056 (-3418)]!.floorH } }
  let walked := stride false
  let ghosted := stride true
  r ← check r "walking a W1 line fires it: the tagged door starts moving"
    (walked.movers.size == 1 && walked.player.y > -3400)
  r ← check r "…while noclip strides across it inert, the map untouched"
    (ghosted.movers.isEmpty && ghosted.player.y > -3400)
  return r

-- Pins for the 2026-08 review-and-fix wave: the save format's meta-checks
-- (version header, `end` sentinel, strict lines), the perpetual plat's
-- both-ends wait, and attacks fizzling when an infight target dies during
-- the attacker's wind-up instead of silently retargeting the player.
set_option maxHeartbeats 1000000 in
def reviewFixTests (r0 : TestRun) (wad : Wad) : IO TestRun := do
  let mut r := r0
  let .ok lvl := Level.load wad "E1M1"
    | check r "setup: E1M1 loads for the review-fix tests" false
  let base := emptied (GameState.newGame lvl)

  -- SAVE META-ROBUSTNESS: the `dillsave <n>` header and the `end` sentinel
  -- are load gates now, and a line the reader does not recognize means
  -- corruption, not something to skip in silence.
  let txt := Save.saveGame base
  let isErr : Except String GameState → Bool := fun
    | .error _ => true | .ok _ => false
  r ← check r "a pristine save still loads" (!isErr (Save.loadGame wad txt))
  r ← check r "a save from the future is refused, not half-loaded"
    (isErr (Save.loadGame wad (txt.replace "dillsave 9" "dillsave 10")))
  r ← check r "a file without the header is not a save at all"
    (isErr (Save.loadGame wad (txt.replace "dillsave 9" "have a nice doom")))
  let truncated := "\n".intercalate ((txt.splitOn "\n").filter (· != "end"))
  r ← check r "a truncated save — no `end` sentinel — is refused"
    (isErr (Save.loadGame wad truncated))
  r ← check r "an unrecognized line is refused, not skipped"
    (isErr (Save.loadGame wad (txt.replace "\nend" "\nmover fnord 3 4\nend")))

  -- PERPETUAL PLAT WAITS AT BOTH ENDS (vanilla `T_PlatRaise`): arriving at
  -- the top parks it for the full wait, and the wait's lapse sends it down
  -- in one continuous run — not an immediate about-face, and not the
  -- endless top-bounce an inverted direction flag briefly produced.
  let liftSec := (lvl.sectorsTagged 2)[0]!
  let runPlat := fun (n : Nat) => Id.run do
    let mut g := { base with
      level := base.level.setFloor liftSec 100,
      movers := #[.perpetual liftSec (-48) 104 0 true] }
    for _ in [0:n] do g := g.stepMovers
    return g
  let arrived := runPlat 4
  let arrivedOk := match arrived.movers[0]? with
    | some (Mover.perpetual _ _ _ w rising _) => w == Speeds.liftWait && !rising
    | _ => false
  r ← check r "reaching the top parks the plat for the full wait"
    (arrived.level.sectors[liftSec]!.floorH == 104.0 && arrivedOk)
  r ← check r "…and it is still parked fifty tics later"
    ((runPlat 54).level.sectors[liftSec]!.floorH == 104.0)
  let departed := runPlat (4 + Speeds.liftWait + 5)
  let departedOk := match departed.movers[0]? with
    | some (Mover.perpetual _ _ _ _ rising _) => !rising
    | _ => false
  r ← check r "…and when the wait lapses it heads down, and keeps going"
    (departed.level.sectors[liftSec]!.floorH == 99.0 && departedOk)

  -- GONE-TARGET FIZZLE: uid 0 means "hunts the player", not "nobody left",
  -- so an attack resolving after its infight target died must fizzle — the
  -- arch-vile's was the worst offender, landing 20 points and the fling on
  -- a player it never acquired.
  let (g1, zi) := base.spawn .zombieman (-500) (-3430) 0
  let zUid := g1.mobjs[zi]!.uid
  let (g2, vi) := g1.spawn .archVile (-350) (-3430) 3.14159265
  let g2 := { g2 with mobjs := g2.mobjs.modify vi fun m =>
    { m with target := zUid, awake := true } }
  let live := g2.aVileAttack vi
  r ← check r "a vile's flame lands on its living infight target"
    (live.mobjs[zi]!.health < g2.mobjs[zi]!.health)
  let corpse := g2.damageMobj zi 1000
  let after := corpse.aVileAttack vi
  r ← check r "…but fizzles once that target has died: the player untouched"
    (after.status.health == corpse.status.health
      && after.player.momZ == corpse.player.momZ)
  return r

-- Pins for the 2026-08-25 review-and-fix wave, world half: the vile fling's
-- z-integration, the charging skull's step limit, the square-box (Chebyshev)
-- reach tests, the 44/72 crushing ceilings, the W1 burn-on-refusal, the
-- corner-box crush occupancy, and the monster-fired gun special.
set_option maxHeartbeats 1000000 in
def reviewFixTests2 (r0 : TestRun) (wad : Wad) : IO TestRun := do
  let mut r := r0
  let .ok lvl := Level.load wad "E1M1"
    | check r "setup: E1M1 loads for the review-fix-2 tests" false
  let base := emptied (GameState.newGame lvl)
  let openSec := lvl.sectorAt (-500) (-3430)
  let openFloor := lvl.sectors[openSec]!.floorH

  -- VILE FLING (`A_VileAttack` + `P_ZMovement`): setting upward momentum on
  -- a grounded body must launch it — the grounded arm used to clamp z back
  -- to the floor and erase the momentum before it ever integrated.
  let (gf, fi) := base.spawn .zombieman (-500) (-3430) 0
  let gf := { gf with mobjs := gf.mobjs.modify fi fun m =>
    { m with momZ := 10 } }
  r ← check r "a vile's fling launches even a grounded monster"
    ((gf.tickMobjs).mobjs[fi]!.z > openFloor)
  let down := Id.run do
    let mut g := gf
    for _ in [0:40] do g := g.tickMobjs
    return g
  r ← check r "…and gravity brings it back to rest on the floor"
    (down.mobjs[fi]!.z == openFloor && down.mobjs[fi]!.momZ == 0.0)

  -- SKULL VS LEDGE (`P_TryMove`'s 24-unit step): a charge into a taller step
  -- ends in a slam on the near side — not a glide up onto the ledge, which
  -- is what an unbounded step allowance used to produce.
  -- the room past line 37 (y = -3392): raise its floor 40 for the step, and
  -- its ceiling out of the way so headroom cannot be what stops the charge
  let s1 := lvl.sectorAt 1056 (-3450)
  let s2 := lvl.sectorAt 1056 (-3380)
  let f1 := lvl.sectors[s1]!.floorH
  let ledgeLvl := (lvl.setFloor s2 (f1 + 40)).setCeil s2 (f1 + 240)
  let gs := emptied (GameState.newGame ledgeLvl)
  let (gs, si) := gs.spawn .lostSoul 1056 (-3450) 1.5707963
  let gs := { gs with mobjs := gs.mobjs.modify si fun m =>
    { m with charging := true, awake := true, momY := 20, z := f1 } }
  let flown := Id.run do
    let mut g := gs
    for _ in [0:8] do g := g.moveSkull si
    return g
  r ← check r "a charging skull slams into a tall ledge instead of gliding atop it"
    (s1 != s2 && !flown.mobjs[si]!.charging && flown.mobjs[si]!.z == f1
      && ledgeLvl.sectorAt flown.mobjs[si]!.x flown.mobjs[si]!.y == s1)

  -- SQUARE-BOX REACH (`PIT_VileCheck`, `P_TeleportMove`): vanilla measures
  -- per-axis boxes, so a diagonal that a circle test rejects is in reach.
  -- 53 units per axis is 74.9 by Euclid — past the vile's 72-unit reach for
  -- the old circle test, comfortably within the box test.
  let (gv, ci) := base.spawn .zombieman (-500) (-3430) 0
  let gv := { gv with mobjs := gv.mobjs.modify ci fun m =>
    { m with corpse := true, health := 0, tics := -1 } }
  let (gv, vi) := gv.spawn .archVile (-447) (-3377) 0
  r ← check r "the vile reaches a corpse square-box distant, as vanilla's box test does"
    ((gv.aVileChase vi).mobjs[ci]!.raising)
  let (gq, qvi) := base.spawn .zombieman (-500) (-3430) 0
  let (gq, cbi) := gq.spawn .spawnCube (-532) (-3462) 0
  r ← check r "the spawn cube telefrags a corner-stander"
    ((gq.moveCube cbi).mobjs[qvi]!.corpse)

  -- 44/72 ARE `lowerAndCrush` (`EV_DoCeiling`): the ceiling heads for 8
  -- above the floor and grinds whoever is beneath it — it must not park on
  -- an occupant's head the way the plain lower-to-floor ceilings (41/43) do.
  let crushLvl := { lvl with
    linedefs := lvl.linedefs.modify 37 ({ · with special := 44, tag := 99 })
    sectors := lvl.sectors.modify openSec ({ · with tag := 99 }) }
  let g0 := emptied (GameState.newGame crushLvl)
  let (g0, zi) := g0.spawn .zombieman (-500) (-3430) 0
  let walker := { g0 with player := { g0.player with
    x := 1056, y := -3380
    z := crushLvl.sectors[crushLvl.sectorAt 1056 (-3380)]!.floorH } }
  let fired := walker.crossSpecials 1056 (-3450)
  r ← check r "crossing a 44 line starts a crushing ceiling and burns the line"
    ((fired.movers.any fun m => match m with
        | Mover.ceiling s _ _ cr => s == openSec && cr
        | _ => false)
      && fired.level.linedefs[37]!.special == 0)
  let travel := (crushLvl.sectors[openSec]!.ceilH - openFloor).toUInt64.toNat + 40
  let ground := Id.run do
    let mut g := fired
    for _ in [0:travel] do g := { g.stepMovers with tics := g.tics + 1 }
    return g
  r ← check r "…and it grinds to eight above the floor, through the zombie"
    (ground.level.sectors[openSec]!.ceilH == openFloor + 8
      && ground.mobjs[zi]!.corpse)

  -- W1 BURNS ON REFUSAL (`P_CrossSpecialLine` clears the special before
  -- asking whether the EV had anywhere to act): a walk-over whose tagged
  -- sector is busy spends the line all the same.
  let armed2 := { lvl with
    linedefs := lvl.linedefs.modify 37 ({ · with special := 2, tag := 3 }) }
  let busy := { (emptied (GameState.newGame armed2)) with
    movers := (armed2.sectorsTagged 3).map fun s =>
      Mover.crusher s (armed2.sectors[s]!.ceilH + 64) (armed2.sectors[s]!.floorH + 8)
        true true }
  let refusedWalk := { busy with player := { busy.player with
    x := 1056, y := -3380
    z := armed2.sectors[armed2.sectorAt 1056 (-3380)]!.floorH } }
  let refused := refusedWalk.crossSpecials 1056 (-3450)
  r ← check r "a refused W1 walk-over is spent all the same"
    (refused.level.linedefs[37]!.special == 0
      && !(refused.movers.any fun m => match m with
        | Mover.door .. => true
        | _ => false))

  -- CORNER-BOX CRUSH (`P_ChangeSector`'s blockbox walk): a body straddling
  -- the crushed sector's edge — centre outside, a clipping-box corner in —
  -- is ground like anyone standing square in it.
  let straddle : Option (Float × Float) := Id.run do
    let mut found : Option (Float × Float) := none
    for li in [0:lvl.linedefs.size] do
      if found.isNone then
        let ld := lvl.linedefs[li]!
        if let some back := ld.back then
          let fs := lvl.sidedefs[ld.front]!.sector
          let bs := lvl.sidedefs[back]!.sector
          let other := if fs == openSec then some bs
            else if bs == openSec then some fs else none
          if let some t := other then
            if lvl.sectors[t]!.floorH == openFloor then
              let p1 := lvl.vertexes[ld.v1]!
              let p2 := lvl.vertexes[ld.v2]!
              let dx := p2.x - p1.x
              let dy := p2.y - p1.y
              let len := Float.sqrt (dx*dx + dy*dy)
              if len ≥ 64 && (Float.abs dx < 0.5 || Float.abs dy < 0.5) then
                let mx := (p1.x + p2.x) / 2
                let my := (p1.y + p2.y) / 2
                let nx := dy / len
                let ny := -dx / len
                let (cx, cy) :=
                  if lvl.sectorAt (mx + nx * 12) (my + ny * 12) == openSec
                  then (mx - nx * 12, my - ny * 12)
                  else (mx + nx * 12, my + ny * 12)
                if lvl.sectorAt cx cy != openSec
                    && (lvl.sectorAt (cx + 20) (cy + 20) == openSec
                      || lvl.sectorAt (cx - 20) (cy + 20) == openSec
                      || lvl.sectorAt (cx + 20) (cy - 20) == openSec
                      || lvl.sectorAt (cx - 20) (cy - 20) == openSec) then
                  found := some (cx, cy)
    return found
  match straddle with
  | none => r ← check r "setup: a straddle spot borders the crushed sector" false
  | some (cx, cy) =>
    let (gc, ei) := base.spawn .zombieman cx cy 0
    let gc := { gc with movers := #[.ceiling openSec (openFloor + 8) 1 true] }
    let grind := Id.run do
      let mut g := gc
      for _ in [0:travel] do g := { g.stepMovers with tics := g.tics + 1 }
      return g
    r ← check r "a sector-edge straddler is caught by the crush"
      (grind.mobjs[ei]!.corpse)

  -- MONSTER-FIRED GUN SPECIALS (`P_ShootSpecialLine`): a non-player shooter
  -- works special 46 and nothing else — 24 and 47 answer to the player's
  -- gun alone. The stray shot rides the same deferred pass the player's
  -- does, queued at the hitscan (`lineAttack`).
  let arm46 := { lvl with
    linedefs := lvl.linedefs.modify 37 ({ · with special := 46, tag := 3 }) }
  let gm46 := emptied (GameState.newGame arm46)
  let shot46 := gm46.shootSpecialLines 1056 (-3450) 1.5707963 2048 (player := false)
  r ← check r "a monster's stray bullet still opens a shot-openable (46) door"
    ((shot46.movers.any fun m => match m with
        | Mover.door .. => true
        | _ => false)
      && shot46.level.linedefs[37]!.special == 46)
  let arm47 := { lvl with
    linedefs := lvl.linedefs.modify 37 ({ · with special := 47, tag := 3 }) }
  let gm47 := emptied (GameState.newGame arm47)
  let shot47 := gm47.shootSpecialLines 1056 (-3450) 1.5707963 2048 (player := false)
  r ← check r "…while 24 and 47 answer to the player's gun alone"
    (shot47.movers.isEmpty && shot47.level.linedefs[37]!.special == 47)
  r ← check r "a monster's hitscan queues the deferred gun-special pass"
    ((gm46.lineAttack 1056 (-3450) 32 1.5707963 512 3 false).monsterShots.size == 1)
  return r

-- Pins for the 2026-08-25 review-and-fix wave, files-and-frames half: the
-- save loader's remaining silent skips, the v9 crush column, subset-packed
-- blockmaps, and the HUD composite's coverage mask.
set_option maxHeartbeats 1000000 in
def reviewFixTests3 (r0 : TestRun) (wad : Wad) : IO TestRun := do
  let mut r := r0
  let .ok lvl := Level.load wad "E1M1"
    | check r "setup: E1M1 loads for the review-fix-3 tests" false
  let base := emptied (GameState.newGame lvl)
  let isErr : Except String GameState → Bool := fun
    | .error _ => true | .ok _ => false

  -- REFUSE, DON'T SKIP: the strict loader's last silent arms — a mangled
  -- skill or stats line, and a mobj roster whose uids collide (or claim 0,
  -- the player-target sentinel) — now throw like every other corruption.
  let (gsv, _) := base.spawn .zombieman (-500) (-3430) 0
  let txt := Save.saveGame gsv
  let doctor := fun (f : String → String) =>
    "\n".intercalate ((txt.splitOn "\n").map f)
  r ← check r "a mangled skill line is refused, not defaulted"
    (isErr (Save.loadGame wad
      (doctor fun l => if l.startsWith "skill" then "skill x" else l)))
  r ← check r "a short stats line is refused, not zeroed"
    (isErr (Save.loadGame wad
      (doctor fun l => if l.startsWith "stats" then "stats 1 2 3 4 5" else l)))
  let mobjLine := ((txt.splitOn "\n").find? (·.startsWith "mobj ")).getD ""
  r ← check r "two mobjs claiming one uid are refused"
    (mobjLine != ""
      && isErr (Save.loadGame wad
          (txt.replace "\nend" s!"\n{mobjLine}\nend")))
  let zeroLine := " ".intercalate ((mobjLine.splitOn " ").set 2 "0")
  r ← check r "a mobj claiming uid 0 — the player's — is refused"
    (isErr (Save.loadGame wad (txt.replace mobjLine zeroLine)))

  -- V9 ROUND-TRIP: a save taken mid-44/72-descent must reload still
  -- crushing, not as a holding ceiling — the `ceil` line's fourth column.
  let sec0 := lvl.sectorAt (-500) (-3430)
  let gC := { base with
    movers := #[.ceiling sec0 (lvl.sectors[sec0]!.floorH + 8) 1 true] }
  r ← check r "a saved crushing ceiling reloads still crushing"
    (match Save.loadGame wad (Save.saveGame gC) with
      | .ok gb => (match gb.movers[0]? with
        | some (Mover.ceiling s _ _ cr) => s == sec0 && cr
        | _ => false)
      | .error _ => false)

  -- SUBSET-PACKED BLOCKMAPS (ZokumBSP): a cell may point into the *middle*
  -- of another cell's list to share its tail. Four cells here share one
  -- eight-entry list; the per-cell lengths sum to 20 words in an 18-word
  -- lump, which the old whole-list cap refused as dishonest.
  let mkLump := fun (ws : Array Nat) => ByteArray.mk <|
    ws.foldl (init := #[]) fun a w =>
      (a.push (UInt8.ofNat (w % 256))).push (UInt8.ofNat (w / 256))
  let packed := mkLump
    #[0, 0, 4, 1, 8, 11, 13, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 65535]
  r ← check r "a subset-packed blockmap loads, its aliased cells the right tails"
    (match Level.parseBlockmap packed with
      | .ok bm => bm.blocks == #[#[1, 2, 3, 4, 5, 6, 7, 8],
          #[3, 4, 5, 6, 7, 8], #[5, 6, 7, 8], #[7, 8]]
      | .error _ => false)
  -- …while a lump aliasing every suffix of one long list — quadratic copies
  -- from linear input, the crafted-DoS shape — still gets refused.
  let hostile := mkLump <|
    #[0, 0, 64, 1] ++ ((Array.range 64).map (68 + ·))
      ++ ((Array.range 64).map (· + 1)) ++ #[65535]
  r ← check r "…and a lump aliasing every suffix of one list is refused"
    (match Level.parseBlockmap hostile with
      | .error e => e == "BLOCKMAP: shared cell lists expand past the copy budget"
      | .ok _ => false)

  -- HUD COMPOSITE BY COVERAGE (`compositeHud`): the mask decides, not the
  -- palette index — a drawn black texel (index 0) survives over the
  -- automap instead of reading as transparent and dropping out.
  let .ok assets := Assets.load wad
    | check r "setup: assets load for the composite test" false
  let px := Render.screenW * Render.screenH
  let gray := ByteArray.mk (Array.replicate (px * 4) 100)
  let hudPal := ByteArray.mk (Array.replicate px 0)
  let mask := (ByteArray.mk (Array.replicate px 0)).set! 5000 1
  let out := Render.compositeHud assets gray hudPal mask
  r ← check r "a covered black HUD texel survives over the automap"
    (out[5000 * 4]! == 0 && out[5000 * 4 + 1]! == 0 && out[5000 * 4 + 2]! == 0
      && out[5000 * 4 + 3]! == 255 && out[4999 * 4]! == 100)
  let blank := ByteArray.mk (Array.replicate px 0)
  let (_, hmask) := Render.withFrameMask blank (Render.drawHud assets
    { health := 100, armor := 50, ammo := some 40, face := some "STFST01" })
  let covered := Id.run do
    let mut n := 0
    for p in [0:px] do
      if hmask[p]! != 0 then n := n + 1
    return n
  r ← check r "…and the status bake records its coverage as it draws"
    (covered > 100)
  return r

-- Pins for the two divergences closed after the 2026-08-25 wave: the
-- crusher's contact-crawl (`T_MoveCeiling` drops to `CEILSPEED/8` while its
-- move reports `crushed`) and the G1 gun specials' unconditional burn
-- (`P_ShootSpecialLine` changes the switch without looking at the EV).
set_option maxHeartbeats 1000000 in
def reviewFixTests4 (r0 : TestRun) (wad : Wad) : IO TestRun := do
  let mut r := r0
  let .ok lvl := Level.load wad "E1M1"
    | check r "setup: E1M1 loads for the review-fix-4 tests" false
  let base := emptied (GameState.newGame lvl)
  let openSec := lvl.sectorAt (-500) (-3430)
  let openFloor := lvl.sectors[openSec]!.floorH
  let ceil0 := lvl.sectors[openSec]!.ceilH

  -- CONTACT-CRAWL: two identical crushing ceilings, one over a body sturdy
  -- enough to stay alive under it. The free one reaches its target; the
  -- grinding one crawls at ⅛ from the moment the gap pinches its victim,
  -- so it is still well above — but below the victim's head, or it merely
  -- held rather than crawled.
  let mover := Mover.ceiling openSec (openFloor + 8) 1 true
  let (gA, zi) := base.spawn .zombieman (-500) (-3430) 0
  let gA := { gA with
    mobjs := gA.mobjs.modify zi ({ · with health := 100000 })
    movers := #[mover] }
  let gB := { base with movers := #[mover] }
  let ticks := fun (g0 : GameState) (n : Nat) => Id.run do
    let mut g := g0
    for _ in [0:n] do g := { g.stepMovers with tics := g.tics + 1 }
    return g
  let span := (ceil0 - openFloor).toUInt64.toNat + 60
  let (runA, runB) := (ticks gA span, ticks gB span)
  let zHeight := runA.mobjs[zi]!.info.height
  r ← check r "an unobstructed crushing ceiling reaches eight above the floor"
    (runB.level.sectors[openSec]!.ceilH == openFloor + 8)
  r ← check r "…while one grinding a live body crawls at an eighth, far behind"
    (runA.level.sectors[openSec]!.ceilH < openFloor + zHeight
      && runA.level.sectors[openSec]!.ceilH > openFloor + 30
      && runA.mobjs[zi]!.health < 100000)

  -- G1 BURNS ON REFUSAL: a shot at a 24 whose tagged sectors are all busy
  -- starts nothing — and spends the line anyway.
  let armG := { lvl with
    linedefs := lvl.linedefs.modify 37 ({ · with special := 24, tag := 3 }) }
  let busyG := { (emptied (GameState.newGame armG)) with
    movers := (armG.sectorsTagged 3).map fun s =>
      Mover.crusher s (armG.sectors[s]!.ceilH + 64)
        (armG.sectors[s]!.floorH + 8) true true }
  let shotG := busyG.shootSpecialLines 1056 (-3450) 1.5707963 2048 (player := true)
  r ← check r "a refused G1 shot spends the line all the same"
    (shotG.level.linedefs[37]!.special == 0
      && !(shotG.movers.any fun m => match m with
        | Mover.floorUp .. => true
        | _ => false))
  return r

-- The whole suite is one `do` block, and by now it is long enough that the
-- code generator's default heartbeat budget runs out partway through — the
-- same pressure that put `geometryIndexTests` and the groups after it in
-- functions of their own. Raising the budget keeps that a formatting choice
-- rather than a hard cap on how many tests `main` may hold.
set_option maxHeartbeats 1000000 in
def main : IO UInt32 := do
  let bytes ← IO.FS.readBinFile "doom.wad"
  let wad ← match Wad.parse bytes with
    | .ok w => pure w
    | .error e => IO.eprintln s!"doom.wad failed to parse: {e}"; return 1


  IO.println "Wad.parse doom.wad:"
  let mut r : TestRun := {}
  r ← check r "is an IWAD" (wad.kind == .iwad)
  r ← check r "has a Doom-sized directory (> 1000 lumps)" (wad.lumps.size > 1000)
  r ← check r "PLAYPAL present, 14 palettes × 768 bytes"
    (wad.find? "PLAYPAL" |>.any (·.size == 14 * 768))
  r ← check r "COLORMAP present, 34 maps × 256 bytes"
    (wad.find? "COLORMAP" |>.any (·.size == 34 * 256))
  r ← check r "TEXTURE1 present" (wad.find? "TEXTURE1").isSome
  r ← check r "PNAMES present" (wad.find? "PNAMES").isSome
  r ← check r "E1M1 marker present, empty"
    (wad.find? "E1M1" |>.any (·.size == 0))

  -- Map lumps must follow their marker in Doom's fixed order.
  let mapLumps := #["THINGS", "LINEDEFS", "SIDEDEFS", "VERTEXES", "SEGS",
                    "SSECTORS", "NODES", "SECTORS", "REJECT", "BLOCKMAP"]
  if let some e1m1 := wad.indexOf? "E1M1" then
    for offset in [0:mapLumps.size] do
      let expected := mapLumps[offset]!
      r ← check r s!"E1M1 + {offset + 1} is {expected}"
        (wad.lumps[e1m1 + 1 + offset]!.name == expected)

  -- Merging a PWAD appends its directory and rebases its lump offsets.
  -- Merging the WAD with itself is the sharpest check of that arithmetic:
  -- the shadowing copy must read back byte-for-byte from the shifted half.
  let doubled := wad.merge wad
  r ← check r "merge appends the directory and the bytes"
    (doubled.lumps.size == 2 * wad.lumps.size
      && doubled.bytes.size == 2 * wad.bytes.size)
  r ← check r "merge resolves a shadowed lump to the later copy"
    (match wad.lastIndexOf? "PLAYPAL" with
     | some i => doubled.lastIndexOf? "PLAYPAL" == some (wad.lumps.size + i)
     | none => false)
  r ← check r "a shadowed lump still reads back its own bytes"
    (match wad.find? "PLAYPAL", doubled.find? "PLAYPAL" with
     | some a, some b => (wad.data a).toList == (doubled.data b).toList
     | _, _ => false)

  -- Sprite and flat sections are delimited by marker lumps.
  r ← check r "sprite markers S_START/S_END in order"
    (match wad.indexOf? "S_START", wad.indexOf? "S_END" with
     | some s, some e => s < e
     | _, _ => false)
  r ← check r "flat markers F_START/F_END in order"
    (match wad.indexOf? "F_START", wad.indexOf? "F_END" with
     | some s, some e => s < e
     | _, _ => false)

  -- The O(1) name index must agree with what a linear scan would say, for
  -- every name in the directory (each entry resolves to a later-or-equal
  -- index bearing the same name — i.e. the last occurrence wins).
  r ← check r "lastIndexOf? resolves every name to its last occurrence"
    (Id.run do
      for k in [0:wad.lumps.size] do
        let name := wad.lumps[k]!.name
        match wad.lastIndexOf? name with
        | some j =>
          if j < k || wad.lumps[j]!.name != name then return false
        | none => return false
      return true)

  -- Malformed data must come back as `Except.error`, never a panic.
  r ← check r "garbage bytes are a parse error, not a panic"
    (match Wad.parse ("NOTAWAD: not a wad at all".toUTF8) with
     | .error _ => true | .ok _ => false)
  r ← check r "a picture claiming 65535×65535 is refused"
    (match Picture.parse (ByteArray.mk #[0xFF, 0xFF, 0xFF, 0xFF, 0, 0, 0, 0]) with
     | .error _ => true | .ok _ => false)
  r ← check r "a truncated MUS lump is an error, not a panic"
    (match Music.musToMidi ("MUS".toUTF8) with
     | .error _ => true | .ok _ => false)

  IO.println "Level.load E1M1:"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1 failed to load: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    -- Golden counts for the E1M1 of the `doom.wad` in the repo root, which
    -- is **Ultimate Doom**. The earlier Doom v1.9 cut of E1M1 is a different
    -- map (467/475/648/85/236/237/732) — if these start failing wholesale,
    -- check which IWAD is present before touching the parser.
    r ← check r "470 vertexes"   (lvl.vertexes.size == 470)
    r ← check r "486 linedefs"   (lvl.linedefs.size == 486)
    r ← check r "666 sidedefs"   (lvl.sidedefs.size == 666)
    r ← check r "88 sectors"     (lvl.sectors.size == 88)
    r ← check r "238 BSP nodes"  (lvl.nodes.size == 238)
    r ← check r "239 subsectors" (lvl.subsectors.size == 239)
    r ← check r "747 segs"       (lvl.segs.size == 747)
    let start := lvl.things.find? (·.type == 1)
    r ← check r "player start at (1056, -3616)"
      (start.any fun t => t.x == 1056 && t.y == -3616 && t.angle == 90)
    let sec := lvl.sectors[lvl.sectorAt 1056 (-3616)]!
    r ← check r "start sector: floor 0, ceiling 72, light 144, FLOOR4_8"
      (sec.floorH == 0 && sec.ceilH == 72 && sec.light == 144
        && sec.floorFlat == "FLOOR4_8")

  IO.println "REJECT (vanilla's trivial sight rejection):"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1 failed to load: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    r ← check r "E1M1's REJECT matrix is loaded" (!lvl.reject.isEmpty)
    -- The matrix may only say "certainly cannot see"; a pair it rejects
    -- must never actually be traceable, or monsters would go blind.
    let pts := lvl.things.map (fun t => (t.x, t.y))
    let n := min 26 pts.size
    let wrong := Id.run do
      let mut bad := 0
      for a in [0:n] do
        for b in [0:n] do
          let (x1, y1) := pts[a]!
          let (x2, y2) := pts[b]!
          let s1 := lvl.sectorAt x1 y1
          let s2 := lvl.sectorAt x2 y2
          if lvl.rejects s1 s2 then
            let z1 := lvl.sectors[s1]!.floorH + 41
            let z2 := lvl.sectors[s2]!.floorH + 41
            if lvl.checkSight x1 y1 z1 x2 y2 z2 then bad := bad + 1
      return bad
    r ← check r "a rejected sector pair is never actually in sight" (wrong == 0)

  r ← mobjGridTests r wad
  r ← blazingTests r wad

  IO.println "vanilla constants and rules:"
  r ← vanillaRuleTests r wad

  IO.println "the melt screen wipe:"
  r ← wipeTests r

  IO.println "second-pass fixes (FF sections, name case, MAP00 music):"
  r ← secondPassTests r wad

  IO.println "geometry indexes (sec->lines[], scroll list, ray walk):"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1 failed to load: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl => r ← geometryIndexTests r lvl

  IO.println "Assets.load:"
  match Assets.load wad with
  | .error e => IO.eprintln s!"  assets failed to load: {e}"; r ← check r "setup: assets load" false
  | .ok assets =>
    r ← check r "palette is 768 bytes" (assets.palette.size == 768)
    r ← check r "STARTAN3 is a 128×128 texture"
      (assets.textures.get? "STARTAN3" |>.any fun t =>
        t.width == 128 && t.height == 128)
    r ← check r "BIGDOOR2 present" (assets.textures.contains "BIGDOOR2")
    r ← check r "FLOOR4_8 flat present" (assets.flats.contains "FLOOR4_8")
    r ← check r "imp sprite TROOA1 is 41×57 with offsets"
      (assets.sprites.get? "TROOA1" |>.any fun s =>
        s.width == 41 && s.height == 57
          && s.leftOffset == 19 && s.topOffset == 52)
    -- Every weapon frame must be a real sprite lump: a missing one silently
    -- draws nothing, so the gun vanishes mid-shot (the shotgun's phantom
    -- E/F/G, the rocket's C/D). Super shotgun is Doom II only, so skip it.
    let guns : List Weapon :=
      [.fist, .chainsaw, .pistol, .shotgun, .chaingun, .rocket, .plasma, .bfg]
    let badFrames := guns.filter fun w => !(w.attack.all fun s =>
      assets.sprites.contains (w.sprite.push s.frame |>.push '0')
        && (match s.flash with
            | some c => assets.sprites.contains (w.flashSprite.push c |>.push '0')
            | none => true))
    unless badFrames.isEmpty do
      IO.println s!"  (weapons with phantom frames: {badFrames.map (·.sprite)})"
    r ← check r "every weapon animation frame is a real sprite"
      badFrames.isEmpty

  IO.println "tick (player physics on E1M1):"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1 failed to load: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
      let mut g := g
      for _ in [0:n] do
        g := tick input g
      return g
    let g0 := GameState.start lvl
    r ← check r "starts at the player 1 start on the floor"
      (g0.player.x == 1056 && g0.player.y == -3616 && g0.player.z == 0)
    -- Walking forward (north) crosses the passage into the main room.
    let fwd := run { forward := true } 100 g0
    r ← check r "walking north 100 tics travels there"
      (fwd.player.y > -3000 && fwd.player.x == 1056)
    r ← check r "walk speed near vanilla (≤ 8 units/tic)"
      ((fwd.player.y - g0.player.y) / 100 ≤ 8)
    -- The south wall is 64 units behind the start; backing up must stop.
    let back := run { back := true } 100 g0
    r ← check r "south wall stops backward movement at its radius"
      (back.player.y ≥ -3680 + 15 && back.player.y ≤ -3640)
    -- Strafing west from the start runs into the room's diagonal SW wall.
    -- `P_SlideMove` carries you *along* it rather than stopping you dead, so
    -- the player travels well past where the wall first blocks them — but
    -- always stays on the legal side of it.
    let west := run { strafeLeft := true } 200 g0
    r ← check r "a diagonal wall is slid along, not stuck on"
      ((Player.checkPosition lvl west.player.x west.player.y west.player.z).isSome
        && west.player.x < 917          -- got past where stairstepping stopped
        && west.player.y > -3616 + 50)  -- by sliding north-west along the wall
    -- The steps at (160..224, -3264..-3200) rise westward 40 → 88.
    let stairsStart := { g0 with player := { g0.player with
      x := 288, y := -3232, z := 40, angle := 3.14159265358979 } }
    let stairs := run { forward := true } 40 stairsStart
    r ← check r "stairs climb as floors rise"
      (stairs.player.z ≥ 88 && stairs.player.x < 160)

  IO.println "specials (doors, lifts, exit on E1M1):"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1 failed to load: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
      let mut g := g
      for _ in [0:n] do
        g := tick input g
      return g
    let at' := fun (x y angle : Float) =>
      let g := GameState.start lvl
      { g with player := { g.player with
          x := x, y := y, angle := angle
          z := lvl.sectors[lvl.sectorAt x y]!.floorH } }
    -- Door: lines 148/149 (x = 1536/1552) close off the room east of the
    -- zigzag; its sector is behind line 148.
    let doorSec := lvl.sidedefs[(lvl.linedefs[148]!.back.getD 0)]!.sector
    let closedCeil := lvl.sectors[doorSec]!.ceilH
    let g := at' 1500 (-2496) 0
    let g := run { «use» := true } 1 g
    let opening := run {} 30 g
    r ← check r "using a door raises its ceiling"
      (opening.level.sectors[doorSec]!.ceilH > closedCeil)
    -- Fully open after ~40 tics, and the 150-tic wait leaves time to walk in.
    let opened := run {} 40 g
    let walked := run { forward := true } 60 opened
    r ← check r "player walks through the opened door"
      (walked.player.x > 1560)
    let cycled := run {} (400 + 150) opened
    r ← check r "the door closes again after its wait"
      (cycled.level.sectors[doorSec]!.ceilH == closedCeil)
    -- Doors don't crush: stand in the doorway through a full close cycle
    -- and the ceiling must bounce off your head, never pinning you.
    let inDoorway := Id.run do
      -- Clear the bestiary first: E1M1's zombiemen shoot the moment the
      -- player is in view, and the knockback walks them out of the doorway
      -- before the door ever reaches their head — which is real behaviour,
      -- but not what this test is asking about.
      let mut g := run { «use» := true } 1
        (emptied (at' 1500 (-2496) 0))
      g := run {} 60 g                       -- let the door open fully
      -- park squarely inside the 16-unit-deep doorway
      g := { g with player := { g.player with x := 1544 } }
      let mut minCeil := 1000.0
      for _ in [0:600] do
        g := tick {} g
        minCeil := min minCeil g.level.sectors[doorSec]!.ceilH
      return (g, minCeil)
    let (afterCycle, minCeil) := inDoorway
    r ← check r "a closing door bounces off the player, never crushing"
      (minCeil ≥ afterCycle.player.z + 56 - 2.5
        && afterCycle.level.sectors[doorSec]!.ceilH > closedCeil + 40)
    -- Using a door mid-motion reverses it, exactly once per press.
    let opening := run {} 10 (run { «use» := true } 1 (at' 1500 (-2496) 0))
    let risingCeil := opening.level.sectors[doorSec]!.ceilH
    r ← check r "the door is rising after use" (risingCeil > closedCeil)
    let toggled := run {} 10 (run { «use» := true } 1 opening)
    r ← check r "using a rising door reverses it into closing"
      (toggled.level.sectors[doorSec]!.ceilH < risingCeil)
    let reversed := run {} 12 (run { «use» := true } 1 toggled)
    r ← check r "using it again sends it back up"
      (reversed.level.sectors[doorSec]!.ceilH
        > toggled.level.sectors[doorSec]!.ceilH)

    -- Lift: walking over line 191 triggers the tag-2 platform.
    let liftSec := (lvl.sectorsTagged 2)[0]!
    let liftTop := lvl.sectors[liftSec]!.floorH
    let gLift := run { forward := true } 40 (at' 2830 (-3060) 1.2)
    r ← check r "crossing the walk line lowers the lift"
      (gLift.level.sectors[liftSec]!.floorH < liftTop)
    -- It comes back up on its own. Sampling a fixed tic is fragile: special
    -- 88 is one a monster may cross too, so one of the map's own can send it
    -- down again afterwards — so look for the return anywhere in the window.
    let returned := Id.run do
      let mut g := gLift
      let mut seen := false
      for _ in [0:400] do
        g := tick {} g
        if g.level.sectors[liftSec]!.floorH == liftTop then seen := true
      return seen
    r ← check r "the lift returns to its height" returned
    -- Riding a lift down, you must still be able to walk. A floor drops 4
    -- units a tic and a fall only starts at 1, so a player left to gravity
    -- hangs in the air the whole way and `onground` — hence the thrust —
    -- reads false. Vanilla carries them on the floor (`P_ThingHeightClip`).
    let liftSector := (lvl.sectorsTagged 2)[0]!
    -- stand at the middle of the lift, worked out from its own linedefs
    let (liftX, liftY) := Id.run do
      let mut sx := 0.0
      let mut sy := 0.0
      let mut n := 0.0
      for l in lvl.linedefs do
        let f := lvl.sidedefs[l.front]!.sector
        let b := (l.back.map (lvl.sidedefs[·]!.sector)).getD f
        if f == liftSector || b == liftSector then
          let p1 := lvl.vertexes[l.v1]!
          let p2 := lvl.vertexes[l.v2]!
          sx := sx + p1.x + p2.x; sy := sy + p1.y + p2.y; n := n + 2.0
      return (sx / n, sy / n)
    -- Ride the same descending lift twice, walking once and standing still
    -- once. Same shaft, same descent, so any difference in distance is the
    -- thrust — which is exactly what the airborne bug used to suppress.
    let liftRide := fun (walk : Bool) => Id.run do
      let top := lvl.sectors[liftSector]!.floorH
      let g := at' liftX liftY 0
      let p := { g.player with x := liftX, y := liftY, z := top, angle := 0 }
      let mv : Mover :=
        .lift liftSector (lvl.lowestNeighborFloor liftSector) top
          Speeds.liftWait false
      let g := { g with player := p, movers := #[mv] }
      let mut gg := g
      let mut glued := true
      for _ in [0:12] do
        gg := tick { forward := walk } gg
        if gg.level.sectorAt gg.player.x gg.player.y == liftSector
            && Float.abs (gg.player.z - gg.level.sectors[liftSector]!.floorH) > 0.001 then
          glued := false
      let dist := Float.sqrt ((gg.player.x - liftX) ^ 2 + (gg.player.y - liftY) ^ 2)
      return (dist, glued, decide (gg.level.sectors[liftSector]!.floorH < top))
    let (walked, glued, descended) := liftRide true
    let (stoodStill, _, _) := liftRide false
    r ← check r "the lift actually goes down under the player" descended
    r ← check r "a descending lift carries the player with it" glued
    -- `onground` stays true, so `P_MovePlayer`'s thrust still runs; left
    -- airborne for the whole descent the player barely creeps.
    r ← check r "and the player can still walk while riding it"
      (stoodStill < 1.0 && walked > 15.0)

    -- Exit switch: line 326 at x = 2912 faces the exit room, which lies
    -- *east* of it (sector 70 spans x 2912–3104). Press from inside the
    -- room looking west — the old spot at x = 2860 was in the void behind
    -- the wall, and only ever worked while `useLines` ignored which side a
    -- press came from.
    let gExit := run { «use» := true } 1 (at' 2960 (-4768) 3.14159265)
    r ← check r "the exit switch ends the level" gExit.exited
    r ← check r "walking around doesn't exit by itself"
      (!(run { forward := true } 100 (at' 1056 (-3616) 1.5707963)).exited)

    -- Dying reloads the map from scratch, as vanilla's `G_DoReborn` does in
    -- single player (`gameaction = ga_loadlevel`). The restart has to build
    -- from the *pristine* level rather than the one just played on, or the
    -- door left open stays open and the dead stay dead.
    let played := Id.run do
      let mut g := run {} 60 (run { «use» := true } 1 (at' 1500 (-2496) 0))
      g := { g with kills := 7, secrets := 1
                    status := { g.status with health := 9, ownsShotgun := true } }
      g := { g with mobjs := g.mobjs.map fun m =>
               if m.info.countKill then { m with awake := true } else m }
      return g
    let doorSec := lvl.sidedefs[(lvl.linedefs[148]!.back.getD 0)]!.sector
    let reborn := GameState.newGame lvl none played.skill
    r ← check r "the door opened before dying is shut again after respawn"
      (played.level.sectors[doorSec]!.ceilH > lvl.sectors[doorSec]!.ceilH
        && reborn.level.sectors[doorSec]!.ceilH == lvl.sectors[doorSec]!.ceilH)
    r ← check r "respawning puts every monster back, asleep"
      (played.mobjs.any (fun m => m.info.countKill && m.awake)
        && !reborn.mobjs.any (fun m => m.info.countKill && m.awake)
        && reborn.killTotal > 0
        && (reborn.mobjs.filter (·.info.countKill)).size == reborn.killTotal)
    r ← check r "respawning clears the tally, the alerts and the automap"
      (reborn.kills == 0 && reborn.secrets == 0 && reborn.items == 0
        && reborn.alerted.isEmpty && !reborn.seen.any id
        && reborn.movers.isEmpty)
    r ← check r "respawning is a pistol start"
      (reborn.status.health == 100 && !reborn.status.ownsShotgun
        && reborn.status.bullets == 50 && reborn.status.weapon == .pistol)

  IO.println "gameplay (scripted combat on E1M1):"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1 failed to load: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
      let mut g := g
      for _ in [0:n] do
        g := tick input g
      return g
    let teleport := fun (g : GameState) (x y angle : Float) =>
      { g with player := { g.player with
          x := x, y := y, angle := angle
          z := lvl.sectors[lvl.sectorAt x y]!.floorH } }
    let g0 := GameState.start lvl
    r ← check r "the world spawns (monsters, items, decorations)"
      (g0.mobjs.size > 100 &&
        g0.mobjs.any (·.kind == .zombieman) && g0.mobjs.any (·.kind == .imp))

    -- The zombieman at (3056,-3584) faces north: stand north of him,
    -- inside his half-circle of vision, and he wakes.
    let nearZomb := teleport g0 3056 (-3330) 4.712
    let woke := run {} 40 nearZomb
    r ← check r "a seen player wakes the monster"
      (woke.mobjs.any fun m => m.kind == .zombieman && m.awake)
    r ← check r "monsters left alone stay dormant"
      ((run {} 40 g0).mobjs.all fun m => !m.awake || m.kind != .imp)

    -- Shoot him: hold fire with the pistol until he drops.
    let aimed := teleport g0 2800 (-3584)
      (Float.atan2 (-3584 + 3584) (3056 - 2800))
    let fought := run { fire := true } 120 aimed
    r ← check r "pistol fire kills the zombieman"
      (fought.mobjs.any fun m =>
        m.kind == .zombieman && m.corpse && m.distanceTo 3056 (-3584) < 300)
    r ← check r "the fight consumed bullets"
      (fought.status.bullets < 50)
    r ← check r "the kill dropped an ammo clip"
      (fought.mobjs.any fun m =>
        m.kind == .item "CLIP" "A" && m.dropped)

    -- Stand among the imps: pain follows.
    let mobbed := run {} 300 (teleport g0 3400 (-3480) 0)
    r ← check r "monsters hurt a player standing among them"
      (mobbed.status.health < 100)

    -- An armor bonus is at (432,-3040).
    let bonus := run {} 5 (teleport g0 432 (-3040) 0)
    r ← check r "walking over an armor bonus picks it up"
      (bonus.status.armor == 1 &&
        !bonus.mobjs.any fun m => m.x == 432 && m.y == -3040 && m.info.pickup)

    -- Intermission stats: E1M1 has 3 secret sectors; kills and items tally.
    r ← check r "E1M1 counts 3 secrets and a full bestiary"
      (g0.secretTotal == 3 && g0.killTotal > 20 && g0.itemTotal > 30)
    r ← check r "kills and items are tallied as they happen"
      (fought.kills > 0 && (run {} 5 (teleport g0 432 (-3040) 0)).items == 1)

    -- Shooting the barrel at (1312,-3264) blows it up.
    let atBarrel := teleport g0 1100 (-3264) 0
    let boom := run { fire := true } 80 atBarrel
    r ← check r "shooting a barrel destroys it"
      (boom.mobjs.all fun m => !(m.kind == .barrel && m.x == 1312 && !m.corpse))

    -- Imps at range throw fireballs: stand across the room from the pair
    -- at (3440,-3472) and fireballs get spawned at some point.
    let baited := teleport g0 3100 (-3600) 0.5
    let ballSeen := Id.run do
      let mut g := baited
      for _ in [0:400] do
        g := tick {} g
        if g.mobjs.any (·.kind == .impBall) then return true
      return false
    r ← check r "imps at range throw fireballs" ballSeen
    r ← check r "fireballs eventually hurt the player"
      ((run {} 500 baited).status.health < 100)

    -- The shotgun at (3264,-3936) arms and switches on pickup.
    let armed := run {} 40 (teleport g0 3264 (-3936) 0)
    r ← check r "picking up the shotgun switches to it"
      (armed.status.ownsShotgun && armed.status.weapon == .shotgun
        && armed.status.shells == 8)

  IO.println "follow-ups (lights, nukage, sound events, progression):"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1 failed to load: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
      let mut g := g
      for _ in [0:n] do
        g := tick input g
      return g
    let teleport := fun (g : GameState) (x y angle : Float) =>
      { g with player := { g.player with
          x := x, y := y, angle := angle
          z := lvl.sectors[lvl.sectorAt x y]!.floorH } }
    let g0 := GameState.newGame lvl
    r ← check r "light thinkers spawn for E1M1's special sectors"
      (g0.lights.size ≥ 4)
    -- Some animated sector's light must change over a couple of seconds.
    let later := run {} 80 (teleport g0 1056 (-3616) 1.5707963)
    let changed := Id.run do
      for i in [0:lvl.sectors.size] do
        if later.level.sectors[i]!.light != lvl.sectors[i]!.light then
          return true
      return false
    r ← check r "sector lights animate" changed
    -- The nukage pool west of the courtyard (sector 13-ish, special 7):
    -- standing in any special-7 sector drains health 5 per 32 tics.
    let nukSec := Id.run do
      for i in [0:lvl.sectors.size] do
        if lvl.sectors[i]!.special == 7 then return i
      return 0
    -- find a point inside it: use a seg vertex nudged inward
    let nukPoint := Id.run do
      for s in [0:lvl.subsectors.size] do
        let sub := lvl.subsectors[s]!
        if lvl.segFrontSector lvl.segs[sub.first]! == nukSec then
          -- centroid of the subsector's seg starts
          let mut cx := 0.0
          let mut cy := 0.0
          for k in [sub.first : sub.first + sub.count] do
            cx := cx + lvl.vertexes[lvl.segs[k]!.v1]!.x
            cy := cy + lvl.vertexes[lvl.segs[k]!.v1]!.y
          return (cx / Float.ofNat sub.count, cy / Float.ofNat sub.count)
      return (0.0, 0.0)
    let (nx, ny) := nukPoint
    let soaked := run {} 100 (teleport g0 nx ny 0)
    r ← check r "standing in nukage drains health"
      (soaked.status.health < 100)
    -- Firing emits a positioned pistol sound event within the tick.
    let shot := tick { fire := true } (run { fire := true } 4 g0)
    r ← check r "firing emits a sound event"
      (shot.sounds.any fun (s, _, _) => s == Sfx.pistol)
    -- The exit switch sets up progression to E1M2. The exit room is east
    -- of the switch line, so stand inside it and face west (see the exit
    -- switch test above).
    let atExit := teleport g0 2960 (-4768) 3.14159265
    let done := run { «use» := true } 1 atExit
    r ← check r "exit switch fires and E1M2 is next"
      (done.exited && !done.secretExit
        && nextMapName done.level.name done.secretExit == some "E1M2")
    r ← check r "E1M8 ends the episode; E1M9 returns to E1M4"
      (nextMapName "E1M8" false == none
        && nextMapName "E1M3" true == some "E1M9"
        && nextMapName "E1M9" false == some "E1M4")
    -- Status carries across maps, but flashes and death do not.
    let carried := GameState.newGame lvl (some { done.status with
      health := 61, damageCount := 44, dead := true })
    r ← check r "vitals carry to the next map, flashes and death don't"
      (carried.status.health == 61 && carried.status.damageCount == 0
        && !carried.status.dead)
    -- E1M2 and E1M3 load and spawn (progression targets are real).
    r ← check r "E1M2 and E1M3 load with live worlds"
      (match Level.load wad "E1M2", Level.load wad "E1M3" with
       | .ok l2, .ok l3 =>
         (GameState.newGame l2).mobjs.size > 50
           && (GameState.newGame l3).mobjs.size > 50
       | _, _ => false)

  IO.println "healing, stairs, teleports, new monsters:"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1 failed to load: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
      let mut g := g
      for _ in [0:n] do
        g := tick input g
      return g
    let teleportTo := fun (g : GameState) (x y angle : Float) =>
      { g with player := { g.player with
          x := x, y := y, angle := angle
          z := lvl.sectors[lvl.sectorAt x y]!.floorH } }
    let g0 := GameState.newGame lvl
    -- Stimpaks and medikits heal a hurt player (and won't overfill).
    let hurt := { (g0.damagePlayer 60) with mobjs := g0.mobjs }
    let atStim := run {} 5 (teleportTo hurt 3072 (-4768) 0)
    r ← check r "a stimpak heals +10"
      (atStim.status.health == hurt.status.health + 10)
    let atMedi := run {} 5 (teleportTo hurt 3232 (-3808) 0)
    r ← check r "a medikit heals +25"
      (atMedi.status.health == hurt.status.health + 25)
    r ← check r "full health leaves the stimpak for later"
      ((run {} 5 (teleportTo g0 3072 (-4768) 0)).status.health == 100)

    -- Vertical autoaim: from the armor stairs (step at 56, so a vanilla
    -- `shootz` of 92) the sergeant on the catwalk (feet at 104) clears the
    -- step lips — bullets must climb to him, not die on the risers. A step
    -- lower, at x = 250, the lips do cut the aim off, which is why this
    -- stands where it does.
    let onStairs := teleportTo g0 200 (-3232) 3.14159265
    let upFight := run { fire := true } 250 onStairs
    r ← check r "bullets autoaim up the stairs and kill the sergeant"
      (upFight.mobjs.any fun m =>
        m.kind == .shotgunGuy && m.corpse && m.distanceTo (-160) (-3232) < 200)

    -- Vanilla's autoaim sweep (`P_BulletSlope`): straight ahead first, then
    -- a sixty-fourth of a turn each way. Only the *vertical* slope comes
    -- from the sweep — the bullet still flies along the player's facing —
    -- so what it buys is the right height on a target you are not quite
    -- lined up with.
    -- open, level ground with a clear line east
    let sx := -700.0
    let sy := -3430.0
    let sweepFinds := fun (offDeg : Float) => Id.run do
      let g := emptied g0
      let g := { g with player := { g.player with
                   x := sx, y := sy, z := lvl.sectors[lvl.sectorAt sx sy]!.floorH } }
      -- an imp dead ahead to the east, then the facing turned off it
      let (g, ii) := g.spawn .imp (sx + 220) sy 0
      let _ := ii
      let face := -offDeg * 3.14159265 / 180.0
      let sz := g.player.z + Player.height / 2 + 8
      let straight := (GameState.aimLineAttack g sx sy sz face 2048 true).isSome
      let swept := (GameState.aimLineAttack g sx sy sz
                     (face + 6.28318530718 / 64.0) 2048 true).isSome
      return (straight, swept)
    r ← check r "the autoaim sweep reaches past what a straight ray finds"
      (Id.run do
        let (s0, _) := sweepFinds 0.0
        let (s7, w7) := sweepFinds 7.0
        -- dead ahead the straight ray suffices; 7° off it does not, and the
        -- swept ray picks the target up instead
        return s0 && !s7 && w7)
    -- vanilla's `shootz` is z + (height>>1) + 8, below the camera at z + 41
    r ← check r "shots leave from vanilla's shootz, below the eye"
      (Player.height / 2 + 8 == 36 && Player.viewHeight == 41)

    -- Spawned monsters from the deeper bestiary behave.
    -- spawn facing west, toward where these tests park the player
    let spawnAt := fun (g : GameState) (kind : ActorKind) (x y : Float) =>
      let (g', i) := g.spawn kind x y 3.14159265
      { g' with mobjs := g'.mobjs.modify i (fun m => { m with
          z := g'.level.sectors[g'.level.sectorAt x y]!.floorH + 30 }) }
    let cacoG := spawnAt (teleportTo g0 (-500) (-3430) 0) .cacodemon (-80) (-3430)
    let cacoRun := run {} 400 cacoG
    r ← check r "a cacodemon wakes and fights"
      ((cacoRun.mobjs.any fun m => m.kind == .cacodemon && m.awake)
        && (cacoRun.mobjs.any (·.kind == .cacoBall)
            || cacoRun.status.health < 100))
    let soulG := spawnAt (teleportTo g0 (-500) (-3430) 0) .lostSoul (-200) (-3430)
    let soulRun := run {} 400 soulG
    r ← check r "a lost soul charges the player"
      (soulRun.status.health < 100 ||
        soulRun.mobjs.any fun m => m.kind == .lostSoul && m.distanceTo (-500) (-3430) < 150)
    -- The revenant's homing rocket = vanilla `A_Tracer`: it steers only on
    -- every fourth tic, snaps its heading one `TRACEANGLE` step (~16.875°)
    -- toward the target, nudges its climb rate a flat 1/8, and trails smoke.
    r ← check r "the revenant rocket homes like vanilla A_Tracer"
      (Id.run do
        let base := teleportTo g0 (-500) (-3430) 0
        let (base, mi) := base.spawn .revenantMissile (-300) (-3430) 0
        let speed := (ActorInfo.ofKind .revenantMissile).speed
        -- heading due east, with the player due west and 40 below the aim point
        let base := base.setMobj mi { base.mobjs[mi]! with
          momX := speed, momY := 0, momZ := 0, z := base.player.z, target := 0 }
        -- an off-beat tic: A_Tracer bails, leaving heading and world untouched
        let idle := ({ base with tics := 3 }).traceMissile mi
        let stood := idle.mobjs[mi]!.momX == speed && idle.mobjs[mi]!.momY == 0.0
          && !idle.mobjs.any (·.kind == .puff)
        -- an on-beat tic: one TRACEANGLE step, a +1/8 climb, and a smoke puff
        let steer := ({ base with tics := 4 }).traceMissile mi
        let r := steer.mobjs[mi]!
        let ang := Float.atan2 r.momY r.momX
        let turned := Float.abs ang > 0.29 && Float.abs ang < 0.30
        let climbed := r.momZ > 0.12 && r.momZ < 0.13
        let smoked := steer.mobjs.any (·.kind == .puff)
        return stood && turned && climbed && smoked)
    r ← check r "E1M8 spawns Barons of Hell"
      (match Level.load wad "E1M8" with
       | .ok l8 => ((GameState.newGame l8).mobjs.filter (·.kind == .baron)).size ≥ 2
       | .error _ => false)
    r ← check r "spectres render as shadows"
      ((ActorInfo.ofKind .spectre).shadow
        && !(ActorInfo.ofKind .demon).shadow)
    r ← check r "E2M8 spawns the Cyberdemon"
      (match Level.load wad "E2M8" with
       | .ok l => (GameState.newGame l).mobjs.any (·.kind == .cyberdemon)
       | .error _ => false)
    r ← check r "E3M8 and E4M8 spawn the Spider Mastermind"
      ((match Level.load wad "E3M8" with
        | .ok l => (GameState.newGame l).mobjs.any (·.kind == .spiderMastermind)
        | .error _ => false) &&
       (match Level.load wad "E4M8" with
        | .ok l => (GameState.newGame l).mobjs.any (·.kind == .spiderMastermind)
        | .error _ => false))
    -- Every state table is hand-written with computed indices, so a typo
    -- shows up as an entry point or a `next` pointing off the end. Check
    -- all of them at once rather than trusting the arithmetic.
    let allKinds : List ActorKind :=
      [.zombieman, .shotgunGuy, .imp, .demon, .spectre, .cacodemon,
       .lostSoul, .baron, .cyberdemon, .spiderMastermind, .chaingunner,
       .wolfSS, .hellKnight, .mancubus, .arachnotron, .revenant,
       .painElemental, .archVile, .commanderKeen, .vileFire, .barrel,
       .impBall, .cacoBall, .baronBall, .rocket, .plasmaBall, .bfgBall,
       .fatShot, .arachPlasma, .revenantMissile]
    let badKinds := allKinds.filter fun k =>
      let i := ActorInfo.ofKind k
      let n := i.states.size
      let inRange := fun (o : Option Nat) => match o with
        | some v => v < n
        | none => true
      !(n > 0 && i.spawnState < n
        && inRange i.seeState && inRange i.painState && inRange i.missileState
        && inRange i.meleeState && inRange i.deathState && inRange i.xdeathState
        && i.states.all fun s => match s.next with
           | some v => v < n
           | none => true)
    unless badKinds.isEmpty do
      IO.println s!"  (out-of-range state tables: {badKinds.length})"
    r ← check r "every actor's state table is internally consistent"
      badKinds.isEmpty
    -- COVERAGE SWEEP. Every distinct linedef special and thing type the
    -- IWAD actually uses, checked by *exercising* it rather than against a
    -- hand-kept list — a list would drift out of date exactly when it
    -- mattered. A special is "dead" if activating it, by use and by walk,
    -- changes nothing observable; a thing type is dead if it maps to no
    -- actor. These are the gaps that stop a map being completable.
    -- Specials that legitimately do nothing when fired from a fresh state.
    -- Listing them with a reason keeps the sweep honest: a genuine gap
    -- shows up as a number that is *not* here.
    let inertByDesign : Array Nat := #[
      48,          -- scrolling wall: animated by `stepScrollers`, not a trigger
      74, 89, 54, 57,  -- "stop crusher/lift": nothing is running yet
      125, 126,    -- monster-only teleports: never move the player
      65535        -- a malformed special in the IWAD's own data
    ]
    -- Known gaps: shooting a line to trigger it needs hitscan plumbing that
    -- does not exist yet. Listed so the sweep stays green and the debt stays
    -- visible; delete an entry the moment it is implemented.
    let notYetImplemented : Array Nat := #[]  -- all clear
    let fingerprint := fun (g : GameState) =>
      (g.movers.size, g.exited, g.secretExit,
       g.level.sectors.foldl (fun a s => a + s.floorH + s.ceilH + Float.ofNat s.light) 0.0,
       (g.level.linedefs.filter (·.special != 0)).size,
       g.player.x, g.player.y)   -- teleports move the player and nothing else
    -- one decode of the episodes, shared by both sweeps below; a marker that
    -- fails to decode is a failure rather than a quiet skip
    let (episodeMaps, episodeFails) := loadMaps wad doomMapNames
    unless episodeFails.isEmpty do
      IO.println s!"  (maps that failed to load: {episodeFails.toList})"
    r ← check r "every episode map the WAD carries decodes"
      episodeFails.isEmpty
    r ← check r "the special sweeps actually had maps to sweep"
      (episodeMaps.size ≥ 9)
    let deadSpecials := Id.run do
      let mut dead : Array Nat := #[]
      let mut seen : Array Nat := #[]
      for (_, lvl) in episodeMaps do
        -- every key, so locked doors are not mistaken for dead ones
        let g0 := GameState.newGame lvl
        let g0 := { g0 with status := { g0.status with
          blueKey := true, yellowKey := true, redKey := true } }
        for i in [0:lvl.linedefs.size] do
          let sp := lvl.linedefs[i]!.special
          if sp == 0 || seen.contains sp || dead.contains sp then continue
          if inertByDesign.contains sp then continue
          seen := seen.push sp
          let byUse := g0.activateLine i (byUse := true)
          let byWalk := g0.activateLine i (byUse := false)
          if fingerprint byUse == fingerprint g0
              && fingerprint byWalk == fingerprint g0 then
            dead := dead.push sp
      return dead
    let deadThings := Id.run do
      let mut dead : Array Nat := #[]
      for (_, lvl) in episodeMaps do
        for t in lvl.things do
          -- 1–4 player starts, 11 deathmatch start, 14 teleport exit:
          -- all handled outside `ofThingType`, by design
          if t.type ≤ 4 || t.type == 11 || t.type == 14 then continue
          if (ActorKind.ofThingType t.type).isNone && !dead.contains t.type then
            dead := dead.push t.type
      return dead
    unless deadSpecials.isEmpty do
      IO.println s!"  (known-unimplemented specials still dead: {deadSpecials.toList})"
    unless deadThings.isEmpty do
      IO.println s!"  (thing types that spawn nothing: {deadThings.toList})"
    r ← check r "every linedef special the IWAD uses actually does something"
      (deadSpecials.all notYetImplemented.contains)
    r ← check r "every thing type the IWAD places actually spawns"
      deadThings.isEmpty

    -- A linedef wearing a switch texture is a switch: if it carries a
    -- special but `answersToUse` says no, pressing it silently does
    -- nothing — the exact shape of the E3M1 and MAP01 bugs. Use the WAD
    -- itself as ground truth rather than a hand-kept list.
    let unpressable := Id.run do
      let mut bad : Array (String × Nat) := #[]
      for (name, lvl) in episodeMaps do
            for i in [0:lvl.linedefs.size] do
              let line := lvl.linedefs[i]!
              if line.special == 0 then continue
              let sd := lvl.sidedefs[line.front]!
              let isSwitch := [sd.upper, sd.middle, sd.lower].any fun t =>
                (Level.switchTwin t).isSome
              if isSwitch && !GameState.answersToUse line.special then
                bad := bad.push (name, line.special)
      return bad
    unless unpressable.isEmpty do
      IO.println s!"  (unpressable switches: {unpressable.toList})"
    r ← check r "every switch-textured line answers to Use"
      unpressable.isEmpty

    -- Two switches that did nothing: E3M1's tag-9 switch is special 18
    -- (raise to next higher floor) and MAP01's north-west one is 102
    -- (lower to highest floor). Neither type was implemented at all.
    r ← check r "E3M1's tag-9 switch raises its floor when used"
      (match Level.load wad "E3M1" with
       | .ok l3 =>
         let g3 := GameState.newGame l3
         match (Id.run do
           for i in [0:l3.linedefs.size] do
             if l3.linedefs[i]!.special == 18 then return some i
           return none) with
         | some i =>
           let before := (l3.sectorsTagged l3.linedefs[i]!.tag)[0]!
           let after := (g3.activateLine i (byUse := true))
           -- a mover must have been scheduled for the tagged sector
           after.movers.any (·.sector == before)
         | none => false
       | .error _ => false)
    r ← check r "Doom II's bestiary is wired to its doomednums"
      (ActorKind.ofThingType 65 == some .chaingunner
        && ActorKind.ofThingType 66 == some .revenant
        && ActorKind.ofThingType 67 == some .mancubus
        && ActorKind.ofThingType 68 == some .arachnotron
        && ActorKind.ofThingType 69 == some .hellKnight
        && ActorKind.ofThingType 84 == some .wolfSS)
    r ← check r "the Hell Knight is a lighter Baron on its own sprite"
      ((ActorInfo.ofKind .hellKnight).health == 500
        && (ActorInfo.ofKind .baron).health == 1000
        && (ActorInfo.ofKind .hellKnight).sprite == "BOS2"
        && (ActorInfo.ofKind .hellKnight).states.size
             == (ActorInfo.ofKind .baron).states.size)
    r ← check r "the super shotgun takes two shells and shares slot 3"
      (Weapon.ammoCost .superShotgun == 2
        && Weapon.ammoType .superShotgun == some .shells
        && Weapon.sprite .superShotgun == "SHT2")
    -- An arch-vile walks its dead back onto their feet.
    r ← check r "an arch-vile resurrects a corpse it stands over"
      (Id.run do
        let g := teleportTo g0 (-500) (-3430) 0
        let g := spawnAt g .demon (-450) (-3430)
        let di := g.mobjs.size - 1
        let g := run {} 20 (g.damageMobj di 500)   -- kill it, let it fall
        let corpsed := g.mobjs[di]!.corpse
        let g := spawnAt g .archVile (-450) (-3430)
        let vi := g.mobjs.size - 1
        let g := { g with mobjs := g.mobjs.modify vi fun m =>
          { m with awake := true, state := m.info.seeState.getD m.state } }
        let after := run {} 400 g
        return corpsed && after.mobjs.any fun m =>
          m.kind == .demon && !m.corpse && m.health > 0)
    -- Invisibility: a blursphere scatters the monsters shooting at you.
    let shotDamage := fun (invis : Bool) => Id.run do
      let g := teleportTo g0 (-500) (-3430) 0
      let g := { g with status := { g.status with health := 4000, invisTics := if invis then 9000 else 0 } }
      let g := spawnAt g .shotgunGuy (-260) (-3430)   -- east, facing the player
      let si := g.mobjs.size - 1
      let mut g := { g with mobjs := g.mobjs.modify si fun m =>
        { m with awake := true, state := m.info.seeState.getD m.state } }
      -- pin the player each tic: knockback would otherwise drift them out of
      -- the line of fire and swamp the aim-scatter this test measures
      for _ in [0:800] do
        g := tick {} g
        g := { g with player := { g.player with
          x := -500, y := -3430, momX := 0, momY := 0 } }
      return 4000 - g.status.health
    r ← check r "a blursphere makes the monsters shooting you miss more"
      (shotDamage true < shotDamage false)
    -- Vanilla MF_SHADOW only fuzzes the *monsters'* aim (tested above): the
    -- player's own bullets land on a spectre exactly as they do on a demon.
    -- The two runs are bit-identical simulations apart from the shadow
    -- flag, so the tic counts must match exactly.
    let shotsToKill := fun (kind : ActorKind) => Id.run do
      -- park the target dead ahead and hose it with the chaingun
      let g := teleportTo g0 (-500) (-3430) 0
      let g := { g with status := { g.status with weapon := .chaingun, ownsChaingun := true, bullets := 400 } }
      let g := spawnAt g kind (-300) (-3430)
      let ti := g.mobjs.size - 1
      let mut g := g
      let mut n := 0
      for _ in [0:400] do
        g := tick { fire := true } g
        n := n + 1
        if g.mobjs[ti]!.corpse then break
      return n
    r ← check r "player bullets hit a spectre exactly as they hit a demon"
      (shotsToKill .spectre == shotsToKill .demon && shotsToKill .demon < 400)
    -- Knockback (vanilla `P_DamageMobj`): a hit thrusts its target away from
    -- the inflictor, harder the lighter the target's `mass`.
    let knockMom := fun (kind : ActorKind) => Id.run do
      let g := spawnAt g0 kind (-300) (-3430)
      let mi := g.mobjs.size - 1
      -- blast from the west (−340): shoves the target east, +momX
      let g := g.damageMobj mi 100 (inflictor := some (-340, -3430))
      return g.mobjs[mi]!.momX
    r ← check r "a damaging hit thrusts the target away from the inflictor"
      (knockMom .demon > 1.0)
    r ← check r "knockback is lighter on heavier monsters (vanilla mass)"
      (knockMom .lostSoul > knockMom .mancubus)
    -- the shove carries into the corpse: the killing blow sends it skidding,
    -- and it moves under its own momentum on the following tics
    r ← check r "the killing blow sends the corpse skidding away"
      (Id.run do
        let g := spawnAt g0 .demon (-300) (-3430)
        let mi := g.mobjs.size - 1
        let g := g.damageMobj mi 500 (inflictor := some (-360, -3430))
        let x0 := g.mobjs[mi]!.x
        let corpse := g.mobjs[mi]!.corpse
        let g := run {} 16 g
        return corpse && g.mobjs[mi]!.x > x0 + 2.0)
    -- the player is shoved too — the same rule that lets a rocket-jump work
    r ← check r "a blast knocks the player back, but crushers (no inflictor) don't"
      (Id.run do
        let g := teleportTo g0 (-500) (-3430) 0
        let shoved := (g.damagePlayer 40 (inflictor := some (-560, -3430))).player.momX
        let crushed := (g.damagePlayer 40).player.momX
        return shoved > 1.0 && Float.abs crushed < 0.001)
    -- The chainsaw's four voices (vanilla A_Saw / P_BringUpWeapon): a rev on
    -- bring-up, a putter while idle, and a bite-vs-air growl on the swing.
    r ← check r "raising the chainsaw revs it up (sawUp)"
      (Id.run do
        let g := teleportTo g0 (-500) (-3430) 0
        let g := { g with status := { g.status with ownsChainsaw := true, weapon := .fist } }
        let g := tick { weapon := some 1 } g       -- slot 1: fist → chainsaw
        let g := run {} 40 g
        return g.status.weapon == .chainsaw && g.sounds.any (·.1 == Sfx.sawUp))
    r ← check r "a held chainsaw keeps up its idle putter (sawIdle)"
      (Id.run do
        let g := teleportTo g0 (-500) (-3430) 0
        let g := { g with status := { g.status with
          ownsChainsaw := true, weapon := .chainsaw, weaponY := 32 } }
        let g := run {} 40 g
        return g.sounds.any (·.1 == Sfx.sawIdle))
    let sawBites := fun (target : Bool) => Id.run do
      let g := teleportTo g0 (-500) (-3430) 0
      let g := { g with status := { g.status with
        ownsChainsaw := true, weapon := .chainsaw, weaponY := 32 } }
      let g := if target then spawnAt g .demon (-460) (-3430) else g  -- dead ahead
      let g := run { fire := true } 4 g
      return g.sounds.any (·.1 == Sfx.sawHit)
    r ← check r "the chainsaw growls sawhit on a bite, not on empty air"
      (sawBites true && !sawBites false)
    -- Monster wake-up by sound (vanilla P_NoiseAlert / soundtarget): a gunshot
    -- floods an alert out through the map's open geometry.
    r ← check r "the sound flood marks the origin and keeps prior alerts"
      (Id.run do
        let lvl := g0.level
        let n := lvl.sectors.size
        let flooded := lvl.soundFlood 0 #[]
        let seeded := lvl.soundFlood 0 ((Array.replicate n false).set! (n - 1) true)
        let oob := lvl.soundFlood (n + 5) (Array.replicate n false)
        return flooded.size == n && flooded[0]!          -- origin heard it
          && seeded[n - 1]!                                -- a prior alert is kept
          && oob == Array.replicate n false)               -- bad origin: no-op
    r ← check r "firing a gun floods the alert out from the player's sector"
      (Id.run do
        let g := teleportTo g0 (-500) (-3430) 0
        let g := { g with status := { g.status with weapon := .pistol, bullets := 50 } }
        let empty := g.alerted.isEmpty
        -- the pistol's first frame is a 4-tic wind-up; it fires on the next
        let after := run { fire := true } 8 g
        let psec := after.level.sectorAt after.player.x after.player.y
        return empty && after.alerted.size == after.level.sectors.size
          && after.alerted[psec]!)
    -- `A_Look` in vanilla's two stages. A monster that hears the alert wakes
    -- on the noise alone; a deaf (`MF_AMBUSH`) one answers the same alert
    -- only when it can *see* the player — and then from any direction, since
    -- the ±90° cone belongs to `P_LookForPlayers`, not the soundtarget path.
    let lookTest := fun (x y : Float) (amb heard : Bool) => Id.run do
      let g := teleportTo g0 (-500) (-3430) 0
      let g := spawnAt g .zombieman x y
      let mi := g.mobjs.size - 1
      -- faced north, away from the player to the south of it
      let g := { g with mobjs := g.mobjs.modify mi ({ · with angle := 1.5707963 }) }
      let sec := g.level.sectorAt g.mobjs[mi]!.x g.mobjs[mi]!.y
      let alert := (Array.replicate g.level.sectors.size false).set! sec true
      let g := { g with alerted := if heard then alert else #[]
                        mobjs := g.mobjs.modify mi ({ · with ambush := amb }) }
      -- long enough for the 10-tic look state to run its A_Look action
      return (run {} 15 g).mobjs[mi]!.awake
    -- (-500, -3200) is in the player's sight; (-500, -2800) is not
    r ← check r "silence leaves a monster facing away asleep"
      (!lookTest (-500) (-2800) false false)
    r ← check r "an alerted sector wakes a monster that can hear"
      (lookTest (-500) (-2800) false true)
    r ← check r "a deaf ambusher ignores the alert when it cannot see"
      (!lookTest (-500) (-2800) true true)
    r ← check r "…but a deaf ambusher in sight wakes, facing or not"
      (lookTest (-500) (-3200) true true)
    -- Every projectile does its damage × one d8 (vanilla's uniform roll),
    -- not a sum of dice — a bell curve would shift the whole distribution.
    r ← check r "monster projectiles roll damage × 1d8, the vanilla way"
      ([ActorKind.impBall, .cacoBall, .baronBall, .fatShot, .arachPlasma,
        .revenantMissile, .rocket, .plasmaBall, .bfgBall].all fun k =>
        (ActorInfo.ofKind k).damageDice == (1, 8))
    -- and the per-projectile multipliers match `info.damage`
    r ← check r "projectile damage multipliers match vanilla mobjinfo"
      ((ActorInfo.ofKind .impBall).damageMult == 3
        && (ActorInfo.ofKind .cacoBall).damageMult == 5
        && (ActorInfo.ofKind .baronBall).damageMult == 8
        && (ActorInfo.ofKind .fatShot).damageMult == 8
        && (ActorInfo.ofKind .arachPlasma).damageMult == 5
        && (ActorInfo.ofKind .revenantMissile).damageMult == 10
        && (ActorInfo.ofKind .rocket).damageMult == 20
        && (ActorInfo.ofKind .bfgBall).damageMult == 100)
    r ← check r "bullet puffs hang in the air, but blood has weight and falls"
      ((ActorInfo.ofKind .puff).noGravity && !(ActorInfo.ofKind .blood).noGravity)
    r ← check r "hanging gore hangs from the ceiling, columns stand on the floor"
      ((ActorInfo.ofKind (.scenery "GOR1" "ABCB" true)).ceilingHang
        && (ActorInfo.ofKind (.scenery "HDB1" "A" true)).ceilingHang
        && !(ActorInfo.ofKind (.scenery "POL1" "A" true)).ceilingHang)
    r ← check r "the Icon of Sin's parts are wired to their doomednums"
      (ActorKind.ofThingType 88 == some .iconBrain
        && ActorKind.ofThingType 89 == some .iconSpit
        && ActorKind.ofThingType 87 == some .iconTarget)
    -- Rocketing the brain to death ends the level (and Doom II) — but only
    -- after the ~120-tic death cascade finishes playing out.
    r ← check r "destroying the Icon of Sin's brain wins the level"
      (Id.run do
        let brainG := spawnAt g0 .iconBrain (-200) (-3430)
        let bi := brainG.mobjs.size - 1
        -- kill it, then let its death state run out into the win
        let dead := brainG.damageMobj bi 500
        -- the win does not land early: mid-cascade the level is still going
        let mid := run {} 40 dead
        let won := run {} 130 dead
        return !mid.exited && won.exited)
    -- The death throes: a wall-wide burst of explosions and the boss roar.
    r ← check r "the Icon of Sin's death erupts in a cascade of explosions"
      (Id.run do
        let brainG := spawnAt g0 .iconBrain (-200) (-3430)
        let bi := brainG.mobjs.size - 1
        let dead := brainG.damageMobj bi 500
        -- a dozen tics past the scream frame the wall is full of burst puffs
        let mid := run {} 12 dead
        let booms := (mid.mobjs.filter (·.kind == .brainExplosion)).size
        return booms > 30 && mid.sounds.any fun (s, _, _) => s == Sfx.bosDeath)
    -- The spitter opens MAP30 with a roar (`A_BrainAwake`).
    r ← check r "the Icon of Sin's spitter wakes with a roar"
      (Id.run do
        let woke := run {} 12 (spawnAt g0 .iconSpit (-200) (-3430))
        return woke.sounds.any fun (s, _, _) => s == Sfx.bosSight)
    -- It then flings cubes at the target spots, and a cube births a monster.
    r ← check r "the spitter flings cubes that birth monsters"
      (Id.run do
        let g := spawnAt g0 .iconSpit (-200) (-3430)
        let g := spawnAt g .iconTarget (-600) (-3430)
        let out := run {} 260 g
        let spat := out.sounds.any fun (s, _, _) => s == Sfx.bosSpit
        let born := out.mobjs.any (·.info.countKill)
        return spat && born)
    r ← check r "the episode bosses are the heaviest things in the bestiary"
      ((ActorInfo.ofKind .cyberdemon).health == 4000
        && (ActorInfo.ofKind .spiderMastermind).health == 3000
        && (ActorInfo.ofKind .cyberdemon).countKill
        && (ActorInfo.ofKind .spiderMastermind).countKill)

    -- E1M3's stair builder raises a rising case of floors.
    match Level.load wad "E1M3" with
    | .error e => IO.eprintln s!"  E1M3: {e}"; r ← check r "setup: E1M3 loads" false
    | .ok l3 =>
      let g3 := GameState.newGame l3
      let stairLine := Id.run do
        for i in [0:l3.linedefs.size] do
          if l3.linedefs[i]!.special == 8 then return i
        return 0
      let tag := l3.linedefs[stairLine]!.tag
      let base := (l3.sectorsTagged tag)[0]!
      let before := l3.sectors[base]!.floorH
      let built := run {} 600 (g3.activateLine stairLine (byUse := false))
      r ← check r "the stair builder raises the tagged floor by 8"
        (built.level.sectors[base]!.floorH == before + 8)
      r ← check r "later steps rise higher than the first"
        (built.movers.isEmpty && Id.run do
          for s in [0:built.level.sectors.size] do
            if built.level.sectors[s]!.floorH > l3.sectors[s]!.floorH + 8 then
              return true
          return false)

    -- E1M9's teleporter moves the player to the tagged destination.
    match Level.load wad "E1M9" with
    | .error e => IO.eprintln s!"  E1M9: {e}"; r ← check r "setup: E1M9 loads" false
    | .ok l9 =>
      let g9 := GameState.newGame l9
      let teleLine := Id.run do
        for i in [0:l9.linedefs.size] do
          let sp := l9.linedefs[i]!.special
          if sp == 39 || sp == 97 then return i
        return 1000000
      if teleLine == 1000000 then
        r ← check r "E1M9 has a teleporter" false
      else
        let tag := l9.linedefs[teleLine]!.tag
        let ported := g9.activateLine teleLine (byUse := false)
        let dest := l9.things.find? fun t =>
          t.type == 14 && l9.sectors[l9.sectorAt t.x t.y]!.tag == tag
        r ← check r "the teleporter lands the player on the destination pad"
          (dest.any fun t =>
            ported.player.x == t.x && ported.player.y == t.y
              && ported.mobjs.any (·.kind == .teleFog))
        -- Vanilla `EV_Teleport` refuses a crossing from the back of the
        -- line, "so you can get out of teleporter" — without it every
        -- landing pad throws you straight back as you step off it.
        let fromBack := g9.activateLine teleLine (byUse := false) (side := 1)
        r ← check r "crossing a teleport line from behind does nothing"
          (fromBack.player.x == g9.player.x && fromBack.player.y == g9.player.y)
        -- and the arrival freeze: vanilla sets the player's reactiontime
        -- to 18, so they stand still for a moment after landing
        r ← check r "the player holds still briefly after teleporting"
          (ported.teleFreeze == 18
            && (tick { forward := true } ported).player.x == ported.player.x)
        -- Monsters take teleporters too: vanilla runs the same `spechit`
        -- walk for anything that moves, filtered to the few specials a
        -- non-player may work.
        let dx := dest.map (·.x) |>.getD 0
        let dy := dest.map (·.y) |>.getD 0
        let (gm, mi) := g9.spawn .imp g9.player.x g9.player.y 0
        let mBefore := gm.mobjs[mi]!
        let mPorted := gm.activateLine teleLine (byUse := false) (actor := some mi)
        r ← check r "a monster crossing a teleport line is teleported too"
          (mPorted.mobjs[mi]!.x == dx && mPorted.mobjs[mi]!.y == dy)
        r ← check r "a teleported monster gets no arrival freeze (players only)"
          (mPorted.teleFreeze == 0)
        -- vanilla `PIT_StompThing`: "monsters don't stomp things except on
        -- boss level", so an occupied pad simply refuses the monster
        let (gb, blocker) := gm.spawn .demon dx dy 0
        let blockedTp := gb.activateLine teleLine (byUse := false) (actor := some mi)
        r ← check r "a monster will not stomp its way onto an occupied pad"
          (blockedTp.mobjs[mi]!.x == mBefore.x
            && blockedTp.mobjs[blocker]!.health
                 == blockedTp.mobjs[blocker]!.info.health)
        -- the player, by contrast, telefrags whatever is standing there
        let stomped := gb.activateLine teleLine (byUse := false)
        r ← check r "the player telefrags whatever is on the destination pad"
          (stomped.mobjs[blocker]!.health ≤ 0)
        let (gr, ri) := g9.spawn .rocket g9.player.x g9.player.y 0
        let rPorted := gr.activateLine teleLine (byUse := false) (actor := some ri)
        r ← check r "a missile is never teleported"
          (rPorted.mobjs[ri]!.x == gr.mobjs[ri]!.x)
        r ← check r "monsters work only the teleports, one door and the lifts"
          (GameState.monsterCanCross 97 && GameState.monsterCanCross 125
            && GameState.monsterCanCross 4 && GameState.monsterCanCross 88
            && !GameState.monsterCanCross 1 && !GameState.monsterCanCross 11
            && !GameState.monsterCanCross 31)

  IO.println "saves, menus, widescreen:"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1 failed to load: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
      let mut g := g
      for _ in [0:n] do
        g := tick input g
      return g
    -- Play a little (fight, open the door, take damage), then round-trip.
    let g0 := GameState.newGame lvl
    let g1 := run { forward := true, fire := true } 150 g0
    let saved := Save.saveGame g1
    match Save.loadGame wad saved with
    | .error e =>
      IO.eprintln s!"  load failed: {e}"
      r ← check r "a save round-trips" false
    | .ok g2 =>
      -- Positions travel as 16.16 fixed point, so a reload lands within
      -- 1/65536 of where it left off. `P_SlideMove` is continuous in the
      -- geometry — unlike stepping one axis at a time, which quantised such
      -- differences away — so that last bit can grow over a few dozen tics.
      -- Compare to the precision the format actually promises, not tighter.
      let closef := fun (a b : Float) => Float.abs (a - b) < 0.001
      let replayClose := fun (a b : Float) => Float.abs (a - b) < 0.05
      r ← check r "a save round-trips the player"
        (closef g2.player.x g1.player.x && closef g2.player.y g1.player.y
          && closef g2.player.angle g1.player.angle)
      r ← check r "…the vitals and arsenal"
        (g2.status.health == g1.status.health
          && g2.status.bullets == g1.status.bullets)
      r ← check r "…the monsters, in their exact states"
        (g2.mobjs.size == (g1.mobjs.filter (!·.removed)).size
          && (g2.mobjs.zip (g1.mobjs.filter (!·.removed))).all fun (a, b) =>
               a.kind == b.kind && a.state == b.state && a.health == b.health
                 && a.awake == b.awake)
      r ← check r "…the sector heights"
        ((g2.level.sectors.zip g1.level.sectors).all fun (a, b) =>
          closef a.floorH b.floorH && closef a.ceilH b.ceilH)
      -- determinism: the loaded game evolves exactly like the original
      let a := run { turnLeft := true, forward := true } 30 g1
      let b := run { turnLeft := true, forward := true } 30 g2
      r ← check r "…and the loaded game replays deterministically"
        (replayClose a.player.x b.player.x && replayClose a.player.y b.player.y
          && a.status.health == b.status.health)
    -- Animated sector lights must not have their bright/dark ranges clamped
    -- to whatever the light happened to be at the moment of the save (the
    -- old code re-derived a thinker's ceiling from the *saved* light, so a
    -- strobe saved mid-dark-phase reloaded permanently dimmed, ratcheting
    -- darker with every save/load cycle). Run until some thinker sits below
    -- its ceiling, save there, load, and the rebuilt thinkers must carry
    -- exactly the ranges a fresh map gets.
    let ranges := fun (g : GameState) => g.lights.map fun l => match l with
      | .blink s mn mx _ => (s, mn, mx)
      | .strobe s mn mx _ _ => (s, mn, mx)
      | .glow s mn mx _ => (s, mn, mx)
      | .flicker s mn mx _ => (s, mn, mx)
    let isMidPhase := fun (g : GameState) => g.lights.any fun l => match l with
      | .blink s _ mx _ | .strobe s _ mx _ _ | .glow s _ mx _
      | .flicker s _ mx _ => g.level.sectors[s]!.light < mx
    let mut mid := run {} 100 g0
    let mut tries := 0
    while tries < 300 && !isMidPhase mid do
      mid := tick {} mid
      tries := tries + 1
    match Save.loadGame wad (Save.saveGame mid) with
    | .error e =>
      IO.eprintln s!"  mid-phase load failed: {e}"
      r ← check r "a save taken mid-strobe keeps every light's range" false
    | .ok reloaded =>
      r ← check r "a save taken mid-strobe keeps every light's range"
        (isMidPhase mid && ranges reloaded == ranges (GameState.newGame lvl))
    -- A corrupted (or stale cross-version) save must refuse with a readable
    -- error, never feed a bad index into the `[...]!` lookups at tick time.
    let refuses := fun (line : String) =>
      match Save.loadGame wad (saved.replace "\nend" s!"\n{line}\nend") with
      | .error _ => true
      | .ok _ => false
    r ← check r "a save with an out-of-range mobj state is refused"
      (refuses "mobj imp 999 0 0 0 0 0 0 0 60 9999 5 0 0 0 0")
    r ← check r "…and bad moveDir / sector / mover / weapon indices too"
      (refuses "mobj imp 999 0 0 0 0 0 0 0 60 0 5 9 0 0 0"
        && refuses "sec 99999 0 0 128 0"
        && refuses "mover door 99999 4718592 150 0 0"
        && refuses "status 100 0 50 0 0 0 0 0 0 99 0")
    -- Special 16 (vanilla `close30ThenOpen`): the door closes, sits shut
    -- for 30 seconds (35·30 tics), then reopens to its old height and the
    -- mover retires. Driven straight through `stepMovers` in an empty world
    -- so nothing under the door can reverse it.
    r ← check r "a close30 door closes, waits its 30 s, and reopens"
      (Id.run do
        let s := lvl.sectorAt 1500 (-2496)
        let top := lvl.sectors[s]!.ceilH
        let floor := lvl.sectors[s]!.floorH
        let mut g : GameState := { (emptied g0) with
          movers := #[.closeOpen s top (30 * 35) false] }
        for _ in [0:100] do g := g.stepMovers
        let shutNow := g.level.sectors[s]!.ceilH == floor && g.movers.size == 1
        for _ in [0:900] do g := g.stepMovers   -- 1000 in: wait still running
        let stillShut := g.level.sectors[s]!.ceilH == floor
        for _ in [0:350] do g := g.stepMovers   -- the wait lapses; it reopens
        return shutNow && stillShut
          && g.level.sectors[s]!.ceilH == top && g.movers.isEmpty)
    match Assets.load wad with
    | .error e => IO.eprintln s!"  assets: {e}"; r ← check r "setup: assets load" false
    | .ok assets =>
      r ← check r "menu graphics and font load"
        (assets.graphics.contains "TITLEPIC" && assets.graphics.contains "M_DOOM"
          && assets.graphics.contains "M_SKULL1"
          && assets.graphics.contains "STCFN065")
      r ← check r "the frame is widescreen 426×200"
        (Render.screenW == 426 &&
          (Render.renderPalette lvl assets
            { x := 1056, y := -3616, height := 41, angle := 1.5707963 }).size
          == 426 * 200)
      -- Partial invisibility fuzzes your own weapon: the fuzzed frame reads
      -- the background through the gun instead of drawing its art, so it
      -- differs from the solid render.
      r ← check r "an invisible player's weapon renders as fuzz, not solid art"
        (Id.run do
          let view : Render.View :=
            { x := 1056, y := -3616, height := 41, angle := 1.5707963 }
          let base : Render.HudInfo :=
            { health := 100, armor := 0, ammo := some 50
              weapon := some ("PISGA0", 1.0, 32.0) }
          let solid := Render.renderPalette lvl assets view #[] (some base)
          let fuzzed := Render.renderPalette lvl assets view #[]
            (some { base with weaponFuzz := some 0 })
          return solid != fuzzed)

  IO.println "E1M7's first door (walk lines + use face together):"
  match Level.load wad "E1M7" with
  | .error e => IO.eprintln s!"  E1M7: {e}"; r ← check r "setup: E1M7 loads" false
  | .ok l7 =>
    let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
      let mut g := g
      for _ in [0:n] do
        g := tick input g
      return g
    -- door sector 72: use face is line 521 (east), walk-over lines 525–527
    let doorSec := 72
    let g0 := GameState.newGame l7
    -- stand just east of the face, looking west at the door
    let atDoor := { g0 with player := { g0.player with
        x := -488, y := -96, angle := 3.14159265
        z := l7.sectors[l7.sectorAt (-488) (-96)]!.floorH } }
    let closedCeil := l7.sectors[doorSec]!.ceilH
    -- a walk trigger firing while the door is already moving must not
    -- toggle it (vanilla: tagged doors skip busy sectors)
    let opening := run {} 10 (run { «use» := true } 1 atDoor)
    let risen := opening.level.sectors[doorSec]!.ceilH
    let poked := run {} 5 (opening.activateLine 526 (byUse := false))
    r ← check r "a walk trigger can't slam a moving door"
      (poked.level.sectors[doorSec]!.ceilH > risen)
    -- walking through the open doorway (crossing its walk lines) must
    -- leave it open
    let opened := run {} 40 opening
    let walked := run { forward := true, run := true } 30 opened
    r ← check r "walking through the doorway doesn't shut it on you"
      (walked.level.sectors[doorSec]!.ceilH
        ≥ opened.level.sectors[doorSec]!.ceilH - 0.01)
    -- the user's exact repro: open, re-use mid-rise (closes), then use
    -- again once shut — it must reopen, even standing on the walk lines
    let mut g := run { «use» := true } 1 atDoor
    g := run {} 10 g
    g := run { «use» := true } 1 g   -- toggle shut
    g := run {} 200 g
    let shut := g.level.sectors[doorSec]!.ceilH == closedCeil
    -- hugging the face, press use: it must reopen. Reset position *and*
    -- momentum: monsters woke during the wait and their fire knocks the
    -- player around now, which would otherwise drift them off the door face.
    g := { g with player := { g.player with x := -510, y := -96, momX := 0, momY := 0 } }
    g := run { «use» := true } 1 g
    g := run {} 30 g
    r ← check r "after toggling it shut, use reopens it"
      (shut && g.level.sectors[doorSec]!.ceilH > closedCeil + 20)
    -- the same full story from the east corridor (its use line is the
    -- far face, so the reach is tight — but it must work)
    let east := { g0 with player := { g0.player with
        x := -470, y := -96, angle := 3.14159265
        z := l7.sectors[l7.sectorAt (-470) (-96)]!.floorH } }
    let mut e := run { «use» := true } 1 east
    e := run {} 8 e
    let eastOpening := e.level.sectors[doorSec]!.ceilH > closedCeil
    e := run { «use» := true } 1 e
    e := run {} 300 e
    let eastShut := e.level.sectors[doorSec]!.ceilH == closedCeil
    -- re-hug the face (knockback from woken monsters may have nudged us off)
    e := { e with player := { e.player with x := -470, y := -96, momX := 0, momY := 0 } }
    e := run { «use» := true } 1 e
    e := run {} 30 e
    r ← check r "…and the same round-trip works from the east side"
      (eastOpening && eastShut
        && e.level.sectors[doorSec]!.ceilH > closedCeil + 20)

  IO.println "E1M7's spawn-room door (two-sided D1, must never trap):"
  match Level.load wad "E1M7" with
  | .error e => IO.eprintln s!"  E1M7: {e}"; r ← check r "setup: E1M7 loads" false
  | .ok l7 =>
    let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
      let mut g := g
      for _ in [0:n] do
        g := tick input g
      return g
    -- Sector 62, right at the player start, is a D1 "open and stay open,
    -- once" door with an independent one-shot switch on each face: line
    -- 950 faces the spawn room, line 949 faces the far side. Opening it
    -- from one face and then hitting the other face's switch mid-rise
    -- must NOT reverse it — vanilla only lets DR doors (1/26/27/28)
    -- reverse when caught moving; D1 (31) ignores a second trigger.
    let doorSec := 62
    let g0 := GameState.newGame l7
    r ← check r "the door starts shut with two untouched D1 switches"
      (l7.sectors[doorSec]!.ceilH == 0
        && l7.linedefs[949]!.special == 31 && l7.linedefs[950]!.special == 31)
    let g1 := g0.activateLine 950 (byUse := true)
    let midRise := run {} 6 g1
    r ← check r "the spawn-side switch opens it and consumes itself"
      (midRise.level.sectors[doorSec]!.ceilH > 0
        && midRise.level.linedefs[950]!.special == 0)
    let poked := midRise.activateLine 949 (byUse := true)
    let settled := run {} 300 poked
    r ← check r "the far switch mid-rise doesn't reverse it shut"
      (settled.level.sectors[doorSec]!.ceilH > 60)
    r ← check r "…it actually finished opening, not stuck mid-way"
      (settled.movers.isEmpty)

  IO.println "weapons (rocket / plasma / bfg / chainsaw) and the face:"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
      let mut g := g
      for _ in [0:n] do g := tick input g
      return g
    let base := GameState.newGame lvl
    -- park in the open nukage courtyard, facing east
    let arena := fun (x y a : Float) =>
      { base with player := { base.player with
          x := x, y := y, angle := a
          z := lvl.sectors[lvl.sectorAt x y]!.floorH } }
    -- Weapon pickups: drop each weapon on the player and walk over it.
    let grab := fun (family frames : String) =>
      let g := arena (-500) (-3430) 0
      let (g, _) := g.spawn (.item family frames) (-500) (-3430) 0
      run {} 40 g
    let rl := grab "LAUN" "A"
    r ← check r "rocket launcher: owned, switched to, +2 rockets"
      (rl.status.ownsRocket && rl.status.weapon == .rocket && rl.status.rockets == 2)
    let pl := grab "PLAS" "A"
    r ← check r "plasma rifle: owned, switched to, +40 cells"
      (pl.status.ownsPlasma && pl.status.weapon == .plasma && pl.status.cells == 40)
    let bf := grab "BFUG" "A"
    r ← check r "BFG9000: owned, switched to, +40 cells"
      (bf.status.ownsBfg && bf.status.weapon == .bfg)
    let cs := grab "CSAW" "A"
    r ← check r "chainsaw: owned and switched to"
      (cs.status.ownsChainsaw && cs.status.weapon == .chainsaw)
    let bp := grab "BPAK" "A"
    r ← check r "backpack doubles ammo capacity"
      (bp.status.backpack)

    -- Weapon switch animation: press "3" (shotgun); the pistol lowers before
    -- the shotgun rises, and the swap only lands once fully lowered.
    let switchStart := Id.run do
      let g := { base with status := { base.status with
          ownsShotgun := true, weapon := .pistol } }
      let g := tick { weapon := some 3 } g   -- begin the switch (pending set)
      tick {} g                              -- next tic: the pistol lowers
    r ← check r "a weapon switch lowers the old gun before swapping"
      (switchStart.status.pending == some .shotgun
        && switchStart.status.weapon == .pistol
        && switchStart.status.weaponY > 32)
    let switchDone := run {} 40 switchStart
    r ← check r "the switch completes with the new weapon raised"
      (switchDone.status.weapon == .shotgun && switchDone.status.pending == none
        && switchDone.status.weaponY == 32)

    -- Firing a rocket at a demon kills it (direct hit + splash).
    let rocketFight := Id.run do
      let g := arena (-600) (-3430) 0
      let g := { g with status := { g.status with
          weapon := .rocket, ownsRocket := true, rockets := 10 } }
      let (g, di) := g.spawn .demon (-320) (-3430) 3.14159
      let g := { g with mobjs := g.mobjs.modify di fun m =>
          { m with z := lvl.sectors[lvl.sectorAt (-320) (-3430)]!.floorH } }
      (run { fire := true } 80 g, di)
    let (rf, _) := rocketFight
    r ← check r "the rocket launcher blasts a demon and spends rockets"
      (rf.status.rockets < 10 && rf.mobjs.any fun m =>
        m.kind == .demon && (m.corpse || m.health < 60))

    -- The BFG spray damages a cluster of monsters at once.
    let bfgFight := Id.run do
      let g := arena (-700) (-3430) 0
      let g := { g with status := { g.status with
          weapon := .bfg, ownsBfg := true, cells := 200 } }
      let mut g := g
      -- a forward cluster: one dead ahead, two flanking within the spray fan
      for (dx, dy) in [((0:Float), (0:Float)), (30, 40), (30, -40)] do
        let ix := -350 + dx
        let iy := -3430 + dy
        let (g', i) := g.spawn .imp ix iy 3.14159
        g := { g' with mobjs := g'.mobjs.modify i fun m =>
          { m with z := lvl.sectors[lvl.sectorAt ix iy]!.floorH } }
      run { fire := true } 90 g
    let impsHurt := (bfgFight.mobjs.filter fun m =>
      m.kind == .imp && (m.corpse || m.health < 60)).size
    r ← check r "the BFG sprays a whole cluster of imps"
      (impsHurt ≥ 2 && bfgFight.status.cells < 200)

    -- The chainsaw shreds at melee range without spending ammo.
    let sawFight := Id.run do
      let g := arena (-380) (-3430) 0        -- face east, toward the demon
      let g := { g with status := { g.status with
          weapon := .chainsaw, ownsChainsaw := true } }
      let (g, di) := g.spawn .demon (-330) (-3430) 3.14159
      let g := { g with mobjs := g.mobjs.modify di fun m =>
          { m with z := lvl.sectors[lvl.sectorAt (-330) (-3430)]!.floorH } }
      run { fire := true } 120 g
    r ← check r "the chainsaw hurts a demon in melee, no ammo used"
      (sawFight.mobjs.any fun m => m.kind == .demon && m.health < 150)

    -- A rocket fired point-blank into a wall must explode and splash the
    -- shooter — it must not skip through the wall it's pressed against.
    -- There's a one-sided wall at x=1024 (y −3680..−3648) by the start.
    let wallSplash := Id.run do
      let x := 1044.0
      let y := -3664.0
      let fz := lvl.sectors[lvl.sectorAt x y]!.floorH
      let g := { base with
        player := { base.player with x := x, y := y, angle := 3.14159265, z := fz }
        status := { base.status with
          weapon := .rocket, ownsRocket := true, rockets := 5 } }
      run { fire := true } 14 g
    r ← check r "a point-blank rocket into a wall splashes the shooter"
      (wallSplash.status.health < 100)

    -- Holding fire on the plasma rifle keeps shooting at a rapid cadence:
    -- over ~20 tics it burns several cells, not one.
    let plasmaHeld := Id.run do
      let g := { base with status := { base.status with
          weapon := .plasma, ownsPlasma := true, cells := 200 } }
      run { fire := true } 21 g
    r ← check r "the plasma rifle auto-fires rapidly while held"
      (plasmaHeld.status.cells ≤ 200 - 4)

    -- Gibbing: massive overkill bursts a zombieman into the gib animation
    -- (frames M–U) and slops; a clean kill uses the plain collapse (H–L).
    let (zg, zi) := base.spawn .zombieman (-500) (-3430) 0
    let overkill := Id.run do
      let g := zg.damageMobj zi 200          -- ≫ 20 + spawnHealth: gibs
      run {} 12 g                            -- let the slop frame fire
    let normalKill := zg.damageMobj zi 25    -- lethal but not overkill
    let gibFrame := (overkill.mobjs.find? (·.uid == zg.mobjs[zi]!.uid)
      ).map (·.stateDef.frame) |>.getD ' '
    let plainFrame := (normalKill.mobjs.find? (·.uid == zg.mobjs[zi]!.uid)
      ).map (·.stateDef.frame) |>.getD ' '
    r ← check r "overkill damage gibs a zombieman"
      (gibFrame ≥ 'M' && gibFrame ≤ 'U'
        && overkill.mobjs.any (·.kind == .zombieman))
    r ← check r "a normal kill uses the plain death animation"
      (plainFrame ≥ 'H' && plainFrame ≤ 'L')

    -- The slop actually sounds: tick from the gib and catch the event.
    let slopHeard := Id.run do
      let mut g := zg.damageMobj zi 200
      let mut heard := false
      for _ in [0:12] do
        g := tick {} g
        if g.sounds.any (fun (s, _, _) => s == Sfx.slop) then heard := true
      heard
    r ← check r "the gib plays the slop sound" slopHeard

    -- The marine's face reacts to state.
    let mk := fun (f : PlayerStatus → PlayerStatus) (look : Nat) =>
      { base with status := f base.status, faceLook := look }
    r ← check r "dead shows the dead face"
      ((mk (fun s => { s with dead := true }) 0).faceLump == "STFDEAD0")
    r ← check r "god mode shows the god face"
      ((mk (fun s => { s with god := true }) 0).faceLump == "STFGOD0")
    r ← check r "a healthy idle face glances around by bracket 0"
      ((mk id 1).faceLump == "STFST01")
    r ← check r "a fresh big hit shows the ouch face"
      ((mk (fun s => { s with damageCount := 25 }) 0).faceLump == "STFOUCH0")
    r ← check r "low health picks a higher pain bracket"
      ((mk (fun s => { s with health := 15 }) 0).faceLump == "STFST40")

  IO.println "infighting, corpses on moving floors, doors vs monsters:"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
      let mut g := g
      for _ in [0:n] do g := tick input g
      return g
    let base := GameState.newGame lvl
    -- Infighting: an imp's fireball that strikes a demon makes the demon
    -- turn on the imp. Put the player out of the fight; line up imp→demon.
    let arena := { base with player := { base.player with
        x := -700, y := -3430, angle := 0
        z := lvl.sectors[lvl.sectorAt (-700) (-3430)]!.floorH } }
    let fight := Id.run do
      let (g, ii) := arena.spawn .imp (-200) (-3430) 3.14159   -- imp faces west toward demon
      let (g, di) := g.spawn .demon (-350) (-3430) 0
      let setZ := fun (g : GameState) (k : Nat) =>
        let mk := g.mobjs[k]!
        let fz := lvl.sectors[lvl.sectorAt mk.x mk.y]!.floorH
        g.setMobj k { mk with z := fz, awake := true, target := 0 }
      let g := setZ (setZ g ii) di
      -- force the imp to face and fire at the demon by aiming it west
      let g := g.setMobj ii { g.mobjs[ii]! with angle := 3.14159 }
      run {} 200 g
    let demonMad := fight.mobjs.find? (·.kind == .demon)
    r ← check r "a monster hurt by another species turns on it"
      (demonMad.any fun d => d.target != 0)

    -- `A_CPosRefire` breaks the burst off when the target goes out of
    -- sight, not only when it dies (vanilla calls `P_CheckSight`).
    let cposBreak := fun (visible : Bool) => Id.run do
      let (g, ci) := arena.spawn .chaingunner
        (if visible then arena.player.x + 64 else arena.player.x + 3000)
        arena.player.y 0
      let g := g.setMobj ci { g.mobjs[ci]! with awake := true, target := 0 }
      -- state 13 is CPOS F 1 A_CPosRefire
      let g := g.setMobj ci ((g.mobjs[ci]!).setState 13)
      let seeState := (ActorInfo.ofKind .chaingunner).seeState.getD 0
      let mut broke := 0
      for k in [0:64] do
        let gg := { g with rng := { seed := UInt32.ofNat (k * 2654435761 + 7) } }
        if ((gg.aCPosRefire ci).mobjs[ci]!).state == seeState then
          broke := broke + 1
      return broke
    r ← check r "a chaingunner keeps firing while it can see its target"
      (cposBreak true == 0)
    r ← check r "a chaingunner breaks off when the target leaves its sight"
      (cposBreak false > 0)

    -- `P_NewChaseDir` steers by `actor->target`, so a monster in an infight
    -- walks toward the monster it is fighting — not toward the player. It
    -- used to read the player either way and stroll off in the wrong
    -- direction mid-feud.
    let chaseToward : Bool → Float := fun feuding => Id.run do
      -- open ground with room to walk east and west, monsters cleared
      let px := arena.player.x
      let py := arena.player.y
      let g := emptied arena
      let (g, ei) := g.spawn .imp (px + 400) py 0        -- enemy to the EAST
      let (g, si) := g.spawn .demon (px + 200) py 0      -- chaser in between
      let eUid := g.mobjs[ei]!.uid
      let g := g.setMobj si { g.mobjs[si]! with
        awake := true, moveCount := 0, moveDir := 2
        target := if feuding then eUid else 0
        threshold := if feuding then 100 else 0 }
      let x0 := g.mobjs[si]!.x
      let mut gg := g
      for _ in [0:6] do gg := gg.aChase si
      return gg.mobjs[si]!.x - x0
    -- the player sits west of both, so the two cases pull opposite ways
    r ← check r "a feuding monster walks toward its enemy, not the player"
      (chaseToward true > 0)
    r ← check r "…while one hunting the player still walks toward them"
      (chaseToward false < 0)

    -- `A_VileAttack` opens with `P_CheckSight` and returns on failure, so
    -- breaking the vile's line of sight during its long wind-up fizzles the
    -- whole attack — no sound, no damage, no blast.
    let vileBurn : Float → Float → Int := fun x y => Id.run do
      let g := emptied arena
      let (g, vi) := g.spawn .archVile x y 0
      let z := lvl.sectors[lvl.sectorAt x y]!.floorH
      let g := g.setMobj vi { g.mobjs[vi]! with awake := true, target := 0, z := z }
      let before := g.status.health
      let after := (g.aVileAttack vi).status.health
      return before - after
    -- the arena puts the player at (-700, -3430) in the open
    r ← check r "an arch-vile in the open burns and launches the player"
      (vileBurn (-600) (-3430) > 0)
    r ← check r "an arch-vile that has lost sight burns nothing"
      (vileBurn 1500 (-2496) == 0)

    -- Vanilla zeroes a charging skull's momentum when it is hit, which ends
    -- the dive; the pain roll still skips it, as `MF_SKULLFLY` does there.
    let skullHit := Id.run do
      let (g, si) := arena.spawn .lostSoul (arena.player.x + 64) arena.player.y 0
      let g := g.setMobj si { g.mobjs[si]! with
        charging := true, momX := 20, momY := 5, awake := true }
      g.damageMobj si 5
    r ← check r "a hit knocks a charging lost soul out of its dive"
      (skullHit.mobjs.find? (·.kind == .lostSoul) |>.any fun s =>
        !s.charging && s.momX == 0 && s.momY == 0)

    -- Damage with nobody behind it (a crusher) hurts but does not rouse:
    -- vanilla's wake sits inside the block guarded by a non-null source.
    let (groundG, groundI) :=
      arena.spawn .imp (arena.player.x + 64) arena.player.y 0
    let crushed := groundG.damageMobj groundI 10 (wakes := false)
    let shot := groundG.damageMobj groundI 10
    r ← check r "a crusher hurts a sleeping monster without waking it"
      (!crushed.mobjs[groundI]!.awake
        && crushed.mobjs[groundI]!.health < (ActorInfo.ofKind .imp).health)
    r ← check r "…but a shot from the player does wake it"
      shot.mobjs[groundI]!.awake

    -- A corpse rides its floor down: kill a monster on a lift sector,
    -- lower that sector, and the corpse's z must follow.
    let corpseTest := Id.run do
      let (g, di) := base.spawn .zombieman 400 (-3232) 0
      let fz := lvl.sectors[lvl.sectorAt 400 (-3232)]!.floorH
      let g := g.setMobj di { g.mobjs[di]! with corpse := true, health := 0, z := fz }
      -- drop the floor under the corpse by 40 and tick
      let s := lvl.sectorAt 400 (-3232)
      let secs := g.level.sectors.modify s (fun sc => { sc with floorH := -40 })
      let g := { g with level := { g.level with sectors := secs } }
      run {} 60 g
    r ← check r "a corpse rides a lowering floor down"
      (corpseTest.mobjs.any fun m => m.corpse && m.z ≤ -38)

    -- A closing door reverses when a monster stands under it. The door
    -- behind E1M1 lines 148/149; park a demon in it while it closes.
    let doorSec := lvl.sidedefs[(lvl.linedefs[148]!.back.getD 0)]!.sector
    let doorMonster := Id.run do
      -- open the door from the front, then place a demon in the doorway.
      -- The map's own monsters are cleared: a stray zombieman round hitting
      -- the demon starts an infight and it wanders off to answer it, so the
      -- doorway would be empty long before the ceiling came down.
      let g := { (emptied base) with player := { base.player with
          x := 1500, y := -2496, angle := 0
          z := lvl.sectors[lvl.sectorAt 1500 (-2496)]!.floorH } }
      let g := run {} 60 (run { «use» := true } 1 g)   -- fully open
      let (g, di) := g.spawn .demon 1544 (-2496) 0
      -- left dormant, so it stands where it is put rather than giving chase
      let g := g.setMobj di { g.mobjs[di]! with
        z := lvl.sectors[lvl.sectorAt 1544 (-2496)]!.floorH }
      -- move the player away so only the demon occupies the doorway
      let g := { g with player := { g.player with x := 1400, y := -2400 } }
      let mut gg := g
      let mut minCeil := 1000.0
      for _ in [0:600] do
        gg := tick {} gg
        minCeil := min minCeil gg.level.sectors[doorSec]!.ceilH
      (gg, minCeil)
    let (_, minCeil2) := doorMonster
    r ← check r "a door reverses off a monster instead of crushing it"
      (minCeil2 ≥ 56 - 2.5)

    -- Timed powerups: scoop an invulnerability sphere, and the timer runs
    -- while incoming damage is nullified; a radsuit shrugs off nukage.
    let px := base.player.x
    let py := base.player.y
    let invulnGot := Id.run do
      let (g, _) := base.spawn (.item "PINV" "ABCD") px py 0
      let g := run {} 2 g            -- walk over it
      let g := { g with status := { g.status with health := 100 } }
      let g := g.damagePlayer 50     -- should be fully absorbed
      g
    r ← check r "invulnerability is picked up, times, and blocks damage"
      (invulnGot.status.invulnTics > 0 && invulnGot.status.health == (100 : Int))
    let berserkGot := Id.run do
      let (g, _) := base.spawn (.item "PSTR" "A") px py 0
      run {} 40 g
    r ← check r "a berserk pack marks berserk and swaps to the fist"
      (berserkGot.status.berserk && berserkGot.status.weapon == .fist)

    -- Animated flats phase-cycle: NUKAGE1 shows a different frame 8 tics on.
    r ← check r "animated flats advance frame with the tic clock"
      (Assets.animName "NUKAGE1" 0 == "NUKAGE1"
        && Assets.animName "NUKAGE1" 8 == "NUKAGE2"
        && Assets.animName "STARTAN3" 8 == "STARTAN3")

    -- A pressed switch flips SW1→SW2 and rebounds after the button timer.
    let switchLine := (List.range lvl.linedefs.size).find? fun i =>
      (Level.switchTwin lvl.sidedefs[lvl.linedefs[i]!.front]!.middle).isSome
    match switchLine with
    | none => pure ()
    | some li =>
      let sd := lvl.linedefs[li]!.front
      let tex := fun (g : GameState) => g.level.sidedefs[sd]!.middle
      let before := tex base
      -- E1M1's lone switch is the exit (special 11): it flips for keeps.
      let pressed := base.flipSwitch li
      r ← check r "the exit switch flips its texture permanently"
        (tex pressed != before && pressed.buttons.isEmpty
          && tex (run {} 40 pressed) != before)
      -- a rebounding button restores its texture when the timer expires
      let manual := { pressed with buttons := #[(sd, 1, before, 3)] }
      let popped := run {} 5 manual
      r ← check r "a pressed switch pops back when its button times out"
        (tex popped == before && popped.buttons.isEmpty)
      -- Two presses inside the rebound window. Vanilla's `P_StartButton`
      -- refuses a second timer for a switch it is already holding: the
      -- texture flips again (back to its unpressed face, the first press
      -- having left the pressed one on) but nothing further is queued.
      -- Queueing a second entry records the *pressed* texture as the one to
      -- restore, and being later it wins — leaving the switch stuck showing
      -- itself pressed for the rest of the map. Driven on special 138 (SR
      -- lights to full) because it is repeatable *and* always succeeds, so
      -- it reaches `flipSwitch` on both presses; E1M1's own switch is the
      -- exit, which queues no button at all.
      let repeatable := { base with level := { base.level with
        linedefs := base.level.linedefs.modify li ({ · with special := 138 }) } }
      let once := repeatable.flipSwitch li
      let twice := once.flipSwitch li
      r ← check r "a switch already rebounding queues no second button"
        (once.buttons.size == 1 && twice.buttons.size == 1)
      r ← check r "…so a double press still leaves it unpressed, not stuck"
        (tex twice == before && tex (run {} 40 twice) == before)

    -- Automap: Xiaolin Wu strokes leave partial-coverage (antialiased) pixels.
    let aaBuf := Id.run do
      let empty := ByteArray.mk
        (Array.replicate (Render.screenW * Render.screenH * 4) 0)
      Render.wuLine empty 2 2 40 18 255 255 255
    let hasPartial := Id.run do
      let mut found := false
      for p in [0 : Render.screenW * Render.screenH] do
        let v := aaBuf[p * 4]!.toNat
        if v > 0 && v < 255 then found := true
      found
    r ← check r "the automap draws antialiased (partial-coverage) lines"
      hasPartial

    -- The computer area-map reveals far more linedefs than an unexplored map.
    let mv : Render.MapView :=
      { x := base.player.x, y := base.player.y, angle := (0 : Float) }
    let lit : ByteArray → Nat := fun buf => Id.run do
      let mut c : Nat := 0
      for p in [0 : Render.screenW * Render.screenH] do
        if buf[p*4]!.toNat > 0 || buf[p*4+1]!.toNat > 0 || buf[p*4+2]!.toNat > 0 then
          c := c + 1
      return c
    let hidden := Render.automap lvl mv (Array.replicate lvl.linedefs.size false)
    let shown := Render.automap lvl { mv with revealAll := true }
      (Array.replicate lvl.linedefs.size false)
    let litShown : Nat := lit shown
    let litHidden : Nat := lit hidden
    r ← check r "the computer map reveals far more of the level"
      (litShown > litHidden + 100)
    -- An *empty* `seen` means "not tracked", not "nothing walked yet", and
    -- both `GameState.seen` and `Save.lean` promise it reads as fully
    -- revealed — a save carries no trail, and `markSeen` will not start one
    -- for an array whose size does not match the linedef count. Reading each
    -- index against a default of `false` instead left the automap blank but
    -- for the player arrow for the rest of any loaded game. A properly sized
    -- all-`false` trail must still show nothing, which is what separates the
    -- two cases.
    let untracked := Render.automap lvl mv #[]
    r ← check r "an untracked (loaded-save) trail draws the map revealed"
      (lit untracked == litShown)
    r ← check r "…while a tracked but unwalked one still draws only the arrow"
      (litHidden < 100 && litHidden > 0)

    -- Skill levels: I'm Too Young To Die spawns a lighter roster than UV.
    let easyCount := (GameState.newGame lvl none 1).mobjs.size
    let uvCount := (GameState.newGame lvl none 4).mobjs.size
    r ← check r "lower skill spawns fewer things than Ultra-Violence"
      (easyCount < uvCount && easyCount > 0)

  -- E1M8 boss trigger: felling the last Baron lowers the tag-666 sectors.
  match Level.load wad "E1M8" with
  | .error e => IO.eprintln s!"  E1M8: {e}"; r ← check r "setup: E1M8 loads" false
  | .ok l8 =>
    let g0 := GameState.newGame l8
    -- record the tag-666 sectors' starting floors, then slay every Baron
    let tagged := (List.range l8.sectors.size).filter (l8.sectors[·]!.tag == 666)
    let before := tagged.map (l8.sectors[·]!.floorH)
    let slain := Id.run do
      let mut g := g0
      for i in [0:g.mobjs.size] do
        if g.mobjs[i]!.kind == .baron then
          g := g.damageMobj i 4000
      -- let the freshly-pushed floor movers run to completion
      for _ in [0:400] do g := tick {} g
      return g
    let after := tagged.map (slain.level.sectors[·]!.floorH)
    r ← check r "E1M8: the last Baron's death lowers the tag-666 floor"
      (before.zip after |>.any fun (b, a) => a < b - 1)

  -- E4M6's boss doors: felling the Cyberdemon opens the tag-666 sectors.
  --
  -- Worth its own test because the *door* half of `A_BossDeath` has a
  -- failure the floor half cannot have. A door opens to the lowest
  -- neighbouring ceiling less 4, and every tag-666 sector on E4M6 (and
  -- MAP32) starts shut — floor and ceiling at the same height. Seed that
  -- minimum with the sector's own ceiling, as a hand-rolled scan naturally
  -- does, and a shut door yields a target *below* where its ceiling already
  -- sits: the mover retires on its first step having moved nothing, and the
  -- door stays shut for good. Vanilla seeds `P_FindLowestCeilingSurrounding`
  -- with MAXINT for exactly this reason, which is what `lowestNeighborCeil`
  -- does — so the assertion is that the door actually travels.
  for (wadName, mapName, bossKind) in
      [("doom.wad", "E4M6", ActorKind.cyberdemon),
       ("doom2.wad", "MAP32", ActorKind.commanderKeen)] do
    -- absent IWAD, or an IWAD without that map (E4M6 needs Ultimate Doom):
    -- nothing to check. A map that is *there* and fails to load is a failure.
    match ← loadWadIfPresent wadName with
    | none => pure ()
    | some bw =>
      if (bw.lastIndexOf? mapName).isNone then pure () else
      match Level.load bw mapName with
      | .error e => IO.eprintln s!"  {mapName}: {e}"; r ← check r s!"setup: {mapName} loads" false
      | .ok bl =>
        let tagged := bl.sectorsTagged 666
        let before := tagged.map (bl.sectors[·]!.ceilH)
        let slain := Id.run do
          let mut g := GameState.newGame bl
          for i in [0:g.mobjs.size] do
            if g.mobjs[i]!.kind == bossKind then
              g := g.damageMobj i 6000
          -- let the freshly-pushed door movers run to completion
          for _ in [0:400] do g := tick {} g
          return g
        let after := tagged.map (slain.level.sectors[·]!.ceilH)
        r ← check r s!"{mapName}: the last boss's death opens the tag-666 door"
          (!tagged.isEmpty
            && (before.zip after |>.all fun (b, a) => a > b + 1))

  IO.println "cheat codes:"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
      let mut g := g
      for _ in [0:n] do
        g := tick input g
      return g
    r ← check r "the scanner recognizes the classics"
      (Cheat.scan "xxdilldqd" == some .god
        && Cheat.scan "walkdillkfa" == some .kfa
        && Cheat.scan "dillclip" == some .noclip
        && Cheat.scan "dillspispopd" == some .noclip
        && Cheat.scan "dillclev13" == some (.warp 1 3)
        && Cheat.scan "dillclev" == none
        && Cheat.scan "dilldq" == none)
    -- the same two digits name different maps depending on the WAD, which
    -- is the only way MAP10-MAP32 are reachable at all
    r ← check r "a warp's digits follow the WAD's naming scheme"
      ((MapId.episode 1 1).warpTarget 1 3 == MapId.episode 1 3
        && (MapId.level 1).warpTarget 0 7 == MapId.level 7
        && (MapId.level 1).warpTarget 3 1 == MapId.level 31
        && ((MapId.level 1).warpTarget 3 1).name == "MAP31")
    let g0 := GameState.newGame lvl
    -- `doom.wad` carries no super shotgun; the cheat must not hand one over
    let ssg := match Assets.load wad with
      | .ok a => a.hasSuperShotgun
      | .error _ => true
    let (god, _) := g0.applyCheat .god ssg
    r ← check r "gods take no damage"
      ((god.damagePlayer 500).status.health ≥ 100)
    let (armed, _) := g0.applyCheat .kfa ssg
    r ← check r "dillkfa grants every weapon, full ammo, and keys"
      (armed.status.ownsShotgun && armed.status.ownsChaingun
        && armed.status.ownsChainsaw && armed.status.ownsRocket
        && armed.status.ownsPlasma && armed.status.ownsBfg
        && armed.status.bullets == 200 && armed.status.shells == 50
        && armed.status.rockets == 50 && armed.status.cells == 300
        && armed.status.redKey && armed.status.armor == 200)
    -- dillfa is the same arsenal but withholds the keys
    let (fa, _) := g0.applyCheat .fa ssg
    r ← check r "dillfa grants the arsenal but no keys"
      (fa.status.ownsBfg && fa.status.cells == 300 && !fa.status.redKey)
    -- noclip: walk straight through the start room's south wall
    let (ghost, _) := g0.applyCheat .noclip ssg
    let south := { ghost with player := { ghost.player with
        angle := 4.71238898 } }
    let through := run { forward := true } 60 south
    r ← check r "noclip walks through walls"
      (through.player.y < -3700)
    -- Doom 1 has no `SHT2` sprites, no `SGN2` and no `DSDSHTGN`, and nothing
    -- on its maps places one — so the arsenal cheat is the only way to end up
    -- holding a super shotgun there, and it must not. Granting it left `3`
    -- toggling onto an invisible, silent gun that still fired.
    r ← check r "doom.wad has no super shotgun to give" (!ssg)
    r ← check r "…so the arsenal cheat withholds it there"
      (!armed.status.ownsSuperShotgun && !fa.status.ownsSuperShotgun)
    r ← check r "…while granting one where the WAD does carry it"
      ((Cheat.fullArsenal g0.status (hasSuperShotgun := true)).ownsSuperShotgun)
    -- vanilla's arsenal cheat never granted the automap; dillddt is the one
    -- that does
    let (mapped, _) := g0.applyCheat .fullMap ssg
    r ← check r "the arsenal cheat does not reveal the map (as vanilla)"
      (!armed.status.ownsMap)
    r ← check r "dillddt reveals it, and toggles back off"
      (mapped.status.ownsMap && !((mapped.applyCheat .fullMap ssg).1).status.ownsMap)
    r ← check r "dillddt is scanned from the typed buffer"
      (Cheat.scan "xxdillddt" == some .fullMap)

  IO.println "input snapshot decoding (the FFI's packed UInt64):"
  -- The layout must match c/shell.c's dill_poll: held-key bits low, the
  -- quit flag at bit 15, and the mouse delta as a signed 16-bit field in
  -- bits 48–63. These snapshots are hand-packed the way the C side does.
  let b := fun (n : Nat) => (1 : UInt64) <<< UInt64.ofNat n
  let snap : UInt64 := b 0 ||| b 8 ||| ((0xFFFB : UInt64) <<< 48)
  let inp := Input.decode snap  -- forward + fire, mouse dx = -5
  r ← check r "held keys and a negative mouse delta decode"
    (inp.forward && inp.fire && !inp.back && !inp.quit && inp.mouseDx == -5)
  let extremes := Input.decode ((0x7FFF : UInt64) <<< 48 ||| b 15)
  r ← check r "the clamp extremes and the quit flag decode"
    (extremes.quit && extremes.mouseDx == 32767
      && (Input.decode ((0x8000 : UInt64) <<< 48)).mouseDx == -32768
      && (Input.decode 0).mouseDx == 0)
  r ← check r "weapon bits decode in priority order, none when unset"
    ((Input.decode (b 9)).weapon == some 1
      && (Input.decode (b 22)).weapon == some 7
      && (Input.decode (b 20)).weapon == some 5
      && (Input.decode 0).weapon == none
      -- several weapon bits at once: the lowest-numbered weapon wins
      && (Input.decode (b 9 ||| b 22)).weapon == some 1
      && (Input.decode (b 12 ||| b 20)).weapon == some 4)

  IO.println "music (MUS → MIDI conversion):"
  let mut converted := 0
  let mut totalBytes := 0
  for m in [1, 2, 3, 4, 5, 6, 7, 8, 9] do
    if let some lump := wad.find? s!"D_E1M{m}" then
      match Music.musToMidi (wad.data lump) with
      | .ok midi =>
        -- a real SMF: MThd magic, format 0, our 140-tick division
        if midi.size > 1000 && midi.get! 0 == 0x4D && midi.get! 1 == 0x54
            && midi.get! 2 == 0x68 && midi.get! 3 == 0x64
            && midi.get! 13 == 140 then
          converted := converted + 1
          totalBytes := totalBytes + midi.size
      | .error _ => pure ()
  r ← check r "all nine episode-1 tracks convert to valid MIDI"
    (converted == 9)
  r ← check r "the tracks have substance (E1M1 is a real song)"
    (totalBytes > 100000)
  -- The three maps that exercise the sky-wall paths this probe was written
  -- for, at the density it used to run them. `lake exe wadtests` sweeps
  -- every episode instead; keeping that out of here is what keeps
  -- `lake test` quick.
  r ← rendererCompletenessTests r wad
        (only := #["E1M1", "E1M8", "E3M9"]) (viewsPerMap := 28)

  -- Doom II regression: MAP18's start room has exactly two exits — a
  -- blue-key door and a gun-activated lion switch (linedef 39, special 46,
  -- door sector 62). A pistol-start player escapes only by shooting the
  -- switch, so it must answer to bullets, and only within the attack's own
  -- reach. Skipped quietly when doom2.wad is not present.
  if ← System.FilePath.pathExists "doom2.wad" then
    IO.println "doom2 MAP18 (the shoot-to-open lion switch):"
    let bytes2 ← IO.FS.readBinFile "doom2.wad"
    match Wad.parse bytes2 >>= fun w => Level.load w "MAP18" with
    | .error e => IO.eprintln s!"  MAP18 failed to load: {e}"; r ← check r "setup: MAP18 loads" false
    | .ok lvl =>
      let run := fun (input : Input) (n : Nat) (g : GameState) => Id.run do
        let mut g := g
        for _ in [0:n] do g := tick input g
        return g
      let doorCeil := fun (g : GameState) => g.level.sectors[62]!.ceilH
      -- aim from the player start at the middle of the switch face
      let g0 := GameState.start lvl
      let aim := Float.atan2 (-920.0 - g0.player.y) (-1376.0 - g0.player.x)
      let aimed := { g0 with player := { g0.player with angle := aim } }
      let shot := run {} 200 (run { fire := true } 5 aimed)
      r ← check r "shooting the lion switch opens the way out"
        (doorCeil shot > 20)
      -- the fist is also a "hitscan", but its swing reaches only 64 units:
      -- from the start (~100 away) it must not trip the switch...
      let toFist := fun (g : GameState) => run {} 40 (tick { weapon := some 1 } g)
      let farPunch := run { fire := true } 40 (toFist aimed)
      r ← check r "a fist swung across the room does not"
        (doorCeil farPunch == 8)
      -- ...while the same swing from arm's length still does (vanilla runs
      -- the punch's traverse over MELEERANGE, and shoot-lines fire in it)
      let close := { g0 with player := { g0.player with
          x := -1420, y := -920, angle := 0 } }
      let nearPunch := run {} 200 (run { fire := true } 40 (toFist close))
      r ← check r "a point-blank punch still trips it"
        (doorCeil nearPunch > 20)
      -- The type is repeatable, but once the door stands open a re-fire is
      -- a refusal: continuing gunfire across the line must add no mover and
      -- replay no door sound (sounds accumulate in a pure run, so a squeak
      -- per shot would show up as a growing doorOpen count).
      let doorSounds := fun (g : GameState) =>
        (g.sounds.filter (·.1 == Sfx.doorOpen)).size
      let reshot := run { fire := true } 60 shot
      r ← check r "further gunfire does not retrigger the open door"
        (!reshot.movers.any (·.sector == 62)
          && doorSounds reshot == doorSounds shot)

  -- Doom II regression: an overhang's underside is commonly cut at the
  -- neighbouring ledge's own floor height (MAP05's corridor lip over the
  -- imp closets). The sprite z-test exempts the flat a sprite stands on;
  -- that exemption must never fire on a *ceiling*, or a monster on the
  -- ledge shines through the lip — worst case, as here, through a portal
  -- the raised lift has sealed entirely.
  if ← System.FilePath.pathExists "doom2.wad" then
    IO.println "doom2 MAP05 (sprites never shine through an overhang):"
    let bytes2 ← IO.FS.readBinFile "doom2.wad"
    match Wad.parse bytes2 with
    | .error e => IO.eprintln s!"  doom2.wad: {e}"; r ← check r "setup: doom2.wad parses" false
    | .ok wad2 =>
      match Level.load wad2 "MAP05", Assets.load wad2 with
      | .error e, _ => IO.eprintln s!"  MAP05: {e}"; r ← check r "setup: MAP05 loads" false
      | _, .error e => IO.eprintln s!"  assets: {e}"; r ← check r "setup: assets load" false
      | .ok lvl, .ok assets =>
        -- Viewer down in the centre imp closet; a zombieman above on the
        -- corridor floor (z = 160 — exactly the closet lip's ceiling
        -- height), just past the lift. With and without him, the picture
        -- must be identical: every one of his pixels is behind the lip or
        -- the lift face.
        let sec := lvl.sectors[lvl.sectorAt 352 (-160)]!
        let view : Render.View :=
          { x := 352, y := -160
            height := lvl.eyeZ 352 (-160) sec.floorH Player.viewHeight
            angle := 1.5707963267948966 }
        let zombie : Render.RenderMobj :=
          { x := 352, y := -40, z := 160, angle := 4.712
            sprite := "POSS", frame := 'A', bright := false, radius := 20 }
        let root := if lvl.nodes.isEmpty then BspChild.subsector 0
                    else BspChild.node (lvl.nodes.size - 1)
        let paint := fun (mobjs : Array Render.RenderMobj) => Id.run do
          let ctx : Render.Ctx := { level := lvl, assets, view, mobjs }
          let go : Render.RenderM Unit := do
            let masked ← Render.renderChild ctx (lvl.nodes.size + 1) root
            Render.drawPlanes ctx
            for m in masked do Render.drawMasked ctx m
            Render.drawSprites ctx
          let (_, st) := go.run Render.DrawState.init
          return st.frame
        r ← check r "a monster on the ledge above stays behind the lip"
          (paint #[zombie] == paint #[])
        -- The platform must carry its rider: vanilla `P_ThingHeightClip`
        -- keeps grounded things on a moving floor, and the blazing lift
        -- falls 8 a tic — faster than a fall gets going, so a rider whose
        -- glue reached only 4 was left floating in the opening, popping
        -- into view at ceiling height the moment the platform was
        -- triggered.
        let g0 := GameState.newGame lvl
        -- the player waits down in the imp closet, out of the rider's
        -- sight, so the probe stays asleep and planted on the platform
        let mut gr := ({ g0 with
          mobjs := #[]
          player := { g0.player with
            x := 352, y := -220, z := 96
            angle := 1.5707963267948966 } }).rebuildIndexes
        let (gr1, zi) := gr.spawn .zombieman 352 (-44) 0
        gr := gr1.activateLine 2 (byUse := false)   -- tag-1 WR blazing lift
        for _ in [0:8] do gr := tick {} gr
        r ← check r "a monster rides the blazing platform down"
          (Float.abs (gr.mobjs[zi]!.z - gr.level.sectors[120]!.floorH) < 0.001)
        -- A rising platform is not a crusher: a player straddling its edge
        -- under the low lip strip must bounce it back down (vanilla
        -- `T_PlatRaise` reverses on a squeeze), never be wedged under the
        -- lip where nothing fits and every move is refused.
        let mut gs := ({ g0 with mobjs := #[] }).rebuildIndexes
        gs := { gs with player := { gs.player with
          x := 352, y := -70, z := 96, angle := 1.5707963267948966 } }
        gs := gs.activateLine 2 (byUse := false)
        for _ in [0:200] do gs := tick {} gs
        for _ in [0:35] do gs := tick { back := true } gs
        r ← check r "a straddled rising platform bounces; the player walks free"
          (gs.player.y < -80)

  -- Second-pass regressions: E1M6's close-30 doors (special 76), the
  -- gun-only line family (24/46/47 answer to bullets, never to feet), and
  -- the Use-list invariant swept against the IWADs themselves.
  IO.println "gun-only lines, close-30 doors, the Use-list invariant:"
  match Level.load wad "E1M6" with
  | .error e => IO.eprintln s!"  E1M6: {e}"; r ← check r "setup: E1M6 loads" false
  | .ok l6 =>
    -- E1M6's corridor doors are special 76 — vanilla's WR close-wait-30 s-
    -- reopen (`close30ThenOpen`), the repeatable twin of 16. A bad mapping
    -- once made them perpetual crushers.
    let g0 := GameState.newGame l6
    -- an empty world, so nothing under a door can reverse it while it shuts
    let g0 := emptied g0
    let g := g0.activateLine 602 (byUse := false)
    let isClose30 := fun (m : Mover) => match m with
      | .closeOpen .. => true | _ => false
    r ← check r "E1M6's special-76 lines start close-30 doors, not crushers"
      (g.movers.size == 2 && g.movers.all isClose30)
    let tag2 := l6.sectorsTagged 2
    let (shutMid, settled) := Id.run do
      let mut gs := g
      for _ in [0:100] do gs := gs.stepMovers
      let shut := tag2.all fun s =>
        gs.level.sectors[s]!.ceilH == gs.level.sectors[s]!.floorH
      for _ in [0:30 * 35 + 80] do gs := gs.stepMovers
      return (shut, gs)
    r ← check r "…they shut, sit their 30 s, reopen fully, and retire"
      (shutMid && settled.movers.isEmpty
        && tag2.all fun s =>
             settled.level.sectors[s]!.ceilH == l6.sectors[s]!.ceilH)
  match Level.load wad "E1M8" with
  | .error e => IO.eprintln s!"  E1M8: {e}"; r ← check r "setup: E1M8 loads" false
  | .ok l8 =>
    -- `stepMovers` rebuilds its mover list from the ones it stepped, but a
    -- crush runs `damageMobj` partway through — and a boss dying there
    -- starts a mover of its own (E1M8's barons drop the tag-666 floors) by
    -- pushing onto `movers` behind the loop's back. Those have to survive
    -- the rebuild, or the level's one exit trigger fires and is discarded on
    -- the same tic, sealing the map.
    let tagged666 := l8.sectorsTagged 666
    r ← check r "E1M8 carries the tag-666 floors a baron's death lowers"
      !tagged666.isEmpty
    let g0 := emptied (GameState.newGame l8)
    let px := g0.player.x
    let py := g0.player.y
    let s := l8.sectorAt px py
    let floorH := g0.level.sectors[s]!.floorH
    -- squash the room to a gap no baron fits in, and stand one in it as the
    -- last of its kind (the world is empty, so `othersLeft` is false)
    let g0 := { g0 with level := g0.level.setCeil s (floorH + 8) }
    let (g0, bi) := g0.spawn .baron px py 0
    let g0 := g0.setMobj bi { g0.mobjs[bi]! with z := floorH, health := 1 }
    let g0 := { g0 with
      movers := #[.crusher s (floorH + 8) (floorH + 8) true] }
    let crushed := g0.stepMovers
    r ← check r "a boss crushed to death still drops the tag-666 floors"
      (crushed.mobjs[bi]!.health ≤ 0
        && crushed.movers.any fun m => match m with
             | .floorDown si _ _ _ => tagged666.contains si
             | _ => false)
    -- and the crusher that killed it is still running alongside
    r ← check r "…without losing the crusher that did it"
      (crushed.movers.any fun m => match m with
         | .crusher .. => true | _ => false)
  match Level.load wad "E2M4" with
  | .error e => IO.eprintln s!"  E2M4: {e}"; r ← check r "setup: E2M4 loads" false
  | .ok l4 =>
    -- Line 282 is the IWAD's only special-24 line (G1: shoot to raise the
    -- floor). It is one-sided, so if walking were its trigger it could
    -- never fire at all.
    let line := l4.linedefs[282]!
    let p1 := l4.vertexes[line.v1]!
    let p2 := l4.vertexes[line.v2]!
    let cx := (p1.x + p2.x) / 2
    let cy := (p1.y + p2.y) / 2
    let len := Float.sqrt ((p2.x - p1.x)^2 + (p2.y - p1.y)^2)
    -- stand 64 out on the line's front side, aiming at its midpoint
    let px := cx + (p2.y - p1.y) / len * 64
    let py := cy - (p2.x - p1.x) / len * 64
    let g0 := GameState.newGame l4
    let g0 := { g0 with player := { g0.player with
      x := px, y := py, z := l4.sectors[l4.sectorAt px py]!.floorH
      angle := Float.atan2 (cy - py) (cx - px) } }
    let shot := g0.shootSpecialLines g0.player.x g0.player.y g0.player.angle 2048
    r ← check r "shooting E2M4's special-24 wall raises the tag-9 floor"
      (shot.movers.size > 0 && shot.level.linedefs[282]!.special == 0
        && (l4.sectorsTagged 9).all fun s => shot.movers.any (·.sector == s))
    r ← check r "…and 24 answers to nothing but the gun"
      (!GameState.answersToUse 24
        && (g0.activateLineOpt 282 (byUse := false)).isNone)
  if ← System.FilePath.pathExists "doom2.wad" then
    let bytes2 ← IO.FS.readBinFile "doom2.wad"
    match Wad.parse bytes2 with
    | .error e => IO.eprintln s!"  doom2.wad: {e}"; r ← check r "setup: doom2.wad parses" false
    | .ok wad2 =>
      -- MAP19's two free-standing special-47 lines are crossable: walking
      -- over one must do nothing (vanilla `P_CrossSpecialLine` has no case
      -- 47), while a shot across it fires and burns the one-shot.
      match Level.load wad2 "MAP19" with
      | .error e => IO.eprintln s!"  MAP19: {e}"; r ← check r "setup: MAP19 loads" false
      | .ok l19 =>
        let line := l19.linedefs[578]!
        let p1 := l19.vertexes[line.v1]!
        let p2 := l19.vertexes[line.v2]!
        let cx := (p1.x + p2.x) / 2
        let cy := (p1.y + p2.y) / 2
        let len := Float.sqrt ((p2.x - p1.x)^2 + (p2.y - p1.y)^2)
        let nx := (p2.y - p1.y) / len
        let ny := -(p2.x - p1.x) / len
        let g0 := GameState.newGame l19
        let before := { g0 with player := { g0.player with
          x := cx - nx * 8, y := cy - ny * 8
          z := l19.sectors[l19.sectorAt (cx - nx * 8) (cy - ny * 8)]!.floorH } }
        let walked := before.crossSpecials (cx + nx * 8) (cy + ny * 8)
        r ← check r "walking across MAP19's special-47 line does nothing"
          (walked.movers.isEmpty && walked.level.linedefs[578]!.special == 47)
        let shooter := { g0 with player := { g0.player with
          x := cx + nx * 32, y := cy + ny * 32
          z := l19.sectors[l19.sectorAt (cx + nx * 32) (cy + ny * 32)]!.floorH
          angle := Float.atan2 (cy - (cy + ny * 32)) (cx - (cx + nx * 32)) } }
        let shot := shooter.shootSpecialLines shooter.player.x shooter.player.y
          shooter.player.angle 2048
        r ← check r "…while shooting across it raises the floor and burns it"
          (shot.movers.size > 0 && shot.level.linedefs[578]!.special == 0)
      -- MAP29's bridge switches are special 131 (S1 raise floor turbo): a
      -- Use press right at the switch face must schedule the mover. These
      -- were dead when 131 was missing from `answersToUse` — pass 1 never
      -- even selected the line, a silent no-op.
      match Level.load wad2 "MAP29" with
      | .error e => IO.eprintln s!"  MAP29: {e}"; r ← check r "setup: MAP29 loads" false
      | .ok l29 =>
        let pressWorks := fun (li : Nat) => Id.run do
          let line := l29.linedefs[li]!
          let p1 := l29.vertexes[line.v1]!
          let p2 := l29.vertexes[line.v2]!
          let cx := (p1.x + p2.x) / 2
          let cy := (p1.y + p2.y) / 2
          let len := Float.sqrt ((p2.x - p1.x)^2 + (p2.y - p1.y)^2)
          let px := cx + (p2.y - p1.y) / len * 32
          let py := cy - (p2.x - p1.x) / len * 32
          let g0 := GameState.newGame l29
          let g := { g0 with player := { g0.player with
            x := px, y := py, z := l29.sectors[l29.sectorAt px py]!.floorH
            angle := Float.atan2 (cy - py) (cx - px) } }
          let g' := g.useLines
          return (l29.sectorsTagged line.tag).all fun s =>
            g'.movers.any (·.sector == s)
        r ← check r "MAP29's special-131 bridge switches answer to Use"
          (pressWorks 583 && pressWorks 655)
      -- The `answersToUse` invariant, checked against the game's own
      -- dispatch instead of a hand-kept list: sweep every special line of
      -- both IWADs, press it (holding all keys, so locks don't mask a
      -- listing gap), and any line that *fires* from Use must be listed —
      -- except the gun trio, which only `shootSpecialLines` may reach.
      let gunOnly : Array Nat := #[24, 46, 47]
      let (doomMaps, doomFails) := loadMaps wad doomMapNames
      let (d2Maps, d2Fails) := loadMaps wad2 doom2MapNames
      unless doomFails.isEmpty && d2Fails.isEmpty do
        IO.println s!"  (maps that failed to load: \
          {(doomFails ++ d2Fails).toList})"
      r ← check r "both IWADs' maps decode for the answersToUse sweep"
        (doomFails.isEmpty && d2Fails.isEmpty)
      -- a sweep over nothing would pass vacuously, which is how the silent
      -- `.error` skip used to hide a broken loader
      r ← check r "the answersToUse sweep saw both IWADs' maps"
        (doomMaps.size ≥ 9 && d2Maps.size ≥ 32)
      let sweep := fun (maps : Array (String × Level)) => Id.run do
        let mut bad : Array (String × Nat) := #[]
        for (name, lvl) in maps do
            let g0 := GameState.newGame lvl
            let g0 := { g0 with status := { g0.status with
              blueKey := true, yellowKey := true, redKey := true } }
            for i in [0:lvl.linedefs.size] do
              let sp := lvl.linedefs[i]!.special
              if sp == 0 || GameState.answersToUse sp
                  || gunOnly.contains sp then continue
              if (g0.activateLineOpt i (byUse := true)).isSome
                  && !bad.any (·.2 == sp) then
                bad := bad.push (name, sp)
        return bad
      let unlisted := sweep doomMaps ++ sweep d2Maps
      unless unlisted.isEmpty do
        IO.println s!"  (byUse specials missing from answersToUse: \
          {unlisted.toList})"
      r ← check r "every special that fires from Use is in answersToUse"
        unlisted.isEmpty

  -- Lost soul and arachnotron vanilla-fidelity regressions (p_enemy.c /
  -- info.c). Doom II monsters simulate fine on E1M1 — the tables are
  -- WAD-independent and these tests never render.
  IO.println "lost soul dives, the pain elemental's cap, the arachnotron:"
  match Level.load wad "E1M1" with
  | .error e => IO.eprintln s!"  E1M1: {e}"; r ← check r "setup: E1M1 loads" false
  | .ok lvl =>
    let floorAt := fun (x y : Float) => lvl.sectors[lvl.sectorAt x y]!.floorH
    -- an empty arena, the player parked out of the flight path
    let base := emptied (GameState.newGame lvl)
    let base := { base with player := { base.player with
      x := -500, y := -3430, z := floorAt (-500) (-3430) } }
    -- A dive across open ground: the slam is (P_Random()%8+1)×3 — one d8
    -- tripled — and it ends the flight: momentum zeroed, back to the spawn
    -- state (vanilla PIT_CheckThing), one slam only.
    let (g, si) := base.spawn .lostSoul (-450) (-3430) 0
    let (g, di) := g.spawn .demon (-250) (-3430) 0
    let soulUid := g.mobjs[si]!.uid
    let demonUid := g.mobjs[di]!.uid
    let g := { g with mobjs := g.mobjs.modify si fun m =>
      { m with z := floorAt (-450) (-3430) + 20, awake := true
               target := demonUid } }
    let g := g.aSkullAttack si
    let launched := g.mobjs[si]!
    let flown := Id.run do
      let mut g := g
      for _ in [0:30] do
        if (g.mobjIdx? soulUid).any (g.mobjs[·]!.charging) then
          g := g.moveSkull ((g.mobjIdx? soulUid).get!)
      return g
    let soul := flown.mobjs[(flown.mobjIdx? soulUid).get!]!
    let dmg := (150 : Int) - (flown.mobjs[(flown.mobjIdx? demonUid).get!]!).health
    r ← check r "a lost soul dives at SKULLSPEED 20 and slams for 1d8×3"
      (Float.abs (Float.sqrt (launched.momX^2 + launched.momY^2) - 20) < 0.001
        && dmg % 3 == 0 && 3 ≤ dmg && dmg ≤ 24)
    r ← check r "…and the slam drops it back to its spawn state, dive over"
      (!soul.charging && soul.momX == 0.0 && soul.momY == 0.0
        && soul.state == soul.info.spawnState)
    -- vanilla clamps the flight time to ≥ 1 tic (`if (dist < 1) dist = 1`
    -- *after* dividing by SKULLSPEED): a point-blank dive climbs the whole
    -- height gap in one tic, never a multiple of it
    let (g2, sj) := base.spawn .lostSoul (-450) (-3430) 0
    let (g2, dj) := g2.spawn .demon (-440) (-3430) 0
    let tuid := g2.mobjs[dj]!.uid
    let g2 := { g2 with mobjs :=
      ((g2.mobjs.modify dj fun m => { m with z := m.z + 40 }).modify sj
        fun m => { m with target := tuid }) }
    let g2 := g2.aSkullAttack sj
    let gap := (g2.mobjs[dj]!.z + g2.mobjs[dj]!.info.height / 2)
      - g2.mobjs[sj]!.z
    r ← check r "a point-blank dive climbs the gap in one tic, not faster"
      (Float.abs (g2.mobjs[sj]!.momZ - gap) < 0.001)
    -- The pain elemental's population cap counts every soul in the level —
    -- corpses mid-burst included (vanilla counts all MT_SKULL thinkers):
    -- 20 live plus one corpse is 21, and the spit is refused.
    let (gp, pi) := base.spawn .painElemental (-500) (-3550) 0
    let capped := Id.run do
      let mut g := gp
      for k in [0:20] do
        g := (g.spawn .lostSoul (-1200 + 60 * Float.ofNat k) (-3300) 0).1
      let (g', ci) := g.spawn .lostSoul (-1200) (-3360) 0
      return { g' with mobjs := g'.mobjs.modify ci fun m =>
        { m with corpse := true, health := 0 } }
    let soulCount := fun (g : GameState) =>
      (g.mobjs.filter fun s => s.kind == .lostSoul && !s.removed).size
    let refused := capped.aPainAttack ((capped.mobjIdx? (gp.mobjs[pi]!.uid)).get!)
    r ← check r "the pain elemental refuses its 22nd soul, corpses counted"
      (soulCount refused == soulCount capped)
    -- an unhindered spit: the newborn steps 4+3·(31+16)/2 out, inherits the
    -- elemental's target, and is already mid-charge (vanilla A_PainShootSkull
    -- ends in A_SkullAttack)
    let spat := gp.aPainAttack pi
    r ← check r "a spat soul charges the elemental's target at once"
      (spat.mobjs.any fun s => s.kind == .lostSoul && s.charging
        && s.target == 0
        && Float.abs (s.distanceTo (-500) (-3550) - 74.5) < 0.001)
    -- a soul born inside a wall bursts on the spot (vanilla spawns it and
    -- deals it 10000) — never a soul clipping through geometry. The exit
    -- room's west wall is at x = 2912; from 70.5 inside, the 74.5 prestep
    -- lands the newborn straddling it.
    let (gw, pw) := base.spawn .painElemental 2982 (-4768) 3.14159265
    let gw := { gw with mobjs := gw.mobjs.modify pw fun m =>
      { m with z := floorAt 2982 (-4768) } }
    let walled := gw.aPainAttack pw
    r ← check r "a soul spat into a wall bursts instead of clipping"
      ((walled.mobjs.filter fun s =>
          s.kind == .lostSoul && !s.removed && s.health > 0).size == 0
        && (walled.mobjs.filter fun s =>
          s.kind == .lostSoul && !s.removed && s.health ≤ 0).size == 1)
    -- Arachnotron per info.c: a 20-tic stock-still S_BSPI_SIGHT on waking,
    -- pain on frame I, death J–P (S_BSPI_DIE1's scream runs on entry, so
    -- the J frame needs no split), and MT_ARACHPLAZ at 25/tic.
    let bspi := ActorInfo.ofKind .arachnotron
    let sight := bspi.states[bspi.seeState.get!]!
    let deathFrames := (bspi.states.toList.drop bspi.deathState.get!).map (·.frame)
    r ← check r "the arachnotron wakes to a 20-tic pause, pains on I, dies J–P"
      (sight.tics == 20 && sight.action.isNone
        && bspi.states[bspi.painState.get!]!.frame == 'I'
        && deathFrames == ['J', 'K', 'L', 'M', 'N', 'O', 'P'])
    let (ga, ai) := base.spawn .arachnotron (-800) (-3430) 0
    let ga := ga.aBspiAttack ai
    let plasmaSpeed := (ga.mobjs.find? (·.kind == .arachPlasma)).map fun b =>
      Float.sqrt (b.momX^2 + b.momY^2)
    let dead := Id.run do
      let mut g := ga.damageMobj ai 1000
      for _ in [0:4] do g := tick {} g
      return g
    r ← check r "its plasma flies at 25 and its death scream plays"
      (plasmaSpeed.any (fun s => Float.abs (s - 25) < 0.001)
        && dead.sounds.any (·.1 == Sfx.bspDeath))


  r ← fixWaveTests r wad
  r ← fixWaveCombatTests r wad
  r ← fixWaveTests2 r wad
  r ← reviewFixTests r wad
  r ← reviewFixTests2 r wad
  r ← reviewFixTests3 r wad
  r ← reviewFixTests4 r wad

  r ← uiTests r wad

  if r.failures == 0 then
    IO.println "all tests passed"
    return 0
  else
    IO.println s!"{r.failures} test(s) failed"
    return 1

