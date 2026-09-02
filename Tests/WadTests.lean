import Dill
import TestSupport

/-!
# The comprehensive WAD sweep

Everything that scales with *how many WAD files are sitting in the project
root*, kept out of `Tests.lean` on purpose. `lake test` stays a fast check
of the engine's behaviour against `doom.wad`; this binary is the broad one:

    lake exe wadtests

It loads every IWAD and PWAD it can find, decodes every map each carries,
and probes the renderer across every episode rather than the three maps the
fast suite can afford. Separating them buys two things — `lake test` is not
slowed down by files that may not even be present, and editing this sweep
does not force a recompile of the (very large) main suite.
-/

open Dill

/-- Every WAD in the project root decodes, end to end.

This is the assertion the suite was missing. It used to open exactly two
files — `doom.wad` and, if present, `doom2.wad` — while `plutonia.wad`,
`tnt.wad`, `nerve.wad`, `masterlevels.wad`, `sigil.wad` and `sigil2.wad`
sat beside them untouched; and because every map sweep treated a failed
`Level.load` as "skip", a map that stopped loading made those sweeps pass
in silence. So: for each WAD present, its assets decode, every map marker
it carries decodes, every map has somewhere to stand, and every texture and
flat its geometry names actually resolves. A configuration whose files are
absent is skipped, as the Doom II groups already are.

The texture/flat check is the one that guards the PWAD path specifically:
the merge has to rebase lump offsets, prefer the patch's copy of a shadowed
name, and read the `FF_`/`SS_` section markers PWADs use, and a slip in any
of those shows up here as a name the renderer could not resolve. -/
def wadCoverageTests (r0 : TestRun) : IO TestRun := do
  IO.println "every WAD in the project root loads:"
  let mut r := r0
  -- (label, IWAD, optional PWAD layered over it with `-file`)
  let configs : List (String × String × Option String) :=
    [ ("doom.wad", "doom.wad", none)
    , ("doom2.wad", "doom2.wad", none)
    , ("plutonia.wad", "plutonia.wad", none)
    , ("tnt.wad", "tnt.wad", none)
    , ("doom2 + nerve.wad", "doom2.wad", some "nerve.wad")
    , ("doom2 + masterlevels.wad", "doom2.wad", some "masterlevels.wad")
    , ("doom + sigil.wad", "doom.wad", some "sigil.wad")
    , ("doom + sigil2.wad", "doom.wad", some "sigil2.wad") ]
  let mut configsRun := 0
  for (label, iwadPath, pwadPath) in configs do
    let havePwad ← match pwadPath with
      | none => pure true
      | some p => System.FilePath.pathExists p
    unless (← System.FilePath.pathExists iwadPath) && havePwad do
      continue
    configsRun := configsRun + 1
    let iwadBytes ← IO.FS.readBinFile iwadPath
    let some base := (Wad.parse iwadBytes).toOption
      | do r ← check r s!"{label}: the IWAD parses" false; continue
    let mut wad := base
    match pwadPath with
    | none => pure ()
    | some p =>
      let some patch := (Wad.parse (← IO.FS.readBinFile p)).toOption
        | do r ← check r s!"{label}: the PWAD parses" false; continue
      wad := base.merge patch
    -- assets first: a map is only as loadable as the palette behind it
    let assets? := Assets.load wad
    r ← check r s!"{label}: assets decode" assets?.toOption.isSome
    let markers := mapMarkers wad
    let (maps, failed) := loadMaps wad markers.toList
    unless failed.isEmpty do
      IO.println s!"    (load failures: {failed.toList})"
    r ← check r s!"{label}: all {markers.size} maps decode" failed.isEmpty
    let noStart := maps.filterMap fun (n, lvl) =>
      if lvl.playerStart.isNone then some n else none
    unless noStart.isEmpty do
      IO.println s!"    (no player 1 start: {noStart.toList})"
    r ← check r s!"{label}: every map has a player start" noStart.isEmpty
    -- every name the geometry references must resolve in the assets
    match assets? with
    | .error _ => pure ()
    | .ok assets =>
      let missing := Id.run do
        let mut tex : Array String := #[]
        let mut flat : Array String := #[]
        for (_, lvl) in maps do
          for sd in lvl.sidedefs do
            for t in [sd.upper, sd.lower, sd.middle] do
              if t != "-" && t != "" && !assets.textures.contains t
                  && !tex.contains t then tex := tex.push t
          for s in lvl.sectors do
            for f in [s.floorFlat, s.ceilFlat] do
              if f != "" && f != "F_SKY1" && !assets.flats.contains f
                  && !flat.contains f then flat := flat.push f
        return (tex, flat)
      let (missTex, missFlat) := missing
      unless missTex.isEmpty && missFlat.isEmpty do
        IO.println s!"    (missing textures: {missTex.toList.take 8}, \
          missing flats: {missFlat.toList.take 8})"
      r ← check r s!"{label}: every texture and flat resolves"
        (missTex.isEmpty && missFlat.isEmpty)
  -- doom.wad is a hard requirement of this suite, so at least it must have run
  r ← check r "at least one WAD configuration was checked" (configsRun > 0)
  return r
def main : IO UInt32 := do
  let mut r : TestRun := {}
  r ← wadCoverageTests r
  -- the completeness probe over every episode the WAD carries, not the
  -- three the fast suite spot-checks
  if ← System.FilePath.pathExists "doom.wad" then
    let bytes ← IO.FS.readBinFile "doom.wad"
    match Wad.parse bytes with
    | .ok wad => r ← rendererCompletenessTests r wad
    | .error e => IO.eprintln s!"doom.wad: {e}"; return 1
  if r.failures == 0 then
    IO.println "all WAD tests passed"
    return 0
  else
    IO.println s!"{r.failures} WAD test(s) failed"
    return 1

