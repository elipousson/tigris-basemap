# TODO

Outstanding questions and tasks for the tigris-basemap pipeline.

## Before the next full pipeline run

- [ ] Run `targets::tar_make()`. Most targets will rebuild because their commands changed, and the El Paso and Washington, DC geographies will be built for the first time.
- [ ] Run `targets::tar_prune()` to remove targets no longer in the pipeline: `msa_counties_init_*`, `msa_counties_full_*`, `msa_filter_geom_*`, `msa_urban_area_*`, and targets named with the old `24510_*` convention.
- [ ] Re-knit `README.md` from `README.Rmd` once the basemaps are built.
- [ ] Decide whether to keep, delete, or `.gitignore` the `comparison/` folder of before and after images.
- [ ] Review `output/`, which still has exports from the old naming convention (e.g. `24510_county_basemap.rds`).

## Basemap design

- [ ] Make the background buffer fit the MSA. `county_buffers[5, ]` is a fixed 50-mile circle around the focal county (see the `FIXME` in `make_county_buffers()`). It doesn't cover the El Paso or Washington, DC MSAs, and because the buffer is part of the MSA basemap bounds it makes the DC map lopsided. Options: scale the distance with the MSA extent (e.g. a bounding box diagonal, as in the `sfext::sf_bbox_diagdist()` TODO), or set the buffer distance per basemap or geography in YAML.
- [ ] Make the water buffer in the `county-lines` layer (`st_buffer` with `dist: 2000`) conditional on the size of the area (see the TODO in `basemaps.yml`).
- [ ] Choose a water source that includes rivers for the county basemap. The NHD waterbody layer (`MapServer/12`) only has lakes and ponds, so the DC county basemap is missing the Potomac and Anacostia. Consider the NHD area layer (`MapServer/10`) or `tigris_area_water` (currently in `staging`), possibly combined.
- [ ] Revisit the PAD park thresholds. `GIS_Acres >= 25` (county) and `>= 40` (MSA) are now true acres. The earlier local filter measured Web Mercator acres (about 1.67 times larger at Baltimore's latitude), so roughly 15 and 24 acres would match the earlier look.
- [ ] Consider filtering PAD by `Category` or `Des_Tp` in the `where` clause. Military and other large federal land (e.g. Fort Bliss in El Paso) dominate the parks layer outside the East Coast.
- [ ] Decide how to handle areas on international borders (e.g. Ciudad Juárez across from El Paso), where all sources are US-only.
- [ ] Decide whether the county basemap should show the `divisions` layer (currently `visibility: none`) and whether it should use a neatline (currently `neatline: false` to match the earlier design).
- [ ] Decide whether the rail layer in the MSA basemap should be shown (currently `visibility: none`) and whether `msa_rail_lines` needs a clip step.
- [ ] Review the `staging` data (`area_water`, `water_lines`, `metro_areas`, `combined_area`) and either add layers that use it or remove it.

## Pipeline structure

- [ ] Decide whether the county basemap's `roads` should come from the shared `state_roads_<state>` target (clipped to the county) instead of a separate `tigris::primary_secondary_roads()` request filtered by the county.
- [ ] Data `args` for MSA `roads` in `basemaps.yml` are now ignored because roads are loaded once per state in `state_roads`. Either document this, reject `args` for that entry, or support arguments for state-level data.
- [ ] `msa_counties_export` depends on the style layer with `id: counties` in `msa_layer_data`. Renaming that layer or setting `visibility: none` produces an empty export. Consider exporting from a dedicated target or clipping in the export target.
- [ ] The data names passed to `process_layers()` in `_targets.R` must match the `data` keys in `basemaps.yml`, and changing a data entry's `source` to a source of a different `type` requires changing the loader in `_targets.R`. Consider generating layer data targets from `basemaps.yml` (deferred when the style spec was designed).
- [ ] Consider treating `st_transform` as an explicit processing step (deferred; it currently runs in the loaders).
- [ ] Consider supporting aliases to a list of steps by flattening nested `processing_steps` (skipped for now; only single-step anchors are supported).
- [ ] Exports use `export_rds()` as a side effect inside regular targets. Consider returning file paths and using `tar_file()` so output files are tracked.
- [ ] `filter_min_area` is no longer used for PAD (replaced by a server-side `where` clause). Decide whether to keep it for other sources or remove it.

## Data and validation

- [ ] The pattern used to parse states from ACS MSA names (`", ST-ST Metro Area"`) was verified for all 935 areas in the 2023 ACS only. Check it against other ACS years before using a different `tigris_year`. `msa_states` can be provided if a name can't be parsed.
- [ ] Metro area delineations change over time (e.g. the 2023 revision). If county files (`CBSAFP`) and ACS data for a year use different delineations, `select_msa_counties()` fails. Consider an optional override with a list of county GEOIDs in `geographies.yml` if this happens.
- [ ] Geometry validity can change as providers update their data. `st_make_valid` is currently only applied to `usgs_pad` and the Baltimore CSA divisions. Recheck sources with `sf::st_is_valid()` periodically.
- [ ] The `us_msa` target uses ACS 5-year estimates for `tigris_year`. Confirm the ACS release is available before increasing `tigris_year`.

## Housekeeping

- [ ] Commit the changes from this session (the last commit is `ef228a7`, before the style spec, processing steps, and multi-state support).
- [ ] The R library used for testing didn't have the `qs` package, so test builds used `rds` storage in a scratch copy. Confirm `format = "qs"` works in your environment, or consider switching to `qs2`.
- [ ] `R/basemap-script.R` and `msa-transportation-101.R` are untracked scripts that aren't part of the pipeline. Decide whether to keep, move, or remove them.
