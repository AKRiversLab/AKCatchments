# Handoff: AKCatchments after the robustness and catalog round

Written 2026-11-02, end of the second build chat; sections 4b-4c added 2026-11-12, 4d (API rebuild and parallel processing) 2026-11-13, 4h (lidar, keep_rasters, paper repo) 2026-11-16. **Read 4d first: the API in sections 2 and 4 is the old one.** For the next chat. The user
is Caleb Robbins (robbinscalebj@gmail.com), working on CAKN (Central Alaska
Network) stream chemistry. Replaces the first-chat handoff.

## 0. Before you start

- You can't see the previous sandbox. The user attaches **AKCatchments.zip**
  and **CAKN-Data.zip** (or the GitHub repos). This file is
  `dev/HANDOFF.md` inside AKCatchments.
- **Ask for `ak_live_test_report.md`** from the user's run of
  `inst/live-tests/ak_live_tests.R` (quick first, then full). It still hasn't
  been run: nothing that calls a web service has met the real service. It is
  the first bug list.
- The sandbox can't reach the data services (proxy allowlist; everything
  but package managers and GitHub is refused). Only the user's machine runs
  live tests.
- Sandbox setup that worked: R 4.3 and the spatial stack from Ubuntu apt
  (`r-base-core r-cran-sf r-cran-terra r-cran-exactextractr r-cran-httr2
  r-cran-testthat r-cran-devtools ...`); the `whitebox` R package from
  GitHub (`git clone opengeos/whiteboxR`, `R CMD INSTALL`); the
  WhiteboxTools binary: conda-forge and GitHub releases were unreachable on
  2026-11-13; building from source worked (`git clone jblindsay/whitebox-tools`,
  delete the `tsp-rs` checksum line in Cargo.lock, `cargo build --release -p
  whitebox_tools`, ~4 min; set `R_WHITEBOX_EXE_PATH` to
  `target/release/whitebox_tools`). purrr 1.1.0 (for `in_parallel()`),
  mirai, nanonext and carrier: `git clone` from GitHub and `R CMD INSTALL`
  (purrr: `git checkout v1.1.0`; its dev version needs a newer rlang); `maptiles` 0.9.0 and `slippymath` from GitHub
  (newer maptiles needs terra >= 1.8). roxygen2 there is 7.3.1: after
  `document()`, delete the `RoxygenNote` line it adds and restore
  `man/AKCatchments-package.Rd` (the user's roxygen is 8.x). Run R CMD check
  with `LANG=C.UTF-8` and `_R_CHECK_FORCE_SUGGESTS_=false`.
- Related project docs: `claude/handoff_akcatchments_cakn_data.md`,
  `claude/error_audit.md`, `claude/data_paper_layer_plan.md`.

## 1. Goal (unchanged)

Coordinates on a stream in, out: (1) a catchment polygon with diagnostics,
flags, an imagery check plot and validation where a reference exists; (2)
any usable environmental layer for it, cached, with a default source,
alternates and uncertainty columns (`_se`, `_lo95`, `_hi95`, `_flag`); (3)
nearby observations in one schema that the user's own data also fit.
**Robustness before breadth.** Every function works on Windows with the
cache on Box Drive, fails with an informative message, and has a live test.

## 2. State (0.1.0.9000)

R CMD check clean (only the sandbox's missing Suggests), vignettes built.
55 offline test blocks. Added in the first round:

| File | What |
|---|---|
| catalog.R | `ak_layers()` (catalog from `inst/extdata/sources.csv`, which now has `id`, `variant`, `default`, `kind`, `extent`, `access`, `redistributable`, `cache_folder`, `approx_download`), `ak_catchments()` (entry point; per-row `ak_delineate()` args from columns; `workers` = PSOCK cluster, dev mode via pkgload), `ak_add()`, `ak_add_timeseries()`, `ak_timeseries()`, `ak_provenance()`, `ak_prefetch()`, `ak_write()` (GeoPackage: catchments, provenance, errors, ts_*). Backends: `column_backends` / `timeseries_backends` lists keyed `"layer/variant"` |
| reliability.R (second round) | `ak_reliability()`, `ak_flag_check()` |
| observations.R | `ak_obs_schema()`, `ak_obs_validate()`, `ak_obs_read()` (transmute-style mapping, `.pivot` for wide tables), `ak_obs_stations()`, `ak_obs_link()` (at_outlet / inside with `station_area_frac` from the D8 accumulation / near), `ak_obs_temperature_stations()`, `ak_obs_temperature()`, `ak_obs_usgs_stations()`, `ak_obs_usgs_daily()` (dataRetrieval `read_waterdata_monitoring_location`, `read_waterdata_ts_meta`, `read_waterdata_daily`; signatures checked against the dataRetrieval reference pages, not run) |

`ak_catchments()` output: one sf row per site with the `ak_delineate()`
columns plus `snapped_lon`, `snapped_lat`, `plot_file`, `dem_file`,
`filled_file`, `pointer_file`, `accum_file`. Attributes `errors`,
`ak_provenance`, optionally `delineations`, `ak_timeseries`. Class
`ak_catchments` is kept first by `ak_add()`; sf/dplyr verbs keep the
attributes but may put `sf` first in the class (then it prints as plain sf).

### Fixed this round (with unit tests)

1. Fire cache keyed by area: `fire_history_ak.gpkg` has layers `perimeters`
   and `coverage`; uncovered catchments trigger one query over the union
   bbox. A geo-workflow file (no coverage layer) counts for its features'
   bbox. Live test Z04 reports what the user's old file covers.
2. `ak_condition_catchments()`: `_<res>m` and `_buf<buffer>` in file names
   when not default (default names unchanged, so geo-workflow scratch files
   are still reused); derived rasters rebuilt when the DEM is.
3. `ak_delineate()` keeps DEM, breached DEM, pointer and accumulation in
   `delineation/<site>/<demtag>_<xmin>_<ymin>_<w>x<h>/`, cropped to the
   catchment + margin, DEFLATE-compressed.
4. Truncation = touches the window's outer frame only. Contact with NA cells
   away from the outlet → `nodata_edge` (failure flag), no widening. No DEM
   value at the point → clear error.
5. Copernicus tiles with different longitude spacing (50/60/70/80 N) are
   projected one by one, then merged (`tiles_to_grid()`).
6. `ak_download()` refuses HTML pages saved as data; HTTP and connection
   errors name the URL (`req_error(body=)`, `ak_perform()`).
7. `extract_archive()` unpacks via a temp folder; `vsizip()` and the cache
   root use forward slashes.
8. `wkt_filter` reads for RGI and SIM 3340 polygons; vectorised
   `ak_positional_uncertainty()` (identical output, ~25x faster);
   `ak_outlet_slope()` crops to the outlet neighbourhood.
9. Invalid input polygons repaired; missing CRS explained; imagery zoom
   computed when NULL (old maptiles needs it); SDA, imagery and hillshade
   failures report their reason; text sample dates accepted.

### CAKN-Data

Unchanged outputs (no step's inputs or defaults changed). New
`scripts/10_observations.R` writes `data/observations_chemistry.csv`: the
lab table in `ak_obs_schema()` with QC folded into `qualifier` and the 02
columns kept. Checked on mock data only. `run_all.R`, README and
`data/README.md` updated (section numbers there fixed too). Step 07 still
uses the per-layer functions, which is fine; porting it to
`ak_catchments()`/`ak_add()` would change column names (`outlet_snap_dist_m`
vs `snap_dist_m`), so don't, unless the N paper's `join_data.R` moves too.

### Second round (2026-11-03): uncertainty review and delineation checks

Asked for: re-check every uncertainty calculation; a vignette on how
uncertainty is computed and carried into observations, with alternatives
and downstream consequences; a vignette on the delineation process, DEM
choice, error handling, pitfalls and how reliably they are reported.

- Vignettes: `vignettes/uncertainty.Rmd` (replaces
  attributes-uncertainty.Rmd) and `vignettes/delineation-method.Rmd`. Their
  numeric chunks run at build time (the WhiteboxTools demo only where WBT
  is installed). Published figures quoted there were checked against
  sources this round: IfSAR 3 m LE90 / 12.2 m CE90 (USGS EROS); Copernicus
  <4 m LE90 absolute, <2 m relative on slopes <=20%, <6 m CE90; NAD27 shifts
  over 200 m in parts of Alaska; Pan et al. 2020 is the passive-microwave
  snow-off paper (its PMW product is a possible independent snow-off check).
- Bugs found: `ak_daymet()` sampling SEs were always NA (summarise order);
  `ak_glaciers()` small-glacier flag (same cause); alder logit interval
  ballooned near 0; filled snow-off SE mixed reference (vs MODIS) with MODIS
  rows (vs ground) and ignored the offset's uncertainty; `multipart` fired
  on corner-connected D8 cells; `snap_ambiguous` missed a main stem just
  above a tributary under 100% of its size.
- New: anomaly sampling SEs, `tdd_snowoff_se`, `ak_reliability()`,
  `ak_flag_check()` (in `ak_validate()$flag_check`), `snap_alt_area_km2`,
  `snap_off_stream` (D8 path test, `same_stream()`), `fallback_dem`,
  `ak_catchments(crs = )` with a NAD27 check, `ak_soils(lh_range = )`.
- CAKN-Data effect: alder intervals, the glacier and soil flags, filled
  snow-off SEs, and delineation flags of re-delineated sites change; the
  time-varying table gains `t_anom*_se`, `weather_flag`, `tdd_snowoff_se`
  (so the N paper's `wc_for_brms` gains those columns); step 06 writes
  `catchment_flag_check.csv`. Values of every other existing column are
  unchanged.
- Still open: Daymet interpolation error for Alaska (station
  cross-validation files), the NLCD 2001 Alaska error matrix, map dates vs
  sample dates (not quantified), SE for climate summaries other than
  `tdd_snowoff`.

## 3. Decisions taken

- `ak_add()` refuses duplicated `site_code` (union first) rather than
  guessing.
- Fire as columns: `fire_burned_pct`, `fire_burned_pct_since1987`,
  `fire_last_year`, `fire_n_years`, `fire_flag` (`small_catchment` under
  1000 acres). As a time series: `ak_fire()`.
- Outlet slope as columns: everything prefixed `outlet_`, first reach
  primary, others `outlet_slope_m_m_<L>`.
- The obs schema has no sample-id column; extra columns are kept after the
  schema's. Duplicates are judged on dataset, station, time, variable,
  depth and method.
- Parallel: base `parallel` (no new dependency). Workers get the cache
  option and the WhiteboxTools path, and `max_procs = 1`.
- Polygon error is not folded into the `_se` columns: it is structural
  (wrong branch, divide across a flat), so it is measured with alternative
  polygons (`ak_polygon_spread()`) and validation, not boundary jitter.
- Filled snow-off SE is the conservative one (MODIS RMSE added); the
  vignette gives the independent-errors alternative.
- The flags stay heuristics; `ak_flag_check()` measures them on the user's
  validation set rather than the package claiming a reliability.
- IfSAR tiles stay keyed by exact window (a fixed tile grid would let
  windows and nearby sites share tiles, but costs more per first window;
  decide after the live tests show download times).

## 4. Next, in order

1. Live run 1 (2026-11-03, Windows, quick): 33 pass, 8 fail. Fixed blind
   (no network here), see NEWS "Fixes from the first live run": fire cache
   write on Windows (D06, H01, Z03), ScienceBase links (E01; NLCD and
   Pastick use the same lookup), PRISM zip contents (G04), elevatr NA
   corners (B03), soils query (H02: cause unconfirmed; now reports the SDA
   error), coarse slope at the DEM's own resolution (F03). Ask for a quick
   rerun to confirm, then a full run (CAKN_DATA_DIR set, LEVEL "full").
2. Fill `approx_download` in `sources.csv` from the report's file sizes.
3. Speed: regional windows (one conditioned DEM, `wbt_unnest_basins` for
   all pour points in it), parallel Daymet pixels and MODIS years, one SDA
   union query for all catchments.
4. Observations: ACIS (GHCN-D/coop), SNOTEL (AWDB REST or the `awdb`
   package), WQP (WQX 3.0) — after probes P02-P04 pass. Gage basins as the
   catchment of each USGS station (`ak_gaged_basins()`; the station-number
   column name comes from live test E01). Invertebrate adapter in CAKN-Data
   when the user supplies the file.
5. Alternates (probe first): ESA WorldCover (P05), ERA5-Land via
   Open-Meteo (P06), TerraClimate/CHELSA/WorldClim normals. Each becomes a
   row in `sources.csv` plus a backend in `column_backends` or
   `timeseries_backends`; nothing else changes.
6. Datasets by priority: ADF&G Anadromous Waters Catalog, MRDS/ARDF mines,
   MTBS severity, waterbodies/wetlands (3DHP, HydroLAKES, NWI), ESA CCI
   permafrost and NCSCD, SoilGrids, MODIS NDVI and Landsat trends, NADP and
   gridded N deposition, NWM v3 retrospective, SNAP CMIP6 (api.snap.uaf.edu),
   GLIMS, GLiM and DGGS quadrangles, other ACCS taxa.
7. Optional: `ak_delineate(method = "hybrid")` for basins over ~1000 km²
   (MERIT/TDX unit catchments upstream, raster refinement in the outlet
   unit); stream burning (`burn = "3dhp"`) for flat lowlands.
8. Vignettes (a quick-start vignette on `ak_catchments`/`ak_add`/obs),
   pkgdown, GitHub Actions, v0.2.0; replace `<github-user>` in the install
   instructions.

At each step: update the live tests, run R CMD check, deliver zips of both
repos, check that the N-paper `join_data.R` output stays identical.

## 4b. Third round (2026-11-12): SNAP projections and Landsat greenness

Written blind (the sandbox reaches neither SNAP nor the Planetary Computer);
tested offline against fake servers. Ask for live tests G06, G07, H10, H11.

- `R/snap.R`. Coverage ids `cmip6_downscaled_<var>_<model>_<run>_v2_wcs`
  (dashes to underscores), from SNAP's data-api source (`routes/cmip6_downscaled.py`,
  `luts.py`). Describe via WCPS `describe($c, "application/json", ...)`; data via
  WCS GetCoverage JSON with X/Y trims a quarter cell inside the outer cell
  centres and a time trim on the axis named in the description (`ansi` or
  `time`). Array layout unknown: the time axis is found by length, X and Y by
  length (blocks are never square), and their directions by point requests at
  block corners; the result is kept in `options(AKCatchments.snap_flip)`.
  If G07 fails on orientation or shape, the first block's raw JSON is what to
  look at. Units: K converted if the median exceeds 150.
  Unknowns to confirm live: nil value, whether trims include edge cells,
  JSON encoding of NaN, request size limits, speed.
- `R/greenness.R`. STAC POST search (`platform` in L5-9, `eo:cloud_cover`),
  tiers filtered locally, SAS token from `/api/sas/v1/token/landsat-c2-l2`,
  `/vsicurl/` reads of `red`, `nir08`, `qa_pixel`, `qa_radsat` at points.
  Unknowns: `view:sun_elevation` present?, read speed per scene, token expiry
  handling over long runs.
- Not done: MODIS MOD13Q1 as an independent greenness check (ORNL web
  service, no login); parallel scene reads; a tile-based SNAP cache if
  many users share it. Item 6 above: SNAP CMIP6 and Landsat greenness are
  now in; MODIS NDVI is not.
- Watch for internal name clashes: a new `snap_point()` silently replaced
  the delineation one (caught by test-robustness; the SNAP helper is now
  `snap_check_point()`). Check for duplicate function names across `R/`
  before delivering.
- Offline suite: 78 test blocks, 66 pass, 12 skip (WhiteboxTools absent here).

## 4c. Vignettes (2026-11-12)

One vignette per dataset on one template (getting the data; using it;
outputs; uncertainty; checking and validating; pitfalls; references),
`delineation` (merged from delineation-method and -validation) and
`uncertainty` (cross-cutting only). The layer index is
`vignettes/_layers.md`, included as a child in both; **the README's table is
a copy of it**: update both when adding a layer. Dataset vignettes were
generated once from a script; edit them directly now. Download chunks are
`eval = FALSE`; checks: every `ak_*()` call and argument in vignette code
exists (a parse-and-walk script, not in the test suite).

## 4d. API rebuild and parallel processing (2026-11-13)

The user approved a rename ("Let's rename and I'll update CAKN-Data
later") and a parallel plan ("Yes do it with the rename"). **CAKN-Data
still calls the old names**; the old-to-new table is at the top of
`NEWS.md`.

Rules the user set: `ak_` = an Alaska product; generic operations have no
prefix and are built around "catchment". One function per dataset with a
`source` argument (`ak_alder`, `ak_permafrost`, `ak_landcover`,
`ak_climate_normals`, `ak_climate_projection`), options rather than
sibling functions (`ak_fire(by_year, samples)`). "B style": functions with
one value per site append their columns to what was passed in (caller's
CRS and columns kept, class `ak_catchments`, a provenance row added);
long results are tables with their own `ak_provenance` attribute;
`catchment_join()` puts a one-row-per-site table on the catchments;
`ak_write(x, file, name = table, ...)` writes everything, with one
provenance layer that has a `table` column. No `ak_add()`.

Plumbing (`R/run.R`): `site_map()` (per-site loop: cli progress bar naming
the site; `purrr::in_parallel()` when mirai daemons are set and the package
is installed, not under load_all; errors caught per site; workers get the
cache option, the WBT exe and `cores %/% daemons` WBT threads);
`site_values()`, `site_errors()` (`site_code`, `step`, `error`);
`add_columns()`, `as_layer_table()`, `add_errors()`; `http_parallel()`
(httr2 `req_perform_parallel`, 4 at a time, batches on httr2 < 1.1).
Per-site seeds with an explicit Mersenne-Twister kind (`with_seed()`):
mirai daemons default to L'Ecuyer, which gave different draws in parallel
until fixed; `test-api.R` checks parallel = sequential (runs only on the
installed package with NOT_CRAN).

Parallel loops: delineation (sites, or regional clusters), validation,
`import_catchments`, topography, outlet slope, positional draws (Jorgenson,
glaciers, `positional_uncertainty`), soils (per-site SDA), SNAP (per
coverage), Landsat (per group of catchments). Daymet uses concurrent HTTP
instead (daymetr dropped: request and parser re-implemented from its
source; check G01 live). Statewide rasters are not parallelised.

Delineation: `ak_delineate()` is split into a window loop,
`outlet_watershed()` and `finish_delineation()`, and records step times
(`t_*_s`), `n_windows`, `window_mode`. `ak_catchments(regional = TRUE)`
groups points (same DEM settings; complete-linkage clusters within
`cluster_km`, default `max_cells` x resolution minus twice the start
buffer, 20 km for IfSAR) and delineates each group on one conditioned
window; sites whose catchment touches the shared window's edge, or fail
there, go through `ak_delineate()`. Default `regional = FALSE` until live
test D05b compares the two paths on CAKN sites. `import_catchments()`
replaces `ak_condition_catchments()` and returns an `ak_catchments`
object with raster paths.

New: `ak_climate_normals(source = "daymet4", weather = w)` (monthly and
annual normals over the baseline, SEs from the daily sampling SEs, flags
`incomplete_baseline`, `few_pixels`); `weather_at_samples()` checks that the
baseline years are in the weather table. `vignette("workflow")`: full run,
where the time goes, daemon counts. `?ak_parallel`.

State: R CMD check clean apart from the sandbox (CRAN index unreachable,
missing Suggests, locale). Offline tests: 461 expectations pass, WhiteboxTools
included; the parallel test passes on the installed package.

Next:
1. Live tests, quick then full, with mirai installed. New or changed:
   D05 (2 daemons, step-time CSV), D05b (regional vs per-site IoU and
   time), D06, F01-F03, G01 (Daymet without daymetr), G03, G05, G07, H01,
   H02, H06, H09, H11, I01-I02. Use D05's step times to decide the next
   speed step (if DEM download dominates: regional default; if
   conditioning: larger WBT thread counts; if plots: `plot = FALSE` on
   reruns).
2. Update CAKN-Data to the new names (user said he would; offer help).
3. Then section 4 items 4 onward.

## 4e. Rock chemistry (2026-11-13)

From the project doc `claude/handoff_rock_n.md` (rock-N paper scoping).
`R/rock_chem.R`: `ak_rock_chem()`, `rock_chem_lithologies()`,
`rock_chem_crosswalk()`; `geology_tables()` (shared SIM 3340 reader, also
used by `ak_geology()` now); catalog row `rock_chemistry/olson2014`;
`vignette("rock-chemistry")`; live test H12; `test-rock-chem.R` (synthetic
SIM 3340 geodatabase and a synthetic workbook).

Sources found (web search, not downloaded; the sandbox can't reach
ScienceBase): the N and S class values are in **Lith-Nutrient.xls**
(ScienceBase item 56e47c89e4b0f59b85d4a69a, 284 classes); the oxides in
**Lith-MajorOxides.xls** (item 56e47649e4b0f59b85d4a694, 279 classes, 78
modifier classes, 15 uncharacterised); the physical properties
(hydraulic conductivity, compressive strength) in a third workbook (item
56e47dbee4b0f59b85d4a69e, 251 classes; not wired in). SIM 3340 rank
definitions (USGS Alaska map viewer help): Major >= 33%, Minor 10-33%,
Incidental < 10%, Indeterminate.

Decisions and assumptions (all stated in the docs):
- Matching by the most specific LITH term, then more general ones
  (`coarsened`). Modifier classes only by hand (`modifiers`, from a filled
  `rock_chem_crosswalk(file = )` CSV with a rationale per row).
- Rank shares: midpoints for the point estimate, uniform within the range
  for draws; Indeterminate = 10-100%. Olson's own weighting is not
  documented beyond "prevalence".
- Draws: class-mean shift `sdlog/sqrt(n)` (shared across units),
  formation deviation lognormal SD `formation_sd x sdlog`
  (mean-preserving; default 1, conservative), shares within rank ranges;
  sparse classes (n < 5) get the 90th percentile sdlog of well-sampled
  classes. Unit draws shared across catchments.
- Surficial deposits as mapped; `surficial = "exclude"` as sensitivity.
- Column names follow the package convention (`rockN_pct`, `_se`, ...),
  not the handoff's `rockN_mean`/`rockN_sd`.
- `ak_geology()`'s equal allocation for `geol_pct_*` is unchanged (it
  ignores the rank ranges; changing it would change CAKN-Data outputs).
  Offer to switch it to rank midpoints.

Unverified, to check in H12: the workbook layout (header detection,
tab-name matching, whether "N" columns are percent), the file names on
ScienceBase (the download falls back to any attached .xls/.xlsx), and how
many SIM 3340 terms match Olson class names.

## 4f. Validation out of the package (2026-11-13)

`validate_catchments()`, `print.ak_validation()`, `validation_summary()` and
the internal `flag_check()` are removed; `test-validate.R` became
`test-reliability.R`. Validation lives in the package paper's script
(`claude/validate_delineation.R` in the project), which queries USGS and
StreamStats itself and calls only `ak_catchments()` and
`compare_catchments()`. Docs now point there. CAKN-Data step 06 still calls
the old function and must move to `compare_catchments()`.

## 4g. Snapping with a reference network (2026-11-15)

- `R/snap_consensus.R`: `reach_area_km2()`, `snap_reaches()`, `d8_path()` (also used by
  `same_stream()` now), `snap_evidence()`, `snap_evidence_columns()`, `snap_alternative_polygons()`.
- `snap_point()` takes `pointer_file` and `reaches`; `outlet_watershed()` takes `reaches`;
  `finish_delineation()` takes `snap_alternatives`. The regional path loads reaches per site.
- The rule went through two designs. Plain vote counting (flow path + area fit) failed when a
  tributary is too small for TDX: the main stem below the junction got both votes. An "is the
  point on a channel" test failed because 5 m IfSAR rills exceed 0.05 km2. Final rule: flow
  path is a constraint; the reference chooses along it; else the largest on the path.
- Known limit, documented: an unmapped tributary just above a mapped main stem snaps below the
  junction (as `largest`), with `snap_ambiguous`.
- GEOGLOWS: VPUs 801-803 hard-coded; the object key is found by an anonymous S3 listing. Not
  reachable from the sandbox: live test C07 is the first real check (columns, units m2).
- Default snap is still `largest` until the paper's validation (`claude/validate_delineation.R`)
  shows the combined rule's value.

## 4h. Lidar, keep_rasters, and the paper repo (2026-11-16)

- `ak_dem(source = "lidar" | "best")`, `ak_lidar_index()` (R/dem.R): the newest project in the State of
  Alaska (DGGS image services) or USGS 3DEP 1 m index that covers the whole window and has no missing
  cells; vertical feet converted to metres; `dem_project` column. Lidar windows start at 2 km
  (`start_buffer()`); a catchment that outgrows the project fails rather than mixing DEMs. Live tests
  B06-B08 and D07 are the first real check (service fields, units, the 3DEP products API).
- `ak_gaged_basins()` reads both Curran (2024) releases (2019 wins for gages in both). Live test E01.
- Inline URLs and single-use values (NEWS "Code style"); shared lookup tables stay defined once.
- `keep_rasters = TRUE` argument of `ak_delineate()` (also through `ak_catchments(...)` and the regional
  path); the internal function that writes them is now `save_rasters()`. The paper's sensitivity stages
  use `FALSE`: at 5 m the kept rasters run to a few hundred MB for large catchments.
- `lidar_index_3dep()` copes with an empty answer.
- `ai-use/` (CRASR policy), excluded from the build. Earlier sessions are one reconstructed row.
- Validation now lives in its own repo, `akcatchments-paper` (R/validation/00-14, overnight run). It
  was dry-run offline on a synthetic catchment with a mocked copy of this package (DEM image service,
  GEOGLOWS files, ArcGIS queries, imagery, NLCD and gaged basins replaced), which found the bugs
  fixed above. Nothing in it has met the real services yet.
- Next for the package, after the paper's run: failure probability per catchment (`p_fail`, `inspect`)
  from the paper's stage 12 (plan in the paper repo, `notes/failure_model_backadd.md`); then decide
  whether `consensus` becomes the default snap.
- Disk: a statewide run caches DEM windows per site (tiles anchored at each window's corner, so
  neighbouring sites don't share tiles). A tile grid anchored on fixed multiples would let clustered
  sites share downloads; worth doing if the paper's run is slow or the cache too big.

## 4i. Tile grid, StreamStats characteristics, and the paper's validation rewrite (2026-11-16)

- `ak_dem()` image-service windows come from a fixed EPSG:3338 tile grid (`tile_px = 1000`; tiles cached as
  `<name>_<res>m_<col>_<row>.tif`, cut to the window from a VRT). Tests in test-dem-grid.R. This answers the
  "Disk" note in 4h.
- `ak_streamstats()`: snapped point, `characteristics = TRUE` (SS-Hydro basin characteristics as a list
  column), server errors stop instead of returning NULL. Tests in test-streamstats.R; live test E02.
- `ak_glaciers()` no longer fails when no glacier is near any catchment in the call (found by the paper's dry
  run); test in test-additions.R.
- The paper's validation is now one script per step (R/validation/01-26 and run_validation.R, with a pilot
  setting and R/validation/README.md listing every output). Steps 18-24 test the attribute layers and their
  intervals (StreamStats characteristics, NPS descriptors, permafrost maps, delineation against stated error,
  lidar topography, GHCN-Daily stations, Monte Carlo and Daymet sampling, test domains). The offline dry run
  now mocks StreamStats, PRISM, Pastick, alder, rock chemistry, Daymet and Soil Data Access too.
- Next for the package: as in 4h (failure probability, consensus default), plus whatever the paper's pilot
  turns up against the real services.

## 4j. Citations and the two manual downloads (2026-11-19)

- `ak_citations()` (R/citations.R): provenance record and/or `layers` -> one row per layer and reference, with
  terms and acknowledgement; `bibtex =` writes unique entries. References live in inst/extdata/references.csv
  (authors as "Family, Given; ..." with {corporate names}; blank year + accessed = "yes" for live services,
  cited with the date the layer was added). sources.csv has `references` (keys), `terms`, `terms_url`,
  `acknowledgement`. New vignette data-sources.Rmd builds its tables from these, so keep them current there.
- Checked against the providers' pages (2026-11-19): NSIDC data use, Daymet citations, PRISM terms, StreamStats
  citation, NLCD Alaska, IfSAR, snow metrics authors, alder authors, TDX-Hydro, HydroRIVERS licence. Not checked:
  the Copernicus DEM year (kept 2024 from the old citation), SNAP's terms page, the full GEOGLOWS author list
  (cited "Hales and others"), dataRetrieval's year.
- Jorgenson: `jorgenson_shapefile()` downloads the North Slope Science Catalog zip (URL from Caleb, 2026-11-19).
- RGI: `glacier_shapefile()` finds the user's netrc file with an urs.earthdata.nasa.gov line (`earthdata_netrc()`:
  ~/.netrc or _netrc, also in USERPROFILE), finds RGI2000-v7.0-G-01_alaska.zip by crawling NSIDC's HTTPS listing
  under nsidc0770_rgi_v7/ (3 levels; the exact path wasn't known), and downloads with curl's netrc options, Basic
  auth sent without a challenge (`httpauth = 1`; R's curl otherwise waits for one and Earthdata Login redirects to its
  sign-in page instead) and a cookie jar: NSIDC's documented `curl -b -c -L -n`. A first version used an Earthdata
  bearer token; Caleb pointed out NSIDC's guide for daacdata.apps documents only username/password, so it was
  replaced the same day. No netrc: error of class `ak_manual_download`, which `ak_prefetch()` reports as `manual`.
  A redirect to the login page is reported as a wrong username or password.
- Checked offline against a local imitation of the Earthdata Login flow (data host -> login host with Basic auth ->
  callback sets cookie -> data): works with the right password, ends at the login page with a wrong one.
- **Not yet tried against the real servers** (the sandbox can't reach them): run `ak_prefetch(x, "glaciers")`
  with a netrc file, and `ak_prefetch(x, "permafrost")`, and add live tests once they work.

## 5. Still open from the error audit

The Selkowitz & Stehman NLCD 2001 Alaska confusion matrix (for
`ak_landcover(error_matrix = )`) and the Daymet station cross-validation
files for Alaska.

## 6. How the user likes to work

- R: concise, pipe-based (`|>`, `\(x)`), tidyverse; minimal defensive
  scaffolding; targeted fixes; don't rewrite what works.
- GitHub only, not CRAN. Don't touch the user's local files.
- Uncertainty for every layer (`_se`, `_lo95`, `_hi95`, `_flag`), 500
  draws. Every delineation plotted on imagery.
- Shared Box Drive cache: `CAKN_DATABASE_CACHE` (CAKN-Data),
  `AKCATCHMENTS_CACHE` (package).
