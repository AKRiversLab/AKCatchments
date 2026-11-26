# AKCatchments 0.2.0.9005 (development)

Built on 0.2.0.9000. It does not include the 0.2.0.9001 edits made elsewhere, which were never run. 0.2.0.9002 to
0.2.0.9004 were never run either: 0.2.0.9002 sized windows from GEOGLOWS upstream footprints (replaced here by a coarse
pass over the same DEM), 0.2.0.9003 kept least-cost breaching, and 0.2.0.9004 switched to complete breaching without a
choice.

## Faster delineation of large catchments

* Windows grow on the sides the catchment touches, not all four: each touched
  side moves out to twice its distance from the point (at least 2 km), up to
  `max_buffer_m`. An elongated catchment no longer pays for a square around
  its outlet.
* A catchment cut off by the first square gets a window sized by a coarse
  pass: the same DEM at 100 m (a 40 km square is 160,000 cells, doubled
  while that catchment reaches its edge), delineated from the coarse cell
  with the most upstream area among those mostly inside the part already
  delineated; its bounding box plus 1 km. A large catchment is then
  conditioned twice instead of at every doubling (3 to 6 windows of up to 36
  million cells before), and no reference network is needed. The pass only
  sizes the window: the catchment is the full-resolution DEM's, and where it
  still touches that window (where coarse and full-resolution divides part,
  often by several km), the touched sides move out by half the window's
  width or height. Lidar windows, which a project bounds, grow side by side.
* Coarsened cell sizes come from a ladder (1.25, 1.5, 2, 2.5, 3, 4, 5, ...
  times the source's: 6.25, 7.5, 10, 12.5, 15, 20, 25 m for IfSAR) instead of
  any value, so a growing window, nearby sites and reruns reuse the same
  image-service tiles rather than downloading a new set at a new cell size
  each time; a window is at most a third coarser than it needs to be.
* A window that cannot get any larger (the DEM's coverage ends) is no longer
  fetched and conditioned again at every widening.
* New `ak_delineate(window = )`: the first window, given (a reference
  polygon's bounding box with a margin, or another run's window). Every
  delineation now records its window (`window_xmin`, `window_ymin`,
  `window_xmax`, `window_ymax`, EPSG:3338) and `window_plan` (`"square"`,
  `"coarse"`, `"given"`); `window` and `res = dem_res_m` repeat it on the same
  grid. In regional windows each point's `window` is part of the shared one.
* `ak_catchments(checkpoint_dir = )` saves each site as soon as it finishes and
  reads it back on a rerun with the same arguments, package and
  WhiteboxTools versions, so an interrupted run resumes.
* `options(AKCatchments.log = file)` writes one line per finished stage of
  every delineation (workers too): site, stage, seconds, window size and cell
  size, edges touched, number of reference flowlines.
* New `depressions` argument (`ak_delineate()`, so `ak_catchments()`, and
  `import_catchments()`): `"breach"` (default) breaches every depression
  (WhiteboxTools BreachDepressions, Lindsay 2016; single-cell pits raised
  first); `"least_cost"` cuts least-cost breach paths of up to 100 cells
  (BreachDepressionsLeastCost) and breaches what is left instead of filling
  it; `"fill"` fills (FillDepressions). The method is recorded in the new
  `depressions` column, the regional path only shares a window between
  sites with the same method, and kept rasters (`delineation/<site>/`,
  `scratch/<site>/`) are named by method, so rasters conditioned another
  way are never reused, including those of earlier versions.
* The default was least-cost breaching with filling of what it left.
  Conditioning a 16-million-cell IfSAR-like DEM took 29 s with
  `"breach"`, 135 s with `"least_cost"` and 133 s with the old default. A
  shorter least-cost search is nearly as fast (34 s at 25 cells) but fills
  more, and filled flats made catchments less stable. Share of catchments
  within IoU 0.95 of their noise-free version, on a real DEM (Cumberland
  Mountains, 80 m cells, 1 m noise added): `"breach"` 88%,
  `"least_cost"` 89%, old default 89%, 25-cell search 77%, `"fill"` 76%;
  on synthetic IfSAR-like terrain full of closed depressions (5 m, 0.4 m
  noise): 78%, 84%, 85%, 75%, 75%. WhiteboxTools' filling step (in the old
  default and FillDepressions) also panics now and then under load: it
  takes its raster back before every thread has let go. `"breach"` and
  `"least_cost"` never run it; `"fill"` gets three tries. Results change
  wherever methods route a depression differently, most where large closed
  depressions decide the drainage.
* A WhiteboxTools tool that fails no longer goes unnoticed. The whitebox
  package only prints a message, so a failed breaching left the last
  window's rasters in place and the delineation carried on with them (a
  wrong window, or one grown for nothing). Old outputs are now removed
  before each run and a missing one stops the run with its name.
* Results differ slightly from 0.2.0.9000 where a catchment needed a larger
  window: windows and their cell sizes are different (usually smaller and
  finer).

## Breaking: one function per dataset, layers append to the catchments

The API is rebuilt around two rules. Functions named `ak_` get Alaska
products; generic operations on catchments have no prefix. Layer functions
with one value per site append their columns to the catchments you pass
in and record provenance (so they chain with `|>`); functions with long
results (by day, year, model or sample) return a table that carries its
own provenance. There is no catalog dispatcher any more.

| was | now |
|---|---|
| `ak_add(x, layer)`, `ak_add_timeseries()`, `ak_timeseries()` | call the layer function: `x <- ak_landcover(x)`; long tables are returned |
| `ak_sources()` | `ak_layers(details = TRUE)`; `ak_layers()` has a `returns` column |
| `ak_condition_catchments()` | `import_catchments()`: same object as `ak_catchments()`, provenance kept |
| `ak_delineate_many()` | `ak_catchments()` |
| `ak_flag_delineation()`, `ak_flag_check()` | flags documented in `?delineation_flags`; the flag check is removed (see below) |
| `ak_validate()`, `ak_compare_catchments()`, `ak_plot_catchment()` | removed (see below), `compare_catchments()`, `plot_catchment()` |
| `ak_alder()`, `ak_alder_wrst()`, `ak_alder_se()` | `ak_alder(source = c("accs2021", "nps_wrst"))`; the SE is always added |
| `ak_permafrost()`, `ak_permafrost_pastick()` | `ak_permafrost(source = c("jorgenson2008", "pastick2015"))` |
| `ak_landcover()` | `ak_landcover(source = "nlcd2016")` (room for other products) |
| `ak_fire()`, `ak_fire_windows()`, static fire columns | `ak_fire(x)` (columns), `ak_fire(x, by_year = TRUE)`, `ak_fire(x, samples = )` (tables) |
| `ak_prism()` | `ak_climate_normals(source = "prism8110")`; new `source = "daymet4"` computes 1991-2020 normals from `ak_weather()` output |
| `ak_daymet()` | `ak_weather()`; pixels requested concurrently (httr2), no daymetr dependency |
| `ak_weather_anomalies()`, `ak_climate_summaries()`, `ak_melt_daymet()` | `weather_at_samples(weather, samples)`: one table with all three; anomalies are documented as departures from the baseline daily climatology, and the baseline years are checked |
| `ak_snowoff_modis()`, `ak_snowoff(modis, melt)` | `ak_snowoff(x, years, weather = w)` (`fill = FALSE` for MODIS only) |
| `ak_snap_cmip6()`, `ak_snap_ensemble()`, `ak_snap_models()` | `ak_climate_projection(source = "snap_cmip6_4km")`, `projection_ensemble(wide = )`, `ak_climate_projection_models()` |
| `ak_greenness_landsat()`, `ak_greenness_summary()` | `ak_peak_ndvi()`, `peak_ndvi_summary()` |
| `ak_raster_mean()`, `ak_class_shares()` | `catchment_mean()`, `catchment_class_shares()` (append, with provenance) |
| `ak_positional_uncertainty()`, `ak_area_adjust()`, `ak_polygon_spread()`, `ak_reliability()` | `positional_uncertainty()`, `adjust_class_shares()`, `polygon_spread()`, `attribute_reliability()` |
| `ak_geology()$occurrence`, `ak_soils()$summary` | the columns are appended; `details = TRUE` for the unit and component tables |
| `ak_temperature_stations()`, `ak_temperature_daily()` | internal; use `ak_obs_temperature_stations()`, `ak_obs_temperature()` |
| `ak_crs()` | `akalbers_crs()` |
| `ak_catchments(workers = )` | removed: start mirai daemons (below) |

New: `catchment_join()` puts a one-row-per-site table (with its
provenance) on the catchments; `ak_write(x, file, ...)` writes named tables
too, with their provenance in the one provenance layer.

## NLCD 2001 Alaska error matrix

* `ak_landcover(error_matrix = "selkowitz2011")` bias-adjusts the NLCD class
  shares with the Level II error matrix of Selkowitz and Stehman (2011, Table
  4a), shipped in `inst/extdata/nlcd_ak2001_error_matrix.csv`. The table
  gives percent of area; each row is turned into counts with an effective
  sample size that reproduces the SE of the class's user's accuracy in their
  Table 3a (the sample was clustered, so the pixel counts overstate it). Two
  printed cells that disagree with the row and column totals and the Level I
  matrix are corrected (map 43 x reference 41: 0.64 to 0.04; map 71 x
  reference 95: 0.40 to 0.00). The transcription and derivation are in
  `data-raw/nlcd_ak2001_error_matrix.R`.
* The land-cover vignette describes the assessment (design, regional and
  edge differences, unsampled classes) and no longer says its sample was
  stratified by map class (it was geographically stratified).
* `ak_citations()` adds Olofsson et al. (2014) to the land-cover references.

## Citing the data, and fewer manual downloads

* `ak_citations(x)` returns the references for every layer recorded in
  `x`'s provenance (or named in `layers`), with each provider's terms of
  use and the acknowledgement text some ask for, and writes them to a
  BibTeX file with `bibtex =`. References to live services carry the date
  the layer was added. The references are in `inst/extdata/references.csv`;
  `sources.csv` gains `references`, `terms`, `terms_url` and
  `acknowledgement`, and `ak_layers(details = TRUE)` shows the last three.
  `citation("AKCatchments")` now has a CITATION file.
* New `vignette("data-sources")`: how to cite, what each provider asks,
  and the downloads that need anything.
* The Jorgenson et al. (2008) permafrost map is downloaded once from the
  North Slope Science Catalog instead of by hand.
* The RGI 7.0 glaciers are downloaded once from NSIDC with the Earthdata
  Login in your `.netrc` file, the method in NSIDC's programmatic access
  guide (`ak_download(netrc = )` sends it to the login server and keeps the
  session cookie, as `curl -n -L -b -c` does). Without it, `ak_glaciers()`
  stops and `ak_prefetch()` reports `manual`, each saying how to set it up
  or download the file by hand.
* Daymet is cited as Version 4 R1 (doi:10.3334/ORNLDAAC/2129, the version
  the Single Pixel Extraction Tool serves) plus the tool itself.

## Lidar where it exists

* `ak_dem()`, and so `ak_delineate()`, `ak_catchments()` and
  `import_catchments()`, take `dem_source = "lidar"`: a lidar bare-earth
  model from the newest project that has elevation for every cell of the
  window. Projects come from the State of Alaska's elevation index (DGGS
  image services; vertical units in feet converted to metres) and the USGS
  3DEP 1 m DEM index (cloud-optimised tiles listed by The National Map).
  `dem_source = "best"` uses lidar where it covers the window and IfSAR
  otherwise. Default cell size 2 m; lidar windows start at a 2 km
  half-width. A new `dem_project` column names the lidar project.
* `ak_lidar_index()`: the lidar footprints, cached; `refresh = TRUE` after
  new projects.
* Live tests B06-B08 (index, a lidar window in Anchorage checked against
  IfSAR, `"best"` at the gage) and D07 (Chester Creek on lidar vs IfSAR).

## Glaciers: catchments far from any glacier

* `ak_glaciers()` stopped with "replacement has 1 row, data has 0" when no
  glacier lay within reach of the catchments it was given. It now returns
  `glacier_pct = 0` with a zero interval, as it already did for single
  catchments without glaciers among others that had some.

## StreamStats basin characteristics

* `ak_streamstats()` returns the snapped point (`ss_lon`, `ss_lat`) and,
  with `characteristics = TRUE`, every basin characteristic SS-Hydro computes
  for the basin (`ss_characteristics`, a list column of code, name, unit,
  value). Server and connection errors now stop instead of returning `NULL`,
  so an outage is not mistaken for a point outside StreamStats' regions.

## DEM downloads on a fixed tile grid

* Image-service windows (IfSAR, DGGS lidar) are cut from tiles on a fixed
  EPSG:3338 grid (`tile_px = 1000` cells a side, 5 km at 5 m) and cached by
  grid position, so nearby sites, a window and its enlargements, and
  repeated runs share downloads. `ak_dem()`'s `max_px` is now `tile_px`.
  Windows cached under the old naming are not reused.

## Keeping the rasters is optional

* `ak_delineate()` and `ak_catchments()` take `keep_rasters = TRUE`. With
  `FALSE` the conditioned rasters (DEM, breached DEM, D8 pointer and
  accumulation) are not kept in the cache, so repeated or sensitivity runs
  that only need polygons don't fill the disk (a few hundred MB for a
  catchment of thousands of km2). `files` is then `NULL`; `ak_topography()`
  conditions the DEM again.
* `ak_lidar_index()` no longer fails when the 3DEP index returns no
  projects, and a window outside every lidar project is named with its size.

## AI use

* AI use in this package follows the [CRASR AI-use policy](https://github.com/CRASR/ai-use);
  the record is in `ai-use/` (excluded from the build).

## USGS gaged basins: both releases

* `ak_gaged_basins()` reads the 2014 release (471 gages) as well as the
  2019 release (253 gages), about 600 gages together; a gage in both comes
  from 2019. New columns `usgs_site_no`, `site_code` (`"USGS-..."`) and
  `release`. The boundaries follow the Watershed Boundary Dataset above
  each gage (documented).

## Code style

* URLs and single-use values are written where they are used rather than
  stored in objects (GEOGLOWS, Landsat STAC, Daymet, Arctic Data Center,
  ACCS alder, NPS WRST, BLM fire, SNAP rasdaman). `ak_rock_chem()` shows its
  `rank_shares` default in the signature. Shared lookup tables (cache folder
  names, class codes, column lists used in several places) stay defined once.

## Snapping with a reference stream network

* `ak_delineate(snap = "reference")` snaps to the channel whose area fits
  a nearby reach of a reference network; `snap = "consensus"` keeps the
  outlet on the point's own D8 flow path (so a snap can't cross into
  another watershed) and lets the network choose along it. Reference:
  `snap_reference =` a source with upstream area (`"geoglows"`, `"tdx"`,
  `"merit"`, `"hydrorivers"`) or sf lines; `snap_reference_dist_m`
  (300 m). New columns: `snap_evidence`, `snap_on_path`,
  `snap_area_match`, `snap_conflict` (also a warning flag), reach
  distance and area range, and the area each rule would choose
  (`snap_area_largest_km2`, `snap_area_reference_km2`,
  `snap_area_consensus_km2`). `snap_alternatives = TRUE` delineates the
  other rules' outlets on the same rasters (`$alternatives`). The default
  is still `snap = "largest"`.
* `ak_flowlines(source = "geoglows")` and `ak_download_flowlines(source =
  "geoglows")`: TDX-Hydro streams (12 m TanDEM-X, from 5 km2, upstream
  area at both reach ends) from the public GEOGLOWS v2 AWS bucket, Alaska
  VPUs 801-803. A route to TDX-Hydro when the NGA site is unreachable.

## Validation moves to the package paper

* `validate_catchments()` (formerly `ak_validate()`) and its flag check are
  removed. Validating the method is the paper's job: its script fetches
  the references (published USGS drainage areas, StreamStats basins)
  directly from USGS and runs the package as a user would. To compare your
  own sites with reference polygons, use `compare_catchments()` and
  `plot_catchment(reference = )`. CAKN-Data step 06 needs that change.
* Live test E04 is gone (E03 covers the gage); E05 compares the CAKN
  delineations with the hand-drawn polygons through `compare_catchments()`.

## Rock chemistry from lithology

* `ak_rock_chem(x, element = c("N", "S", "CaO", "MgO", "P2O5", ...))`: whole-rock
  N, S and the eight major oxides of each catchment, from the lithology
  geochemistry of Olson and Hawkins (2014; the method behind StreamCat's
  rock %N) applied to the lithologies and ranks of SIM 3340, with Monte
  Carlo uncertainty (class mean, formation-to-formation spread, shares
  within the rank ranges: Major >= 33%, Minor 10-33%, Incidental < 10%),
  the share without a value, the share from sparse classes and from
  coarsened term matches, and flags. `surficial = "exclude"` as a
  sensitivity; `modifiers` for Olson's modifier classes (e.g.
  carbonaceous shale) assigned by hand.
* `rock_chem_lithologies()`: the class values with records and spread per
  class (the workbook audit). `rock_chem_crosswalk()`: which SIM 3340
  terms matched which class, and a CSV template for the modifier
  assignments.
* `vignette("rock-chemistry")`; `ak_geology()` and its vignette document
  the rank definitions. Written without access to the workbooks or the
  SIM 3340 tables: the workbook parser and the vocabulary match must be
  checked in live test H12.

## Speed: progress, parallel processing, shared DEM windows

* Every per-site loop shows a progress bar naming the site;
  `options(AKCatchments.verbose = TRUE)` logs one line per site.
* Parallel processing through `purrr::in_parallel()` and mirai: start
  daemons (`mirai::daemons(4)`) and the loops of `ak_catchments()`,
  `import_catchments()`, `ak_topography()`,
  `ak_outlet_slope()`, the positional draws (`ak_permafrost()`,
  `ak_glaciers()`, `positional_uncertainty()`), `ak_soils()`,
  `ak_climate_projection()` (by coverage) and `ak_peak_ndvi()` (by group of
  catchments) use them. Results are identical sequentially or in parallel
  (random draws seeded per site). WhiteboxTools threads are shared out
  among daemons. See `?ak_parallel`. Requires purrr >= 1.1.0.
* `ak_catchments(regional = TRUE)`: nearby points share one DEM window,
  downloaded and conditioned once; points whose catchment reaches the
  shared window's edge are delineated on their own. Tested on the synthetic
  DEM to give the same polygons as per-site windows; compare the two on
  your sites (live test D05b).
* Each delineation records its step times (`t_dem_s`, `t_condition_s`,
  `t_watershed_s`, `t_finish_s`, `t_flowlines_s`, `t_plot_s`,
  `t_total_s`), `n_windows` and `window_mode`.
* `vignette("workflow")`: a full run for many sites, where the time goes,
  and what to parallelise.

# AKCatchments 0.1.0.9000

## New

* Vignettes, one per dataset: `topography`, `alder-accs`, `alder-wrst`,
  `landcover-nlcd`, `greenness-landsat`, `permafrost-jorgenson`,
  `permafrost-pastick`, `glaciers-rgi`, `geology-sim3340`, `soils-statsgo`,
  `fire-afs`, `climate-prism`, `weather-daymet`, `projections-snap`,
  `snowoff-modis`, `observations`. Each covers getting the data, using it on
  one catchment, many, or your own polygons, the output columns, how its
  uncertainty is computed, how to check and validate it, and pitfalls.
  `delineation-method` and `delineation-validation` are merged into
  `vignette("delineation")` (with the reference flowlines, StreamStats and
  gaged basins, and polygon uncertainty), which ends with a map of the
  layers by theme. The same table (`vignettes/_layers.md`) is in
  `vignette("uncertainty")`, whose product-by-product sections moved into
  the dataset vignettes; it keeps what all layers share. Each layer
  function's help page points to its vignette.
* Every layer function takes a single `ak_delineate()` result, a list of
  them, or `ak_delineate_many()` output as well as sf polygons, and so do
  `ak_add()` and `ak_add_timeseries()`.
* Climate projections: `ak_snap_cmip6()`, `ak_snap_ensemble()`, `ak_snap_models()`, `ak_seasons()`
  (catalog: `ak_add(x, "climate_projection")` for ensemble columns,
  `ak_add_timeseries(x, "climate_projection")` for the per-model table). SNAP's
  4 km daily downscaled CMIP6 (QDM against ERA5-WRF; up to 13 models, four
  SSPs), queried on SNAP's rasdaman server directly: the SNAP Data API's
  point endpoint rounds daily values to whole units. Returns each model's
  change from its own baseline (default 1991-2020: historical run to 2014,
  scenario from 2015) by month and season, to apply to Daymet, with an
  internal-variability SE (autocorrelation-discounted years; log scale for
  precipitation), the local spread of the change around sub-grid catchments,
  and an ensemble summary (between-model SD, prediction interval for one more
  model, sign agreement, flags). Monthly means of each 4 km cell are cached
  (`snap_cmip6/`). The server's array layout is worked out from the array's
  shape and corner check points rather than assumed.
* Greenness: `ak_greenness_landsat()`, `ak_greenness_summary()` (catalog:
  `ak_add_timeseries(x, "greenness")`, then `ak_add(x, "greenness")`). Landsat
  5/7/8/9 Collection 2 annual peak NDVI from the Microsoft Planetary Computer
  (no login), read at regularly spaced points, with the screening,
  cross-sensor calibration and phenology-adjusted peak of Berner et al. (2020)
  and 'LandsatTS' (ported, MIT). Two refinements, from tests with known sensor
  offsets: calibration pairs are centred on the seasonal peak and each
  observation is moved to the focal day along its own sensor's seasonal curve
  (residual calibration bias 0.001-0.002 instead of up to 0.03), and the
  calibration SE has a floor of 0.002. Annual SE = sampling + peak estimation
  + calibration; the trend SE adds the step a calibration offset makes where
  the sensor mix changes. Readings are cached per catchment (`landsat_c2/`).
* Live tests G06 (SNAP coverage description), G07 (SNAP deltas for two
  catchments), H10 (Planetary Computer search and token), H11 (Landsat
  greenness 2013-2020, full level).

* `ak_alder_wrst()` (catalog: `ak_add(x, "alder", "nps_wrst")`): alder cover
  from the NPS Wrangell-St. Elias land cover map (2004-2007, 28.5 m,
  DataStore 2171667), for the southern WRST catchments outside the ACCS map.
  Uses the alder cover recorded for each of the map's 10,062 spectral
  signatures; signatures whose plots recorded life forms only take their
  class's mean (`unrecorded = "class_mean"`, the default: r = 0.93 with ACCS
  at 58 WRST sites, against 0.80 with `"zero"`) or 0. Exact SE from the
  within-class spread, Beta 95% interval, coverage, unrecorded share and
  flags. Columns are prefixed `alnus_wrst_`, so they sit beside ACCS's for
  calibration.

## Fixes from the full live run (2026-11-03)

* `ak_permafrost()` failed on the 2008 shapefile from the North Slope Science
  Catalog ("Could not find the extent field"): its `PF_EXTENT` field codes
  the classes as letters (C, D, S, I, plus G glacier, U unfrozen, W water).
  Letter codes are now recognised, and the function stops if no extent class
  is recognised at all, instead of returning 0% permafrost everywhere.

* `ak_alder()` failed on every call: the coverage column came out as
  `alnus_cover_coverage_pct` while the flag (and the docs) used
  `alnus_coverage_pct`. It is now `alnus_coverage_pct`.
* `ak_alder()`: catchments outside the ACCS map (south of about 61-61.8 N,
  e.g. southern Wrangell-St. Elias) get `NA` (was `NaN`) and the new flag
  `outside_map`; `partial_coverage` is kept for 0-95% mapped.
  `ak_raster_mean()` returns `NA` instead of `NaN` for any layer with no data
  in a catchment.
* `ak_permafrost_pastick()`: the ScienceBase `.7z` holds a `.tar`, which was
  left unpacked ("No raster found"). Archives found inside the folder are
  unpacked in place and then deleted, so a cache left with only the `.tar`
  recovers without downloading again.
* `ak_geology()`: the layer CRS is read without SQL (the `LIMIT 1` query
  failed on Windows / GDAL 3.12: "Query execution failed"), and the
  geodatabase holding `AKStategeol_poly` and `NSALith` is chosen when the
  archive unpacks more than one. A missing layer now stops with the layers
  that are there.
* Unit tests: each test writes its synthetic DEM to its own folder. On
  Windows a raster left open by an earlier test blocked the overwrite
  ("cannot overwrite existing file"), failing two tests.
* Live tests P07 and P08 probe the NPS WRST land cover map (2004-2007,
  28.5 m) as an alder source for southern WRST, where ACCS has no data:
  unpacking, grid, class table and alder classes (P07); coverage at every
  WRST catchment and overlap with ACCS under draft Viereck weights (P08).
* Live tests: `ONLY` can be set with env `AK_TEST_ONLY` ("H03,Z02").
  C04 (TDX-Hydro) is skipped; earth-info.nga.mil is unreachable
  from the user's networks.

## Fixes from the first live run (Windows, quick level)

* Fire perimeters: the cache is written in the temp folder, copied next to
  the target and renamed, with retries. On Windows the rename into the cache
  failed ("used by another process"), and the leftover file then blocked
  every later fire call in the session. Downloads also retry the final rename.
* `ak_gaged_basins()`, NLCD and Pastick: ScienceBase download links are now
  looked up in the item record (`sb_download()`); the old
  `catalog/file/get/<id>?name=` links return 404. The gaged-basin shapefile
  is attached as separate .shp/.shx/.dbf/.prj files, not the zip it was.
* `ak_prism()`: the Alaska normals are ARC ASCII grids named `.txt` inside
  the `_asc.zip`; it looked for `.asc` and got `NA`. Their longitudes run
  172 to 237 E (across the date line), so catchments are shifted to 0-360
  before extraction; every value was `NaN`.
* `ak_soils()` retries a Soil Data Access query up to three times.
* Downloads have no total time limit (the 10-minute limit cut off the 5.4 GB
  alder archive at 3.5 GB); they stop only if no data arrives for 2 minutes.
* `ak_geology()` (and other `wkt_filter` reads): the layer CRS is read from
  one feature; `st_layers()` returned an empty CRS for the SIM 3340
  geodatabase and the read failed.
* Pastick: also finds .bil/.dat/ESRI grid rasters, and lists what was
  unpacked when no raster is found.
* Unit tests always use a temporary cache, even when the session has one set.
* `ak_dem("elevatr")`: downloads whole tiles and projects them onto an
  EPSG:3338 grid over the area; elevatr's own bbox clip left 12% `NA` corners.
* `ak_soils()`: outlines are simplified to 30 m before the Soil Data Access
  query (raster-traced outlines are too long a query), SDA errors are
  reported instead of dropped silently, and sites with no soil data get `NA`
  and `no_soil_data` instead of an error.
* No default cache folder: `ak_cache_dir()` stops until one is set with
  `ak_set_cache(path, persist = "user")` (startup message says so). It used
  to fall back silently to a per-user folder. The live tests' cache now
  defaults to the session temp folder (or `AK_TEST_CACHE`), not Documents.
* `ak_topography()`: `topo_slope_mean_deg_10m` is the native mean on a 10 m
  DEM (it was `NA`); a scale finer than the DEM is still `NA`, as documented.

## Uncertainty review (second round)

Every uncertainty calculation was re-checked against its source. Fixes,
each with a test:

* `ak_daymet()`: the between-pixel SD was computed after the column had
  been averaged (`summarise()` sees the replaced column), so `tmean_sd`,
  `prcp_sd`, `tmean_se_sampling` and `prcp_se_sampling` were always `NA`.
* `ak_glaciers()`: the same ordering bug made the `small_glaciers` flag
  use the summed glacier area instead of the pieces.
* `ak_alder()`: the 95% interval is now the quantiles of a Beta
  distribution with the map value as mean and the SE as SD. The logit
  delta method used before balloons near the bounds (at 2% cover in a 10
  km² catchment its upper limit was 87%; now 20%). New flag
  `error_exceeds_cover` where no Beta fits.
* `ak_snowoff()`: `snowoff_se_d` for Daymet-filled years is now
  sqrt(RMSE² + within-site SD² + the site offset's variance), on the same
  footing (against the ground) as the MODIS rows and specific to each
  site: sites with few or no overlap years get larger SEs. Before, every
  filled year got the pooled residual SD, smaller than the MODIS RMSE.
  The calibration attribute adds `within_site_sd_d`.
* `ak_soils()`: one-component map units no longer count toward the
  `assumed_concentration` flag. New `lh_range` argument (the L-H range as
  a 90% or 80% range).
* `ak_area_adjust()` refuses map classes with fewer than two reference
  samples (the SE formula divides by n - 1).
* Documentation: the 127 m positional SD suggested for 1:250,000 maps is
  the map-accuracy standard's 90th-percentile error (CE90), i.e. about twice
  the standard's SD (59 m); now said so.

New uncertainty carried into observations:

* `ak_weather_anomalies()` adds the point-sampling SE of every anomaly
  (`t_anom7_se`, ..., `p_anom30_se`) and `weather_flag`, from the anomalies
  of the individual Daymet pixels (exact for temperature). `ak_daymet()`
  stores the catchment pixel counts (`npix_total`) for it.
* `ak_climate_summaries()` adds `tdd_snowoff_se` (snow-off SE carried to
  thawing degree days by the delta method).
* `ak_reliability()`: how much of each variable's spread across sites is
  error (the reliability ratio).

Delineation checks:

* `snap_ambiguous` now fires when another channel in reach differs in area
  by more than 25% (was a factor of 2, which missed a main stem just above
  a sizeable tributary), and `snap_alt_area_km2` gives that channel's area.
* New warning `snap_off_stream`: by the D8 flow paths, the snapped outlet
  is not on the sample point's stream (neighbouring stream, or the other
  branch above a confluence).
* `multipart` no longer fires on D8 catchments whose cells touch only at a
  corner (all D8 catchments are connected; that was a false failure).
* `ak_delineate(fallback_dem = )`: a second DEM used when the first fails
  or runs into missing data, recorded as `dem_fallback`.
* `ak_catchments(crs = )`: coordinates in NAD83 or NAD27; NAD27 conversions
  that PROJ can't do (missing NADCON grid) stop or warn instead of silently
  placing points 100-200 m off.
* `ak_validate()` returns `flag_check` (`ak_flag_check()`): for each flag,
  how many sites it marked, how many of those disagree with the reference,
  and the share of all disagreeing sites it caught.

Vignettes: `vignette("uncertainty")` (sources, calculations, propagation
into observations, alternatives and downstream consequences, product by
product; replaces "Catchment attributes and their uncertainty") and
`vignette("delineation-method")` (each step, which DEM when, error
handling, pitfalls and how reliably they are reported).

## First round

### New

* `ak_catchments()`: the entry point. Points in (data frame or sf, any CRS),
  checked catchments out (one sf row per site, with diagnostics, flags,
  snapped outlet, plot path and the conditioned rasters), failures recorded
  in `attr(x, "errors")`. Per-site arguments come from columns named like
  `ak_delineate()` arguments. `workers` delineates in parallel R processes
  (socket cluster, so Windows works; one WhiteboxTools core per worker).
* `ak_layers()`: the catalog of layers and variants (default and
  alternates, kind, extent, access, licence, cache folder), built from the
  same table as `ak_sources()`.
* `ak_add()` and `ak_add_timeseries()` / `ak_timeseries()`: add a layer to
  any catchments by name, through the existing functions; `ak_provenance()`
  records product, citation, licence, download date (from the cache
  manifest), arguments and columns for every layer; `ak_write()` writes it
  all to one GeoPackage. `ak_add(x, "fire")` gives a static fire summary.
* `ak_prefetch()`: download what a set of layers needs up front, layer by
  layer, reporting manual downloads and failures.
* Observations: `ak_obs_schema()`, `ak_obs_validate()`, `ak_obs_read()`
  (map your own tables, wide or long, into the schema), `ak_obs_stations()`,
  `ak_obs_link()` (stations at the outlet, inside with their share of the
  catchment, or near), `ak_obs_temperature_stations()` /
  `ak_obs_temperature()` (AKTEMP-VIZ in the schema; the old
  `ak_temperature_*()` keep their columns), and `ak_obs_usgs_stations()` /
  `ak_obs_usgs_daily()` (USGS Water Data APIs through dataRetrieval's
  `read_waterdata_*`).
* `ak_snowoff_modis(keep_full = TRUE)` keeps the statewide annual files, for
  shared caches.
* `ak_topography()` and `ak_outlet_slope()` use the rasters carried by
  `ak_catchments()` output; `ak_outlet_slope()` takes its points from the
  catchments' `lat`/`lon` when `points` is not given.

### Fixes

* Fire perimeters: the cache (`fire_history_ak.gpkg`) records the area it
  covers; catchments outside it trigger one query over both areas, instead
  of silently reusing perimeters for another area. A geo-workflow file
  without that record counts for the box its perimeters span.
* `ak_condition_catchments()`: non-default `res` or `buffer_m` get their own
  files, instead of reusing a DEM built with other settings. Derived rasters
  are rebuilt whenever the DEM is.
* `ak_delineate()` keeps its rasters (DEM, breached DEM, D8 pointer and
  accumulation) in a folder named by DEM and extent, so runs with another
  DEM no longer overwrite them; they are cropped to the catchment plus a
  margin and compressed, which also makes them far smaller.
* `ak_delineate()`: a catchment that runs into missing elevation data (the
  edge of IfSAR coverage at the Canadian border, voids) is flagged
  `nodata_edge` (a failure) instead of widening the window to its maximum
  and being flagged `truncated`; only the window's outer edge counts as
  truncation. A point with no elevation data stops with a clear message.
* Copernicus GLO-30 windows that cross 50, 60, 70 or 80 N (where tile
  longitude spacing changes) are merged correctly.
* Downloads that come back as a web page (moved link, login page) are
  refused with the page's text instead of being cached as data. HTTP and
  connection errors name the URL.
* Archives are unpacked through a temporary folder, so an interrupted unpack
  never looks complete. `/vsizip/` paths and the cache root use forward
  slashes (Windows).
* Statewide glacier and geology maps are read only around the catchments.
* `ak_positional_uncertainty()` intersects all shifted copies at once:
  identical results, about 25 times faster (permafrost, glaciers).
* `ak_outlet_slope()` reads only the outlet's neighbourhood, not the whole
  DEM window, into memory.
* Invalid (self-intersecting) polygons are repaired on input; a polygon set
  without a CRS gets a clear error.
* Imagery plots choose a tile zoom when none is given (older maptiles
  versions require one); when imagery or the hillshade fallback fails, the
  reason is reported. `ak_soils()` reports why an SDA query failed.
* Sample dates given as text are accepted by the weather, climate and fire
  window functions.

# AKCatchments 0.1.0

First version.

* Live tests: `inst/live-tests/ak_live_tests.R` runs every downloading
  function against the real services and writes a report.
* Downloads are written to a per-process `.part` file that is removed if the
  download fails, so failed and parallel downloads leave nothing behind.
* `ak_dem("copernicus")` reports why no tile could be read instead of only
  "no tiles found".
* Delineation from a latitude and longitude (`ak_delineate()`,
  `ak_delineate_many()`) on IfSAR 5 m, Copernicus GLO-30, local or elevatr
  DEMs, with automatic checks (`ak_flag_delineation()`) and a satellite
  imagery check plot (`ak_plot_catchment()`).
* Reference flowlines from 3DHP and NHD (web services), TDX-Hydro,
  MERIT-Basins and HydroRIVERS (downloaded once by `ak_download_flowlines()`),
  or a local file (`ak_flowlines()`).
* Validation against hand-drawn catchments and USGS StreamStats
  (`ak_validate()`, `ak_compare_catchments()`, `ak_streamstats()`).
* Catchment attributes with uncertainty columns: topography, alder, land
  cover, permafrost, geology, soils, fire, Daymet, MODIS snow-off, and
  AKTEMP-VIZ stream temperature, PRISM 1981-2010 Alaska normals, Pastick et
  al. 2015 permafrost probability and RGI glaciers. Every layer has a
  `_flag` column; simulations default to 500 draws. Source registry in
  `ak_sources()`.
* The AKSynthesis_RawGeoData workflow's calculations, ported:
  - `ak_condition_catchments()` and `ak_outlet_slope()`: D8 upstream walk and
    stream power;
  - full `ak_topography()`;
  - `ak_melt_daymet()`, `ak_weather_anomalies()` and `ak_climate_summaries()`;
  - `ak_snowoff()`: MODIS snow-off with an empirical-Bayes Daymet fill;
  - `ak_fire_windows()`;
  - equal-allocation lithology classes in `ak_geology()`.
* `ak_polygon_spread()` for delineation error across alternative polygons;
  `ak_gaged_basins()` for the USGS Alaska gaged-basin reference.
* One download cache, settable with `ak_set_cache()` (or `AKCATCHMENTS_CACHE`),
  shareable through a synced folder, with a download manifest
  (`ak_cache_info()`). Its layout matches the AKSynthesis_RawGeoData
  workflow, so existing downloads are reused.
