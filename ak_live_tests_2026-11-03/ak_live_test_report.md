# AKCatchments live test report

Paste this whole file into the chat with Claude.

**46 PASS, 4 WARN, 5 FAIL, 3 SKIP** (of 58 tests run or skipped)

- Run: 2026-11-03 16:31, level **full**, 88.1 mins elapsed
- R: R version 4.6.1 (2026-06-24 ucrt) on Windows 11 x64 (build 26200) (x86_64-w64-mingw32)
- AKCatchments loaded from: source folder D:/GitHub_Repos/AKRivers/AKCatchments
- Cache: `D:/AKCatchments_test_cache`
- GDAL/GEOS/PROJ: GEOS 3.14.1, GDAL 3.12.1, proj.4 9.7.1, GDAL_with_GEOS true, USE_PROJ_H true, PROJ 9.7.1
- WhiteboxTools: WhiteboxTools v2.4.0 (c) Dr. John Lindsay 2017-2023 at "C:\Users\Caleb_Robbins\AppData\Roaming/R/data/R/whitebox/WBT/whi
- Packages: AKCatchments 0.1.0.9000, sf 1.1.1, terra 1.9.46, whitebox 2.4.3, httr2 1.2.3, exactextractr 0.10.1, dplyr 1.2.1, purrr 1.2.2, tidyr 1.3.2, ggplot2 4.0.3, maptiles 0.12.0, jsonlite 2.0.0, daymetr 1.7.1, soilDB 2.9.2, archive 1.1.14, elevatr 0.99.1, testthat 3.3.2
- Gage coordinates used: 61.70974, -149.23196 (USGS Water Data API (NAD83))

## Summary

| ID | Test | Status | Time | Result / reason |
|---|---|---|---|---|
| A01 | Install missing dependencies and load AKCatchments | PASS | 2 s | AKCatchments 0.1.0.9000 from source folder D:/GitHub_Repos/AKRivers/AKCatchments |
| A02 | Set the download cache | PASS | 0 s | cache D:/AKCatchments_test_cache (14 sub-folders already present) |
| A03 | WhiteboxTools binary installed and working | PASS | 1 s | WhiteboxTools v2.4.0 (c) Dr. John Lindsay 2017-2023 at "C:\Users\Caleb_Robbins\AppData\Roaming/R/data/R/whitebox/WBT/whitebox_tools.exe" |
| A05 | USGS Water Data API: gage 15290000 location and drainage area | PASS | 0 s | 61.70974, -149.23196 (295 m from the HCDN coordinates); drainage area field drainage_area/contributing_drainage_area = 62 mi2 |
| A06 | Find CAKN-Data sites and hand-drawn catchments | PASS | 0 s | D:/GitHub_Repos/AKRivers/CAKN-Data/data: picked WRST-065, WRST-095, WRST-GRTS-004, WRST-GRTS-375, WRST-GRTS-302 (NPS areas 0.11, 3.38, 8.84, 16.90, 28.55 km2) |
| B01 | ak_dem('ifsar'): DGGS IfSAR 5 m image service, 2 km window at the gage | PASS | 0 s | 401 x 401 cells, 251-449 m; 286 m at the gage (gage datum 280 m); tag dem_source |
| B02 | ak_dem('copernicus'): Copernicus GLO-30 COGs on AWS; bias against IfSAR | PASS | 4 s | 68 x 68 cells, 252-447 m; Copernicus - IfSAR: mean 1.4 m, RMS 2.4 m |
| B03 | ak_dem('elevatr'): AWS terrain tiles | PASS | 2 s | 112 x 112 cells at 18 m, 250-446 m |
| B04 | ak_dem('local'): your DEM tiles in the cache | SKIP |  | no .tif tiles in the cache's dem folder (normal for the test cache) |
| B05 | ak_dem('copernicus') across 60 N, where GLO-30 tiles change longitude spacing | PASS | 4 s | 101 x 101 cells, 638-1198 m, no NA rows |
| C01 | ak_flowlines('3dhp'): 3D Hydrography Program flowlines (ArcGIS service) | PASS | 3 s | 343 features, 153.0 km |
| C02 | ak_flowlines('nhd'): NHD flowlines (ArcGIS service) | PASS | 2 s | 343 features, 153.0 km |
| C03 | ak_flowline_sources() and ak_download_flowlines(download = FALSE) plans | PASS | 11 s | tdx: 8020000010; merit: 81; hydrorivers: ar |
| C04 | ak_download_flowlines('tdx') then ak_flowlines('tdx') | FAIL | 46 s | ERROR: [38;5;252m[36m<U+2139>[38;5;252m In index: 1.[39m [1mCaused by error:[22m [33m![39m Could not reach https://earth-info.nga.mil/php/download.php?file=8020000010-streamnet-gpkg [1mCaused by error in `httr2::req_perform()`:[22m [33m![39m Failed to perform HTTP request. [1mCaused by error in `curl::curl_fetch_disk()`:[22m [33m![39m Timeout was reached [earth-info.nga.mil]: Failed to co |
| C05 | ak_download_flowlines('merit') then ak_flowlines('merit') | PASS | 7 s | 1 features, 74.5 km; files: rivers81.db (113 MB) |
| C06 | ak_download_flowlines('hydrorivers') then ak_flowlines('hydrorivers') | PASS | 7 s | 4 features, 28.8 km; files: HydroRIVERS_v10_ar.gdb.zip (25 MB) |
| D01 | ak_delineate(): USGS 15290000 blind (no reference area), IfSAR; area vs published 160.3 km2 | PASS | 884 s | area 160.5 km2 vs published 160.3 (ratio 1.00); snapped 296 m (largest); DEM ifsar_6.67m; window 20 km; delin_ok TRUE; flags: snap_at_limit;flowline_crossings |
| D02 | Check plot on satellite imagery (written by ak_delineate and by ak_plot_catchment) | PASS | 6 s | USGS-15290000.png (1488 KB), D02_ak_plot_catchment.png (1437 KB): open them to check the outline |
| D03 | ak_delineate(): same gage from Copernicus GLO-30; DEM sensitivity (IoU against IfSAR) | PASS | 44 s | Copernicus area 160.6 km2; IoU with IfSAR 0.99, Hausdorff 206 m |
| D04 | ak_delineate(): CAKN sites from IfSAR, guided by the NPS area | PASS | 1697 s | WRST-065 0.11 km2 (NPS 0.11, ratio 1.00, flags: snap_ambiguous;flowline_crossings); WRST-095 3.36 km2 (NPS 3.38, ratio 0.99, flags: snap_at_limit); WRST-GRTS-004 8.61 km2 (NPS 8.84, ratio 0.97, flags: snap_at_limit;snap_ambiguous); WRST-GRTS-375 16.24 km2 (NPS 16.90, ratio 0.96, flags: snap_at_limit); WRST-GRTS-302 26.36 km2 (NPS 28.55, ratio 0.92, flags: snap_at_limit) |
| D05 | ak_catchments(): gage and CAKN sites in 2 parallel workers; plots and kept rasters | PASS | 1858 s | 6 sites; 6 delin_ok; kept rasters 125 MB in all; gage IoU with D01 1.000 |
| D06 | ak_add(): topography, outlet slope and fire from the catalog; provenance; ak_write() | PASS | 28 s | 78 columns; burned % 0.0/0.0/0.0/0.0/0.0/0.0; provenance 4 rows; D06_catchments.gpkg |
| E01 | ak_gaged_basins(): USGS gaged-basin boundaries (Curran 2024); find 15290000 | PASS | 1 s | 253 basins statewide; id column UniqSiteID; 15290000 polygon 160.5 km2 (published 160.3); columns: UniqSiteID, SiteNumber, StationNm, SpecConds, AreaSqMi, AreaSqKm, EditYear, OrigPub |
| E02 | ak_streamstats(): SS-Delineate at 15290000 (Cook Inlet region) | PASS | 16 s | area 160.5 km2 (ratio 1.00 to published); snapped 60 m; warning: NA |
| E03 | ak_compare_catchments(): delineation vs gaged basin and StreamStats | PASS | 1 s | gaged_basin: IoU 0.99, area ratio 1.00, Hausdorff 265 m; streamstats: IoU 0.99, area ratio 1.00, Hausdorff 312 m |
| E04 | ak_validate(): both pathways at the gage (gaged basin as the 'hand' reference, plus StreamStats) | PASS | 5 s | auto_vs_hand (all): n 1, IoU median 0.99, area ratio median 1.00, agree 100%; auto_vs_hand (delin_ok): n 1, IoU median 0.99, area ratio median 1.00, agree 100%; auto_vs_streamstats (all): n 1, IoU median 0.99, area ratio median 1.00, agree 100%; auto_vs_streamstats (delin_ok): n 1, IoU median 0.99, area ratio median 1.00, agree 100%; hand_vs_streamstats (all): n 1, IoU median 1.00, area ratio medi |
| E05 | ak_validate(): CAKN sites against your hand-drawn polygons | WARN | 14 s | WRST-065 IoU 0.59; WRST-095 IoU 0.90; WRST-GRTS-004 IoU 0.96; WRST-GRTS-375 IoU 0.93; WRST-GRTS-302 IoU 0.86 NOTE: IoU < 0.7 at WRST-065 |
| S01 | Build the test catchment set (delineated, else reference polygon, else a 2 km circle) | PASS | 0 s | USGS-15290000 160.46 km2, WRST-065 0.11 km2, WRST-095 3.36 km2, WRST-GRTS-004 8.61 km2, WRST-GRTS-375 16.24 km2, WRST-GRTS-302 26.36 km2; smallest: WRST-065 |
| F01 | ak_condition_catchments(): IfSAR window, breach, D8 (smallest test catchment) | PASS | 0 s | WRST-065 at 5 m: dem_ifsar.tif, d8_accum_ifsar.tif |
| F02 | ak_outlet_slope(): 100 and 200 m reaches | PASS | 0 s | slope 0.1338/0.1764 m/m at 100/200 m; contrib 0.11 km2 vs catchment 0.11 km2 |
| F03 | ak_topography(): topo_* attributes | PASS | 1 s | 23 columns; elev mean 1192 m, slope mean 32.1 deg, fws 38.4%, flag: none |
| G01 | ak_daymet(): Daymet V4 pixels, 2019-2020, smallest catchment | PASS | 0 s | 730 days; 3 pixels; mean July tmax 13.6 C; annual prcp 531 mm; peak SWE 115 mm; flag: none |
| G02 | ak_melt_daymet(): snowmelt day from Daymet SWE | PASS | 0 s | melt day 139/155 in 2019/2020 |
| G03 | ak_weather_anomalies() and ak_climate_summaries() for a sample on 2020-07-15 | PASS | 0 s | anomalies: 18 columns (site_code, sample_date, t_anom7, t_anom30, t_anom7_dt, t_anom30_dt ...); summaries: 11 columns (site_code, sample_date, year, doy, gdd_base0, gdd_base5 ...) |
| G04 | ak_prism(): PRISM 1981-2010 Alaska normals | PASS | 127 s | USGS-15290000: July tmean 8.6 C, annual ppt 1036 mm; WRST-065: July tmean 9.0 C, annual ppt 832 mm; WRST-095: July tmean 6.7 C, annual ppt 1415 mm; WRST-GRTS-004: July tmean 6.5 C, annual ppt 490 mm; WRST-GRTS-375: July tmean 8.1 C, annual ppt 847 mm; WRST-GRTS-302: July tmean 7.9 C, annual ppt 548 mm |
| G05 | ak_snowoff_modis() and ak_snowoff(): MODIS snow-off with the Daymet offset | PASS | 0 s | MODIS doy 138.260616226407/138.892027438386; ak_snowoff columns: site_code, year, daymet_doy, modis_valid_frac, modis_doy, modis_cloud_days, snowoff_rmse_d, snowoff_flag, snowoff_offset, snowoff_doy, snowoff_source, snowoff_se_d |
| H01 | ak_fire() and ak_fire_windows(): AICC fire perimeters (cache records its area) | PASS | 7 s | 0 burned site-years; hist burn frac 0.000/0.000/0.000/0.000/0.000/0.000 |
| H02 | ak_soils(): NRCS SSURGO/STATSGO via Soil Data Access | WARN | 1 s | 2 map units, 6 components; columns: site_code, soil_om_pct, soil_clay_pct, soil_sand_pct, soil_ksat, soil_bulkdens, soil_gelisol_pct, soil_histosol_pct, soil_poorly_drained_pct, soil_om_pct_lo NOTE: soil_flag: assumed_concentration |
| H03 | ak_geology(): USGS SIM 3340 geologic map | FAIL | 0 s | ERROR: Query execution failed, cannot open layer. |
| H04 | ak_landcover(): NLCD 2016 Alaska | PASS | 1 s | 17 columns; coverage 100.0/100.0/100.0/100.0/100.0/100.0% |
| H05 | ak_alder(): ACCS Alnus foliar cover | FAIL | 299 s | ERROR: [38;5;252m[36m<U+2139>[38;5;252m In argument: `alnus_cover_flag = flag_text(partial_coverage = .data$alnus_coverage_pct < 95, error_exceeds_cover = !ci$feasible)`.[39m [1mCaused by error in `.data$alnus_coverage_pct`:[22m [38;5;252m[33m![38;5;252m Column `alnus_coverage_pct` not found in `.data`.[39m |
| H06 | ak_permafrost_pastick(): Pastick et al. 2015 permafrost probability | FAIL | 94 s | ERROR: No raster found in D:/AKCatchments_test_cache/pastick_2015; it holds: AK_PFProbability_1m.tar |
| H07 | ak_permafrost(): Jorgenson et al. 2008 (manual download) | SKIP |  | no Jorgenson shapefile in the cache's jorgenson_2008 folder (manual download) |
| H08 | ak_glaciers(): RGI 7 region 01 (manual download) | SKIP |  | no RGI shapefile in the cache's rgi folder (manual download from NSIDC) |
| I01 | ak_temperature_stations(): AKTEMP-VIZ station list | PASS | 0 s | 778 stations from USGS, NPS, AKTEMP; nearest to the gage: USGS / USGS:15290000, 0.0 km |
| I02 | ak_temperature_daily(): daily water temperature at the nearest station | PASS | 0 s | 4131 days, 2014-10-11 to 2026-11-01; water 0.0-13.1 C |
| I03 | ak_obs_temperature_stations(), ak_obs_temperature() and ak_obs_link() | PASS | 1 s | 778 stations; 8251 observations at 15290000 (water_temperature/air_temperature_era5land); 7 station-catchment links (inside 7) |
| I04 | ak_obs_usgs_stations() and ak_obs_usgs_daily(): dataRetrieval read_waterdata_*, near the gage | PASS | 1 s | 4 stations; June 2023 mean discharge 933 ft^3/s; qualifiers Approved |
| P01 | Probe: USGS Water Data API daily discharge (OGC API 'daily'), 15290000, June 2023 | PASS | 0 s | 10 daily values; mean 1142 cfs; property fields: time_series_id, monitoring_location_id, parameter_code, statistic_id, time, value, unit_of_measure, approval_status, qualifier, last_modified |
| P02 | Probe: Water Quality Portal stations near the gage | PASS | 2 s | 2976 stations; providers NWIS/STORET |
| P03 | Probe: NRCS AWDB REST API, Alaska SNOTEL stations | PASS | 0 s | 78 stations; fields: stationTriplet, stationId, stateCode, networkCode, name, dcoCode, countyName, huc, elevation, latitude |
| P04 | Probe: ACIS (GHCN-Daily and coop) stations near the gage | PASS | 1 s | 83 stations with daily maxt within ~1 degree |
| P05 | Probe: ESA WorldCover 2021 10 m COG (AWS), window at the gage | PASS | 3 s | class counts: 10=552098, 20=98, 30=302260, 50=3789, 60=629, 70=2, 80=137, 90=1151, 100=3836 |
| P06 | Probe: ERA5-Land daily through the Open-Meteo archive API (one week) | PASS | 1 s | tmean 14.4/15.9/17.2/19/18.1/15/14.7 C; grid cell elevation 301 m |
| Z01 | ak_cache_info() and ak_cache_layout(): manifest and folders | WARN | 0 s | 88 downloads logged; 26.89 GB in 14 folders NOTE: 1 leftover .part files (interrupted downloads): flowlines/tdx/TDX_streamnet_8020000010_01.gpkg.87088.part |
| Z03 | ak_prefetch(): fire, soils and the manual layers for the test catchments | PASS | 3 s | fire: ok; soils: not needed; permafrost: manual; glaciers: manual |
| Z04 | Your shared cache's old fire perimeter file: what area it covers (read only) | WARN | 0 s | layers perimeters/coverage; 124 perimeters, 1942-2025; lon -149.58 to -141.37, lat 60.91 to 62.76; coverage layer: TRUE NOTE: not statewide: catchments outside lon -149.58 to -141.37, lat 60.91 to 62.76 trigger a new query |
| Z02 | Package unit tests (offline testthat suite) | FAIL | 96 s | ERROR: 2 failing tests: test-robustness.R > a fallback DEM is used when the first one has a hole, and the result says so; test-validate.R > ak_validate compares with hand-drawn and StreamStats references and plots both |

## Failures in detail

### C04 ak_download_flowlines('tdx') then ak_flowlines('tdx')

Error:
```text
[38;5;252m[36m<U+2139>[38;5;252m In index: 1.[39m
[1mCaused by error:[22m
[33m![39m Could not reach https://earth-info.nga.mil/php/download.php?file=8020000010-streamnet-gpkg
[1mCaused by error in `httr2::req_perform()`:[22m
[33m![39m Failed to perform HTTP request.
[1mCaused by error in `curl::curl_fetch_disk()`:[22m
[33m![39m Timeout was reached [earth-info.nga.mil]:
Failed to connect to earth-info.nga.mil port 443 after 42125 ms: Could not connect to server
```
Condition class: `purrr_error_indexed, rlang_error, error, condition`

Call stack (innermost last):
```text
ak_download_flowlines(aoi, FL_SRC)
purrr::walk2(todo$url, todo$file, function(u, f) {
map2(.x, .y, .f, ..., .progress = .progress)
map2_("list", .x, .y, .f, ..., .progress = .progress)
with_indexed_errors(i = i, names = names, error_call = .purrr_error_call, call_with_cleanup(map2_impl, environment(), .type, .progress, n, names, i))
call_with_cleanup(map2_impl, environment(), .type, .progress, n, names, i)
.f(.x[[i]], .y[[i]], ...)
ak_download(u, f)
value[[3L]](cond)
stop(e)
(function (cnd) 
cli::cli_abort(message, location = i, name = name, parent = cnd, call = error_call, class = "purrr_error_indexed")
rlang::abort(message, ..., call = call, use_cli_format = TRUE, .frame = .frame)
signal_abort(cnd, .file)
```

Last messages:
```text
Downloading 1 tdx file(s) to D:/AKCatchments_test_cache/flowlines; large files take a while
```

Test code:
```r
{
    aoi <- pt_buffer(GAGE, 3000)
    plan <- ak_download_flowlines(aoi, FL_SRC)
    expect(all(plan$exists), "not downloaded: ", paste(plan$url[!plan$exists], collapse = ", "))
    fl <- ak_flowlines(aoi, FL_SRC)
    expect(nrow(fl) > 0, "no ", FL_SRC, " flowlines within 3 km of the gage")
    sprintf("%d features, %.1f km; files: %s (%.0f MB)", nrow(fl), sum(as.numeric(sf::st_length(fl)))/1000, 
        paste(basename(plan$file), collapse = ", "), sum(file.size(plan$file))/1000000)
}
```

### H03 ak_geology(): USGS SIM 3340 geologic map

Error:
```text
Query execution failed, cannot open layer.

```
Condition class: `Rcpp::exception, C++Error, error, condition`

Call stack (innermost last):
```text
ak_geology(got("catch"))
sf::st_filter(sf::st_transform(sf::st_make_valid(dplyr::transmute(read("AKStategeol_poly", wkt_filter = bbox_wkt(sf::st_union(sf::st_geometry(x)), gdb[1], 
sf::st_transform(sf::st_make_valid(dplyr::transmute(read("AKStategeol_poly", wkt_filter = bbox_wkt(sf::st_union(sf::st_geometry(x)), gdb[1], poly_lyr)), 
sf::st_make_valid(dplyr::transmute(read("AKStategeol_poly", wkt_filter = bbox_wkt(sf::st_union(sf::st_geometry(x)), gdb[1], poly_lyr)), NSACLASS = as.character(
dplyr::transmute(read("AKStategeol_poly", wkt_filter = bbox_wkt(sf::st_union(sf::st_geometry(x)), gdb[1], poly_lyr)), NSACLASS = as.character(.data$NSACLASS))
read("AKStategeol_poly", wkt_filter = bbox_wkt(sf::st_union(sf::st_geometry(x)), gdb[1], poly_lyr))
dplyr::rename_with(suppressWarnings(sf::st_read(gdb[1], layer = lyr[tolower(lyr) == tolower(n)][1], quiet = TRUE, ...)), toupper, -dplyr::any_of(c("Shape", 
suppressWarnings(sf::st_read(gdb[1], layer = lyr[tolower(lyr) == tolower(n)][1], quiet = TRUE, ...))
sf::st_read(gdb[1], layer = lyr[tolower(lyr) == tolower(n)][1], quiet = TRUE, ...)
st_read.character(gdb[1], layer = lyr[tolower(lyr) == tolower(n)][1], quiet = TRUE, ...)
CPL_read_ogr(dsn, layer, query, as.character(options), quiet, type, fid_column_name, drivers, wkt_filter, promote_to_multi, int64_as_string, dsn_exists, 
bbox_wkt(sf::st_union(sf::st_geometry(x)), gdb[1], poly_lyr)
sf::st_crs(sf::st_read(dsn, query = sprintf("SELECT * FROM \"%s\" LIMIT 1", layer), quiet = TRUE))
sf::st_read(dsn, query = sprintf("SELECT * FROM \"%s\" LIMIT 1", layer), quiet = TRUE)
st_read.character(dsn, query = sprintf("SELECT * FROM \"%s\" LIMIT 1", layer), quiet = TRUE)
CPL_read_ogr(dsn, layer, query, as.character(options), quiet, type, fid_column_name, drivers, wkt_filter, promote_to_multi, int64_as_string, dsn_exists, 
stop(structure(list(message = "Query execution failed, cannot open layer.\n", call = eval(ei, envir), cppstack = NULL), class = c("Rcpp::exception", "C++Error",
```

Test code:
```r
{
    ge <- ak_geology(got("catch"))
    expect(nrow(ge$occurrence) == nrow(got("catch")), "occurrence rows ", nrow(ge$occurrence))
    sprintf("%d columns; flags: %s", ncol(ge$occurrence), paste(unique(ge$occurrence$geol_flag), collapse = "/"))
}
```

### H05 ak_alder(): ACCS Alnus foliar cover

Error:
```text
[38;5;252m[36m<U+2139>[38;5;252m In argument: `alnus_cover_flag = flag_text(partial_coverage =
  .data$alnus_coverage_pct < 95, error_exceeds_cover = !ci$feasible)`.[39m
[1mCaused by error in `.data$alnus_coverage_pct`:[22m
[38;5;252m[33m![38;5;252m Column `alnus_coverage_pct` not found in `.data`.[39m
```
Condition class: `dplyr:::mutate_error, rlang_error, error, condition`

Call stack (innermost last):
```text
ak_alder(got("catch"))
dplyr::mutate(out, alnus_cover_se = se, alnus_cover_lo95 = ci$lo, alnus_cover_hi95 = ci$hi, alnus_cover_flag = flag_text(partial_coverage = .data$alnus_coverage
mutate.data.frame(out, alnus_cover_se = se, alnus_cover_lo95 = ci$lo, alnus_cover_hi95 = ci$hi, alnus_cover_flag = flag_text(partial_coverage = .data$alnus_cove
mutate_cols(.data, dplyr_quosures(...), by)
mutate_col(dots[[i]], data, mask, new_columns)
mask$eval_all_mutate(quo)
eval()
flag_text(partial_coverage = .data$alnus_coverage_pct < 95, error_exceeds_cover = !ci$feasible)
.data$alnus_coverage_pct
`$.rlang_data_pronoun`(.data, alnus_coverage_pct)
data_pronoun_get(x, nm, call = I(call("$", quote(.data), sym(nm))))
abort_data_pronoun(x, call = y)
abort(msg, "rlang_error_data_pronoun_not_found", call = call)
signal_abort(cnd, .file)
(function (cnd) 
abort(message, class = error_class, parent = parent, call = error_call)
signal_abort(cnd, .file)
```

Last HTTP response during this test:
```text
GET https://zenodo.org/records/4770218/files/alnus.zip?download=1
-> HTTP 200, content-type application/octet-stream
<body not text, or saved to a file>
```

Test code:
```r
{
    al <- ak_alder(got("catch"))
    expect(all(al$alnus_cover >= 0 & al$alnus_cover <= 100, na.rm = TRUE), "cover outside 0-100")
    sprintf("alder cover %s%% (coverage %s%%)", paste(fmt(al$alnus_cover), collapse = "/"), paste(fmt(al$alnus_coverage_pct), 
        collapse = "/"))
}
```

### H06 ak_permafrost_pastick(): Pastick et al. 2015 permafrost probability

Error:
```text
No raster found in D:/AKCatchments_test_cache/pastick_2015; it holds: AK_PFProbability_1m.tar
```
Condition class: `simpleError, error, condition`

Call stack (innermost last):
```text
ak_permafrost_pastick(got("catch"))
pastick_raster()
stop("No raster found in ", dir, "; it holds: ", paste(utils::head(list.files(dir, recursive = TRUE), 20), collapse = ", "), call. = FALSE)
```

Last messages:
```text
<U+283C> 0 extracted | 5.8 GB (111 MB/s) | 52.7s
<U+2834> 0 extracted | 5.9 GB (111 MB/s) | 52.9s
<U+2826> 0 extracted | 5.9 GB (111 MB/s) | 53.1s
<U+2827> 0 extracted | 5.9 GB (110 MB/s) | 53.3s
<U+2807> 0 extracted | 5.9 GB (110 MB/s) | 53.5s
<U+280F> 0 extracted | 5.9 GB (110 MB/s) | 53.7s
<U+280B> 0 extracted | 5.9 GB (110 MB/s) | 53.9s
<U+2819> 0 extracted | 6.0 GB (110 MB/s) | 54.1s
<U+2839> 0 extracted | 6.1 GB (111 MB/s) | 54.3s
<U+2838> 0 extracted | 6.1 GB (111 MB/s) | 54.5s
<U+283C> 0 extracted | 6.1 GB (111 MB/s) | 54.8s
<U+2834> 0 extracted | 6.1 GB (111 MB/s) | 55s
```

Test code:
```r
{
    pp <- ak_permafrost_pastick(got("catch"))
    expect(all(pp$pf_prob_mean >= 0 & pp$pf_prob_mean <= 100, na.rm = TRUE), "probability outside 0-100")
    sprintf("pf_prob_mean %s; flags: %s", paste(fmt(pp$pf_prob_mean), collapse = "/"), paste(unique(pp$pf_flag), 
        collapse = "/"))
}
```

### Z02 Package unit tests (offline testthat suite)

Error:
```text
2 failing tests: test-robustness.R > a fallback DEM is used when the first one has a hole, and the result says so; test-validate.R > ak_validate compares with hand-drawn and StreamStats references and plots both
```
Condition class: `simpleError, error, condition`

Call stack (innermost last):
```text
expect(!nrow(bad), nrow(bad), " failing tests: ", paste(bad$file, bad$test, sep = " > ", collapse = "; "))
stop(paste0(...), call. = FALSE)
```

Last messages:
```text
SYN-1: catchment reached the window edge; widening to 10 km
B: The DEM tiles in C:\Users\CALEB_~1\AppData\Local\Temp\RtmpG6MG9N/dem_synth do not cover this area (1 tiles)
```

Test code:
```r
{
    cache_now <- ak_cache_dir()
    res <- tryCatch(as.data.frame(testthat::test_local(AK_PKG_PATH, reporter = "silent", stop_on_failure = FALSE)), 
        finally = ak_set_cache(cache_now))
    bad <- res[res$failed > 0 | res$error, ]
    expect(!nrow(bad), nrow(bad), " failing tests: ", paste(bad$file, bad$test, sep = " > ", collapse = "; "))
    sprintf("%d tests, %d expectations, %d skipped", nrow(res), sum(res$nb), sum(res$skipped))
}
```


## Notes and R warnings from tests that passed

**E05 ak_validate(): CAKN sites against your hand-drawn polygons** (WARN)
- NOTE: IoU < 0.7 at WRST-065

**H02 ak_soils(): NRCS SSURGO/STATSGO via Soil Data Access** (WARN)
- NOTE: soil_flag: assumed_concentration

**Z01 ak_cache_info() and ak_cache_layout(): manifest and folders** (WARN)
- NOTE: 1 leftover .part files (interrupted downloads): flowlines/tdx/TDX_streamnet_8020000010_01.gpkg.87088.part

**Z04 Your shared cache's old fire perimeter file: what area it covers (read only)** (WARN)
- NOTE: not statewide: catchments outside lon -149.58 to -141.37, lat 60.91 to 62.76 trigger a new query

