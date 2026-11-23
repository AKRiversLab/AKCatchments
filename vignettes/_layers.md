Every layer function takes **catchments** as its first argument: sf
polygons with a `site_code` column, one row per catchment, in any CRS. That
can be the output of `ak_catchments()` or `import_catchments()`, a single
`ak_delineate()` result, a list of them, or your own polygons (hand-drawn,
StreamStats, gaged basins). Each function crops its dataset to the
catchments. What it returns depends on the dataset (the `returns` column of
`ak_layers()`):

- **one value per site** (columns): the catchments you passed in, with the
  layer's columns (and their `_se`, `_lo95`, `_hi95`, `_flag`) appended and
  a provenance row added, so layers chain with `|>`;
- **a long table** (by day, year, model or sample): a table with its own
  provenance; summarise it to one row per site with its companion function
  and add that with `catchment_join()`, or write it alongside the
  catchments with `ak_write(x, file, name = table)`.

`ak_layers()` lists the layers and their sources (`source` argument);
`ak_layers(details = TRUE)` adds citations, published accuracy and how the
uncertainty columns are computed. `ak_provenance(x)` shows where every
column on `x` came from.

| theme | dataset | function (`source`) | returns | vignette |
|---|---|---|---|---|
| Terrain | delineation DEM (IfSAR 5 m, Copernicus 30 m) | `ak_topography()`, `ak_outlet_slope()` | columns | `vignette("topography")` |
| Vegetation | ACCS *Alnus* foliar cover | `ak_alder()` (`"accs2021"`) | columns | `vignette("alder-accs")` |
| Vegetation | NPS Wrangell-St. Elias land cover (alder) | `ak_alder(source = "nps_wrst")` | columns | `vignette("alder-wrst")` |
| Vegetation | NLCD 2016 Alaska | `ak_landcover()` (`"nlcd2016"`) | columns | `vignette("landcover-nlcd")` |
| Vegetation | Landsat annual peak NDVI | `ak_peak_ndvi()`, then `peak_ndvi_summary()` | table, then one row per site | `vignette("greenness-landsat")` |
| Permafrost and ice | Jorgenson et al. 2008 extent and ground ice | `ak_permafrost()` (`"jorgenson2008"`) | columns | `vignette("permafrost-jorgenson")` |
| Permafrost and ice | Pastick et al. 2015 near-surface probability | `ak_permafrost(source = "pastick2015")` | columns | `vignette("permafrost-pastick")` |
| Permafrost and ice | Randolph Glacier Inventory 7.0 | `ak_glaciers()` | columns | `vignette("glaciers-rgi")` |
| Geology and soils | USGS SIM 3340 geologic map | `ak_geology()` | columns | `vignette("geology-sim3340")` |
| Geology and soils | Rock N, S and major oxides (Olson & Hawkins lithology geochemistry on SIM 3340) | `ak_rock_chem()` (`"olson2014"`) | columns | `vignette("rock-chemistry")` |
| Geology and soils | STATSGO2 (Soil Data Access) | `ak_soils()` | columns | `vignette("soils-statsgo")` |
| Disturbance | BLM AFS fire perimeters | `ak_fire()`; `by_year = TRUE` or `samples =` for tables | columns or table | `vignette("fire-afs")` |
| Climate and weather | PRISM 1981-2010 normals | `ak_climate_normals()` (`"prism8110"`) | columns | `vignette("climate-prism")` |
| Climate and weather | Daymet V4 daily weather | `ak_weather()`; then `weather_at_samples()` (anomalies, melt, degree days) and `ak_climate_normals(source = "daymet4")` | table; table; columns | `vignette("weather-daymet")` |
| Climate and weather | SNAP downscaled CMIP6 projections | `ak_climate_projection()` (`"snap_cmip6_4km"`), then `projection_ensemble()` | table, then one row per site (`wide = TRUE`) | `vignette("projections-snap")` |
| Snow | MODIS snow-off, filled from Daymet | `ak_snowoff()` | table | `vignette("snowoff-modis")` |
| Observations | AKTEMP-VIZ stream temperature, USGS daily values, your own data | `ak_obs_*()` | linked, not cropped | `vignette("observations")` |

Your own rasters and maps: `catchment_mean()` and
`catchment_class_shares()` append their columns the same way, with a
provenance row naming the file. A full run for many sites, with the
parallel options, is in `vignette("workflow")`. How the uncertainty
columns are built in general (aggregating map error, propagation into
observations, what it does downstream) is in `vignette("uncertainty")`;
how catchments are delineated and validated is in `vignette("delineation")`.
