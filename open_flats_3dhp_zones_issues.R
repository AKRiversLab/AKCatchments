# Opens four more issues on AKRiversLab/AKCatchments: flat wetland divides, the new 3DHP service, 3DHP catchments as a
# delineation method and check, and catchment zones (stream buffers, near-outlet areas). Needs a GitHub token with write
# access (gitcreds::gitcreds_set()). Titles that already exist are skipped, so running it twice does not duplicate issues.

issues <- tibble::tribble(
  ~title, ~labels, ~body,

  "Flat wetland divides: detect, bound the area, and optionally burn mapped lines locally", "enhancement", r"-(
**Problem**

In an otherwise well-defined catchment, a flat wetland on the divide is routed arbitrarily: breaching (R/terrain.R:19) and D8 choose a path the DEM can't support, so the polygon can take in ground that drains elsewhere or miss part of its own.

**Now** (0e9ff84)

- `low_relief` (R/delineate.R:495) is catchment-wide (> 30% of cells under 1 degree, or relief < 50 m). One wetland corner in steep terrain doesn't trip it.
- `flowline_crossings` (R/delineate.R:433) counts reference lines crossing the divide more than 250 m from the outlet, but not where, which way, or how much area is at stake. It misses wetlands with no mapped channel (fens, sheet flow).

**Wanted**

1. Detect locally, with no new data: flat patches the divide runs through, where local relief is below the DEM's vertical error (slope under 1 degree on 5 m IfSAR cells is mostly noise). Columns `divide_flat_frac`, `area_lo_km2` (area minus the inside parts of cut patches) and `area_hi_km2` (plus the outside parts); warning `flat_divide` when either part is more than about 5% of the area. Bounds are the honest answer where a wetland drains both ways or fills and spills.
2. Locate and orient crossings: for each reference-line crossing, whether it lies in a flagged flat and which way the line flows. Out across the divide away from the outlet: `flowline_exits` (probably took in another catchment's ground). In: `flowline_enters` (probably cut off part of this one). Confirm 3DHP lines are digitised in the flow direction. Optional: area in WBD HU12s that don't drain to the outlet's HU12, only where WBD has been redone from IfSAR.
3. Constrain only where flagged, opt-in: `flats = c("warn", "burn", "ask")`. `"burn"` lowers 3DHP EDH lines (not legacy NHD) into the DEM inside the flagged patches plus a buffer, then breaches and delineates again. The unburned polygon goes into `alternatives` (`rule = "burned"`), with IoU and area change from `compare_catchments()`. A fixed lowering in terra is enough; `wbt_fill_burn()` works on the whole DEM.

**Not this**

Burning GEOGLOWS or TDX-Hydro lines: they are DEM-derived (12 m TanDEM-X), start at 5 km2, and face the same ambiguity in flats.

**Later**

Monte Carlo delineation over DEMs perturbed by the IfSAR error, giving each cell's probability of being in the catchment. Flat divides show up as wide uncertain bands without a definition of "flat".

**Before setting thresholds**

Calibrate the flat test and the 5% on sites with known wetland divides. Check plots must show the flat patches and the crossings.
)-",

  "Move 3DHP queries to the new usgs_3dhp_all service and tag lines as EDH or NHD", "bug", r"-(
**Now** (0e9ff84)

`ak_flowlines(source = "3dhp")` queries https://hydro.nationalmap.gov/arcgis/rest/services/3DHP_all/FeatureServer/50 (R/flowlines.R:37). USGS has announced an updated service at https://3dhp.nationalmap.gov/arcgis/rest/services/usgs_3dhp_all/FeatureServer. On 2026-11-23 the old service's root answered "Invalid Token", while its layer 50 still answered.

**New service layers**

20, 30, 40 HydroLocations; 50 Flowline; 60 Waterbody; 80 Catchment (`areasqkm`, `totestdrainareasqkm`); 90 Spatial Metadata - EDH Workunit Area; 92 Elevation Workunit Area; 94 DEM LimitationArea.

**Wanted**

- Switch the endpoint; record it and the query date in provenance.
- Flowlines carry no field saying EDH or legacy NHD. Tag each line from layer 90 (`flowline_basis = "edh"` or `"nhd"`) so the checks, burning and snapping know what they use. Check whether the flowline `workunitid` is filled only for EDH lines.
- Look at what layer 94 (DEM limitation areas) holds; it may be a flag source.
- Update `ak_flowline_sources()` and the delineation and data-sources vignettes.
)-",

  'Use 3DHP catchments where EDH exists (method = "3dhp" or "auto") and compare them with DEM polygons', "enhancement", r"-(
**Why**

In EDH work units, 3DHP comes from the same 5 m IfSAR, conditioned and edited against imagery, and carries catchments and total drainage area. It should match our polygons in incised terrain and beat them in flats. Elsewhere 3DHP is legacy NHD with no catchments.

**Wanted**

1. `ak_delineate(method = c("dem", "3dhp", "auto"))`:
   - `"3dhp"`: find the 3DHP flowline at the point (same snapping rules), trace upstream (hydrosequence and levelpath fields on the Flowline layer), union the upstream catchments, and split the outlet catchment at the point with a DEM delineation confined to that catchment, as NLDI's split catchment does. Fail clearly outside EDH work units, off the network, or on channels smaller than the network's.
   - `"auto"`: `"3dhp"` inside EDH work units, `"dem"` elsewhere; `delin_method` records which.
   - The DEM is still cropped to the polygon for topography, outlet slope and `ak_obs_link()`, without the growing windows.
2. Compare wherever both are possible: `iou_3dhp` and `area_ratio_3dhp` from `compare_catchments()`, warning `disagrees_3dhp`, both outlines in the check plot. This is also validation for the package paper: agreement where 3DHP exists says how far to trust the DEM method where it doesn't.
3. `snap_reference = "3dhp"`: a reach's area range runs from `totestdrainareasqkm - areasqkm` to `totestdrainareasqkm`, which the consensus snap already accepts as `area_lo_km2` and `area_hi_km2`.

**Open**

- Whether upstream tracing through the web service is fast enough for large basins, or the bulk 3DHP downloads are needed.
- How many CAKN sites fall inside EDH work units (map of layer 90).
- Whether mixing methods by coverage is acceptable for downstream models (the method becomes a covariate), or `"dem"` stays the default and `"3dhp"` is a check only.
- Depends on the endpoint issue.
)-",

  "Catchment zones: stream buffers and near-outlet areas, with check plots", "enhancement", r"-(
**Goal**

Compute any layer over part of a catchment: within a distance of the stream network, near the outlet, or both (for example riparian land cover close to the sampling point).

**Zones** (`catchment_zones()`, generic, so unprefixed)

- Stream buffers by Euclidean distance to channels (`dist_m`), by flow distance down to the channel (`wbt_downslope_distance_to_stream()`), or by height above the channel (HAND, `wbt_elevation_above_stream()`). The last two follow the terrain, so a buffer doesn't climb a valley wall or cross a divide.
- Channels: the delineation's DEM channels at a chosen `stream_threshold_km2`, or 3DHP EDH lines; optionally waterbody shores. The threshold matters a lot: a 100 m buffer around every channel above 0.05 km2 covers most of a small catchment.
- Near the outlet: Euclidean radius from the snapped outlet, or flow-path distance to the outlet (`wbt_downslope_flowpath_length()` on the D8 pointer, within the catchment).
- Combinations, such as near-outlet riparian (stream buffer intersected with "within Y km of the outlet").
- Always clipped to the catchment. Uses the kept rasters (`files`) or conditions the DEM again, as `ak_topography()` does.

**Outputs**

sf polygons with `site_code`, `zone`, a unique `zone_id`, `zone_area_km2`, `zone_pct_of_catchment`, and the definition (type, distance, channel source, threshold) in columns and in provenance.

**Layers on zones**

Every layer function should take the zone table as it takes a catchment table. Decide between (a) a separate table keyed by `zone_id`, written as its own `ak_write()` layer, and (b) suffixed columns on the catchment table (`nlcd_pct_forest_rip100`). (a) avoids a column explosion. The partial-coverage rule (NA above 5% missing) applies to the zone's area.

**Resolution and uncertainty**

- Flag `zone_below_resolution` when a zone is narrower than a few cells of a layer's native resolution (a 100 m buffer means nothing on 800 m PRISM, 1 km Daymet or 1:250k STATSGO2). Needs a resolution column in `sources.csv`.
- Zone edges follow DEM channels, so carry positional uncertainty (`sigma_m`, as in the existing positional draws) into zone values.

**Check plots** (required)

`plot_catchment(zones = )` or `plot_zones()`: catchment outline, zone fill, the channels used, the outlet and, for near-outlet zones, the distance limit, on imagery or hillshade. Subtitle with zone area and % of the catchment; one panel per zone definition; written to `plot_dir` like the delineation plots.
)-"
)

existing <- gh::gh("GET /repos/AKRiversLab/AKCatchments/issues", state = "all", .limit = Inf) |>
  purrr::map_chr("title")

issues |>
  dplyr::filter(!title %in% existing) |>
  purrr::pwalk(\(title, labels, body) {
    res <- gh::gh("POST /repos/AKRiversLab/AKCatchments/issues", title = title, labels = list(labels), body = trimws(body))
    message("#", res$number, " ", res$html_url)
  })
