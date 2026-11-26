#' Delineate the catchment draining to a stream point
#'
#' Fetches elevation data around the point, conditions it for flow routing
#' (depressions breached or filled, D8 directions and accumulation, WhiteboxTools),
#' snaps the point onto the channel network, delineates the upstream area and
#' runs the checks described in [delineation_flags].
#'
#' Window. The first window is a square of half-width `start_buffer_m`
#' around the point, or `window` if given. While the catchment reaches the
#' window's edge, only the sides it touches move out, each to twice its
#' distance from the point (at least 2 km further), up to `max_buffer_m`.
#' A catchment cut off by the first square instead gets a window sized by a
#' quick pass over the same DEM at 100 m: the 100 m catchment of the lowest
#' 100 m channel inside the part already delineated, plus 1 km. So a large
#' catchment is conditioned twice, on the small square and on its own window,
#' rather than once per doubling. The pass only sizes the window: the
#' catchment is the full-resolution DEM's, and where it still touches that
#' window, the touched sides move out by half its width or height.
#' Where a window's long side is more than `max_cells` cells, the cell size
#' coarsens to the next of 1.25, 1.5, 2, 2.5, 3, 4, 5, ... times the source's
#' own (6.25, 7.5, 10, 12.5, 15, 20, 25 m for IfSAR), so windows of similar
#' size share their DEM tiles. The cell size used is in `dem_res_m`, the window in
#' `window_xmin`, `window_ymin`, `window_xmax`, `window_ymax` (EPSG:3338)
#' and how it was chosen in `window_plan` (`"square"`, `"coarse"` or
#' `"given"`). To repeat a delineation on the same grid (other snap radii,
#' shifted points), pass those four values as `window` and `dem_res_m` as
#' `res`.
#'
#' Snapping. A sample point rarely sits exactly on the DEM channel. Within
#' `snap_dist_m` the point moves to a channel cell (contributing area of at
#' least `stream_threshold_km2`):
#' * with `ref_area_km2` (a hand-drawn, published or StreamStats area), to the
#'   cell whose contributing area best matches it;
#' * otherwise (`snap = "largest"`, the default) to the largest channel in
#'   reach, which is right for a point on a main stem but wrong for a point on
#'   a small tributary just above a confluence; or (`snap = "nearest"`) to the
#'   nearest channel cell, which is right on the tributary but can pick a side
#'   gully. Either way `snap_ambiguous` flags points where another channel in
#'   reach has a contributing area more than 25% different (for example the
#'   main stem just above a confluence when the point snapped below it);
#'   `snap_alt_area_km2` gives that area, so the size of the possible error is
#'   on the table. Check those plots.
#' * `snap = "reference"`: to the cell whose contributing area best fits a
#'   reach of a reference stream network (`snap_reference`) within
#'   `snap_reference_dist_m` of the point. The fit is to the reach's range of
#'   upstream area, from its upstream to its downstream end. This is the
#'   reference network on its own; where it has no reach nearby, the rule
#'   falls back to `"largest"` (`snap_method = "largest_no_reference"`).
#' * `snap = "consensus"`: combines the terrain and the reference network.
#'   The terrain is a constraint: the outlet must lie on the sample point's
#'   own D8 flow path, downstream of it, so the snap never crosses into
#'   another watershed (a neighbouring stream, or the other branch at a
#'   confluence). Among the downstream channel cells, the reference network
#'   chooses: the cell whose area fits a nearby reach within a factor of 1.5
#'   (allowing for the different DEM). With no fit (no reach nearby, such as
#'   a stream under the network's 5 km2 threshold), the largest downstream
#'   channel: the `"largest"` rule restricted to the point's own drainage.
#'   With no channel on the path within the radius, the best reference fit,
#'   else the largest channel. Area tells branches apart only when their sizes
#'   differ by more than that factor; a point on an unmapped tributary just
#'   above a mapped main stem still snaps below the junction, as with
#'   `"largest"`, and carries the `snap_ambiguous` warning.
#'   `snap_evidence` records what the pick rests on, `snap_conflict` warns
#'   when the reference fits a channel other than the pick (for example a
#'   point on a tributary too small for the network, beside a mapped main
#'   stem), and
#'   `snap_area_largest_km2`, `snap_area_reference_km2` and
#'   `snap_area_consensus_km2` give the area each rule would have chosen.
#'   With `snap_alternatives = TRUE` the catchments of the outlets the other
#'   rules would have chosen are delineated on the same rasters (in
#'   `alternatives`), as a measure of how much the snap matters.
#'
#' Where automated delineation struggles: braided and glacial outwash channels,
#' low-relief wetlands and lake-dominated lowlands, karst, and anywhere the DEM
#' channel is displaced from the mapped stream. D8 routing on a 5 m IfSAR
#' model is reliable in upland, well-incised terrain; `low_relief` and
#' `flowline_crossings` are the main signals that a site is outside that.
#'
#' A plot of every delineation on satellite imagery is written to `plot_dir`
#' (see [plot_catchment()]); look at it before using the polygon.
#'
#' For many points use [ak_catchments()], which calls this for each one
#' (or, with `regional = TRUE`, shares one DEM window between nearby
#' points), shows progress, can run in parallel, and returns one sf table.
#'
#' @param lat,lon Point in decimal degrees (WGS84).
#' @param site_code Identifier carried to the outputs and plot file name.
#' @param ref_area_km2 Known catchment area to guide snapping, or `NA`.
#' @param dem_source,res,dem_dir Passed to [ak_dem()].
#' @param depressions How depressions are removed before flow routing
#'   (WhiteboxTools):
#'   * `"breach"` (default): every depression breached (BreachDepressions,
#'     Lindsay 2016), single-cell pits raised first. Fastest.
#'   * `"least_cost"`: least-cost breach paths up to 100 cells long
#'     (BreachDepressionsLeastCost, Lindsay and Dhun 2015), the lowest-impact
#'     cut through a road embankment or other narrow barrier; what that
#'     leaves is breached as by `"breach"`, not filled. About 4.5 times
#'     slower.
#'   * `"fill"`: depressions filled to their spill level (FillDepressions),
#'     the classic approach; flats then route arbitrarily.
#'
#'   In tests (IfSAR-like and real DEMs with noise added), `"breach"` and
#'   `"least_cost"` kept about as many catchments within IoU 0.95 of their
#'   noise-free version on a real DEM (88% and 89%), `"least_cost"` more on
#'   synthetic terrain full of closed depressions (84% against 78%), and
#'   `"fill"` fewer (76% and 75%). The methods disagree most where large
#'   closed depressions (thermokarst, kettle lakes) or roads decide the
#'   drainage; compare them there. Recorded in the `depressions` column.
#' @param snap Snapping rule when `ref_area_km2` is missing: `"largest"`,
#'   `"nearest"`, `"reference"` or `"consensus"` (see Snapping).
#' @param snap_reference Reference stream network for `snap = "reference"`
#'   or `"consensus"`: a source name for [ak_flowlines()] whose lines carry
#'   upstream area (`"geoglows"`, TDX-Hydro as distributed by GEOGLOWS, is the
#'   best in Alaska; `"tdx"`, `"merit"`, `"hydrorivers"`), or sf lines with
#'   `USContArea`/`DSContArea` (m2), `uparea` or `UPLAND_SKM` (km2), or
#'   `area_lo_km2`/`area_hi_km2`. Downloaded sources must have been
#'   downloaded with [ak_download_flowlines()] first.
#' @param snap_reference_dist_m Reaches within this distance of the point are
#'   used as evidence.
#' @param snap_alternatives Also delineate the outlets the other rules would
#'   have chosen (see Snapping)?
#' @param keep_rasters Keep the conditioned rasters of the catchment (`files`,
#'   below)? They take disk space: a few MB for a small catchment, a few
#'   hundred MB for one of thousands of km2. Use `FALSE` when only the
#'   polygon is needed (repeated or sensitivity runs); `files` is then `NULL`,
#'   [ak_topography()] conditions the DEM again, and the check plot has no
#'   hillshade to fall back on when imagery is unavailable.
#' @param snap_dist_m Snap search radius in metres.
#' @param stream_threshold_km2 Contributing area that defines a channel.
#' @param start_buffer_m First window half-width; defaults to
#'   `max(5000, 2 * sqrt(ref_area))`.
#' @param max_buffer_m Largest distance from the point to any side of the
#'   window.
#' @param max_cells Largest window edge in cells; sets the coarsening.
#' @param window The first window instead of the square of `start_buffer_m`:
#'   an sf, sfc or bbox object (its bounding box), or `c(xmin, ymin, xmax,
#'   ymax)` in EPSG:3338, used as given (extended only to hold the point and
#'   its snap radius). The catchment should fit inside with room to spare,
#'   e.g. `sf::st_buffer(hand_drawn_polygon, 2000)`; a window it reaches
#'   grows as usual. `NULL` (default): the square, then the coarse pass or
#'   side by side growth (see Window).
#' @param flowlines Reference flowlines: an sf object, a source name for
#'   [ak_flowlines()] (default `"3dhp"`), or `NULL` for none.
#' @param plot Write the satellite-imagery check plot?
#' @param plot_dir Folder for the plot.
#' @param fallback_dem A second `dem_source` (e.g. `"copernicus"`) used when
#'   the first fails (no elevation data at the point, a service or tile
#'   error) or gives a catchment that runs into missing data (`nodata_edge`),
#'   as at the Canadian border for IfSAR. The result then carries
#'   `dem_fallback = TRUE` and the `dem_fallback` warning flag, and
#'   `dem_source` says which DEM was used. `NULL` (default): no fallback; the
#'   DEM used is always the one asked for.
#' @return An `ak_delineation` object: a list with `catchment` (one-row sf
#'   with diagnostics and flags), `point`, `snapped`, `streams` (DEM-derived
#'   channels), `flowlines` (reference, if any) and `files`: the DEM,
#'   breached DEM, D8 pointer and D8 accumulation of the final window,
#'   cropped to the catchment plus a margin and kept in the cache's
#'   `delineation/<site_code>/<dem>_<depressions>_<extent>/` folder, so runs
#'   with another DEM, depression method or snap never overwrite them. Accumulation is computed on the whole
#'   window before cropping. [ak_topography()] and [ak_outlet_slope()] can
#'   use these files directly (`files` is `NULL` with `keep_rasters = FALSE`).
#'   With `snap_alternatives = TRUE` also
#'   `alternatives`: sf polygons with `rule`, `snap_area_km2`, `area_km2` and
#'   `truncated` for each rule whose outlet differs from the pick (`NULL`
#'   when they all agree).
#'
#'   The catchment also records how long each step took, in seconds:
#'   `t_dem_s` (DEM download and reading), `t_condition_s` (breaching, D8
#'   pointer and accumulation), `t_watershed_s` (snapping and watershed),
#'   `t_finish_s` (polygon, channels, diagnostics, cropping the rasters),
#'   `t_flowlines_s` (reference flowlines), `t_plot_s` (imagery and plot)
#'   and `t_total_s`, with `n_windows` (windows tried) and `window_mode`
#'   (`"site"`, or `"regional:<cluster>"` when the window was shared, in
#'   which case the DEM and conditioning time is the shared time divided
#'   among the cluster's sites). Use them to see where a run spends its time;
#'   for a long run, `options(AKCatchments.log = "stages.tsv")` also writes a
#'   line as each stage of each site ends (time, process, site, stage,
#'   seconds, detail), so an interrupted or slow run shows where it was.
#' @seealso `vignette("delineation")` (download, use, uncertainty, validation)
#' @export
ak_delineate <- function(lat, lon, site_code = NA_character_, ref_area_km2 = NA_real_,
                         dem_source = "ifsar", res = NULL, dem_dir = NULL, snap = c("largest", "nearest", "reference", "consensus"),
                         snap_dist_m = 150, stream_threshold_km2 = 0.05, start_buffer_m = NULL,
                         max_buffer_m = 100000, max_cells = 6000, flowlines = "3dhp",
                         plot = TRUE, plot_dir = ".", fallback_dem = NULL, snap_reference = NULL,
                         snap_reference_dist_m = 300, snap_alternatives = FALSE, keep_rasters = TRUE, window = NULL,
                         depressions = c("breach", "least_cost", "fill")) {
  if (!is.null(fallback_dem) && !identical(fallback_dem, dem_source)) {
    args <- as.list(environment())[setdiff(names(formals(ak_delineate)), "fallback_dem")]
    first <- tryCatch(do.call(ak_delineate, args), error = \(e) e)
    if (inherits(first, "ak_delineation") && !isTRUE(first$catchment$nodata_edge)) {
      first$catchment$dem_fallback <- FALSE
      return(first)
    }
    why <- if (inherits(first, "error")) conditionMessage(first) else "the catchment runs into cells without elevation data"
    message(site_code, ": ", dem_source, " DEM not usable (", why, "); delineating from ", fallback_dem)
    args$dem_source <- fallback_dem
    args["res"] <- list(NULL)
    second <- do.call(ak_delineate, args)
    second$catchment$dem_fallback <- TRUE
    second$catchment <- flag_delineation(second$catchment, snap_dist_m = snap_dist_m)
    return(second)
  }
  snap <- match.arg(snap)
  depressions <- match.arg(depressions)
  wbt_ready()
  t_start <- Sys.time()
  site_code <- if (is.na(site_code)) sprintf("pt_%.5f_%.5f", lat, lon) else site_code
  pt <- site_point(site_code, lat, lon)
  reaches <- if (snap %in% c("reference", "consensus")) {
    tryCatch(snap_reaches(pt, snap_reference, snap_reference_dist_m),
             error = \(e) { message(site_code, ": snap reference unavailable (", conditionMessage(e), "); flow path evidence only"); NULL })
  }
  res0 <- res %||% default_res(dem_source)
  xy <- sf::st_coordinates(pt)[1, ]
  win <- first_window(xy, window, start_buffer_m %||% start_buffer(ref_area_km2, dem_source), snap_dist_m, max_buffer_m)
  plan <- if (is.null(window)) "square" else "given"
  wd <- tempfile("akdelin_")
  dir.create(wd)
  on.exit(unlink(wd, recursive = TRUE), add = TRUE)
  tt <- c(dem = 0, condition = 0, watershed = 0)
  n_windows <- 0L
  stage_log(site_code, "start", t_start, sprintf("%s, %s, snap %s %g m", dem_source, depressions, snap, snap_dist_m))
  repeat {
    n_windows <- n_windows + 1L
    t0 <- Sys.time()
    dem <- ak_dem(window_aoi(win), dem_source, res = window_res(res0, win, max_cells), dir = dem_dir)
    check_dem_at(dem, pt, site_code, lat, lon, dem_source)
    got <- window_of(dem)
    tt["dem"] <- tt["dem"] + since(t0)
    stage_log(site_code, paste("window", n_windows, "dem"), t0, window_text(got, dem))
    t0 <- Sys.time()
    f <- ak_condition(dem, wd, depressions)
    tt["condition"] <- tt["condition"] + since(t0)
    stage_log(site_code, paste("window", n_windows, "condition"), t0)
    t0 <- Sys.time()
    w <- outlet_watershed(pt, f, wd, snap_dist_m, ref_area_km2, stream_threshold_km2, snap, reaches)
    tt["watershed"] <- tt["watershed"] + since(t0)
    stage_log(site_code, paste("window", n_windows, "watershed"), t0,
              paste("edges touched:", if (length(w$sides)) paste(w$sides, collapse = "") else "none"))
    # stop when the catchment fits, or when the window could not get any larger (the DEM's coverage ends there)
    if (!w$truncated || (n_windows > 1 && isTRUE(all.equal(got, win_prev)))) break
    win_prev <- got
    t0 <- Sys.time()
    nxt <- if (n_windows == 1 && is.null(window)) coarse_window(got, w, dem_source, dem_dir, xy, max_buffer_m, wd, depressions)
    if (!is.null(nxt)) {
      plan <- "coarse"
      tt["dem"] <- tt["dem"] + since(t0)
      stage_log(site_code, "coarse pass", t0, window_text(nxt))
    } else nxt <- grow_window(got, w$sides, xy, max_buffer_m, by = if (plan == "coarse") 0.5)
    if (is.null(nxt)) break
    win <- nxt
    message(site_code, ": catchment reached the window edge (", paste(w$sides, collapse = ", "), "); window now ",
            window_text(win), if (identical(plan, "coarse") && n_windows == 1) " (from a 100 m pass over the same DEM)")
  }
  finish_delineation(site_code, lat, lon, ref_area_km2, pt, w, f, dem, wd, win = got, plan = plan, snap_dist_m = snap_dist_m,
                     stream_threshold_km2 = stream_threshold_km2, flowlines = flowlines, plot = plot, plot_dir = plot_dir,
                     times = tt, n_windows = n_windows, window_mode = "site", t_start = t_start, snap_alternatives = snap_alternatives,
                     keep_rasters = keep_rasters, depressions = depressions)
}

since <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))

site_point <- function(site_code, lat, lon) {
  sf::st_sf(site_code = site_code, geometry = sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)) |>
    sf::st_transform(akalbers_crs())
}

# First window half-width: 5 km, or 2 km for lidar (lidar projects are small, and a window must lie inside one), or
# twice the side of a square of the reference area when that is larger
start_buffer <- function(ref_area_km2, dem_source = "ifsar") {
  max(if (dem_source %in% c("lidar", "best")) 2000 else 5000, 2 * sqrt(ref_area_km2 * 1e6), na.rm = TRUE)
}

# Windows are c(xmin, ymin, xmax, ymax) in EPSG:3338.
# The first: the square of half-width buf around the point, or the given extent, holding the point's snap radius
# (and a margin) and no side further than max_buffer_m from the point
first_window <- function(xy, window, buf, snap_dist_m, max_buffer_m) {
  if (is.null(window)) {
    b <- min(buf, max_buffer_m)
    return(c(xmin = xy[[1]] - b, ymin = xy[[2]] - b, xmax = xy[[1]] + b, ymax = xy[[2]] + b))
  }
  bb <- if (inherits(window, c("sf", "sfc", "bbox"))) {
    sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(window)), akalbers_crs()))[c("xmin", "ymin", "xmax", "ymax")]
  } else stats::setNames(as.numeric(window), c("xmin", "ymin", "xmax", "ymax"))
  need <- snap_dist_m + 500
  c(xmin = max(min(bb[["xmin"]], xy[[1]] - need), xy[[1]] - max_buffer_m), ymin = max(min(bb[["ymin"]], xy[[2]] - need), xy[[2]] - max_buffer_m),
    xmax = min(max(bb[["xmax"]], xy[[1]] + need), xy[[1]] + max_buffer_m), ymax = min(max(bb[["ymax"]], xy[[2]] + need), xy[[2]] + max_buffer_m))
}

window_aoi <- function(win) sf::st_as_sfc(sf::st_bbox(win, crs = sf::st_crs(akalbers_crs())))

window_of <- function(dem) {
  e <- as.vector(terra::ext(dem))
  c(xmin = e[["xmin"]], ymin = e[["ymin"]], xmax = e[["xmax"]], ymax = e[["ymax"]])
}

window_text <- function(win, dem = NULL) {
  paste0(sprintf("%.1f x %.1f km", (win[["xmax"]] - win[["xmin"]]) / 1000, (win[["ymax"]] - win[["ymin"]]) / 1000),
         if (!is.null(dem)) sprintf(" at %g m (%.1f M cells)", terra::res(dem)[1], terra::ncell(dem) / 1e6))
}

# Cell size for a window: the source's own (res0), or where the window's long side is more than max_cells of those,
# the smallest of 1.25, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10, ... times it that fits. A ladder rather than any value, so
# windows of similar size (a site's windows as they grow, nearby sites, reruns) share their image-service tiles; at
# most a third coarser than the window needs.
window_res <- function(res0, win, max_cells) {
  if (is.na(res0)) return(NULL)
  side <- max(win[["xmax"]] - win[["xmin"]], win[["ymax"]] - win[["ymin"]])
  steps <- c(1, 1.25, 1.5, 2, 2.5, 3, 4, 5, 6, 8) * rep(10^(0:3), each = 10)
  res0 * steps[which(res0 * steps * max_cells >= side - 1e-6)[1]]
}

# The window with each side the catchment touches moved out to twice its distance from the point, or with `by`, by
# that share of the window's width or height (after the coarse pass: the catchment overruns that window where the
# coarse and full-resolution divides part, often by several km); at least 2 km further either way, and no further
# than max_buffer_m. NULL when none of those sides can move.
grow_window <- function(win, sides, xy, max_buffer_m, by = NULL) {
  d <- c(w = xy[[1]] - win[["xmin"]], s = xy[[2]] - win[["ymin"]], e = win[["xmax"]] - xy[[1]], n = win[["ymax"]] - xy[[2]])
  size <- c(w = win[["xmax"]] - win[["xmin"]], s = win[["ymax"]] - win[["ymin"]], e = win[["xmax"]] - win[["xmin"]], n = win[["ymax"]] - win[["ymin"]])
  step <- if (is.null(by)) d else by * size
  new <- d
  new[sides] <- pmin(d[sides] + pmax(step[sides], 2000), max_buffer_m)
  if (all(new[sides] <= d[sides] + 1e-6)) return(NULL)
  c(xmin = xy[[1]] - new[["w"]], ymin = xy[[2]] - new[["s"]], xmax = xy[[1]] + new[["e"]], ymax = xy[[2]] + new[["n"]])
}

# After the first square cuts the catchment off: a window over the catchment the same DEM gives at res_c (a quick
# pass: a 40 km square at 100 m is 160,000 cells, doubled while that catchment reaches its edge, up to max_buffer_m),
# plus margin_m, joined to the current window. The coarse outlet is the coarse cell with the most upstream area among
# those mostly inside the part of the catchment delineated so far, so the coarse catchment is this one and not a
# neighbouring river's. NULL when no coarse cell is mostly inside that part (or for lidar, whose windows a project
# bounds), and the window then grows side by side.
coarse_window <- function(win, w, dem_source, dem_dir, xy, max_buffer_m, wd, depressions, res_c = 100, margin_m = 1000) {
  if (identical(dem_source, "lidar")) return(NULL)
  cwd <- file.path(wd, "coarse")
  dir.create(cwd, showWarnings = FALSE)
  half <- min(20000, max_buffer_m)
  prev <- NULL
  repeat {
    dem <- ak_dem(window_aoi(c(xmin = xy[[1]] - half, ymin = xy[[2]] - half, xmax = xy[[1]] + half, ymax = xy[[2]] + half)),
                  if (identical(dem_source, "best")) "ifsar" else dem_source, res = res_c, dir = dem_dir)
    if (isTRUE(all.equal(window_of(dem), prev))) break                      # the DEM's coverage ends: no larger pass
    prev <- window_of(dem)
    f <- ak_condition(dem, cwd, depressions)
    acc <- terra::rast(f$accum)
    share <- terra::resample(!is.na(w$ws), acc, method = "average")          # of each coarse cell, inside the part
    acc[is.na(share) | share < 0.5] <- NA
    if (all(is.na(terra::values(acc)))) return(NULL)
    outlet <- sf::st_sfc(sf::st_point(terra::xyFromCell(acc, which.max(terra::values(acc)[, 1]))[1, ]), crs = akalbers_crs())
    ws <- watershed_raster(outlet, f, cwd, tag = "coarse")
    if (!touches_frame(ws) || half >= max_buffer_m) break
    half <- min(2 * half, max_buffer_m)
  }
  b <- window_of(terra::trim(ws))
  c(xmin = max(min(win[["xmin"]], b[["xmin"]] - margin_m), xy[[1]] - max_buffer_m),
    ymin = max(min(win[["ymin"]], b[["ymin"]] - margin_m), xy[[2]] - max_buffer_m),
    xmax = min(max(win[["xmax"]], b[["xmax"]] + margin_m), xy[[1]] + max_buffer_m),
    ymax = min(max(win[["ymax"]], b[["ymax"]] + margin_m), xy[[2]] + max_buffer_m))
}

# One line per finished stage of a delineation, in the file named by options(AKCatchments.log): where a long run is,
# and how long each stage took
stage_log <- function(site_code, stage, t0, detail = "") {
  f <- getOption("AKCatchments.log")
  if (is.null(f)) return(invisible())
  try(cat(sprintf("%s\t%d\t%s\t%s\t%.1f\t%s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Sys.getpid(), site_code, stage, since(t0), detail),
          file = f, append = TRUE), silent = TRUE)
  invisible()
}

check_dem_at <- function(dem, pt, site_code, lat, lon, dem_source) {
  if (is.na(terra::extract(dem, terra::vect(sf::st_transform(pt, terra::crs(dem))))[1, 2])) {
    stop(site_code, ": no elevation data at ", round(lat, 5), ", ", round(lon, 5), " from the ", dem_source,
         " DEM. The point is outside its coverage (IfSAR covers Alaska only), offshore, or lat/lon are swapped.", call. = FALSE)
  }
}

# Snap the point and delineate the area draining to it on conditioned rasters `f`
outlet_watershed <- function(pt, f, wd, snap_dist_m, ref_area_km2, stream_threshold_km2, snap, reaches = NULL) {
  snapped <- snap_point(pt, f$accum, snap_dist_m, ref_area_km2, stream_threshold_km2, snap, f$pointer, reaches)
  snapped$snap_path_ok <- if (snapped$snap_method == "none") NA else
    same_stream(f$pointer, sf::st_coordinates(pt)[1, ], sf::st_coordinates(snapped)[1, ], snap_dist_m)
  ws <- watershed_raster(snapped, f, wd, tag = pt$site_code)
  sides <- frame_sides(ws)
  list(snapped = snapped, ws = ws, sides = sides, truncated = length(sides) > 0)
}

# From a watershed raster to the ak_delineation: polygon, channels, reference flowlines, diagnostics, flags,
# the rasters kept in the cache (unless keep_rasters = FALSE), and the check plot. `win`: the window (window_of()),
# `plan`: how it was chosen.
finish_delineation <- function(site_code, lat, lon, ref_area_km2, pt, w, f, dem, wd, win, plan, snap_dist_m, stream_threshold_km2,
                               flowlines, plot, plot_dir, times, n_windows, window_mode, t_start, snap_alternatives = FALSE,
                               keep_rasters = TRUE, depressions = "breach") {
  t0 <- Sys.time()
  ws <- w$ws
  snapped <- w$snapped
  nodata <- nodata_contact(ws, dem, snapped, tol_m = max(250, 2 * snap_dist_m))
  if (nodata) message(site_code, ": catchment runs into cells without elevation data (edge of DEM coverage, e.g. the Canadian border",
                      " for IfSAR); it is cut off there. Try dem_source = \"copernicus\".")
  poly <- terra::as.polygons(ws) |> sf::st_as_sf() |> sf::st_make_valid() |> sf::st_union()
  if (!length(poly) || all(sf::st_is_empty(poly))) stop(site_code, ": the delineated watershed is empty", call. = FALSE)
  streams <- (f$streams %||% dem_streams(f, stream_threshold_km2, wd)) |>
    sf::st_filter(sf::st_buffer(poly, 10 * terra::res(dem)[1]))
  alternatives <- if (snap_alternatives) snap_alternative_polygons(snapped, f, wd, site_code)
  files <- if (keep_rasters) save_rasters(f, poly, site_code, dem_source(dem), depressions)
  t_finish <- since(t0)
  stage_log(site_code, "finish", t0)
  t0 <- Sys.time()
  ref <- if (is.character(flowlines)) {
    tryCatch(ak_flowlines(sf::st_buffer(poly, 500), flowlines),
             error = \(e) { message("Reference flowlines unavailable: ", conditionMessage(e)); NULL })
  } else flowlines
  t_flow <- since(t0)
  stage_log(site_code, "flowlines", t0, if (is.character(flowlines)) paste(NROW(ref), flowlines, "lines") else "")
  xy <- sf::st_coordinates(pt)[1, ]
  catchment <- dplyr::bind_cols(
    dplyr::tibble(site_code = site_code, lat = lat, lon = lon, ref_area_km2 = ref_area_km2,
                  dem_source = dem_source(dem), dem_res_m = terra::res(dem)[1], dem_project = dem_tag(dem, "lidar_project"),
                  depressions = depressions,
                  window_m = max(xy[[1]] - win[["xmin"]], win[["xmax"]] - xy[[1]], xy[[2]] - win[["ymin"]], win[["ymax"]] - xy[[2]]),
                  window_xmin = win[["xmin"]], window_ymin = win[["ymin"]], window_xmax = win[["xmax"]], window_ymax = win[["ymax"]],
                  window_plan = plan, truncated = w$truncated,
                  nodata_edge = nodata),
    sf::st_drop_geometry(snapped),
    catchment_diagnostics(poly, dem, snapped),
    if (!is.null(ref) && nrow(ref)) flowline_diagnostics(poly, snapped, ref)
  )
  catchment <- sf::st_sf(catchment, geometry = poly) |> flag_delineation(snap_dist_m = snap_dist_m)
  out <- structure(list(catchment = catchment, point = pt, snapped = sf::st_sf(site_code = site_code, sf::st_geometry(snapped)),
                        streams = streams, flowlines = ref, files = files, alternatives = alternatives), class = "ak_delineation")
  t0 <- Sys.time()
  if (plot) {
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
    out$plot_file <- plot_catchment(out, file = file.path(plot_dir, paste0(site_code, ".png")))
    stage_log(site_code, "plot", t0)
  }
  tm <- c(t_dem_s = times[["dem"]], t_condition_s = times[["condition"]], t_watershed_s = times[["watershed"]],
          t_finish_s = t_finish, t_flowlines_s = t_flow, t_plot_s = since(t0))
  for (k in names(tm)) out$catchment[[k]] <- round(tm[[k]], 1)
  out$catchment$t_total_s <- round(since(t_start), 1)
  out$catchment$n_windows <- n_windows
  out$catchment$window_mode <- window_mode
  g <- attr(out$catchment, "sf_column")
  out$catchment <- out$catchment[c(setdiff(names(out$catchment), g), g)]
  out
}

#' @export
print.ak_delineation <- function(x, ...) {
  c1 <- x$catchment
  cat("<ak_delineation>", c1$site_code, "\n")
  win <- if (is.null(c1$window_xmin)) sprintf("%.0f km", c1$window_m / 1000) else      # objects from before 0.2.0.9002
    sprintf("%s (%s, %d tried)", window_text(c(xmin = c1$window_xmin, ymin = c1$window_ymin, xmax = c1$window_xmax, ymax = c1$window_ymax)),
            c1$window_plan, c1$n_windows)
  cat(sprintf("  area %.3f km2 | DEM %s | snap %s, %.0f m | window %s\n", c1$area_km2,
              paste(c(c1$dem_source, c1$depressions), collapse = ", "), c1$snap_method, c1$snap_dist_m, win))
  cat("  delin_ok:", c1$delin_ok, "| flags:", if (nzchar(c1$flags)) c1$flags else "none", "\n")
  if (!is.null(x$plot_file)) cat("  plot:", x$plot_file, "\n")
  invisible(x)
}

wbt_ready <- function() {
  if (!isTRUE(suppressMessages(whitebox::wbt_init()))) {
    stop("WhiteboxTools is not installed: run whitebox::install_whitebox() once")
  }
  whitebox::wbt_options(verbose = FALSE)
}

ak_condition <- function(dem, dir, depressions = "breach") {
  f <- stats::setNames(as.list(file.path(dir, c("dem.tif", "breached.tif", "d8_pointer.tif", "d8_accum.tif"))),
                       c("dem", "filled", "pointer", "accum"))
  terra::writeRaster(dem, f$dem, overwrite = TRUE, NAflag = -9999, datatype = "FLT4S")
  wbt_condition(f, depressions)
}

# Depressions removed as `depressions` says (see ak_delineate()), then D8 pointer and D8 accumulation of f$dem into
# f$filled, f$pointer and f$accum. Least-cost breaching hands what it cannot reach to complete breaching rather than to
# its own filling step, which, like FillDepressions, now and then panics under load (it takes its raster back before
# every thread has let go: "Error unwrapping 'output'"); so every method gets three tries, which filling may need.
# WhiteboxTools reports a failed tool only in a message, so the old outputs go first (a window must never be routed on
# the last window's rasters) and a missing output stops the run.
wbt_condition <- function(f, depressions = "breach") {
  out <- c(f$filled, f$pointer, f$accum)
  lc <- sub("\\.tif$", "_least_cost.tif", f$filled)
  unlink(c(out, lc))
  for (i in 1:3) if (!file.exists(f$filled)) switch(depressions,
    breach = whitebox::wbt_breach_depressions(f$dem, f$filled, fill_pits = TRUE),
    least_cost = {
      whitebox::wbt_breach_depressions_least_cost(f$dem, lc, dist = 100, fill = FALSE)
      whitebox::wbt_breach_depressions(lc, f$filled, fill_pits = TRUE)
    },
    fill = whitebox::wbt_fill_depressions(f$dem, f$filled, fix_flats = TRUE))
  unlink(lc)
  whitebox::wbt_d8_pointer(f$filled, f$pointer)
  whitebox::wbt_d8_flow_accumulation(f$filled, f$accum, out_type = "cells")
  if (!all(file.exists(out))) {
    stop("WhiteboxTools did not write ", paste(basename(out[!file.exists(out)]), collapse = ", "), " in ", dirname(f$dem),
         " (its messages above say why)", call. = FALSE)
  }
  f
}

snap_point <- function(pt, accum_file, snap_dist_m, ref_km2, threshold_km2, snap, pointer_file = NULL, reaches = NULL) {
  acc <- terra::rast(accum_file)
  cell_km2 <- prod(terra::res(acc)) / 1e6
  xy <- sf::st_coordinates(pt)[1, ]
  a <- terra::extract(acc, terra::vect(sf::st_buffer(pt, snap_dist_m)), cells = TRUE, xy = TRUE)
  a$km2 <- a[[2]] * cell_km2
  a$dist <- sqrt((a$x - xy[[1]])^2 + (a$y - xy[[2]])^2)
  thr <- if (isTRUE(ref_km2 > 0)) max(min(threshold_km2, ref_km2 / 5), 5 * cell_km2) else threshold_km2
  a <- a[!is.na(a$km2) & a$km2 >= thr, ]
  method <- if (!nrow(a)) "none" else if (isTRUE(ref_km2 >= thr)) "ref_area" else snap
  ev <- if (method %in% c("reference", "consensus")) snap_evidence(a, xy, pointer_file, reaches, snap_dist_m)
  if (identical(method, "reference") && !ev$has_ref) method <- "largest_no_reference"
  pick <- switch(method,
                 none = data.frame(x = xy[[1]], y = xy[[2]], km2 = NA_real_, dist = 0),
                 ref_area = a[order(abs(log(a$km2 / ref_km2)), a$dist)[1], ],
                 largest = , largest_no_reference = a[order(-a$km2, a$dist)[1], ],
                 nearest = a[order(a$dist, -a$km2)[1], ],
                 reference = ev$picks$reference,
                 consensus = ev$picks$consensus)
  # Other channels in reach whose area differs from the pick by more than 25% (and that are more than a
  # fifth of it, not side gullies): a confluence or a neighbouring stream. Along one channel, area changes by
  # far less than that over a snap radius, so a difference this size means the choice of channel matters.
  alt <- if (nrow(a) && !is.na(pick$km2)) a[a$km2 > 0.2 * pick$km2 & abs(log(a$km2 / pick$km2)) > log(1.25), ]
  ambiguous <- isTRUE(nrow(alt) > 0)
  d <- data.frame(snap_method = method, snap_dist_m = pick$dist, snap_area_km2 = pick$km2, snap_ambiguous = ambiguous,
                  snap_alt_area_km2 = if (ambiguous) max(alt$km2) else NA_real_)
  if (!is.null(ev)) d <- cbind(d, snap_evidence_columns(ev, pick))
  out <- sf::st_sf(d, geometry = sf::st_sfc(sf::st_point(c(pick$x, pick$y)), crs = sf::st_crs(pt)))
  if (!is.null(ev)) attr(out, "alternatives") <- ev$alternatives
  out
}

# Is the snapped outlet on the sample point's stream? Following D8 flow paths: either the point drains
# through the outlet (outlet downstream), or the outlet's path reaches the point's path within the snap
# radius of the point (outlet upstream, point on or beside the channel). Two branches meeting at a
# confluence further down, or neighbouring streams, fail both tests.
same_stream <- function(pointer_file, pt_xy, out_xy, snap_dist_m) {
  ptr <- terra::rast(pointer_file)
  cs <- terra::res(ptr)[1]
  k <- ceiling((2 * snap_dist_m + 100) / cs)
  pp <- d8_path(ptr, pt_xy, k)
  po <- d8_path(ptr, out_xy, k)
  meet <- pp[pp %in% po][1]
  if (is.na(meet)) return(FALSE)
  match(meet, po) == 1 || match(meet, pp) <= ceiling(snap_dist_m / cs) + 1
}

# Which outer rows or columns of the window the watershed reaches: "w", "s", "e", "n"
frame_sides <- function(ws) {
  nr <- terra::nrow(ws)
  nc <- terra::ncol(ws)
  hit <- \(cells) any(!is.na(ws[cells][, 1]))
  sides <- c(w = hit(terra::cellFromRowCol(ws, seq_len(nr), 1)), s = hit(terra::cellFromRowCol(ws, nr, seq_len(nc))),
             e = hit(terra::cellFromRowCol(ws, seq_len(nr), nc)), n = hit(terra::cellFromRowCol(ws, 1, seq_len(nc))))
  names(sides)[sides]
}

touches_frame <- function(ws) length(frame_sides(ws)) > 0

# Does the watershed border cells without elevation data (other than near the outlet, where the sea or
# a lake may legitimately start)? Widening the window cannot fix that, so it is reported, not chased.
nodata_contact <- function(ws, dem, snapped, tol_m) {
  if (!isTRUE(terra::global(is.na(dem), "max", na.rm = TRUE)[1, 1] > 0)) return(FALSE)
  near_na <- terra::focal(is.na(dem), w = 3, fun = "max", na.rm = TRUE)
  cells <- terra::cells(terra::mask(near_na, ws), 1)[[1]]
  if (!length(cells)) return(FALSE)
  xy <- terra::xyFromCell(ws, cells)
  o <- sf::st_coordinates(snapped)[1, ]
  any(sqrt((xy[, 1] - o[[1]])^2 + (xy[, 2] - o[[2]])^2) > tol_m)
}

# Keep the window's rasters for later use (topography, outlet slope, plots), cropped to the catchment plus
# a margin and compressed, in a folder named by DEM, depression method and extent so different runs never collide
save_rasters <- function(f, poly, site_code, dem_tag, depressions = "breach") {
  r <- terra::rast(f$dem)
  margin <- max(500, 0.15 * sqrt(as.numeric(sf::st_area(poly))), 10 * terra::res(r)[1])
  e <- terra::intersect(terra::align(terra::ext(terra::vect(sf::st_buffer(poly, margin))), r), terra::ext(r))
  v <- as.vector(e)
  key <- sprintf("%s_%s_%.0f_%.0f_%.0fx%.0f", gsub("[^A-Za-z0-9.]+", "_", dem_tag %||% "dem"), depressions, v[["xmin"]], v[["ymin"]],
                 v[["xmax"]] - v[["xmin"]], v[["ymax"]] - v[["ymin"]])
  keep <- ak_cache_dir("delineation", gsub("[^A-Za-z0-9._-]+", "_", site_code), key)
  out <- stats::setNames(file.path(keep, c("dem.tif", "dem_breached.tif", "d8_pointer.tif", "d8_accum.tif")),
                         c("dem", "filled", "pointer", "accum"))
  for (k in names(out)) {
    terra::crop(terra::rast(f[[k]]), e, filename = out[[k]], overwrite = TRUE,
                gdal = c("COMPRESS=DEFLATE"), datatype = if (k == "pointer") "INT2S" else "FLT4S")
  }
  as.list(out)
}

watershed_raster <- function(snapped, f, dir, tag = "outlet") {
  tag <- gsub("[^A-Za-z0-9_-]+", "_", tag)
  pp <- file.path(dir, paste0("pour_point_", tag, ".shp"))
  sf::st_write(sf::st_sf(id = 1L, geometry = sf::st_geometry(snapped)), pp, delete_dsn = file.exists(pp), quiet = TRUE)
  out <- file.path(dir, paste0("watershed_", tag, ".tif"))
  unlink(out)                                                     # not the last window's, if WhiteboxTools fails
  whitebox::wbt_watershed(f$pointer, pp, out)
  ws <- terra::rast(out)
  terra::crs(ws) <- terra::crs(terra::rast(f$dem))
  terra::ifel(ws > 0, 1, NA)
}

dem_streams <- function(f, threshold_km2, dir) {
  acc <- terra::rast(f$accum)
  thr <- threshold_km2 * 1e6 / prod(terra::res(acc))
  s <- file.path(dir, "streams.tif")
  v <- file.path(dir, "streams.shp")
  whitebox::wbt_extract_streams(f$accum, s, threshold = thr)
  whitebox::wbt_raster_streams_to_vector(s, f$pointer, v)
  x <- sf::st_read(v, quiet = TRUE)
  sf::st_crs(x) <- sf::st_crs(akalbers_crs())
  sf::st_sf(stream_source = "dem", geometry = sf::st_geometry(x))
}

catchment_diagnostics <- function(poly, dem, snapped) {
  v <- terra::vect(poly)
  d <- terra::mask(terra::crop(dem, v), v)
  s <- terra::values(terra::terrain(d, "slope", unit = "degrees"), na.rm = TRUE)
  z <- terra::values(d, na.rm = TRUE)
  a <- as.numeric(sf::st_area(poly))
  p <- as.numeric(sf::st_length(sf::st_boundary(poly)))
  dplyr::tibble(area_km2 = a / 1e6, elev_min_m = min(z), elev_mean_m = mean(z), relief_m = max(z) - min(z),
                 slope_mean_deg = mean(s), flat_frac = mean(s < 1), compactness = 4 * pi * a / p^2,
                 n_parts = n_parts(poly, terra::res(dem)[1]),
                 outlet_boundary_m = as.numeric(sf::st_distance(snapped, sf::st_boundary(poly)))[1])
}

# Parts of a polygon, counting pieces that touch only at a corner as one: a D8 watershed is connected
# through diagonal steps, which become corner contacts when the raster is turned into polygons
n_parts <- function(poly, cell) {
  g <- sf::st_union(sf::st_buffer(poly, cell / 100))
  length(sf::st_cast(sf::st_cast(g, "MULTIPOLYGON"), "POLYGON"))
}

flowline_diagnostics <- function(poly, snapped, flowlines, outlet_tol_m = 250) {
  fl <- sf::st_union(sf::st_geometry(sf::st_transform(flowlines, akalbers_crs())))
  x <- suppressWarnings(sf::st_intersection(sf::st_boundary(poly), fl))
  co <- if (length(x) && !all(sf::st_is_empty(x))) sf::st_coordinates(x)[, c("X", "Y"), drop = FALSE] else matrix(numeric(0), 0, 2)
  d_out <- sqrt((co[, 1] - sf::st_coordinates(snapped)[1, 1])^2 + (co[, 2] - sf::st_coordinates(snapped)[1, 2])^2)
  dplyr::tibble(flowline_dist_m = as.numeric(sf::st_distance(snapped, fl))[1],
                 flowline_crossings = sum(d_out > outlet_tol_m),
                 flowline_km_inside = as.numeric(sum(sf::st_length(suppressWarnings(sf::st_intersection(fl, poly))))) / 1000)
}

#' Delineation flags
#'
#' Every delineation ([ak_delineate()], [ak_catchments()]) carries its
#' diagnostics as columns and turns them into flags: `flags` lists them
#' (semicolon-separated), and any failure sets `delin_ok` to `FALSE`;
#' warnings only appear in `flags`. How well each flag picks out the
#' delineations that disagree with published USGS drainage areas and
#' StreamStats is reported in the package paper's validation.
#'
#' Failures: `truncated` (catchment still touches the largest window),
#' `nodata_edge` (catchment runs into cells without elevation data away from
#' the outlet, so it is cut off: the edge of the DEM's coverage, such as the
#' Canadian border for IfSAR; try another `dem_source`),
#' `no_channel` (no channel cell within the snap radius), `area_mismatch`
#' (area more than 1.5x off `ref_area_km2`), `multipart` (polygon in several
#' pieces; pieces touching only at a corner, as D8 watersheds do, count as
#' one), `tiny` (< 0.05 km2).
#'
#' Warnings: `snap_at_limit` (snapped at >= 90% of the radius),
#' `snap_ambiguous` (another channel in reach whose contributing area differs
#' from the chosen one by more than 25%, such as the other branch of a
#' confluence; its area is in `snap_alt_area_km2`), `snap_off_stream` (by
#' the D8 flow paths, the snapped outlet is not on the sample point's
#' stream: the point doesn't drain through it, and it doesn't drain past the
#' point within the snap radius; the paths meet only further down, at a
#' confluence, or not at all), `dem_fallback` (the first DEM failed and
#' `fallback_dem` was used), `snap_conflict` (with `snap = "consensus"`: the
#' reference network's area fits a channel other than the one the flow path
#' chose, so the two disagree),
#' `low_relief` (> 30% of cells under 1 degree, or relief < 50 m: D8 routing
#' is unreliable), `flowline_far` (snapped outlet > 100 m from the reference
#' flowline), `flowline_crossings` (a reference flowline crosses the divide
#' away from the outlet: either the polygon or the flowline is wrong).
#'
#' @name delineation_flags
#' @seealso `vignette("delineation")`
NULL

# `x` with `flags` and `delin_ok` from its diagnostic columns
flag_delineation <- function(x, snap_dist_m = 150) {
  g <- \(col, default = NA) if (col %in% names(x)) x[[col]] else rep(default, nrow(x))
  fail <- cbind(truncated = isTRUE_v(g("truncated")),
                nodata_edge = isTRUE_v(g("nodata_edge")),
                no_channel = isTRUE_v(g("snap_method", "") == "none"),
                area_mismatch = isTRUE_v(abs(log(g("area_km2") / g("ref_area_km2"))) > log(1.5)),
                multipart = isTRUE_v(g("n_parts", 1) > 1),
                tiny = isTRUE_v(g("area_km2") < 0.05))
  warn <- cbind(snap_at_limit = isTRUE_v(g("snap_dist_m") >= 0.9 * snap_dist_m),
                snap_ambiguous = isTRUE_v(g("snap_ambiguous")),
                snap_off_stream = isTRUE_v(!g("snap_path_ok", TRUE)),
                snap_conflict = isTRUE_v(g("snap_conflict")),
                dem_fallback = isTRUE_v(g("dem_fallback")),
                low_relief = isTRUE_v(g("flat_frac") > 0.3 | g("relief_m") < 50),
                flowline_far = isTRUE_v(g("flowline_dist_m") > 100),
                flowline_crossings = isTRUE_v(g("flowline_crossings") > 0))
  all_f <- cbind(fail, warn)
  x$flags <- apply(all_f, 1, \(r) paste(colnames(all_f)[r], collapse = ";"))
  x$delin_ok <- !apply(fail, 1, any)
  x
}

isTRUE_v <- function(x) !is.na(x) & x
