# Conditioned DEM window for one catchment (used by import_catchments()): the DEM for the polygon plus
# `buffer_m`, masked to that buffer (so flow accumulation sees only the catchment and its margin), then
# least-cost breaching, D8 pointer and D8 accumulation. Breaching rather than filling: filled flats give
# arbitrary flow directions, which would corrupt the upstream walk in ak_outlet_slope(). Files are cached in
# scratch/<site_code>/, named by DEM source (and res and buffer when not the defaults), as in the geo workflow.
condition_one <- function(poly, dem_source = "ifsar", res = NULL, dem_dir = NULL, buffer_m = 500, refresh = FALSE) {
  tag <- paste0(switch(dem_source, local = "tiles", elevatr = "elevatr_z12", dem_source),
                if (!is.null(res)) paste0("_", res, "m"), if (buffer_m != 500) paste0("_buf", buffer_m))
  wd <- ak_cache_dir("scratch", poly$site_code)
  f <- list(dem = file.path(wd, sprintf("dem_%s.tif", tag)), filled = file.path(wd, sprintf("dem_breached_%s.tif", tag)),
            pointer = file.path(wd, sprintf("d8_pointer_%s.tif", tag)), accum = file.path(wd, sprintf("d8_accum_%s.tif", tag)))
  new_dem <- refresh || !file.exists(f$dem)
  if (new_dem) {
    win <- sf::st_buffer(sf::st_geometry(poly), buffer_m)
    d <- terra::mask(ak_dem(win, dem_source, res = res, dir = dem_dir), terra::vect(win))
    terra::writeRaster(d, f$dem, overwrite = TRUE, NAflag = -9999, datatype = "FLT4S")
  }
  if (new_dem || !all(file.exists(unlist(f[-1])))) {
    whitebox::wbt_breach_depressions_least_cost(f$dem, f$filled, dist = 100, fill = TRUE)
    whitebox::wbt_d8_pointer(f$filled, f$pointer)
    whitebox::wbt_d8_flow_accumulation(f$filled, f$accum, out_type = "cells")
  }
  data.frame(site_code = poly$site_code, dem_source = tag, dem_res_m = terra::res(terra::rast(f$dem))[1], f)
}

# The conditioned rasters for each catchment: those carried by catchments from ak_catchments() or
# import_catchments(), else built now (with a message)
site_rasters <- function(catchments, x, ...) {
  r <- carried_rasters(catchment_sf(catchments))
  if (!is.null(r)) return(r)
  message("The catchments carry no conditioned DEM; building it (import_catchments() keeps it for reuse)")
  carried_rasters(import_catchments(x, ...)) %||% stop("No conditioned DEM could be built for any site", call. = FALSE)
}

#' Channel gradient at the outlet
#'
#' From the sample point snapped onto the channel whose contributing area
#' best matches the catchment, walks upstream along the D8 network,
#' following the neighbour with the most flow at each step and accumulating
#' true along-channel distance (cell size, or cell size x sqrt(2) on
#' diagonals), until `reach_m` is reached (the final step is interpolated to
#' land on it). Gradient = elevation drop / distance, on the breached DEM.
#' This is the reach-based approach of the stream-power literature; a
#' moving-window DEM slope measures hillslope and understates channel
#' gradient. Where the walk reaches a channel head first, the slope uses the
#' distance actually traced (`reached_target = FALSE`).
#'
#' The stream power index at the outlet is ln(A x tan(slope)), with A the
#' contributing area in m2.
#'
#' Uncertainty: slope over 100 m on a 5 m DEM is sensitive to DEM noise and to
#' the snap (`snap_dist_m`, `contrib_area_km2` against `catch_area_km2`);
#' compare the 100 and 200 m reaches. On coarse DEMs (elevatr) treat it as
#' rough.
#'
#' @param catchments Catchments: sf polygons with `site_code`, one row per
#'   catchment (any CRS; from [ak_catchments()] or your own), an
#'   `ak_delineation`, or a list of them.
#' @param points sf sample points with `site_code`, or a data frame with
#'   `site_code`, `lat`, `lon`. Defaults to the `lat`/`lon` columns of
#'   `catchments` (as returned by [ak_catchments()]).
#' @param reach_m Reach lengths, metres.
#' @param snap_dist_m,stream_threshold_km2 Snapping radius and channel
#'   threshold, as in [ak_delineate()].
#' @param ... Passed to [import_catchments()] when the catchments carry no
#'   conditioned DEM (`dem_source`, `res`, `buffer_m`).
#' @return `catchments` with columns added: `outlet_slope_m_m` and
#'   `outlet_slope_deg` for the first reach length, `outlet_slope_m_m_<L>` for
#'   the others, `outlet_elev_m`, `outlet_elev_drop_m`, `outlet_reach_m`,
#'   `outlet_reach_actual_m`, `outlet_reach_complete` (the walk reached the
#'   full length), `outlet_snap_dist_m`, `outlet_snap_method`,
#'   `outlet_contrib_area_km2`, `outlet_catch_area_km2`, `outlet_spi` and
#'   `outlet_dem_source`. The catchments need their conditioned DEM: those
#'   from [ak_catchments()] and [import_catchments()] carry it; for others it
#'   is built.
#' @seealso `vignette("topography")` (download, use, uncertainty, validation)
#' @export
ak_outlet_slope <- function(catchments, points = NULL, reach_m = c(100, 200), snap_dist_m = 150,
                            stream_threshold_km2 = 0.05, ...) {
  x <- as_catchments(catchments)
  pts <- as_points(points %||% catchment_points(catchment_sf(catchments)))
  rs <- site_rasters(catchments, x, ...)
  sites <- intersect(x$site_code, intersect(pts$site_code, rs$site_code))
  items <- lapply(sites, \(sc) list(site_code = sc, catch_km2 = sum(x$area_m2[x$site_code == sc]) / 1e6,
                                    pt = pts[pts$site_code == sc, ][1, ], f = rs[rs$site_code == sc, ][1, ]))
  res <- site_map(items, outlet_slope_one, reach_m = reach_m, snap_dist_m = snap_dist_m, thr = stream_threshold_km2,
                  ids = sites, label = "Outlet slope")
  long <- purrr::list_rbind(site_values(res))
  add_columns(catchments, outlet_wide(long %||% dplyr::tibble()), layer_row("outlet_slope", fn = "ak_outlet_slope"),
              list(reach_m = reach_m, snap_dist_m = snap_dist_m, stream_threshold_km2 = stream_threshold_km2, ...),
              errors = site_errors(res, "outlet_slope"))
}

outlet_slope_one <- function(it, reach_m, snap_dist_m, thr) {
  f <- it$f
  sc <- it$site_code
  snapped <- snap_point(it$pt, f$accum, snap_dist_m, it$catch_km2, thr, "largest")
  dem <- terra::rast(f$filled)
  xy <- sf::st_coordinates(snapped)[1, ]
  cs <- terra::res(dem)[1]
  # The walk never leaves max(reach_m) of the outlet: read only that neighbourhood, not the whole window
  rad <- 1.5 * max(reach_m) + 3 * cs
  win <- terra::intersect(terra::ext(xy[[1]] - rad, xy[[1]] + rad, xy[[2]] - rad, xy[[2]] + rad), terra::ext(dem))
  if (is.null(win)) stop("snapped outlet outside the DEM window")
  rs <- lapply(list(ptr = f$pointer, acc = f$accum, dem = f$filled), \(r) terra::crop(terra::rast(r), win, snap = "out"))
  cell <- terra::cellFromXY(rs$dem, rbind(xy))
  if (is.na(cell)) stop("snapped outlet outside the DEM window")
  rc <- terra::rowColFromCell(rs$dem, cell)
  m <- lapply(rs, \(r) terra::as.matrix(r, wide = TRUE))
  z0 <- m$dem[rc[1], rc[2]]
  a0 <- m$acc[rc[1], rc[2]]
  purrr::map(reach_m, \(L) {
    w <- walk_upstream(rc[1], rc[2], m$ptr, m$acc, m$dem, cs, L)
    drop <- w$z_end - z0
    s <- drop / max(w$dist, cs)
    data.frame(site_code = sc, reach_length_m = L, actual_length_m = round(w$dist, 1), reached_target = w$reached,
               elev_drop_m = round(drop, 3), slope_m_m = s, slope_deg = atan(s) * 180 / pi)
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(outlet_elev_m = round(z0, 2), snap_dist_m = round(snapped$snap_dist_m, 1), snap_method = snapped$snap_method,
                  contrib_area_km2 = a0 * cs^2 / 1e6, catch_area_km2 = it$catch_km2,
                  spi = log(pmax(a0 * cs^2, 1) * pmax(.data$slope_m_m, 1e-6)),
                  dem_source = f$dem_source, dem_res_m = round(cs, 2))
}

# Conditioned rasters carried by catchments from ak_catchments() (columns dem_file ... accum_file)
carried_rasters <- function(x) {
  cols <- c(dem = "dem_file", filled = "filled_file", pointer = "pointer_file", accum = "accum_file")
  if (!inherits(x, "data.frame") || !all(cols %in% names(x))) return(NULL)
  d <- sf::st_drop_geometry(x)
  out <- data.frame(site_code = d$site_code, dem_source = d$dem_source %||% NA_character_,
                    dem_res_m = d$dem_res_m %||% NA_real_, stats::setNames(d[cols], names(cols)))
  out <- out[stats::complete.cases(out[names(cols)]) & file.exists(dplyr::coalesce(out$dem, "")), ]
  if (nrow(out)) out
}

catchment_points <- function(x) {
  if (!all(c("site_code", "lat", "lon") %in% names(x))) stop("`points` is needed: the catchments have no `lat` and `lon` columns")
  sf::st_drop_geometry(x)[c("site_code", "lat", "lon")]
}

# WhiteboxTools D8 pointer: the direction flow LEAVES a cell, clockwise from NE
#   64 128   1
#   32   0   2
#   16   8   4
d8 <- data.frame(code = c(1, 2, 4, 8, 16, 32, 64, 128), dr = c(-1, 0, 1, 1, 1, 0, -1, -1), dc = c(1, 1, 1, 0, -1, -1, -1, 0))

# Neighbours that flow into (r, c): their pointer points at it
upstream_neighbours <- function(r, c, ptr) {
  out <- list()
  for (i in seq_len(nrow(d8))) {
    nr <- r - d8$dr[i]
    nc <- c - d8$dc[i]
    if (nr < 1 || nc < 1 || nr > nrow(ptr) || nc > ncol(ptr)) next
    v <- ptr[nr, nc]
    if (!is.na(v) && v == d8$code[i]) out[[length(out) + 1]] <- c(nr, nc, d8$dr[i] != 0 && d8$dc[i] != 0)
  }
  out
}

walk_upstream <- function(r, c, ptr, acc, dem, cellsize, target_m) {
  dist <- 0
  repeat {
    ups <- upstream_neighbours(r, c, ptr)
    if (!length(ups)) break
    best <- ups[[which.max(vapply(ups, \(u) acc[u[1], u[2]], numeric(1)))]]
    step <- if (best[3] == 1) cellsize * sqrt(2) else cellsize
    if (dist + step > target_m) {
      frac <- (target_m - dist) / step
      return(list(dist = target_m, z_end = dem[r, c] + frac * (dem[best[1], best[2]] - dem[r, c]), reached = TRUE))
    }
    r <- best[1]
    c <- best[2]
    dist <- dist + step
    if (dist >= target_m) break
  }
  list(dist = dist, z_end = dem[r, c], reached = dist >= target_m)
}

#' Catchment topography
#'
#' Elevation, slope, flow-weighted slope, channel slope, wetness, aspect and
#' heat load, summarised over each catchment from its DEM window:
#' * elevation mean, min, max and relief;
#' * cell slope mean, min and max at the native resolution, and the mean at
#'   10 m and 30 m (slope is scale-dependent, so these show how much; `NA`
#'   when the DEM is coarser than that, the native mean when it is equal);
#' * flow-weighted slope `topo_fws_pct` = sum(slope % x accumulation) /
#'   sum(accumulation) (Shaftel et al. 2011);
#' * `topo_chan_slope_pct`, mean slope of channel cells (contributing area at
#'   least `stream_threshold_km2`);
#' * `topo_wetx`, mean ln(a / tan(beta)) with a = accumulation x cell size and
#'   tan(beta) floored at 0.001;
#' * aspect as mean northness and eastness, their resultant length and mean
#'   direction; heat load index (McCune and Keon 2002, eq. 3).
#'
#' Uncertainty: IfSAR vertical accuracy is 3 m (90% confidence) on 0-10
#' degree slopes; elevation means are insensitive to it, but slope depends on
#' resolution, so `topo_dem_res_m` and the 10 m and 30 m slopes are reported.
#' `topo_flag`: `few_cells` where the catchment has under 100 cells.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own), an
#'   `ak_delineation`, or a list of them.
#' @param dem,accum Alternatively one DEM (and optional D8 accumulation) for
#'   all catchments, e.g. a statewide or regional grid you already have.
#' @param stream_threshold_km2 Channel threshold for `topo_chan_slope_pct`.
#' @param ... Passed to [import_catchments()] when the catchments carry no
#'   conditioned DEM and `dem` is not given.
#' @return `catchments` with the `topo_*` columns added.
#' @seealso `vignette("topography")` (download, use, uncertainty, validation)
#' @export
ak_topography <- function(catchments, dem = NULL, accum = NULL, stream_threshold_km2 = 0.05, ...) {
  y <- as_catchments(catchments)
  items <- if (!is.null(dem)) {
    lapply(seq_len(nrow(y)), \(i) list(g = y[i, ], dem = dem, accum = accum, src = dem_source(dem)))
  } else {
    rs <- site_rasters(catchments, y, ...)
    lapply(seq_len(nrow(y)), \(i) {
      f <- rs[rs$site_code == y$site_code[i], ][1, ]
      if (is.na(f$dem)) return(NULL)
      list(g = y[i, ], dem = f$dem, accum = f$accum, src = f$dem_source)
    })
  }
  names(items) <- y$site_code
  items <- purrr::compact(items)
  res <- site_map(items, topo_item, thr = stream_threshold_km2, label = "Topography", parallel = is.null(dem))
  add_columns(catchments, purrr::list_rbind(site_values(res)) %||% dplyr::tibble(site_code = character()),
              layer_row("topography", fn = "ak_topography"), list(stream_threshold_km2 = stream_threshold_km2, ...),
              errors = site_errors(res, "topography"))
}

# Raster paths (or, for a user DEM, the rasters themselves: those stay in this session)
topo_item <- function(it, thr) {
  r <- \(f) if (is.null(f) || inherits(f, "SpatRaster")) f else terra::rast(f)
  topo_one(it$g, r(it$dem), r(it$accum), it$src, thr)
}

topo_one <- function(g, dem, acc, src, thr) {
  g <- sf::st_transform(g, terra::crs(dem))
  ee <- \(r, fun, ...) exactextractr::exact_extract(r, g, fun, progress = FALSE, ...)
  cs <- terra::res(dem)[1]
  slp <- terra::terrain(dem, "slope", unit = "degrees")
  asp <- terra::terrain(dem, "aspect", unit = "radians")
  pct <- tan(slp * pi / 180) * 100
  lat <- sf::st_coordinates(sf::st_transform(sf::st_centroid(sf::st_geometry(g)), 4326))[1, 2] * pi / 180
  fold <- abs(pi - abs(terra::ifel(is.na(asp), 0, asp) - 5 * pi / 4))
  sr <- slp * pi / 180
  hli <- 0.339 + 0.808 * cos(lat) * cos(sr) - 0.196 * sin(lat) * sin(sr) - 0.482 * cos(fold) * sin(sr)
  nrth <- ee(cos(asp), "mean")
  est <- ee(sin(asp), "mean")
  coarse <- purrr::map_dbl(c(10, 30), \(r) {
    if (r < cs - 1e-6) return(NA_real_)                 # finer than the DEM: unknown
    if (r < cs + 1e-6) return(ee(slp, "mean"))          # the DEM's own resolution
    terra::project(dem, terra::rast(terra::ext(dem), resolution = r, crs = terra::crs(dem)), method = "average") |>
      terra::terrain("slope", unit = "degrees") |>
      ee("mean")
  })
  flow <- if (is.null(acc)) list(fws = NA_real_, chan = NA_real_, wetx = NA_real_) else {
    terra::crs(acc) <- terra::crs(dem)
    list(fws = ee(pct, "weighted_mean", weights = acc),
         chan = ee(terra::ifel(acc * cs^2 / 1e6 >= thr, pct, NA), "mean"),
         wetx = ee(log(acc * cs / terra::clamp(tan(slp * pi / 180), lower = 0.001)), "mean"))
  }
  ncell <- ee(dem, "count")
  dplyr::tibble(site_code = g$site_code, topo_dem_source = src, topo_dem_res_m = cs, topo_ncell = ncell,
                topo_area_km2 = as.numeric(sf::st_area(g)) / 1e6, topo_elev_mean_m = ee(dem, "mean"),
                topo_elev_min_m = ee(dem, "min"), topo_elev_max_m = ee(dem, "max"),
                topo_slope_mean_deg = ee(slp, "mean"), topo_slope_min_deg = ee(slp, "min"), topo_slope_max_deg = ee(slp, "max"),
                topo_slope_mean_deg_10m = coarse[1], topo_slope_mean_deg_30m = coarse[2],
                topo_fws_pct = flow$fws, topo_chan_slope_pct = flow$chan, topo_wetx = flow$wetx,
                topo_northness = nrth, topo_eastness = est, topo_aspect_mean_deg = (atan2(est, nrth) * 180 / pi) %% 360,
                topo_aspect_r = sqrt(nrth^2 + est^2), topo_hli = ee(hli, "mean"),
                topo_flag = flag_text(few_cells = ncell < 100)) |>
    dplyr::mutate(topo_relief_m = .data$topo_elev_max_m - .data$topo_elev_min_m, .after = "topo_elev_max_m")
}
# Outlet slope, one row per site, as in CAKN-Data step 07: the first reach length is the primary one
outlet_wide <- function(sl) {
  if (!nrow(sl)) return(dplyr::tibble(site_code = character()))
  first <- sl$reach_length_m == sl$reach_length_m[1]
  main <- sl[first, ] |>
    dplyr::transmute(.data$site_code, outlet_elev_m = .data$outlet_elev_m, outlet_snap_dist_m = .data$snap_dist_m,
                     outlet_snap_method = .data$snap_method, outlet_contrib_area_km2 = .data$contrib_area_km2,
                     outlet_catch_area_km2 = .data$catch_area_km2, outlet_slope_m_m = .data$slope_m_m,
                     outlet_slope_deg = .data$slope_deg, outlet_elev_drop_m = .data$elev_drop_m,
                     outlet_reach_m = .data$reach_length_m, outlet_reach_actual_m = .data$actual_length_m,
                     outlet_reach_complete = .data$reached_target, outlet_spi = .data$spi, outlet_dem_source = .data$dem_source)
  if (all(first)) return(main)
  other <- sl[!first, ] |>
    tidyr::pivot_wider(id_cols = "site_code", names_from = "reach_length_m", values_from = "slope_m_m", names_prefix = "outlet_slope_m_m_")
  dplyr::left_join(main, other, by = "site_code")
}
