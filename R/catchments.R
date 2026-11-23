#' Catchments for stream points: delineated, checked, plotted
#'
#' The entry point of the package. Give it sample points on streams and it
#' delineates the catchment draining to each one ([ak_delineate()]), flags
#' likely failures ([delineation_flags]), writes a check plot on satellite
#' imagery for every site, and returns one sf polygon per site. Pass the
#' result to the layer functions ([ak_topography()], [ak_landcover()],
#' [ak_alder()], ...), which append their columns, and to [ak_obs_link()].
#'
#' Any column of `points` named like an argument of [ak_delineate()]
#' (`ref_area_km2`, `snap_dist_m`, `snap`, `dem_source`, `res`,
#' `max_buffer_m`, ...) sets that argument for its row, overriding `...`, so
#' one table can carry per-site corrections. A failure at one site is
#' recorded in `attr(x, "errors")` and does not stop the others.
#'
#' **Regional windows** (`regional = TRUE`). Nearby points (within
#' `cluster_km` of each other, and with the same DEM settings) share one DEM
#' window: it is downloaded and conditioned once, and every point in it is
#' snapped and delineated on the shared rasters. A point whose catchment
#' reaches the shared window's edge, or that fails there, is delineated again
#' on its own with the usual expanding windows, so the result for each site
#' is the same kind of delineation either way. For networks of sampling sites
#' along the same rivers this saves most of the DEM download and
#' conditioning time. The shared window is at most `max_cells` cells a side
#' (as for a single site), so the DEM resolution is the same as on the
#' per-site path. `window_mode` records which path each site took. Compare
#' the two paths on your sites before relying on it: breaching a larger
#' window can, rarely, route a flat area differently.
#'
#' **Progress and parallel processing**: a progress bar names the site (or
#' cluster) being delineated. With mirai daemons running
#' (`mirai::daemons(n)`), sites (or clusters) are delineated in parallel; see
#' [ak_parallel]. The step timings on each row (`t_dem_s`, `t_condition_s`,
#' ...) show where the time goes.
#'
#' Look at the plots (`plot_file`) before using the polygons. Sites with
#' `delin_ok = FALSE` need attention: a reference area (`ref_area_km2`), a
#' larger snap radius, another DEM, or a hand-drawn polygon (bring those in
#' with [import_catchments()]).
#'
#' @param points A data frame with `site_code`, `lat` and `lon` (decimal
#'   degrees; `latitude`/`longitude` also accepted), or sf points in any CRS
#'   with `site_code`.
#' @param ... Arguments for [ak_delineate()] that apply to every site.
#' @param crs The datum of `lat`/`lon` in a data frame: 4326 (WGS84, default),
#'   4269 (NAD83, within a metre or two of WGS84) or 4267 (NAD27, about 100-200
#'   m off in Alaska: older USGS and agency site lists often use it). Points
#'   are converted to WGS84; a warning is given if PROJ cannot do the NAD27
#'   shift (missing NADCON grid).
#' @param regional Share DEM windows between nearby points?
#' @param cluster_km Largest distance between two points sharing a window.
#'   `NULL`: as large as fits in one window at the DEM's resolution
#'   (`max_cells` x resolution, less the starting buffer on each side; 20 km
#'   for 5 m IfSAR).
#' @param plot Write the check plots?
#' @param plot_dir Folder for the plots.
#' @param keep_delineations Also keep each full `ak_delineation` (streams,
#'   flowlines, snapped points) in `attr(x, "delineations")`, e.g. to redraw
#'   with [plot_catchment()] and a reference outline.
#' @return An `ak_catchments` object: sf polygons (EPSG:3338), one row per
#'   delineated site, with the [ak_delineate()] diagnostics, flags and step
#'   timings, the snapped outlet (`snapped_lon`, `snapped_lat`), `plot_file`,
#'   and the paths of the conditioned rasters (`dem_file`, `filled_file`,
#'   `pointer_file`, `accum_file`) that [ak_topography()] and
#'   [ak_outlet_slope()] reuse. Attributes: `errors` (`site_code`, `step`,
#'   `error`), `ak_provenance` (see [ak_provenance()]).
#' @examples
#' \dontrun{
#' pts <- data.frame(site_code = "LSR", lat = 61.7089, lon = -149.2267)
#' x <- ak_catchments(pts, snap_dist_m = 300) |>
#'   ak_topography() |>
#'   ak_landcover()
#' ak_provenance(x)
#' }
#' @seealso `vignette("delineation")`, `vignette("workflow")`
#' @export
ak_catchments <- function(points, ..., crs = 4326, regional = FALSE, cluster_km = NULL, plot = TRUE,
                          plot_dir = "catchment_plots", keep_delineations = FALSE) {
  pts <- point_table(points, crs)
  common <- list(...)
  if ("workers" %in% names(common)) stop("`workers` is gone: start mirai daemons instead, e.g. mirai::daemons(4); see ?ak_parallel", call. = FALSE)
  arg_names <- setdiff(names(formals(ak_delineate)), c("lat", "lon", "site_code", "plot", "plot_dir"))
  bad <- setdiff(names(common), arg_names)
  if (length(bad)) stop("Not arguments of ak_delineate(): ", paste(bad, collapse = ", "), call. = FALSE)
  per_row <- intersect(setdiff(names(pts), c("site_code", "lat", "lon")), arg_names)
  jobs <- lapply(seq_len(nrow(pts)), \(i) {
    own <- lapply(pts[i, per_row, drop = FALSE], \(v) v[[1]])
    own <- own[!vapply(own, \(v) length(v) == 1 && is.na(v), TRUE)]
    utils::modifyList(c(common, list(plot = plot, plot_dir = plot_dir)),
                      c(list(lat = pts$lat[i], lon = pts$lon[i], site_code = pts$site_code[i]), own))
  })
  names(jobs) <- pts$site_code
  res <- if (regional) {
    groups <- cluster_jobs(jobs, cluster_km)
    message(length(jobs), " sites in ", length(groups), " DEM windows (", sum(lengths(groups) > 1), " shared)")
    by_site <- list()
    for (o in site_map(groups, delineate_cluster, label = "Delineating (regional)")) if (is.null(o$error)) by_site[names(o$value)] <- o$value
    # a group that failed as a whole: its sites one by one
    miss <- setdiff(pts$site_code, names(by_site))
    if (length(miss)) by_site[miss] <- site_map(jobs[miss], delineate_job, label = "Delineating")
    by_site[pts$site_code]
  } else {
    site_map(jobs, delineate_job, label = "Delineating")
  }
  names(res) <- pts$site_code
  ok <- vapply(res, \(r) inherits(r$value, "ak_delineation"), TRUE)
  errors <- dplyr::tibble(site_code = pts$site_code[!ok], step = "delineation",
                          error = vapply(res[!ok], \(r) r$error %||% "unknown error", ""))
  if (!any(ok)) stop("No site could be delineated:\n", paste(errors$site_code, errors$error, sep = ": ", collapse = "\n"), call. = FALSE)
  dels <- purrr::map(res[ok], "value")
  x <- dplyr::bind_rows(purrr::map(dels, delineation_row))
  dem <- unique(stats::na.omit(x$dem_source))
  src <- unique(sub("_[0-9.]+m$", "", dem))
  prov <- purrr::map(src, \(v) provenance_row(layer_row("catchment", if (v %in% ak_layers("catchment")$variant) v, fn = "ak_catchments"),
                                              columns = setdiff(names(x), attr(x, "sf_column")),
                                              args = deparse_args(c(common, regional = regional)),
                                              note = paste("DEM:", paste(dem[startsWith(dem, v)], collapse = ", ")))) |>
    dplyr::bind_rows()
  x <- new_ak_catchments(x, prov, NULL, if (keep_delineations) dels)
  add_errors(x, errors, nrow(pts))
}

# ---- regional windows ---------------------------------------------------------------------------------

# Group delineation jobs that can share a DEM window: same DEM settings, and all points within `cluster_km`
# of each other (complete linkage, so a group's bounding box is at most that wide)
cluster_jobs <- function(jobs, cluster_km = NULL) {
  dflt <- formals(ak_delineate)
  get <- \(a, k) a[[k]] %||% eval(dflt[[k]])
  key <- vapply(jobs, \(a) paste(get(a, "dem_source"), deparse(a$res), deparse(a$dem_dir), get(a, "max_cells"),
                                 get(a, "stream_threshold_km2"), deparse(a$flowlines %||% "3dhp"), deparse(a$fallback_dem), sep = "|"), "")
  out <- list()
  for (k in unique(key)) {
    jj <- jobs[key == k]
    a1 <- jj[[1]]
    res0 <- a1$res %||% default_res(get(a1, "dem_source"))
    buf <- max(vapply(jj, \(a) a$start_buffer_m %||% start_buffer(a$ref_area_km2 %||% NA_real_, get(a, "dem_source")), 0))
    span <- if (!is.null(cluster_km)) cluster_km * 1000 else if (is.na(res0)) 20000 else res0 * get(a1, "max_cells") - 2 * buf
    xy <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(data.frame(lon = vapply(jj, `[[`, 0, "lon"), lat = vapply(jj, `[[`, 0, "lat")),
                                                          coords = c("lon", "lat"), crs = 4326), akalbers_crs()))
    cl <- if (length(jj) > 1 && span > 0) stats::cutree(stats::hclust(stats::dist(xy), method = "complete"), h = span) else seq_along(jj)
    for (g in unique(cl)) out[[length(out) + 1]] <- jj[cl == g]
  }
  names(out) <- vapply(out, \(g) if (length(g) == 1) g[[1]]$site_code else sprintf("%s+%d", g[[1]]$site_code, length(g) - 1), "")
  out
}

delineate_job <- function(a) do.call(ak_delineate, a)

# Delineate a group of nearby points on one shared DEM window. A single point goes through ak_delineate().
# Points whose catchment reaches the shared window's edge, or that fail on it, are delineated on their own.
delineate_cluster <- function(jobs) {
  one <- \(a) tryCatch(list(value = delineate_job(a)), error = \(e) list(error = conditionMessage(e)))
  if (length(jobs) == 1) return(stats::setNames(list(one(jobs[[1]])), jobs[[1]]$site_code))
  wbt_ready()
  t_start <- Sys.time()
  a1 <- jobs[[1]]
  dflt <- formals(ak_delineate)
  get <- \(a, k) if (k %in% names(a)) a[[k]] else eval(dflt[[k]])
  dem_src <- get(a1, "dem_source")
  res0 <- get(a1, "res") %||% default_res(dem_src)
  pts <- purrr::map(jobs, \(a) site_point(a$site_code, a$lat, a$lon))
  buf <- max(vapply(jobs, \(a) get(a, "start_buffer_m") %||% start_buffer(get(a, "ref_area_km2"), dem_src), 0))
  win <- sf::st_union(sf::st_buffer(dplyr::bind_rows(pts), buf))
  side <- max(diff(sf::st_bbox(win)[c(1, 3)]), diff(sf::st_bbox(win)[c(2, 4)]))
  r <- if (is.na(res0)) NULL else max(res0, side / get(a1, "max_cells"))
  wd <- tempfile("akregion_")
  dir.create(wd)
  on.exit(unlink(wd, recursive = TRUE), add = TRUE)
  t0 <- Sys.time()
  dem <- ak_dem(win, dem_src, res = r, dir = get(a1, "dem_dir"))
  t_dem <- since(t0)
  t0 <- Sys.time()
  f <- ak_condition(dem, wd)
  f$streams <- dem_streams(f, get(a1, "stream_threshold_km2"), wd)
  t_cond <- since(t0)
  n <- length(jobs)
  label <- sprintf("regional:%s+%d", a1$site_code, n - 1)
  out <- purrr::map(jobs, \(a) {
    t0 <- Sys.time()
    res <- tryCatch({
      pt <- site_point(a$site_code, a$lat, a$lon)
      check_dem_at(dem, pt, a$site_code, a$lat, a$lon, dem_src)
      snap <- get(a, "snap")[1]
      reaches <- if (snap %in% c("reference", "consensus")) {
        tryCatch(snap_reaches(pt, get(a, "snap_reference"), get(a, "snap_reference_dist_m")), error = \(e) NULL)
      }
      w <- outlet_watershed(pt, f, wd, get(a, "snap_dist_m"), get(a, "ref_area_km2"), get(a, "stream_threshold_km2"),
                            snap, reaches)
      if (w$truncated) NULL else {
        d <- finish_delineation(a$site_code, a$lat, a$lon, get(a, "ref_area_km2"), pt, w, f, dem, wd, buf = buf,
                                snap_dist_m = get(a, "snap_dist_m"), stream_threshold_km2 = get(a, "stream_threshold_km2"),
                                flowlines = get(a, "flowlines"), plot = get(a, "plot"), plot_dir = get(a, "plot_dir"),
                                times = c(dem = t_dem / n, condition = t_cond / n, watershed = since(t0)), n_windows = 1L,
                                window_mode = label, t_start = t0 - (t_dem + t_cond) / n,
                                snap_alternatives = isTRUE(get(a, "snap_alternatives")), keep_rasters = get(a, "keep_rasters"))
        if (isTRUE(d$catchment$nodata_edge) && !is.null(get(a, "fallback_dem"))) NULL else list(value = d)
      }
    }, error = \(e) NULL)
    # not finished on the shared window: on its own, with expanding windows (and the fallback DEM, if any)
    res %||% one(a)
  })
  names(out) <- vapply(jobs, `[[`, "", "site_code")
  if (isTRUE(getOption("AKCatchments.verbose"))) message(label, ": ", n, " sites in ", round(since(t_start)), " s")
  out
}

#' Bring your own catchment polygons into the package
#'
#' For catchments you did not delineate with [ak_catchments()]: hand-drawn
#' polygons, StreamStats or gaged-basin boundaries, or polygons from another
#' tool. Checks them (one valid polygon per unique `site_code`), fetches the
#' DEM for each polygon plus `buffer_m`, masks it to that buffer and
#' conditions it for flow routing (least-cost breaching, D8 pointer and
#' accumulation), so [ak_topography()] and [ak_outlet_slope()] work on them
#' as on delineated catchments. Returns the same kind of object as
#' [ak_catchments()], ready for every layer function.
#'
#' The conditioned rasters are cached in the cache's `scratch/<site_code>/`
#' folder, named by DEM source (and by `res` and `buffer_m` when these are
#' not the defaults), so reruns reuse them. With the defaults the files are
#' named as in the geo workflow (`dem_tiles.tif` for local tiles,
#' `dem_elevatr_z12.tif` for elevatr), so its windows are reused. Breaching
#' is used rather than filling because filled flats give arbitrary flow
#' directions, which would corrupt the upstream walk in [ak_outlet_slope()].
#'
#' Polygons only, without the DEM (`condition = FALSE`), is enough for every
#' layer except topography and outlet slope.
#'
#' @param catchments sf polygons with a unique `site_code` (any CRS), or
#'   `ak_delineation` objects.
#' @param condition Fetch and condition the DEM?
#' @param dem_source,res,dem_dir Passed to [ak_dem()].
#' @param buffer_m Margin around each catchment, metres.
#' @param refresh Rebuild cached files?
#' @return An `ak_catchments` object: the polygons with their columns (and
#'   any provenance they already carry, with a row for the import added),
#'   `area_km2`, and (with `condition`) `dem_source`, `dem_res_m` and the
#'   raster paths `dem_file`, `filled_file`, `pointer_file`, `accum_file`.
#'   Sites whose DEM fails keep their polygon, without raster paths, and are
#'   listed in `attr(x, "errors")`.
#' @examples
#' \dontrun{
#' hand <- sf::st_read("hand_drawn_catchments.gpkg")
#' x <- import_catchments(hand) |> ak_topography() |> ak_outlet_slope(points = pts)
#' }
#' @seealso `vignette("delineation")`, `vignette("topography")`
#' @export
import_catchments <- function(catchments, condition = TRUE, dem_source = "ifsar", res = NULL, dem_dir = NULL,
                              buffer_m = 500, refresh = FALSE) {
  x0 <- catchment_sf(catchments)
  if (!inherits(x0, "sf") || !"site_code" %in% names(x0)) stop("`catchments` must be sf polygons with a site_code column", call. = FALSE)
  check_catchments(x0)
  y <- as_catchments(x0)
  x <- sf::st_transform(x0[match(y$site_code, x0$site_code), ], akalbers_crs())
  sf::st_geometry(x) <- sf::st_geometry(y)   # made valid, empty geometries dropped
  x$area_km2 <- y$area_m2 / 1e6
  errors <- NULL
  if (condition) {
    wbt_ready()
    items <- stats::setNames(lapply(seq_len(nrow(y)), \(i) y[i, ]), y$site_code)
    res_ <- site_map(items, condition_one, dem_source = dem_source, res = res, dem_dir = dem_dir, buffer_m = buffer_m,
                     refresh = refresh, label = "Conditioning DEM")
    r <- purrr::list_rbind(site_values(res_))
    errors <- site_errors(res_, "condition_dem")
    if (NROW(r)) {
      i <- match(x$site_code, r$site_code)
      x$dem_source <- r$dem_source[i]
      x$dem_res_m <- r$dem_res_m[i]
      for (k in c("dem", "filled", "pointer", "accum")) x[[paste0(k, "_file")]] <- r[[k]][i]
    }
  }
  g <- attr(x, "sf_column")
  x <- x[c(setdiff(names(x), g), g)]
  v <- if (dem_source %in% ak_layers("catchment")$variant) dem_source else "local"
  prov <- provenance_row(layer_row("catchment", v, fn = "import_catchments"), setdiff(names(x), g),
                         deparse_args(list(condition = condition, dem_source = dem_source, res = res, buffer_m = buffer_m)),
                         note = if (condition) "imported polygons; DEM conditioned for topography and outlet slope" else "imported polygons")
  out <- new_ak_catchments(x, dplyr::bind_rows(attr(catchments, "ak_provenance"), prov), attr(catchments, "errors"))
  add_errors(out, errors, nrow(x))
}

# Points as a data frame of site_code, lat, lon (+ any other columns), checked
point_table <- function(points, crs = 4326) {
  if (inherits(points, "sf")) {
    if (is.na(sf::st_crs(points))) stop("`points` has no coordinate reference system")
    g <- sf::st_geometry(points)
    ll <- sf::st_coordinates(sf::st_transform(g, 4326))
    check_datum_shift(g, ll)
    points <- data.frame(sf::st_drop_geometry(points), lon = ll[, 1], lat = ll[, 2])
    crs <- 4326
  }
  p <- as.data.frame(points)
  if (!"lat" %in% names(p) && "latitude" %in% names(p)) p$lat <- p$latitude
  if (!"lon" %in% names(p) && "longitude" %in% names(p)) p$lon <- p$longitude
  miss <- setdiff(c("site_code", "lat", "lon"), names(p))
  if (length(miss)) stop("`points` needs columns ", paste(miss, collapse = ", "), " (or be sf points with site_code)")
  if (!isTRUE(sf::st_crs(crs) == sf::st_crs(4326))) {
    ok <- !is.na(p$lat) & !is.na(p$lon)
    g <- sf::st_sfc(lapply(which(ok), \(i) sf::st_point(c(p$lon[i], p$lat[i]))), crs = crs)
    ll <- sf::st_coordinates(sf::st_transform(g, 4326))
    check_datum_shift(g, ll)
    p$lon[ok] <- ll[, 1]
    p$lat[ok] <- ll[, 2]
  }
  p$site_code <- as.character(p$site_code)
  if (anyNA(p$site_code) || any(duplicated(p$site_code))) {
    stop("`site_code` must be unique and not missing; duplicated: ", paste(unique(p$site_code[duplicated(p$site_code)]), collapse = ", "))
  }
  bad <- is.na(p$lat) | is.na(p$lon) | abs(p$lat) > 90 | abs(p$lon) > 180
  if (any(bad)) stop("Missing or impossible coordinates for: ", paste(p$site_code[bad], collapse = ", "))
  outside <- p$lat < 51 | p$lat > 72 | (p$lon > -129 & p$lon < 170)
  if (any(outside)) {
    warning("Outside Alaska (check lat/lon order and the sign of longitude): ", paste(p$site_code[outside], collapse = ", "),
            ". The IfSAR DEM covers Alaska only.", call. = FALSE)
  }
  p
}

# NAD27 to NAD83/WGS84 moves points by roughly 100-200 m in Alaska. PROJ needs the NADCON grid for it; without
# the grid it may apply no shift at all and say nothing, which puts points on the wrong stream.
check_datum_shift <- function(g, ll) {
  if (any(!is.finite(ll[, 1:2]))) {
    stop("Could not convert the points' coordinates to WGS84 (PROJ returned no result; for NAD27 it needs the NADCON ",
         "grid: run sf::sf_proj_network(TRUE) with internet access, or install the PROJ grid us_noaa_alaska.tif)", call. = FALSE)
  }
  wkt <- sf::st_crs(g)$wkt
  if (!length(g) || !grepl("1927|NAD27|North American Datum 1927", wkt)) return(invisible())
  src <- sf::st_coordinates(sf::st_transform(g, sf::st_crs(sf::st_crs(g)$wkt)))
  if (sf::st_is_longlat(g) && max(abs(src[, 1:2] - ll[, 1:2])) < 1e-5) {
    warning("NAD27 coordinates were not shifted to WGS84: PROJ lacks the NADCON grid for Alaska. ",
            "Run sf::sf_proj_network(TRUE) (or install the PROJ grid us_noaa_alaska.tif) and try again; ",
            "otherwise points may be placed 100-200 m off.", call. = FALSE)
  }
  invisible()
}

new_ak_catchments <- function(x, provenance, errors = NULL, delineations = NULL) {
  attr(x, "ak_provenance") <- provenance
  attr(x, "errors") <- errors %||% dplyr::tibble(site_code = character(), step = character(), error = character())
  if (!is.null(delineations)) attr(x, "delineations") <- delineations
  ak_class(x)
}

#' @export
print.ak_catchments <- function(x, ...) {
  if (!inherits(x, "sf")) return(NextMethod())
  n_ok <- sum(x$delin_ok %in% TRUE)
  cat(sprintf("<ak_catchments> %d sites (%d delin_ok)", nrow(x), n_ok))
  err <- attr(x, "errors")
  if (NROW(err)) cat(sprintf(", %d failed", nrow(err)))
  cat("\n")
  if ("flags" %in% names(x)) {
    fl <- unlist(strsplit(x$flags[nzchar(x$flags)], ";"))
    if (length(fl)) cat("  flags:", paste(sprintf("%s (%d)", names(table(fl)), table(fl)), collapse = ", "), "\n")
  }
  pv <- attr(x, "ak_provenance")
  if (NROW(pv)) cat("  layers:", paste(unique(paste0(pv$layer, " (", pv$variant, ")")), collapse = ", "), "\n")
  cat("\n")
  d <- sf::st_drop_geometry(x)
  class(d) <- setdiff(class(d), "ak_catchments")
  print(d[, intersect(c("site_code", "area_km2", "dem_source", "snap_method", "snap_dist_m", "delin_ok", "flags"), names(d))])
  invisible(x)
}
