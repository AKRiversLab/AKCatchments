#' Elevation data for an area of interest
#'
#' Returns a digital elevation model for `aoi` in Alaska Albers (EPSG:3338),
#' cached on disk so repeated calls are fast.
#'
#' Sources, best first for catchment delineation:
#'
#' * `"lidar"`: a lidar bare-earth model, where one lidar project has
#'   elevation for every cell of the window (see [ak_lidar_index()]): the
#'   State of Alaska's projects (Division of Geological & Geophysical Surveys
#'   image services) and USGS 3DEP 1 m DEMs, newest project first. Vertical
#'   units in feet are converted to metres. Windows that reach the sea or the
#'   edge of a project have missing cells, so they are not lidar windows.
#'   Lidar covers small parts of Alaska (towns, roads, hazard sites); an
#'   error says so where it does not.
#' * `"best"`: lidar where it covers the window, else IfSAR (at a cell size of
#'   at least 5 m).
#' * `"ifsar"`: the statewide 5 m IfSAR digital terrain model (bare earth,
#'   NAVD88, 3DEP quality level 5), served by the Alaska Division of Geological
#'   & Geophysical Surveys as an image service. The window is built from
#'   tiles of `tile_px` cells a side on a fixed grid at resolution `res` (the
#'   server resamples when `res` > 5), cached, so overlapping windows (nearby
#'   sites, a window and its enlargements, repeated runs) share downloads.
#'   Vertical accuracy is reported as 3 m at 90% confidence on 0-10 degree
#'   slopes. This is the default.
#' * `"copernicus"`: Copernicus GLO-30 (30 m) from its public AWS bucket. A
#'   surface model (it includes vegetation), derived from TanDEM-X, the same
#'   source as the global TDX-Hydro flowlines; useful as a check on the IfSAR
#'   result and outside IfSAR coverage.
#' * `"local"`: GeoTIFF tiles you already have in `dir` (for example IfSAR or
#'   ArcticDEM tiles), mosaicked as a virtual raster.
#' * `"elevatr"`: AWS terrain tiles through the elevatr package. Coarse and
#'   noisy in Alaska; a last resort.
#'
#' @param aoi An sf, sfc or bbox object in any CRS.
#' @param source One of `"ifsar"`, `"lidar"`, `"best"`, `"copernicus"`,
#'   `"local"`, `"elevatr"`.
#' @param res Cell size in metres. Defaults to 2 for lidar, 5 for IfSAR and 30
#'   for Copernicus; for `"local"` the native resolution is kept unless given.
#' @param dir Folder of GeoTIFF tiles, for `source = "local"` (default: the
#'   cache's `dem` folder, where the geo workflow kept IfSAR tiles).
#' @param tile_px Tile edge, in cells, of the fixed grid in which windows are
#'   requested from an image service. Smaller tiles waste less beyond the
#'   window's edge but take more requests.
#' @return A single-layer SpatRaster named `elevation` in EPSG:3338, with a
#'   `dem_source` metadata tag (for example `"ifsar_5m"` or `"lidar_2m"`) and,
#'   for lidar, a `lidar_project` tag naming the project used.
#' @seealso `vignette("delineation")` (download, use, uncertainty, validation)
#' @export
ak_dem <- function(aoi, source = c("ifsar", "lidar", "best", "copernicus", "local", "elevatr"), res = NULL, dir = NULL,
                   tile_px = 1000) {
  source <- match.arg(source)
  aoi <- as_aoi(aoi)
  dem <- switch(source,
                ifsar = dem_ifsar(aoi, res %||% 5, tile_px),
                lidar = dem_lidar(aoi, res %||% 2, tile_px),
                best = tryCatch(dem_lidar(aoi, res %||% 2, tile_px), error = \(e) dem_ifsar(aoi, max(res %||% 5, 5), tile_px)),
                copernicus = dem_copernicus(aoi, res %||% 30),
                local = dem_local(aoi, dir, res),
                elevatr = dem_elevatr(aoi, res))
  project <- dem_tag(dem, "lidar_project")
  names(dem) <- "elevation"
  used <- if (!is.na(project)) "lidar" else if (source == "best") "ifsar" else source
  terra::metags(dem) <- c(dem_source = paste0(used, "_", signif(terra::res(dem)[1], 3), "m"),
                          if (!is.na(project)) c(lidar_project = project))
  dem
}

# Default cell size (m) for each DEM source; NA keeps the source's own
default_res <- function(dem_source) unname(c(ifsar = 5, lidar = 2, best = 2, copernicus = 30)[dem_source])

dem_tag <- function(dem, name) {
  m <- terra::metags(dem)
  v <- if (is.data.frame(m)) m$value[m$name == name] else unname(m[name])
  if (length(v) && !is.na(v[1]) && nzchar(v[1])) v[1] else NA_character_
}

dem_source <- function(dem) dem_tag(dem, "dem_source")

dem_ifsar <- function(aoi, res, tile_px) {
  dem_imageserver("https://geoportal.dggs.dnr.alaska.gov/arcgis/rest/services/elevation/IFSAR_DTM/ImageServer",
                  aoi, res, tile_px, "ifsar")
}

# A window from an ArcGIS image service (the IfSAR DTM, or a lidar DTM), cut from tiles of `tile_px` cells a side on
# a fixed EPSG:3338 grid (tile col, row covers col * step to (col + 1) * step), so any window touching a tile shares it.
# Tiles are cached as dem_windows/<name>/<name>_<res>m_<col>_<row>.tif.
dem_imageserver <- function(service, aoi, res, tile_px, name) {
  bb <- sf::st_bbox(aoi)
  lo <- floor(bb[c("xmin", "ymin")] / res) * res
  hi <- ceiling(bb[c("xmax", "ymax")] / res) * res
  step <- tile_px * res
  tiles <- tidyr::expand_grid(col = floor(lo[[1]] / step):(ceiling(hi[[1]] / step) - 1),
                              row = floor(lo[[2]] / step):(ceiling(hi[[2]] / step) - 1)) |>
    dplyr::mutate(file = file.path(ak_cache_dir("dem_windows", name), sprintf("%s_%gm_%d_%d.tif", name, res, .data$col, .data$row)))
  files <- purrr::pmap_chr(tiles, \(col, row, file) {
    if (!file.exists(file)) {
      ak_download(paste0(service, "/exportImage"), file, list(
        bbox = paste(col * step, row * step, (col + 1) * step, (row + 1) * step, sep = ","), bboxSR = 3338, imageSR = 3338,
        size = paste(tile_px, tile_px, sep = ","),
        format = "tiff", pixelType = "F32", noData = -9999, interpolation = "RSP_BilinearInterpolation", f = "image"))
      if (inherits(try(terra::rast(file), silent = TRUE), "try-error")) {
        msg <- readChar(file, min(file.size(file), 500))
        unlink(file)
        stop("The image service ", service, " did not return a raster: ", msg, call. = FALSE)
      }
    }
    file
  })
  dem <- terra::vrt(files, filename = tempfile(fileext = ".vrt"))
  terra::NAflag(dem) <- -9999
  terra::crs(dem) <- "EPSG:3338"
  terra::crop(dem, terra::ext(c(lo[[1]], hi[[1]], lo[[2]], hi[[2]])))
}

# Lidar bare-earth DEM for the window: the newest project in ak_lidar_index() whose footprint covers the window
# and whose DEM has elevation for every cell of it
dem_lidar <- function(aoi, res, tile_px) {
  idx <- ak_lidar_index()
  win <- sf::st_as_sfc(sf::st_bbox(aoi))
  cand <- idx[sf::st_covers(idx, win, sparse = FALSE)[, 1], ]
  cand <- cand[order(-cand$year), ]
  if (!nrow(cand)) {
    bb <- sf::st_bbox(win)
    stop(sprintf("No lidar project in the State of Alaska or USGS 3DEP index covers this window (%.1f x %.1f km)",
                 (bb[["xmax"]] - bb[["xmin"]]) / 1000, (bb[["ymax"]] - bb[["ymin"]]) / 1000), call. = FALSE)
  }
  why <- character()
  for (i in seq_len(nrow(cand))) {
    p <- cand[i, ]
    dem <- tryCatch(if (p$source == "dggs") dem_imageserver(p$service, aoi, res, tile_px, p$project) else dem_3dep(aoi, res, p$project),
                    error = \(e) { why <<- c(why, paste0(p$project, ": ", conditionMessage(e))); NULL })
    if (is.null(dem)) next
    missing_cells <- terra::global(is.na(dem), "sum")[[1]]
    if (missing_cells == 0) {
      if (p$z_to_m != 1) dem <- dem * p$z_to_m
      terra::metags(dem) <- c(lidar_project = p$project)
      return(dem)
    }
    why <- c(why, sprintf("%s: %d cells without elevation", p$project, missing_cells))
  }
  stop("No lidar project has elevation for the whole window:\n", paste(why, collapse = "\n"), call. = FALSE)
}

# USGS 3DEP 1 m DEM tiles of one project (cloud-optimised GeoTIFFs listed by The National Map's products API),
# read for the window and averaged onto a `res` grid
dem_3dep <- function(aoi, res, project) {
  bb <- sf::st_bbox(sf::st_transform(aoi, 4326))
  items <- ak_json("https://tnmaccess.nationalmap.gov/api/v1/products",
                   list(datasets = "Digital Elevation Model (DEM) 1 meter", bbox = paste(bb, collapse = ","),
                        prodFormats = "GeoTIFF", max = 500, outputFormat = "JSON"))$items
  urls <- grep(paste0("/", project, "/"), items$downloadURL, fixed = TRUE, value = TRUE)
  if (!length(urls)) stop("The National Map lists no 1 m DEM tiles of this project for the window", call. = FALSE)
  files <- paste0("/vsicurl/", urls)
  tile_bb <- sf::st_bbox(sf::st_transform(aoi, terra::crs(terra::rast(files[1]))))
  tiles_to_grid(files, tile_bb, grid_3338(aoi, res), "3DEP 1 m DEM", method = if (res > 1) "average" else "bilinear")
}

#' Lidar coverage in Alaska
#'
#' Footprints of the lidar bare-earth elevation models that
#' `ak_dem(source = "lidar")` (and `"best"`, and so delineation and
#' conditioning with those sources) can use:
#' * the State of Alaska's index of elevation projects (Division of Geological
#'   & Geophysical Surveys), keeping lidar projects with a digital terrain
#'   model image service and known vertical units;
#' * the USGS 3DEP 1 m DEM project index.
#'
#' Downloaded once and cached as `dem_windows/lidar_index.gpkg`; new projects
#' are added each year, so use `refresh = TRUE` now and then.
#'
#' @param refresh Download the indexes again?
#' @return sf polygons in EPSG:3338: `source` (`"dggs"` or `"3dep"`),
#'   `project`, `year`, `service` (the DGGS image service; `NA` for 3DEP) and
#'   `z_to_m` (factor from the vertical units to metres).
#' @examples
#' \dontrun{
#' idx <- ak_lidar_index()
#' pts <- sf::st_as_sf(data.frame(lon = -149.85, lat = 61.19), coords = c("lon", "lat"), crs = 4326)
#' idx[sf::st_covers(idx, sf::st_transform(sf::st_buffer(sf::st_transform(pts, 3338), 2000), 3338),
#'                   sparse = FALSE)[, 1], ]
#' }
#' @seealso [ak_dem()]
#' @export
ak_lidar_index <- function(refresh = FALSE) {
  f <- file.path(ak_cache_dir("dem_windows"), "lidar_index.gpkg")
  if (file.exists(f) && !refresh) return(sf::read_sf(f))
  alaska <- sf::st_bbox(c(xmin = -180, ymin = 51, xmax = -129, ymax = 72), crs = 4326)
  state <- ak_arcgis_query("https://geoportal.alaska.gov/arcgis/rest/services/Hosted/AK_DGGS_Elevation/FeatureServer/0", alaska)
  usgs <- ak_arcgis_query("https://index.nationalmap.gov/arcgis/rest/services/3DEPElevationIndex/MapServer/18", alaska)
  idx <- dplyr::bind_rows(lidar_index_state(state), lidar_index_3dep(usgs))
  tmp <- tempfile(fileext = ".gpkg")
  sf::write_sf(idx, tmp)
  if (!replace_file(tmp, f)) stop("Could not write ", f, call. = FALSE)
  log_download("https://geoportal.alaska.gov/arcgis/rest/services/Hosted/AK_DGGS_Elevation/FeatureServer/0", f)
  idx
}

# Lidar projects in the state's elevation index that have a DTM image service (found in any text field) and
# vertical units that say metres or feet
lidar_index_state <- function(x) {
  x <- dplyr::rename_with(x, tolower)
  need <- c("data_type", "vert_units", "c_year")
  if (!all(need %in% names(x))) {
    stop("The State of Alaska elevation index lacks ", paste(setdiff(need, names(x)), collapse = ", "),
         "; its fields: ", paste(names(x), collapse = ", "), call. = FALSE)
  }
  att <- sf::st_drop_geometry(x)
  text <- do.call(paste, unname(att[vapply(att, is.character, TRUE)]))
  m <- regexpr("https?://[^ \"'<>,;|]+_dtm/ImageServer", text, ignore.case = TRUE)
  service <- rep(NA_character_, nrow(x))
  service[m > 0] <- regmatches(text, m)
  feet <- grepl("f(oo|ee)t|\\bft\\b", x$vert_units, ignore.case = TRUE)
  z_to_m <- dplyr::case_when(feet & grepl("survey", x$vert_units, ignore.case = TRUE) ~ 1200 / 3937,
                             feet ~ 0.3048,
                             grepl("met(er|re)|^m$", x$vert_units, ignore.case = TRUE) ~ 1)
  keep <- grepl("lidar", x$data_type, ignore.case = TRUE) & !is.na(service) & !is.na(z_to_m)
  project <- basename(dirname(service[keep]))
  sf::st_sf(source = rep("dggs", sum(keep)), project = project,
            year = dplyr::coalesce(as.integer(x$c_year[keep]), first_year(project)), service = service[keep],
            z_to_m = z_to_m[keep], geometry = sf::st_make_valid(sf::st_geometry(x)[keep]))
}

# USGS 3DEP 1 m DEM projects (layer "1 Meter" of the 3DEP elevation index)
lidar_index_3dep <- function(x) {
  x <- dplyr::rename_with(x, tolower)
  if (!nrow(x)) x$project <- character()
  if (!"project" %in% names(x)) stop("The 3DEP 1 m index has no `project` field; its fields: ", paste(names(x), collapse = ", "), call. = FALSE)
  sf::st_sf(source = rep("3dep", nrow(x)), project = x$project, year = first_year(x$project),
            service = rep(NA_character_, nrow(x)), z_to_m = rep(1, nrow(x)), geometry = sf::st_make_valid(sf::st_geometry(x)))
}

# First four-digit year (1900-2099) in each string; NA where there is none
first_year <- function(x) {
  m <- regexpr("(19|20)[0-9]{2}", x)
  out <- rep(NA_integer_, length(x))
  out[m > 0] <- as.integer(regmatches(x, m))
  out
}

dem_copernicus <- function(aoi, res) {
  bb <- sf::st_bbox(sf::st_transform(sf::st_buffer(aoi, 2 * res), 4326))
  tiles <- expand.grid(lat = floor(bb[["ymin"]]):floor(bb[["ymax"]]), lon = floor(bb[["xmin"]]):floor(bb[["xmax"]]))
  name <- sprintf("Copernicus_DSM_COG_10_%s%02d_00_%s%03d_00_DEM", ifelse(tiles$lat < 0, "S", "N"), abs(tiles$lat),
                  ifelse(tiles$lon < 0, "W", "E"), abs(tiles$lon))
  tmpl <- grid_3338(aoi, res)
  paste0("/vsicurl/https://copernicus-dem-30m.s3.amazonaws.com/", name, "/", name, ".tif") |>
    tiles_to_grid(bb, tmpl, "Copernicus GLO-30")
}

# Read the parts of geographic tiles inside `bb` and project them onto `tmpl`. Tiles that don't exist
# (ocean) are skipped; their reasons are reported if none can be read. Copernicus tiles change
# longitude spacing at 50, 60, 70 and 80 N, so tiles that differ are projected one by one and merged.
tiles_to_grid <- function(files, bb, tmpl, label, method = "bilinear") {
  errs <- character()
  r <- files |>
    purrr::map(\(u) tryCatch(terra::crop(suppressWarnings(terra::rast(u)), terra::ext(bb[c("xmin", "xmax", "ymin", "ymax")])),
                             error = \(e) { errs <<- c(errs, paste0(basename(u), ": ", conditionMessage(e))); NULL })) |>
    purrr::compact()
  if (!length(r)) stop("No ", label, " tile could be read for this area:\n", paste(errs, collapse = "\n"))
  same <- length(unique(purrr::map_chr(r, \(t) paste(signif(terra::res(t), 6), collapse = "x")))) == 1
  if (same) return(terra::project(if (length(r) > 1) terra::merge(terra::sprc(r)) else r[[1]], tmpl, method = method))
  purrr::map(r, \(t) terra::project(t, tmpl, method = method)) |>
    terra::sprc() |>
    terra::merge()
}

dem_local <- function(aoi, dir, res) {
  dir <- dir %||% ak_cache_dir(cache_paths$dem_tiles)
  tifs <- list.files(dir, "\\.tif$", full.names = TRUE, recursive = TRUE)
  if (!length(tifs)) stop("No .tif files in ", dir)
  v <- terra::vrt(tifs, filename = tempfile(fileext = ".vrt"))
  win <- sf::st_bbox(sf::st_transform(aoi, terra::crs(v)))
  if (is.null(terra::intersect(terra::ext(v), terra::ext(win[c("xmin", "xmax", "ymin", "ymax")])))) {
    stop("The DEM tiles in ", dir, " do not cover this area (", length(tifs), " tiles)", call. = FALSE)
  }
  d <- terra::crop(v, terra::ext(win[c("xmin", "xmax", "ymin", "ymax")]), snap = "out")
  same <- isTRUE(sf::st_crs(terra::crs(d)) == sf::st_crs(akalbers_crs()))
  if (same && (is.null(res) || isTRUE(all.equal(terra::res(d)[1], res)))) return(d)
  r <- res %||% terra::res(d)[1]
  terra::project(d, grid_3338(aoi, r), method = if (r > terra::res(d)[1]) "average" else "bilinear")
}

# EPSG:3338 grid over the area's bounding box, snapped to multiples of r
grid_3338 <- function(aoi, r) {
  bb <- sf::st_bbox(sf::st_transform(aoi, akalbers_crs()))
  terra::rast(terra::ext(floor(bb[["xmin"]] / r) * r, ceiling(bb[["xmax"]] / r) * r,
                         floor(bb[["ymin"]] / r) * r, ceiling(bb[["ymax"]] / r) * r),
              resolution = r, crs = "EPSG:3338")
}

dem_elevatr <- function(aoi, res) {
  if (!requireNamespace("elevatr", quietly = TRUE)) stop("Install the elevatr package for source = \"elevatr\"")
  ll <- sf::st_transform(aoi, 4326)
  # Whole tiles, projected onto a 3338 grid here: elevatr's own clip = "bbox" leaves NA corners
  d <- elevatr::get_elev_raster(sf::st_as_sf(ll), z = 12, clip = "tile", verbose = FALSE, override_size_check = TRUE)
  d <- if (inherits(d, "SpatRaster")) d else terra::rast(d)
  native <- round(38.2 * cos(mean(sf::st_bbox(ll)[c("ymin", "ymax")]) * pi / 180))  # zoom-12 cell size at this latitude
  r <- res %||% native
  terra::project(d, grid_3338(aoi, r), method = if (r > native) "average" else "bilinear")
}
