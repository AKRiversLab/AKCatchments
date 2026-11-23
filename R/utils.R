#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data .env %||% :=
NULL

#' Working coordinate reference system
#'
#' All spatial work in the package happens in NAD83 / Alaska Albers
#' (EPSG:3338): metric and equal-area, so lengths and areas are in metres.
#'
#' @return The EPSG code, 3338.
#' @export
akalbers_crs <- function() 3338

#' The download cache (data store)
#'
#' Every download (DEM windows, Daymet pixels, MODIS, land cover, alder,
#' geology, flowlines, imagery tiles, ...) is kept in one folder, so a rerun,
#' or a colleague pointing at the same folder, does not fetch it again. The
#' folder is `options(AKCatchments.cache = )` or else the environment
#' variable `AKCATCHMENTS_CACHE`. There is no default: it can grow to tens of
#' GB (statewide rasters, TDX-Hydro flowlines), so you choose where it goes,
#' once, with `ak_set_cache(path, persist = "user")`. Pick a drive with room,
#' not a folder that syncs to a size-limited cloud plan unless that is the
#' point (a shared group cache). Scratch files that are not worth keeping go
#' to R's session temp folder (`tempdir()`) and are removed when R closes.
#'
#' `ak_set_cache()` sets it for the session and, with `persist`, writes
#' `AKCATCHMENTS_CACHE` to the project's or your user `.Renviron` so it sticks.
#' A shared folder works: for example a Box Drive, OneDrive or network folder
#' that a group syncs. Files are written to a temporary `.part` name and
#' renamed when complete, so a sync client never uploads half a file. With Box
#' Drive, mark the folder "Make available offline" (rasters are read many
#' times) and check your plan's per-file size limit before caching
#' TDX-Hydro files, which can be several GB.
#'
#' The sub-folder names follow the AKSynthesis_RawGeoData workflow
#' (`ak_cache_layout()`), so pointing the cache at that folder reuses what it
#' has already downloaded.
#'
#' @param ... Sub-directories, created if needed.
#' @return The path.
#' @export
ak_cache_dir <- function(...) {
  root <- cache_root()
  if (is.null(root)) stop(no_cache_msg, call. = FALSE)
  d <- gsub("\\\\", "/", file.path(root, ...))  # forward slashes: safe in /vsizip/ paths and on every OS
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

cache_root <- function() getOption("AKCatchments.cache") %||% (Sys.getenv("AKCATCHMENTS_CACHE") |> (\(e) if (nzchar(e)) e)())

no_cache_msg <- paste0("No AKCatchments cache folder is set. Downloads can reach tens of GB, so choose a folder ",
                       "on a drive with room, once:\n  ak_set_cache(\"D:/AKCatchments_cache\", persist = \"user\")")

.onAttach <- function(libname, pkgname) {
  if (is.null(cache_root())) packageStartupMessage(no_cache_msg)
}

#' @rdname ak_cache_dir
#' @param path Folder to use as the cache.
#' @param persist `"session"` (default) sets it for this R session only;
#'   `"project"` also writes it to `.Renviron` in the working directory;
#'   `"user"` to your user `.Renviron` (`R_ENVIRON_USER` if set, else
#'   `~/.Renviron`, which on Windows is in Documents).
#' @export
ak_set_cache <- function(path, persist = c("session", "project", "user")) {
  persist <- match.arg(persist)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stop("Cannot create or reach ", path)
  options(AKCatchments.cache = path)
  Sys.setenv(AKCATCHMENTS_CACHE = path)
  if (persist != "session") {
    # The user file is wherever R reads it: R_ENVIRON_USER if set, else ~/.Renviron (Documents on Windows)
    f <- if (persist == "project") file.path(getwd(), ".Renviron") else Sys.getenv("R_ENVIRON_USER", path.expand("~/.Renviron"))
    lines <- if (file.exists(f)) readLines(f, warn = FALSE) else character()
    writeLines(c(lines[!startsWith(lines, "AKCATCHMENTS_CACHE=")], paste0("AKCATCHMENTS_CACHE=", path)), f)
    message("Saved AKCATCHMENTS_CACHE=", path, " in ", normalizePath(f, winslash = "/"), " (R's startup settings file)")
  }
  invisible(path)
}

#' @rdname ak_cache_dir
#' @return `ak_cache_layout()`: the sub-folder used for each dataset.
#' @export
ak_cache_layout <- function() {
  data.frame(
    dataset = c("Daymet pixels", "MODIS snow metrics (cropped)", "SIM 3340 geology", "ACCS alder", "NLCD 2016 Alaska",
                "Jorgenson permafrost", "Pastick permafrost", "Fire perimeters", "DEM tiles (local)",
                "DEM windows (IfSAR, Copernicus, elevatr)", "Delineations", "PRISM Alaska normals", "Reference flowlines",
                "Imagery tiles", "Gaged basins", "Glaciers (RGI; Earthdata Login .netrc or manual download)", "NPS WRST land cover (alder alternate)",
                "SNAP downscaled CMIP6 (monthly cell means)", "Landsat readings at sample points",
                "Olson & Hawkins lithology geochemistry workbooks"),
    folder = c(cache_paths$daymet, cache_paths$modis, cache_paths$geology, cache_paths$alder, cache_paths$nlcd,
               cache_paths$jorgenson, cache_paths$pastick, cache_paths$fire, cache_paths$dem_tiles,
               "dem_windows", "delineation", cache_paths$prism, "flowlines", "tiles", "gaged_basins", cache_paths$glaciers,
               cache_paths$nps_wrst, cache_paths$snap, cache_paths$landsat, cache_paths$olson),
    same_as_geo_workflow = c(rep(TRUE, 6), FALSE, TRUE, TRUE, rep(FALSE, 11)))
}

# Sub-folder names, matching the AKSynthesis_RawGeoData workflow where it had one
cache_paths <- list(daymet = "scratch/daymet_pixels", modis = "modis_snow", geology = "ak_geology", alder = "accs_alnus",
                    nlcd = "nlcd_2016_ak", jorgenson = "jorgenson_2008", pastick = "pastick_2015",
                    fire = "fire_history_ak.gpkg", dem_tiles = "dem", prism = "prism_ak_8110", glaciers = "rgi",
                    nps_wrst = "nps_wrst_veg", snap = "snap_cmip6", landsat = "landsat_c2", olson = "olson_lithology")

#' @rdname ak_cache_dir
#' @return `ak_cache_info()`: one row per downloaded file recorded in the
#'   cache manifest (`cache_manifest.csv`: URL, file, size, date), plus the
#'   size of each top-level folder.
#' @export
ak_cache_info <- function() {
  root <- ak_cache_dir()
  top <- list.dirs(root, recursive = FALSE)
  sizes <- data.frame(folder = basename(top),
                      gb = vapply(top, \(d) sum(file.size(list.files(d, recursive = TRUE, full.names = TRUE)), na.rm = TRUE) / 1e9, 0),
                      row.names = NULL)
  man <- file.path(root, "cache_manifest.csv")
  list(root = root, folders = sizes,
       downloads = if (file.exists(man)) utils::read.csv(man) else data.frame())
}

ak_request <- function(url, query = list(), netrc = NULL) {
  req <- httr2::request(url) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("AKCatchments R package") |>
    httr2::req_retry(max_tries = 4, is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)) |>
    httr2::req_timeout(600) |>
    httr2::req_error(body = \(resp) c(paste("URL:", resp$url), resp_excerpt(resp)))
  # Earthdata Login as in NSIDC's curl example (curl -b/-c cookies -L -n): credentials from the netrc file for the
  # redirect to urs.earthdata.nasa.gov, sent as Basic auth without waiting for a challenge (R's curl otherwise waits,
  # and the login server redirects to its sign-in page instead of challenging), and the session cookie kept
  if (is.null(netrc)) req else httr2::req_options(req, netrc = 1L, netrc_file = netrc, httpauth = 1L) |>
    httr2::req_cookie_preserve(file.path(tempdir(), "earthdata_cookies.txt"))
}

# HTTP errors name the URL (req_error above); so do connection failures
ak_perform <- function(req, path = NULL) {
  tryCatch(httr2::req_perform(req, path = path),
           httr2_failure = \(e) rlang::abort(paste("Could not reach", req$url), parent = e, call = NULL))
}

resp_excerpt <- function(resp, n = 300) {
  tryCatch(substr(gsub("\\s+", " ", httr2::resp_body_string(resp)), 1, n), error = \(e) character())
}

ak_json <- function(url, query = list(), simplify = TRUE) {
  ak_request(url, query) |>
    ak_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(simplifyVector = simplify)
}

ak_download <- function(url, dest, query = list(), netrc = NULL) {
  if (file.exists(dest)) return(dest)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- sprintf("%s.%s.part", dest, Sys.getpid())  # per process, so parallel workers don't collide
  # No limit on total time (the alder and Pastick archives are 5-6 GB); give up only if it stalls for 2 minutes
  tryCatch(ak_request(url, query, netrc) |>
             httr2::req_options(timeout_ms = 0, connecttimeout = 60, low_speed_limit = 1000, low_speed_time = 120) |>
             ak_perform(path = tmp),
           error = \(e) { unlink(tmp); stop(e) })
  if (is_web_page(tmp) && !grepl("\\.html?$", dest)) {
    txt <- gsub("<[^>]+>|\\s+", " ", readChar(tmp, min(file.size(tmp), 2000)))
    unlink(tmp)
    stop("Expected a data file from ", url, " but got a web page (moved link, or a login or error page): ",
         substr(trimws(gsub(" +", " ", txt)), 1, 300), call. = FALSE)
  }
  if (!rename_retry(tmp, dest)) {
    unlink(tmp)
    if (!file.exists(dest)) stop("Could not move the download to ", dest)
  }
  log_download(url, dest)
  dest
}

# Provenance for a shared cache: one line per completed download
log_download <- function(url, dest) {
  root <- normalizePath(ak_cache_dir(), winslash = "/", mustWork = FALSE)
  d <- normalizePath(dest, winslash = "/", mustWork = FALSE)
  if (!startsWith(d, root)) return(invisible())
  man <- file.path(root, "cache_manifest.csv")
  row <- data.frame(file = substring(d, nchar(root) + 2), url = url, bytes = file.size(dest),
                    downloaded_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                    package_version = tryCatch(as.character(utils::packageVersion("AKCatchments")), error = \(e) NA))
  try(utils::write.table(row, man, sep = ",", row.names = FALSE, col.names = !file.exists(man), append = file.exists(man)),
      silent = TRUE)
  invisible()
}

# An HTML page saved under a data file's name (servers answer moved links and logins with 200 and a page)
is_web_page <- function(f) {
  if (!file.exists(f) || !file.size(f)) return(FALSE)
  b <- readBin(f, "raw", 512)
  grepl("^\\s*(<!doctype html|<html|<head|<body)", rawToChar(b[b != as.raw(0)]), ignore.case = TRUE, useBytes = TRUE)
}

# Unpack an archive into `dir` through a temporary folder, so an interrupted unpack never looks finished
extract_archive <- function(archive, dir, unpack = utils::unzip) {
  tmp <- paste0(dir, ".", Sys.getpid(), ".part")
  unlink(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp, recursive = TRUE)
  unpack(archive, exdir = tmp)
  if (!length(list.files(tmp, recursive = TRUE))) stop("Nothing was unpacked from ", archive)
  if (dir.exists(dir) && !length(list.files(dir, all.files = TRUE, no.. = TRUE))) unlink(dir, recursive = TRUE)
  if (dir.exists(dir) || !file.rename(tmp, dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(list.files(tmp, full.names = TRUE), dir, recursive = TRUE)
  }
  invisible(dir)
}

# GDAL path to a file inside a zip
vsizip <- function(zip, inner = NULL) {
  paste0("/vsizip/", gsub("\\\\", "/", normalizePath(zip, winslash = "/", mustWork = FALSE)), if (!is.null(inner)) paste0("/", inner))
}

# Bounding box of `g` as WKT in the CRS of a layer, for st_read(wkt_filter = ): reads only what's needed
bbox_wkt <- function(g, dsn, layer = NULL) {
  info <- sf::st_layers(dsn)
  i <- if (is.null(layer)) 1L else match(layer, info$name)
  if (is.na(i)) stop("No layer '", layer, "' in ", dsn, "; it has: ", paste(info$name, collapse = ", "), call. = FALSE)
  # No SQL (OGR SQL on File Geodatabases varies by GDAL version); terra's proxy reads the CRS without the features
  crs <- info$crs[[i]]
  if (is.na(crs)) crs <- sf::st_crs(terra::crs(terra::vect(dsn, layer = info$name[i], proxy = TRUE)))
  if (is.na(crs)) return(character(0)) # CRS unknown: read the whole layer rather than filter in the wrong CRS
  sf::st_as_text(sf::st_as_sfc(sf::st_bbox(sf::st_transform(g, crs))))
}

as_aoi <- function(x) {
  if (inherits(x, "bbox")) x <- sf::st_as_sfc(x)
  sf::st_geometry(x) |>
    sf::st_transform(akalbers_crs()) |>
    sf::st_union()
}

as_catchments <- function(x, id = "site_code") {
  x <- catchment_sf(x)
  if (!inherits(x, "sf")) stop("`catchments` must be an sf object with a `", id, "` column")
  if (!id %in% names(x)) stop("`catchments` has no `", id, "` column")
  if (is.na(sf::st_crs(x))) stop("`catchments` has no coordinate reference system: set one with sf::st_set_crs()")
  x <- sf::st_transform(x[!sf::st_is_empty(x), ], akalbers_crs())
  bad <- !sf::st_is_valid(x) %in% TRUE
  if (any(bad)) x[bad, ] <- sf::st_make_valid(x[bad, ])  # hand-drawn polygons often self-intersect; valid ones are untouched
  x <- sf::st_sf(site_code = x[[id]], geometry = sf::st_geometry(x))
  x$area_m2 <- as.numeric(sf::st_area(x))
  x
}

# Delineation results to their catchment polygons: one `ak_delineation` or a list of them (or a list holding
# `catchments`). Anything else is returned unchanged.
catchment_sf <- function(x) {
  if (inherits(x, "ak_delineation")) return(delineation_row(x))
  if (is.list(x) && !inherits(x, "data.frame")) {
    if (inherits(x$catchments, "sf")) return(x$catchments)
    if (length(x) && all(vapply(x, inherits, TRUE, "ak_delineation"))) return(dplyr::bind_rows(purrr::map(x, delineation_row)))
  }
  x
}

# One delineation as a catchment row: the polygon with its diagnostics, the snapped outlet, the check plot and
# the conditioned rasters (dem_file, filled_file, pointer_file, accum_file) that ak_topography() and
# ak_outlet_slope() reuse
delineation_row <- function(d) {
  c1 <- d$catchment
  if (!is.null(d$snapped)) {
    sn <- sf::st_coordinates(sf::st_transform(d$snapped, 4326))
    c1$snapped_lon <- sn[1, 1]
    c1$snapped_lat <- sn[1, 2]
  }
  c1$plot_file <- d$plot_file %||% NA_character_
  for (k in names(d$files)) c1[[paste0(k, "_file")]] <- d$files[[k]]
  c1[c(setdiff(names(c1), attr(c1, "sf_column")), attr(c1, "sf_column"))]
}

# Interval for a percentage with a given SE: quantiles of the Beta distribution with that mean and SD
# (method of moments), so it stays within 0-100 and is skewed near the bounds. Where the SE is too large
# for any Beta with that mean (SD >= sqrt(p(1 - p))), the interval is the normal one cut at 0 and 100 and
# `feasible` is FALSE. (A logit-scale delta-method interval was used before: near 0 or 100 % it balloons
# to almost the whole range when the SE is comparable to the value.)
beta_interval <- function(pct, se_pct, level = 0.95) {
  a <- (1 - level) / 2
  p <- pmin(pmax(pct / 100, 1e-6), 1 - 1e-6)
  v <- (se_pct / 100)^2
  feasible <- !is.na(v) & v > 0 & v < p * (1 - p)
  phi <- ifelse(feasible, p * (1 - p) / v - 1, NA_real_)
  z <- stats::qnorm(1 - a)
  lo <- ifelse(feasible, 100 * stats::qbeta(a, p * phi, (1 - p) * phi), pmax(0, pct - z * se_pct))
  hi <- ifelse(feasible, 100 * stats::qbeta(1 - a, p * phi, (1 - p) * phi), pmin(100, pct + z * se_pct))
  zero <- !is.na(se_pct) & se_pct == 0
  list(lo = ifelse(zero, pct, lo), hi = ifelse(zero, pct, hi), feasible = feasible | zero | is.na(se_pct))
}

# Semicolon-separated flag text from named logical vectors (NA counts as FALSE)
flag_text <- function(...) {
  f <- list(...)
  n <- max(lengths(f))
  m <- matrix(vapply(f, \(v) rep_len(dplyr::coalesce(as.logical(v), FALSE), n), logical(n)), nrow = n)
  apply(m, 1, \(r) paste(names(f)[r], collapse = ";"))
}

# Sample points: sf with site_code, or a data frame with site_code, lat, lon
as_points <- function(p) {
  if (!inherits(p, "sf")) {
    p <- as.data.frame(p)
    if (!all(c("site_code", "lat", "lon") %in% names(p))) stop("points need `site_code`, `lat` and `lon` columns (or be sf)")
    p <- sf::st_as_sf(p[!is.na(p$lat) & !is.na(p$lon), ], coords = c("lon", "lat"), crs = 4326)
  }
  sf::st_transform(p[!sf::st_is_empty(p), "site_code"], akalbers_crs())
}

# On Windows a virus scanner or file sync can hold a new file for a moment, so renames are retried
rename_retry <- function(from, to, tries = 5) {
  for (i in seq_len(tries)) if (file.rename(from, to)) return(TRUE) else Sys.sleep(i)
  FALSE
}

# Put a finished file in place: copy it next to the target, then rename (atomic on one volume).
# `from` can be on another drive, or still held open by the program that wrote it.
replace_file <- function(from, to) {
  part <- sprintf("%s.%s.part", to, Sys.getpid())
  on.exit(unlink(c(from, part)))
  file.copy(from, part, overwrite = TRUE) && rename_retry(part, to)
}

# ScienceBase file links have changed form before; look up the current link in the item record.
# Files shown as "Shapefile: ..." are attached through a facet, so facets are searched too.
sb_download <- function(item, name, dest = file.path(ak_cache_dir(), name), pattern = NULL) {
  if (file.exists(dest)) return(dest)
  j <- ak_request(paste0("https://www.sciencebase.gov/catalog/item/", item), list(format = "json")) |>
    ak_perform() |>
    httr2::resp_body_json()
  files <- c(j$files, unlist(purrr::map(j$facets, "files"), recursive = FALSE))
  f <- purrr::detect(files, \(x) identical(x$name, name)) %||%
    if (!is.null(pattern)) purrr::detect(files, \(x) grepl(pattern, x$name, ignore.case = TRUE))
  if (is.null(f)) stop(name, " is not attached to ScienceBase item ", item, "; it has: ",
                       paste(purrr::map_chr(files, "name"), collapse = ", "), call. = FALSE)
  ak_download(f$downloadUri %||% f$url, dest)
}
