#' Daily weather averaged over each catchment (Daymet)
#'
#' Samples Daymet V4 (1 km, daily; Thornton et al. 2021) at points spread
#' evenly over each catchment (one per `km2_per_point`, between `n_min` and
#' `n_max`), downloads each 1 km pixel once through the single-pixel API
#' (cached), and averages the pixels to a catchment daily series. Pixels are
#' requested a few at a time (`options(AKCatchments.max_active = 4)`; see
#' [ak_parallel]), with a progress bar; failed requests are retried one by
#' one, and pixels that still fail are listed in `attr(w, "errors")`.
#'
#' This is the daily table the other weather functions work from:
#' [weather_at_samples()] (anomalies, melt and antecedent summaries at
#' sample dates), [ak_climate_normals()] with `source = "daymet4"` (normals
#' over a baseline) and [ak_snowoff()] (the melt model that fills MODIS
#' gaps). Download the years those need: the baseline (1991-2020 by
#' default) and the sample years.
#'
#' Uncertainty has two parts. Point sampling: the catchment mean comes from a
#' sample of its pixels, so `*_se_sampling` = between-pixel SD / sqrt(n) x
#' sqrt(1 - n / N) (finite population, N = catchment pixels). Interpolation:
#' Daymet's own station cross-validation gives North American daily MAE of
#' 1.52 C (Tmax) and 1.78 C (Tmin) with no Alaska-specific figure; error is
#' larger where stations are sparse (high elevation, remote Alaska) and is
#' largely persistent, so it partly cancels in anomalies. Use the Daymet
#' station cross-validation files (ORNL DAAC) for an Alaska estimate.
#'
#' Sites without a polygon can be given as points (sf POINT geometry): they
#' use the single pixel at the point, as the geo workflow did for outlets.
#' Sampling uses one random grid offset per call (`seed`), drawn over the
#' sites in the order given.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (or sf
#'   points), an `ak_delineation`, or a list of them.
#' @param years Years to download, e.g. `1991:2023`.
#' @param km2_per_point,n_min,n_max Sampling density and limits.
#' @param seed Random seed for the sampling grid offset.
#' @return A table (one row per site and day): `site_code`, `year`, `yday`, `date`, `tmin`, `tmax`,
#'   `tmean`, `prcp`, `swe`, `n_pixels`, `tmean_sd`, `prcp_sd`,
#'   `tmean_se_sampling`, `prcp_se_sampling`, `daymet_flag` (`few_pixels`
#'   where fewer than 3 distinct pixels make the mean, so the sampling SE is
#'   unreliable). The pixel table is attached as attribute `pixels`, the
#'   cached file of each pixel as attribute `pixel_files`, and the number of
#'   1 km pixels in each catchment as `npix_total` (used by
#'   by the melt model and the anomaly SEs of [weather_at_samples()]), and
#'   the provenance ([ak_provenance()]).
#' @seealso `vignette("weather-daymet")` (download, use, uncertainty, validation)
#' @export
ak_weather <- function(catchments, years, km2_per_point = 5, n_min = 10, n_max = 50, seed = 2026) {
  last <- as.integer(format(Sys.Date(), "%Y")) - 1
  if (min(years) < 1980 || max(years) > last) stop("Daymet years run from 1980 to ", last, call. = FALSE)
  catchments <- catchment_sf(catchments)
  x <- sf::st_transform(catchments[!sf::st_is_empty(catchments), "site_code"], akalbers_crs())
  x$area_m2 <- as.numeric(sf::st_area(x))
  set.seed(seed)
  n_pts <- pmin(n_max, pmax(n_min, ceiling(x$area_m2 / 1e6 / km2_per_point)))
  pts <- purrr::map2(sf::st_geometry(x), n_pts, \(g, n) {
    g <- sf::st_sfc(g, crs = akalbers_crs())
    if (inherits(g, "sfc_POINT")) return(g)
    p <- sf::st_sample(g, n, type = "regular")
    if (!length(p)) p <- sf::st_point_on_surface(g)
    sf::st_cast(p, "POINT")
  })
  allp <- do.call(c, pts)
  ll <- sf::st_coordinates(sf::st_transform(allp, 4326))
  # Daymet's 1 km Lambert conformal conic grid
  xy <- sf::st_transform(allp, "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") |>
    sf::st_coordinates()
  pixels <- dplyr::tibble(site_code = rep(x$site_code, lengths(pts)), lon = ll[, 1], lat = ll[, 2],
                          pixel = paste0("p", floor((xy[, 1] + 4560750) / 1000), "_", floor((4984500 - xy[, 2]) / 1000))) |>
    dplyr::distinct(.data$site_code, .data$pixel, .keep_all = TRUE)
  file_of <- daymet_file_of(ak_cache_dir(cache_paths$daymet), years)
  need <- pixels |>
    dplyr::distinct(.data$pixel, .keep_all = TRUE) |>
    dplyr::filter(!file.exists(file_of(.data$pixel)))
  failed <- daymet_download(need, years, file_of)
  npix_total <- stats::setNames(pmax(1, x$area_m2 / 1e6), x$site_code)
  ok_px <- pixels[file.exists(file_of(pixels$pixel)), ]
  out <- split(ok_px$pixel, ok_px$site_code) |>
    purrr::imap(\(px, sc) daymet_site_mean(purrr::map(px, \(p) readRDS(file_of(p))), sc, years, npix_total[[sc]])) |>
    dplyr::bind_rows()
  if (!nrow(out)) stop("No Daymet pixel could be downloaded: ", paste(utils::head(unique(failed$error), 3), collapse = "; "), call. = FALSE)
  out <- dplyr::mutate(out, date = as.Date(paste0(.data$year, "-01-01")) + (.data$yday - 1), .after = "yday")
  errors <- if (nrow(failed)) {
    pixels |>
      dplyr::inner_join(failed, by = "pixel") |>
      dplyr::summarise(error = paste0(dplyr::n(), " pixel(s) failed: ", dplyr::first(.data$error)), .by = "site_code") |>
      dplyr::mutate(step = "weather", .after = "site_code")
  }
  attr(out, "pixels") <- pixels
  attr(out, "pixel_files") <- stats::setNames(file_of(unique(ok_px$pixel)), unique(ok_px$pixel))
  attr(out, "npix_total") <- npix_total
  as_layer_table(out, layer_row("weather_daily", fn = "ak_weather"),
                 list(years = range(years), km2_per_point = km2_per_point, n_min = n_min, n_max = n_max, seed = seed),
                 note = paste(dplyr::n_distinct(ok_px$pixel), "Daymet pixels"), errors = errors)
}

# Download Daymet pixels (pixel, lon, lat) a few at a time; requests that fail are tried once more on their own
# (with retries). Each pixel is saved as an RDS of year, yday, prcp, swe, tmax, tmin. Returns failed pixels.
daymet_download <- function(need, years, file_of) {
  if (!nrow(need)) return(dplyr::tibble(pixel = character(), error = character()))
  req <- \(lat, lon) ak_request("https://daymet.ornl.gov/single-pixel/api/data", list(lat = lat, lon = lon, vars = "tmax,tmin,dayl,prcp,srad,swe,vp",
                                                 year = paste(seq(min(years), max(years)), collapse = ",")))
  tmp <- file.path(tempdir(), paste0("daymet_", need$pixel, "_", Sys.getpid(), ".csv"))
  on.exit(unlink(tmp))
  reqs <- purrr::map2(need$lat, need$lon, req)
  resps <- http_parallel(reqs, tmp, label = sprintf("Daymet pixels (%d)", length(reqs)))
  err <- character(nrow(need))
  for (i in seq_len(nrow(need))) {
    if (!inherits(resps[[i]], "httr2_response") || !file.exists(tmp[i])) {
      r <- tryCatch(ak_perform(reqs[[i]], path = tmp[i]), error = \(e) e)
      if (inherits(r, "error")) {
        err[i] <- if (grepl("HTTP 400", conditionMessage(r))) "outside Daymet coverage" else conditionMessage(r)
        next
      }
    }
    d <- tryCatch(daymet_parse(tmp[i]), error = \(e) e)
    if (inherits(d, "error")) {
      err[i] <- conditionMessage(d)
      next
    }
    saveRDS(d, file_of(need$pixel[i]))
  }
  dplyr::tibble(pixel = need$pixel, error = err) |> dplyr::filter(nzchar(.data$error))
}

# The single-pixel CSV: a few header lines, then a table starting "year,yday" (as daymetr::read_daymet reads it)
daymet_parse <- function(file) {
  head <- readLines(file, n = 30, warn = FALSE)
  k <- grep("^year,yday", head)[1]
  if (is.na(k)) stop("not a Daymet single-pixel table (format change?): ", substr(paste(head[1:3], collapse = " "), 1, 200))
  d <- utils::read.table(file, sep = ",", skip = k - 1, header = TRUE)
  dplyr::select(dplyr::as_tibble(d), "year", "yday", prcp = dplyr::starts_with("prcp"), swe = dplyr::starts_with("swe"),
                tmax = dplyr::starts_with("tmax"), tmin = dplyr::starts_with("tmin"))
}

snowoff_modis <- function(x, years = 2001:2022, min_valid_frac = 0.5, rmse_d = 8, cloud_days_max = NULL, keep_full = FALSE) {
  win <- sf::st_union(sf::st_buffer(x, 1000))
  bb <- round(sf::st_bbox(win) / 1000)
  key <- paste(bb, collapse = "_")
  file_of <- modis_file_of(ak_cache_dir(cache_paths$modis), win, key)
  need <- years[!file.exists(file_of(years))]
  if (length(need)) {
    docs <- ak_json("https://arcticdata.io/metacat/d1/mn/v2/query/solr/", list(q = "resourceMap:*A2057CV28*", fq = "-obsoletedBy:*",
                                                      fl = "identifier,fileName,dateUploaded", rows = 1000, wt = "json"))$response$docs
    docs <- docs[grepl("^\\d{4}_MODIS_snow_metrics_v6\\.tif$", docs$fileName), ]
    docs <- docs[order(docs$dateUploaded, decreasing = TRUE), ]
    docs <- docs[!duplicated(docs$fileName) & as.integer(substr(docs$fileName, 1, 4)) %in% need, ]
    if (!nrow(docs)) stop("The Arctic Data Center lists no MODIS snow metrics files for ", paste(need, collapse = ", "),
                          " (doi:10.18739/A2057CV28 has snow years 2001-2022)")
    purrr::pwalk(list(docs$identifier, docs$fileName, as.integer(substr(docs$fileName, 1, 4))), \(id, fn, yr) {
      full <- file.path(ak_cache_dir(cache_paths$modis, "full"), fn)
      temp <- !keep_full && !file.exists(full)
      if (temp) full <- tempfile(fileext = ".tif")
      ak_download(paste0("https://arcticdata.io/metacat/d1/mn/v2/object/", id), full)
      r <- terra::rast(full)[[c(5, 10, 11)]]
      terra::crop(r, terra::vect(sf::st_transform(win, terra::crs(r))), snap = "out", filename = file_of(yr), overwrite = TRUE)
      if (temp) unlink(full)
    })
  }
  years <- years[file.exists(file_of(years))]
  out <- purrr::map(years, \(yr) {
    r <- terra::rast(file_of(yr))
    off <- 365 + lubridate_leap(yr - 1)
    d <- terra::ifel(r[[2]] == 32 & r[[1]] > off & r[[1]] <= off + 211, r[[1]] - off, NA)
    xr <- sf::st_transform(x, terra::crs(d))
    exactextractr::exact_extract(d, xr, c("median", "count"),
                                 append_cols = c("site_code", "area_m2"), coverage_area = TRUE, progress = FALSE) |>
      dplyr::mutate(year = yr,
                    cloud = if (terra::nlyr(r) >= 3) exactextractr::exact_extract(r[[3]], xr, "mean", progress = FALSE) else NA_real_)
  }) |>
    purrr::list_rbind() |>
    dplyr::transmute(.data$site_code, .data$year, modis_valid_frac = .data$count / .data$area_m2,
                     modis_doy = dplyr::if_else(.data$modis_valid_frac >= min_valid_frac, .data$median, NA_real_),
                     modis_cloud_days = .data$cloud, snowoff_rmse_d = rmse_d)
  cut <- cloud_days_max %||% stats::quantile(out$modis_cloud_days, 0.9, na.rm = TRUE, names = FALSE)
  dplyr::mutate(out, snowoff_flag = flag_text(low_valid = .data$modis_valid_frac < min_valid_frac,
                                              cloudy = .data$modis_cloud_days > cut))
}

# Catchment daily series from its pixels' series, with the point-sampling SE (finite population of n_total pixels)
daymet_site_mean <- function(pixel_tables, site_code, years, n_total) {
  dplyr::bind_rows(pixel_tables) |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::mutate(tmean = (.data$tmin + .data$tmax) / 2) |>
    # the SDs first: summarise() sees columns it has already replaced by their mean
    dplyr::summarise(tmean_sd = stats::sd(.data$tmean), prcp_sd = stats::sd(.data$prcp), n_pixels = dplyr::n(),
                     dplyr::across(c("tmin", "tmax", "tmean", "prcp", "swe"), mean), .by = c("year", "yday")) |>
    dplyr::relocate("tmean_sd", "prcp_sd", "n_pixels", .after = "swe") |>
    dplyr::mutate(site_code = site_code, .before = 1,
                  fpc = sqrt(pmax(0, 1 - .data$n_pixels / n_total)),
                  tmean_se_sampling = .data$tmean_sd / sqrt(.data$n_pixels) * .data$fpc,
                  prcp_se_sampling = .data$prcp_sd / sqrt(.data$n_pixels) * .data$fpc,
                  daymet_flag = flag_text(few_pixels = .data$n_pixels < 3)) |>
    dplyr::select(-"fpc")
}

# Cached Daymet pixel files: reuse any <pixel>_<start>_<end>.rds whose years cover the request
# (the geo workflow wrote the same names); otherwise the name to download to
daymet_file_of <- function(cache, years) {
  have <- list.files(cache, "^p-?\\d+_\\d+_\\d{4}_\\d{4}\\.rds$")
  have <- data.frame(file = have, pixel = sub("_\\d{4}_\\d{4}\\.rds$", "", have),
                     y0 = as.integer(sub(".*_(\\d{4})_\\d{4}\\.rds$", "\\1", have)),
                     y1 = as.integer(sub(".*_(\\d{4})\\.rds$", "\\1", have)))
  have <- have[have$y0 <= min(years) & have$y1 >= max(years), ]
  \(p) ifelse(p %in% have$pixel, file.path(cache, have$file[match(p, have$pixel)]),
              file.path(cache, sprintf("%s_%d_%d.rds", p, min(years), max(years))))
}

# Cached MODIS crops: the geo workflow's modis_snow_<year>.tif if it covers the window, else a keyed name
modis_file_of <- function(dir, win, key) {
  covers <- \(f) file.exists(f) && {
    r <- terra::rast(f)
    e <- as.vector(terra::ext(r))
    b <- sf::st_bbox(sf::st_transform(win, terra::crs(r)))
    e[["xmin"]] <= b[["xmin"]] && e[["xmax"]] >= b[["xmax"]] && e[["ymin"]] <= b[["ymin"]] && e[["ymax"]] >= b[["ymax"]]
  }
  \(yr) vapply(yr, \(y) {
    plain <- file.path(dir, sprintf("modis_snow_%d.tif", y))
    if (covers(plain)) plain else file.path(dir, sprintf("modis_snow_%d_%s.tif", y, key))
  }, "")
}

lubridate_leap <- function(y) as.integer((y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0)

classify_lith <- function(x) {
  x <- tolower(as.character(x))
  dplyr::case_when(
    grepl("limestone|dolomit|carbonate|marble|calcareous", x) ~ "carbonate",
    grepl("alluvium|colluvium|glacial|surficial|unconsolidated|moraine|loess|\\bsands?\\b|gravel|\\btill\\b", x) ~ "unconsolidated",
    grepl("shale|siltstone|mudstone|argillite|claystone|flysch", x) ~ "fine_clastic",
    grepl("granit|rhyolit|granodiorit|quartz monzonit|felsic|tonalit", x) ~ "felsic_intrusive",
    grepl("basalt|gabbro|diabase|ultramafic|peridotit|dunite|mafic|greenstone|volcanic", x) ~ "mafic_volcanic",
    grepl("schist|gneiss|phyllite|slate|amphibolit|metamorph|migmatit", x) ~ "metamorphic",
    grepl("sandstone|conglomerate|graywacke|greywacke", x) ~ "coarse_clastic",
    .default = "other")
}

#' Lithology patterns searched in the Alaska geologic map
#'
#' Regular expressions matched against the SIM 3340 lithology fields
#' (LITH1-LITH5). Edit or extend and pass to [ak_geology()].
#'
#' @return A named character vector.
#' @seealso `vignette("geology-sim3340")` (download, use, uncertainty, validation)
#' @export
ak_lithology_patterns <- function() {
  c(gneiss = "gneiss", schist = "schist", mica_mentioned = "mica|muscovite|biotite|phlogopite|lepidolite|tobelite",
    breccia = "breccia", shale = "shale", mudstone = "mudstone|claystone", siltstone = "siltstone",
    argillite = "argillite", slate = "slate", phyllite = "phyllite", coal = "\\bcoal\\b|coaly|lignite|anthracite",
    black_shale = "black[ -]+shale|shale[^;,.]*black", carbonaceous = "carbonaceous|organic[ -]+rich|coaly|graphitic|graphite",
    fine_sedimentary = "shale|mudstone|claystone|siltstone|argillite|flysch",
    metasedimentary = "metasediment|pelitic|metapelit|paragneiss", carbonate = "limestone|dolomit|carbonate|marble|calcareous",
    unconsolidated = "alluvium|colluvium|glacial|surficial|unconsolidated|moraine|loess|gravel|\\btill\\b",
    mafic = "basalt|gabbro|diabase|ultramafic|peridotit|dunite|mafic|greenstone",
    felsic = "granit|rhyolit|granodiorit|quartz monzonit|felsic|tonalit")
}

#' Mapped lithology occurrence in each catchment
#'
#' From the Geologic Map of Alaska (Wilson et al. 2015, USGS SIM 3340;
#' compiled from 1:250,000 maps, nominal use 1:250,000), downloaded once.
#' For each pattern, the percent of the whole catchment under map units whose
#' lithology lists it, using all lithology ranks (`geol_contains_pct_*`) and
#' major ranks only (`geol_major_contains_pct_*`). Categories overlap and
#' must not be summed; a zero means no matching description, not absence.
#'
#' Uncertainty: composition within mixed map units is unknown, so the two
#' versions bound the share (major-only low, all-rank high). Boundary
#' placement adds positional error: pass the returned `units` polygons with a
#' 0/1 field to [positional_uncertainty()] with `sigma_m` about 127 m (the
#' 1:250,000 map-accuracy tolerance, itself a 90th percentile, used as an SD:
#' about twice the standard, because contacts are not well-defined points;
#' test 2-4 times that).
#'
#' `occurrence` also carries the geo workflow's equal-allocation classes:
#' `geol_pct_<class>` splits each map unit equally among its major
#' lithologies (all of them if none is marked major), classified as
#' carbonate, unconsolidated, fine clastic, felsic intrusive, mafic volcanic,
#' metamorphic, coarse clastic or other (percent of the mapped part, summing
#' to 100), with `geol_fast_weathering_pct` (carbonate + mafic) and
#' `geol_thaw_susceptible_pct` (unconsolidated + fine clastic). Equal
#' allocation is an assumption, so prefer the occurrence bounds.
#'
#' Ranks are SIM 3340's: Major (at least 33% of the unit), Minor (10-33%),
#' Incidental (under 10%), Indeterminate. For rock N, S and major oxides
#' from the same lithologies see [ak_rock_chem()].
#'
#' `geol_flag`: `partial_coverage` (under 95% mapped),
#' `undescribed_units` (some mapped units have no lithology description).
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param patterns Named regular expressions, see [ak_lithology_patterns()].
#' @param url Map geodatabase download URL.
#' @param details Also return the map-unit tables?
#' @return `catchments` with the `geol_*` occurrence and class columns
#'   added. With `details = TRUE`, a list: `catchments` (that), `unit_areas`
#'   (site x map unit) and `units` (the map units in the catchments, with one
#'   logical column per pattern and rank).
#' @seealso `vignette("geology-sim3340")` (download, use, uncertainty, validation)
#' @export
ak_geology <- function(catchments, patterns = ak_lithology_patterns(), url = "https://pubs.usgs.gov/sim/3340/sim3340_gdb.zip",
                       details = FALSE) {
  x <- as_catchments(catchments)
  g <- geology_tables(x, url)
  units <- dplyr::transmute(g$units, NSACLASS = as.character(.data$NSACLASS))
  lith <- g$lith
  lith_cols <- intersect(paste0("LITH", 1:5), names(lith))
  ev <- lith |>
    dplyr::mutate(NSACLASS = as.character(.data$NSACLASS), dplyr::across(dplyr::all_of(lith_cols), \(v) dplyr::coalesce(as.character(v), "")),
                  major = !is.na(.data$RANK) & grepl("major", .data$RANK, ignore.case = TRUE),
                  described = nzchar(trimws(do.call(paste0, dplyr::pick(dplyr::all_of(lith_cols))))))
  for (nm in names(patterns)) ev[[nm]] <- Reduce(`|`, lapply(ev[lith_cols], \(z) grepl(patterns[[nm]], z, ignore.case = TRUE, perl = TRUE)))
  flags <- ev |>
    dplyr::summarise(has_lith = any(.data$described), has_major = any(.data$described & .data$major),
                     dplyr::across(dplyr::all_of(names(patterns)), any, .names = "{.col}_any"), .by = "NSACLASS") |>
    dplyr::left_join(ev |>
                       dplyr::filter(.data$major) |>
                       dplyr::summarise(dplyr::across(dplyr::all_of(names(patterns)), any, .names = "{.col}_major"), .by = "NSACLASS"),
                     by = "NSACLASS")
  units <- dplyr::left_join(units, flags, by = "NSACLASS")
  p <- suppressWarnings(sf::st_intersection(x[c("site_code", "area_m2")], units))
  p$a <- as.numeric(sf::st_area(p))
  pd <- sf::st_drop_geometry(p)
  share <- \(col) pd |> dplyr::summarise(v = 100 * sum(.data$a[isTRUE_v(.data[[col]])]) / dplyr::first(.data$area_m2), .by = "site_code")
  occ <- dplyr::tibble(site_code = x$site_code) |>
    dplyr::left_join(pd |> dplyr::summarise(geol_mapped_pct = 100 * sum(.data$a) / dplyr::first(.data$area_m2), .by = "site_code"), by = "site_code") |>
    dplyr::left_join(dplyr::rename(share("has_lith"), geol_described_pct = "v"), by = "site_code") |>
    dplyr::left_join(dplyr::rename(share("has_major"), geol_major_described_pct = "v"), by = "site_code")
  for (nm in names(patterns)) {
    occ <- occ |>
      dplyr::left_join(stats::setNames(share(paste0(nm, "_any")), c("site_code", paste0("geol_contains_pct_", nm))), by = "site_code") |>
      dplyr::left_join(stats::setNames(share(paste0(nm, "_major")), c("site_code", paste0("geol_major_contains_pct_", nm))), by = "site_code")
  }
  lith_share <- lith |>
    dplyr::transmute(NSACLASS = as.character(.data$NSACLASS),
                     lith_class = classify_lith(do.call(paste, dplyr::pick(dplyr::all_of(lith_cols)))),
                     major = grepl("major", .data$RANK, ignore.case = TRUE)) |>
    dplyr::filter(.data$major | !any(.data$major), .by = "NSACLASS") |>
    dplyr::count(.data$NSACLASS, .data$lith_class) |>
    dplyr::mutate(share = .data$n / sum(.data$n), .by = "NSACLASS")
  classes <- pd |>
    dplyr::select("site_code", "NSACLASS", "a") |>
    dplyr::left_join(lith_share, by = "NSACLASS", relationship = "many-to-many") |>
    dplyr::mutate(lith_class = factor(dplyr::coalesce(.data$lith_class, "other"),
                                      c("carbonate", "unconsolidated", "fine_clastic", "felsic_intrusive",
                                        "mafic_volcanic", "metamorphic", "coarse_clastic", "other")),
                  share = dplyr::coalesce(.data$share, 1)) |>
    dplyr::summarise(a = sum(.data$a * .data$share), .by = c("site_code", "lith_class")) |>
    dplyr::mutate(pct = 100 * .data$a / sum(.data$a), .by = "site_code") |>
    tidyr::pivot_wider(id_cols = "site_code", names_from = "lith_class", values_from = "pct", names_expand = TRUE,
                       names_prefix = "geol_pct_", values_fill = 0) |>
    dplyr::mutate(geol_fast_weathering_pct = .data$geol_pct_carbonate + .data$geol_pct_mafic_volcanic,
                  geol_thaw_susceptible_pct = .data$geol_pct_unconsolidated + .data$geol_pct_fine_clastic)
  occ <- dplyr::left_join(occ, classes, by = "site_code")
  occ$geol_flag <- flag_text(partial_coverage = occ$geol_mapped_pct < 95,
                             undescribed_units = occ$geol_described_pct < occ$geol_mapped_pct - 1)
  out <- add_columns(catchments, occ, layer_row("geology", fn = "ak_geology"), list(patterns = names(patterns)))
  if (!details) return(out)
  list(catchments = out,
       unit_areas = pd |> dplyr::summarise(area_km2 = sum(.data$a) / 1e6, pct_catchment = 100 * sum(.data$a) / dplyr::first(.data$area_m2),
                                           .by = c("site_code", "NSACLASS")),
       units = units)
}

# The SIM 3340 geodatabase, downloaded and unpacked once
# The SIM 3340 map units around the catchments (all their attribute fields, EPSG:3338) and the lithology table
# (NSALith: NSACLASS, LITH1-LITH5, RANK, ...). The unpacked archive may hold more than one .gdb: use the one with
# the map units.
geology_tables <- function(x, url = "https://pubs.usgs.gov/sim/3340/sim3340_gdb.zip") {
  gdbs <- geology_gdb(url)
  lyrs <- lapply(gdbs, \(g) sf::st_layers(g)$name)
  k <- which(vapply(lyrs, \(l) all(c("akstategeol_poly", "nsalith") %in% tolower(l)), logical(1)))[1]
  if (is.na(k)) stop("No geodatabase with AKStategeol_poly and NSALith layers in ", ak_cache_dir(cache_paths$geology), ": ",
                     paste(basename(gdbs), "has", vapply(lyrs, paste, "", collapse = ", "), collapse = "; "), call. = FALSE)
  gdb <- gdbs[k]
  lyr <- lyrs[[k]]
  poly_lyr <- lyr[tolower(lyr) == "akstategeol_poly"][1]
  read <- \(n, ...) suppressWarnings(sf::st_read(gdb, layer = lyr[tolower(lyr) == tolower(n)][1], quiet = TRUE, ...)) |>
    dplyr::rename_with(toupper, -dplyr::any_of(c("Shape", "geometry", "geom")))
  units <- read("AKStategeol_poly", wkt_filter = bbox_wkt(sf::st_union(sf::st_geometry(x)), gdb, poly_lyr)) |>
    dplyr::mutate(NSACLASS = as.character(.data$NSACLASS)) |>
    sf::st_make_valid() |>
    sf::st_transform(akalbers_crs()) |>
    sf::st_filter(x)
  list(units = units, lith = dplyr::mutate(sf::st_drop_geometry(read("nsalith")), NSACLASS = as.character(.data$NSACLASS)),
       gdb = gdb, layers = lyr)
}

geology_gdb <- function(url = "https://pubs.usgs.gov/sim/3340/sim3340_gdb.zip") {
  dir <- ak_cache_dir(cache_paths$geology)
  find <- \() list.files(dir, "\\.gdb$", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  if (!length(find())) extract_archive(ak_download(url, file.path(ak_cache_dir(), "sim3340_gdb.zip")), dir)
  find()
}

#' STATSGO2 soil properties for each catchment
#'
#' Queries Soil Data Access (STATSGO2, 1:250,000; the only soil survey with
#' full coverage of these catchments) for the map units in each catchment and
#' summarises them area-weighted: organic matter, clay, sand, Ksat and bulk
#' density (0-50 cm, component-weighted), and the share of Gelisols,
#' Histosols and poorly drained components.
#'
#' Uncertainty: each property has low / representative / high values in SDA
#' (spread of the property in the component, often about 10th-90th or
#' 5th-95th percentiles; NSSH Part 618) and each component a low /
#' representative / high share of its map unit. Both are returned in
#' `mapunits` and `components`. They describe variability, not a confidence
#' interval for the catchment value.
#'
#' In `summary`:
#' * property bounds `soil_*_lo` / `soil_*_hi`: the area-weighted L and H
#'   values (a range, not an interval);
#' * `_se`, `_lo95`, `_hi95` for the Gelisol, Histosol and poorly drained
#'   shares, from `n` draws of component shares: a Dirichlet per map unit
#'   centred on the representative shares, with its concentration set so the
#'   L-H spread is the `lh_range` central range (median over the map unit's
#'   components),
#'   or `concentration` where L/H are missing;
#' * `soil_flag`: `assumed_concentration` where map units without L/H make up
#'   more than half the catchment, `no_soil_data` where SDA returned nothing.
#' Map-unit boundaries carry 1:250,000 positional error; add it with
#' [positional_uncertainty()] on the map-unit polygons if needed.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param n Number of draws.
#' @param concentration Dirichlet concentration where L/H are missing.
#' @param seed Random seed.
#' @param lh_range The central share of the component-share distribution
#'   that the L-H range is taken to span: 0.9 (5th-95th percentiles, the
#'   default) or 0.8 (10th-90th, which implies a larger SD). NSSH Part 618
#'   allows either.
#' @param details Also return the map-unit and component tables?
#' @return `catchments` with the `soil_*` columns added. With `details =
#'   TRUE`, a list: `catchments` (that), `mapunits` (site x map unit weights
#'   and property L/R/H) and `components` (site x component with share
#'   L/R/H, order, drainage class).
#' @seealso `vignette("soils-statsgo")` (download, use, uncertainty, validation)
#' @export
ak_soils <- function(catchments, n = 500, concentration = 20, seed = 1, lh_range = 0.9, details = FALSE) {
  if (!requireNamespace("soilDB", quietly = TRUE)) stop("Install the soilDB package")
  x <- as_catchments(catchments)
  props <- paste0(rep(c("om", "claytotal", "sandtotal", "ksat", "dbthirdbar"), each = 3), c("_l", "_r", "_h"))
  # Simplified to 30 m: raster-traced outlines are too long a query for SDA, and STATSGO is 1:250,000
  items <- stats::setNames(lapply(seq_len(nrow(x)), \(i) sf::st_simplify(x[i, ], preserveTopology = TRUE, dTolerance = 30) |>
                                    sf::st_transform(4326)), x$site_code)
  sres <- site_map(items, soils_one, props = props, label = "Soil Data Access")
  res <- site_values(sres)
  summary <- soils_summary(x, res, n, concentration, seed, lh_range, props)
  out <- add_columns(catchments, summary$summary, layer_row("soils", fn = "ak_soils"),
                     list(n = n, concentration = concentration, seed = seed, lh_range = lh_range), errors = site_errors(sres, "soils"))
  if (!details) return(out)
  list(catchments = out, mapunits = summary$mapunits, components = summary$components)
}

# One catchment's STATSGO map units, properties and components from Soil Data Access
soils_one <- function(g, props) {
  sc <- g$site_code
  # SDA sometimes drops a query server-side ("session is in the kill state"); try up to 3 times
  for (k in 1:3) {
    mu <- tryCatch(soilDB::SDA_spatialQuery(g, what = "mupolygon", db = "STATSGO", geomIntersection = TRUE),
                   error = \(e) e)
    if (inherits(mu, "sf")) break
    Sys.sleep(5 * k)
  }
  if (!inherits(mu, "sf") || !nrow(mu)) {
    why <- if (inherits(mu, c("error", "try-error"))) conditionMessage(attr(mu, "condition") %||% mu) else "no STATSGO map units returned"
    if (inherits(mu, c("error", "try-error"))) stop("SDA: ", why, call. = FALSE)
    warning(sc, " SDA: ", why, call. = FALSE)
    return(NULL)
  }
  mu <- sf::st_drop_geometry(mu) |>
    dplyr::mutate(mukey = as.character(.data$mukey)) |>
    dplyr::summarise(w = sum(.data$area_ac), .by = "mukey") |>
    dplyr::mutate(w = .data$w / sum(.data$w), site_code = sc, .before = 1)
  pr <- tryCatch(soilDB::get_SDA_property(property = props, method = "Weighted Average", mukeys = unique(mu$mukey),
                                          top_depth = 0, bottom_depth = 50, include_minors = TRUE, miscellaneous_areas = FALSE),
                 error = \(e) { warning(sc, " SDA properties: ", conditionMessage(e)); NULL })
  co <- tryCatch(soilDB::SDA_query(sprintf("SELECT c.mukey, c.cokey, c.comppct_l, c.comppct_r, c.comppct_h, c.taxorder, c.drainagecl
                                            FROM component c WHERE c.mukey IN (%s)", paste(unique(mu$mukey), collapse = ","))),
                 error = \(e) { warning(sc, " SDA components: ", conditionMessage(e)); NULL })
  if (inherits(co, "try-error")) warning(sc, " SDA components: ", co)
  if (inherits(pr, "try-error")) warning(sc, " SDA properties: ", pr)
  list(mapunits = if (is.data.frame(pr)) dplyr::left_join(mu, dplyr::mutate(pr, mukey = as.character(.data$mukey)), by = "mukey"),
       components = if (is.data.frame(co)) dplyr::left_join(mu, dplyr::mutate(co, mukey = as.character(.data$mukey)), by = "mukey"))
}

soils_summary <- function(x, res, n, concentration, seed, lh_range, props) {
  # Zero-row templates, so sites without soil data get NA and a flag instead of an error
  mapunits <- purrr::map(res, "mapunits") |> dplyr::bind_rows()
  if (!nrow(mapunits)) mapunits <- dplyr::tibble(site_code = character(), mukey = character(), w = numeric(),
                                                 !!!stats::setNames(rep(list(numeric()), length(props)), props))
  components <- purrr::map(res, "components") |> dplyr::bind_rows()
  if (!nrow(components)) components <- dplyr::tibble(site_code = character(), mukey = character(), w = numeric(),
                                                     comppct_r = numeric(), taxorder = character(), drainagecl = character())
  wm <- \(v, w) stats::weighted.mean(v, w, na.rm = TRUE)
  summary <- dplyr::tibble(site_code = x$site_code) |>
    dplyr::left_join(mapunits |> dplyr::summarise(soil_om_pct = wm(.data$om_r, .data$w), soil_clay_pct = wm(.data$claytotal_r, .data$w),
                                                  soil_sand_pct = wm(.data$sandtotal_r, .data$w), soil_ksat = wm(.data$ksat_r, .data$w),
                                                  soil_bulkdens = wm(.data$dbthirdbar_r, .data$w), .by = "site_code"), by = "site_code") |>
    dplyr::left_join(components |> dplyr::mutate(wt = .data$w * .data$comppct_r / 100) |>
                       dplyr::summarise(soil_gelisol_pct = 100 * sum(.data$wt[isTRUE_v(.data$taxorder == "Gelisols")], na.rm = TRUE) / sum(.data$wt, na.rm = TRUE),
                                        soil_histosol_pct = 100 * sum(.data$wt[isTRUE_v(.data$taxorder == "Histosols")], na.rm = TRUE) / sum(.data$wt, na.rm = TRUE),
                                        soil_poorly_drained_pct = 100 * sum(.data$wt[isTRUE_v(grepl("poorly", .data$drainagecl, ignore.case = TRUE))], na.rm = TRUE) /
                                          sum(.data$wt, na.rm = TRUE), .by = "site_code"), by = "site_code")
  bounds <- if (nrow(mapunits)) {
    mapunits |>
      dplyr::summarise(soil_om_pct_lo = wm(.data$om_l, .data$w), soil_om_pct_hi = wm(.data$om_h, .data$w),
                       soil_clay_pct_lo = wm(.data$claytotal_l, .data$w), soil_clay_pct_hi = wm(.data$claytotal_h, .data$w),
                       soil_sand_pct_lo = wm(.data$sandtotal_l, .data$w), soil_sand_pct_hi = wm(.data$sandtotal_h, .data$w),
                       soil_ksat_lo = wm(.data$ksat_l, .data$w), soil_ksat_hi = wm(.data$ksat_h, .data$w),
                       soil_bulkdens_lo = wm(.data$dbthirdbar_l, .data$w), soil_bulkdens_hi = wm(.data$dbthirdbar_h, .data$w),
                       .by = "site_code")
  }
  draws <- if (nrow(components)) soil_share_draws(components, n, concentration, seed, lh_range)
  summary <- summary |>
    dplyr::left_join(bounds %||% dplyr::tibble(site_code = character()), by = "site_code") |>
    dplyr::left_join(draws %||% dplyr::tibble(site_code = character(), assumed_frac = numeric()), by = "site_code") |>
    dplyr::mutate(soil_flag = flag_text(no_soil_data = is.na(.data$soil_om_pct) & is.na(.data$soil_gelisol_pct),
                                        assumed_concentration = .data$assumed_frac > 0.5)) |>
    dplyr::select(-"assumed_frac")
  list(summary = summary, mapunits = mapunits, components = components)
}

# Dirichlet draws of component shares per map unit -> SE and 95% interval of the
# Gelisol, Histosol and poorly drained shares of each catchment
soil_share_draws <- function(components, n = 500, concentration = 20, seed = 1, lh_range = 0.9) {
  set.seed(seed)
  co <- components |>
    dplyr::filter(!is.na(.data$comppct_r), .data$comppct_r > 0) |>
    dplyr::mutate(p = .data$comppct_r / sum(.data$comppct_r),
                  sd = (.data$comppct_h - .data$comppct_l) / 100 / (2 * stats::qnorm(1 - (1 - lh_range) / 2)),
                  conc_k = .data$p * (1 - .data$p) / .data$sd^2 - 1, .by = c("site_code", "mukey"))
  mu <- co |>
    dplyr::summarise(n_comp = dplyr::n(), w = dplyr::first(.data$w),
                     conc = stats::median(.data$conc_k[is.finite(.data$conc_k) & .data$conc_k > 0]),
                     .by = c("site_code", "mukey")) |>
    # a one-component map unit has no composition to be uncertain about, so nothing is assumed for it
    dplyr::mutate(assumed = is.na(.data$conc) & .data$n_comp > 1, conc = pmin(pmax(dplyr::coalesce(.data$conc, concentration), 2), 1000))
  co <- dplyr::left_join(co, mu[c("site_code", "mukey", "conc")], by = c("site_code", "mukey"))
  ind <- list(gelisol = co$taxorder %in% "Gelisols", histosol = co$taxorder %in% "Histosols",
              poorly_drained = grepl("poorly", co$drainagecl, ignore.case = TRUE))
  g <- matrix(stats::rgamma(nrow(co) * n, shape = co$p * co$conc), nrow = nrow(co))
  key <- paste(co$site_code, co$mukey)
  share <- g / rowsum(g, key)[key, , drop = FALSE]          # Dirichlet draws, columns = draws
  wt <- share * co$w
  out <- purrr::imap(ind, \(i, nm) {
    num <- rowsum(wt * i, co$site_code)
    den <- rowsum(wt, co$site_code)
    v <- 100 * num / den
    stats::setNames(data.frame(rownames(v), apply(v, 1, stats::sd), apply(v, 1, stats::quantile, 0.025),
                               apply(v, 1, stats::quantile, 0.975)),
                    c("site_code", paste0("soil_", nm, "_pct_", c("se", "lo95", "hi95"))))
  }) |>
    purrr::reduce(dplyr::left_join, by = "site_code")
  dplyr::left_join(out, mu |> dplyr::summarise(assumed_frac = sum(.data$w[.data$assumed]) / sum(.data$w), .by = "site_code"),
                   by = "site_code")
}

aktemp_stations <- function(base = "https://aktemp.uaa.alaska.edu/viz/data/") {
  s <- ak_json(paste0(base, "stations.json"))
  if (!all(c("latitude", "longitude") %in% names(s))) stop("stations.json has no latitude/longitude; fields: ", paste(names(s), collapse = ", "))
  sf::st_as_sf(s, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |> sf::st_transform(akalbers_crs())
}

aktemp_daily <- function(stations, base = "https://aktemp.uaa.alaska.edu/viz/data/") {
  s <- sf::st_drop_geometry(stations)
  purrr::pmap(s[c("dataset", "provider_station_code", "filename")], \(dataset, provider_station_code, filename) {
    d <- ak_json(paste0(base, "data/", filename))
    dplyr::tibble(dataset = dataset, provider_station_code = provider_station_code, date = as.Date(d$date),
                  temp_c = d$temp_c, airtemp_c = d$airtemp_c)
  }) |>
    purrr::list_rbind()
}
