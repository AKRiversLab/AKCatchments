#' Annual peak greenness (Landsat NDVImax) per catchment
#'
#' Annual maximum summer NDVI for each catchment from Landsat 5, 7, 8 and 9
#' Collection 2 Level-2 surface reflectance (30 m, 1984 on), read from the
#' Microsoft Planetary Computer (no login needed), following the methods of
#' Berner et al. (2020) and the 'LandsatTS' R package (Berner et al. 2023):
#' the same screening, cross-sensor calibration and phenology-adjusted peak.
#'
#' **Sampling.** Each catchment is sampled on a regular grid of points
#' (about one per `km2_per_point`, between `n_min` and `n_max`); every summer
#' scene (`doy`, day of year) is read only at those points.
#'
#' **Screening** (as LandsatTS `lsat_clean_data()`, a little stricter): a
#' pixel is kept if QA_PIXEL marks it clear and not fill, dilated cloud,
#' cloud, cloud shadow, snow or water; QA_RADSAT is 0; the scene's cloud
#' cover is at most `max_cloud`; the sun is at least 30 degrees up; and red
#' and NIR reflectance are within 0.005-1.
#'
#' **Cross-sensor calibration** (`xcal = TRUE`, as LandsatTS
#' `lsat_calibrate_poly()`): Landsat 5 and Landsat 8/9 NDVI are mapped onto
#' Landsat 7 with a polynomial (degree 1-3 by AIC) fitted to paired 15-day
#' medians at points and years both sensors observed, trained on 75% of the
#' points and evaluated on the rest (`attr(, "xcal")`). Landsat 9 is treated
#' as Landsat 8 (OLI-2 and OLI are near identical). The calibration is pooled
#' over all catchments in the call, so call it on all of them together;
#' sensors with fewer than 30 training points stay uncalibrated (flag).
#'
#' **Annual maximum** (as LandsatTS `lsat_fit_phenological_curves()` and
#' `lsat_summarize_growing_seasons()`): for each point and year, a smoothing
#' spline of NDVI against day of year is fitted to the observations of a
#' 7-year window (weights decay with distance from the year), outliers more
#' than 30% off the curve are removed, and each observation of the year is
#' moved up to the curve's peak; `NDVImax` is the median of these
#' adjusted values (observations on the curve's shoulders, below 75% of the
#' peak, left out), or the 90th percentile of the observations if that is
#' higher. Points without 20 observations of NDVI >= 0.15 in a window
#' (rock, ice, water, mostly bare ground) get no value: `veg_frac` is the
#' share of a catchment's points that are vegetated, and `ndvi_max`
#' describes the vegetated part only.
#'
#' **Uncertainty** of a catchment's value in a year, `ndvi_max_se`, combines
#' three parts (square root of the sum of squares):
#' * spatial sampling: SD of the point values / sqrt(points);
#' * peak estimation: the root-mean-square of the points' SEs (residual SD
#'   of their spline fit / sqrt(observations used)), taken as fully shared
#'   among points (they see the same scenes on the same days), which is
#'   conservative;
#' * calibration: the mean SE of the fitted calibration curve at the
#'   observations used (systematic, shared by all points).
#' The interval is normal, cut to -1-1. Not included: change of the map
#' between pixel and point (none: points are read from the native grid),
#' sensor radiometric error beyond the calibration, and the bias of a peak
#' estimated from very few clear days (flag `few_obs`).
#'
#' `flag`: `few_points` (fewer than 10 points with a value), `low_coverage`
#' (under half the vegetated points have a value that year),
#' `sparse_vegetation` (`veg_frac` under 0.5), `few_obs` (median of under 3
#' clear observations per point), `record_edge` (within 3 years of the ends of
#' the point records, where the 7-year window is one-sided), `uncalibrated`
#' (some observations from a sensor that could not be calibrated).
#'
#' **Cost and parallel processing.** Every summer scene is read at the points (four small reads of
#' cloud-optimised GeoTIFFs per scene). Nearby catchments share scenes, so
#' they are processed together in groups. Readings are cached per catchment
#' in `landsat_c2/` and only new scenes are read later (`refresh = TRUE` to
#' start again). A first run for one region and 40 years can take hours.
#' Groups are read in parallel when mirai daemons are running (see
#' [ak_parallel]); each worker reads its own scenes.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param years Years.
#' @param doy Days of year counted as summer.
#' @param km2_per_point,n_min,n_max Point density and limits per catchment.
#' @param max_cloud Maximum scene cloud cover, percent.
#' @param tiers Landsat collection categories (`"T1"`, `"T2"`).
#' @param xcal Cross-calibrate sensors to Landsat 7.
#' @param refresh Read every scene again.
#' @return A table, one row per site and year: `ndvi_max`, `ndvi_max_se`,
#'   `ndvi_max_lo95`, `ndvi_max_hi95`, `ndvi_max_cal_se`, `n_points`
#'   (sampled), `n_points_valid`, `veg_frac`, `obs_per_point` (median),
#'   `sensors`, `flag`. Attributes: `points` (per point and year), `xcal`
#'   (calibration evaluation), `failed` (scenes that could not be read), and
#'   the provenance. Summarise it to one row per site with
#'   [peak_ndvi_summary()] and add that to the catchments with
#'   [catchment_join()].
#' @references Berner et al. 2020, Nat. Commun. 11:4621; Berner et al. 2023,
#'   Ecography e06768 ('LandsatTS', <https://github.com/logan-berner/LandsatTS>,
#'   MIT licence; the calibration and phenology steps here are ported from it).
#' @seealso [peak_ndvi_summary()]
#' @seealso `vignette("greenness-landsat")` (download, use, uncertainty, validation)
#' @export
ak_peak_ndvi <- function(catchments, years = 1984:(as.integer(format(Sys.Date(), "%Y")) - 1), doy = 152:243,
                                 km2_per_point = 0.1, n_min = 20, n_max = 150, max_cloud = 80, tiers = "T1",
                                 xcal = TRUE, refresh = FALSE) {
  x <- as_catchments(catchments)
  pts <- lsat_points(x, km2_per_point, n_min, n_max)
  obs <- lsat_observations(x, pts, years, doy, max_cloud, tiers, refresh)
  clean <- lsat_clean(obs$obs, max_cloud)
  cal <- if (xcal) lsat_xcal(clean, doy) else
    list(obs = dplyr::mutate(clean, ndvi_x = .data$ndvi, cal_se = 0, calibrated = .data$sensor == "L7"), eval = NULL)
  pp <- lsat_peaks(cal$obs)
  out <- lsat_catchment_years(pp, pts, years)
  sd <- if (is.null(cal$eval)) NULL else stats::setNames(cal$eval$cal_sd, cal$eval$sensor)
  out$cal_sd_L5 <- unname(sd["L5"] %||% NA_real_)
  out$cal_sd_L8 <- unname(sd["L8"] %||% NA_real_)
  attr(out, "points") <- pp
  attr(out, "xcal") <- cal$eval
  attr(out, "failed") <- obs$failed
  as_layer_table(out, layer_row("greenness", fn = "ak_peak_ndvi"),
                 list(years = range(years), doy = range(doy), km2_per_point = km2_per_point, n_min = n_min, n_max = n_max,
                      max_cloud = max_cloud, tiers = tiers, xcal = xcal))
}

#' Long-term greenness and its trend per catchment
#'
#' From the annual values of [ak_peak_ndvi()]: the mean NDVImax over
#' `years` and the linear trend (per decade) over `trend_years`.
#'
#' * `ndvimax_mean_se`: sqrt(SD of the annual values^2 / n + mean calibration
#'   SE^2). The first part treats the years as a sample of the period and
#'   includes their estimation error; the second is the calibration error,
#'   which doesn't average out.
#' * `ndvimax_trend_decade_se`: the OLS slope SE, with the number of years
#'   discounted for lag-1 autocorrelation of the residuals. Calibration
#'   offsets that differ between sensors can make or hide a trend, since the
#'   sensors follow one another in time (5 to 1999-2011, 7 to 2022, 8/9 from
#'   2013); flag `sensor_change` marks trends spanning a change of sensors
#'   without calibration.
#' * Flags: `few_years` (under 10 years with values), plus the most common
#'   annual flags (`sparse_vegetation`).
#'
#' @param ts Output of [ak_peak_ndvi()].
#' @param years Years for the mean (default: all).
#' @param trend_years Years for the trend (default: all).
#' @return One row per site, with the provenance of `ts`: add it to the
#'   catchments with [catchment_join()].
#' @examples
#' \dontrun{
#' g <- ak_peak_ndvi(x, years = 2000:2024)
#' x <- catchment_join(x, peak_ndvi_summary(g, trend_years = 2000:2024))
#' }
#' @seealso `vignette("greenness-landsat")` (download, use, uncertainty, validation)
#' @export
peak_ndvi_summary <- function(ts, years = NULL, trend_years = NULL) {
  z <- stats::qnorm(0.975)
  out <- ts |>
    dplyr::filter(!is.na(.data$ndvi_max)) |>
    dplyr::group_by(.data$site_code) |>
    dplyr::group_modify(\(d, k) {
      m <- if (is.null(years)) d else d[d$year %in% years, ]
      tr <- if (is.null(trend_years)) d else d[d$year %in% trend_years, ]
      n <- nrow(m)
      mean_se <- sqrt((if (n > 1) stats::var(m$ndvi_max) / n else NA_real_) + mean(m$ndvi_max_cal_se)^2)
      slope <- slope_se <- NA_real_
      if (nrow(tr) >= 5) {
        fit <- stats::lm(ndvi_max ~ I(year / 10), tr)
        r <- r1_detrended(stats::residuals(fit), tr$year)
        infl <- sqrt((1 + r) / (1 - r))
        slope <- unname(stats::coef(fit)[2])
        # A calibration offset left in a sensor's values is a step wherever that sensor's share of the
        # observations changes: its effect on the slope is offset x (slope of the share on time)
        step <- \(share, sd) if (is.null(share) || all(is.na(sd)) || stats::var(share) == 0) 0 else
          (mean(sd, na.rm = TRUE) * unname(stats::coef(stats::lm(share ~ I(tr$year / 10)))[2]))^2
        slope_se <- sqrt((unname(sqrt(diag(stats::vcov(fit)))[2]) * infl)^2 +
                           step(tr$share_L5, tr$cal_sd_L5) + step(tr$share_L8, tr$cal_sd_L8))
      }
      uncal <- any(grepl("uncalibrated", tr$flag))
      dplyr::tibble(ndvimax_mean = mean(m$ndvi_max), ndvimax_mean_se = mean_se,
                    ndvimax_mean_lo95 = mean(m$ndvi_max) - z * mean_se, ndvimax_mean_hi95 = mean(m$ndvi_max) + z * mean_se,
                    ndvimax_mean_flag = flag_text(few_years = n < 10, sparse_vegetation = mean(m$veg_frac) < 0.5),
                    ndvimax_trend_decade = slope, ndvimax_trend_decade_se = slope_se,
                    ndvimax_trend_decade_lo95 = slope - z * slope_se, ndvimax_trend_decade_hi95 = slope + z * slope_se,
                    ndvimax_trend_decade_flag = flag_text(few_years = nrow(tr) < 10,
                                                          sensor_change = uncal && length(unique(unlist(strsplit(tr$sensors, ";")))) > 1,
                                                          sparse_vegetation = mean(tr$veg_frac) < 0.5),
                    ndvimax_n_years = n, ndvimax_veg_frac = mean(m$veg_frac))
    }) |>
    dplyr::ungroup()
  attr(out, "ak_provenance") <- ak_provenance(ts)
  attr(out, "errors") <- attr(ts, "errors")
  out
}

# ---- sampling points -----------------------------------------------------------------------------------------------

# A regular grid of points in each catchment (deterministic: the same polygon always gets the same points)
lsat_points <- function(x, km2_per_point = 0.1, n_min = 20, n_max = 150) {
  purrr::map(seq_len(nrow(x)), \(i) {
    g <- sf::st_geometry(x)[i]
    n <- min(n_max, max(n_min, ceiling(x$area_m2[i] / 1e6 / km2_per_point)))
    s <- sqrt(x$area_m2[i] / n)
    p <- sf::st_make_grid(g, cellsize = s, what = "centers")
    p <- p[lengths(sf::st_intersects(p, g)) > 0]
    if (length(p) < n_min) {   # thin or small polygons: a finer grid
      s <- s * sqrt(length(p) / n_min) * 0.9
      if (is.finite(s) && s > 0) p <- sf::st_make_grid(g, cellsize = s, what = "centers")
      p <- p[lengths(sf::st_intersects(p, g)) > 0]
    }
    if (!length(p)) p <- sf::st_point_on_surface(g)
    if (length(p) > n_max) p <- p[round(seq(1, length(p), length.out = n_max))]
    sf::st_sf(site_code = x$site_code[i], point_id = sprintf("%s_p%03d", x$site_code[i], seq_along(p)), geometry = p)
  }) |>
    dplyr::bind_rows()
}

# ---- reading Landsat at the points ---------------------------------------------------------------------------------

# Readings for every point, from each catchment's cache plus the scenes not read yet
# Read every summer scene of `years` at the points of a group of nearby catchments (which share scenes), adding
# to each catchment's cache file after each year
lsat_group <- function(job, years, doy, max_cloud, tiers) {
  sites <- job$sites
  gp <- job$gp
  caches <- job$caches
  failed <- list()
  bb <- sf::st_bbox(sf::st_transform(gp, 4326))
  for (yr in years) {
    items <- tryCatch(lsat_search(bb, yr, doy, max_cloud, tiers),
                      error = \(e) { failed[[length(failed) + 1]] <<- dplyr::tibble(item = paste("search", sites[1], yr),
                                                                                    error = conditionMessage(e)); NULL })
    if (!length(items)) next
    for (it in items) {
      need <- sites[!vapply(sites, \(s) it$id %in% caches[[s]]$items, TRUE)]
      if (!length(need)) next
      v <- tryCatch(lsat_read_item(it, gp[gp$site_code %in% need, ]),
                    error = \(e) { failed[[length(failed) + 1]] <<- dplyr::tibble(item = it$id, error = conditionMessage(e)); NULL })
      if (is.null(v)) next
      for (s in need) {
        caches[[s]]$obs <- dplyr::bind_rows(caches[[s]]$obs, v[v$site_code == s, ])
        caches[[s]]$items <- c(caches[[s]]$items, it$id)
      }
    }
    for (i in seq_along(sites)) lsat_save(caches[[sites[i]]], job$files[i])   # after each year: an interruption loses little
  }
  list(caches = caches, failed = dplyr::bind_rows(failed))
}

lsat_observations <- function(x, pts, years, doy, max_cloud, tiers, refresh = FALSE) {
  dir <- ak_cache_dir(cache_paths$landsat)
  key <- purrr::map_chr(split(pts, pts$site_code)[x$site_code], \(p) rlang::hash(list(round(sf::st_coordinates(p), 1), tiers)))
  files <- file.path(dir, paste0(x$site_code, "_", substr(key, 1, 12), ".rds"))
  caches <- purrr::map(files, \(f) if (file.exists(f) && !refresh) readRDS(f) else list(obs = lsat_empty(), items = character()))
  names(caches) <- x$site_code
  ctr <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(x)))
  group <- paste(floor(ctr[, 1] / 50000), floor(ctr[, 2] / 50000))      # catchments within ~50 km share scenes
  jobs <- lapply(unique(group), \(gr) {
    sites <- x$site_code[group == gr]
    list(sites = sites, gp = pts[pts$site_code %in% sites, ], files = files[group == gr], caches = caches[sites])
  })
  names(jobs) <- vapply(jobs, \(j) if (length(j$sites) == 1) j$sites else paste0(j$sites[1], "+", length(j$sites) - 1), "")
  res <- site_map(jobs, lsat_group, years = years, doy = doy, max_cloud = max_cloud, tiers = tiers, label = "Landsat scenes")
  for (r in site_values(res)) caches[names(r$caches)] <- r$caches
  failed <- c(purrr::map(site_values(res), "failed"),
              list(dplyr::tibble(item = paste("group", site_errors(res, "landsat")$site_code), error = site_errors(res, "landsat")$error)))
  obs <- dplyr::bind_rows(purrr::map(caches, "obs")) |>
    dplyr::filter(.data$year %in% years, as.integer(format(.data$date, "%j")) %in% doy)
  list(obs = obs, failed = dplyr::bind_rows(failed))
}

lsat_empty <- function() {
  dplyr::tibble(site_code = character(), point_id = character(), item = character(), date = as.Date(character()),
                year = integer(), platform = character(), category = character(), cloud_cover = numeric(),
                sun_elevation = numeric(), red = numeric(), nir = numeric(), qa_pixel = integer(), qa_radsat = integer())
}

lsat_save <- function(cache, f) {
  tmp <- tempfile(fileext = ".rds")
  saveRDS(cache, tmp)
  replace_file(tmp, f)
}

# Summer scenes over a box in one year (STAC search, all pages)
lsat_search <- function(bb, year, doy, max_cloud, tiers) {
  d <- as.Date(paste0(year, "-01-01")) + range(doy) - 1
  body <- list(collections = list("landsat-c2-l2"), bbox = unname(as.numeric(bb)),
               datetime = sprintf("%sT00:00:00Z/%sT23:59:59Z", d[1], d[2]),
               query = list(platform = list(`in` = list("landsat-5", "landsat-7", "landsat-8", "landsat-9")),
                            `eo:cloud_cover` = list(lte = max_cloud)),
               limit = 250)
  feats <- list()
  req <- httr2::request("https://planetarycomputer.microsoft.com/api/stac/v1/search") |> httr2::req_body_json(body, auto_unbox = TRUE)
  repeat {
    j <- req |>
      httr2::req_user_agent("AKCatchments R package") |>
      httr2::req_retry(max_tries = 4, is_transient = \(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504)) |>
      ak_perform() |>
      httr2::resp_body_json()
    feats <- c(feats, j$features)
    nxt <- purrr::detect(j$links %||% list(), \(l) identical(l$rel, "next"))
    if (is.null(nxt)) break
    req <- httr2::request(nxt$href)
    if (identical(toupper(nxt$method %||% "GET"), "POST")) req <- httr2::req_body_json(req, utils::modifyList(body, nxt$body %||% list()),
                                                                                     auto_unbox = TRUE)
  }
  items <- purrr::map(feats, \(f) list(id = f$id, date = as.Date(substr(f$properties$datetime, 1, 10)), platform = f$properties$platform,
                              category = f$properties$`landsat:collection_category` %||% NA_character_,
                              cloud_cover = as.numeric(f$properties$`eo:cloud_cover` %||% NA),
                              sun_elevation = as.numeric(f$properties$`view:sun_elevation` %||% NA),
                              bbox = unlist(f$bbox),
                              href = purrr::map_chr(c("red", "nir08", "qa_pixel", "qa_radsat"), \(a) f$assets[[a]]$href %||% NA_character_)))
  # tiers filtered here rather than in the query: not every STAC server can query that property
  purrr::keep(items, \(it) it$category %in% tiers && !anyNA(it$href))
}

# Planetary Computer SAS token (anonymous), reused until 5 minutes before it expires
lsat_token <- local({
  tok <- NULL
  function() {
    if (is.null(tok) || Sys.time() > tok$expiry - 300) {
      j <- ak_json("https://planetarycomputer.microsoft.com/api/sas/v1/token/landsat-c2-l2")
      tok <<- list(token = j$token, expiry = as.POSIXct(sub("Z$", "", j$`msft:expiry`), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    }
    tok$token
  }
})

# Values of one scene at the points inside its footprint
lsat_read_item <- function(it, gp) {
  ll <- sf::st_coordinates(sf::st_transform(gp, 4326))
  inside <- ll[, 1] >= it$bbox[1] & ll[, 1] <= it$bbox[3] & ll[, 2] >= it$bbox[2] & ll[, 2] <= it$bbox[4]
  gp <- gp[inside, ]
  if (!nrow(gp)) return(NULL)
  terra::setGDALconfig(c("GDAL_DISABLE_READDIR_ON_OPEN=EMPTY_DIR", "GDAL_HTTP_MAX_RETRY=3", "GDAL_HTTP_RETRY_DELAY=2",
                         "CPL_VSIL_CURL_ALLOWED_EXTENSIONS=.TIF,.tif"))
  tok <- lsat_token()
  vals <- purrr::map(it$href, \(h) {
    r <- terra::rast(paste0("/vsicurl/", h, if (grepl("\\?", h)) "&" else "?", tok))
    terra::extract(r, terra::vect(sf::st_transform(gp, terra::crs(r))), ID = FALSE)[[1]]
  })
  dplyr::tibble(site_code = gp$site_code, point_id = gp$point_id, item = it$id, date = it$date,
                year = as.integer(format(it$date, "%Y")), platform = it$platform, category = it$category,
                cloud_cover = it$cloud_cover, sun_elevation = it$sun_elevation,
                red = as.numeric(vals[[1]]), nir = as.numeric(vals[[2]]), qa_pixel = as.integer(vals[[3]]),
                qa_radsat = as.integer(vals[[4]])) |>
    dplyr::filter(!is.na(.data$qa_pixel))
}

# ---- screening, calibration, peak ---------------------------------------------------------------------------------

# Clear, snow-free, water-free, unsaturated observations with valid reflectance; NDVI
lsat_clean <- function(obs, max_cloud = 80, min_sun = 30) {
  bit <- \(v, b) bitwAnd(v, bitwShiftL(1L, b)) > 0
  obs |>
    dplyr::filter(!bit(.data$qa_pixel, 0), !bit(.data$qa_pixel, 1), !bit(.data$qa_pixel, 3), !bit(.data$qa_pixel, 4),
                  !bit(.data$qa_pixel, 5), bit(.data$qa_pixel, 6), !bit(.data$qa_pixel, 7),
                  .data$qa_radsat == 0, .data$cloud_cover <= max_cloud, is.na(.data$sun_elevation) | .data$sun_elevation >= min_sun) |>
    dplyr::mutate(red = .data$red * 0.0000275 - 0.2, nir = .data$nir * 0.0000275 - 0.2) |>
    dplyr::filter(.data$red > 0.005, .data$red < 1, .data$nir > 0.005, .data$nir < 1) |>
    dplyr::mutate(ndvi = (.data$nir - .data$red) / (.data$nir + .data$red),
                  doy = as.integer(format(.data$date, "%j")),
                  sensor = dplyr::case_match(.data$platform, "landsat-5" ~ "L5", "landsat-7" ~ "L7",
                                             c("landsat-8", "landsat-9") ~ "L8"))
}

# Calibrate L5 and L8/9 NDVI to L7 (ported from LandsatTS lsat_calibrate_poly): paired 15-day medians at points
# and years both sensors observed, a polynomial chosen by AIC, 75/25 train/evaluation split by point.
lsat_xcal <- function(obs, doy = 152:243, min_obs = 5, min_points = 30, seed = 1, cal_floor = 0.002) {
  obs <- dplyr::mutate(obs, ndvi_x = .data$ndvi, cal_se = 0, calibrated = .data$sensor == "L7")
  evals <- list()
  for (s in setdiff(intersect(unique(obs$sensor), c("L5", "L8")), "L7")) {
    pairs <- lsat_xcal_pairs(obs, s, doy, min_obs)
    if (nrow(pairs) < min_points || !"L7" %in% obs$sensor) {
      evals[[s]] <- dplyr::tibble(sensor = s, n_points = nrow(pairs), degree = NA_integer_, uncal_bias = NA_real_,
                                  xcal_bias = NA_real_, xcal_rmse = NA_real_, cal_sd = NA_real_, calibrated = FALSE)
      next
    }
    pts <- unique(pairs$point_id)
    train_pts <- with_seed(seed, sample(pts, round(0.75 * length(pts))))
    tr <- pairs[pairs$point_id %in% train_pts, ]
    ev <- pairs[!pairs$point_id %in% train_pts, ]
    fits <- purrr::map(1:3, \(k) stats::lm(l7 ~ stats::poly(other, k, raw = TRUE), tr))
    best <- which.min(vapply(fits, stats::AIC, 0))
    fit <- fits[[best]]
    pe <- stats::predict(fit, ev)
    i <- obs$sensor == s
    p <- stats::predict(fit, data.frame(other = obs$ndvi[i]), se.fit = TRUE)
    bias <- stats::median(pe - ev$l7)
    sys <- max(abs(bias), cal_floor)   # bias left on the held-out points, at least the pairing's own error
    obs$ndvi_x[i] <- p$fit
    obs$cal_se[i] <- sqrt(p$se.fit^2 + sys^2)
    obs$calibrated[i] <- TRUE
    evals[[s]] <- dplyr::tibble(sensor = s, n_points = length(pts), degree = best,
                                uncal_bias = stats::median(ev$other - ev$l7), xcal_bias = bias,
                                xcal_rmse = sqrt(mean((pe - ev$l7)^2)), cal_sd = sqrt(mean(p$se.fit^2) + sys^2),
                                calibrated = TRUE)
  }
  list(obs = obs, eval = dplyr::bind_rows(evals))
}

with_seed <- function(seed, expr) {
  old <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv())
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, globalenv()))
  # explicit generator: mirai daemons default to L'Ecuyer-CMRG, which would give other draws than this session
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  expr
}

lsat_xcal_pairs <- function(obs, s, doy, min_obs) {
  o <- obs[obs$sensor %in% c("L7", s) & obs$doy %in% doy, ]
  both <- o |> dplyr::distinct(.data$point_id, .data$year, .data$sensor) |> dplyr::count(.data$point_id, .data$year) |>
    dplyr::filter(.data$n == 2)
  o <- dplyr::semi_join(o, both, by = c("point_id", "year"))
  if (!nrow(o)) return(dplyr::tibble(point_id = character(), l7 = numeric(), other = numeric()))
  # a focal day per point where both sensors have at least min_obs observations within +/- 7 days
  # Among qualifying days, the one nearest the point's seasonal peak: there NDVI changes least, so the two
  # sensors' medians are not biased by seeing different stages of green-up or senescence (LandsatTS picks one
  # at random; with fixed orbits that bias can reach a few hundredths)
  peak <- o |> dplyr::summarise(pk = peak_doy(.data$doy, .data$ndvi), .by = "point_id")
  cnt <- o |> dplyr::count(.data$point_id, .data$sensor, .data$doy)
  focal <- tidyr::expand_grid(point_id = unique(o$point_id), sensor = c("L7", s), d = doy) |>
    dplyr::left_join(cnt, by = c("point_id", "sensor"), relationship = "many-to-many") |>
    dplyr::summarise(n = sum(.data$n[abs(.data$doy - .data$d) <= 7], na.rm = TRUE), .by = c("point_id", "sensor", "d")) |>
    tidyr::pivot_wider(names_from = "sensor", values_from = "n") |>
    dplyr::filter(.data$L7 >= min_obs, .data[[s]] >= min_obs) |>
    dplyr::left_join(peak, by = "point_id") |>
    dplyr::summarise(d = .data$d[which.min(abs(.data$d - .data$pk))], .by = "point_id")
  # each observation is moved to the focal day along the point's seasonal curve for that sensor before the
  # medians, so the two sensors are compared at the same stage of the season even when they see different days
  # (each sensor's own curve: a curve of the other sensor would carry the very difference being calibrated)
  dplyr::inner_join(o, focal, by = "point_id") |>
    dplyr::filter(abs(.data$doy - .data$d) <= 7) |>
    dplyr::mutate(v = .data$ndvi + season_shift(.data$doy, .data$d[1],
                                                o[o$point_id == .data$point_id[1] & o$sensor == .data$sensor[1], ]),
                  .by = c("point_id", "sensor")) |>
    dplyr::summarise(v = stats::median(.data$v), .by = c("point_id", "sensor")) |>
    tidyr::pivot_wider(names_from = "sensor", values_from = "v") |>
    dplyr::transmute(.data$point_id, l7 = .data$L7, other = .data[[s]]) |>
    dplyr::filter(!is.na(.data$l7), !is.na(.data$other)) |>
    dplyr::mutate(dif = .data$other - .data$l7) |>
    dplyr::filter(dplyr::percent_rank(.data$dif) > 0.025, dplyr::percent_rank(.data$dif) < 0.975) |>
    dplyr::select(-"dif")
}

# A point's summer NDVI curve for the calibration pairing: a quadratic in day of year (few parameters, so it
# holds up with few distinct acquisition days, unlike a smoothing spline). NULL if it can't be fitted.
season_quad <- function(doy, v) {
  if (length(unique(doy)) < 4) return(NULL)
  stats::lm(v ~ doy + I(doy^2))
}

# Change along the curve fitted to `ref` (a point's observations from one sensor) from `doy` to `to`; 0 if no curve
season_shift <- function(doy, to, ref) {
  near <- ref[abs(ref$doy - to) <= 30, ]        # a quadratic fits the season best near the focal day
  if (length(unique(near$doy)) >= 5) ref <- near
  fit <- season_quad(ref$doy, ref$ndvi)
  if (is.null(fit)) return(rep(0, length(doy)))
  stats::predict(fit, data.frame(doy = to)) - stats::predict(fit, data.frame(doy = doy))
}

# Day of the seasonal peak (vertex of the quadratic if it is a maximum inside the observed days, else the day of
# the highest fitted value)
peak_doy <- function(doy, v) {
  fit <- season_quad(doy, v)
  if (is.null(fit)) return(stats::median(doy))
  g <- min(doy):max(doy)
  g[which.max(stats::predict(fit, data.frame(doy = g)))]
}

# Annual NDVImax per point (ported from LandsatTS lsat_fit_phenological_curves + lsat_summarize_growing_seasons)
lsat_peaks <- function(obs, window_yrs = 7, window_min_obs = 20, si_min = 0.15, spar = 0.78, pct_thresh = 30,
                       min_frac = 0.75) {
  o <- obs[obs$ndvi_x >= si_min, c("site_code", "point_id", "year", "doy", "ndvi_x", "cal_se", "calibrated", "sensor")]
  if (!nrow(o)) return(lsat_peaks_empty())
  purrr::map(split(o, o$point_id), \(p) lsat_point_peaks(p, window_yrs, window_min_obs, spar, pct_thresh, min_frac)) |>
    dplyr::bind_rows()
}

lsat_peaks_empty <- function() {
  dplyr::tibble(site_code = character(), point_id = character(), year = integer(), ndvi_max = numeric(), se = numeric(),
                cal_se = numeric(), n_obs = integer(), uncalibrated = logical(), edge = logical(), sensors = character(),
                share_L5 = numeric(), share_L8 = numeric())
}

spline_fit <- function(doy, y, w, spar) {
  if (length(unique(doy)) < 4) return(NULL)
  stats::smooth.spline(doy, y, w = w, spar = spar)
}

lsat_point_peaks <- function(p, window_yrs, window_min_obs, spar, pct_thresh, min_frac) {
  if (nrow(p) < window_min_obs) return(NULL)
  rough <- spline_fit(p$doy, p$ndvi_x, rep(1, nrow(p)), spar)
  if (is.null(rough)) return(NULL)
  f <- stats::predict(rough, p$doy)$y
  p <- p[abs((p$ndvi_x - f) / ((p$ndvi_x + f) / 2) * 100) < 100, ]
  yrs <- min(p$year):max(p$year)
  half <- round(window_yrs / 2)
  purrr::map(intersect(yrs, unique(p$year)), \(yr) {
    win <- if (yr - half < min(yrs)) min(yrs):(min(yrs) + window_yrs - 1)
           else if (yr + half > max(yrs)) (max(yrs) - window_yrs + 1):max(yrs) else (yr - half):(yr + half)
    d <- p[p$year %in% win, ]
    for (k in 1:6) {                      # refit without outliers until none are left (at most 5 removals)
      if (nrow(d) < window_min_obs) return(NULL)
      fit <- spline_fit(d$doy, d$ndvi_x, exp(-0.25 * abs(d$year - yr)), spar)
      if (is.null(fit)) return(NULL)
      s <- stats::predict(fit, d$doy)$y
      bad <- abs((d$ndvi_x - s) / ((d$ndvi_x + s) / 2) * 100) > pct_thresh
      if (!any(bad) || k == 6) break
      d <- d[!bad, ]
    }
    grid <- min(d$doy):max(d$doy)
    curve <- stats::predict(fit, grid)$y
    peak <- max(curve)
    resid_sd <- stats::sd(d$ndvi_x - s)
    fy <- d[d$year == yr, ]
    if (!nrow(fy)) return(NULL)
    sy <- stats::predict(fit, fy$doy)$y
    keep <- sy / peak >= min_frac
    if (!any(keep)) return(NULL)
    adj <- fy$ndvi_x[keep] + (peak - sy[keep])
    v <- max(stats::median(adj), stats::quantile(fy$ndvi_x[keep], 0.9, names = FALSE))
    dplyr::tibble(site_code = p$site_code[1], point_id = p$point_id[1], year = yr, ndvi_max = v,
                  se = resid_sd / sqrt(sum(keep)), cal_se = mean(fy$cal_se[keep]), n_obs = sum(keep),
                  uncalibrated = any(!fy$calibrated[keep]), edge = yr - min(yrs) < half | max(yrs) - yr < half,
                  sensors = paste(sort(unique(fy$sensor[keep])), collapse = ";"),
                  share_L5 = mean(fy$sensor[keep] == "L5"), share_L8 = mean(fy$sensor[keep] == "L8"))
  }) |>
    dplyr::bind_rows()
}

# Catchment-year values from the point peaks
lsat_catchment_years <- function(pp, pts, years) {
  npts <- dplyr::count(sf::st_drop_geometry(pts), .data$site_code, name = "n_points")
  veg <- pp |> dplyr::distinct(.data$site_code, .data$point_id) |> dplyr::count(.data$site_code, name = "n_veg")
  z <- stats::qnorm(0.975)
  yr <- pp |>
    dplyr::summarise(n_points_valid = dplyr::n(), ndvi_max = mean(.data$ndvi_max),
                     se_sampling = if (dplyr::n() > 1) stats::sd(.data$ndvi_max) / sqrt(dplyr::n()) else NA_real_,
                     se_peak = sqrt(mean(.data$se^2, na.rm = TRUE)), ndvi_max_cal_se = mean(.data$cal_se),
                     obs_per_point = stats::median(.data$n_obs), uncalibrated = any(.data$uncalibrated),
                     edge = mean(.data$edge) > 0.5,
                     sensors = paste(sort(unique(unlist(strsplit(.data$sensors, ";")))), collapse = ";"),
                     share_L5 = mean(.data$share_L5), share_L8 = mean(.data$share_L8),
                     .by = c("site_code", "year"))
  tidyr::expand_grid(site_code = npts$site_code, year = years) |>
    dplyr::inner_join(yr, by = c("site_code", "year")) |>
    dplyr::left_join(npts, by = "site_code") |>
    dplyr::left_join(veg, by = "site_code") |>
    dplyr::mutate(veg_frac = .data$n_veg / .data$n_points,
                  ndvi_max_se = sqrt(dplyr::coalesce(.data$se_sampling, 0)^2 + dplyr::coalesce(.data$se_peak, 0)^2 +
                                       .data$ndvi_max_cal_se^2),
                  ndvi_max_lo95 = pmax(-1, .data$ndvi_max - z * .data$ndvi_max_se),
                  ndvi_max_hi95 = pmin(1, .data$ndvi_max + z * .data$ndvi_max_se),
                  flag = flag_text(few_points = .data$n_points_valid < 10,
                                   low_coverage = .data$n_points_valid < 0.5 * .data$n_veg,
                                   sparse_vegetation = .data$veg_frac < 0.5, few_obs = .data$obs_per_point < 3,
                                   record_edge = .data$edge, uncalibrated = .data$uncalibrated)) |>
    dplyr::select("site_code", "year", "ndvi_max", "ndvi_max_se", "ndvi_max_lo95", "ndvi_max_hi95", "ndvi_max_cal_se",
                  "n_points", "n_points_valid", "veg_frac", "obs_per_point", "sensors", "share_L5", "share_L8", "flag")
}

