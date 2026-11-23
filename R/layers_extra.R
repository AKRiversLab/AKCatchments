#' Climate normals of each catchment
#'
#' Monthly and annual normals of mean, minimum and maximum temperature and
#' precipitation, averaged over each catchment and appended to it, from one
#' of two sources:
#' * `source = "prism8110"` (default): PRISM 1981-2010 normals for Alaska,
#'   800 m (columns `prism_<var>_<period>`, `prism_flag`). The normals behind
#'   the NPS basin descriptors.
#' * `source = "daymet4"`: normals computed from the daily Daymet weather of
#'   [ak_weather()] over the `baseline` years (default 1991-2020, the
#'   current WMO normal period), so they match the anomalies of
#'   [weather_at_samples()] (columns `daymet_<var>_<period>`, with `_se` for
#'   `tmean` and `ppt`, and `daymet_normals_flag`).
#'
#' Units: degrees C, and millimetres of precipitation per month (monthly) or
#' per year (annual). The two sources differ in period, resolution and
#' method (PRISM uses station data and terrain regressions; Daymet
#' interpolates stations), so do not mix their columns in one analysis
#' without checking them against each other on your sites.
#'
#' @section PRISM (`prism8110`):
#' PRISM 1981-2010 climate normals for Alaska.
#'
#' Catchment means of the PRISM 30-arc-second (~800 m) monthly and annual
#' normals for Alaska (PRISM Climate Group and NPS Alaska Inventory and
#' Monitoring, 2018), downloaded once per variable and period from
#' prism.oregonstate.edu. Units: millimetres (`ppt`) and degrees C. These are
#' the normals behind the NPS basin descriptors' monthly climate columns.
#'
#' Uncertainty: PRISM publishes none for these grids. `prism_flag` marks
#' internal inconsistencies, the known way bad values show up: `tmean_not_mid`
#' (|tmean - (tmax + tmin) / 2| > 0.1 C in a month), `annual_mismatch` (annual
#' temperature not the mean of the months, or annual precipitation not their
#' sum, beyond 0.1 C or 1%) and `out_of_order` (tmax < tmean or tmean < tmin).
#' Checks run only on the variables and periods requested.
#'
#' Terms: free to use with attribution (PRISM Climate Group, Oregon State
#' University, URL, date); confirm redistribution terms for derived tables.
#'
#'
#' @section Daymet (`daymet4`):
#' Each month's normal is the mean over the baseline years of that month's
#' mean (temperature) or total (precipitation) of the catchment daily
#' series; annual normals likewise over whole years. Daymet years have 365
#' days (31 December is dropped in leap years), so December totals are
#' short by one day in leap years.
#'
#' Uncertainty: `_se` is the point-sampling error of the catchment mean
#' (from the `tmean_se_sampling` and `prcp_se_sampling` of [ak_weather()]).
#' A pixel sample that runs warm or wet does so on every day, so this error
#' does not shrink by averaging days: the temperature SE is the mean of the
#' daily SEs and the precipitation SE of a total is the sum of the daily SEs
#' (the upper bound, for errors fully correlated in time). Daymet's own
#' interpolation error, larger in remote and mountainous Alaska, is not
#' included (see `vignette("weather-daymet")`). `daymet_normals_flag`:
#' `incomplete_baseline` where a site lacks more than 5% of the baseline
#' days, `few_pixels` where fewer than 3 pixels make the mean.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param source `"prism8110"` or `"daymet4"`.
#' @param vars Any of `"tmean"`, `"tmin"`, `"tmax"`, `"ppt"`.
#' @param periods Month numbers `"01"`-`"12"` and/or `"annual"`.
#' @param weather For `daymet4`: daily weather from [ak_weather()] covering
#'   the baseline years.
#' @param baseline For `daymet4`: the normal period.
#' @return `catchments` with the normals added.
#' @examples
#' \dontrun{
#' x <- ak_climate_normals(x)                                    # PRISM 1981-2010
#' x <- ak_climate_normals(x, "daymet4", weather = w)            # Daymet 1991-2020
#' }
#' @seealso `vignette("climate-prism")`, `vignette("weather-daymet")`
#' @export
ak_climate_normals <- function(catchments, source = c("prism8110", "daymet4"), vars = c("tmean", "tmin", "tmax", "ppt"),
                               periods = c(sprintf("%02d", 1:12), "annual"), weather = NULL, baseline = 1991:2020) {
  source <- match.arg(source)
  vars <- match.arg(vars, several.ok = TRUE)
  x <- as_catchments(catchments)
  if (source == "prism8110") {
    return(add_columns(catchments, climate_prism(x, vars, periods), layer_row("climate_normals", source, fn = "ak_climate_normals"),
                       list(source = source, vars = vars, periods = periods)))
  }
  if (is.null(weather)) stop("source = \"daymet4\" needs the daily weather: weather = ak_weather(x, years = ",
                             min(baseline), ":", max(baseline), ")", call. = FALSE)
  out <- daymet_normals(weather[weather$site_code %in% x$site_code, ], vars, periods, baseline)
  add_columns(catchments, out, layer_row("climate_normals", source, fn = "ak_climate_normals"),
              list(source = source, vars = vars, periods = periods, baseline = range(baseline)),
              note = paste("from", paste(unique(ak_provenance(weather)$fn), collapse = ", "), "output"))
}

# Monthly and annual normals over the baseline from the catchment daily Daymet series
daymet_normals <- function(w, vars, periods, baseline) {
  w <- w[w$year %in% baseline, ]
  if (!nrow(w)) stop("`weather` has no days in the baseline years ", min(baseline), "-", max(baseline), call. = FALSE)
  w$month <- sprintf("%02d", as.integer(format(w$date, "%m")))
  w$ppt <- w$prcp
  agg <- \(d) d |>
    dplyr::summarise(tmean = mean(.data$tmean), tmin = mean(.data$tmin), tmax = mean(.data$tmax), ppt = sum(.data$ppt),
                     tmean_se = mean(.data$tmean_se_sampling), ppt_se = sum(.data$prcp_se_sampling),
                     .by = c("site_code", "year", "period")) |>
    dplyr::summarise(dplyr::across(c("tmean", "tmin", "tmax", "ppt", "tmean_se", "ppt_se"), \(v) mean(v, na.rm = TRUE)),
                     .by = c("site_code", "period"))
  long <- dplyr::bind_rows(if (any(periods != "annual")) agg(dplyr::mutate(w, period = .data$month)),
                           if ("annual" %in% periods) agg(dplyr::mutate(w, period = "annual"))) |>
    dplyr::filter(.data$period %in% periods)
  keep <- c(vars, paste0(intersect(vars, c("tmean", "ppt")), "_se"))
  wide <- long |>
    tidyr::pivot_longer(dplyr::all_of(keep), names_to = "var") |>
    dplyr::mutate(name = ifelse(grepl("_se$", .data$var), paste0("daymet_", sub("_se$", "", .data$var), "_", .data$period, "_se"),
                                paste0("daymet_", .data$var, "_", .data$period))) |>
    tidyr::pivot_wider(id_cols = "site_code", names_from = "name", values_from = "value")
  cover <- w |>
    dplyr::summarise(days = dplyr::n(), few = any(.data$n_pixels < 3), .by = "site_code")
  dplyr::left_join(wide, cover, by = "site_code") |>
    dplyr::mutate(daymet_normals_flag = flag_text(incomplete_baseline = .data$days < 0.95 * 365 * length(baseline), few_pixels = .data$few)) |>
    dplyr::select(-"days", -"few")
}

climate_prism <- function(x, vars = c("tmean", "tmin", "tmax", "ppt"), periods = c(sprintf("%02d", 1:12), "annual")) {
  out <- tidyr::expand_grid(var = vars, period = periods) |>
    purrr::pmap(\(var, period) {
      r <- prism_raster(var, period)
      g <- sf::st_transform(x, terra::crs(r))
      if (terra::xmax(r) > 180) g <- sf::st_shift_longitude(g)  # the Alaska grids run 172 to 237 E, across the date line
      dplyr::tibble(site_code = x$site_code, name = paste0("prism_", var, "_", period),
                    value = exactextractr::exact_extract(r, g, "mean", progress = FALSE))
    }) |>
    purrr::list_rbind() |>
    tidyr::pivot_wider()
  dplyr::mutate(out, prism_flag = prism_consistency(out))
}

# One PRISM Alaska normal grid, downloaded once
prism_raster <- function(var, period) {
  f <- sprintf("PRISM_%s_ak_30yr_normal_800mM1_%s_asc.zip", var, period)
  zip <- ak_download(paste("https://prism.oregonstate.edu/normals_other/public/alaska/grids", var, f, sep = "/"),
                     file.path(ak_cache_dir(cache_paths$prism), f))
  inside <- utils::unzip(zip, list = TRUE)$Name
  # The Alaska normals are ARC ASCII grids named .txt inside an "_asc.zip"; take any grid format
  grid <- c(grep("\\.(asc|tif|bil)$", inside, value = TRUE), grep("\\.txt$", inside, value = TRUE))[1]
  if (is.na(grid)) stop("No grid file in ", zip, "; it contains: ", paste(inside, collapse = ", "), call. = FALSE)
  r <- terra::rast(vsizip(zip, grid))
  if (!nzchar(terra::crs(r))) terra::crs(r) <- "EPSG:4269"
  r
}

prism_consistency <- function(d) {
  col <- \(v, p) d[[paste0("prism_", v, "_", p)]]
  has <- \(v, p) paste0("prism_", v, "_", p) %in% names(d)
  months <- sprintf("%02d", 1:12)
  mid <- order <- annual <- rep(FALSE, nrow(d))
  for (p in c(months, "annual")) {
    if (has("tmean", p) && has("tmin", p) && has("tmax", p)) {
      mid <- mid | abs(col("tmean", p) - (col("tmax", p) + col("tmin", p)) / 2) > 0.1
      order <- order | col("tmax", p) < col("tmean", p) - 0.05 | col("tmean", p) < col("tmin", p) - 0.05
    }
  }
  for (v in c("tmean", "tmin", "tmax", "ppt")) {
    if (!all(has(v, c(months, "annual")))) next
    m <- as.matrix(d[paste0("prism_", v, "_", months)])
    a <- col(v, "annual")
    annual <- annual | if (v == "ppt") abs(rowSums(m) - a) > 0.01 * a else abs(rowMeans(m) - a) > 0.1
  }
  flag_text(tmean_not_mid = mid, annual_mismatch = annual, out_of_order = order)
}

permafrost_pastick <- function(x) {
  pf <- pastick_raster()
  summ <- function(values, coverage_fraction) {
    values[values > 105] <- NA
    w <- coverage_fraction
    mapped <- !is.na(values)
    veg <- mapped & values <= 100
    land <- mapped & !values %in% c(101, 102)
    data.frame(pf_prob_mean = sum(values[veg] * w[veg]) / sum(w[veg]),
               pf_land_lo = sum(values[veg] * w[veg]) / sum(w[land]),
               pf_land_hi = (sum(values[veg] * w[veg]) + 100 * sum(w[land & !veg])) / sum(w[land]),
               pf_extent_pct = 100 * sum(w[veg & values >= 50]) / sum(w[veg]),
               pf_masked_pct = 100 * sum(w[mapped & !veg]) / sum(w[mapped]),
               pf_icesnow_pct = 100 * sum(w[values %in% 102]) / sum(w[mapped]),
               pf_barren_pct = 100 * sum(w[values %in% 104]) / sum(w[mapped]),
               mapped_cells = sum(w[mapped]))
  }
  xr <- sf::st_transform(x, terra::crs(pf))
  exactextractr::exact_extract(pf, xr, fun = summ, append_cols = "site_code", progress = extract_progress(x)) |>
    dplyr::mutate(pf_coverage_pct = 100 * .data$mapped_cells * prod(terra::res(pf)) / as.numeric(sf::st_area(xr)),
                  dplyr::across(dplyr::where(is.numeric), \(v) dplyr::if_else(is.nan(v), NA_real_, v)),
                  pf_flag = flag_text(partial_coverage = .data$pf_coverage_pct < 90, mostly_masked = .data$pf_masked_pct > 25)) |>
    dplyr::select(-"mapped_cells")
}


# The Pastick et al. 2015 probability raster, downloaded and unpacked once
pastick_raster <- function() {
  dir <- ak_cache_dir(cache_paths$pastick)
  find <- \() list.files(dir, "\\.(tif|img|bil|dat)$|^hdr\\.adf$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  nested <- \() list.files(dir, "\\.(tar|tar\\.gz|tgz|zip|7z)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (!length(find()) && !length(nested())) extract_archive(sb_download("5602ab5ae4b03bc34f5448b4", "AK_PFProbability_1m.7z"), dir, extract_7z)
  # The .7z holds a .tar: unpack it beside itself, then drop it (several GB)
  for (a in nested()) if (!length(find())) {
    extract_archive(a, sub("\\.(tar|tar\\.gz|tgz|zip|7z)$", "", a, ignore.case = TRUE), extract_7z)
    unlink(a)
  }
  f <- find()
  if (!length(f)) stop("No raster found in ", dir, "; it holds: ",
                       paste(utils::head(list.files(dir, recursive = TRUE), 20), collapse = ", "), call. = FALSE)
  terra::rast(if (basename(f[1]) == "hdr.adf") dirname(f[1]) else f[1])
}

extract_7z <- function(arc, exdir) {
  if (requireNamespace("archive", quietly = TRUE)) return(invisible(archive::archive_extract(arc, exdir)))
  z <- Sys.which(c("7z", "7za", "7zr"))
  z <- z[nzchar(z)][1]
  if (is.na(z)) stop("Install the archive package (or 7-Zip) to unpack ", arc)
  status <- system2(z, c("x", shQuote(arc), paste0("-o", shQuote(exdir)), "-y"), stdout = FALSE)
  if (!identical(as.integer(status), 0L)) stop("7-Zip failed (status ", status, ") unpacking ", arc)
  invisible(exdir)
}

#' Glacier cover from the Randolph Glacier Inventory
#'
#' Percent of each catchment covered by glacier outlines from an RGI regional
#' file. The RGI 7.0 glacier product (NSIDC-0770, doi:10.5067/F6JMOVY5NAVZ;
#' region 01 Alaska, outlines for about 2000) is distributed by NSIDC behind
#' an Earthdata login, so download it by hand and unzip it into the cache's
#' `rgi` folder (or pass `path`). Earlier versions (RGI 6.0) work the same way.
#'
#' Uncertainty: outline placement is shifted at random with SD `sigma_m`
#' (default 30 m, about one Landsat pixel) to give `glacier_pct_se`, `_lo95`
#' and `_hi95` (shares of the whole catchment). Outline area error is larger
#' for small glaciers, and the outlines date from about 2000, so later
#' retreat is not included. `glacier_flag`: `small_glaciers` where glaciers
#' under 1 km2 make up more than half the glacier area.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param path RGI shapefile, or a folder containing it.
#' @param sigma_m Positional SD in metres; 0 skips the simulation.
#' @param n,seed Draws and random seed.
#' @return `catchments` with `glacier_pct`, `glacier_pct_se`,
#'   `glacier_pct_lo95`, `glacier_pct_hi95` and `glacier_flag` added.
#' @seealso `vignette("glaciers-rgi")` (download, use, uncertainty, validation)
#' @export
ak_glaciers <- function(catchments, path = ak_cache_dir(cache_paths$glaciers), sigma_m = 30, n = 500, seed = 1) {
  x <- as_catchments(catchments)
  shp <- if (dir.exists(path)) list.files(path, "\\.shp$", recursive = TRUE, full.names = TRUE)[1] else path
  if (is.na(shp)) stop("No RGI shapefile in ", path, ": download the RGI 7.0 region 01 glacier product from NSIDC (NSIDC-0770)")
  g <- sf::st_read(shp, wkt_filter = bbox_wkt(sf::st_buffer(sf::st_union(sf::st_geometry(x)), 4 * sigma_m + 1), shp), quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(akalbers_crs()) |>
    sf::st_filter(sf::st_buffer(x, 4 * sigma_m + 1))
  g$class <- rep("glacier", nrow(g))
  g$small <- as.numeric(sf::st_area(g)) < 1e6
  p <- suppressWarnings(sf::st_intersection(x[c("site_code", "area_m2")], g[c("class", "small")]))
  p$a <- as.numeric(sf::st_area(p))
  base <- dplyr::tibble(site_code = x$site_code, area_m2 = x$area_m2) |>
    dplyr::left_join(sf::st_drop_geometry(p) |> dplyr::summarise(a_small = sum(.data$a[.data$small]), a = sum(.data$a), .by = "site_code"),
                     by = "site_code") |>
    dplyr::mutate(glacier_pct = 100 * dplyr::coalesce(.data$a, 0) / .data$area_m2,
                  glacier_flag = flag_text(small_glaciers = .data$a_small > 0.5 * .data$a))
  if (sigma_m > 0) {
    pu <- dplyr::tibble(site_code = character(), glacier_pct_se = numeric(), glacier_pct_lo95 = numeric(), glacier_pct_hi95 = numeric())
    if (nrow(g)) {
      pu <- positional_uncertainty(x, g, "class", sigma_m, n, seed = seed, of = "catchment") |>
        dplyr::filter(.data$class == "glacier") |>
        dplyr::select("site_code", glacier_pct_se = "pct_sd", glacier_pct_lo95 = "pct_lo95", glacier_pct_hi95 = "pct_hi95")
    }
    base <- dplyr::left_join(base, pu, by = "site_code") |>
      dplyr::mutate(dplyr::across(c("glacier_pct_se", "glacier_pct_lo95", "glacier_pct_hi95"), \(v) dplyr::coalesce(v, 0)))
  }
  add_columns(catchments, dplyr::select(base, "site_code", "glacier_pct", dplyr::starts_with("glacier_pct_"), "glacier_flag"),
              layer_row("glaciers", fn = "ak_glaciers"), list(sigma_m = sigma_m, n = n, seed = seed))
}

#' USGS streamgage basin boundaries for Alaska (validation reference)
#'
#' Downloads once and reads the selected drainage-basin boundaries for USGS
#' streamgages in Alaska (Curran 2024): the release through 2019 (253 gages
#' with at least 5 years of daily flow, doi:10.5066/P13TMJUP) and the release
#' through 2014 (471 gages with at least 10 years of peak flow, or in the Cook
#' Inlet Basin, doi:10.5066/P143KNEJ). The 2019 release repeats the 2014
#' boundaries of its gages unchanged, so a gage in both is returned once, from
#' 2019. Boundaries were digitised from each gage to the nearest Watershed
#' Boundary Dataset (WBD) boundary and then follow WBD boundaries to the
#' headwaters; drainage areas published in NWIS were updated from them. They
#' are a reference for checking delineations: delineate at the gage locations
#' and compare with [compare_catchments()].
#'
#' A few gages also have boundaries for an alternate (regulated or
#' unregulated) condition, marked in `SpecConds`; most uses want the rows
#' where it is empty.
#'
#' @param release Which releases to read: `"2019"`, `"2014"` or both.
#' @return sf polygons in EPSG:3338 with the published attributes plus
#'   `usgs_site_no` (the gage number as published), `site_code`
#'   (`"USGS-"` and the number, as in the USGS Water Data API) and `release`.
#' @seealso `vignette("delineation")` (download, use, uncertainty, validation)
#' @export
ak_gaged_basins <- function(release = c("2019", "2014")) {
  release <- match.arg(release, several.ok = TRUE)
  # Each shapefile is attached to its ScienceBase item as separate files, not a zip
  read_release <- function(item, stem, year) {
    f <- paste0(stem, ".", c("shp", "shx", "dbf", "prj"))
    shp <- purrr::map_chr(f, \(x) sb_download(item, x, file.path(ak_cache_dir("gaged_basins"), x)))[1]
    x <- sf::st_read(shp, quiet = TRUE) |>
      sf::st_make_valid() |>
      sf::st_transform(akalbers_crs())
    no <- grep("^site_?n(o|um)|^sitenumber$|^staid$", names(x), ignore.case = TRUE, value = TRUE)[1]
    if (is.na(no)) stop(stem, " has no gage-number field; its fields: ", paste(names(x), collapse = ", "), call. = FALSE)
    x$usgs_site_no <- as.character(x[[no]])
    x$site_code <- paste0("USGS-", x$usgs_site_no)
    x$release <- year
    x
  }
  b2019 <- if ("2019" %in% release) read_release("66440948d34e1955f5a427ff", "USGS_AK_GageBasinDA_SFR2020", "2019")
  b2014 <- if ("2014" %in% release) read_release("66440867d34e1955f5a427fa", "USGS_AK_GageBasinDA_FF2016", "2014")
  if (!is.null(b2019) && !is.null(b2014)) b2014 <- b2014[!b2014$usgs_site_no %in% b2019$usgs_site_no, ]
  dplyr::bind_rows(b2019, b2014)
}
