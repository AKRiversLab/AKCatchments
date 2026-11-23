#' Projected climate change per catchment and model (downscaled CMIP6)
#'
#' Projected change in monthly and seasonal temperature and precipitation for
#' each catchment, model and emissions scenario. `source = "snap_cmip6_4km"`
#' (the only source so far) is the Scenarios Network
#' for Alaska + Arctic Planning (SNAP, University of Alaska Fairbanks) daily
#' CMIP6 downscaling: 4 km, 1965-2100, bias-adjusted by quantile delta mapping
#' (QDM, xclim) against WRF-downscaled ERA5, for up to 13 models and four SSP
#' scenarios ([ak_climate_projection_models()]).
#'
#' **Deltas, not absolute values.** The downscaling reference is ERA5-WRF,
#' not Daymet or PRISM, so the projections' absolute climate differs from the
#' Daymet normals and anomalies the rest of the package uses. Each model's
#' change is therefore computed against the same model's own baseline (bias
#' and reference differences cancel), and you apply it to your observed
#' baseline: `T_future = T_daymet + delta_c` and
#' `P_future = P_daymet * ratio`. QDM preserves each model's relative change
#' by quantile, so these deltas carry the global model's change signal with
#' the 4 km reference's spatial detail. `baseline_mean` and `future_mean` are
#' returned for checks, not for use as climate.
#'
#' **Baseline.** CMIP6 historical runs end in 2014. A baseline running past
#' 2014 (the default 1991-2020, matching [weather_at_samples()]) uses the
#' historical run through 2014 and the scenario run from 2015, the usual
#' convention; scenarios barely differ before 2020.
#'
#' **Seasons** ([ak_seasons()]): temperature is the day-weighted mean of the
#' monthly means; precipitation the total (mean daily rate x days in each
#' calendar month, so 360-day and no-leap model calendars don't shorten
#' months). A season listed across the year end (`c(12, 1, 2)`) counts the
#' December before the January. Only complete season-years are used.
#'
#' **Catchment values.** The area-weighted mean of the 4 km cells under the
#' catchment (exact cell coverage fractions); cells with no data (ocean,
#' outside the grid) are left out and reported in `coverage_pct`. A catchment
#' smaller than a cell (16 km^2) gets the mean of the one to four cells it
#' touches: that is the 4 km cell's change, not the catchment's (`subgrid`
#' flag). Changes vary smoothly in space, far more than absolute climate
#' does, so `delta_local_sd` (the SD of the per-cell change over the
#' catchment's cells and their neighbours) measures how much this matters.
#'
#' **Uncertainty per model** (`delta_se`, `log_ratio_se`): the sampling error
#' of the difference of two period means from interannual variability
#' (internal variability: a model run is one realisation), with the number of
#' years discounted for lag-1 autocorrelation of the detrended series,
#' `n_eff = n (1 - r1) / (1 + r1)`. Precipitation ratios are on the log
#' scale. Between-model (structural) uncertainty is in [projection_ensemble()];
#' scenario uncertainty is never averaged: scenarios stay separate. Not
#' included: downscaling and bias-adjustment error (QDM's reference is a
#' model too) and the one-ensemble-member-per-model limit.
#'
#' **Access.** SNAP's rasdaman server is queried directly (one request per
#' variable, model, run and block of nearby catchments): the SNAP Data API
#' point endpoint rounds daily values to whole units, which zeroes most
#' daily precipitation. Monthly means of each 4 km cell are cached in
#' `snap_cmip6/` (one file per variable, model and run), so later calls,
#' other catchments in the same cells and a shared cache reuse them. The
#' first call for a region downloads roughly 1-5 MB per request; a full
#' default run for ~250 catchments is hundreds of requests and can take
#' hours. A progress bar counts the coverages (variable x model x run);
#' with mirai daemons running they are fetched in parallel (see
#' [ak_parallel]). Coverages that fail are reported and left out, not
#' fatal.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param source Projection product: `"snap_cmip6_4km"`.
#' @param scenarios SSP scenarios.
#' @param models Model names (default: every model with the variable and
#'   scenario, excluding SNAP's multi-model average `"7ModelAvg"`, whose
#'   spread is not model spread).
#' @param vars `"tasmax"`, `"tasmin"`, `"pr"`; `"tmean"` is added when both
#'   temperatures are requested.
#' @param baseline Baseline years.
#' @param periods Named list of future periods (years).
#' @param seasons Named list of months, see [ak_seasons()].
#' @param keep_series Keep the catchment season-year series (attribute
#'   `"series"`) for your own analyses (e.g. transient change by decade).
#' @param refresh Fetch again even when cached.
#' @return A table, one row per site, variable, scenario, model, period and
#'   season:
#'   `baseline_mean`, `future_mean` (C, or mm for the season), `delta` (C or
#'   mm), `delta_se`, `ratio` and `log_ratio_se` (precipitation), `n_base`,
#'   `n_fut` (years), `n_eff_base`, `n_eff_fut`, `delta_local_sd`, `n_cells`,
#'   `coverage_pct`. Attributes: `failed` (coverages that failed),
#'   `series` with `keep_series = TRUE`, and the provenance. Summarise
#'   across models with [projection_ensemble()].
#' @references Lavoie et al. 2024, Sci. Data, doi:10.1038/s41597-023-02855-z
#'   (the workflow SNAP follows); Cannon et al. 2015, J. Climate 28:6938 (QDM). SNAP CMIP6
#'   downscaling code: <https://github.com/ua-snap/cmip6-downscaling>.
#' @seealso [projection_ensemble()] to summarise across models.
#' @seealso `vignette("projections-snap")` (download, use, uncertainty, validation)
#' @export
ak_climate_projection <- function(catchments, source = "snap_cmip6_4km", scenarios = c("ssp245", "ssp585"), models = NULL,
                          vars = c("tasmax", "tasmin", "pr"), baseline = 1991:2020,
                          periods = list(`2041-2070` = 2041:2070, `2071-2100` = 2071:2100),
                          seasons = ak_seasons(), keep_series = FALSE, refresh = FALSE) {
  source <- match.arg(source)
  x <- as_catchments(catchments)
  if (is.null(names(periods)) || any(!nzchar(names(periods)))) stop("`periods` must be a named list of years")
  avail <- ak_climate_projection_models()
  combos <- avail[avail$var %in% vars & avail$scenario %in% scenarios & avail$model != "7ModelAvg", ]
  if (!is.null(models)) combos <- combos[combos$model %in% models, ]
  combos <- combos[combos$model %in% avail$model[avail$scenario == "historical"], ]
  if (!nrow(combos)) stop("No SNAP model has those variables and scenarios; see ak_climate_projection_models()", call. = FALSE)
  years <- sort(unique(c(baseline, unlist(periods))))
  grid <- snap_grid(snap_cov_id(combos$var[1], combos$model[1], "historical"), refresh)
  w <- snap_weights(x, grid)
  hist_years <- intersect(years, 1965:2014)
  fut_years <- intersect(years, 2015:2100)
  # One job per coverage (variable, model, run): each has its own cache file, so they can run in parallel
  runs <- unique(combos[c("var", "model")])
  covs <- dplyr::bind_rows(dplyr::mutate(runs, run = "historical"), dplyr::mutate(combos[c("var", "model")], run = combos$scenario))
  jobs <- purrr::pmap(covs, \(var, model, run) list(var = var, model = model, run = run,
                                                    years = if (run == "historical") hist_years else fut_years))
  names(jobs) <- paste(covs$var, covs$model, covs$run)
  res <- site_map(jobs, snap_job, cells = unique(w$cell), grid = grid, refresh = refresh, label = "SNAP coverages")
  bad <- site_errors(res, "snap")
  failed <- if (nrow(bad)) list(dplyr::tibble(var = covs$var[match(bad$site_code, names(jobs))], model = covs$model[match(bad$site_code, names(jobs))],
                                              run = covs$run[match(bad$site_code, names(jobs))], error = bad$error)) else list()
  cells_of <- \(var, model, run) res[[paste(var, model, run)]]$value
  monthly <- purrr::pmap(combos[c("var", "model", "scenario")], \(var, model, scenario) {
    h <- cells_of(var, model, "historical")
    f <- cells_of(var, model, scenario)
    if (is.null(h) || is.null(f)) return(NULL)
    dplyr::bind_rows(h, f) |> dplyr::mutate(var = var, model = model, scenario = scenario)
  }) |> dplyr::bind_rows()
  if (!nrow(monthly)) stop("No SNAP data could be read: ", paste(unique(purrr::map_chr(failed, "error")), collapse = "; "), call. = FALSE)
  if (all(c("tasmax", "tasmin") %in% unique(monthly$var))) {
    tm <- monthly |>
      dplyr::filter(.data$var %in% c("tasmax", "tasmin")) |>
      tidyr::pivot_wider(id_cols = c("cell", "year", "month", "model", "scenario"), names_from = "var", values_from = "value") |>
      dplyr::filter(!is.na(.data$tasmax), !is.na(.data$tasmin)) |>
      dplyr::transmute(.data$cell, .data$year, .data$month, value = (.data$tasmax + .data$tasmin) / 2, var = "tmean",
                       .data$model, .data$scenario)
    monthly <- dplyr::bind_rows(monthly, tm)
  }
  sy <- snap_season_years(monthly, seasons)
  out <- snap_deltas(sy, w, x, baseline, periods)
  attr(out, "failed") <- dplyr::bind_rows(failed)
  if (keep_series) attr(out, "series") <- snap_catchment_series(sy, w)
  if (length(failed)) warning(nrow(attr(out, "failed")), " SNAP coverage(s) failed and were left out; see attr(, \"failed\")", call. = FALSE)
  as_layer_table(out, layer_row("climate_projection", source, fn = "ak_climate_projection"),
                 list(source = source, scenarios = scenarios, models = models, vars = vars, baseline = range(baseline),
                      periods = names(periods), seasons = names(seasons)),
                 note = paste(dplyr::n_distinct(out$model), "models"))
}

#' Seasons for climate summaries
#'
#' Named list of month numbers. A season listed across the year end, such as
#' `djf = c(12, 1, 2)`, assigns the months before the wrap to the following
#' year (December 2040 belongs to the 2041 DJF). Add your own, for example a
#' winter matching your precipitation predictor: `c(ak_seasons(), nov_mar =
#' list(c(11, 12, 1, 2, 3)))`.
#'
#' @param months Also include each calendar month (`m01` ... `m12`).
#' @return A named list.
#' @seealso `vignette("projections-snap")` (download, use, uncertainty, validation)
#' @export
ak_seasons <- function(months = TRUE) {
  s <- list(annual = 1:12, djf = c(12L, 1L, 2L), mam = 3:5, jja = 6:8, son = 9:11)
  if (months) s <- c(s, stats::setNames(as.list(1:12), sprintf("m%02d", 1:12)))
  s
}

#' Models and scenarios available for climate projections
#'
#' The variable, model and scenario combinations in SNAP's 4 km downscaled
#' CMIP6 (SNAP Data API, `cmip6_downscaled`, checked November 2026). Every
#' model has a `"historical"` run (1965-2014); scenarios run 2015-2100.
#' `"7ModelAvg"` is SNAP's equal-weight mean of GFDL-ESM4, KACE-1-0-G, MIROC6,
#' MPI-ESM1-2-HR, MRI-ESM2-0, NorESM2-MM and TaiESM1.
#'
#' @param source Projection product: `"snap_cmip6_4km"`.
#' @return Data frame: `var`, `model`, `scenario`.
#' @seealso `vignette("projections-snap")` (download, use, uncertainty, validation)
#' @export
ak_climate_projection_models <- function(source = "snap_cmip6_4km") {
  source <- match.arg(source)
  all4 <- c("ssp126", "ssp245", "ssp370", "ssp585")
  tas <- list(`7ModelAvg` = all4, `CNRM-CM6-1-HR` = c("ssp126", "ssp585"), `E3SM-2-0` = "ssp370",
              `EC-Earth3-Veg` = c("ssp370", "ssp585"), `GFDL-ESM4` = all4, `HadGEM3-GC31-LL` = c("ssp126", "ssp245", "ssp585"),
              `HadGEM3-GC31-MM` = c("ssp126", "ssp585"), `KACE-1-0-G` = all4, MIROC6 = all4, `MPI-ESM1-2-HR` = all4,
              `MRI-ESM2-0` = all4, `NorESM2-MM` = all4, TaiESM1 = all4)
  pr <- c(tas, list(CESM2 = all4))
  pr$`EC-Earth3-Veg` <- c("ssp126", "ssp370", "ssp585")
  tab <- \(l, v) dplyr::bind_rows(purrr::imap(l, \(s, m) dplyr::tibble(var = v, model = m, scenario = c("historical", s))))
  dplyr::bind_rows(tab(tas, "tasmax"), tab(tas, "tasmin"), tab(pr, "pr"))
}

#' Summarise downscaled CMIP6 changes across models
#'
#' The model ensemble for each site, variable, scenario, period and season.
#' Models are weighted equally (an "ensemble of opportunity": they are not
#' independent or a random sample of possible models, so the spread is a
#' guide to structural uncertainty, not a probability).
#'
#' * Temperature: `delta` is the ensemble mean change (C), `delta_se` the
#'   between-model SD, and `delta_lo95`/`delta_hi95` the 95% prediction
#'   interval for one more model treated as exchangeable with these
#'   (`mean +/- t(n-1) sd sqrt(1 + 1/n)`), which is wider than the range of
#'   a few models. `delta_min`/`delta_max` are the model range.
#' * Precipitation: changes are averaged on the log scale. `pct` is the
#'   percent change of the geometric-mean ratio, `pct_se` the between-model
#'   SD carried to percent (delta method), and `pct_lo95`/`pct_hi95` the
#'   log-scale prediction interval back-transformed. `delta` (mm) is also
#'   summarised.
#' * `internal_se` is the root-mean-square of the models' own sampling SEs:
#'   when it approaches the between-model SD, much of the model spread is
#'   year-to-year noise rather than model difference.
#' * `sign_agree` is the share of models agreeing with the ensemble sign.
#'
#' `flag`: `few_models` (fewer than 5), `subgrid` (catchment smaller than a
#' 4 km cell), `partial_coverage` (under 95% of the catchment in cells with
#' data), `no_coverage`, `sign_disagree` (under 2/3 of models agree on the
#' sign of the precipitation change), `noise_dominated` (`internal_se` at
#' least half the between-model SD), `dry_baseline` (baseline season
#' precipitation under 5 mm, where ratios are unstable), `local_gradient`
#' (the change differs between neighbouring cells by more than half the
#' between-model SD).
#'
#' With `wide = TRUE` the ensemble is returned one row per site, ready for
#' [catchment_join()]: for each variable in `keep`, season, scenario and
#' period, `cmip6_d<var>_<season>_<scenario>_<period>` (temperature change,
#' C) or `cmip6_pr_pct_...` (precipitation change, percent) with `_se`,
#' `_lo95`, `_hi95`, `_nmodels` and `_flag`. Choose the seasons, scenarios
#' and periods in [ak_climate_projection()]: every combination becomes five
#' columns.
#'
#' @param deltas Output of [ak_climate_projection()].
#' @param level Interval level.
#' @param wide One row per site (wide columns) instead of the long table?
#' @param keep With `wide`: variables to keep.
#' @return The long table (one row per site, variable, scenario, period and
#'   season) or, with `wide`, one row per site; with the provenance of
#'   `deltas`.
#' @examples
#' \dontrun{
#' d <- ak_climate_projection(x, seasons = ak_seasons(months = FALSE)[c("annual", "jja")],
#'                            periods = list(`2041-2070` = 2041:2070))
#' x <- catchment_join(x, projection_ensemble(d, wide = TRUE))
#' }
#' @seealso `vignette("projections-snap")` (download, use, uncertainty, validation)
#' @export
projection_ensemble <- function(deltas, level = 0.95, wide = FALSE, keep = c("tmean", "pr")) {
  q <- 1 - (1 - level) / 2
  d <- dplyr::filter(deltas, !is.na(.data$delta))
  e <- d |>
    dplyr::summarise(
      n_models = dplyr::n(),
      delta_mean = mean(.data$delta), delta_sd = stats::sd(.data$delta),
      delta_min = min(.data$delta), delta_max = max(.data$delta),
      lr_mean = mean(log(.data$ratio)), lr_sd = stats::sd(log(.data$ratio)),
      internal_se = sqrt(mean(.data$delta_se^2)), internal_lr_se = sqrt(mean(.data$log_ratio_se^2)),
      sign_agree = mean(sign(.data$delta) == sign(mean(.data$delta))),
      baseline_mean = mean(.data$baseline_mean),
      delta_local_sd = mean(.data$delta_local_sd), n_cells = max(.data$n_cells), coverage_pct = min(.data$coverage_pct),
      subgrid = any(.data$subgrid), .by = c("site_code", "var", "scenario", "period", "season")) |>
    dplyr::mutate(
      half = stats::qt(q, pmax(.data$n_models - 1, 1)) * sqrt(1 + 1 / .data$n_models),
      precip = .data$var == "pr",
      delta = .data$delta_mean, delta_se = .data$delta_sd,
      delta_lo95 = .data$delta_mean - .data$half * .data$delta_sd,
      delta_hi95 = .data$delta_mean + .data$half * .data$delta_sd,
      pct = ifelse(.data$precip, 100 * (exp(.data$lr_mean) - 1), NA_real_),
      pct_se = ifelse(.data$precip, 100 * exp(.data$lr_mean) * .data$lr_sd, NA_real_),
      pct_lo95 = ifelse(.data$precip, 100 * (exp(.data$lr_mean - .data$half * .data$lr_sd) - 1), NA_real_),
      pct_hi95 = ifelse(.data$precip, 100 * (exp(.data$lr_mean + .data$half * .data$lr_sd) - 1), NA_real_),
      spread = ifelse(.data$precip, .data$lr_sd, .data$delta_sd),
      noise = ifelse(.data$precip, .data$internal_lr_se, .data$internal_se),
      flag = flag_text(few_models = .data$n_models < 5, subgrid = .data$subgrid,
                       partial_coverage = .data$coverage_pct < 95 & .data$coverage_pct > 0, no_coverage = .data$coverage_pct == 0,
                       sign_disagree = .data$precip & .data$sign_agree < 2 / 3,
                       noise_dominated = .data$noise >= 0.5 * .data$spread,
                       dry_baseline = .data$precip & .data$baseline_mean < 5,
                       local_gradient = .data$delta_local_sd > 0.5 * .data$delta_sd)) |>
    dplyr::select("site_code", "var", "scenario", "period", "season", "n_models", "delta", "delta_se", "delta_lo95",
                  "delta_hi95", "delta_min", "delta_max", "pct", "pct_se", "pct_lo95", "pct_hi95", "internal_se",
                  "sign_agree", "delta_local_sd", "n_cells", "coverage_pct", "flag")
  out <- if (wide) ensemble_wide(e, keep) else e
  attr(out, "ak_provenance") <- ak_provenance(deltas)
  out
}

snap_job <- function(j, cells, grid, refresh) snap_cells(j$var, j$model, j$run, j$years, cells, grid, refresh)

# ---- server access ---------------------------------------------------------------------------------------------

snap_cov_id <- function(var, model, run) paste0("cmip6_downscaled_", var, "_", gsub("-", "_", model), "_", run, "_v2_wcs")

# Coverage description (axes, grid, time coordinates), cached: it is the same for every block
snap_describe <- function(cov, refresh = FALSE) {
  f <- file.path(ak_cache_dir(cache_paths$snap, "describe"), paste0(cov, ".rds"))
  if (file.exists(f) && !refresh) return(readRDS(f))
  q <- sprintf('for $c in (%s) return describe($c, "application/json", "outputType=GeneralGridCoverage")', cov)
  j <- ak_json("https://zeus.snap.uaf.edu/rasdaman/ows", list(SERVICE = "WCS", VERSION = "2.1.0", REQUEST = "ProcessCoverages", query = q), simplify = FALSE)
  d <- snap_parse_describe(j)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(d, f)
  d
}

snap_parse_describe <- function(j) {
  axes <- j$domainSet$generalGrid$axis
  lab <- purrr::map_chr(axes, "axisLabel")
  ax <- \(l) axes[[match(l, lab)]]
  tl <- intersect(c("ansi", "time"), lab)[1]
  if (is.na(tl) || !all(c("X", "Y") %in% lab)) stop("Unexpected SNAP coverage axes: ", paste(lab, collapse = ", "), call. = FALSE)
  num <- \(v) as.numeric(unlist(v))
  nil <- suppressWarnings(num(purrr::map(j$rangeType$field %||% list(), \(f) purrr::map(f$nilValues$nilValue %||% list(), "value"))))
  list(xmin = num(ax("X")$lowerBound), xmax = num(ax("X")$upperBound), xres = abs(num(ax("X")$resolution)),
       ymin = num(ax("Y")$lowerBound), ymax = num(ax("Y")$upperBound), yres = abs(num(ax("Y")$resolution)),
       time_label = tl, time = as.Date(substr(unlist(ax(tl)$coordinate), 1, 10)), nil = nil[!is.na(nil)],
       uom = unlist(purrr::map(j$rangeType$field %||% list(), \(f) f$uom$code %||% f$uom)))
}

# The 4 km grid as a raster template whose values are cell numbers; cell ids are "col_row" (0-based from the NW corner)
snap_grid <- function(cov, refresh = FALSE) {
  d <- snap_describe(cov, refresh)
  d$ncol <- round((d$xmax - d$xmin) / d$xres)
  d$nrow <- round((d$ymax - d$ymin) / d$yres)
  d
}

snap_template <- function(g) {
  r <- terra::rast(xmin = g$xmin, xmax = g$xmax, ymin = g$ymin, ymax = g$ymax, ncols = g$ncol, nrows = g$nrow,
                   crs = paste0("EPSG:", akalbers_crs()))
  terra::values(r) <- seq_len(terra::ncell(r))
  r
}

# Cells under each catchment (coverage fraction) plus the ring of neighbours (weight 0), for delta_local_sd
snap_weights <- function(x, g) {
  r <- snap_template(g)
  cell_area <- g$xres * g$yres
  ex <- exactextractr::exact_extract(r, x, progress = FALSE)
  purrr::map2(ex, seq_along(ex), \(d, i) {
    d <- d[!is.na(d$value), ]
    if (!nrow(d)) return(NULL)
    col <- (d$value - 1) %% g$ncol
    row <- (d$value - 1) %/% g$ncol
    nb <- tidyr::expand_grid(col = (min(col) - 1):(max(col) + 1), row = (min(row) - 1):(max(row) + 1)) |>
      dplyr::filter(.data$col >= 0, .data$col < g$ncol, .data$row >= 0, .data$row < g$nrow)
    dplyr::tibble(col = col, row = row, w = d$coverage_fraction * cell_area) |>
      dplyr::right_join(nb, by = c("col", "row")) |>
      dplyr::mutate(site_code = x$site_code[i], w = dplyr::coalesce(.data$w, 0), cell = paste(.data$col, .data$row, sep = "_"),
                    area_m2 = x$area_m2[i])
  }) |>
    dplyr::bind_rows()
}

# Monthly means of the requested cells and years for one variable, model and run, from the cache or the server
snap_cells <- function(var, model, run, years, cells, g, refresh = FALSE) {
  if (!length(years)) return(NULL)
  cov <- snap_cov_id(var, model, run)
  f <- file.path(ak_cache_dir(cache_paths$snap), paste0(cov, ".rds"))
  have <- if (file.exists(f) && !refresh) readRDS(f) else snap_empty()
  done <- have |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::distinct(.data$cell, .data$year) |>
    dplyr::count(.data$cell) |>
    dplyr::filter(.data$n == length(years))
  todo <- setdiff(cells, done$cell)
  if (length(todo)) {
    d <- snap_describe(cov, refresh)
    yrs <- range(years)
    new <- purrr::map(snap_blocks(todo, g), \(b) snap_fetch_block(cov, b, d, yrs, g)) |> dplyr::bind_rows()
    if (!nrow(new)) stop("No data returned for ", cov)
    have <- dplyr::bind_rows(dplyr::anti_join(have, dplyr::distinct(new, .data$cell, .data$year), by = c("cell", "year")), new)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(fileext = ".rds")
    saveRDS(have, tmp)
    if (!replace_file(tmp, f)) stop("Could not write ", f)
    log_download(paste0("https://zeus.snap.uaf.edu/rasdaman/ows?COVERAGEID=", cov), f)
  }
  dplyr::filter(have, .data$cell %in% cells, .data$year %in% years)
}

snap_empty <- function() dplyr::tibble(cell = character(), year = integer(), month = integer(), value = numeric(), n_days = integer())

# Group cells into rectangular request blocks: nearby cells share a request, distant ones don't drag in the space between.
# Blocks are never square (nx != ny), so the axis order of the returned array can be told from its shape.
snap_blocks <- function(cells, g, gap = 3, max_cells = 400) {
  cr <- do.call(rbind, strsplit(cells, "_", fixed = TRUE))
  p <- dplyr::tibble(col = as.integer(cr[, 1]), row = as.integer(cr[, 2])) |> dplyr::arrange(.data$col, .data$row)
  blocks <- list()
  for (i in seq_len(nrow(p))) {
    hit <- FALSE
    for (k in seq_along(blocks)) {
      b <- blocks[[k]]
      near <- p$col[i] >= b$c0 - gap && p$col[i] <= b$c1 + gap && p$row[i] >= b$r0 - gap && p$row[i] <= b$r1 + gap
      nb <- list(c0 = min(b$c0, p$col[i]), c1 = max(b$c1, p$col[i]), r0 = min(b$r0, p$row[i]), r1 = max(b$r1, p$row[i]))
      if (near && (nb$c1 - nb$c0 + 1) * (nb$r1 - nb$r0 + 1) <= max_cells) {
        blocks[[k]] <- nb
        hit <- TRUE
        break
      }
    }
    if (!hit) blocks[[length(blocks) + 1]] <- list(c0 = p$col[i], c1 = p$col[i], r0 = p$row[i], r1 = p$row[i])
  }
  purrr::map(blocks, \(b) {
    if (b$c1 - b$c0 == b$r1 - b$r0) {   # square: widen by one column, inside the grid
      if (b$c1 + 1 < g$ncol) b$c1 <- b$c1 + 1 else b$c0 <- b$c0 - 1
    }
    b
  })
}

# One GetCoverage request (JSON) for a block and year range, reduced to monthly means per cell
snap_fetch_block <- function(cov, b, d, yrs, g) {
  t <- d$time[d$time >= as.Date(paste0(yrs[1], "-01-01")) & d$time <= as.Date(paste0(yrs[2], "-12-31"))]
  if (!length(t)) return(NULL)
  cx <- \(c) g$xmin + (c + 0.5) * g$xres
  cy <- \(r) g$ymax - (r + 0.5) * g$yres
  # bounds a quarter cell inside the outer cell centres: selects exactly these cells whether the server
  # picks cells by footprint or by centre
  sub <- c(sprintf("X(%.3f,%.3f)", cx(b$c0) - g$xres / 4, cx(b$c1) + g$xres / 4),
           sprintf("Y(%.3f,%.3f)", cy(b$r1) - g$yres / 4, cy(b$r0) + g$yres / 4),
           sprintf('%s("%s","%s")', d$time_label, min(t), max(t)))
  a <- snap_parse_array(snap_get(snap_url(cov, sub)), d$nil)
  nx <- b$c1 - b$c0 + 1
  ny <- b$r1 - b$r0 + 1
  a <- snap_orient(a, length(t), nx, ny, \(c, r) snap_check_point(cov, c, r, d, t[1:min(5, length(t))], cx, cy),
                   corners = list(c(b$c0, b$r0), c(b$c1, b$r0), c(b$c0, b$r1), c(b$c1, b$r1)))
  m <- matrix(a, nrow = length(t))          # time x cells, cells with col fastest from the west and rows from the north
  cells <- paste(rep(b$c0:b$c1, times = ny), rep(b$r0:b$r1, each = nx), sep = "_")
  snap_monthly(m, t, cells)
}

snap_get <- function(url) ak_request(url) |> ak_perform() |> httr2::resp_body_string()

snap_url <- function(cov, subsets, format = "application/json") {
  enc <- \(v) utils::URLencode(v, reserved = TRUE)
  paste0("https://zeus.snap.uaf.edu/rasdaman/ows?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=", cov,
         paste0("&SUBSET=", vapply(subsets, enc, ""), collapse = ""), "&FORMAT=", enc(format))
}

snap_parse_array <- function(txt, nil = numeric()) {
  txt <- gsub("(?<![\"A-Za-z])-?(NaN|nan|Infinity|inf)(?![\"A-Za-z])", "null", txt, perl = TRUE)
  a <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  if (!is.numeric(a)) stop("SNAP returned an unexpected response: ", substr(txt, 1, 200), call. = FALSE)
  a[a %in% nil | a < -1000 | a > 1e6] <- NA
  if (is.null(dim(a))) dim(a) <- length(a)
  a
}

# Reorder a returned array to [time, x (west to east), y (north to south)]. The time axis is the one with the
# time length; x and y are told apart by length (blocks are never square). The direction of each spatial axis
# is checked against point requests at the block's corners (NW, NE, SW, SE; corners with no data are skipped)
# until one orientation alone fits, once per session (all coverages share a layout).
snap_orient <- function(a, nt, nx, ny, point, corners) {
  dm <- dim(a)
  if (length(dm) != 3) stop("SNAP returned an array with ", length(dm), " dimensions; expected 3", call. = FALSE)
  it <- which(dm == nt)[1]
  ix <- setdiff(which(dm == nx), it)[1]
  iy <- setdiff(which(dm == ny), c(it, ix))[1]
  if (anyNA(c(it, ix, iy))) stop("SNAP array shape ", paste(dm, collapse = "x"), " does not match time ", nt, ", x ", nx,
                                 ", y ", ny, call. = FALSE)
  a <- aperm(a, c(it, ix, iy))
  flip <- getOption("AKCatchments.snap_flip")
  if (is.null(flip)) {
    cand <- list(c(FALSE, FALSE), c(FALSE, TRUE), c(TRUE, FALSE), c(TRUE, TRUE))
    alive <- rep(TRUE, 4)
    pos <- list(c(1, 1), c(nx, 1), c(1, ny), c(nx, ny))      # corner positions with no flip: NW, NE, SW, SE
    for (k in seq_along(corners)) {
      p <- point(corners[[k]][1], corners[[k]][2])
      if (all(is.na(p))) next
      i <- seq_along(p)
      for (j in which(alive)) {
        f <- cand[[j]]
        at <- c(if (f[1]) nx + 1 - pos[[k]][1] else pos[[k]][1], if (f[2]) ny + 1 - pos[[k]][2] else pos[[k]][2])
        alive[j] <- isTRUE(all.equal(as.numeric(a[i, at[1], at[2]]), as.numeric(p), tolerance = 1e-4))
      }
      if (sum(alive) <= 1) break
    }
    if (sum(alive) != 1) stop("Could not tell the orientation of SNAP's arrays from check points at the block corners",
                              call. = FALSE)
    flip <- cand[[which(alive)]]
    options(AKCatchments.snap_flip = flip)
  }
  if (isTRUE(flip[1])) a <- a[, rev(seq_len(nx)), , drop = FALSE]
  if (isTRUE(flip[2])) a <- a[, , rev(seq_len(ny)), drop = FALSE]
  a
}

# A few days at one cell centre, for the orientation check
snap_check_point <- function(cov, col, row, d, t, cx, cy) {
  sub <- c(sprintf("X(%.3f)", cx(col)), sprintf("Y(%.3f)", cy(row)), sprintf('%s("%s","%s")', d$time_label, min(t), max(t)))
  as.numeric(snap_parse_array(snap_get(snap_url(cov, sub)), d$nil))
}

# Daily matrix (time x cells) to monthly means per cell; units checked (K to C, kg m-2 s-1 to mm/day)
snap_monthly <- function(m, t, cells) {
  med <- stats::median(m, na.rm = TRUE)
  if (isTRUE(med > 150)) m <- m - 273.15
  ym <- format(t, "%Y-%m")
  s <- rowsum(m, ym, na.rm = TRUE)
  n <- rowsum((!is.na(m)) * 1, ym)
  v <- ifelse(n > 0, s / n, NA_real_)
  # cells without data (ocean) are kept as NA rows, so the cache knows they were fetched
  dplyr::tibble(cell = rep(cells, each = nrow(v)), ym = rep(rownames(v), times = length(cells)),
                value = as.numeric(v), n_days = as.integer(n)) |>
    dplyr::mutate(year = as.integer(substr(.data$ym, 1, 4)), month = as.integer(substr(.data$ym, 6, 7)), .keep = "unused") |>
    dplyr::select("cell", "year", "month", "value", "n_days")
}

# ---- summaries ----------------------------------------------------------------------------------------------------

# Cell season-year values: temperature day-weighted mean of monthly means, precipitation total (rate x calendar days)
snap_season_years <- function(monthly, seasons) {
  purrr::imap(seasons, \(m, nm) {
    k <- which(diff(m) < 0)[1]
    early <- if (is.na(k)) integer() else m[seq_len(k)]
    monthly |>
      dplyr::filter(.data$month %in% m, !is.na(.data$value)) |>
      dplyr::mutate(sy = .data$year + (.data$month %in% early),
                    days = month_days(.data$year, .data$month)) |>
      dplyr::summarise(n = dplyr::n(), vd = sum(.data$value * .data$days), d = sum(.data$days),
                       .by = c("var", "model", "scenario", "cell", "sy")) |>
      dplyr::filter(.data$n == length(m)) |>
      dplyr::transmute(.data$var, .data$model, .data$scenario, .data$cell, year = .data$sy, season = nm,
                       value = ifelse(.data$var == "pr", .data$vd, .data$vd / .data$d))
  }) |>
    dplyr::bind_rows()
}

month_days <- function(year, month) {
  as.integer(as.Date(sprintf("%d-%02d-01", year + (month == 12), month %% 12 + 1)) - as.Date(sprintf("%d-%02d-01", year, month)))
}

# Catchment season-year series: weighted mean of the cells with data
snap_catchment_series <- function(sy, w) {
  dplyr::inner_join(sy, w[w$w > 0, c("site_code", "cell", "w", "area_m2")], by = "cell", relationship = "many-to-many") |>
    dplyr::summarise(value = sum(.data$w * .data$value) / sum(.data$w), covered_m2 = sum(.data$w),
                     n_cells = dplyr::n(), area_m2 = .data$area_m2[1],
                     .by = c("site_code", "var", "model", "scenario", "season", "year"))
}

# Lag-1 autocorrelation of a linearly detrended series (0 if it can't be estimated or is negative)
r1_detrended <- function(y, t) {
  if (length(y) < 5 || stats::sd(y) == 0) return(0)
  e <- stats::residuals(stats::lm(y ~ t))
  r <- suppressWarnings(stats::cor(e[-1], e[-length(e)]))
  if (is.na(r)) 0 else max(0, r)
}

period_stats <- function(y, t) {
  n <- length(y)
  r <- r1_detrended(y, t)
  list(mean = mean(y), var = stats::var(y), n = n, n_eff = max(2, n * (1 - r) / (1 + r)))
}

snap_deltas <- function(sy, w, x, baseline, periods) {
  cs <- snap_catchment_series(sy, w)
  one <- \(d, yrs) { d <- d[d$year %in% yrs, ]; period_stats(d$value, d$year) }
  res <- cs |>
    dplyr::group_by(.data$site_code, .data$var, .data$model, .data$scenario, .data$season) |>
    dplyr::group_modify(\(d, k) {
      b <- one(d, baseline)
      purrr::imap(periods, \(yrs, p) {
        f <- one(d, yrs)
        se <- sqrt(f$var / f$n_eff + b$var / b$n_eff)
        dplyr::tibble(period = p, baseline_mean = b$mean, future_mean = f$mean, delta = f$mean - b$mean, delta_se = se,
                      ratio = if (k$var == "pr" && b$mean > 0) f$mean / b$mean else NA_real_,
                      log_ratio_se = if (k$var == "pr" && b$mean > 0 && f$mean > 0)
                        sqrt(f$var / (f$n_eff * f$mean^2) + b$var / (b$n_eff * b$mean^2)) else NA_real_,
                      n_base = b$n, n_fut = f$n, n_eff_base = b$n_eff, n_eff_fut = f$n_eff,
                      n_cells = max(d$n_cells), coverage_pct = min(100, 100 * max(d$covered_m2) / d$area_m2[1]))
      }) |> dplyr::bind_rows()
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(delta = ifelse(.data$n_base < 10 | .data$n_fut < 10, NA_real_, .data$delta))
  # Local spread of the change: per-cell deltas over the catchment's cells and their neighbours
  cell_delta <- sy |>
    dplyr::summarise(b = mean(.data$value[.data$year %in% baseline]),
                     f = list(purrr::map_dbl(periods, \(yrs) mean(.data$value[.data$year %in% yrs]))),
                     .by = c("var", "model", "scenario", "season", "cell")) |>
    tidyr::unnest_longer("f", indices_to = "period") |>
    dplyr::mutate(d = .data$f - .data$b)
  local <- dplyr::inner_join(cell_delta, w[c("site_code", "cell")], by = "cell", relationship = "many-to-many") |>
    dplyr::summarise(delta_local_sd = if (dplyr::n() > 1) stats::sd(.data$d, na.rm = TRUE) else NA_real_,
                     .by = c("site_code", "var", "model", "scenario", "season", "period"))
  sub_area <- w |> dplyr::summarise(area_m2 = .data$area_m2[1], .by = "site_code")
  res |>
    dplyr::left_join(local, by = c("site_code", "var", "model", "scenario", "season", "period")) |>
    dplyr::left_join(sub_area, by = "site_code") |>
    dplyr::mutate(subgrid = .data$area_m2 < 16e6) |>
    dplyr::select("site_code", "var", "scenario", "model", "period", "season", "baseline_mean", "future_mean", "delta",
                  "delta_se", "ratio", "log_ratio_se", "n_base", "n_fut", "n_eff_base", "n_eff_fut", "delta_local_sd",
                  "n_cells", "coverage_pct", "subgrid")
}

# Wide ensemble columns: one set per variable, season, scenario and period
ensemble_wide <- function(e, keep = c("tmean", "pr")) {
  e <- e[e$var %in% keep, ]
  stem <- paste("cmip6", ifelse(e$var == "pr", "pr_pct", paste0("d", e$var)), e$season, e$scenario,
                gsub("-", "_", e$period), sep = "_")
  pr <- e$var == "pr"
  long <- dplyr::bind_rows(
    dplyr::tibble(site_code = e$site_code, col = stem, value = ifelse(pr, e$pct, e$delta)),
    dplyr::tibble(site_code = e$site_code, col = paste0(stem, "_se"), value = ifelse(pr, e$pct_se, e$delta_se)),
    dplyr::tibble(site_code = e$site_code, col = paste0(stem, "_lo95"), value = ifelse(pr, e$pct_lo95, e$delta_lo95)),
    dplyr::tibble(site_code = e$site_code, col = paste0(stem, "_hi95"), value = ifelse(pr, e$pct_hi95, e$delta_hi95)),
    dplyr::tibble(site_code = e$site_code, col = paste0(stem, "_nmodels"), value = e$n_models))
  num <- tidyr::pivot_wider(long, names_from = "col", values_from = "value")
  flg <- tidyr::pivot_wider(dplyr::tibble(site_code = e$site_code, col = paste0(stem, "_flag"), value = e$flag),
                            names_from = "col", values_from = "value")
  out <- dplyr::left_join(num, flg, by = "site_code")
  out[c("site_code", sort(setdiff(names(out), "site_code")))]
}
