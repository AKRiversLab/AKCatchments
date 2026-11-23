#' Weather at each sample date: anomalies, melt timing and antecedent summaries
#'
#' From the daily catchment weather of [ak_weather()], one row per site and
#' sample date with three groups of columns, each explained in a section
#' below:
#' * **anomalies** of temperature and precipitation over the 7 and 30 days
#'   before the sample, against that catchment's normal for those days of
#'   the year;
#' * **melt timing** from Daymet snow water equivalent (`melt = TRUE`);
#' * **antecedent summaries**: snowfall over the snow year, summer mean
#'   temperature, degree days to the sample date, and (with `snowoff` from
#'   [ak_snowoff()]) days and thawing degree days since snow-off.
#'
#' **What an anomaly is compared with.** The normal is the catchment's
#' daily climatology over the `baseline` years (default 1991-2020, the
#' current WMO normal period): a smooth seasonal cycle, three harmonics of
#' the day of year, fitted to every day of those years. So `t_anom7` on 15
#' July is the mean temperature of 9-15 July that year minus the normal mean
#' temperature of 9-15 July, not the departure from a monthly mean or from
#' the same calendar day in the raw record (which is noisy). This is the
#' daily counterpart of the monthly normals of [ak_climate_normals()];
#' `weather` must cover the baseline years as well as the sample years.
#'
#' With `samples = NULL` the anomalies are returned for every day of the
#' series (no SEs, no melt or summaries), e.g. to plot them.
#'
#' @section Anomalies:
#' Weather anomalies around each sample date.
#'
#' From the catchment daily Daymet series: the climatology of each catchment
#' is a three-harmonic seasonal cycle fitted to `baseline_years`, and the
#' anomalies are
#' * `t_anom7`, `t_anom30`: mean daily temperature anomaly (C) over the 7 and
#'   30 days ending on the date;
#' * `t_anom7_dt`, `t_anom30_dt`: the same after removing the catchment's
#'   linear trend (pure weather; the raw versions carry the warming trend);
#' * `p_anom7`, `p_anom30`: log(1 + precipitation total, mm) over the window
#'   minus its climatology.
#' With `melt = TRUE`, also `melt_doy`, `melt_anom` and
#' `days_since_melt` (sample day of year minus melt day).
#'
#' Daymet years have 365 days (31 December is dropped in leap years), so day
#' of year matches the calendar for every sample date. `weather` must include
#' the baseline years.
#'
#' Uncertainty (at sample dates): `*_se` is the
#' point-sampling SE of each anomaly. The same anomalies are computed for
#' every Daymet pixel behind the catchment (each against its own
#' climatology), and the SE is their between-pixel SD / sqrt(n) x sqrt(1 -
#' n / N). For temperature this is exact: the harmonic fit, the trend and the
#' window means are linear, so the catchment anomaly is the mean of the pixel
#' anomalies. It is much smaller than the daily SE in `weather`, because a
#' pixel that is warmer than the catchment (higher, lower, south-facing) is
#' warmer in its climatology too, and that cancels. For precipitation (log
#' of window totals) it is an approximation. `weather_flag`: `few_pixels`
#' where fewer than 3 pixels make the mean, so the SE is unreliable.
#' Daymet's own interpolation error is not included: it is largely
#' persistent, so it mostly cancels in anomalies too (see
#' `vignette("weather-daymet")`).
#'
#'
#' @section Melt timing (`melt = TRUE`):
#' Snowmelt timing from Daymet snow water equivalent.
#'
#' For each Daymet pixel behind a catchment, the melt day of each year is the
#' first day after peak SWE (January to day `max_doy`) on which SWE falls
#' below `swe_min` kg/m2; the catchment value is the median over its pixels.
#' Pixels that never melt (glacier, perennial snow) or never hold snow drop
#' out of the median. `melt_anom` is the departure from the catchment's mean
#' over `baseline_years`.
#'
#'
#' @section Antecedent summaries:
#' Snowfall, summer temperature and degree days for each sample.
#'
#' From the catchment daily Daymet series, per observation:
#' * `snowfall_swe_mm`, `snowfall_depth_cm`: total over the snow year (from
#'   `snow_year_start` month, labelled by the year it ends). Daymet gives
#'   total precipitation, so it is split into snow with a linear ramp (all
#'   snow below `t_snow`, all rain above `t_rain`) and converted to depth
#'   with Hedstrom and Pomeroy (1998) new-snow density, 67.92 + 51.25 exp(T /
#'   2.59) kg/m3, limited to 50-200. Partial snow years are dropped.
#' * `summer_tmean_c`: mean temperature in `summer_months` of the calendar
#'   year.
#' * degree days from 1 January to the sample date: `gdd_base<b>` for each
#'   base, `tdd` (thawing, base 0) and `fdd` (freezing, negative).
#' * with `snowoff` (from [ak_snowoff()]): `snowoff_doy`, `snowoff_source`,
#'   `tdd_snowoff` (thawing degree days from snow-off to the sample, 0 if
#'   sampled before snow-off), `tdd_snowoff_se` and `days_since_snowoff`.
#'
#' Uncertainty: the snow-off date's error carries into the two snow-off
#' columns. `days_since_snowoff` has the SE of the date, `snowoff_se_d` in
#' [ak_snowoff()]. `tdd_snowoff_se` is that SE times the mean thawing
#' temperature over the days within one SE of snow-off (up to the sample
#' date): a first-order (delta-method) propagation, since moving snow-off by
#' a day adds or removes that day's thawing degrees. The other columns carry
#' the Daymet sampling and interpolation error of the series (see
#' `vignette("weather-daymet")`); no SE is computed for them.
#'
#' Sample dates must be real dates, not site means.
#'
#'
#' @param weather Daily weather from [ak_weather()], covering the baseline
#'   years, the sample years and the year before each.
#' @param samples Data frame of `site_code` and `sample_date` (other columns
#'   are kept), or `NULL` for the daily anomaly series.
#' @param baseline Years defining the normals (climatology and normal melt
#'   day).
#' @param snowoff Optional snow-off table from [ak_snowoff()], for the
#'   snow-off columns.
#' @param melt Add the Daymet melt-day columns?
#' @param se Compute the anomaly sampling SEs (reads the cached pixel files)?
#' @param t_snow,t_rain Rain-snow ramp limits, C.
#' @param gdd_bases Growing degree day bases, C.
#' @param summer_months Months for the summer mean.
#' @param snow_year_start First month of the snow year.
#' @param max_doy,swe_min Melt day: last day of year searched, and the SWE
#'   (kg/m2) below which a pixel counts as melted.
#' @return `samples` with the weather columns added (one row per row of
#'   `samples`), and the provenance of `weather`; or, with `samples = NULL`,
#'   the daily anomaly series.
#' @examples
#' \dontrun{
#' w <- ak_weather(x, years = 1990:2024)
#' s <- ak_snowoff(x, years = 2001:2024, weather = w)
#' ws <- weather_at_samples(w, samples, snowoff = s)
#' }
#' @seealso `vignette("weather-daymet")`, `vignette("snowoff-modis")`
#' @export
weather_at_samples <- function(weather, samples = NULL, baseline = 1991:2020, snowoff = NULL, melt = TRUE, se = TRUE,
                               t_snow = -1, t_rain = 3, gdd_bases = c(0, 5), summer_months = 6:8, snow_year_start = 10,
                               max_doy = 212, swe_min = 1) {
  check_baseline(weather, baseline)
  if (is.null(samples)) return(weather_anomalies(weather, NULL, baseline))
  if (!all(c("site_code", "sample_date") %in% names(samples))) stop("`samples` needs site_code and sample_date columns", call. = FALSE)
  samples <- dplyr::mutate(as.data.frame(samples), sample_date = as.Date(.data$sample_date))
  obs <- dplyr::distinct(samples, .data$site_code, .data$sample_date)
  m <- if (melt) melt_daymet(weather, baseline, max_doy, swe_min)
  an <- weather_anomalies(weather, obs, baseline, m, se)
  cs <- climate_summaries(weather, obs, snowoff, t_snow, t_rain, gdd_bases, summer_months, snow_year_start)
  cs <- cs[c("site_code", "sample_date", setdiff(names(cs), c(names(an), names(samples))))]
  out <- samples |>
    dplyr::left_join(an, by = c("site_code", "sample_date")) |>
    dplyr::left_join(dplyr::distinct(cs, .data$site_code, .data$sample_date, .keep_all = TRUE), by = c("site_code", "sample_date"))
  attr(out, "ak_provenance") <- dplyr::bind_rows(ak_provenance(weather), if (!is.null(snowoff)) ak_provenance(snowoff))
  as_layer_table(out, layer_row("weather_daily", fn = "weather_at_samples"),
                 list(baseline = range(baseline), melt = melt, se = se, snowoff = !is.null(snowoff)))
}

# The baseline years must be in the weather table (a normal from part of them is not that normal)
check_baseline <- function(weather, baseline) {
  miss <- setdiff(baseline, unique(weather$year))
  if (length(miss)) stop("`weather` lacks baseline years ", paste(range(miss), collapse = "-"), " (", length(miss),
                         " years): download them with ak_weather(x, years = ", min(baseline), ":...)", call. = FALSE)
}

melt_daymet <- function(daymet, baseline_years = 1991:2020, max_doy = 212, swe_min = 1) {
  px <- attr(daymet, "pixels")
  files <- attr(daymet, "pixel_files")
  if (is.null(px) || is.null(files)) stop("`weather` must come from ak_weather()")
  yrs <- unique(daymet$year)
  melt_pixel <- \(p) readRDS(files[[p]]) |>
    dplyr::filter(.data$year %in% yrs, .data$yday <= max_doy) |>
    dplyr::summarise(melt_doy = if (max(.data$swe) < swe_min) NA_real_ else {
      k <- which.max(.data$swe)
      .data$yday[k - 1 + match(TRUE, .data$swe[k:dplyr::n()] < swe_min)]
    }, .by = "year")
  dplyr::distinct(px, .data$pixel) |>
    dplyr::mutate(d = purrr::map(.data$pixel, melt_pixel)) |>
    tidyr::unnest("d") |>
    dplyr::inner_join(dplyr::select(px, "site_code", "pixel"), by = "pixel", relationship = "many-to-many") |>
    dplyr::summarise(melt_doy = stats::median(.data$melt_doy, na.rm = TRUE), .by = c("site_code", "year")) |>
    dplyr::mutate(melt_doy = dplyr::if_else(is.nan(.data$melt_doy), NA_real_, .data$melt_doy),
                  melt_anom = .data$melt_doy - mean(.data$melt_doy[.data$year %in% baseline_years], na.rm = TRUE),
                  .by = "site_code") |>
    dplyr::arrange(.data$site_code, .data$year)
}

weather_anomalies <- function(daymet, obs = NULL, baseline_years = 1991:2020, melt = NULL, se = TRUE) {
  an <- daymet |>
    dplyr::select("site_code", "year", "yday", "tmean", "prcp") |>
    dplyr::arrange(.data$site_code, .data$year, .data$yday) |>
    tidyr::nest(.by = "site_code") |>
    dplyr::mutate(data = purrr::map(.data$data, \(d) anomaly_series(d, baseline_years))) |>
    tidyr::unnest("data") |>
    dplyr::select("site_code", "year", "yday", "t_anom", dplyr::all_of(anomaly_cols))
  if (is.null(obs)) return(an)
  out <- obs |>
    dplyr::distinct(.data$site_code, .data$sample_date) |>
    dplyr::mutate(sample_date = as.Date(.data$sample_date),
                  year = as.integer(format(.data$sample_date, "%Y")), yday = as.integer(format(.data$sample_date, "%j"))) |>
    dplyr::left_join(dplyr::select(an, -"t_anom"), by = c("site_code", "year", "yday"))
  if (se && !is.null(attr(daymet, "pixel_files"))) {
    out <- dplyr::left_join(out, anomaly_sampling_se(daymet, out, baseline_years), by = c("site_code", "year", "yday"))
  }
  if (!is.null(melt)) {
    out <- dplyr::left_join(out, melt, by = c("site_code", "year")) |>
      dplyr::mutate(days_since_melt = .data$yday - .data$melt_doy)
  }
  dplyr::select(out, -"year", -"yday")
}

anomaly_cols <- c("t_anom7", "t_anom30", "t_anom7_dt", "t_anom30_dt", "p_anom7", "p_anom30")

# Anomalies of one daily series (year, yday, tmean, prcp; in date order): harmonic climatology on the
# baseline years, linear detrending, trailing 7- and 30-day windows
anomaly_series <- function(d, baseline_years) {
  harm <- \(yday) cbind(sin(2 * pi * yday / 365), cos(2 * pi * yday / 365), sin(4 * pi * yday / 365),
                        cos(4 * pi * yday / 365), sin(6 * pi * yday / 365), cos(6 * pi * yday / 365))
  roll_sum <- \(x, k) as.numeric(stats::filter(x, rep(1, k), sides = 1))
  base <- d$year %in% baseline_years
  if (!any(base)) stop("`daymet` has none of the baseline years")
  d$t_anom <- d$tmean - stats::predict(stats::lm(tmean ~ harm(yday), d[base, ]), d)
  d$time <- d$year + d$yday / 365
  d$lp7 <- log1p(roll_sum(d$prcp, 7))
  d$lp30 <- log1p(roll_sum(d$prcp, 30))
  d$t_anom_dt <- d$t_anom - stats::predict(stats::lm(t_anom ~ time, d), d)
  d |>
    dplyr::mutate(t_anom7 = roll_sum(.data$t_anom, 7) / 7, t_anom30 = roll_sum(.data$t_anom, 30) / 30,
                  t_anom7_dt = roll_sum(.data$t_anom_dt, 7) / 7, t_anom30_dt = roll_sum(.data$t_anom_dt, 30) / 30,
                  p_anom7 = .data$lp7 - stats::predict(stats::lm(lp7 ~ harm(yday), d[base, ]), d),
                  p_anom30 = .data$lp30 - stats::predict(stats::lm(lp30 ~ harm(yday), d[base, ]), d))
}

# Point-sampling SE of each anomaly on the requested site-days, from the anomalies of the individual pixels
anomaly_sampling_se <- function(daymet, keys, baseline_years) {
  px <- attr(daymet, "pixels")
  files <- attr(daymet, "pixel_files")
  n_total <- attr(daymet, "npix_total")
  yrs <- unique(daymet$year)
  days <- dplyr::distinct(keys, .data$year, .data$yday)
  px <- px[px$site_code %in% keys$site_code, c("site_code", "pixel")]
  pa <- purrr::map(unique(px$pixel), \(p) readRDS(files[[p]]) |>
                     dplyr::filter(.data$year %in% yrs) |>
                     dplyr::arrange(.data$year, .data$yday) |>
                     dplyr::mutate(tmean = (.data$tmin + .data$tmax) / 2) |>
                     anomaly_series(baseline_years) |>
                     dplyr::inner_join(days, by = c("year", "yday")) |>
                     dplyr::select("year", "yday", dplyr::all_of(anomaly_cols)) |>
                     dplyr::mutate(pixel = p)) |>
    purrr::list_rbind()
  dplyr::distinct(keys, .data$site_code, .data$year, .data$yday) |>
    dplyr::inner_join(px, by = "site_code", relationship = "many-to-many") |>
    dplyr::inner_join(pa, by = c("pixel", "year", "yday")) |>
    dplyr::summarise(n = dplyr::n(), dplyr::across(dplyr::all_of(anomaly_cols), stats::sd), .by = c("site_code", "year", "yday")) |>
    dplyr::mutate(fpc = sqrt(pmax(0, 1 - .data$n / dplyr::coalesce(unname(n_total[.data$site_code]), Inf))),
                  dplyr::across(dplyr::all_of(anomaly_cols), \(v) v / sqrt(.data$n) * .data$fpc, .names = "{.col}_se"),
                  weather_flag = flag_text(few_pixels = .data$n < 3)) |>
    dplyr::select("site_code", "year", "yday", dplyr::ends_with("_se"), "weather_flag")
}

snowoff_fill <- function(modis, melt) {
  s <- dplyr::select(melt, "site_code", "year", daymet_doy = "melt_doy") |>
    dplyr::full_join(modis, by = c("site_code", "year"))
  overlap <- dplyr::filter(s, !is.na(.data$modis_doy), !is.na(.data$daymet_doy))
  if (!nrow(overlap)) stop("No site-years with both MODIS and Daymet melt to calibrate the offset")
  cal <- overlap |>
    dplyr::summarise(n = dplyr::n(), m = mean(.data$modis_doy - .data$daymet_doy),
                     v = stats::var(.data$modis_doy - .data$daymet_doy), .by = "site_code")
  s2 <- stats::weighted.mean(cal$v, cal$n - 1, na.rm = TRUE)
  if (!is.finite(s2)) s2 <- stats::var(overlap$modis_doy - overlap$daymet_doy)   # no site has two overlap years
  mu <- stats::weighted.mean(cal$m, cal$n)
  tau2 <- max(stats::var(cal$m) - mean(s2 / cal$n), 0, na.rm = TRUE)
  shrink <- (s2 / cal$n) / (tau2 + s2 / cal$n)                                   # weight on the all-site mean
  cal$snowoff_offset <- mu + (cal$m - mu) * (1 - shrink)
  var_mu <- sum(cal$n^2 * (tau2 + s2 / cal$n)) / sum(cal$n)^2
  cal$offset_var <- (if (tau2 > 0) 1 / (1 / tau2 + cal$n / s2) else 0) + shrink^2 * var_mu
  resid <- overlap |>
    dplyr::left_join(cal[c("site_code", "snowoff_offset")], by = "site_code") |>
    dplyr::mutate(e = .data$modis_doy - (.data$daymet_doy + .data$snowoff_offset))
  resid_sd <- stats::sd(resid$e)
  slope <- if (dplyr::n_distinct(overlap$site_code) > 1) {
    stats::coef(stats::lm(modis_doy ~ daymet_doy + site_code, overlap))[["daymet_doy"]]
  } else stats::coef(stats::lm(modis_doy ~ daymet_doy, overlap))[["daymet_doy"]]
  rmse <- stats::median(modis$snowoff_rmse_d, na.rm = TRUE)
  out <- s |>
    dplyr::left_join(cal[c("site_code", "snowoff_offset", "offset_var")], by = "site_code") |>
    dplyr::mutate(snowoff_offset = dplyr::coalesce(.data$snowoff_offset, mu),
                  offset_var = dplyr::coalesce(.data$offset_var, tau2 + var_mu),
                  snowoff_doy = round(dplyr::coalesce(.data$modis_doy, .data$daymet_doy + .data$snowoff_offset)),
                  snowoff_source = dplyr::case_when(!is.na(.data$modis_doy) ~ "modis", !is.na(.data$daymet_doy) ~ "daymet_offset"),
                  snowoff_se_d = dplyr::case_when(.data$snowoff_source == "modis" ~ dplyr::coalesce(.data$snowoff_rmse_d, rmse),
                                                  .data$snowoff_source == "daymet_offset" ~ sqrt(rmse^2 + s2 + .data$offset_var))) |>
    dplyr::select(-"offset_var") |>
    dplyr::arrange(.data$site_code, .data$year)
  attr(out, "calibration") <- c(n_site_years = nrow(overlap), n_sites = nrow(cal), mean_offset_d = mu,
                                between_site_sd_d = sqrt(tau2), within_site_sd_d = sqrt(s2), residual_sd_d = resid_sd,
                                within_site_slope = slope)
  out
}

climate_summaries <- function(daymet, obs, snowoff = NULL, t_snow = -1, t_rain = 3, gdd_bases = c(0, 5),
                                 summer_months = 6:8, snow_year_start = 10) {
  mon_of <- \(d) as.integer(format(d, "%m"))
  yr_of <- \(d) as.integer(format(d, "%Y"))
  d <- daymet |>
    dplyr::mutate(snow_frac = dplyr::case_when(.data$tmean <= t_snow ~ 1, .data$tmean >= t_rain ~ 0,
                                               .default = (t_rain - .data$tmean) / (t_rain - t_snow)),
                  swe_mm = .data$prcp * .data$snow_frac,
                  depth_cm = .data$swe_mm * (1000 / pmin(pmax(67.92 + 51.25 * exp(.data$tmean / 2.59), 50), 200)) / 10,
                  snow_yr = dplyr::if_else(mon_of(.data$date) >= snow_year_start, yr_of(.data$date) + 1L, yr_of(.data$date)))
  snow <- d |>
    dplyr::summarise(snowfall_depth_cm = sum(.data$depth_cm, na.rm = TRUE), snowfall_swe_mm = sum(.data$swe_mm, na.rm = TRUE),
                     n_days = dplyr::n(), .by = c("site_code", "snow_yr")) |>
    dplyr::filter(.data$n_days > 350) |>
    dplyr::select(-"n_days", year = "snow_yr")
  summer <- d |>
    dplyr::filter(mon_of(.data$date) %in% summer_months) |>
    dplyr::summarise(summer_tmean_c = mean(.data$tmean, na.rm = TRUE), .by = c("site_code", "year"))
  o <- obs |>
    dplyr::distinct(.data$site_code, .data$sample_date) |>
    dplyr::mutate(sample_date = as.Date(.data$sample_date), year = yr_of(.data$sample_date), doy = as.integer(format(.data$sample_date, "%j")))
  if (is.null(snowoff)) {
    o$snowoff_doy <- NA_real_
    o$snowoff_se_d <- NA_real_
  } else {
    o <- dplyr::left_join(o, dplyr::select(snowoff, "site_code", "year", "snowoff_doy", "snowoff_source", dplyr::any_of("snowoff_se_d")),
                          by = c("site_code", "year"))
    if (!"snowoff_se_d" %in% names(o)) o$snowoff_se_d <- NA_real_
  }
  dd <- o |>
    dplyr::left_join(dplyr::select(d, "site_code", "year", doy_day = "yday", "tmean"), by = c("site_code", "year"),
                     relationship = "many-to-many") |>
    dplyr::filter(.data$doy_day <= .data$doy) |>
    dplyr::summarise(tdd = sum(pmax(.data$tmean, 0), na.rm = TRUE), fdd = sum(pmin(.data$tmean, 0), na.rm = TRUE),
                     tdd_snowoff = sum(pmax(.data$tmean[.data$doy_day >= dplyr::first(.data$snowoff_doy)], 0)),
                     tdd_snowoff_se = tdd_shift_se(.data$tmean, .data$doy_day, dplyr::first(.data$snowoff_doy), dplyr::first(.data$snowoff_se_d)),
                     !!!stats::setNames(lapply(gdd_bases, \(b) rlang::expr(sum(pmax(.data$tmean - !!b, 0), na.rm = TRUE))),
                                        paste0("gdd_base", gdd_bases)),
                     .by = c("site_code", "sample_date"))
  out <- o |>
    dplyr::left_join(dd, by = c("site_code", "sample_date")) |>
    dplyr::mutate(days_since_snowoff = .data$doy - .data$snowoff_doy) |>
    dplyr::left_join(snow, by = c("site_code", "year")) |>
    dplyr::left_join(summer, by = c("site_code", "year")) |>
    dplyr::relocate(dplyr::starts_with("gdd_base"), .after = "doy")
  out <- dplyr::relocate(out, "tdd_snowoff_se", .after = "tdd_snowoff") |> dplyr::select(-dplyr::any_of("snowoff_se_d"))
  if (is.null(snowoff)) out <- dplyr::select(out, -"snowoff_doy", -"tdd_snowoff", -"tdd_snowoff_se", -"days_since_snowoff")
  out
}

# SE of thawing degree days since snow-off from the SE of the snow-off day (delta method): the mean thawing
# temperature over the days within one SE of snow-off, times the SE; 0 if all of them fall after the sample
tdd_shift_se <- function(tmean, doy, snowoff, se) {
  if (is.na(snowoff) || is.na(se)) return(NA_real_)
  w <- doy >= snowoff - se & doy <= snowoff + se
  if (!any(w)) return(0)
  se * mean(pmax(tmean[w], 0), na.rm = TRUE)
}

#' Snow-off date of each catchment and year
#'
#' The day snow cover ends in each catchment, from MODIS where it is
#' observed and, with `fill = TRUE`, from Daymet snow water equivalent
#' calibrated to MODIS where it is not (before 2001, after the MODIS record
#' ends, and in cloudy or partial catchment-years). Returns a table, one row
#' per site and year; pass it to [weather_at_samples()] for days and thawing
#' degree days since snow-off.
#'
#' For the fill, `weather` (from [ak_weather()]) must cover the baseline
#' years and the years to fill; the melt day of each Daymet pixel is the
#' first day after peak SWE (January to day `max_doy`) on which SWE falls
#' below `swe_min` kg/m2, and the catchment melt day is the median over its
#' pixels.
#'
#' @section MODIS snow-off:
#' MODIS snow-off date per catchment and year.
#'
#' Median catchment melt-out (end of the longest continuous snow season,
#' band 5 of the NPS/GINA MODIS snow metrics; Zhu et al. 2025, Arctic Data
#' Center doi:10.18739/A2057CV28, snow years 2001-2022, 500 m), with the
#' valid share of the catchment. Each annual file (about 740 MB) is
#' downloaded once and cropped to the catchments plus 1 km.
#'
#' Uncertainty: compared with SNOTEL, MODIS snow-off has MAE of 6, 12 and 14
#' days and RMSE of 8, 14 and 14 days in Interior, Western and North Slope
#' Alaska (Pan et al. 2020); it runs early, more so under cloud (Lindsay et
#' al. 2015). `snowoff_rmse_d` carries the regional RMSE you pass.
#' `modis_cloud_days` is the catchment mean of the product's cloud-days band
#' (band 11), the main driver of that error. `snowoff_flag`: `low_valid`
#' (valid share below `min_valid_frac`, so `modis_doy` is `NA`) and `cloudy`
#' (cloud days above `cloud_days_max`; by default the 90th percentile of the
#' site-years returned).
#'
#'
#' @section Filling from Daymet (`fill = TRUE`):
#' Snow-off date per catchment and year: MODIS, filled from Daymet.
#'
#' Uses the MODIS snow-off date where it exists, and
#' otherwise the Daymet melt day plus a site offset
#' (MODIS minus Daymet). The offset is estimated on the years with both and
#' shrunk toward the all-site mean by empirical Bayes, so sites with few
#' overlap years borrow strength and sites with none get the mean. This fills
#' years without MODIS (2023 on) and catchment-years with too little valid
#' MODIS.
#'
#' Uncertainty: `snowoff_se_d` is the SE of the date as an estimate of the
#' ground snow-off date, for every row.
#' * MODIS rows: the published MODIS RMSE against SNOTEL
#'   (`snowoff_rmse_d`).
#' * Filled rows: sqrt(RMSE^2 + s^2 + v), where s is the within-site SD of
#'   MODIS minus Daymet across overlap years (how well Daymet + offset
#'   predicts the MODIS date in a new year) and v the variance of the site's
#'   offset (small for sites with many overlap years; the between-site
#'   variance for sites with none). Adding the MODIS RMSE assumes the fill's
#'   departure from MODIS is independent of MODIS's departure from the
#'   ground, which is conservative: if Daymet's and MODIS's errors are
#'   independent, the fill's error against the ground is smaller (see
#'   `vignette("snowoff-modis")`).
#'
#' The calibration (mean offset, between-site SD, within-site SD, residual
#' SD, within-site slope of MODIS on Daymet, where 1 means an offset is
#' enough) is attached as attribute `calibration`.
#'
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param years MODIS snow years (2001-2022 available).
#' @param weather Daily weather from [ak_weather()], for the fill.
#' @param fill Fill years and catchment-years without MODIS from Daymet?
#' @param baseline Years defining each site's normal Daymet melt day.
#' @param min_valid_frac Minimum valid MODIS share for a catchment-year value.
#' @param rmse_d Regional MODIS RMSE against SNOTEL (days) to attach.
#' @param cloud_days_max Cloud-day threshold for the `cloudy` flag.
#' @param keep_full Keep each statewide annual MODIS file (about 740 MB) in
#'   the cache's `modis_snow/full` folder, so later calls for other areas crop
#'   it instead of downloading it again. Worth it for a shared cache.
#' @param max_doy,swe_min Daymet melt day: last day of year searched, and the
#'   SWE threshold for "melted", kg/m2.
#' @return A table, one row per site and year: `site_code`, `year`,
#'   `modis_doy`, `modis_valid_frac`, `modis_cloud_days`, `snowoff_rmse_d`,
#'   `snowoff_flag`; with `fill`, also `daymet_doy`, `snowoff_offset`,
#'   `snowoff_doy`, `snowoff_source` (`modis` or `daymet_offset`) and
#'   `snowoff_se_d`, and the calibration as attribute `calibration`. With its
#'   provenance.
#' @examples
#' \dontrun{
#' w <- ak_weather(x, years = 1990:2024)
#' s <- ak_snowoff(x, years = 2001:2022, weather = w)
#' }
#' @seealso `vignette("snowoff-modis")`
#' @export
ak_snowoff <- function(catchments, years = 2001:2022, weather = NULL, fill = TRUE, baseline = 1991:2020,
                       min_valid_frac = 0.5, rmse_d = 8, cloud_days_max = NULL, keep_full = FALSE, max_doy = 212, swe_min = 1) {
  if (fill && is.null(weather)) {
    stop("fill = TRUE needs the daily weather: weather = ak_weather(x, years = ", min(baseline), ":", max(years),
         ") (or fill = FALSE for MODIS only)", call. = FALSE)
  }
  x <- as_catchments(catchments)
  modis <- snowoff_modis(x, years, min_valid_frac, rmse_d, cloud_days_max, keep_full)
  args <- list(years = range(years), fill = fill, baseline = range(baseline), min_valid_frac = min_valid_frac, rmse_d = rmse_d)
  if (!fill) return(as_layer_table(modis, layer_row("snowoff", fn = "ak_snowoff"), args))
  out <- snowoff_fill(modis, melt_daymet(weather, baseline, max_doy, swe_min))
  attr(out, "ak_provenance") <- ak_provenance(weather)
  as_layer_table(out, layer_row("snowoff", fn = "ak_snowoff"), args)
}

