# A fake Landsat archive: point NDVI follows a seasonal curve peaking at day 200, with a known per-point peak that
# rises by 0.002 a year. Landsat 5 reads 0.03 low and Landsat 8 0.02 high + 3% (sensor offsets to calibrate away).
dn <- \(sr) round((sr + 0.2) / 0.0000275)
true_peak <- \(pid, year) 0.45 + 0.3 * ((as.integer(sub(".*_p", "", pid)) %% 7) / 6) + 0.002 * (year - 2000)
sensor_view <- \(v, platform) switch(platform, `landsat-5` = v - 0.03, `landsat-8` = 0.02 + 1.03 * v, v)

fake_landsat <- function() {
  reads <- 0
  search <- function(bb, year, doy, max_cloud, tiers) {
    plats <- c(if (year <= 2011) "landsat-5", "landsat-7", if (year >= 2013) "landsat-8")
    days <- seq(152 + (year * 3) %% 8, 240, by = 8)      # acquisition days shift from year to year, as orbits do
    purrr::map(seq_along(days), \(i) {
      pl <- plats[(i %% length(plats)) + 1]
      list(id = sprintf("%s_%d_%03d", pl, year, days[i]), date = as.Date(paste0(year, "-01-01")) + days[i] - 1, platform = pl,
           category = "T1", cloud_cover = 20, sun_elevation = 45, bbox = c(-180, 0, 0, 90), href = rep("x", 4))
    })
  }
  read <- function(it, gp) {
    reads <<- reads + 1
    doy <- as.integer(format(it$date, "%j"))
    yr <- as.integer(format(it$date, "%Y"))
    v <- true_peak(gp$point_id, yr) * exp(-((doy - 200) / 45)^2)
    v <- sensor_view(v, it$platform)
    nir <- 0.3
    red <- nir * (1 - v) / (1 + v)
    cloudy <- (seq_along(v) + doy) %% 9 == 0
    dplyr::tibble(site_code = gp$site_code, point_id = gp$point_id, item = it$id, date = it$date, year = yr,
                  platform = it$platform, category = "T1", cloud_cover = 20, sun_elevation = 45,
                  red = dn(red), nir = dn(nir), qa_pixel = ifelse(cloudy, 64L + 8L, 64L), qa_radsat = 0L)
  }
  list(search = search, read = read, reads = \() reads)
}

test_that("screening keeps clear pixels only and computes NDVI from scaled reflectance", {
  o <- dplyr::tibble(qa_pixel = c(64L, 72L, 96L, 65L, 64L, 192L, 66L), qa_radsat = c(0L, 0L, 0L, 0L, 1L, 0L, 0L),
                     cloud_cover = 10, sun_elevation = 40, red = dn(0.05), nir = dn(0.35), platform = "landsat-8",
                     date = as.Date("2010-07-01"))
  k <- lsat_clean(o)
  expect_equal(nrow(k), 1)                         # cloud, snow, fill, saturated, water, dilated cloud all removed
  expect_equal(k$ndvi, 0.3 / 0.4, tolerance = 1e-4)
  expect_equal(k$sensor, "L8")
})

test_that("cross-calibration removes a known sensor offset", {
  set.seed(2)
  pts <- sprintf("s_p%03d", 1:120)
  grid <- tidyr::expand_grid(point_id = pts, year = 2013:2020, doy = seq(160, 240, 8), sensor = c("L7", "L8"))
  base <- runif(length(pts), 0.3, 0.8)[match(grid$point_id, pts)]
  truth <- base * exp(-((grid$doy - 200) / 60)^2)
  obs <- grid |>
    dplyr::mutate(site_code = "s", ndvi = ifelse(.data$sensor == "L8", 0.02 + 1.03 * truth, truth) + rnorm(dplyr::n(), 0, 0.01),
                  truth = truth)
  cal <- lsat_xcal(obs)
  l8 <- cal$obs[cal$obs$sensor == "L8", ]
  expect_lt(abs(mean(l8$ndvi_x - l8$truth)), 0.005)
  expect_gt(abs(mean(l8$ndvi - l8$truth)), 0.02)
  expect_true(cal$eval$calibrated[cal$eval$sensor == "L8"])
  expect_true(all(l8$cal_se > 0 & l8$cal_se < 0.01))
})

test_that("Landsat greenness end to end: peaks, calibration, flags, cache", {
  fk <- fake_landsat()
  local_mocked_bindings(lsat_search = fk$search, lsat_read_item = fk$read)
  sq <- \(x, y, w) sf::st_polygon(list(rbind(c(x, y), c(x + w, y), c(x + w, y + w), c(x, y + w), c(x, y))))
  x <- sf::st_sf(site_code = c("a", "b"), geometry = sf::st_sfc(sq(200000, 1600000, 1000), sq(205000, 1600000, 2000), crs = 3338))
  g <- ak_peak_ndvi(x, years = 2000:2020)
  expect_true(all(c("ndvi_max", "ndvi_max_se", "ndvi_max_lo95", "ndvi_max_hi95", "veg_frac", "flag") %in% names(g)))
  pp <- attr(g, "points")
  err <- pp$ndvi_max - true_peak(pp$point_id, pp$year)
  expect_lt(abs(stats::median(err)), 0.02)           # the peak is recovered from 3-6 clear days a year
  expect_true(all(attr(g, "xcal")$calibrated))
  expect_true(all(g$veg_frac == 1))
  expect_true(all(g$ndvi_max_se > 0))
  expect_true(all(grepl("record_edge", g$flag[g$year %in% c(2000, 2020)])))
  n1 <- fk$reads()
  g2 <- ak_peak_ndvi(x, years = 2000:2020)
  expect_equal(fk$reads(), n1)                        # every scene from the cache the second time
  expect_equal(g2$ndvi_max, g$ndvi_max)
  s <- peak_ndvi_summary(g)
  expect_equal(s$ndvimax_trend_decade, c(0.02, 0.02), tolerance = 0.15)        # within 15% of the truth
  expect_true(all(s$ndvimax_trend_decade_lo95 < 0.02 & s$ndvimax_trend_decade_hi95 > 0.02))  # and the interval covers it
  # without calibration, the sensor offsets bias the trend and the flag says so
  u <- peak_ndvi_summary(ak_peak_ndvi(x, years = 2000:2020, xcal = FALSE))
  expect_match(u$ndvimax_trend_decade_flag[1], "sensor_change")
})

test_that("bare points get no value and lower veg_frac", {
  obs <- tidyr::expand_grid(point_id = c("s_p001", "s_p002"), year = 2005:2012, doy = seq(160, 240, 8)) |>
    dplyr::mutate(site_code = "s", sensor = "L7", calibrated = TRUE, cal_se = 0,
                  ndvi_x = ifelse(.data$point_id == "s_p001", 0.6 * exp(-((.data$doy - 200) / 50)^2), 0.05))
  pp <- lsat_peaks(obs)
  expect_equal(unique(pp$point_id), "s_p001")
  pts <- sf::st_sf(site_code = "s", point_id = c("s_p001", "s_p002"),
                   geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 3338))
  cy <- lsat_catchment_years(pp, pts, 2005:2012)
  expect_equal(unique(cy$veg_frac), 0.5)
  expect_true(all(grepl("few_points", cy$flag)))
})
