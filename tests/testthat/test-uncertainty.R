sq <- function(x, y, w, h = w) sf::st_polygon(list(rbind(c(x, y), c(x + w, y), c(x + w, y + h), c(x, y + h), c(x, y))))

test_that("class shares work on rasters and polygons", {
  catch <- sf::st_sf(site_code = c("a", "b"), geometry = sf::st_sfc(sq(0, 0, 1000), sq(2000, 0, 1000), crs = 3338))
  r <- terra::rast(terra::ext(-100, 3100, -100, 1100), resolution = 10, crs = "EPSG:3338")
  terra::values(r) <- ifelse(terra::crds(r)[, 1] < 500, 41, ifelse(terra::crds(r)[, 1] < 2500, 52, 0))
  cs <- catchment_class_shares(catch, r, prefix = "nlcd_", labels = c(`41` = "deciduous", `52` = "shrub"))
  expect_equal(cs$nlcd_deciduous, c(50, 0), tolerance = 1e-6)
  expect_equal(cs$nlcd_shrub, c(50, 100), tolerance = 1e-6)
  expect_equal(cs$nlcd_coverage_pct, c(100, 50), tolerance = 1e-6)
  m <- sf::st_sf(cls = c("x", "y"), geometry = sf::st_sfc(sq(-100, -100, 600, 1200), sq(500, -100, 3000, 1200), crs = 3338))
  cp <- catchment_class_shares(catch, m, "cls", prefix = "m_")
  expect_equal(cp$m_x, c(50, 0), tolerance = 1e-6)
})

test_that("positional uncertainty grows with sigma and vanishes for uniform maps", {
  catch <- sf::st_sf(site_code = "a", geometry = sf::st_sfc(sq(0, 0, 1000), crs = 3338))
  m <- sf::st_sf(cls = c("x", "y"), geometry = sf::st_sfc(sq(-5000, -5000, 5500, 11000), sq(500, -5000, 5500, 11000), crs = 3338))
  small <- positional_uncertainty(catch, m, "cls", sigma_m = 10, n = 50)
  big <- positional_uncertainty(catch, m, "cls", sigma_m = 300, n = 50)
  expect_lt(small$pct_sd[small$class == "x"], big$pct_sd[big$class == "x"])
  expect_equal(mean(big$pct_mean[big$class == "x"]), 50, tolerance = 10)
  uni <- sf::st_sf(cls = "x", geometry = sf::st_sfc(sq(-5000, -5000, 11000), crs = 3338))
  expect_equal(positional_uncertainty(catch, uni, "cls", 300, n = 20)$pct_sd, 0)
})

test_that("area adjustment recovers shares under a perfect map and shifts under a biased one", {
  cls <- c("forest", "shrub")
  perfect <- matrix(c(50, 0, 0, 50), 2, dimnames = list(cls, cls))
  a1 <- adjust_class_shares(c(forest = 70, shrub = 30), perfect)
  expect_equal(a1$pct_adjusted, c(70, 30))
  expect_equal(a1$se, c(0, 0))
  biased <- matrix(c(40, 0, 10, 50), 2, dimnames = list(cls, cls))
  a2 <- adjust_class_shares(c(forest = 70, shrub = 30), biased)
  expect_equal(a2$pct_adjusted, c(56, 44))
  expect_true(all(a2$se > 0))
})

test_that("alder error follows the published scale dependence", {
  se <- AKCatchments:::alder_se(c(1e-5, 1, 100, 1e6))
  expect_equal(se[c(1, 3, 4)], c(10.5, 4.9, 1.4))
  expect_true(se[2] < 10.5 && se[2] > 4.9)
  ci <- AKCatchments:::beta_interval(c(0, 5, 50, 95), c(5, 5, 5, 5))
  expect_true(all(ci$lo >= 0 & ci$hi <= 100 & ci$lo <= ci$hi))
  expect_equal(c(ci$lo[3], ci$hi[3]), c(50 - 1.96 * 5, 50 + 1.96 * 5), tolerance = 0.01)   # mid-range: about normal
  expect_false(ci$feasible[1])                                   # 0 % cover with SE 5: no Beta; normal interval cut at 0
  low <- AKCatchments:::beta_interval(2, 5.8)                    # 2 % cover, SE 5.8 (a ~10 km2 catchment)
  expect_lt(low$hi, 30)                                          # a logit delta-method interval reached about 87 %
  expect_gt(low$hi, 2 + 5.8)
  draws <- stats::rbeta(2e5, 0.02 * (0.02 * 0.98 / 0.058^2 - 1), 0.98 * (0.02 * 0.98 / 0.058^2 - 1))
  expect_equal(stats::sd(draws) * 100, 5.8, tolerance = 0.02)    # the Beta has the SE as its SD
})

test_that("permafrost shares and total carry class-range and positional error", {
  catch <- sf::st_sf(site_code = c("a", "b"), geometry = sf::st_sfc(sq(0, 0, 1000), sq(10000, 0, 1000), crs = 3338))
  m <- sf::st_sf(GEOL = "x", EXTENT = c("Continuous", "Sporadic (10-50%)", "Glacier"), GRND_ICE = c("High", "Low", "None"),
                 geometry = sf::st_sfc(sq(-3000, -3000, 3500, 7000), sq(500, -3000, 3500, 7000), sq(8000, -3000, 6000, 7000), crs = 3338))
  path <- file.path(tempfile("jorg_"), "pf.shp")
  dir.create(dirname(path))
  sf::st_write(m, path, quiet = TRUE)
  p0 <- ak_permafrost(catch, path = dirname(path), sigma_m = 0, n = 400)
  expect_equal(p0$jorg_pct_continuous, c(50, 0), tolerance = 1e-6)
  expect_equal(p0$jorg_pct_glacier, c(0, 100), tolerance = 1e-6)
  expect_equal(p0$totperm_pct, c(50 * 0.95 + 50 * 0.3, 0), tolerance = 1e-6)
  expect_gt(p0$totperm_pct_se[1], 0)
  expect_equal(p0$totperm_pct_se[2], 0)
  expect_true(all(c("groundice_pct_high", "groundice_pct_low") %in% names(p0)))
  p1 <- ak_permafrost(catch, path = dirname(path), sigma_m = 300, n = 100)
  expect_gt(p1$totperm_pct_se[1], p0$totperm_pct_se[1])
  expect_equal(p0$totperm_pct_flag[1], "")
  expect_true("totperm_pct_flag" %in% names(p1))
})

test_that("the source registry documents every layer function", {
  s <- AKCatchments:::sources_table()
  fns <- unlist(strsplit(s$fn, "; "))
  expect_true(all(fns %in% getNamespaceExports("AKCatchments")))
  expect_true(all(nzchar(s$uncertainty_method)))
})

test_that("anomaly SEs are the between-pixel spread of pixel anomalies, far below the daily SE", {
  dir <- tempfile(); dir.create(dir)
  d <- tidyr::expand_grid(year = 1991:2010, yday = 1:365)
  set.seed(3)
  wx <- 5 - 20 * cos(2 * pi * (d$yday - 15) / 365) + stats::rnorm(nrow(d), 0, 3)        # shared weather
  px <- lapply(c(-2, 0, 3), \(off) dplyr::mutate(d, tmax = wx + off + 4 + stats::rnorm(nrow(d), 0, 0.5), tmin = tmax - 8,
                                               prcp = stats::rexp(nrow(d)) * (1 + off / 10), swe = 0))
  files <- stats::setNames(file.path(dir, paste0("p", 1:3, ".rds")), paste0("p", 1:3))
  for (i in 1:3) saveRDS(px[[i]], files[i])
  m <- dplyr::bind_rows(px, .id = "k") |> dplyr::mutate(tmean = (tmin + tmax) / 2) |>
    dplyr::summarise(tmean_sd = stats::sd(tmean), n_pixels = dplyr::n(), dplyr::across(c("tmin", "tmax", "tmean", "prcp", "swe"), mean),
                     .by = c("year", "yday")) |>
    dplyr::mutate(site_code = "A", tmean_se_sampling = tmean_sd / sqrt(3) * sqrt(1 - 3 / 10),
                  date = as.Date(paste0(year, "-01-01")) + (yday - 1))
  attr(m, "pixels") <- data.frame(site_code = "A", pixel = names(files))
  attr(m, "pixel_files") <- files
  attr(m, "npix_total") <- c(A = 10)
  obs <- data.frame(site_code = "A", sample_date = as.Date(c("2005-07-10", "2009-08-02")))
  an <- AKCatchments:::weather_anomalies(m, obs)
  expect_true(all(c("t_anom7_se", "t_anom30_dt_se", "p_anom30_se", "weather_flag") %in% names(an)))
  # pixel anomalies by hand: catchment anomaly is their mean, SE their SD / sqrt(3) x sqrt(1 - 3/10)
  pa <- sapply(px, \(p) {
    a <- AKCatchments:::anomaly_series(dplyr::mutate(p, tmean = (tmin + tmax) / 2), 1991:2020)
    a$t_anom7[a$year == 2005 & a$yday == as.integer(format(as.Date("2005-07-10"), "%j"))]
  })
  expect_equal(an$t_anom7[1], mean(pa), tolerance = 1e-8)
  expect_equal(an$t_anom7_se[1], stats::sd(pa) / sqrt(3) * sqrt(0.7), tolerance = 1e-8)
  daily_se <- m$tmean_se_sampling[m$year == 2005 & m$yday == 191]
  expect_lt(an$t_anom7_se[1], daily_se / 5)       # persistent pixel offsets cancel in anomalies
  expect_equal(an$weather_flag, c("", ""))
  expect_false("t_anom7_se" %in% names(AKCatchments:::weather_anomalies(m, obs, se = FALSE)))
})

test_that("filled snow-off dates carry the MODIS error, the year-to-year scatter and the offset's uncertainty", {
  melt <- tidyr::expand_grid(site_code = c("A", "B", "C"), year = 2001:2024) |>
    dplyr::mutate(melt_doy = 130 + (year %% 5) + (site_code == "B") * 4)
  set.seed(4)
  modis <- melt |> dplyr::filter(site_code != "C", year <= 2022) |>
    dplyr::transmute(site_code, year, modis_doy = melt_doy + 5 + 6 * (site_code == "B") + stats::rnorm(dplyr::n(), 0, 3),
                     modis_valid_frac = 1, snowoff_rmse_d = 8)
  so <- AKCatchments:::snowoff_fill(modis, melt)
  cal <- attr(so, "calibration")
  expect_true(all(so$snowoff_se_d[so$snowoff_source == "modis"] == 8))
  fill <- so[so$snowoff_source == "daymet_offset", ]
  expect_true(all(fill$snowoff_se_d > 8))
  expect_gt(fill$snowoff_se_d[fill$site_code == "C"][1], fill$snowoff_se_d[fill$site_code == "A"][1])   # C has no overlap years
  expect_gt(cal[["between_site_sd_d"]], 0)
  expect_equal(fill$snowoff_se_d[fill$site_code == "A"][1]^2, 8^2 + cal[["within_site_sd_d"]]^2, tolerance = 0.05)
  # the SE carries into thawing degree days since snow-off
  dm <- tidyr::expand_grid(site_code = "C", year = 2023, yday = 1:365) |>
    dplyr::mutate(tmean = 10 * sin(pi * (yday - 90) / 220), prcp = 0, date = as.Date("2023-01-01") + yday - 1)
  cl <- AKCatchments:::climate_summaries(dm, data.frame(site_code = "C", sample_date = as.Date("2023-07-15")), snowoff = so)
  so_c <- so[so$site_code == "C" & so$year == 2023, ]
  w <- dm$yday >= so_c$snowoff_doy - so_c$snowoff_se_d & dm$yday <= so_c$snowoff_doy + so_c$snowoff_se_d
  expect_equal(cl$tdd_snowoff_se, so_c$snowoff_se_d * mean(pmax(dm$tmean[w], 0)))
  expect_false("snowoff_se_d" %in% names(cl))
})

test_that("the Olofsson SE needs at least two samples per map class, and the soil L-H range is an assumption", {
  em <- matrix(c(1, 0, 0, 5), 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_error(adjust_class_shares(c(a = 50, b = 50), em), "at least 2")
  co <- data.frame(site_code = "A", mukey = "1", w = 1, comppct_l = c(50, 20), comppct_r = c(70, 30), comppct_h = c(85, 45),
                   taxorder = c("Gelisols", "Inceptisols"), drainagecl = NA)
  se90 <- AKCatchments:::soil_share_draws(co, n = 2000, lh_range = 0.9)$soil_gelisol_pct_se
  se80 <- AKCatchments:::soil_share_draws(co, n = 2000, lh_range = 0.8)$soil_gelisol_pct_se
  expect_gt(se80, se90)
  expect_equal(se80 / se90, stats::qnorm(0.95) / stats::qnorm(0.9), tolerance = 0.1)
})

test_that("the Daymet catchment mean carries a between-pixel SD and sampling SE", {
  d <- tidyr::expand_grid(year = 2019:2020, yday = 1:365)
  px <- lapply(c(0, 2, 4), \(off) dplyr::mutate(d, tmax = 10 + off, tmin = 0 + off, prcp = 1 + off, swe = 0))
  m <- AKCatchments:::daymet_site_mean(px, "A", 2019:2020, n_total = 12)
  expect_equal(unique(m$tmean), 7)
  expect_equal(unique(m$tmean_sd), 2)                              # was NA: sd() saw the already averaged column
  expect_equal(unique(m$tmean_se_sampling), 2 / sqrt(3) * sqrt(1 - 3 / 12))
  expect_equal(unique(m$prcp_se_sampling), 2 / sqrt(3) * sqrt(1 - 3 / 12))
  expect_setequal(names(m), c("site_code", "tmean_se_sampling", "prcp_se_sampling", "daymet_flag", "year", "yday", "tmin", "tmax",
                              "tmean", "prcp", "swe", "tmean_sd", "prcp_sd", "n_pixels"))
})

test_that("the small-glacier flag uses the glacier pieces, not their sum", {
  sq <- \(x0, y0, s) sf::st_polygon(list(cbind(c(x0, x0 + s, x0 + s, x0, x0), c(y0, y0, y0 + s, y0 + s, y0))))
  catch <- sf::st_sf(site_code = c("A", "B"), geometry = sf::st_sfc(sq(0, 0, 4000), sq(10000, 0, 4000), crs = 3338))
  gl <- sf::st_sf(id = 1:4, geometry = sf::st_sfc(sq(100, 100, 500), sq(1000, 1000, 500),        # A: two small glaciers
                                                  sq(10100, 100, 500), sq(11000, 1000, 2000),   # B: one small, one large
                                                  crs = 3338))
  dir <- tempfile(); dir.create(dir); sf::st_write(gl, file.path(dir, "rgi.shp"), quiet = TRUE)
  g <- ak_glaciers(catch, dir, sigma_m = 0)
  expect_equal(g$glacier_flag, c("small_glaciers", ""))
})
