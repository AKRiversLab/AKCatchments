sq_catch <- function(ids = "A", d = 1000) {
  sf::st_sf(site_code = ids, geometry = sf::st_sfc(lapply(seq_along(ids), \(i) {
    x0 <- (i - 1) * 5 * d
    sf::st_polygon(list(cbind(c(x0, x0 + d, x0 + d, x0, x0), c(0, 0, d, d, 0))))
  }), crs = 3338))
}

test_that("site_map catches errors per site and names the results", {
  res <- AKCatchments:::site_map(list(a = 1, b = 2, c = 3), \(v, k) if (v == 2) stop("boom") else v * k, k = 10)
  expect_equal(names(res), c("a", "b", "c"))
  expect_equal(AKCatchments:::site_values(res), list(a = 10, c = 30))
  e <- AKCatchments:::site_errors(res, "test")
  expect_equal(e$site_code, "b")
  expect_equal(e$step, "test")
  expect_match(e$error, "boom")
  expect_true(all(vapply(res, \(r) is.numeric(r$seconds), TRUE)))
  expect_equal(AKCatchments:::site_map(list(), identity), list())
})

test_that("site_map gives the same results on mirai daemons as sequentially", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")
  skip_on_cran()
  skip_if(AKCatchments:::dev_loaded(), "parallel needs the installed package")
  x <- sq_catch(c("A", "B", "C"))
  m <- sf::st_sf(cls = c("a", "b"), geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(-5000, 400, 400, -5000, -5000), c(-5000, -5000, 5000, 5000, -5000)))),
    sf::st_polygon(list(cbind(c(400, 20000, 20000, 400, 400), c(-5000, -5000, 5000, 5000, -5000)))), crs = 3338))
  seq_res <- positional_uncertainty(x, m, "cls", sigma_m = 200, n = 50)
  mirai::daemons(2)
  on.exit(mirai::daemons(0), add = TRUE)
  expect_equal(AKCatchments:::parallel_workers(), 2L)
  par_res <- positional_uncertainty(x, m, "cls", sigma_m = 200, n = 50)
  expect_equal(par_res, seq_res)                     # per-site seeds: identical either way
  res <- AKCatchments:::site_map(list(a = 1, b = 2), \(v) if (v == 2) stop("boom") else Sys.getpid())
  expect_match(AKCatchments:::site_errors(res, "x")$error, "boom")
})

test_that("layer functions append to the caller's object, keep its CRS and columns, and refuse duplicates", {
  x <- sf::st_transform(sq_catch(c("A", "B")), 4326)
  x$keep_me <- c("a", "b")
  r <- terra::rast(terra::ext(-1000, 7000, -1000, 2000), resolution = 100, crs = "EPSG:3338", vals = 2)
  y <- catchment_mean(x, r, "v")
  expect_equal(sf::st_crs(y), sf::st_crs(x))
  expect_equal(y$keep_me, c("a", "b"))
  expect_equal(y$v, c(2, 2))
  expect_s3_class(y, "ak_catchments")
  expect_message(y2 <- catchment_mean(y, r * 3, "v"), "Replacing 2 existing")
  expect_equal(y2$v, c(6, 6))
  expect_equal(nrow(ak_provenance(y2)), 2)
  expect_error(catchment_mean(rbind(x, x), r, "v"), "more than one row")
  cs <- catchment_class_shares(x, r, prefix = "c_")
  expect_equal(cs$c_2, c(100, 100))
})

test_that("import_catchments makes the same kind of object as ak_catchments", {
  x <- sq_catch(c("A", "B"))
  x$hand_note <- c("drawn 2019", "drawn 2020")
  y <- import_catchments(x, condition = FALSE)
  expect_s3_class(y, "ak_catchments")
  expect_equal(y$hand_note, x$hand_note)
  expect_equal(y$area_km2, c(1, 1))
  expect_equal(ak_provenance(y)$fn, "import_catchments")
  expect_error(import_catchments(rbind(x, x), condition = FALSE), "more than one row")
})

test_that("Daymet pixels are downloaded a few at a time, parsed, cached, and failures recorded", {
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  csv <- \(lat) paste0("Latitude: ", lat, "  Longitude: -150\nX & Y on Lambert Conformal Conic: x y\nTile: 13\nElevation: 100 meters\n",
                       "All years; all variables; Daymet Software Version 4.0\n\n",
                       "year,yday,dayl (s),prcp (mm/day),srad (W/m^2),swe (kg/m^2),tmax (deg c),tmin (deg c),vp (Pa)\n",
                       paste(sprintf("2000,%d,30000,%g,100,0,%g,%g,500", 1:365, 1, 10 + lat - 61, lat - 61), collapse = "\n"), "\n")
  calls <- 0
  local_mocked_bindings(http_parallel = function(reqs, paths, label) {
    calls <<- calls + 1
    lapply(seq_along(reqs), \(i) {
      lat <- as.numeric(sub(".*lat=([0-9.]+).*", "\\1", reqs[[i]]$url))
      if (lat > 61.05) return(simpleError("HTTP 400 Bad Request"))
      writeLines(csv(lat), paths[i])
      structure(list(), class = "httr2_response")
    })
  }, ak_perform = function(req, path = NULL) stop("HTTP 400 Bad Request"))
  x <- sf::st_transform(sf::st_as_sf(data.frame(site_code = c("A", "OFF"), lon = -150, lat = c(61, 61.1)),
                                     coords = c("lon", "lat"), crs = 4326), 3338)
  w <- suppressMessages(ak_weather(x, years = 2000))
  expect_equal(unique(w$site_code), "A")
  expect_equal(w$tmax[1], 10)
  expect_equal(w$tmean[1], 5)
  expect_equal(nrow(w), 365)
  expect_equal(attr(w, "errors")$site_code, "OFF")
  expect_match(attr(w, "errors")$error, "outside Daymet coverage")
  expect_equal(ak_provenance(w)$fn, "ak_weather")
  expect_true(all(file.exists(attr(w, "pixel_files"))))
  w2 <- suppressMessages(ak_weather(x[1, ], years = 2000))          # cached: no request
  expect_equal(calls, 1)
  expect_equal(w2$tmax, w$tmax)
  expect_error(ak_weather(x, years = 1970), "1980")
})

fake_weather <- function(years = 1991:2020) {
  d <- tidyr::expand_grid(site_code = "A", year = years, yday = 1:365) |>
    dplyr::mutate(date = as.Date(paste0(year, "-01-01")) + (yday - 1),
                  tmax = 5 - 20 * cos(2 * pi * (yday - 15) / 365) + 4, tmin = tmax - 8, tmean = (tmin + tmax) / 2,
                  prcp = 2, swe = 0, n_pixels = 5, tmean_se_sampling = 0.1, prcp_se_sampling = 0.05)
  attr(d, "ak_provenance") <- AKCatchments:::provenance_row(AKCatchments:::layer_row("weather_daily", fn = "ak_weather"), "tmean")
  d
}

test_that("Daymet climate normals average the baseline, carry persistent sampling error, and flag gaps", {
  x <- sq_catch("A")
  w <- fake_weather()
  n <- ak_climate_normals(x, "daymet4", vars = c("tmean", "ppt"), periods = c("01", "07", "annual"), weather = w)
  expect_equal(n$daymet_ppt_01, 62)                       # 31 days x 2 mm
  expect_equal(n$daymet_ppt_annual, 730)
  expect_equal(n$daymet_ppt_01_se, 31 * 0.05)            # summed: errors persistent in time
  expect_equal(n$daymet_tmean_01_se, 0.1)
  jul <- w[format(w$date, "%m") == "07" & w$year == 2000, ]
  expect_equal(n$daymet_tmean_07, mean(jul$tmean))
  expect_equal(n$daymet_normals_flag, "")
  expect_false("daymet_tmin_01" %in% names(n))
  expect_equal(utils::tail(ak_provenance(n)$variant, 1), "daymet4")
  short <- ak_climate_normals(x, "daymet4", vars = "tmean", periods = "annual", weather = w[w$year >= 2000, ])
  expect_equal(short$daymet_normals_flag, "incomplete_baseline")
  expect_error(ak_climate_normals(x, "daymet4"), "needs the daily weather")
})

test_that("weather_at_samples checks the baseline and keeps every sample row", {
  w <- fake_weather(1991:2021)
  samples <- data.frame(site_code = "A", sample_date = as.Date(c("2021-07-15", "2021-07-15", "2021-08-01")), analyte = c("NO3", "NH4", "NO3"))
  out <- weather_at_samples(w, samples, melt = FALSE, se = FALSE)
  expect_equal(nrow(out), 3)
  expect_equal(out$analyte, samples$analyte)
  expect_true(all(c("t_anom7", "t_anom30", "p_anom30", "tdd", "gdd_base5", "summer_tmean_c") %in% names(out)))
  expect_lt(max(abs(out$t_anom30)), 1e-6)                # every year is the normal year
  expect_equal(ak_provenance(out)$fn, c("ak_weather", "weather_at_samples"))
  expect_error(weather_at_samples(w[w$year > 1995, ], samples), "lacks baseline years")
  daily <- weather_at_samples(w, NULL)
  expect_equal(nrow(daily), nrow(w))
})

test_that("ak_snowoff asks for the weather when it is to fill", {
  expect_error(ak_snowoff(sq_catch("A"), years = 2001), "needs the daily weather")
})
