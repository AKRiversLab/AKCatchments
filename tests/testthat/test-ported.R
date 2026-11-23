ellipse <- function(s, a = 2000, b = 4000, id = "SYN") {
  t <- seq(0, 2 * pi, length.out = 200)
  m <- cbind(s$x0 + a * cos(t), s$y0 + 4000 + b * sin(t)); m[200, ] <- m[1, ]
  sf::st_sf(site_code = id, geometry = sf::st_sfc(sf::st_polygon(list(m)), crs = 3338))
}

test_that("outlet slope follows the channel gradient and topography is complete", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  s <- synthetic_dem(tempfile("dem_synth_"))
  catch <- ellipse(s)
  pt <- sf::st_sf(site_code = "SYN", geometry = sf::st_sfc(sf::st_point(c(s$x0 + 40, s$y0 + 200)), crs = 3338))
  cond <- import_catchments(catch, dem_source = "local", dem_dir = s$dir)
  expect_s3_class(cond, "ak_catchments")
  expect_true(all(file.exists(unlist(sf::st_drop_geometry(cond)[c("dem_file", "filled_file", "pointer_file", "accum_file")]))))
  sl <- ak_outlet_slope(cond, pt)
  expect_equal(sl$outlet_reach_m, 100)
  expect_true(sl$outlet_reach_complete)
  expect_equal(c(sl$outlet_slope_m_m, sl$outlet_slope_m_m_200), c(0.02, 0.02), tolerance = 0.25)
  expect_gt(sl$outlet_contrib_area_km2, 20)
  expect_equal(nrow(ak_provenance(sl)), 2)    # the import and the outlet slope
  tp <- ak_topography(cond)
  expect_true(all(c("topo_fws_pct", "topo_chan_slope_pct", "topo_wetx", "topo_slope_mean_deg_30m", "topo_hli", "topo_flag") %in% names(tp)))
  expect_true(is.na(tp$topo_slope_mean_deg_10m))   # 20 m DEM: no 10 m version
  expect_gt(tp$topo_relief_m, 50)
})

fake_daymet <- function(dir, years = 1991:2012) {
  d <- tidyr::expand_grid(year = years, yday = 1:365)
  px <- function(shift) d |>
    dplyr::mutate(tmax = 5 - 20 * cos(2 * pi * (yday - 15) / 365) + 0.05 * (year - 2000) + 4,
                  tmin = tmax - 8, prcp = ifelse(yday %% 5 == 0, 4, 0),
                  swe = pmax(0, pmin(yday, 100 + shift) - pmax(0, (yday - 100 - shift) * 4)) * (yday < 200))
  files <- c(p1_1 = file.path(dir, "p1_1_1991_2012.rds"), p1_2 = file.path(dir, "p1_2_1991_2012.rds"))
  saveRDS(px(0), files[1]); saveRDS(px(10), files[2])
  out <- px(5) |>
    dplyr::mutate(site_code = "A", tmean = (tmin + tmax) / 2, date = as.Date(paste0(year, "-01-01")) + (yday - 1))
  attr(out, "pixels") <- data.frame(site_code = "A", pixel = c("p1_1", "p1_2"))
  attr(out, "pixel_files") <- files
  out
}

test_that("Daymet melt, anomalies, snow-off fill and climate summaries", {
  dir <- tempfile(); dir.create(dir)
  dm <- fake_daymet(dir)
  melt <- AKCatchments:::melt_daymet(dm)
  # pixel 1: SWE peaks at day 100 and falls below 1 on day 125; pixel 2 peaks at 110, melts on 138; median 131.5
  expect_equal(unique(melt$melt_doy), 131.5)
  expect_equal(unique(melt$melt_anom), 0)
  obs <- data.frame(site_code = "A", sample_date = as.Date(c("2010-07-15", "2012-08-01")))
  an <- AKCatchments:::weather_anomalies(dm, obs, melt = melt)
  expect_equal(nrow(an), 2)
  expect_lt(max(abs(an$t_anom30_dt)), 0.5)
  expect_equal(an$days_since_melt, c(196 - 131.5, 214 - 131.5))
  modis <- data.frame(site_code = "A", year = 2001:2010, modis_doy = 136 + rep(c(-1, 1), 5),
                      modis_valid_frac = 1, snowoff_rmse_d = 8)
  so <- AKCatchments:::snowoff_fill(modis, melt)
  expect_equal(so$snowoff_source[so$year == 2012], "daymet_offset")
  expect_equal(so$snowoff_doy[so$year == 2012], 136)
  expect_equal(so$snowoff_se_d[so$year == 2005], 8)
  expect_equal(attr(so, "calibration")[["mean_offset_d"]], 136 - 131.5)
  cl <- AKCatchments:::climate_summaries(dm, obs, snowoff = so)
  day <- dm[dm$year == 2010 & dm$yday <= 196, ]
  expect_equal(cl$tdd[1], sum(pmax(day$tmean, 0)))
  expect_equal(cl$gdd_base5[1], sum(pmax(day$tmean - 5, 0)))
  expect_equal(cl$tdd_snowoff[1], sum(pmax(day$tmean[day$yday >= 137], 0)))   # 2010 MODIS date: 136 + 1
  expect_equal(cl$days_since_snowoff[1], 196 - 137)
  expect_true(all(c("snowfall_swe_mm", "snowfall_depth_cm", "summer_tmean_c") %in% names(cl)))
})

test_that("fire windows union reburns and use the cached perimeters", {
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  sq <- \(x0, s) sf::st_polygon(list(cbind(c(x0, x0 + s, x0 + s, x0, x0), c(0, 0, s, s, 0))))
  catch <- sf::st_sf(site_code = "A", geometry = sf::st_sfc(sq(0, 1000), crs = 3338))
  fires <- sf::st_sf(FIREYEAR = c("2001", "2005", "2008"), PRESCRIBED = c(NA, NA, "Y"),
                     geometry = sf::st_sfc(sq(0, 500), sq(0, 500), sq(500, 500), crs = 3338))
  cache <- file.path(ak_cache_dir(), "fire_history_ak.gpkg")
  sf::st_write(fires, cache, layer = "perimeters", quiet = TRUE)
  cover <- sf::st_sf(queried_utc = "2026-01-01", geometry = sf::st_as_sfc(sf::st_bbox(sf::st_buffer(catch, 5000))))
  sf::st_write(cover, cache, layer = "coverage", quiet = TRUE)
  w <- suppressMessages(ak_fire(catch, samples = data.frame(site_code = "A", year = c(2006, 2010))))
  expect_equal(w$burn_frac_5yr, c(0.25, 0.25))      # 2001-2005 burns overlap: counted once
  expect_equal(w$burn_frac_hist, c(0.25, 0.25))     # 2008 burn is prescribed: dropped
  expect_equal(w$burn_frac_same_yr, c(0, 0))
})

test_that("the fire cache knows its area: catchments outside it are queried with the old area", {
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  sq <- \(x0, s) sf::st_polygon(list(cbind(c(x0, x0 + s, x0 + s, x0, x0), c(0, 0, s, s, 0))))
  near <- sf::st_sf(site_code = "A", geometry = sf::st_sfc(sq(0, 1000), crs = 3338))
  far <- sf::st_sf(site_code = "B", geometry = sf::st_sfc(sq(50000, 1000), crs = 3338))
  fires <- sf::st_sf(FIREID = "1", FIREYEAR = "2001", PRESCRIBED = NA_character_, ACRES = 1,
                     geometry = sf::st_sfc(sq(0, 500), crs = 3338))
  asked <- list()
  local_mocked_bindings(ak_arcgis_query = function(layer_url, aoi, ...) { asked[[length(asked) + 1]] <<- sf::st_bbox(aoi); fires })
  suppressMessages(ak_fire(near, by_year = TRUE))
  expect_length(asked, 1)
  expect_true(all(c("perimeters", "coverage") %in% sf::st_layers(file.path(ak_cache_dir(), "fire_history_ak.gpkg"))$name))
  suppressMessages(ak_fire(near, by_year = TRUE))        # covered: no new query
  expect_length(asked, 1)
  f <- suppressMessages(ak_fire(far, by_year = TRUE))    # not covered: one query over both areas
  expect_length(asked, 2)
  expect_lt(asked[[2]][["xmin"]], 0)
  expect_gt(asked[[2]][["xmax"]], 51000)
  expect_equal(nrow(f), 0)
  # a geo-workflow file (no coverage layer) counts for the box its perimeters span
  unlink(file.path(ak_cache_dir(), "fire_history_ak.gpkg"))
  sf::st_write(fires, file.path(ak_cache_dir(), "fire_history_ak.gpkg"), quiet = TRUE)
  small <- sf::st_sf(site_code = "C", geometry = sf::st_sfc(sf::st_buffer(sf::st_point(c(250, 250)), 10), crs = 3338))
  expect_equal(nrow(suppressMessages(ak_fire(small, by_year = TRUE))), 1)
  expect_length(asked, 3)                                 # 1 km margin exceeds the 500 m box: queried
  unlink(file.path(ak_cache_dir(), "fire_history_ak.gpkg"))
  big <- fires
  sf::st_geometry(big) <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(small, 5000)))
  sf::st_write(big, file.path(ak_cache_dir(), "fire_history_ak.gpkg"), quiet = TRUE)
  expect_equal(nrow(suppressMessages(ak_fire(small, by_year = TRUE))), 1)
  expect_length(asked, 3)                                 # inside the box its perimeters span: reused
})

test_that("lithology classes follow the geo workflow", {
  expect_equal(AKCatchments:::classify_lith(c("Limestone", "glacial till", "basalt flows", "quartzite")),
               c("carbonate", "unconsolidated", "mafic_volcanic", "other"))
})
