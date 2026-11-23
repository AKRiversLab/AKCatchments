test_that("flag_text joins named flags and treats NA as unset", {
  expect_equal(AKCatchments:::flag_text(a = c(TRUE, FALSE, NA), b = c(TRUE, TRUE, FALSE)), c("a;b", "b", ""))
  expect_equal(AKCatchments:::flag_text(a = TRUE, b = c(FALSE, TRUE)), c("a", "a;b"))
})

test_that("the cache can be set, records downloads and follows the geo workflow layout", {
  old <- options(AKCatchments.cache = NULL)
  old_env <- Sys.getenv("AKCATCHMENTS_CACHE")
  on.exit({ options(old); Sys.setenv(AKCATCHMENTS_CACHE = old_env) }, add = TRUE)
  d <- tempfile("shared_cache_")
  ak_set_cache(d)
  expect_equal(normalizePath(ak_cache_dir()), normalizePath(d))
  options(AKCatchments.cache = NULL)
  expect_equal(normalizePath(ak_cache_dir()), normalizePath(d))   # from the environment variable
  f <- file.path(ak_cache_dir("ak_geology"), "x.zip"); writeLines("x", f)
  AKCatchments:::log_download("https://example.org/x.zip", f)
  info <- ak_cache_info()
  expect_equal(info$downloads$file, "ak_geology/x.zip")
  lay <- ak_cache_layout()
  expect_true(all(c("scratch/daymet_pixels", "modis_snow", "ak_geology", "accs_alnus", "nlcd_2016_ak", "jorgenson_2008") %in% lay$folder))
})

test_that("existing Daymet pixel and MODIS files from the geo workflow are reused", {
  cache <- tempfile(); dir.create(cache)
  saveRDS(data.frame(), file.path(cache, "p1_2_1991_2023.rds"))
  f <- AKCatchments:::daymet_file_of(cache, 2006:2020)
  expect_equal(basename(f(c("p1_2", "p9_9"))), c("p1_2_1991_2023.rds", "p9_9_2006_2020.rds"))
  expect_equal(basename(AKCatchments:::daymet_file_of(cache, 1980:2020)("p1_2")), "p1_2_1980_2020.rds")
  r <- terra::rast(terra::ext(0, 10000, 0, 10000), resolution = 500, crs = "EPSG:3338", vals = 1)
  terra::writeRaster(r, file.path(cache, "modis_snow_2010.tif"))
  inside <- sf::st_as_sfc(sf::st_bbox(c(xmin = 1000, ymin = 1000, xmax = 5000, ymax = 5000), crs = 3338))
  outside <- sf::st_as_sfc(sf::st_bbox(c(xmin = 1000, ymin = 1000, xmax = 50000, ymax = 5000), crs = 3338))
  expect_equal(basename(AKCatchments:::modis_file_of(cache, inside, "k")(2010)), "modis_snow_2010.tif")
  expect_equal(basename(AKCatchments:::modis_file_of(cache, outside, "k")(2010)), "modis_snow_2010_k.tif")
})

test_that("soil component draws give an SE and flag assumed concentrations", {
  co <- data.frame(site_code = "A", mukey = c("1", "1", "2", "2"), w = c(0.6, 0.6, 0.4, 0.4),
                   comppct_l = c(50, 20, NA, NA), comppct_r = c(70, 30, 60, 40), comppct_h = c(85, 45, NA, NA),
                   taxorder = c("Gelisols", "Inceptisols", "Gelisols", "Histosols"), drainagecl = c("Poorly drained", "Well drained", NA, NA))
  d <- AKCatchments:::soil_share_draws(co, n = 400)
  expect_gt(d$soil_gelisol_pct_se, 0)
  expect_true(d$soil_gelisol_pct_lo95 < 0.6 * 70 + 0.4 * 60 & d$soil_gelisol_pct_hi95 > 0.6 * 70 + 0.4 * 60)
  expect_equal(d$assumed_frac, 0.4)
})

test_that("NLCD shares are bias-adjusted per catchment from an error matrix", {
  shares <- data.frame(site_code = c("A", "B"), nlcd_pct_evergreen = c(50, 80), nlcd_pct_shrub = c(50, 20))
  em <- matrix(c(40, 10, 0, 50), 2, dimnames = list(c("42", "52"), c("42", "52")))
  adj <- AKCatchments:::landcover_adjust(shares, em)
  expect_equal(adj$nlcd_adj_pct_evergreen[1], 100 * (0.5 + 0.5 / 6), tolerance = 1e-6)  # map 42 rows all 42; 1 in 6 mapped shrub is 42
  expect_true(all(c("nlcd_adj_pct_shrub_se", "nlcd_adj_pct_shrub_lo95") %in% names(adj)))
})

test_that("PRISM consistency checks flag inconsistent normals", {
  d <- data.frame(site_code = c("A", "B"), prism_tmean_07 = c(12, 12), prism_tmax_07 = c(18, 18), prism_tmin_07 = c(6, 3))
  expect_equal(AKCatchments:::prism_consistency(d), c("", "tmean_not_mid"))
})

test_that("attribute spread across alternative polygons", {
  sq <- \(s) sf::st_sf(site_code = "A", geometry = sf::st_sfc(sf::st_polygon(list(cbind(c(0, s, s, 0, 0), c(0, 0, s, s, 0)))), crs = 3338))
  f <- \(x) data.frame(site_code = x$site_code, area = x$area_m2 / 1e6, area_se = 1)
  sp <- polygon_spread(list(hand = sq(1000), auto = sq(2000)), f)
  expect_equal(sp$n_polygons, 2)
  expect_equal(sp$min, 1); expect_equal(sp$max, 4)
  expect_false("area_se" %in% sp$variable)
})

test_that("glacier cover is a share of the whole catchment, with positional spread", {
  catch <- sf::st_sf(site_code = "A", geometry = sf::st_sfc(sf::st_polygon(list(cbind(c(0, 4000, 4000, 0, 0), c(0, 0, 4000, 4000, 0)))), crs = 3338))
  gl <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_polygon(list(cbind(c(0, 2000, 2000, 0, 0), c(0, 0, 2000, 2000, 0)))), crs = 3338))
  dir <- tempfile(); dir.create(dir); sf::st_write(gl, file.path(dir, "rgi.shp"), quiet = TRUE)
  g <- ak_glaciers(catch, dir, sigma_m = 100, n = 100)
  expect_equal(g$glacier_pct, 25)
  expect_gt(g$glacier_pct_se, 0)
  expect_equal(g$glacier_flag, "")
  far <- sf::st_sf(site_code = "B", geometry = sf::st_geometry(catch) + c(50000, 0))   # no glacier within reach
  sf::st_crs(far) <- 3338
  g0 <- ak_glaciers(far, dir, sigma_m = 100, n = 100)
  expect_equal(g0$glacier_pct, 0)
  expect_equal(g0$glacier_pct_hi95, 0)
})
