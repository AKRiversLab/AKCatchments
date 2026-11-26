test_that("conditioned windows with other res or buffer get their own files", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  s <- synthetic_dem(tempfile("dem_synth_"))
  catch <- sf::st_sf(site_code = "SYN", geometry = sf::st_sfc(sf::st_buffer(sf::st_point(c(s$x0, s$y0 + 4000)), 2000), crs = 3338))
  a <- import_catchments(catch, dem_source = "local", dem_dir = s$dir)
  b <- import_catchments(catch, dem_source = "local", dem_dir = s$dir, res = 40)
  d <- import_catchments(catch, dem_source = "local", dem_dir = s$dir, buffer_m = 1000)
  expect_equal(basename(a$dem_file), "dem_tiles.tif")              # the geo workflow's name for the defaults
  expect_equal(basename(b$dem_file), "dem_tiles_40m.tif")
  expect_equal(basename(d$dem_file), "dem_tiles_buf1000.tif")
  expect_equal(c(a$dem_res_m, b$dem_res_m), c(20, 40))
  expect_gt(terra::ncell(terra::rast(d$dem_file)), terra::ncell(terra::rast(a$dem_file)))
})

test_that("delineations from different DEMs keep separate rasters, and terrain functions use them", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  s <- synthetic_dem(tempfile("dem_synth_"))
  ll <- xy_to_ll(s$x0, s$y0 + 200)
  run <- \(res) ak_delineate(ll[2], ll[1], site_code = "SYN-K", dem_source = "local", dem_dir = s$dir, res = res,
                             start_buffer_m = 10000, max_buffer_m = 10000, flowlines = NULL, plot = FALSE)
  d20 <- run(NULL)
  d40 <- run(40)
  expect_false(identical(d20$files$dem, d40$files$dem))
  expect_true(all(file.exists(c(unlist(d20$files), unlist(d40$files)))))
  expect_equal(terra::res(terra::rast(d20$files$accum))[1], 20)
  expect_equal(terra::res(terra::rast(d40$files$accum))[1], 40)
  # kept rasters are cropped to the catchment plus a margin, not the whole 20 km window
  expect_lt(terra::ncell(terra::rast(d20$files$dem)), 0.5 * 1000^2)
  expect_equal(ak_topography(d20)$topo_dem_res_m, 20)
  expect_equal(ak_topography(d40)$topo_dem_res_m, 40)
})

test_that("a catchment running into missing elevation data is flagged, not widened", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  s <- synthetic_dem(tempfile("dem_synth_"))
  r <- terra::rast(file.path(s$dir, "synthetic_dem.tif"))
  r[terra::crds(r, na.rm = FALSE)[, 2] > s$y0 + 6000] <- NA   # the head of the basin has no data (a border)
  gap <- tempfile("dem_gap_")
  dir.create(gap, showWarnings = FALSE)
  terra::writeRaster(r, file.path(gap, "gap.tif"), overwrite = TRUE)
  ll <- xy_to_ll(s$x0, s$y0 + 200)
  d <- suppressMessages(ak_delineate(ll[2], ll[1], site_code = "SYN-NA", dem_source = "local", dem_dir = gap,
                                     start_buffer_m = 10000, max_buffer_m = 20000, flowlines = NULL, plot = FALSE))
  expect_true(d$catchment$nodata_edge)
  expect_false(d$catchment$truncated)
  expect_equal(d$catchment$n_windows, 1)                          # no pointless widening
  expect_lt(abs(d$catchment$window_m - 10000), 20)                # the window as fetched, on the DEM's 20 m grid
  expect_match(d$catchment$flags, "nodata_edge")
  expect_false(d$catchment$delin_ok)
  inside_gap <- xy_to_ll(s$x0, s$y0 + 7000)
  expect_error(ak_delineate(inside_gap[2], inside_gap[1], site_code = "SYN-X", dem_source = "local", dem_dir = gap,
                            start_buffer_m = 3000, flowlines = NULL, plot = FALSE), "no elevation data")
})

test_that("tiles of different resolution are projected one by one and merged", {
  dir <- tempfile(); dir.create(dir)
  mk <- \(ymin, nx, f) {
    r <- terra::rast(terra::ext(-150, -149, ymin, ymin + 1), ncols = nx, nrows = 50, crs = "EPSG:4326", vals = ymin)
    terra::writeRaster(r, file.path(dir, f), overwrite = TRUE)
    file.path(dir, f)
  }
  files <- c(mk(59, 80, "a.tif"), mk(60, 60, "b.tif"), file.path(dir, "missing.tif"))
  bb <- sf::st_bbox(c(xmin = -149.8, ymin = 59.7, xmax = -149.2, ymax = 60.3), crs = 4326)
  tmpl <- terra::rast(terra::ext(sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb), 3338))[c("xmin", "xmax", "ymin", "ymax")]),
                      resolution = 1000, crs = "EPSG:3338")
  g <- AKCatchments:::tiles_to_grid(files, bb, tmpl, "test")
  v <- terra::values(g, mat = FALSE)
  expect_setequal(unique(stats::na.omit(v)), c(59, 60))
  expect_error(AKCatchments:::tiles_to_grid(file.path(dir, "missing.tif"), bb, tmpl, "test"), "No test tile could be read")
})

test_that("a web page saved in place of a data file is caught", {
  f <- tempfile(fileext = ".zip")
  writeLines(c("  <!DOCTYPE html>", "<html><body>Sign in</body></html>"), f)
  expect_true(AKCatchments:::is_web_page(f))
  g <- tempfile(fileext = ".zip")
  writeBin(as.raw(c(0x50, 0x4b, 0x03, 0x04, 0, 0, 1)), g)
  expect_false(AKCatchments:::is_web_page(g))
})

test_that("archives unpack through a temporary folder", {
  src <- tempfile(); dir.create(src)
  writeLines("x", file.path(src, "a.txt"))
  skip_if_not(nzchar(Sys.which("zip")), "no zip program")
  z <- tempfile(fileext = ".zip")
  old <- setwd(src); utils::zip(z, "a.txt", flags = "-q", zip = Sys.which("zip")); setwd(old)
  out <- file.path(tempfile(), "layer")
  dir.create(out, recursive = TRUE)                               # empty, as ak_cache_dir() leaves it
  AKCatchments:::extract_archive(z, out)
  expect_true(file.exists(file.path(out, "a.txt")))
  expect_false(any(grepl("\\.part$", list.files(dirname(out)))))
})

test_that("invalid hand-drawn polygons are repaired and a missing CRS is explained", {
  bow <- sf::st_polygon(list(rbind(c(0, 0), c(1000, 1000), c(1000, 0), c(0, 1000), c(0, 0))))
  x <- sf::st_sf(site_code = "A", geometry = sf::st_sfc(bow, crs = 3338))
  expect_true(all(sf::st_is_valid(AKCatchments:::as_catchments(x))))
  expect_error(AKCatchments:::as_catchments(sf::st_set_crs(x, NA)), "no coordinate reference system")
})

test_that("a point just above a confluence is flagged as ambiguous, with the other branch's area", {
  r <- terra::rast(terra::ext(0, 1000, 0, 1000), resolution = 10, crs = "EPSG:3338", vals = 1)
  rc <- terra::rowColFromCell(r, seq_len(terra::ncell(r)))
  v <- rep(1, terra::ncell(r))
  v[rc[, 2] == 50 & rc[, 1] < 60] <- 10000                         # main stem above the confluence: 1 km2
  v[rc[, 1] == 60 & rc[, 2] > 50 & rc[, 2] <= 80] <- 4000          # tributary from the east: 0.4 km2
  v[rc[, 2] == 50 & rc[, 1] >= 60] <- 14000                        # below the confluence: 1.4 km2
  terra::values(r) <- v
  f <- tempfile(fileext = ".tif"); terra::writeRaster(r, f)
  xy <- terra::xyFromCell(r, terra::cellFromRowCol(r, 57, 52))
  pt <- sf::st_sf(site_code = "Y", geometry = sf::st_sfc(sf::st_point(xy[1, ]), crs = 3338))
  s <- AKCatchments:::snap_point(pt, f, 150, NA, 0.05, "largest")
  expect_equal(s$snap_area_km2, 1.4)
  expect_true(s$snap_ambiguous)                                     # a factor-of-2 rule missed this
  expect_equal(s$snap_alt_area_km2, 1)
  g <- AKCatchments:::snap_point(pt, f, 150, 1, 0.05, "largest")   # with the known area it takes the main stem
  expect_equal(g$snap_area_km2, 1)
  one <- AKCatchments:::snap_point(pt, f, 25, NA, 0.05, "largest") # only the main stem in reach
  expect_false(one$snap_ambiguous)
  expect_true(is.na(one$snap_alt_area_km2))
})

test_that("cells touching at a corner make one part, as D8 watersheds do", {
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 40, ymin = 0, ymax = 40, crs = "EPSG:3338", vals = NA)
  r[c(1, 2, 5, 6, 11)] <- 1
  p <- terra::as.polygons(r) |> sf::st_as_sf() |> sf::st_make_valid() |> sf::st_union()
  expect_equal(AKCatchments:::n_parts(p, 10), 1)
  r[16] <- 1                                                        # a cell apart from the rest
  r[11] <- NA
  p2 <- terra::as.polygons(r) |> sf::st_as_sf() |> sf::st_make_valid() |> sf::st_union()
  expect_equal(AKCatchments:::n_parts(p2, 10), 2)
})

test_that("a snap onto a neighbouring stream or the other branch is caught by the flow paths", {
  r <- terra::rast(terra::ext(0, 500, 0, 2000), resolution = 10, crs = "EPSG:3338", vals = 8)   # everything flows south
  rc <- terra::rowColFromCell(r, seq_len(terra::ncell(r)))
  v <- terra::values(r)[, 1]
  v[rc[, 1] == 150 & rc[, 2] < 25] <- 2                          # far downstream the west stream turns east
  v[rc[, 1] == 25 & rc[, 2] %in% 40:49] <- 2                     # a branch at col 40 joins col 50 at row 25
  v[rc[, 1] < 30 & rc[, 2] %in% 21:22] <- 32                       # the east bank of col 20 drains west into it
  terra::values(r) <- v
  f <- tempfile(fileext = ".tif"); terra::writeRaster(r, f)
  xy <- \(row, col) terra::xyFromCell(r, terra::cellFromRowCol(r, row, col))[1, ]
  ss <- \(a, b) AKCatchments:::same_stream(f, a, b, 150)
  expect_true(ss(xy(10, 20), xy(14, 20)))                         # outlet just below the point
  expect_true(ss(xy(14, 20), xy(10, 20)))                         # outlet just above the point
  expect_true(ss(xy(14, 22), xy(10, 20)))                         # point 20 m beside the channel, outlet upstream
  expect_false(ss(xy(10, 20), xy(10, 30)))                        # parallel stream 100 m away, never meets
  expect_false(ss(xy(10, 40), xy(10, 50)))                        # the other branch: paths meet only at the confluence 150 m down
  x <- sf::st_sf(area_km2 = 5, snap_dist_m = 20, snap_path_ok = FALSE, geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3338))
  expect_match(AKCatchments:::flag_delineation(x)$flags, "snap_off_stream")
  expect_true(AKCatchments:::flag_delineation(x)$delin_ok)                    # a warning, not a failure
})

test_that("a fallback DEM is used when the first one has a hole, and the result says so", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  s <- synthetic_dem(tempfile("dem_synth_"))
  r <- terra::rast(file.path(s$dir, "synthetic_dem.tif"))
  r[terra::crds(r, na.rm = FALSE)[, 2] > s$y0 + 6000] <- NA
  gap <- tempfile("dem_gap_"); dir.create(gap, showWarnings = FALSE)
  terra::writeRaster(r, file.path(gap, "gap.tif"), overwrite = TRUE)
  ll <- xy_to_ll(s$x0, s$y0 + 200)
  local_mocked_bindings(ak_dem = function(aoi, source, res = NULL, dir = NULL, ...) {
    d <- dem_local(as_aoi(aoi), if (source == "copernicus") s$dir else gap, 20)    # "copernicus" stands in as the complete DEM
    names(d) <- "elevation"
    terra::metags(d) <- c(dem_source = paste0(source, "_20m"))
    d
  })
  d <- suppressMessages(ak_delineate(ll[2], ll[1], site_code = "SYN-FB", dem_source = "ifsar", res = 20, start_buffer_m = 10000,
                                     max_buffer_m = 10000, flowlines = NULL, plot = FALSE, fallback_dem = "copernicus"))
  expect_true(d$catchment$dem_fallback)
  expect_false(d$catchment$nodata_edge)
  expect_match(d$catchment$dem_source, "^copernicus")
  expect_match(d$catchment$flags, "dem_fallback")
  expect_equal(d$catchment$area_km2, s$area_km2, tolerance = 0.1)
})

test_that("ScienceBase files are found through the item record, including facet attachments", {
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  rec <- list(files = list(list(name = "meta.html", url = "https://sb/meta")),
              facets = list(list(files = list(list(name = "basins.zip", downloadUri = "https://sb/basins")))))
  local_mocked_bindings(ak_perform = \(req, path = NULL) httr2::response_json(body = rec),
                        ak_download = \(url, dest, query = list()) url)
  expect_equal(sb_download("123", "basins.zip"), "https://sb/basins")
  expect_error(sb_download("123", "gone.zip"), "meta.html, basins.zip")
})

test_that("replace_file puts a copy in place and cleans up", {
  from <- tempfile(fileext = ".gpkg"); to <- tempfile(fileext = ".gpkg")
  writeLines("new", from); writeLines("old", to)
  expect_true(replace_file(from, to))
  expect_equal(readLines(to), "new")
  expect_false(file.exists(from))
  expect_length(list.files(dirname(to), paste0(basename(to), ".*part")), 0)
})

test_that("PRISM grids are read whatever format the zip holds", {
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  d <- tempfile(); dir.create(d)
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 210, xmax = 211, ymin = 61, ymax = 62, crs = "EPSG:4269", vals = 1:4)  # 0-360 E, as PRISM
  terra::writeRaster(r, file.path(d, "ak_tmean_1981_2010.07.txt"), filetype = "AAIGrid")  # as PRISM ships it
  zip <- file.path(ak_cache_dir(cache_paths$prism), "PRISM_tmean_ak_30yr_normal_800mM1_07_asc.zip")
  utils::zip(zip, file.path(d, "ak_tmean_1981_2010.07.txt"), flags = "-qj")
  expect_equal(as.vector(terra::values(prism_raster("tmean", "07"))), 1:4)
  pt <- sf::st_sf(site_code = "A", geometry = sf::st_sfc(sf::st_point(c(-149.75, 61.75)), crs = 4326)) |>  # = 210.25 E, the cell with 1
    sf::st_transform(3338) |> sf::st_buffer(1000)
  expect_equal(ak_climate_normals(pt, vars = "tmean", periods = "07")$prism_tmean_07, 1, tolerance = 1e-6)
})

test_that("the coarse-scale slope equals the native mean at the DEM's own resolution", {
  dem <- terra::rast(nrows = 60, ncols = 60, xmin = 0, xmax = 600, ymin = 0, ymax = 600, crs = "EPSG:3338")
  terra::values(dem) <- outer(1:60, 1:60, \(i, j) i * 3 + sin(j / 3) * 5)
  g <- sf::st_sf(site_code = "A", geometry = sf::st_sfc(sf::st_buffer(sf::st_point(c(300, 300)), 250), crs = 3338))
  t <- topo_one(g, dem, NULL, "test_10m", 1)
  expect_equal(t$topo_slope_mean_deg_10m, t$topo_slope_mean_deg)
  expect_true(is.finite(t$topo_slope_mean_deg_30m))
  expect_true(is.na(topo_one(g, terra::aggregate(dem, 2), NULL, "test_20m", 1)$topo_slope_mean_deg_10m))
})

test_that("there is no default cache: the user has to choose one", {
  old <- options(AKCatchments.cache = NULL)
  on.exit(options(old), add = TRUE)
  env <- Sys.getenv("AKCATCHMENTS_CACHE"); Sys.unsetenv("AKCATCHMENTS_CACHE")
  on.exit(if (nzchar(env)) Sys.setenv(AKCATCHMENTS_CACHE = env), add = TRUE)
  expect_error(ak_cache_dir(), "ak_set_cache")
})

test_that("a filling run that fails is tried again", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  s <- synthetic_dem(tempfile("dem_synth_"))
  dem <- terra::rast(file.path(s$dir, "synthetic_dem.tif"))
  fill <- whitebox::wbt_fill_depressions
  tries <- 0
  # the first try writes nothing, as when the tool panics
  local_mocked_bindings(wbt_fill_depressions = function(...) if ((tries <<- tries + 1) > 1) fill(...), .package = "whitebox")
  f <- ak_condition(dem, withr::local_tempdir(), "fill")
  expect_equal(tries, 2)
  expect_true(all(file.exists(unlist(f))))
})

test_that("outputs WhiteboxTools did not write stop the run, and the last run's rasters are not used", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  s <- synthetic_dem(tempfile("dem_synth_"))
  dem <- terra::rast(file.path(s$dir, "synthetic_dem.tif"))
  dir <- withr::local_tempdir()
  f <- ak_condition(dem, dir)
  expect_true(all(file.exists(unlist(f))))
  # breaching writes nothing (as when the tool fails) on a second, smaller window in the same folder
  local_mocked_bindings(wbt_breach_depressions = function(...) invisible(), .package = "whitebox")
  expect_error(suppressMessages(ak_condition(terra::crop(dem, terra::ext(dem) - 1000), dir)),
               "did not write breached.tif, d8_pointer.tif, d8_accum.tif")
})
