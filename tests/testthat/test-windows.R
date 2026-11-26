# Delineation windows: the cell-size ladder, side-by-side growth, the coarse pass, given windows, and checkpoints
# in ak_catchments()

test_that("coarsened cell sizes come from the ladder, and windows grow on the touched sides only", {
  w <- \(x, y) c(xmin = 0, ymin = 0, xmax = x, ymax = y)
  expect_equal(window_res(5, w(30000, 10000), 6000), 5)
  expect_equal(window_res(5, w(30001, 10000), 6000), 6.25)
  expect_equal(window_res(5, w(45001, 10000), 6000), 10)
  expect_equal(window_res(5, w(10000, 59000), 6000), 10)
  expect_equal(window_res(5, w(160000, 40000), 6000), 30)
  expect_equal(window_res(30, w(100000, 100000), 6000), 30)
  expect_null(window_res(NA, w(1, 1), 6000))

  xy <- c(X = 1000, Y = 2000)
  sq <- first_window(xy, NULL, 5000, 150, 100000)
  expect_equal(unname(sq), c(-4000, -3000, 6000, 7000))
  expect_equal(unname(grow_window(sq, c("n", "e"), xy, 100000)), c(-4000, -3000, 11000, 12000))
  expect_equal(unname(grow_window(sq, "s", xy, 7000)), c(-4000, -5000, 6000, 7000))    # +2 km at least, capped at 7 km
  expect_equal(unname(grow_window(sq, c("w", "n"), xy, 100000, by = 0.5)), c(-9000, -3000, 6000, 12000))   # half the size
  expect_null(grow_window(c(xmin = -99000, ymin = 0, xmax = 2000, ymax = 102000), c("w", "n"), xy, 100000))
  expect_equal(unname(first_window(xy, c(0, 0, 3000, 1000), 5000, 150, 100000)), c(0, 0, 3000, 2650))   # holds the point's snap radius

  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10, vals = NA)
  r[2:5, 1:3] <- 1
  expect_equal(frame_sides(r), "w")
  r[1, 5] <- 1
  expect_equal(frame_sides(r), c("w", "n"))
  expect_true(touches_frame(r))
})

# The long valley (ellipse 2.5 x 9 km) of synthetic_valley()
valley_run <- function(s, start_buffer_m = 3000, ...) {
  pt <- xy_to_ll(s$x0, s$y0 + 200)
  suppressMessages(ak_delineate(pt[2], pt[1], "VALLEY", dem_source = "local", dem_dir = s$dir, start_buffer_m = start_buffer_m,
                                max_buffer_m = 20000, flowlines = NULL, plot = FALSE, ...))
}

test_that("a long catchment gets its own window, from the coarse pass or side by side, as from one big square", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  s <- synthetic_valley(tempfile("valley_"))
  same <- \(a, b) sum(as.numeric(sf::st_area(sf::st_sym_difference(sf::st_geometry(a), sf::st_geometry(b)))))
  big <- valley_run(s, start_buffer_m = 20000)$catchment
  expect_equal(big$n_windows, 1)
  expect_equal(big$area_km2, s$area_km2, tolerance = 0.05)

  coarse <- valley_run(s)$catchment
  expect_equal(coarse$window_plan, "coarse")
  expect_equal(coarse$n_windows, 2)
  expect_false(coarse$truncated)
  expect_lt(coarse$window_xmax - coarse$window_xmin, 8000)                  # fitted to the valley, not a square
  expect_equal(same(coarse, big), 0, tolerance = 1)

  # a given first window (the square here) grows side by side: only the north side moves
  pt <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(xy_to_ll(s$x0, s$y0 + 200)), crs = 4326), 3338))[1, ]
  sides <- valley_run(s, window = c(pt[1] - 3000, pt[2] - 3000, pt[1] + 3000, pt[2] + 3000))$catchment
  expect_equal(sides$window_plan, "given")
  expect_gt(sides$n_windows, 2)
  expect_equal(sides$window_xmax - sides$window_xmin, 6000, tolerance = 50)
  expect_equal(same(sides, big), 0, tolerance = 1)

  # the same grid again from the recorded window, in one window
  again <- valley_run(s, window = c(coarse$window_xmin, coarse$window_ymin, coarse$window_xmax, coarse$window_ymax))$catchment
  expect_equal(c(again$window_plan, again$n_windows), c("given", "1"))
  expect_equal(c(again$window_xmin, again$window_ymax), c(coarse$window_xmin, coarse$window_ymax))
  expect_equal(again$area_km2, coarse$area_km2)
})

test_that("a window that cannot grow (the end of the DEM's coverage) is not fetched again", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  s <- synthetic_confluence(tempfile("conf_"))            # drains its whole extent: the catchment always reaches the edge
  pt <- xy_to_ll(s$xm, s$yj - 1000)
  d <- suppressMessages(ak_delineate(pt[2], pt[1], "EDGE", dem_source = "local", dem_dir = s$dir, start_buffer_m = 20000,
                                     max_buffer_m = 100000, flowlines = NULL, plot = FALSE))
  expect_true(d$catchment$truncated)
  expect_equal(d$catchment$n_windows, 2)
})

test_that("ak_catchments resumes from checkpoints and redoes sites whose arguments changed", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  s <- synthetic_dem(tempfile("dem_synth_"))
  ll <- xy_to_ll(s$x0, s$y0 + 200)
  pts <- data.frame(site_code = c("C/1", "C2"), lat = ll[[2]], lon = ll[[1]] + c(0, 0.0005))
  cp <- tempfile("checkpoints_")
  run <- \(...) ak_catchments(pts, dem_source = "local", dem_dir = s$dir, start_buffer_m = 10000, max_buffer_m = 10000,
                              flowlines = NULL, plot = FALSE, checkpoint_dir = cp, ...)
  a <- suppressMessages(run())
  expect_length(list.files(cp, "\\.rds$"), 2)
  expect_message(b <- run(), "2 of 2 sites read from their checkpoints")
  expect_equal(sf::st_drop_geometry(b)[c("site_code", "area_km2", "snap_dist_m")], sf::st_drop_geometry(a)[c("site_code", "area_km2", "snap_dist_m")])
  m <- capture_messages(c2 <- run(snap_dist_m = 60))                         # another argument: delineated again, and saved
  expect_false(any(grepl("read from their checkpoints", m)))
  expect_message(run(snap_dist_m = 60), "2 of 2 sites read")
  unlink(c2$dem_file[1])
  expect_message(run(snap_dist_m = 60), "1 of 2 sites read")                  # its kept rasters are gone: delineated again
})
