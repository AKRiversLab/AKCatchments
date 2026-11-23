test_that("reach areas are read from TDX-Hydro, MERIT and HydroRIVERS attributes", {
  expect_equal(reach_area_km2(data.frame(USContArea = 5e6, DSContArea = 8e6)), data.frame(lo = 5, hi = 8))
  expect_equal(reach_area_km2(data.frame(uparea = 40))$lo, 20)
  expect_equal(reach_area_km2(data.frame(UPLAND_SKM = 12))$hi, 12)
  expect_error(reach_area_km2(data.frame(GNIS_NAME = "x")), "no upstream-area attribute")
})

test_that("the consensus never leaves the point's own flow path; the reference chooses along it", {
  ptr <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100, crs = "EPSG:3338", vals = 2)  # all flow east (WhiteboxTools D8 code 2)
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(ptr, f)
  xy <- c(x = 15, y = 55)                                                    # row 5, column 2
  cell <- \(x, y) terra::cellFromXY(ptr, cbind(x, y))
  a <- data.frame(cell = c(cell(35, 55), cell(65, 55), cell(25, 85)), x = c(35, 65, 25), y = c(55, 55, 85),
                  km2 = c(2, 2.4, 40), dist = c(20, 50, 31.6))               # own stream at two points; a big neighbour off the path
  none <- snap_evidence(a, xy, f, NULL, 50)
  expect_equal(none$picks$largest$km2, 40)
  expect_equal(none$picks$consensus$km2, 2.4)
  expect_equal(none$how, "flowpath (largest downstream)")
  ref <- snap_evidence(a, xy, f, data.frame(lo = 1.5, hi = 2.1, dist_m = 10), 50)
  expect_equal(ref$picks$consensus$km2, 2)
  expect_equal(ref$how, "flowpath+reference")
  wrong <- snap_evidence(a, xy, f, data.frame(lo = 35, hi = 45, dist_m = 10), 50)
  expect_equal(wrong$picks$reference$km2, 40)                                # the reference alone jumps to the neighbour
  expect_equal(wrong$picks$consensus$km2, 2.4)
  expect_true(wrong$conflict)
  off <- snap_evidence(a[3, ], xy, f, NULL, 50)
  expect_equal(off$how, "largest (no evidence)")
})

test_that("consensus and reference snapping on a synthetic confluence, with the alternatives delineated", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  s <- synthetic_confluence(tempfile("conf_"))
  pt <- xy_to_ll(s$xm + 60, s$yj + 60)
  run <- \(snap, ...) suppressMessages(ak_delineate(pt[2], pt[1], "TRIB", dem_source = "local", dem_dir = s$dir, snap = snap,
                                                    start_buffer_m = 9000, max_buffer_m = 9000, flowlines = NULL, plot = FALSE, ...))
  largest <- run("largest")$catchment
  plain <- run("consensus")$catchment
  expect_equal(plain$area_km2, largest$area_km2)                               # no reference: largest downstream = below the junction
  expect_equal(plain$snap_evidence, "flowpath (largest downstream)")

  line <- \(x1, y1, x2, y2) sf::st_sfc(sf::st_linestring(rbind(c(x1, y1), c(x2, y2))), crs = 3338)
  reach <- \(g, lo, hi) sf::st_sf(area_lo_km2 = lo, area_hi_km2 = hi, geometry = g)
  trib_km2 <- plain$snap_area_consensus_km2
  d <- run("consensus", snap_reference = reach(line(s$xm, s$yj, s$xm + 2000, s$yj + 2000), 1, 1), snap_alternatives = TRUE)
  expect_equal(d$catchment$snap_evidence, "flowpath (largest downstream)")
  expect_equal(d$alternatives$rule, "reference")                             # only the reference rule picks another outlet

  # Measure the tributary and the main stem above the junction from the alternatives
  t2 <- run("reference", snap_reference = reach(line(s$xm, s$yj, s$xm + 2000, s$yj + 2000), 5, 0.6 * largest$area_km2),
            snap_alternatives = TRUE)
  trib_km2 <- t2$catchment$area_km2
  expect_lt(trib_km2, 0.7 * largest$area_km2)
  main_above_km2 <- largest$area_km2 - trib_km2
  trib <- reach(line(s$xm, s$yj, s$xm + 2000, s$yj + 2000), 0.5, trib_km2)
  main_above <- reach(line(s$xm, s$yj, s$xm, s$yj + 6000), 0.9 * main_above_km2, main_above_km2)

  agree <- run("consensus", snap_reference = rbind(trib, main_above), snap_alternatives = TRUE)
  expect_equal(agree$catchment$area_km2, trib_km2)
  expect_equal(agree$catchment$snap_evidence, "flowpath+reference")
  expect_setequal(agree$alternatives$rule, "largest")

  wrong <- run("reference", snap_reference = main_above)$catchment                # the main stem above the junction: another watershed
  expect_lt(abs(log(wrong$area_km2 / main_above_km2)), log(1.15))
  # With only the main stem mapped, the consensus stays on the point's flow path: below the junction, as
  # "largest" does (the known limit for unmapped tributaries), with the snap_ambiguous warning
  kept <- run("consensus", snap_reference = main_above)$catchment
  expect_equal(kept$area_km2, largest$area_km2, tolerance = 0.02)
  expect_match(kept$flags, "snap_ambiguous")

  # Same answer on a shared (regional) window
  pts <- data.frame(site_code = c("TRIB", "MAIN"), lat = c(pt[2], xy_to_ll(s$xm, s$yj - 1500)[2]), lon = c(pt[1], xy_to_ll(s$xm, s$yj - 1500)[1]))
  reg <- suppressMessages(ak_catchments(pts, regional = TRUE, snap = "consensus", snap_reference = rbind(trib, main_above),
                                        dem_source = "local", dem_dir = s$dir, start_buffer_m = 9000, max_buffer_m = 9000,
                                        flowlines = NULL, plot = FALSE))
  expect_equal(reg$area_km2[reg$site_code == "TRIB"], trib_km2)

  far <- run("reference", snap_reference = reach(line(s$xm + 5000, s$yj + 5000, s$xm + 5500, s$yj + 5500), 1, 2))$catchment
  expect_equal(far$snap_method, "largest_no_reference")
  expect_equal(far$area_km2, largest$area_km2)
})
