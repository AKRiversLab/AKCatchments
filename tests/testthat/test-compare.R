sq <- function(x, y, w) sf::st_polygon(list(rbind(c(x, y), c(x + w, y), c(x + w, y + w), c(x, y + w), c(x, y))))

test_that("polygon comparison measures overlap correctly", {
  a <- sf::st_sf(site_code = c("s1", "s2"), geometry = sf::st_sfc(sq(0, 0, 1000), sq(5000, 0, 1000), crs = 3338))
  b <- sf::st_sf(site_code = c("s1", "s2"), geometry = sf::st_sfc(sq(0, 0, 1000), sq(5500, 0, 1000), crs = 3338))
  cmp <- compare_catchments(a, b)
  expect_equal(cmp$iou, c(1, 1 / 3), tolerance = 1e-6)
  expect_equal(cmp$area_ratio, c(1, 1))
  expect_equal(cmp$agree, c(TRUE, FALSE))
  expect_equal(cmp$hausdorff_m[2], 500, tolerance = 1e-6)
})

test_that("flags respond to diagnostics", {
  x <- sf::st_sf(area_km2 = c(10, 10, 0.01), ref_area_km2 = c(10, 30, NA), snap_method = c("ref_area", "ref_area", "none"),
                 snap_dist_m = c(10, 149, 0), snap_ambiguous = c(FALSE, TRUE, FALSE), flat_frac = c(0.1, 0.5, 0),
                 relief_m = c(500, 500, 10), n_parts = c(1, 1, 2), truncated = c(FALSE, FALSE, TRUE),
                 geometry = sf::st_sfc(sq(0, 0, 1), sq(0, 0, 1), sq(0, 0, 1), crs = 3338))
  f <- AKCatchments:::flag_delineation(x, snap_dist_m = 150)
  expect_equal(f$delin_ok, c(TRUE, FALSE, FALSE))
  expect_equal(f$flags[1], "")
  expect_match(f$flags[2], "area_mismatch.*snap_at_limit.*snap_ambiguous.*low_relief")
  expect_match(f$flags[3], "truncated.*no_channel.*multipart.*tiny")
})
