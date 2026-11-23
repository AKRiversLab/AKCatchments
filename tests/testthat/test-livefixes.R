# Fixes from the full live run of 2026-11-03 (H03 geology, H05 alder, H06 Pastick)

square <- function(x0 = 200000, y0 = 1600000, d = 2000)
  sf::st_sf(site_code = "A", geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(x0, y0), c(x0 + d, y0), c(x0 + d, y0 + d), c(x0, y0 + d), c(x0, y0)))), crs = 3338))

test_that("bbox_wkt reads the layer CRS without SQL and names the layers when one is missing", {
  gdb <- tempfile(fileext = ".gdb")
  sf::st_write(sf::st_transform(square(), 4269), gdb, "polys", driver = "OpenFileGDB", quiet = TRUE)
  w <- AKCatchments:::bbox_wkt(sf::st_geometry(square()), gdb, "polys")
  expect_equal(sf::st_crs(sf::st_read(gdb, "polys", wkt_filter = w, quiet = TRUE))$epsg, 4269L)
  expect_lt(abs(sf::st_bbox(sf::st_as_sfc(w))[["xmin"]] - sf::st_bbox(sf::st_transform(square(), 4269))[["xmin"]]), 1e-4)
  expect_error(AKCatchments:::bbox_wkt(sf::st_geometry(square()), gdb, "nope"), "it has: polys")
})

test_that("ak_geology uses the geodatabase that holds the map units, even when another comes first", {
  dir <- ak_cache_dir(AKCatchments:::cache_paths$geology)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sf::st_write(square(), file.path(dir, "aaa_other.gdb"), "something_else", driver = "OpenFileGDB", quiet = TRUE)
  g <- file.path(dir, "sim3340.gdb")
  units <- sf::st_sf(NSACLASS = c("1", "2"), geometry = sf::st_geometry(rbind(square(d = 1000), square(200000 + 1000, d = 3000))))
  sf::st_write(units, g, "AKStategeol_poly", driver = "OpenFileGDB", quiet = TRUE)
  sf::st_write(data.frame(NSACLASS = c("1", "2"), LITH1 = c("limestone", "basalt"), RANK = "major"), g, "NSALith",
               driver = "OpenFileGDB", quiet = TRUE)
  x <- dplyr::mutate(square(), area_m2 = as.numeric(sf::st_area(geometry)))
  ge <- ak_geology(x, details = TRUE)
  expect_equal(nrow(ge$catchments), 1)
  expect_true("geol_mapped_pct" %in% names(ge$catchments))
  expect_true("geol_mapped_pct" %in% names(ak_geology(x)))
  expect_setequal(ge$units$NSACLASS, c("1", "2"))
  unlink(g, recursive = TRUE)
  expect_error(ak_geology(x), "aaa_other.gdb has something_else")
})

test_that("ak_alder returns alnus_coverage_pct and flags partial coverage", {
  dir <- ak_cache_dir(AKCatchments:::cache_paths$alder)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  r <- terra::rast(terra::ext(200000, 201000, 1600000, 1602000), resolution = 10, crs = "EPSG:3338", vals = 20)
  terra::writeRaster(r, file.path(dir, "alnus.tif"))
  al <- ak_alder(dplyr::mutate(square(), area_m2 = as.numeric(sf::st_area(geometry))))
  expect_true(all(c("alnus_cover", "alnus_coverage_pct", "alnus_cover_se", "alnus_cover_flag") %in% names(al)))
  expect_equal(al$alnus_cover, 20)
  expect_equal(al$alnus_coverage_pct, 50, tolerance = 1e-6)
  expect_match(al$alnus_cover_flag, "partial_coverage")
})

test_that("a Pastick archive that holds a .tar is unpacked through to the raster, and the .tar removed", {
  skip_if_not_installed("archive")
  dir <- ak_cache_dir(AKCatchments:::cache_paths$pastick)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  src <- tempfile(); dir.create(src)
  terra::writeRaster(terra::rast(nrows = 2, ncols = 2, vals = 1:4, crs = "EPSG:3338"), file.path(src, "AK_PFProbability_1m.tif"))
  tar_file <- file.path(dir, "AK_PFProbability_1m.tar")
  old_wd <- setwd(src); utils::tar(tar_file, "AK_PFProbability_1m.tif"); setwd(old_wd)
  r <- AKCatchments:::pastick_raster()
  expect_equal(as.vector(terra::values(r)), 1:4)
  expect_false(file.exists(tar_file))
})

test_that("ak_alder gives NA and outside_map for a catchment off the map, not NaN", {
  dir <- ak_cache_dir(AKCatchments:::cache_paths$alder)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  r <- terra::rast(terra::ext(100000, 101000, 1600000, 1602000), resolution = 10, crs = "EPSG:3338", vals = 20)
  terra::writeRaster(r, file.path(dir, "alnus.tif"))
  x <- dplyr::mutate(square(), area_m2 = as.numeric(sf::st_area(geometry)))
  al <- ak_alder(x)
  expect_true(is.na(al$alnus_cover) && !is.nan(al$alnus_cover))
  expect_true(is.na(al$alnus_cover_se) && is.na(al$alnus_cover_lo95))
  expect_equal(al$alnus_coverage_pct, 0)
  expect_equal(al$alnus_cover_flag, "outside_map")
})

# A 1 x 1 km grid at 10 m in strips of 10 columns, with an NPS WRST-style attribute table:
# values 11-15 recorded by species (ISO_CL, alder 30-50) and 16 not (GRSAIR, mixed tall shrub) in one class;
# 21 GRSAIR with alder as the primary component (70%) in a class of its own; 0 unmapped
wrst_fixture <- function() {
  dir <- file.path(ak_cache_dir(AKCatchments:::cache_paths$nps_wrst), "detailed")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  r <- terra::rast(terra::ext(200000, 201000, 1600000, 1601000), resolution = 10, crs = "EPSG:3338")
  strip <- c(11:16, 21, 0, 0, 0)
  terra::values(r) <- rep(rep(strip, each = 10), times = 100)
  levels(r) <- data.frame(VALUE = c(0, 11:16, 21),
                          SOURCE_COVER = c("", rep("ISO_CL", 5), "GRSAIR", "GRSAIR"),
                          CALC_CLASS = c("", rep("Tall shrub:Closed:Mix", 6), "Tall shrub:Closed:Alder"),
                          MAJOR_CLASS = c("", rep("Tall Shrub", 7)),
                          PR_COMP = c("", rep("Tall shrub mix", 6), "Tall shrub aln"),
                          PR_COMP_COVER = c(0, rep(60, 6), 70), ALNUS_COVER = c(0, 30, 35, 40, 45, 50, 0, 0))
  terra::writeRaster(r, file.path(dir, "lc_wrst.tif"), overwrite = TRUE)
}
strip_poly <- function(id, x0, x1) sf::st_sf(site_code = id, geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = x0, ymin = 1600000, xmax = x1, ymax = 1601000), crs = 3338)))

test_that("ak_alder(source = 'nps_wrst') averages signature alder, fills unrecorded signatures from the class, and gives the exact SE", {
  on.exit(unlink(ak_cache_dir(AKCatchments:::cache_paths$nps_wrst), recursive = TRUE), add = TRUE)
  wrst_fixture()
  x <- rbind(strip_poly("A", 200500, 200700),   # half value 16 (unrecorded), half 21 (alder 70)
             strip_poly("B", 200600, 200800),   # half 21, half unmapped
             strip_poly("C", 200800, 201000),   # unmapped only
             strip_poly("D", 300000, 301000))   # outside the grid
  s_class <- sd(c(30, 35, 40, 45, 50))           # class of value 16 (5 recorded signatures)
  s_life <- sd(c(30, 35, 40, 45, 50, 70))        # value 21's class has 1 recorded signature: life-form SD
  a <- ak_alder(x, source = "nps_wrst")
  expect_equal(a$alnus_wrst_cover[1], (40 + 70) / 2)
  expect_equal(a$alnus_wrst_cover_se[1], sqrt(0.25 * s_class^2 + 0.25 * s_life^2))
  expect_equal(a$alnus_wrst_unrecorded_pct[1], 50)
  expect_equal(a$alnus_wrst_coverage_pct[1:2], c(100, 50))
  expect_equal(a$alnus_wrst_cover[2], 70)
  expect_equal(a$alnus_wrst_cover_se[2], s_life)
  expect_match(a$alnus_wrst_cover_flag[2], "partial_coverage")
  expect_true(all(is.na(a$alnus_wrst_cover[3:4])))
  expect_equal(a$alnus_wrst_coverage_pct[3:4], c(0, 0))
  expect_equal(a$alnus_wrst_cover_flag[3:4], c("outside_map", "outside_map"))
  expect_true(all(a$alnus_wrst_cover_lo95[1:2] < a$alnus_wrst_cover[1:2] & a$alnus_wrst_cover[1:2] < a$alnus_wrst_cover_hi95[1:2]))
  z <- ak_alder(x[1, ], source = "nps_wrst", unrecorded = "zero")
  expect_equal(z$alnus_wrst_cover, 70 / 2)
  expect_equal(z$alnus_wrst_cover_se, a$alnus_wrst_cover_se[1])
})

test_that("the NPS WRST alder is a catalog alternate and sits beside the ACCS columns", {
  on.exit(unlink(ak_cache_dir(AKCatchments:::cache_paths$nps_wrst), recursive = TRUE), add = TRUE)
  wrst_fixture()
  expect_true("nps_wrst" %in% ak_layers("alder")$variant)
  expect_false(ak_layers("alder")$default[ak_layers("alder")$variant == "nps_wrst"])
  x <- AKCatchments:::ak_class(strip_poly("A", 200500, 200700))
  y <- ak_alder(x, "nps_wrst", unrecorded = "zero")
  expect_equal(y$alnus_wrst_cover, 35)
  expect_equal(attr(y, "ak_provenance")$variant, "nps_wrst")
})

test_that("ak_permafrost reads the 2008 shapefile's letter codes, and stops if no extent class is recognised", {
  half <- \(x0, code, ice) sf::st_sf(PF_EXTENT = code, ICECLOWASS = ice, TEXTURE = "Silty",
                                     geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = x0, ymin = 1590000, xmax = x0 + 1e4, ymax = 1620000), crs = 3338)))
  dir <- tempfile("jorg_"); dir.create(dir)
  sf::st_write(rbind(half(190000, "C", "High"), half(200000, "U", "Unfrozen"), half(210000, "C", "High")),
               file.path(dir, "Permafrost_AK_2008_AlbAK83.shp"), quiet = TRUE)
  x <- sf::st_sf(site_code = "A", geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = 195000, ymin = 1600000, xmax = 205000, ymax = 1610000), crs = 3338)))
  p <- ak_permafrost(x, path = dir, sigma_m = 0, n = 50)
  expect_equal(p$jorg_pct_continuous, 50)
  expect_equal(p$jorg_pct_unfrozen, 50)
  expect_equal(p$groundice_pct_high, 50)
  expect_equal(p$totperm_pct, 0.95 * 50)
  dir2 <- tempfile("jorg_"); dir.create(dir2)
  sf::st_write(rbind(half(190000, "X", "Low"), half(200000, "Y", "Low")), file.path(dir2, "pf.shp"), quiet = TRUE)
  expect_error(ak_permafrost(x, path = dir2, ext_field = "PF_EXTENT", sigma_m = 0, n = 10), "No permafrost extent class recognised")
})
