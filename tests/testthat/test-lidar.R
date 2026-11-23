# Lidar DEM sources: the project index, the choice of project for a window, and the "best" fallback to IfSAR.
# The services are mocked; the live tests (B06-B08, D07) use the real ones.

# A 4 km square window in Alaska Albers and a raster over it (1 m of elevation per km east)
window_4km <- function() sf::st_as_sfc(sf::st_bbox(c(xmin = 200000, ymin = 1200000, xmax = 204000, ymax = 1204000), crs = 3338))
raster_over <- function(aoi, res, na_cells = 0) {
  r <- terra::rast(terra::ext(sf::st_bbox(aoi)[c("xmin", "xmax", "ymin", "ymax")]), resolution = res, crs = "EPSG:3338")
  terra::values(r) <- 100 + (terra::xFromCell(r, seq_len(terra::ncell(r))) - 200000) / 1000
  if (na_cells) r[seq_len(na_cells)] <- NA
  r
}
fake_index <- function() {
  big <- sf::st_buffer(window_4km(), 1000)
  sf::st_sf(source = c("dggs", "dggs", "3dep"), project = c("old_2012_dtm", "new_2021_dtm", "AK_Project_2016"),
            year = c(2012L, 2021L, 2016L), service = c("https://example.org/old_2012_dtm/ImageServer",
                                                        "https://example.org/new_2021_dtm/ImageServer", NA),
            z_to_m = c(0.3048, 1, 1), geometry = c(big, big, big))
}

test_that("the newest project with elevation for the whole window is used, in metres", {
  asked <- character()
  local_mocked_bindings(
    ak_lidar_index = function(refresh = FALSE) fake_index(),
    dem_imageserver = function(service, aoi, res, tile_px, name) {
      asked <<- c(asked, name)
      raster_over(aoi, res, na_cells = if (name == "new_2021_dtm") 10 else 0)   # the newest has a gap
    },
    dem_3dep = function(aoi, res, project) { asked <<- c(asked, project); raster_over(aoi, res, na_cells = 5) })
  d <- ak_dem(window_4km(), "lidar", res = 2)
  expect_equal(asked, c("new_2021_dtm", "AK_Project_2016", "old_2012_dtm"))   # newest first
  expect_equal(AKCatchments:::dem_tag(d, "lidar_project"), "old_2012_dtm")
  expect_equal(AKCatchments:::dem_source(d), "lidar_2m")
  expect_equal(terra::global(d, "min")[[1]], 100 * 0.3048, tolerance = 1e-3)   # feet to metres
})

test_that("lidar errors name each project tried, and best falls back to IfSAR at 5 m", {
  local_mocked_bindings(
    ak_lidar_index = function(refresh = FALSE) fake_index(),
    dem_imageserver = function(service, aoi, res, tile_px, name) {
      if (name == "ifsar") return(raster_over(aoi, res))
      raster_over(aoi, res, na_cells = 3)
    },
    dem_3dep = function(aoi, res, project) stop("tile server down"))
  expect_error(ak_dem(window_4km(), "lidar"), "new_2021_dtm: 3 cells without elevation.*AK_Project_2016: tile server down")
  d <- ak_dem(window_4km(), "best")
  expect_equal(AKCatchments:::dem_source(d), "ifsar_5m")
  expect_true(is.na(AKCatchments:::dem_tag(d, "lidar_project")))
})

test_that("a window outside every footprint is not a lidar window", {
  local_mocked_bindings(ak_lidar_index = function(refresh = FALSE) fake_index())
  far <- sf::st_as_sfc(sf::st_bbox(c(xmin = 500000, ymin = 1200000, xmax = 504000, ymax = 1204000), crs = 3338))
  expect_error(ak_dem(far, "lidar"), "No lidar project")
})

test_that("the state index keeps lidar projects with a DTM service and known vertical units", {
  sq <- \(x0) sf::st_as_sfc(sf::st_bbox(c(xmin = x0, ymin = 0, xmax = x0 + 1, ymax = 1), crs = 3338))
  x <- sf::st_sf(DATA_TYPE = c("Lidar", "lidar", "IfSAR", "Lidar", "Lidar"),
                 VERT_UNITS = c("US Survey Feet", "Meters", "Meters", "Meters", NA),
                 C_YEAR = c(2015L, NA, 2012L, 2019L, 2020L),
                 image_services = c('<a href="https://geo.example/arcgis/rest/services/elevation/anchorage_2015_dtm/ImageServer">DTM</a>',
                                    "https://geo.example/arcgis/rest/services/kenai_2019_dtm/ImageServer, https://x/kenai_dsm/ImageServer",
                                    "https://geo.example/arcgis/rest/services/elevation/IFSAR_DTM/ImageServer",
                                    "https://geo.example/arcgis/rest/services/elevation/somewhere_dsm/ImageServer",
                                    "https://geo.example/arcgis/rest/services/elevation/units_unknown_dtm/ImageServer"),
                 geometry = c(sq(0), sq(2), sq(4), sq(6), sq(8)))
  idx <- AKCatchments:::lidar_index_state(x)
  expect_equal(idx$project, c("anchorage_2015_dtm", "kenai_2019_dtm"))
  expect_equal(idx$z_to_m, c(1200 / 3937, 1))
  expect_equal(idx$year, c(2015L, 2019L))                                    # from the project name when C_YEAR is missing
  expect_match(idx$service[2], "kenai_2019_dtm/ImageServer$")
  expect_error(AKCatchments:::lidar_index_state(x[c("DATA_TYPE")]), "lacks vert_units, c_year")
})

test_that("the 3DEP index keeps every project, and an empty answer gives an empty index", {
  sq <- \(x0) sf::st_as_sfc(sf::st_bbox(c(xmin = x0, ymin = 0, xmax = x0 + 1, ymax = 1), crs = 3338))
  idx <- AKCatchments:::lidar_index_3dep(sf::st_sf(Project = c("AK_Kenai_2019_D19", "AK_Juneau"), geometry = c(sq(0), sq(2))))
  expect_equal(idx$source, c("3dep", "3dep"))
  expect_equal(idx$year, c(2019L, NA))
  expect_equal(idx$z_to_m, c(1, 1))
  empty <- AKCatchments:::lidar_index_3dep(sf::st_sf(geometry = sf::st_sfc(crs = 3338)))
  expect_equal(nrow(empty), 0)
  expect_named(empty, c("source", "project", "year", "service", "z_to_m", "geometry"))
})

test_that("lidar starts from smaller windows, and DEM defaults agree", {
  expect_equal(AKCatchments:::start_buffer(NA, "lidar"), 2000)
  expect_equal(AKCatchments:::start_buffer(NA, "ifsar"), 5000)
  expect_equal(AKCatchments:::start_buffer(100, "best"), 2 * sqrt(100e6))
  expect_equal(AKCatchments:::default_res(c("ifsar", "lidar", "best", "copernicus", "local")), c(5, 2, 2, 30, NA))
  expect_equal(AKCatchments:::first_year(c("AK_Anchorage_2015", "no year", "x1999y2003")), c(2015L, NA, 1999L))
})
