test_that("own tables map into the observation schema, wide or long", {
  chem <- data.frame(site = c("S1", "S1", "S2"), collected = c("2019-07-01", "2020-07-03", "2019-07-02"),
                     lat = 63.7, lon = -149, tn_mg_l = c(0.21, 0.18, 0.3), tp_mg_l = c(0.004, NA, 0.01))
  o <- ak_obs_read(chem, .pivot = c(tn_mg_l, tp_mg_l), dataset = "CAKN chemistry", provider = "NPS", station_id = site,
                   lat = lat, lon = lon, date = as.Date(collected), variable = sub("_mg_l$", "", variable), value = value, unit = "mg/L")
  expect_equal(names(o)[1:16], names(ak_obs_schema()))
  expect_equal(nrow(o), 5)                                         # the NA tp is dropped
  expect_type(o$value, "double")
  expect_s3_class(o$date, "Date")
  st <- ak_obs_stations(o)
  expect_s3_class(st, "sf")
  expect_equal(st$n, c(3, 2))
  expect_error(ak_obs_validate(data.frame(dataset = "d", station_id = "s")), "required")
  bad <- dplyr::bind_rows(o, o[1, ])
  expect_error(ak_obs_validate(bad), "duplicated_rows")
  expect_warning(w <- ak_obs_validate(bad, strict = FALSE), "duplicated_rows")
  expect_equal(attr(w, "problems")[["duplicated_rows"]], 1)
  mixed <- o
  mixed$unit[1] <- "ug/L"
  expect_error(ak_obs_validate(mixed), "several_units")
})

test_that("stations are linked to catchments: at the outlet, inside with their share, or near", {
  sq <- \(x0, y0, s) sf::st_polygon(list(cbind(c(x0, x0 + s, x0 + s, x0, x0), c(y0, y0, y0 + s, y0 + s, y0))))
  catch <- sf::st_sf(site_code = "A", geometry = sf::st_sfc(sq(200000, 1600000, 2000), crs = 3338))
  o <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(201000, 1600010)), crs = 3338), 4326))
  catch$snapped_lon <- o[1, 1]
  catch$snapped_lat <- o[1, 2]
  acc <- terra::rast(terra::ext(199000, 203000, 1599000, 1603000), resolution = 100, crs = "EPSG:3338", vals = 40)
  f <- tempfile(fileext = ".tif"); terra::writeRaster(acc, f)
  catch$dem_file <- catch$filled_file <- catch$pointer_file <- f
  catch$accum_file <- f
  st <- sf::st_sf(station_id = c("out", "up", "near", "far"), dataset = "t",
                  geometry = sf::st_sfc(sf::st_point(c(201000, 1600100)), sf::st_point(c(201000, 1601500)),
                                        sf::st_point(c(202300, 1601000)), sf::st_point(c(210000, 1601000)), crs = 3338))
  l <- ak_obs_link(catch, st, outlet_m = 300, near_m = 500)
  expect_equal(l$station_id, c("out", "up", "near"))
  expect_equal(l$relation, c("at_outlet", "inside", "near"))
  expect_equal(l$dist_catchment_m, c(0, 0, 300))
  expect_equal(l$station_area_km2[2], 0.4)                          # 40 cells of 100 m
  expect_equal(l$station_area_frac[2], 0.1)
  expect_equal(nrow(ak_obs_link(catch, st[4, ])), 0)
})

test_that("USGS parameter codes get readable names", {
  expect_equal(AKCatchments:::usgs_variable(c("00060", "00010", "99999")), c("discharge", "water_temperature", "p99999"))
})
