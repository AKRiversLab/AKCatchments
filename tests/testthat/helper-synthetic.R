# Synthetic basin: an elliptical bowl (semi-axes 2 x 4 km) draining south through
# a gap in its rim, with a channel along x = x0. True area above the outlet is
# about pi * 2 * 4 = 25.1 km2.
synthetic_dem <- function(dir, res = 20) {
  xg <- 200000; y0 <- 1600000; a <- 2000; b <- 4000; yc <- y0 + 4000
  x0 <- xg + res / 2
  r <- terra::rast(terra::ext(xg - 10000, xg + 10000, y0 - 6000, y0 + 14000), resolution = res, crs = "EPSG:3338")
  xy <- terra::crds(r)
  zin <- 100 + 0.02 * (xy[, 2] - y0) + 0.1 * abs(xy[, 1] - x0)
  rr <- sqrt(((xy[, 1] - x0) / a)^2 + ((xy[, 2] - yc) / b)^2)
  gap <- abs(xy[, 1] - x0) < 3 * res & xy[, 2] < y0 + 400
  z <- ifelse(rr < 1 | gap, zin, zin + 50 - 0.2 * (rr - 1) * a)
  terra::values(r) <- z
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  terra::writeRaster(r, file.path(dir, "synthetic_dem.tif"), overwrite = TRUE)
  list(dir = dir, x0 = x0, y0 = y0, area_km2 = pi * a * b / 1e6)
}

xy_to_ll <- function(x, y) sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(x, y)), crs = 3338), 4326))[1, ]

wbt_available <- function() isTRUE(suppressMessages(whitebox::wbt_init()))

# Synthetic confluence: a main valley along x = xm draining south, joined at (xm, yj) by a tributary valley
# coming in from the north-east at 45 degrees. A point on the tributary just above the junction has the
# main stem within the snap radius, which is where "largest" snapping goes wrong.
synthetic_confluence <- function(dir, res = 20) {
  xm <- 300000 + res / 2; y0 <- 1700000; yj <- y0 + 2000 + res / 2
  r <- terra::rast(terra::ext(xm - 9000 - res / 2, xm + 6000 - res / 2, y0, y0 + 9000), resolution = res, crs = "EPSG:3338")
  xy <- terra::crds(r)
  dx <- xy[, 1] - xm; dy <- xy[, 2] - yj
  s <- pmax(0, (dx + dy) / 2)
  d_trib <- ifelse(dx > 0 & dy > 0, sqrt((dx - s)^2 + (dy - s)^2), Inf)
  terra::values(r) <- 100 + 0.02 * (xy[, 2] - y0) + 0.15 * pmin(abs(dx), d_trib)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  terra::writeRaster(r, file.path(dir, "synthetic_confluence.tif"), overwrite = TRUE)
  list(dir = dir, xm = xm, yj = yj, res = res)
}
