# Image-service windows come from tiles on a fixed grid: overlapping windows share tiles, and each window is cut
# exactly to its own extent.
test_that("overlapping windows share tiles and are cut to their own extent", {
  cache <- tempfile("akcache_")
  old <- options(AKCatchments.cache = cache)
  on.exit(options(old), add = TRUE)
  asked <- character()
  local_mocked_bindings(ak_download = function(url, dest, query = list()) {
    asked <<- c(asked, query$bbox)
    bb <- as.numeric(strsplit(query$bbox, ",")[[1]])
    size <- as.numeric(strsplit(query$size, ",")[[1]])
    r <- terra::rast(terra::ext(bb[1], bb[3], bb[2], bb[4]), ncols = size[1], nrows = size[2], crs = "EPSG:3338", vals = 1)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    terra::writeRaster(r, dest)
    dest
  })
  window <- \(x, y, half) sf::st_as_sfc(sf::st_bbox(c(xmin = x - half, ymin = y - half, xmax = x + half, ymax = y + half), crs = sf::st_crs(3338)))
  d1 <- AKCatchments:::dem_imageserver("https://example.org/ImageServer", window(250100, 1500100, 4000), 5, 1000, "test")
  n1 <- length(asked)
  expect_equal(n1, 4)                                    # 8 km window over the 5 km grid: 2 x 2 tiles
  expect_equal(unname(as.vector(terra::ext(d1))), c(246100, 254100, 1496100, 1504100))
  d2 <- AKCatchments:::dem_imageserver("https://example.org/ImageServer", window(251000, 1501000, 2000), 5, 1000, "test")
  expect_equal(length(asked), n1)                         # inside the first window's tiles: nothing new
  expect_equal(terra::res(d2), c(5, 5))
  expect_equal(unname(as.vector(terra::ext(d2))), c(249000, 253000, 1499000, 1503000))
  AKCatchments:::dem_imageserver("https://example.org/ImageServer", window(256000, 1500100, 4000), 5, 1000, "test")
  expect_equal(length(asked), n1 + 2)                     # shifted east: only the new column of tiles
  expect_true(all(grepl("^test_5m_-?[0-9]+_-?[0-9]+\\.tif$", basename(list.files(file.path(cache, "dem_windows", "test"))))))
})
