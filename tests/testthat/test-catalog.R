test_that("the catalog and the source registry are one table", {
  cat <- ak_layers()
  expect_true(all(c("layer", "variant", "default", "kind", "returns", "fn", "licence", "cache_folder") %in% names(cat)))
  expect_false(any(duplicated(paste(cat$layer, cat$variant))))
  per_layer <- tapply(cat$default, cat$layer, sum)
  expect_true(all(per_layer[names(per_layer) != "reference_catchment"] == 1))     # one default per layer
  expect_true(all(cat$returns %in% c("columns", "table", "columns; table", "catchments", "function")))
  # every function named in the catalog exists and is exported
  fns <- unique(trimws(unlist(strsplit(cat$fn, ";"))))
  expect_true(all(fns %in% getNamespaceExports("AKCatchments")), info = paste(setdiff(fns, getNamespaceExports("AKCatchments")), collapse = ", "))
  d <- ak_layers("permafrost", details = TRUE)
  expect_true(all(c("citation", "uncertainty_method", "published_error") %in% names(d)))
  expect_equal(nrow(cat), nrow(AKCatchments:::sources_table()))
  expect_error(ak_layers("nope"), "Unknown layer")
  expect_equal(AKCatchments:::layer_row("permafrost")$variant, "jorgenson2008")
  expect_error(AKCatchments:::layer_row("permafrost", "x"), "no variant")
  expect_equal(AKCatchments:::layer_row("alder", "nps_wrst", fn = "ak_alder")$fn, "ak_alder")
})

test_that("points are checked before anything is downloaded", {
  expect_error(AKCatchments:::point_table(data.frame(site_code = c("a", "a"), lat = 61, lon = -150)), "unique")
  expect_error(AKCatchments:::point_table(data.frame(site_code = "a", lat = NA, lon = -150)), "coordinates")
  expect_error(AKCatchments:::point_table(data.frame(id = "a", lat = 61, lon = -150)), "site_code")
  expect_warning(AKCatchments:::point_table(data.frame(site_code = "a", lat = 30, lon = -150)), "Outside Alaska")
  p <- AKCatchments:::point_table(data.frame(site_code = "a", latitude = 61, longitude = -150))
  expect_equal(c(p$lat, p$lon), c(61, -150))
  s <- sf::st_transform(sf::st_sf(site_code = "b", geometry = sf::st_sfc(sf::st_point(c(-150, 61)), crs = 4326)), 3338)
  expect_equal(AKCatchments:::point_table(s)$lat, 61, tolerance = 1e-8)
})

test_that("ak_catchments delineates, records failures, and layer functions / ak_write carry provenance", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  s <- synthetic_dem(tempfile("dem_synth_"))
  ll <- xy_to_ll(s$x0 + 60, s$y0 + 200)
  pts <- data.frame(site_code = c("A", "OUT"), lat = c(ll[2], 64.3), lon = c(ll[1], -140), snap_dist_m = c(200, NA))
  x <- suppressMessages(suppressWarnings(ak_catchments(pts, dem_source = "local", dem_dir = s$dir, start_buffer_m = 10000,
                                                       max_buffer_m = 10000, flowlines = NULL, plot = FALSE)))
  expect_s3_class(x, "ak_catchments")
  expect_equal(x$site_code, "A")
  expect_equal(attr(x, "errors")$site_code, "OUT")
  expect_equal(attr(x, "errors")$step, "delineation")
  expect_true(all(file.exists(c(x$dem_file, x$accum_file, x$pointer_file, x$filled_file))))
  expect_lt(x$snap_dist_m, 200)                                   # per-row snap radius used
  expect_true(all(c("snapped_lon", "snapped_lat") %in% names(x)))
  # step timings
  tcols <- c("t_dem_s", "t_condition_s", "t_watershed_s", "t_finish_s", "t_flowlines_s", "t_plot_s", "t_total_s")
  expect_true(all(c(tcols, "n_windows", "window_mode") %in% names(x)))
  expect_true(all(sf::st_drop_geometry(x)[tcols] >= 0))
  expect_equal(x$window_mode, "site")
  expect_error(ak_catchments(pts, workers = 2), "mirai")
  x <- x |> ak_topography() |> ak_outlet_slope()
  expect_equal(class(x)[1], "ak_catchments")
  expect_true(all(c("topo_hli", "outlet_slope_m_m", "outlet_slope_m_m_200") %in% names(x)))
  expect_equal(x$outlet_slope_m_m, 0.02, tolerance = 0.25)
  pv <- ak_provenance(x)
  expect_equal(pv$layer, c("catchment", "topography", "outlet_slope"))
  expect_equal(pv$fn, c("ak_catchments", "ak_topography", "ak_outlet_slope"))
  expect_equal(pv$variant[1], "local")
  expect_match(pv$args[1], "dem_source = \"local\"")
  expect_message(x2 <- ak_topography(x), "Replacing")
  expect_equal(nrow(ak_provenance(x2)), 4)
  # a long table and catchment_join
  tbl <- dplyr::tibble(site_code = "A", year = 2001:2002, v = 1:2)
  attr(tbl, "ak_provenance") <- AKCatchments:::provenance_row(AKCatchments:::layer_row("fire", fn = "ak_fire"), "v")
  expect_error(catchment_join(x, tbl), "more than one row")
  j <- catchment_join(x, dplyr::summarise(tbl, v_mean = mean(v), .by = "site_code") |> structure(ak_provenance = attr(tbl, "ak_provenance")))
  expect_equal(j$v_mean, 1.5)
  expect_equal(utils::tail(ak_provenance(j)$layer, 1), "fire")
  f <- tempfile(fileext = ".gpkg")
  ak_write(x, f, fire_by_year = tbl)
  expect_setequal(sf::st_layers(f)$name, c("catchments", "provenance", "errors", "fire_by_year"))
  back <- sf::st_read(f, layer = "catchments", quiet = TRUE)
  expect_equal(back$topo_hli, x$topo_hli)
  pv_back <- sf::st_read(f, layer = "provenance", quiet = TRUE)
  expect_equal(pv_back$table, c(rep("catchments", 3), "fire_by_year"))
  expect_error(ak_write(x, f, tbl), "Name the tables")
  expect_output(print(x), "ak_catchments")
  y <- x[1, ]
  expect_equal(nrow(ak_provenance(y)), 3)
  # any sf with a unique site_code works (its rasters as carried columns); duplicated site codes are refused
  hand <- sf::st_sf(site_code = "A", dplyr::select(sf::st_drop_geometry(x), dplyr::ends_with("_file"), "dem_source"), geometry = sf::st_geometry(x))
  h <- ak_topography(hand)
  expect_true("topo_dem_source" %in% names(h))
  expect_equal(ak_provenance(h)$layer, "topography")
  expect_error(ak_topography(rbind(hand, hand)), "more than one row")
})

test_that("regional windows give the same catchments as per-site windows, and share the DEM", {
  skip_if_not(wbt_available(), "WhiteboxTools not installed")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  s <- synthetic_dem(tempfile("dem_synth_"))
  ll1 <- xy_to_ll(s$x0 + 60, s$y0 + 200)
  ll2 <- xy_to_ll(s$x0 + 60, s$y0 + 2200)
  pts <- data.frame(site_code = c("A", "B"), lat = c(ll1[2], ll2[2]), lon = c(ll1[1], ll2[1]))
  run <- \(regional) suppressMessages(ak_catchments(pts, dem_source = "local", dem_dir = s$dir, start_buffer_m = 8000,
                                                    max_buffer_m = 8000, snap_dist_m = 200, flowlines = NULL, plot = FALSE,
                                                    regional = regional))
  a <- run(FALSE)
  b <- run(TRUE)
  expect_equal(b$site_code, a$site_code)
  expect_true(all(startsWith(b$window_mode, "regional:")))
  expect_equal(b$area_km2, a$area_km2, tolerance = 0.02)
  iou <- compare_catchments(a, b)$iou
  expect_true(all(iou > 0.97))
  expect_true(all(file.exists(b$dem_file)))
  # clustering: points farther apart than the span get their own windows
  jobs <- list(A = list(site_code = "A", lat = 61, lon = -150), B = list(site_code = "B", lat = 61.01, lon = -150),
               C = list(site_code = "C", lat = 62, lon = -150))
  g <- AKCatchments:::cluster_jobs(jobs, cluster_km = 20)
  expect_equal(unname(lengths(g)), c(2L, 1L))
  expect_equal(names(g), c("A+1", "C"))
})

test_that("prefetch downloads the Jorgenson map, explains the glaciers without a login, and skips what needs nothing", {
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  local_mocked_bindings(earthdata_netrc = function() NA_character_, ak_download = function(url, dest, query = list(), netrc = NULL) {
    expect_match(url, "^https://catalog\\.northslopescience\\.org/.*\\.zip$")
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(test_path("fixtures", "jorgenson2008.zip"), dest)
    dest
  })
  sq <- sf::st_polygon(list(cbind(c(0, 1000, 1000, 0, 0), c(0, 0, 1000, 1000, 0))))
  x <- sf::st_sf(site_code = "A", geometry = sf::st_sfc(sq, crs = 3338))
  p <- ak_prefetch(x, c("permafrost", "glaciers", "soils", "topography", "weather_daily"))
  expect_equal(p$status, c("ok", "manual", "not needed", "not needed", "failed"))
  expect_match(p$detail[1], "PermafrostCharacteristics\\.shp$")
  expect_match(p$detail[2], "machine urs.earthdata.nasa.gov login", fixed = TRUE)
  expect_match(p$detail[2], "nsidc-0770")
})

test_that("the RGI glaciers are found on NSIDC's server and downloaded with the Earthdata Login in .netrc", {
  dir <- tempfile("rgi_")
  local_mocked_bindings(earthdata_netrc = function() NA_character_)
  expect_error(AKCatchments:::glacier_shapefile(dir), class = "ak_manual_download")
  netrc <- tempfile("netrc_")
  base <- "https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0770_rgi_v7/"
  pages <- list(
    '<a href="?C=N;O=D">Name</a> <a href="/pub/DATASETS/">Parent</a> <a href="global_files/">g</a> <a href="regional_files/">r</a>',
    '<a href="RGI2000-v7.0-G-global.zip">all regions</a>',
    '<a href="RGI2000-v7.0-G/">G</a> <a href="RGI2000-v7.0-C/">C</a>',
    '<a href="RGI2000-v7.0-G-01_alaska.zip">x</a> <a href="RGI2000-v7.0-G-02_western_canada_usa.zip">y</a>'
  )
  names(pages) <- paste0(base, c("", "global_files/", "regional_files/", "regional_files/RGI2000-v7.0-G/"))
  login <- FALSE
  local_mocked_bindings(
    earthdata_netrc = function() netrc,
    ak_perform = function(req) {
      expect_equal(req$options$netrc_file, netrc)
      httr2::response(200, url = if (login) "https://urs.earthdata.nasa.gov/oauth/authorize" else req$url,
                      headers = list(`Content-Type` = "text/html"), body = charToRaw(pages[[req$url]] %||% ""))
    },
    ak_download = function(url, dest, query = list(), netrc = NULL) {
      expect_equal(url, paste0(base, "regional_files/RGI2000-v7.0-G/RGI2000-v7.0-G-01_alaska.zip"))
      expect_equal(netrc, !!netrc)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(test_path("fixtures", "rgi7_alaska.zip"), dest)
      dest
    }
  )
  expect_equal(basename(AKCatchments:::glacier_shapefile(dir)), "RGI2000-v7.0-G-01_alaska.shp")
  login <- TRUE
  expect_error(AKCatchments:::glacier_shapefile(tempfile("rgi_")), "login page")
})

test_that("the Earthdata Login is found in .netrc or _netrc in the home folder", {
  home <- withr::local_tempdir()
  withr::local_envvar(HOME = home, USERPROFILE = "")
  expect_true(is.na(AKCatchments:::earthdata_netrc()))
  writeLines("machine example.org login a password b", file.path(home, ".netrc"))
  expect_true(is.na(AKCatchments:::earthdata_netrc()))       # a netrc without Earthdata Login doesn't count
  writeLines("machine urs.earthdata.nasa.gov login a password b", file.path(home, "_netrc"))
  expect_equal(AKCatchments:::earthdata_netrc(), file.path(home, "_netrc"))
})

test_that("ak_fire appends static columns, or returns a table by year or for sample windows", {
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  sq <- \(x0, s) sf::st_polygon(list(cbind(c(x0, x0 + s, x0 + s, x0, x0), c(0, 0, s, s, 0))))
  x <- sf::st_sf(site_code = "A", other = "kept", geometry = sf::st_sfc(sq(0, 1000), crs = 3338))
  fires <- sf::st_sf(FIREYEAR = c("2001", "2005"), PRESCRIBED = NA, geometry = sf::st_sfc(sq(0, 500), sq(0, 500), crs = 3338))
  cache <- file.path(ak_cache_dir(), "fire_history_ak.gpkg")
  sf::st_write(fires, cache, layer = "perimeters", quiet = TRUE)
  sf::st_write(sf::st_sf(queried_utc = "x", geometry = sf::st_as_sfc(sf::st_bbox(sf::st_buffer(x, 5000)))), cache, layer = "coverage", quiet = TRUE)
  by_year <- suppressMessages(ak_fire(x, by_year = TRUE))
  expect_equal(nrow(by_year), 2)
  expect_equal(ak_provenance(by_year)$layer, "fire")
  expect_false(inherits(by_year, "sf"))
  x <- suppressMessages(ak_fire(x))
  expect_equal(x$other, "kept")                           # the caller's columns stay
  expect_equal(x$fire_burned_pct, 25)
  expect_equal(x$fire_burned_pct_since1987, 25)
  expect_equal(x$fire_last_year, 2005L)
  expect_equal(x$fire_n_years, 2L)
  expect_equal(x$fire_flag, "small_catchment")
  expect_output(print(x), "fire \\(afs_perimeters\\)")
})

test_that("coordinates in another datum are converted, and a silent NAD27 non-shift is caught", {
  p <- data.frame(site_code = "a", lat = 61.7089, lon = -149.2267)
  same <- suppressWarnings(AKCatchments:::point_table(p, crs = 4269))   # PROJ may try (and fail) to fetch grids
  expect_lt(abs(same$lat - 61.7089) + abs(same$lon + 149.2267), 1e-4)       # NAD83 ~ WGS84
  nad27 <- tryCatch(suppressWarnings(AKCatchments:::point_table(p, crs = 4267)), error = \(e) e)
  nad27w <- if (!inherits(nad27, "error")) tryCatch(AKCatchments:::point_table(p, crs = 4267), warning = \(w) w)
  if (inherits(nad27, "error")) {
    expect_match(conditionMessage(nad27), "NADCON")                         # PROJ could not shift at all
  } else if (inherits(nad27w, "warning") && grepl("NADCON", conditionMessage(nad27w))) {
    succeed()                                                               # PROJ applied no shift and said nothing
  } else {
    d <- as.numeric(sf::st_distance(sf::st_sfc(sf::st_point(c(-149.2267, 61.7089)), crs = 4326),
                                    sf::st_sfc(sf::st_point(c(nad27$lon, nad27$lat)), crs = 4326)))
    expect_gt(d, 50)                                                        # a real NAD27 shift
  }
})

test_that("layer functions take a delineation or a list of them, and keep the caller's sf", {
  sqp <- \(x, y, w) sf::st_polygon(list(rbind(c(x, y), c(x + w, y), c(x + w, y + w), c(x, y + w), c(x, y))))
  one <- \(s, x0) structure(list(catchment = sf::st_sf(site_code = s, geometry = sf::st_sfc(sqp(x0, 0, 1000), crs = 3338))),
                            class = "ak_delineation")
  d1 <- one("a", 0); d2 <- one("b", 5000)
  r <- terra::rast(terra::ext(-1000, 7000, -1000, 2000), resolution = 100, crs = "EPSG:3338")
  terra::values(r) <- 1
  expect_equal(catchment_mean(d1, r, "v")$site_code, "a")
  m <- catchment_mean(list(d1, d2), r, "v")
  expect_equal(m$site_code, c("a", "b"))
  expect_equal(m$v, c(1, 1))
  expect_equal(ak_provenance(m)$fn, "catchment_mean")
  ll <- sf::st_transform(dplyr::bind_rows(d1$catchment, d2$catchment), 4326)
  m2 <- catchment_mean(ll, r, "v")
  expect_equal(sf::st_crs(m2)$epsg, 4326)                  # CRS of the input kept
  expect_s3_class(catchment_sf(d1), "sf")
})

test_that("every layer's references build, and ak_citations follows the provenance record", {
  refs <- ak_citations()
  keys <- trimws(unlist(strsplit(AKCatchments:::sources_table()$references, ";")))
  expect_setequal(unique(refs$key), c("akcatchments", keys))
  expect_true(all(nzchar(refs$reference)))
  expect_false(any(grepl("????", refs$reference, fixed = TRUE)))        # every entry has a year
  expect_false(any(grepl("<date>", refs$acknowledgement, fixed = TRUE)))
  sq <- sf::st_polygon(list(cbind(c(0, 1000, 1000, 0, 0), c(0, 0, 1000, 1000, 0))))
  x <- sf::st_sf(site_code = "A", geometry = sf::st_sfc(sq, crs = 3338))
  attr(x, "ak_provenance") <- dplyr::bind_rows(
    AKCatchments:::provenance_row(AKCatchments:::layer_row("climate_normals", "prism8110")),
    AKCatchments:::provenance_row(AKCatchments:::layer_row("glaciers"))
  )
  attr(x, "ak_provenance")$added_utc[1] <- "2026-03-02 10:00:00"
  bib <- tempfile(fileext = ".bib")
  r <- ak_citations(x, layers = "permafrost/pastick2015", bibtex = bib)
  expect_equal(r$key, c("akcatchments", "prism_ak", "pastick2015data", "pastick2015", "rgi2023"))
  expect_match(r$acknowledgement[2], "accessed 2026-03-02")
  expect_match(r$reference[2], "Accessed 2026-03-02")
  expect_equal(sum(startsWith(readLines(bib), "@")), 5)
  expect_error(ak_citations(layers = "nope"), "Unknown layer")
})
