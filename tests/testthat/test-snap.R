# A fake SNAP rasdaman server: a 10 x 8 grid of 4 km cells whose daily values are a known function of cell, day,
# model and run. It answers GetCoverage with nested JSON in the order [time][Y, south first][X, west first], so
# the package has to work out the axis order and flip Y itself, as it would with an unknown server layout.
fake_grid <- list(xmin = 100000, xmax = 140000, xres = 4000, ymin = 1500000, ymax = 1532000, yres = 4000,
                  time_label = "ansi", nil = -9999, uom = "C")

fake_describe <- function(cov, refresh = FALSE) {
  run <- sub(".*_(historical|ssp\\d+)_v2_wcs$", "\\1", cov)
  t <- if (run == "historical") seq(as.Date("1990-01-01"), as.Date("2014-12-31"), "day")
       else seq(as.Date("2015-01-01"), as.Date("2100-12-31"), "day")
  c(fake_grid, list(time = t))
}

fake_value <- function(cov, t, col, row) {
  var <- sub("^cmip6_downscaled_(tasmax|tasmin|pr)_.*", "\\1", cov)
  model <- sub("^cmip6_downscaled_[a-z]+_(.*)_(historical|ssp\\d+)_v2_wcs$", "\\1", cov)
  run <- sub(".*_(historical|ssp\\d+)_v2_wcs$", "\\1", cov)
  m <- if (model == "MIROC6") 1 else 0                                     # model difference in the change
  yr <- as.integer(format(t, "%Y"))
  doy <- as.integer(format(t, "%j"))
  warm <- if (run == "historical") 0 else (3 + m) * (yr >= 2041)            # step change from 2041
  iav <- outer(yr, col, \(y, c) sin(y * 1.7 + c))                         # year-to-year variability
  base <- outer(10 * sin(2 * pi * doy / 365) + warm, 0.1 * col - 0.05 * row, `+`) + iav
  switch(var, tasmax = base + 5, tasmin = base - 5,
         pr = outer((2 + 0.5 * sin(yr * 2.3)) * (if (run == "historical") 1 else 1 + (0.1 + 0.05 * m) * (yr >= 2041)),
                    1 + 0.1 * col, `*`))
}

fake_server <- function() {
  calls <- 0
  get <- function(url) {
    calls <<- calls + 1
    q <- utils::URLdecode(url)
    cov <- sub(".*COVERAGEID=([^&]+).*", "\\1", q)
    rng <- \(ax) as.numeric(strsplit(sub(paste0(".*SUBSET=", ax, "\\(([^)]+)\\).*"), "\\1", q), ",")[[1]])
    tt <- as.Date(regmatches(q, gregexpr("\\d{4}-\\d{2}-\\d{2}", q))[[1]])
    xr <- rng("X"); yr <- rng("Y")
    cx <- fake_grid$xmin + (0:9 + 0.5) * 4000
    cy <- fake_grid$ymax - (0:7 + 0.5) * 4000
    cols <- which(cx >= min(xr) - 1 & cx <= max(xr) + 1) - 1
    rows <- which(cy >= min(yr) - 1 & cy <= max(yr) + 1) - 1
    t <- seq(min(tt), max(tt), "day")
    rows <- rev(sort(rows))                                                 # Y south first
    a <- array(NA_real_, c(length(t), length(rows), length(cols)))
    for (i in seq_along(rows)) a[, i, ] <- fake_value(cov, t, cols, rows[i])
    if (length(rows) == 1 && length(cols) == 1) return(jsonlite::toJSON(as.numeric(a), digits = NA))
    jsonlite::toJSON(a, digits = NA)
  }
  list(get = get, calls = \() calls)
}

snap_test_catchments <- function() {
  sq <- \(x, y, w, h = w) sf::st_polygon(list(rbind(c(x, y), c(x + w, y), c(x + w, y + h), c(x, y + h), c(x, y))))
  sf::st_sf(site_code = c("big", "tiny"),
            geometry = sf::st_sfc(sq(108000, 1516000, 8000, 4000), sq(125500, 1509500, 1000), crs = 3338))
}

test_that("SNAP deltas recover a known change, flip the axes, and use the cache", {
  options(AKCatchments.snap_flip = NULL)
  srv <- fake_server()
  local_mocked_bindings(snap_describe = fake_describe, snap_get = srv$get)
  x <- snap_test_catchments()
  args <- list(x, scenarios = "ssp245", models = c("GFDL-ESM4", "MIROC6"), baseline = 1995:2014,
               periods = list(late = 2071:2100), seasons = ak_seasons(months = FALSE))
  d <- do.call(ak_climate_projection, args)
  expect_equal(getOption("AKCatchments.snap_flip"), c(FALSE, TRUE))
  tm <- d[d$var == "tmean" & d$season == "annual", ]
  expect_equal(tm$delta[tm$model == "GFDL-ESM4"], c(3, 3), tolerance = 0.05)
  expect_equal(tm$delta[tm$model == "MIROC6"], c(4, 4), tolerance = 0.05)
  expect_true(all(tm$delta_se > 0 & tm$delta_se < 0.5))
  # the catchment value is the weighted mean of its cells: big covers cols 2-3 (x offsets 0.2, 0.3) of row 3
  b <- d[d$var == "tasmax" & d$site_code == "big" & d$season == "annual" & d$model == "GFDL-ESM4", ]
  expect_equal(b$n_cells, 2)
  expect_equal(b$coverage_pct, 100, tolerance = 1e-6)
  pr <- d[d$var == "pr" & d$season == "jja", ]
  yf <- \(y) mean(2 + 0.5 * sin(y * 2.3))                  # the fake's year-to-year precipitation factor
  expect_equal(pr$ratio[pr$model == "GFDL-ESM4"], rep(1.1 * yf(2071:2100) / yf(1995:2014), 2), tolerance = 1e-6)
  expect_equal(pr$ratio[pr$model == "MIROC6"], rep(1.15 * yf(2071:2100) / yf(1995:2014), 2), tolerance = 1e-6)
  # JJA precipitation is a total over 92 days
  expect_equal(pr$baseline_mean[pr$site_code == "tiny" & pr$model == "MIROC6"], 92 * yf(1995:2014) * (1 + 0.1 * 6),
               tolerance = 1e-6)
  expect_true(all(d$subgrid[d$site_code == "tiny"]))
  expect_false(any(d$subgrid[d$site_code == "big"]))
  n1 <- srv$calls()
  d2 <- do.call(ak_climate_projection, args)
  expect_equal(srv$calls(), n1)                         # everything from the cache the second time
  expect_equal(d2$delta, d$delta)
  e <- projection_ensemble(d)
  et <- e[e$var == "tmean" & e$season == "annual" & e$site_code == "big", ]
  expect_equal(et$delta, 3.5, tolerance = 0.05)
  expect_equal(et$n_models, 2)
  expect_match(et$flag, "few_models")
  expect_gt(et$delta_hi95 - et$delta_lo95, et$delta_max - et$delta_min)   # prediction interval wider than the range
  ep <- e[e$var == "pr" & e$season == "jja" & e$site_code == "big", ]
  expect_equal(ep$pct, 100 * (sqrt(1.1 * 1.15) * yf(2071:2100) / yf(1995:2014) - 1), tolerance = 1e-6)
  expect_match(e$flag[e$site_code == "tiny"][1], "subgrid")
  options(AKCatchments.snap_flip = NULL)
})

test_that("wide ensemble columns are named by variable, season, scenario and period, and join onto catchments", {
  options(AKCatchments.snap_flip = NULL)
  srv <- fake_server()
  local_mocked_bindings(snap_describe = fake_describe, snap_get = srv$get)
  x <- snap_test_catchments()
  d <- ak_climate_projection(x, scenarios = "ssp245", periods = list(`2071-2100` = 2071:2100), models = c("GFDL-ESM4", "MIROC6"),
                             baseline = 1995:2014, seasons = ak_seasons(months = FALSE)[c("annual", "jja", "djf")])
  expect_equal(ak_provenance(d)$fn, "ak_climate_projection")
  cols <- projection_ensemble(d, wide = TRUE)
  expect_equal(nrow(cols), 2)
  j <- catchment_join(x, cols)
  expect_equal(utils::tail(ak_provenance(j)$layer, 1), "climate_projection")
  expect_equal(j$cmip6_dtmean_annual_ssp245_2071_2100, cols$cmip6_dtmean_annual_ssp245_2071_2100[match(j$site_code, cols$site_code)])
  expect_true(all(c("cmip6_dtmean_annual_ssp245_2071_2100", "cmip6_dtmean_annual_ssp245_2071_2100_se",
                    "cmip6_pr_pct_jja_ssp245_2071_2100_lo95", "cmip6_pr_pct_djf_ssp245_2071_2100_flag") %in% names(cols)))
  expect_equal(cols$cmip6_dtmean_annual_ssp245_2071_2100, c(3.5, 3.5), tolerance = 0.05)
  options(AKCatchments.snap_flip = NULL)
})

test_that("seasons across the year end take the December before", {
  m <- dplyr::tibble(var = "tasmax", model = "a", scenario = "s", cell = "0_0", year = rep(2000:2001, each = 12),
                     month = rep(1:12, 2), value = rep(c(1, 2), each = 12), n_days = 30L)
  sy <- snap_season_years(m, list(djf = c(12, 1, 2)))
  expect_equal(sy$year, 2001)                     # Dec 2000 + Jan, Feb 2001; 2000 and 2002 are incomplete
  expect_equal(sy$value, (31 * 1 + 31 * 2 + 28 * 2) / 90, tolerance = 1e-9)
  p <- snap_season_years(dplyr::mutate(m, var = "pr"), list(jja = 6:8))
  expect_equal(p$value, c(92, 184))               # 1 mm/day over 92 days, then 2
})

test_that("the internal-variability SE is calibrated and discounts autocorrelation", {
  set.seed(1)
  t <- 1:30
  se <- replicate(400, {
    b <- period_stats(rnorm(30), t); f <- period_stats(rnorm(30), t)
    c(diff = f$mean - b$mean, se = sqrt(f$var / f$n_eff + b$var / b$n_eff))
  })
  expect_equal(stats::sd(se["diff", ]), mean(se["se", ]), tolerance = 0.15)
  set.seed(3)
  ar <- as.numeric(stats::arima.sim(list(ar = 0.8), 30))
  expect_lt(period_stats(ar, t)$n_eff, 30)
  expect_equal(period_stats(rep(1:2, 15), t)$n_eff, 30)  # negative autocorrelation is not rewarded
})

test_that("SNAP JSON parsing handles NaN and nil values", {
  a <- snap_parse_array("[[[1, NaN],[-9999, 4]],[[5,6],[7,8]]]", nil = -9999)
  expect_equal(dim(a), c(2, 2, 2))
  expect_equal(sum(is.na(a)), 2)
  expect_equal(ak_climate_projection_models()$scenario[ak_climate_projection_models()$model == "E3SM-2-0" & ak_climate_projection_models()$var == "pr"],
               c("historical", "ssp370"))
})
