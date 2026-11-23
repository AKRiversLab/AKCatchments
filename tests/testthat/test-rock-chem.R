# Rock chemistry from SIM 3340 lithologies and the Olson & Hawkins class values

rock_sq <- function(x0, y0 = 1600000, d = 2000)
  sf::st_polygon(list(rbind(c(x0, y0), c(x0 + d, y0), c(x0 + d, y0 + d), c(x0, y0 + d), c(x0, y0))))

rock_fixture <- function() {
  dir <- ak_cache_dir(AKCatchments:::cache_paths$geology)
  g <- file.path(dir, "sim3340.gdb")
  units <- sf::st_sf(NSACLASS = c("1", "2", "3"), STATE_UNITNAME = c("Shale and limestone", "Alluvium", "Siltstone, carbonaceous"),
                     geometry = sf::st_sfc(rock_sq(200000), rock_sq(202000), rock_sq(204000), crs = 3338))
  sf::st_write(units, g, "AKStategeol_poly", driver = "OpenFileGDB", quiet = TRUE)
  lith <- data.frame(NSACLASS = c("1", "1", "2", "3", "3"),
                     LITH1 = c("Sedimentary", "Sedimentary", "Unconsolidated", "Sedimentary", "Water"),
                     LITH2 = c("Clastic", "Carbonate", "Alluvium", "Clastic", ""),
                     LITH3 = c("Shale", "Limestone", "", "Siltstone", ""),
                     RANK = c("Major", "Minor", "Major", "Major", "Incidental"))
  sf::st_write(lith, g, "NSALith", driver = "OpenFileGDB", quiet = TRUE)
  dir
}

rock_table <- data.frame(element = "N", lithology = c("Shale", "Limestone", "Alluvium", "Clastic", "Carbonaceous shale", "Water"),
                         central = c(0.06, 0.01, 0.03, 0.05, 0.2, NA), n = c(20, 10, 3, 30, 8, 0),
                         sdlog = c(0.8, 0.5, NA, 0.6, 0.9, NA))

test_that("rock N is the rank-weighted class mean, with draws, NA shares, coarse matches and surficial units reported", {
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  rock_fixture()
  x <- sf::st_sf(site_code = c("A", "B", "C"), geometry = sf::st_sfc(rock_sq(200000), rock_sq(201000), rock_sq(204000), crs = 3338))
  r <- ak_rock_chem(x, "N", lith_table = rock_table, n = 400)
  wA <- c(mean(c(33, 100)), mean(c(10, 33)))
  expect_equal(r$rockN_pct[1], sum(wA * c(0.06, 0.01)) / sum(wA), tolerance = 1e-9)
  expect_equal(r$rockN_pct[2], (r$rockN_pct[1] + 0.03) / 2, tolerance = 1e-9)        # half unit 1, half alluvium
  expect_equal(r$rock_surficial_frac, c(0, 0.5, 0))
  expect_equal(r$rockN_sparse_frac, c(0, 0.5, 0))                                  # alluvium: 3 records
  expect_equal(r$rockN_flag[1], "")
  # unit 3: siltstone is not a class, so it matches "Clastic"; water has no value
  expect_equal(r$rockN_pct[3], 0.05)
  expect_equal(r$rockN_coarsened_frac[3], 1)
  expect_equal(r$rockN_na_frac[3], 5 / (5 + mean(c(33, 100))), tolerance = 1e-9)
  expect_true(all(r$rockN_pct_se > 0))
  expect_true(all(r$rockN_pct_lo95 < r$rockN_pct & r$rockN_pct < r$rockN_pct_hi95))
  expect_equal(ak_provenance(r)$layer, "rock_chemistry")
  # reproducible, and the formation term is most of the spread
  expect_equal(ak_rock_chem(x, "N", lith_table = rock_table, n = 400)$rockN_pct_se, r$rockN_pct_se)
  r0 <- ak_rock_chem(x, "N", lith_table = rock_table, n = 400, formation_sd = 0)
  expect_true(all(r0$rockN_pct_se < r$rockN_pct_se))
  # excluding surficial units: B becomes unit 1 only
  ex <- ak_rock_chem(x, "N", lith_table = rock_table, n = 50, surficial = "exclude")
  expect_equal(ex$rockN_pct[2], r$rockN_pct[1], tolerance = 1e-9)
  # a hand-assigned modifier class
  mods <- data.frame(NSACLASS = "1", lithology = "Shale", olson_class = "Carbonaceous shale", rationale = "DMU: black, carbonaceous")
  m <- ak_rock_chem(x, "N", lith_table = rock_table, n = 50, modifiers = mods)
  expect_equal(m$rockN_pct[1], sum(wA * c(0.2, 0.01)) / sum(wA), tolerance = 1e-9)
  expect_equal(m$rockN_modifier_frac[1], wA[1] / sum(wA), tolerance = 1e-9)
  expect_error(ak_rock_chem(x, "N", lith_table = rock_table, modifiers = data.frame(NSACLASS = "1", olson_class = "Nope")), "not in the N table")
  d <- ak_rock_chem(x, "N", lith_table = rock_table, n = 50, details = TRUE)
  expect_true(all(c("crosswalk", "units") %in% names(d)))
  expect_equal(d$crosswalk$status[d$crosswalk$lith_term == "Alluvium"], "sparse class")
  cw <- rock_chem_crosswalk(x, "N", lith_table = rock_table, file = f <- tempfile(fileext = ".csv"))
  expect_true(file.exists(f))
  expect_true(cw$suggest[cw$NSACLASS == "3"][1])                    # "carbonaceous" in the unit name
  back <- utils::read.csv(f)
  back$modifier_class[back$NSACLASS == 1 & back$lith_term == "Shale"] <- "Carbonaceous shale"
  utils::write.csv(back, f, row.names = FALSE)
  expect_equal(ak_rock_chem(x, "N", lith_table = rock_table, n = 50, modifiers = f)$rockN_pct, m$rockN_pct)
  expect_error(ak_rock_chem(x, "Zn"), "Unknown element")
})

test_that("the lithology workbook is read by its headers, and per-class tabs give records and spread", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  old <- options(AKCatchments.cache = tempfile("akcache_"))
  on.exit(options(old), add = TRUE)
  long_name <- "Carbonaceous shale and mudstone, black"
  summ <- data.frame(a = c("Lithology summary", NA, "Lithology", "Shale", long_name, "Water"),
                     b = c(NA, NA, "N (%)", "0.06", "0.2", NA), c = c(NA, NA, "S (%)", "0.3", "1.1", NA))
  rec <- \(v) data.frame(a = c("Source", "EarthChem", rep("lit", length(v) - 1)), b = c("% N", v))
  sheets <- list(ReadMe = data.frame(a = "read me"), `Lith Summary` = summ, Shale = rec(c(0.03, 0.06, 0.12, 0.05, 0.08)))
  sheets[[substr(long_name, 1, 31)]] <- rec(c(0.2, 0.25))
  f <- file.path(ak_cache_dir(AKCatchments:::cache_paths$olson), "Lith-Nutrient.xls")    # xlsx content under the .xls name
  writexl::write_xlsx(lapply(sheets, \(d) { names(d) <- paste0("V", seq_along(d)); d }), f, col_names = FALSE)
  lt <- rock_chem_lithologies(c("N", "S"))
  n <- lt[lt$element == "N", ]
  expect_equal(n$lithology, c("Shale", long_name, "Water"))
  expect_equal(n$central, c(0.06, 0.2, NA))
  expect_equal(n$n, c(5L, 2L, NA))
  expect_equal(n$sdlog[1], stats::sd(log(c(0.03, 0.06, 0.12, 0.05, 0.08))))
  expect_equal(n$tab[2], substr(long_name, 1, 31))                  # truncated tab name matched
  expect_equal(lt$central[lt$element == "S"], c(0.3, 1.1, NA))
  expect_true(all(is.na(lt$n[lt$element == "S"])))                   # no S records in the tabs
  # a bare "n" column is a count, not nitrogen
  expect_equal(AKCatchments:::element_header(c("Lithology", "n", "N (%)"), "N"), 3L)
  expect_equal(AKCatchments:::element_header(c("Lithology", "Nitrogen"), "N"), 2L)
  expect_equal(AKCatchments:::element_header(c("Lithology", "CaO", "MgO"), "MgO"), 3L)
})
