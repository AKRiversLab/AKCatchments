# =============================================================================
# ak_live_tests.R -- one-shot live tests of AKCatchments on your own machine
# =============================================================================
#
# WHAT IT DOES
#   Calls every function of AKCatchments that downloads data, against the
#   real services, and checks the answers against known values (published
#   drainage area of a USGS gage, the USGS gaged-basin boundary, StreamStats,
#   plausible ranges). It also probes the web services planned for the
#   observations module (USGS Water Data APIs, WQP, SNOTEL, ACIS, ERA5-Land,
#   ESA WorldCover), so the next development step knows which ones answer.
#   Every test is isolated: a failure is recorded, and the run carries on.
#
# HOW TO RUN (in a fresh R session)
#   1. Check the SETTINGS block below. The defaults are usually fine.
#   2. RStudio: open this file and click "Source".
#      Terminal: Rscript ak_live_tests.R
#   3. When it finishes, open  <OUT_DIR>/ak_live_test_report.md  and paste
#      the WHOLE file into the chat with Claude. If a check plot in
#      <OUT_DIR>/figures looks wrong, attach it too.
#
#   - Stuck on one test? Press Esc once (RStudio). The test is recorded as
#     interrupted, and the run moves on to the next one.
#   - After a fix, re-run only some tests: set ONLY <- c("D01", "E03").
#     Tests that need objects from other tests skip until those tests run.
#   - The report is rewritten after every test, so it is useful even if R
#     crashes part-way.
#
# LEVELS
#   "quick": every function with small downloads (roughly 20-45 min and
#            under 1 GB, mostly DEM windows).
#   "full" : also the large statewide downloads (SIM 3340 geology, NLCD,
#            ACCS alder, MODIS, Pastick permafrost, TDX-Hydro, MERIT and
#            HydroRIVERS flowlines): several GB and 1-3 h. The downloads
#            stay in the test cache, so a second run is fast.
#
# TEST IDS
#   A setup | B DEMs | C flowlines | D delineation | E validation references
#   S test catchments | F terrain | G climate/weather | H map layers
#   I observations (current) | P probes for planned modules | Z cache, unit tests
# =============================================================================

# ---- SETTINGS ----------------------------------------------------------------
LEVEL <- Sys.getenv("AK_TEST_LEVEL", "full")  # "quick" or "full"

# Where the report, the .rds and the figures go
OUT_DIR <- file.path(getwd(), paste0("ak_live_tests_", format(Sys.Date())))

# Download cache for the tests, kept apart from your real (shared) cache so a
# broken download can't leave anything there. By default it is in R's session
# temp folder, so it is removed when R closes and each run downloads again
# (a few hundred MB at quick level). To keep it between runs, set
# AK_TEST_CACHE to a folder on a drive with room.
# Set USE_SHARED_CACHE <- TRUE to test against your real cache instead
# (AKCATCHMENTS_CACHE or CAKN_DATABASE_CACHE). That also checks that the
# package reuses downloads that are already there.
TEST_CACHE <- Sys.getenv("AK_TEST_CACHE", file.path(tempdir(), "AKCatchments_test_cache"))
USE_SHARED_CACHE <- FALSE

# AKCatchments source folder (the unzipped package) to load with
# pkgload::load_all(). Leave "" to use the installed package.
AK_PKG_PATH <- Sys.getenv("AK_PKG_PATH", "")

# CAKN-Data "data" folder (with sites_geo.csv and catchments.gpkg), used to
# delineate and validate some of your own sites. "" = look in the usual
# places; the CAKN tests are skipped if it isn't found.
CAKN_DATA_DIR <- Sys.getenv("CAKN_DATA_DIR", "")
N_CAKN_SITES <- c(quick = 2, full = 5)   # how many CAKN sites to delineate

# Install missing CRAN packages that AKCatchments imports or suggests
# (mirai, soilDB, archive, elevatr ...), and the WhiteboxTools binary?
INSTALL_MISSING <- TRUE

# Run only these test IDs (e.g. c("D01", "E02")), or env AK_TEST_ONLY = "D01,E02"; NULL runs everything
ONLY <- if (nzchar(Sys.getenv("AK_TEST_ONLY"))) trimws(strsplit(Sys.getenv("AK_TEST_ONLY"), ",")[[1]]) else NULL

# Also run the package's own offline unit tests at the end? Needs
# AK_PKG_PATH and the testthat package.
RUN_UNIT_TESTS <- TRUE
# ------------------------------------------------------------------------------


# ==============================================================================
# Reference sites (checked values; change only with a source)
# ==============================================================================
# USGS 15290000 Little Susitna River near Palmer: in the Cook Inlet Basin, so
# StreamStats delineates it; a long-record HCDN gage. Drainage area 61.9 mi2
# = 160.3 km2. Coordinates 61 42 32 N, 149 13 36 W (Slack et al. 1993, USGS
# WRI 93-4076, HCDN station page; probably NAD27). Test A05 replaces them with
# the NAD83 coordinates from the USGS Water Data API when that works.
GAGE <- list2env(list(site_code = "USGS-15290000", name = "Little Susitna River near Palmer",
                      lat = 61 + 42 / 60 + 32 / 3600, lon = -(149 + 13 / 60 + 36 / 3600),
                      area_km2 = 61.9 * 2.589988, gage_datum_m = 918.6 * 0.3048,
                      coord_source = "HCDN (Slack et al. 1993)"))


# ==============================================================================
# Test harness (no need to read this; scroll down to "THE TESTS")
# ==============================================================================
if (!exists("%||%", mode = "function")) `%||%` <- function(x, y) if (is.null(x)) y else x
stopifnot(LEVEL %in% c("quick", "full"))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
FIG <- file.path(OUT_DIR, "figures")
dir.create(FIG, showWarnings = FALSE)
REPORT_MD <- file.path(OUT_DIR, "ak_live_test_report.md")
REPORT_RDS <- file.path(OUT_DIR, "ak_live_test_results.rds")
options(timeout = max(3600, getOption("timeout")), warn = 1)
RUN_START <- Sys.time()

.R <- new.env()       # test records
OBJ <- new.env()      # objects shared between tests
OBJ_SRC <- character() # object name -> test that made it
ORDER <- character()  # test ids in the order declared
.cur <- new.env()     # the running test
ALWAYS <- c("A01", "A02", "A03", "A05", "A06")  # setup: runs even when ONLY is set

keep <- function(name, value) {
  assign(name, value, envir = OBJ)
  OBJ_SRC[[name]] <<- .cur$id
  invisible(value)
}
got <- function(name) get(name, envir = OBJ)
has <- function(name) exists(name, envir = OBJ, inherits = FALSE) && !is.null(get(name, envir = OBJ))
# Hard check: stops the test with your message
expect <- function(ok, ...) if (!isTRUE(ok)) stop(paste0(...), call. = FALSE)
# Soft check: the test passes, but with status WARN and this note
note <- function(...) .cur$notes <- c(.cur$notes, paste0(...))
fmt <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")
pt_sf <- function(lat, lon, site_code = NA) sf::st_sf(site_code = site_code, geometry = sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326))
pt_buffer <- function(g, m) sf::st_buffer(sf::st_transform(pt_sf(g$lat, g$lon, g$site_code), 3338), m)
dist_m <- function(lat1, lon1, lat2, lon2) as.numeric(sf::st_distance(pt_sf(lat1, lon1), pt_sf(lat2, lon2)))
get_json <- function(url, query = list(), simplify = TRUE) {
  httr2::request(url) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("AKCatchments live tests") |>
    httr2::req_timeout(180) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(simplifyVector = simplify)
}
http_detail <- function(resp) {
  if (is.null(resp)) return(NULL)
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "<body not text, or saved to a file>")
  ct <- tryCatch(httr2::resp_content_type(resp), error = function(e) NA)
  sprintf("%s %s\n-> HTTP %s, content-type %s\n%s", resp$method %||% "", resp$url %||% "", resp$status_code %||% "?",
          ct, substr(body, 1, 800))
}
last_resp <- function() if (requireNamespace("httr2", quietly = TRUE)) tryCatch(httr2::last_response(), error = function(e) NULL)
call_stack <- function() {
  txt <- vapply(sys.calls(), function(cl) substr(paste(deparse(cl, nlines = 1L, width.cutoff = 150L), collapse = ""), 1, 160), "")
  start <- max(c(0, grep("^eval\\(\\.code, \\.env\\)", txt)))
  txt <- txt[seq_along(txt) > start]
  txt[!grepl("^(\\.handleSimpleError|h\\(simpleError|withCallingHandlers|tryCatch|tryCatchList|tryCatchOne|doTryCatch|signalCondition|withOneRestart|withRestarts|doWithOneRestart|\\(function \\(e\\)|call_stack)", txt)]
}

test <- function(id, title, code, level = "quick", uses = NULL, uses_any = NULL, pkgs = NULL, skip_if = NULL) {
  .code <- substitute(code)
  ORDER <<- c(ORDER, id)
  rec <- list(id = id, title = title, level = level, status = NA_character_, seconds = NA_real_, result = "",
              notes = character(), error = NULL, error_class = NULL, trace = NULL, http = NULL,
              warnings = character(), messages = character(), code = paste(deparse(.code, width.cutoff = 100L), collapse = "\n"))
  finish <- function(rec) {
    assign(id, rec, envir = .R)
    tag <- switch(rec$status, PASS = "PASS", WARN = "WARN", FAIL = "FAIL", "SKIP")
    cat(sprintf("[%s] %s %s%s\n", tag, id, title,
                if (nzchar(rec$result)) paste0("\n       ", substr(gsub("\n", " ", rec$result), 1, 300)) else ""))
    if (!is.null(rec$error)) cat("       ERROR:", substr(short_err(rec$error), 1, 400), "\n")
    for (n in rec$notes) cat("       NOTE:", n, "\n")
    write_report()
    invisible(rec)
  }
  if (!is.null(ONLY) && !id %in% c(ONLY, ALWAYS)) return(invisible())
  reason <- NULL
  if (level == "full" && LEVEL != "full") reason <- "full level only (set LEVEL <- \"full\")"
  miss_pkg <- pkgs[!vapply(pkgs, requireNamespace, TRUE, quietly = TRUE)]
  if (is.null(reason) && length(miss_pkg)) reason <- paste0("package not installed: install.packages(c(\"", paste(miss_pkg, collapse = "\", \""), "\"))")
  miss <- uses[!vapply(uses, has, TRUE)]
  if (is.null(reason) && length(miss)) {
    reason <- paste0("needs ", paste0(miss, collapse = ", "), " (made by ",
                     paste(unique(vapply(miss, src_of, "")), collapse = ", "), ", which did not succeed or did not run)")
  }
  if (is.null(reason) && length(uses_any) && !any(vapply(uses_any, has, TRUE))) reason <- paste0("needs one of: ", paste(uses_any, collapse = ", "))
  if (is.null(reason) && !is.null(skip_if)) reason <- skip_if
  if (!is.null(reason)) {
    rec$status <- "SKIP"
    rec$result <- reason
    return(finish(rec))
  }

  cat(sprintf("\n---- %s %s (started %s) ----\n", id, title, format(Sys.time(), "%H:%M:%S")))
  .cur$id <- id
  .cur$notes <- character()
  msgs <- warns <- character()
  trace <- NULL
  lr0 <- last_resp()
  .env <- new.env(parent = globalenv())
  t0 <- Sys.time()
  out <- tryCatch(
    withCallingHandlers(eval(.code, .env),
                        message = function(m) msgs <<- c(msgs, trimws(conditionMessage(m))),
                        warning = function(w) {
                          warns <<- c(warns, conditionMessage(w))
                          invokeRestart("muffleWarning")
                        },
                        error = function(e) {
                          s <- call_stack()
                          if (is.null(trace) || length(s) > length(trace)) trace <<- s
                        }),
    error = function(e) e,
    interrupt = function(i) structure(list(message = "interrupted by the user (Esc)"), class = c("ak_interrupt", "condition")))
  rec$seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  rec$notes <- .cur$notes
  rec$messages <- utils::tail(unique(msgs[nzchar(msgs)]), 30)
  rec$warnings <- utils::head(unique(warns), 15)
  if (inherits(out, "ak_interrupt")) {
    rec$status <- "SKIP"
    rec$result <- "interrupted by the user (Esc)"
  } else if (inherits(out, "error")) {
    rec$status <- "FAIL"
    rec$error <- conditionMessage(out)
    rec$error_class <- paste(class(out), collapse = ", ")
    rec$trace <- trace
    resp <- if (!is.null(out$resp)) out$resp else {
      lr <- last_resp()
      if (!is.null(lr) && !identical(lr, lr0)) lr
    }
    rec$http <- http_detail(resp)
  } else {
    rec$status <- if (length(rec$notes)) "WARN" else "PASS"
    rec$result <- if (is.character(out) && length(out)) paste(out, collapse = "; ") else ""
  }
  finish(rec)
}
PRODUCERS <- c(gage_api = "A05", dem_ifsar = "B01", d01 = "D01", d_cop = "D03", cakn_sites = "A06",
               cakn_hand = "A06", cakn_delin = "D04", akc = "D05", gb = "E01", ss = "E02", catch = "S01",
               catch_small = "S01", pts_small = "S01", cond = "F01", dm = "G01", melt = "G02", aktemp_near = "I01",
               obs_st = "I03", usgs_st = "I04")
src_of <- function(m) if (m %in% names(OBJ_SRC)) OBJ_SRC[[m]] else if (m %in% names(PRODUCERS)) PRODUCERS[[m]] else "?"

clean <- function(x) gsub("\\|", "/", gsub("[\r\n]+", " ", x))
# The innermost cause of a chained error (purrr/httr2/rlang wrap errors in "Caused by error ...")
short_err <- function(msg) {
  parts <- strsplit(msg, "Caused by error[^\n]*:\n", perl = TRUE)[[1]]
  trimws(gsub("\\s+", " ", gsub("^! ", "", utils::tail(parts, 1))))
}
pkg_ver <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed")
session_lines <- function() {
  soft <- tryCatch({ v <- sf::sf_extSoftVersion(); paste(names(v), v, sep = " ", collapse = ", ") }, error = function(e) "?")
  wbt <- .wbt_ver %||% "?"
  pk <- c("AKCatchments", "sf", "terra", "whitebox", "httr2", "exactextractr", "dplyr", "purrr", "tidyr", "ggplot2",
          "maptiles", "jsonlite", "mirai", "soilDB", "archive", "elevatr", "testthat")
  c(sprintf("- Run: %s, level **%s**, %s elapsed", format(RUN_START, "%Y-%m-%d %H:%M"), LEVEL,
            format(round(difftime(Sys.time(), RUN_START, units = "mins"), 1))),
    sprintf("- R: %s on %s (%s)", R.version.string, utils::sessionInfo()$running %||% "?", R.version$platform),
    sprintf("- AKCatchments loaded from: %s", .ak_source %||% "?"),
    sprintf("- Cache: `%s`", tryCatch(AKCatchments::ak_cache_dir(), error = function(e) "?")),
    sprintf("- GDAL/GEOS/PROJ: %s", soft),
    sprintf("- WhiteboxTools: %s", substr(wbt, 1, 120)),
    sprintf("- Packages: %s", paste(pk, vapply(pk, pkg_ver, ""), collapse = ", ")),
    sprintf("- Gage coordinates used: %.5f, %.5f (%s)", GAGE$lat, GAGE$lon, GAGE$coord_source))
}

write_report <- function() {
  recs <- mget(intersect(ORDER, ls(.R)), envir = .R)
  st <- vapply(recs, `[[`, "", "status")
  n <- table(factor(st, c("PASS", "WARN", "FAIL", "SKIP")))
  L <- c("# AKCatchments live test report", "",
         "Paste this whole file into the chat with Claude.", "",
         sprintf("**%d PASS, %d WARN, %d FAIL, %d SKIP** (of %d tests run or skipped)", n[["PASS"]], n[["WARN"]], n[["FAIL"]], n[["SKIP"]], length(recs)), "",
         tryCatch(session_lines(), error = function(e) paste("- (session info failed:", conditionMessage(e), ")")), "",
         "## Summary", "", "| ID | Test | Status | Time | Result / reason |", "|---|---|---|---|---|")
  for (r in recs) {
    L <- c(L, sprintf("| %s | %s | %s | %s | %s |", r$id, clean(r$title), r$status,
                      if (is.na(r$seconds)) "" else paste0(round(r$seconds), " s"),
                      clean(substr(if (r$status == "FAIL") paste("ERROR:", short_err(r$error)) else paste(c(r$result, if (length(r$notes)) paste("NOTE:", r$notes)), collapse = " "), 1, 400))))
  }
  fails <- Filter(function(r) r$status == "FAIL", recs)
  if (length(fails)) {
    L <- c(L, "", "## Failures in detail", "")
    for (r in fails) {
      L <- c(L, sprintf("### %s %s", r$id, r$title), "", "Error:", "```text", r$error, "```",
             sprintf("Condition class: `%s`", r$error_class))
      if (length(r$trace)) L <- c(L, "", "Call stack (innermost last):", "```text", utils::tail(r$trace, 25), "```")
      if (!is.null(r$http)) L <- c(L, "", "Last HTTP response during this test:", "```text", r$http, "```")
      if (length(r$warnings)) L <- c(L, "", "Warnings:", "```text", r$warnings, "```")
      if (length(r$messages)) L <- c(L, "", "Last messages:", "```text", utils::tail(r$messages, 12), "```")
      L <- c(L, "", "Test code:", "```r", r$code, "```", "")
    }
  }
  warned <- Filter(function(r) r$status == "WARN" || (r$status == "PASS" && length(r$warnings)), recs)
  if (length(warned)) {
    L <- c(L, "", "## Notes and R warnings from tests that passed", "")
    for (r in warned) {
      L <- c(L, sprintf("**%s %s** (%s)", r$id, r$title, r$status))
      if (length(r$notes)) L <- c(L, paste("- NOTE:", r$notes))
      if (length(r$warnings)) L <- c(L, paste("- warning:", substr(utils::head(r$warnings, 5), 1, 300)))
      L <- c(L, "")
    }
  }
  writeLines(L, REPORT_MD)
  saveRDS(list(records = recs, settings = list(LEVEL = LEVEL, ONLY = ONLY, TEST_CACHE = TEST_CACHE, USE_SHARED_CACHE = USE_SHARED_CACHE,
                                                AK_PKG_PATH = AK_PKG_PATH, CAKN_DATA_DIR = CAKN_DATA_DIR)), REPORT_RDS)
  invisible(REPORT_MD)
}
.ak_source <- NULL
.wbt_ver <- NULL


# ==============================================================================
# THE TESTS
# ==============================================================================

# ---- A: setup ----------------------------------------------------------------
test("A01", "Install missing dependencies and load AKCatchments", {
  desc <- if (nzchar(AK_PKG_PATH)) {
    expect(file.exists(file.path(AK_PKG_PATH, "DESCRIPTION")), "AK_PKG_PATH has no DESCRIPTION: ", AK_PKG_PATH)
    read.dcf(file.path(AK_PKG_PATH, "DESCRIPTION"))
  } else {
    expect(requireNamespace("AKCatchments", quietly = TRUE),
           "AKCatchments is not installed. Install it (remotes::install_local(\"path/to/AKCatchments\")) or set AK_PKG_PATH to the unzipped package folder.")
    read.dcf(system.file("DESCRIPTION", package = "AKCatchments"))
  }
  field <- function(f) if (f %in% colnames(desc)) trimws(sub("\\(.*", "", strsplit(desc[, f], ",")[[1]])) else character()
  imports <- setdiff(field("Imports"), "")
  suggests <- setdiff(field("Suggests"), c("", "knitr", "rmarkdown"))
  extra <- c(if (nzchar(AK_PKG_PATH)) "pkgload", if (RUN_UNIT_TESTS && nzchar(AK_PKG_PATH)) "testthat")
  want <- unique(c(imports, suggests, extra))
  missing <- want[!vapply(want, requireNamespace, TRUE, quietly = TRUE)]
  if (length(missing) && INSTALL_MISSING) {
    message("Installing: ", paste(missing, collapse = ", "))
    utils::install.packages(missing)
    missing <- want[!vapply(want, requireNamespace, TRUE, quietly = TRUE)]
  }
  expect(!any(imports %in% missing), "Imports not installed: ", paste(intersect(imports, missing), collapse = ", "))
  if (length(missing)) note("not installed (tests that need them are skipped): ", paste(missing, collapse = ", "))
  if (nzchar(AK_PKG_PATH)) {
    pkgload::load_all(AK_PKG_PATH, quiet = TRUE, export_all = FALSE)
    .ak_source <<- paste0("source folder ", normalizePath(AK_PKG_PATH, winslash = "/"))
  } else {
    library(AKCatchments)
    .ak_source <<- paste0("installed package ", system.file(package = "AKCatchments"))
  }
  sprintf("AKCatchments %s from %s", utils::packageVersion("AKCatchments"), .ak_source)
})
if (!"package:AKCatchments" %in% search() && !isNamespaceLoaded("AKCatchments")) {
  write_report()
  stop("AKCatchments could not be loaded: see ", REPORT_MD, call. = FALSE)
}
if (!"package:AKCatchments" %in% search()) suppressMessages(library(AKCatchments))

test("A02", "Set the download cache", {
  path <- if (USE_SHARED_CACHE) {
    p <- Sys.getenv("AKCATCHMENTS_CACHE", Sys.getenv("CAKN_DATABASE_CACHE", ""))
    expect(nzchar(p), "USE_SHARED_CACHE is TRUE but neither AKCATCHMENTS_CACHE nor CAKN_DATABASE_CACHE is set")
    p
  } else TEST_CACHE
  ak_set_cache(path)
  expect(normalizePath(ak_cache_dir(), winslash = "/") == normalizePath(path, winslash = "/"), "ak_cache_dir() is ", ak_cache_dir(), ", not ", path)
  probe <- file.path(ak_cache_dir(), ".write_test")
  expect(isTRUE(file.create(probe)), "cannot write to the cache folder ", path)
  unlink(probe)
  free <- tryCatch(if (.Platform$OS.type == "windows") NA else as.numeric(system2("df", c("-Pk", shQuote(path)), stdout = TRUE)[2] |> strsplit(" +") |> unlist() |> (\(v) v[4])()) / 1e6, error = function(e) NA)
  if (!is.na(free) && free < 5) note("only ", fmt(free), " GB free on the cache drive")
  sprintf("cache %s (%d sub-folders already present)", ak_cache_dir(), length(list.dirs(ak_cache_dir(), recursive = FALSE)))
})

test("A03", "WhiteboxTools binary installed and working", {
  ok <- isTRUE(suppressMessages(whitebox::wbt_init()))
  if (!ok && INSTALL_MISSING) {
    message("Installing the WhiteboxTools binary")
    whitebox::install_whitebox()
    ok <- isTRUE(suppressMessages(whitebox::wbt_init()))
  }
  expect(ok, "WhiteboxTools is not available: run whitebox::install_whitebox()")
  v <- suppressMessages(whitebox::wbt_version())
  .wbt_ver <<- paste0(v[nzchar(v)][1], " at ", whitebox::wbt_exe_path())
  .wbt_ver
})

test("A05", "USGS Water Data API: gage 15290000 location and drainage area", {
  j <- get_json("https://api.waterdata.usgs.gov/ogcapi/v0/collections/monitoring-locations/items/USGS-15290000",
                list(f = "json"), simplify = FALSE)
  xy <- unlist(j$geometry$coordinates)
  expect(length(xy) >= 2, "no coordinates in the response; fields: ", paste(names(j), collapse = ", "))
  p <- j$properties
  da_field <- grep("drainage_area", names(p), value = TRUE)
  da <- suppressWarnings(as.numeric(unlist(p[da_field])[1]))
  shift <- dist_m(GAGE$lat, GAGE$lon, xy[2], xy[1])
  keep("gage_api", list(lat = xy[2], lon = xy[1], drainage_area_mi2 = da, fields = names(p)))
  GAGE$lat <- xy[2]
  GAGE$lon <- xy[1]
  GAGE$coord_source <- "USGS Water Data API (NAD83)"
  if (!is.na(da) && abs(da * 2.589988 - GAGE$area_km2) > 1) note("API drainage area ", da, " mi2 differs from HCDN 61.9 mi2")
  if (shift > 500) note("API coordinates are ", round(shift), " m from the HCDN coordinates")
  sprintf("%.5f, %.5f (%.0f m from the HCDN coordinates); drainage area field %s = %s mi2",
          xy[2], xy[1], shift, paste(da_field, collapse = "/"), da)
})

test("A06", "Find CAKN-Data sites and hand-drawn catchments", skip_if = {
  cands <- c(CAKN_DATA_DIR, "CAKN-Data/data", "../CAKN-Data/data", "../../CAKN-Data/data", "~/CAKN-Data/data",
             file.path(Sys.getenv("CAKN_REPO"), "data"))
  cands <- cands[nzchar(cands) & file.exists(file.path(cands, "sites_geo.csv"))]
  if (length(cands)) { CAKN_DATA_DIR <- normalizePath(cands[1], winslash = "/"); NULL } else
    "CAKN-Data/data/sites_geo.csv not found (set CAKN_DATA_DIR to run the CAKN tests)"
}, {
  s <- utils::read.csv(file.path(CAKN_DATA_DIR, "sites_geo.csv"))
  expect(all(c("site_code", "latitude", "longitude") %in% names(s)), "sites_geo.csv lacks site_code/latitude/longitude; columns: ", paste(names(s), collapse = ", "))
  s <- s[!is.na(s$area_km2) & s$area_km2 > 0.1, ]
  expect(nrow(s) > 0, "no sites with a positive NPS area_km2 in sites_geo.csv")
  s <- s[order(s$area_km2), ]
  k <- min(N_CAKN_SITES[[LEVEL]], nrow(s))
  pick <- s[unique(round(seq(1, max(1, ceiling(nrow(s) * 0.6)), length.out = k))), ]  # small to median sizes: fast
  keep("cakn_sites", data.frame(site_code = pick$site_code, lat = pick$latitude, lon = pick$longitude, ref_area_km2 = pick$area_km2))
  gp <- file.path(CAKN_DATA_DIR, "catchments.gpkg")
  if (file.exists(gp)) {
    h <- sf::st_read(gp, layer = "catchments", quiet = TRUE)
    h <- h[h$site_code %in% pick$site_code & h$source != "redelineated", ]
    if (nrow(h)) keep("cakn_hand", h[, "site_code"]) else note("no hand-drawn polygons in catchments.gpkg for the picked sites")
  } else note("catchments.gpkg not found in ", CAKN_DATA_DIR, " (run CAKN-Data step 05 to enable E05)")
  sprintf("%s: picked %s (NPS areas %s km2)", CAKN_DATA_DIR, paste(pick$site_code, collapse = ", "), paste(fmt(pick$area_km2, 2), collapse = ", "))
})
if (has("cakn_sites") && !nzchar(CAKN_DATA_DIR)) CAKN_DATA_DIR <- "(found automatically)"


# ---- B: DEMs ------------------------------------------------------------------
test("B01", "ak_dem('ifsar'): DGGS IfSAR 5 m image service, 2 km window at the gage", {
  d <- ak_dem(pt_buffer(GAGE, 1000), "ifsar")
  keep("dem_ifsar", d)
  v <- terra::values(d, mat = FALSE)
  expect(abs(terra::res(d)[1] - 5) < 0.01, "cell size ", terra::res(d)[1], " m, expected 5")
  expect(mean(!is.na(v)) > 0.95, fmt(100 * mean(is.na(v))), "% of cells are NA")
  rng <- range(v, na.rm = TRUE)
  expect(rng[1] > -50 && rng[2] < 6200, "elevations out of range: ", fmt(rng[1]), " to ", fmt(rng[2]), " m")
  z <- terra::extract(d, terra::vect(sf::st_transform(pt_sf(GAGE$lat, GAGE$lon), 3338)))[1, 2]
  if (is.finite(z) && abs(z - GAGE$gage_datum_m) > 40) note("elevation at the gage ", fmt(z), " m vs gage datum ", fmt(GAGE$gage_datum_m), " m")
  sprintf("%d x %d cells, %.0f-%.0f m; %.0f m at the gage (gage datum %.0f m); tag %s",
          terra::nrow(d), terra::ncol(d), rng[1], rng[2], z, GAGE$gage_datum_m, terra::metags(d)[1])
})

test("B02", "ak_dem('copernicus'): Copernicus GLO-30 COGs on AWS; bias against IfSAR", {
  d <- ak_dem(pt_buffer(GAGE, 1000), "copernicus")
  v <- terra::values(d, mat = FALSE)
  expect(abs(terra::res(d)[1] - 30) < 0.01, "cell size ", terra::res(d)[1], " m, expected 30")
  expect(mean(!is.na(v)) > 0.95, fmt(100 * mean(is.na(v))), "% of cells are NA")
  out <- sprintf("%d x %d cells, %.0f-%.0f m", terra::nrow(d), terra::ncol(d), min(v, na.rm = TRUE), max(v, na.rm = TRUE))
  if (has("dem_ifsar")) {
    i30 <- terra::resample(got("dem_ifsar"), d, method = "average")
    dd <- terra::values(d - i30, mat = FALSE)
    bias <- mean(dd, na.rm = TRUE)
    if (abs(bias) > 25) note("Copernicus minus IfSAR mean ", fmt(bias), " m (vegetation in the DSM, or a datum problem)")
    out <- paste0(out, sprintf("; Copernicus - IfSAR: mean %.1f m, RMS %.1f m", bias, sqrt(mean(dd^2, na.rm = TRUE))))
  }
  out
})

test("B03", "ak_dem('elevatr'): AWS terrain tiles", pkgs = "elevatr", {
  d <- ak_dem(pt_buffer(GAGE, 1000), "elevatr")
  v <- terra::values(d, mat = FALSE)
  expect(mean(!is.na(v)) > 0.9, fmt(100 * mean(is.na(v))), "% of cells are NA")
  sprintf("%d x %d cells at %.0f m, %.0f-%.0f m", terra::nrow(d), terra::ncol(d), terra::res(d)[1], min(v, na.rm = TRUE), max(v, na.rm = TRUE))
})

test("B04", "ak_dem('local'): your DEM tiles in the cache", skip_if = {
  n <- length(list.files(file.path(ak_cache_dir(), "dem"), "\\.tif$", recursive = TRUE))
  if (n == 0) "no .tif tiles in the cache's dem folder (normal for the test cache)"
}, {
  tifs <- list.files(file.path(ak_cache_dir(), "dem"), "\\.tif$", recursive = TRUE, full.names = TRUE)
  r <- terra::rast(tifs[1])
  ctr <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(r))) |> sf::st_centroid() |> sf::st_buffer(500)
  d <- ak_dem(ctr, "local")
  expect(mean(!is.na(terra::values(d))) > 0.5, "mostly NA around the centre of ", basename(tifs[1]))
  sprintf("%d tiles; %d x %d cells at %.1f m around the centre of %s", length(tifs), terra::nrow(d), terra::ncol(d), terra::res(d)[1], basename(tifs[1]))
})


test("B05", "ak_dem('copernicus') across 60 N, where GLO-30 tiles change longitude spacing", {
  d <- ak_dem(pt_buffer(list(lat = 60.0, lon = -150.6, site_code = "kenai_60N"), 1500), "copernicus")
  v <- terra::values(d, mat = FALSE)
  expect(mean(!is.na(v)) > 0.95, fmt(100 * mean(is.na(v))), "% of cells are NA (a seam between the N59 and N60 tiles?)")
  rows_na <- rowSums(is.na(terra::as.matrix(d, wide = TRUE))) == terra::ncol(d)
  expect(!any(rows_na), sum(rows_na), " rows entirely NA (seam)")
  sprintf("%d x %d cells, %.0f-%.0f m, no NA rows", terra::nrow(d), terra::ncol(d), min(v, na.rm = TRUE), max(v, na.rm = TRUE))
})


ANCH <- list(lat = 61.19, lon = -149.82, site_code = "anchorage_midtown")   # inside the Municipality of Anchorage lidar

test("B06", "ak_lidar_index(): State of Alaska (DGGS) and USGS 3DEP 1 m lidar footprints", {
  idx <- ak_lidar_index(refresh = TRUE)
  keep("lidar_index", idx)
  n <- table(idx$source)
  expect("dggs" %in% names(n), "no State of Alaska lidar projects with a DTM service; sources: ", paste(names(n), n, collapse = ", "))
  if (!"3dep" %in% names(n)) note("no USGS 3DEP 1 m projects in Alaska in the index")
  expect(all(idx$z_to_m %in% c(1, 0.3048, 1200 / 3937)), "unexpected vertical unit factors: ", paste(unique(idx$z_to_m), collapse = ", "))
  sprintf("%s; %d DGGS projects in feet; newest %s (%s)", paste(names(n), n, collapse = ", "), sum(idx$z_to_m != 1),
          idx$project[which.max(idx$year)], max(idx$year, na.rm = TRUE))
})

test("B07", "ak_dem('lidar'): a lidar DTM in midtown Anchorage, in metres (checked against IfSAR)", uses = "lidar_index", {
  d <- ak_dem(pt_buffer(ANCH, 1000), "lidar")
  ifs <- ak_dem(pt_buffer(ANCH, 1000), "ifsar")
  both <- c(terra::resample(d, ifs, method = "average"), ifs)
  v <- terra::values(both)
  ratio <- stats::median(v[, 1] / v[, 2], na.rm = TRUE)
  expect(abs(terra::res(d)[1] - 2) < 0.01, "cell size ", terra::res(d)[1], " m, expected 2")
  expect(!anyNA(terra::values(d)), "the lidar window has missing cells")
  expect(ratio > 0.8 && ratio < 1.25, "lidar/IfSAR elevation ratio ", fmt(ratio, 2), " (vertical units?)")
  sprintf("project %s (%s); median lidar - IfSAR %.1f m; elevation ratio %.2f", AKCatchments:::dem_tag(d, "lidar_project"),
          AKCatchments:::dem_source(d), stats::median(v[, 1] - v[, 2], na.rm = TRUE), ratio)
})

test("B08", "ak_dem('best') at the gage: lidar if a project covers the window, else IfSAR", uses = "lidar_index", {
  d <- ak_dem(pt_buffer(GAGE, 2000), "best")
  src <- AKCatchments:::dem_source(d)
  expect(src %in% c("lidar_2m", "ifsar_5m"), "dem_source tag ", src)
  sprintf("used %s%s", src, if (startsWith(src, "lidar")) paste0(" (", AKCatchments:::dem_tag(d, "lidar_project"), ")") else "")
})

# ---- C: reference flowlines -------------------------------------------------------
test("C01", "ak_flowlines('3dhp'): 3D Hydrography Program flowlines (ArcGIS service)", {
  fl <- ak_flowlines(pt_buffer(GAGE, 3000), "3dhp")
  expect(inherits(fl, "sf") && nrow(fl) > 0, "no 3DHP flowlines within 3 km of the gage")
  expect("flowline_source" %in% names(fl), "no flowline_source column")
  sprintf("%d features, %.1f km", nrow(fl), sum(as.numeric(sf::st_length(fl))) / 1000)
})

test("C02", "ak_flowlines('nhd'): NHD flowlines (ArcGIS service)", {
  fl <- ak_flowlines(pt_buffer(GAGE, 3000), "nhd")
  expect(inherits(fl, "sf") && nrow(fl) > 0, "no NHD flowlines within 3 km of the gage")
  sprintf("%d features, %.1f km", nrow(fl), sum(as.numeric(sf::st_length(fl))) / 1000)
})

test("C03", "ak_flowline_sources() and ak_download_flowlines(download = FALSE) plans", {
  src <- ak_flowline_sources()
  expect(is.data.frame(src) && nrow(src) >= 3, "ak_flowline_sources() returned no table")
  aoi <- pt_buffer(GAGE, 3000)
  plans <- lapply(c("tdx", "merit", "hydrorivers"), function(s) ak_download_flowlines(aoi, s, download = FALSE))
  expect(all(vapply(plans, function(p) is.data.frame(p) && nrow(p) >= 1, TRUE)), "an empty download plan")
  paste(vapply(plans, function(p) paste0(p$source[1], ": ", paste(p$unit, collapse = "/")), ""), collapse = "; ")
})

for (FL_SRC in c("tdx", "merit", "hydrorivers")) {
  test(c(tdx = "C04", merit = "C05", hydrorivers = "C06")[[FL_SRC]],
       sprintf("ak_download_flowlines('%s') then ak_flowlines('%s')", FL_SRC, FL_SRC), level = "full",
       skip_if = if (FL_SRC == "tdx") "TDX-Hydro dropped: earth-info.nga.mil is unreachable from your networks", {
    aoi <- pt_buffer(GAGE, 3000)
    plan <- ak_download_flowlines(aoi, FL_SRC)
    expect(all(plan$exists), "not downloaded: ", paste(plan$url[!plan$exists], collapse = ", "))
    fl <- ak_flowlines(aoi, FL_SRC)
    expect(nrow(fl) > 0, "no ", FL_SRC, " flowlines within 3 km of the gage")
    sprintf("%d features, %.1f km; files: %s (%.0f MB)", nrow(fl), sum(as.numeric(sf::st_length(fl))) / 1000,
            paste(basename(plan$file), collapse = ", "), sum(file.size(plan$file)) / 1e6)
  })
}


test("C07", "ak_download_flowlines('geoglows'): TDX-Hydro via GEOGLOWS v2 (AWS); reach area at the gage vs published 160.3 km2", level = "full", {
  aoi <- pt_buffer(GAGE, 3000)
  plan <- ak_download_flowlines(aoi, "geoglows")
  expect(all(plan$exists), "not downloaded: ", paste(plan$url[!plan$exists], collapse = ", "))
  fl <- ak_flowlines(aoi, "geoglows")
  expect(nrow(fl) > 0, "no GEOGLOWS reaches within 3 km of the gage")
  expect(all(c("USContArea", "DSContArea") %in% names(fl)), "no USContArea/DSContArea; columns: ", paste(names(fl), collapse = ", "))
  near <- AKCatchments:::snap_reaches(sf::st_transform(pt_sf(GAGE$lat, GAGE$lon), 3338), fl, 300)
  hit <- near[GAGE$area_km2 >= near$lo / 1.5 & GAGE$area_km2 <= near$hi * 1.5, ]
  if (!nrow(hit)) note("no reach within 300 m fits the published area; nearest ranges: ",
                       paste(sprintf("%.0f-%.0f km2 at %.0f m", near$lo, near$hi, near$dist_m), collapse = "; "))
  sprintf("%d reaches; files %s (%.0f MB); reaches within 300 m: %s", nrow(fl), paste(basename(plan$file), collapse = ", "),
          sum(file.size(plan$file)) / 1e6, paste(sprintf("%.0f-%.0f km2 at %.0f m", near$lo, near$hi, near$dist_m), collapse = "; "))
})

# ---- D: delineation ---------------------------------------------------------------
test("D01", "ak_delineate(): USGS 15290000 blind (no reference area), IfSAR; area vs published 160.3 km2", {
  d <- ak_delineate(GAGE$lat, GAGE$lon, site_code = GAGE$site_code, dem_source = "ifsar", snap_dist_m = 300,
                    max_buffer_m = 60000, max_cells = if (LEVEL == "full") 6000 else 4000, plot_dir = FIG)
  keep("d01", d)
  c1 <- d$catchment
  r <- c1$area_km2 / GAGE$area_km2
  desc <- sprintf("area %.1f km2 vs published %.1f (ratio %.2f); snapped %.0f m (%s); DEM %s; window %.0f km; delin_ok %s; flags: %s",
                  c1$area_km2, GAGE$area_km2, r, c1$snap_dist_m, c1$snap_method, c1$dem_source, c1$window_m / 1000,
                  c1$delin_ok, if (nzchar(c1$flags)) c1$flags else "none")
  expect(r > 0.8 && r < 1.25, desc)
  if (!isTRUE(c1$delin_ok)) note("delin_ok is FALSE")
  desc
})

test("D02", "Check plot on satellite imagery (written by ak_delineate and by plot_catchment)", uses = "d01", {
  d <- got("d01")
  expect(!is.null(d$plot_file) && file.exists(d$plot_file), "ak_delineate() wrote no plot")
  f <- plot_catchment(d, file = file.path(FIG, "D02_plot_catchment.png"), subtitle_extra = "live test D02")
  expect(file.exists(f), "plot_catchment() wrote no file")
  kb <- file.size(c(d$plot_file, f)) / 1024
  if (any(kb < 150)) note("small PNG (", paste(round(kb), collapse = "/"), " KB): the imagery may have failed and fallen back to hillshade; look at it")
  sprintf("%s (%.0f KB), %s (%.0f KB): open them to check the outline", basename(d$plot_file), kb[1], basename(f), kb[2])
})

test("D03", "ak_delineate(): same gage from Copernicus GLO-30; DEM sensitivity (IoU against IfSAR)", level = "full", uses = "d01", {
  d <- ak_delineate(GAGE$lat, GAGE$lon, site_code = GAGE$site_code, dem_source = "copernicus", snap_dist_m = 300,
                    max_buffer_m = 60000, plot_dir = file.path(FIG, "copernicus"))
  keep("d_cop", d)
  cmp <- compare_catchments(d$catchment, got("d01")$catchment)
  if (cmp$iou < 0.8) note("IoU Copernicus vs IfSAR only ", fmt(cmp$iou, 2))
  sprintf("Copernicus area %.1f km2; IoU with IfSAR %.2f, Hausdorff %.0f m", d$catchment$area_km2, cmp$iou, cmp$hausdorff_m)
})

test("D04", "ak_delineate(): CAKN sites from IfSAR, guided by the NPS area", uses = "cakn_sites", {
  s <- got("cakn_sites")
  res <- lapply(seq_len(nrow(s)), function(i) tryCatch(
    ak_delineate(s$lat[i], s$lon[i], site_code = s$site_code[i], ref_area_km2 = s$ref_area_km2[i], dem_source = "ifsar",
                 max_buffer_m = 60000, plot_dir = FIG),
    error = function(e) conditionMessage(e)))
  names(res) <- s$site_code
  ok <- vapply(res, inherits, TRUE, "ak_delineation")
  if (any(ok)) keep("cakn_delin", res[ok])
  tab <- vapply(names(res)[ok], function(n) {
    c1 <- res[[n]]$catchment
    sprintf("%s %.2f km2 (NPS %.2f, ratio %.2f, flags: %s)", n, c1$area_km2, c1$ref_area_km2, c1$area_km2 / c1$ref_area_km2,
            if (nzchar(c1$flags)) c1$flags else "none")
  }, "")
  expect(all(ok), "failed: ", paste(names(res)[!ok], unlist(res[!ok]), sep = ": ", collapse = " || "),
         if (any(ok)) paste(" || succeeded:", paste(tab, collapse = "; ")) else "")
  bad <- vapply(res, function(x) !isTRUE(x$catchment$delin_ok), TRUE)
  if (any(bad)) note("delin_ok FALSE for ", paste(names(res)[bad], collapse = ", "))
  paste(tab, collapse = "; ")
})


test("D05", "ak_catchments(): gage and CAKN sites, 2 mirai daemons if available; plots, kept rasters, step timings", uses = "d01", {
  pts <- rbind(data.frame(site_code = GAGE$site_code, lat = GAGE$lat, lon = GAGE$lon, ref_area_km2 = NA_real_, snap_dist_m = 300),
               if (has("cakn_sites")) cbind(got("cakn_sites"), snap_dist_m = 150))
  par <- requireNamespace("mirai", quietly = TRUE) && nrow(pts) > 1
  if (par) { mirai::daemons(2); on.exit(mirai::daemons(0), add = TRUE) } else note("mirai not installed: sites delineated one at a time")
  t0 <- Sys.time()
  x <- ak_catchments(pts, dem_source = "ifsar", max_buffer_m = 60000,
                     max_cells = if (LEVEL == "full") 6000 else 4000, plot_dir = file.path(FIG, "ak_catchments"))
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  keep("akc", x)
  err <- attr(x, "errors")
  expect(!NROW(err), "failed: ", paste(err$site_code, err$error, sep = ": ", collapse = " || "))
  files <- unlist(sf::st_drop_geometry(x)[c("dem_file", "filled_file", "pointer_file", "accum_file", "plot_file")])
  expect(all(file.exists(files)), "missing files: ", paste(files[!file.exists(files)], collapse = ", "))
  g <- x[x$site_code == GAGE$site_code, ]
  cmp <- compare_catchments(g, got("d01")$catchment)
  expect(cmp$iou > 0.99, "the parallel delineation of the gage differs from D01 (IoU ", fmt(cmp$iou, 3), ")")
  mb <- sum(file.size(files[!grepl("plot", names(files))])) / 1e6
  tt <- colSums(sf::st_drop_geometry(x)[c("t_dem_s", "t_condition_s", "t_watershed_s", "t_finish_s", "t_flowlines_s", "t_plot_s")])
  utils::write.csv(sf::st_drop_geometry(x)[c("site_code", "area_km2", "n_windows", grep("^t_", names(x), value = TRUE))],
                   file.path(OUT_DIR, "D05_step_times.csv"), row.names = FALSE)
  sprintf("%d sites in %.0f s wall (%s); %d delin_ok; kept rasters %.0f MB in all; gage IoU with D01 %.3f; summed step times: %s",
          nrow(x), wall, if (par) "2 daemons" else "sequential", sum(x$delin_ok), mb, cmp$iou,
          paste(sprintf("%s %.0f s", sub("^t_(.*)_s$", "\\1", names(tt)), tt), collapse = ", "))
})

test("D05b", "ak_catchments(regional = TRUE): shared DEM windows give the same catchments as D05, faster", level = "full", uses = "akc", {
  x <- got("akc")
  pts <- data.frame(site_code = x$site_code, lat = x$lat, lon = x$lon, snap_dist_m = ifelse(x$site_code == GAGE$site_code, 300, 150))
  t0 <- Sys.time()
  r <- ak_catchments(pts, regional = TRUE, dem_source = "ifsar", max_buffer_m = 60000, max_cells = 6000,
                     plot_dir = file.path(FIG, "ak_catchments_regional"))
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cmp <- compare_catchments(x, r)
  utils::write.csv(cmp, file.path(OUT_DIR, "D05b_regional_vs_site.csv"), row.names = FALSE)
  low <- cmp$site_code[cmp$iou < 0.98]
  if (length(low)) note("regional and per-site catchments differ (IoU < 0.98) at: ", paste(low, collapse = ", "))
  expect(nrow(cmp) == nrow(x), "regional run delineated ", nrow(cmp), " of ", nrow(x), " sites")
  sprintf("%.0f s wall; %d windows shared by %d sites, %d on their own; IoU with per-site: median %s, min %s; summed DEM %.0f s and conditioning %.0f s (per-site run: %.0f s and %.0f s)",
          wall, length(unique(r$window_mode[startsWith(r$window_mode, "regional")])), sum(startsWith(r$window_mode, "regional")),
          sum(r$window_mode == "site"), fmt(stats::median(cmp$iou), 3), fmt(min(cmp$iou), 3), sum(r$t_dem_s), sum(r$t_condition_s),
          sum(x$t_dem_s), sum(x$t_condition_s))
})

test("D07", "ak_delineate(dem_source = 'best'): Chester Creek at Arctic Blvd (USGS 15275100) on Anchorage lidar vs IfSAR", level = "full", {
  j <- get_json("https://api.waterdata.usgs.gov/ogcapi/v0/collections/monitoring-locations/items/USGS-15275100", list(f = "json"),
                simplify = FALSE)
  xy <- unlist(j$geometry$coordinates)
  da_km2 <- suppressWarnings(as.numeric(j$properties$drainage_area)) * 2.589988
  best <- ak_delineate(xy[2], xy[1], site_code = "USGS-15275100", dem_source = "best", plot_dir = file.path(FIG, "best"))
  ifs <- ak_delineate(xy[2], xy[1], site_code = "USGS-15275100", dem_source = "ifsar", plot_dir = file.path(FIG, "ifsar"))
  cmp <- compare_catchments(best$catchment, ifs$catchment)
  c1 <- best$catchment
  if (!startsWith(c1$dem_source, "lidar")) note("the final window was not covered by lidar; used ", c1$dem_source)
  sprintf("best: %s (%s), %.1f km2; IfSAR %.1f km2; published %.1f km2; IoU best vs IfSAR %.2f",
          c1$dem_source, c1$dem_project, c1$area_km2, ifs$catchment$area_km2, da_km2, cmp$iou)
})

test("D06", "Layer functions chained: topography, outlet slope, fire; provenance; ak_write() with a table", uses = "akc", {
  x <- got("akc") |> ak_topography() |> ak_outlet_slope() |> ak_fire()
  fy <- ak_fire(x, by_year = TRUE)
  need <- c("topo_elev_mean_m", "outlet_slope_m_m", "fire_burned_pct", "fire_flag")
  expect(all(need %in% names(x)), "missing columns: ", paste(setdiff(need, names(x)), collapse = ", "))
  expect(all(is.finite(x$outlet_slope_m_m) & x$outlet_slope_m_m >= 0), "outlet slopes: ", paste(fmt(x$outlet_slope_m_m, 4), collapse = "/"))
  pv <- ak_provenance(x)
  expect(identical(pv$layer, c("catchment", "topography", "outlet_slope", "fire")), "provenance layers: ", paste(pv$layer, collapse = ", "))
  f <- file.path(OUT_DIR, "D06_catchments.gpkg")
  ak_write(x, f, fire_by_year = fy)
  lay <- sf::st_layers(f)$name
  expect(all(c("catchments", "provenance", "fire_by_year") %in% lay), "GeoPackage layers: ", paste(lay, collapse = ", "))
  if (any(is.na(pv$downloaded_utc[pv$layer == "fire"]))) note("fire provenance has no download date (cache manifest not written?)")
  sprintf("%d columns; burned %% %s; provenance %d rows; %s", ncol(x), paste(fmt(x$fire_burned_pct), collapse = "/"), nrow(pv), basename(f))
})


# ---- E: validation references ------------------------------------------------------
test("E01", "ak_gaged_basins(): USGS gaged-basin boundaries, releases 2019 and 2014 (Curran 2024); find 15290000", {
  all_b <- ak_gaged_basins()
  expect(inherits(all_b, "sf") && nrow(all_b) > 500, "fewer than 500 basins returned: ", nrow(all_b))
  n <- table(all_b$release)
  b <- all_b[all_b$site_code == GAGE$site_code & !nzchar(dplyr::coalesce(as.character(all_b$SpecConds), "")), ]
  expect(nrow(b) == 1, nrow(b), " rows for ", GAGE$site_code)
  gb <- sf::st_sf(site_code = GAGE$site_code, geometry = sf::st_geometry(b))
  keep("gb", gb)
  a <- as.numeric(sf::st_area(gb)) / 1e6
  if (abs(a / GAGE$area_km2 - 1) > 0.05) note("gaged-basin polygon is ", fmt(a), " km2 vs published ", fmt(GAGE$area_km2))
  sprintf("%s basins (releases %s); 15290000 from the %s release, %.1f km2 (published %.1f)",
          nrow(all_b), paste(names(n), n, sep = ": ", collapse = ", "), b$release, a, GAGE$area_km2)
})

test("E02", "ak_streamstats(): SS-Delineate and SS-Hydro characteristics at 15290000 (Cook Inlet region)", {
  ss <- ak_streamstats(GAGE$lat, GAGE$lon, site_code = GAGE$site_code, characteristics = TRUE)
  expect(!is.null(ss), "ak_streamstats() returned NULL (see the messages below for the reason)")
  keep("ss", ss)
  r <- ss$ss_area_km2 / GAGE$area_km2
  if (abs(r - 1) > 0.1) note("StreamStats area off the published area by ", fmt(100 * (r - 1)), "%")
  bc <- ss$ss_characteristics[[1]]
  expect(nrow(bc) > 0 && all(c("code", "name", "unit", "value") %in% names(bc)), "no basin characteristics came back")
  sprintf("area %.1f km2 (ratio %.2f to published); snapped %.0f m to %.4f, %.4f; %d characteristics (%s); warning: %s",
          ss$ss_area_km2, r, ss$ss_snap_dist_m, ss$ss_lat, ss$ss_lon, nrow(bc), paste(head(bc$code, 8), collapse = ", "),
          ss$ss_warning %||% "none")
})

test("E03", "compare_catchments(): delineation vs gaged basin and StreamStats", uses = "d01", uses_any = c("gb", "ss"), {
  refs <- Filter(Negate(is.null), list(gaged_basin = if (has("gb")) got("gb"), streamstats = if (has("ss")) got("ss")))
  cmp <- do.call(rbind, lapply(names(refs), function(n) cbind(reference = n, compare_catchments(got("d01")$catchment, refs[[n]]))))
  expect(nrow(cmp) == length(refs), "compare_catchments() returned ", nrow(cmp), " rows for ", length(refs), " references")
  txt <- sprintf("%s: IoU %.2f, area ratio %.2f, Hausdorff %.0f m", cmp$reference, cmp$iou, cmp$area_ratio, cmp$hausdorff_m)
  expect(all(cmp$iou >= 0.6), "low agreement: ", paste(txt, collapse = "; "))
  if (any(cmp$iou < 0.8)) note("IoU below 0.8: ", paste(txt[cmp$iou < 0.8], collapse = "; "))
  paste(txt, collapse = "; ")
})

test("E05", "compare_catchments(): CAKN sites against your hand-drawn polygons, with the delineation flags", uses = c("cakn_delin", "cakn_hand"), {
  auto <- dplyr::bind_rows(purrr::map(got("cakn_delin"), "catchment"))
  cm <- compare_catchments(auto, got("cakn_hand")) |>
    dplyr::left_join(sf::st_drop_geometry(auto)[c("site_code", "delin_ok", "flags")], by = "site_code")
  expect(nrow(cm) >= 1, "no site in both sets")
  utils::write.csv(cm, file.path(OUT_DIR, "E05_cakn_vs_hand.csv"), row.names = FALSE)
  if (any(cm$iou < 0.7)) note("IoU < 0.7 at ", paste(cm$site_code[cm$iou < 0.7], collapse = ", "))
  paste(sprintf("%s IoU %.2f%s", cm$site_code, cm$iou, ifelse(cm$delin_ok, "", " (failure flag)")), collapse = "; ")
})

# ---- S: the catchments used by the layer tests --------------------------------------
test("S01", "Build the test catchment set (delineated, else reference polygon, else a 2 km circle)", {
  pick <- function(x) if (!is.null(x)) sf::st_sf(site_code = x$site_code, geometry = sf::st_geometry(sf::st_transform(x, 3338)))
  gage_c <- if (has("d01")) pick(got("d01")$catchment) else if (has("gb")) pick(got("gb")) else if (has("ss")) pick(got("ss")) else {
    note("no gage catchment available: using a 2 km circle at the gage")
    pick(sf::st_sf(site_code = GAGE$site_code, geometry = sf::st_geometry(pt_buffer(GAGE, 2000))))
  }
  cakn_c <- if (has("cakn_delin")) do.call(rbind, lapply(got("cakn_delin"), function(d) pick(d$catchment))) else if (has("cakn_hand")) pick(got("cakn_hand"))
  catch <- rbind(gage_c, cakn_c)
  catch$area_km2 <- as.numeric(sf::st_area(catch)) / 1e6
  keep("catch", catch)
  small <- catch[which.min(catch$area_km2), ]
  keep("catch_small", small)
  pts <- rbind(data.frame(site_code = GAGE$site_code, lat = GAGE$lat, lon = GAGE$lon),
               if (has("cakn_sites")) got("cakn_sites")[, c("site_code", "lat", "lon")])
  keep("pts_small", pts[pts$site_code == small$site_code, ])
  sprintf("%s; smallest: %s", paste(sprintf("%s %.2f km2", catch$site_code, catch$area_km2), collapse = ", "), small$site_code)
})


# ---- F: terrain ----------------------------------------------------------------------
test("F01", "import_catchments(): IfSAR window, breach, D8 (smallest test catchment)", uses = "catch_small", {
  cs <- got("catch_small")
  cond <- import_catchments(cs, dem_source = "ifsar", res = if (cs$area_km2 > 50) 10 else NULL)
  keep("cond", cond)
  f <- unlist(sf::st_drop_geometry(cond)[c("dem_file", "filled_file", "pointer_file", "accum_file")])
  expect(all(file.exists(f)), "missing conditioned rasters")
  sprintf("%s at %.0f m: %s", cond$site_code, cond$dem_res_m, paste(basename(f[c(1, 4)]), collapse = ", "))
})

test("F02", "ak_outlet_slope(): 100 and 200 m reaches", uses = c("pts_small", "cond"), {
  sl <- ak_outlet_slope(got("cond"), got("pts_small"))
  sl1 <- c(sl$outlet_slope_m_m, sl$outlet_slope_m_m_200)
  expect(nrow(sl) == 1 && all(is.finite(sl1)), "outlet slopes missing: ", paste(fmt(sl1, 4), collapse = ", "))
  expect(all(sl1 >= 0 & sl1 < 1), "implausible slopes: ", paste(fmt(sl1, 4), collapse = ", "))
  r <- sl$outlet_contrib_area_km2 / sl$outlet_catch_area_km2
  if (abs(log(r)) > log(1.5)) note("contributing area at the snapped outlet is ", fmt(r, 2), " x the catchment area")
  sprintf("slope %s m/m at 100/200 m; contrib %.2f km2 vs catchment %.2f km2", paste(fmt(sl1, 4), collapse = "/"),
          sl$outlet_contrib_area_km2, sl$outlet_catch_area_km2)
})

test("F03", "ak_topography(): topo_* attributes", uses = "cond", {
  tp <- sf::st_drop_geometry(ak_topography(got("cond")))
  expect(nrow(tp) == 1, nrow(tp), " rows, expected 1")
  tp <- tp[grepl("^topo_", names(tp))]
  num <- unlist(tp[vapply(tp, is.numeric, TRUE)])
  finer <- paste0("topo_slope_mean_deg_", c(10, 30)[c(10, 30) < tp$topo_dem_res_m], "m")  # NA is right there
  bad <- names(num)[!is.finite(num) & !(names(num) %in% finer & is.na(num))]
  expect(!length(bad), "non-finite values in: ", paste(bad, collapse = ", "))
  sprintf("%d columns; elev mean %.0f m, slope mean %.1f deg, fws %.1f%%, flag: %s", ncol(tp), tp$topo_elev_mean_m,
          tp$topo_slope_mean_deg, tp$topo_fws_pct, if (nzchar(tp$topo_flag)) tp$topo_flag else "none")
})


# ---- G: climate and weather -------------------------------------------------------------
test("G01", "ak_weather(): Daymet V4 pixels (concurrent requests), 2019-2020, smallest catchment", uses = "catch_small", {
  t0 <- Sys.time()
  dm <- ak_weather(got("catch_small"), years = 2019:2020)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (NROW(attr(dm, "errors"))) note("pixel failures: ", attr(dm, "errors")$error[1])
  keep("dm", dm)
  need <- c("site_code", "date", "tmin", "tmax", "tmean", "prcp", "swe", "n_pixels", "tmean_se_sampling", "daymet_flag")
  expect(all(need %in% names(dm)), "missing columns: ", paste(setdiff(need, names(dm)), collapse = ", "))
  expect(nrow(dm) >= 728, nrow(dm), " rows, expected about 730 (two years)")
  jul <- dm[format(dm$date, "%m") == "07", ]
  expect(mean(jul$tmax) > 5 && mean(jul$tmax) < 30, "mean July tmax ", fmt(mean(jul$tmax)), " C")
  expect(all(dm$prcp >= 0, na.rm = TRUE), "negative precipitation")
  if (max(dm$n_pixels) > 1) expect(any(is.finite(dm$tmean_se_sampling)), "tmean_se_sampling is all NA although several pixels were averaged")
  fl <- paste(unique(dm$daymet_flag[!is.na(dm$daymet_flag) & nzchar(dm$daymet_flag)]), collapse = "/")
  sprintf("%.0f s; %d days; %d pixels; mean July tmax %.1f C; annual prcp %.0f mm; peak SWE %.0f mm; flag: %s", secs, nrow(dm),
          dm$n_pixels[1], mean(jul$tmax), sum(dm$prcp) / 2, max(dm$swe), if (nzchar(fl)) fl else "none")
})

test("G02", "Melt day from Daymet SWE (internal melt_daymet, used by weather_at_samples and ak_snowoff)", uses = "dm", {
  m <- AKCatchments:::melt_daymet(got("dm"), baseline_years = 2019:2020)
  keep("melt", m)
  expect(nrow(m) == 2, nrow(m), " rows, expected 2 (one per year)")
  if (any(is.na(m$melt_doy))) note("melt_doy NA in ", paste(m$year[is.na(m$melt_doy)], collapse = ", "))
  ok <- m$melt_doy[!is.na(m$melt_doy)]
  expect(all(ok > 60 & ok < 213), "melt days out of range: ", paste(ok, collapse = ", "))
  sprintf("melt day %s in %s", paste(m$melt_doy, collapse = "/"), paste(m$year, collapse = "/"))
})

test("G03", "weather_at_samples(): anomalies, melt and summaries for a sample on 2020-07-15 (2019-2020 as a stand-in baseline)", uses = c("dm", "catch_small"), {
  obs <- data.frame(site_code = unique(got("dm")$site_code), sample_date = as.Date("2020-07-15"))
  ws <- weather_at_samples(got("dm"), obs, baseline = 2019:2020)
  expect(nrow(ws) == 1, nrow(ws), " rows; expected 1")
  need <- c("t_anom7_se", "p_anom30_se", "weather_flag", "melt_doy", "tdd", "snowfall_swe_mm")
  expect(all(need %in% names(ws)), "missing columns: ", paste(setdiff(need, names(ws)), collapse = ", "))
  if (max(got("dm")$n_pixels) > 1) expect(is.finite(ws$t_anom7_se), "t_anom7_se not finite")
  nm <- ak_climate_normals(got("catch_small"), "daymet4", weather = got("dm"), baseline = 2019:2020, vars = "tmean", periods = "07")
  sprintf("%d columns (%s ...); Daymet July tmean 2019-2020 %.1f C (SE %.2f)", ncol(ws), paste(utils::head(names(ws), 8), collapse = ", "),
          nm$daymet_tmean_07, nm$daymet_tmean_07_se)
})

test("G04", "ak_climate_normals(): PRISM 1981-2010 Alaska normals", uses = "catch", {
  pr <- if (LEVEL == "full") ak_climate_normals(got("catch")) else ak_climate_normals(got("catch"), vars = c("tmean", "ppt"), periods = c("07", "annual"))
  expect(all(c("prism_tmean_07", "prism_ppt_annual") %in% names(pr)), "missing columns; got ", paste(names(pr), collapse = ", "))
  expect(all(pr$prism_tmean_07 > 0 & pr$prism_tmean_07 < 20), "July tmean out of range: ", paste(fmt(pr$prism_tmean_07), collapse = ", "))
  expect(all(pr$prism_ppt_annual > 100 & pr$prism_ppt_annual < 6000), "annual ppt out of range: ", paste(fmt(pr$prism_ppt_annual, 0), collapse = ", "))
  if (any(nzchar(pr$prism_flag))) note("prism_flag: ", paste(unique(pr$prism_flag[nzchar(pr$prism_flag)]), collapse = "/"))
  paste(sprintf("%s: July tmean %.1f C, annual ppt %.0f mm", pr$site_code, pr$prism_tmean_07, pr$prism_ppt_annual), collapse = "; ")
})

test("G05", "ak_snowoff(): MODIS snow-off, filled with the Daymet offset", level = "full", uses = c("catch_small", "dm"), {
  mo <- ak_snowoff(got("catch_small"), years = 2019:2020, fill = FALSE)
  expect(nrow(mo) >= 1, "no MODIS rows")
  so <- ak_snowoff(got("catch_small"), years = 2019:2020, weather = got("dm"), baseline = 2019:2020)
  expect(nrow(so) >= 1, "no snow-off rows")
  expect(all(is.finite(so$snowoff_se_d[!is.na(so$snowoff_doy)])), "snowoff_se_d missing where there is a date")
  sprintf("MODIS doy %s; ak_snowoff columns: %s", paste(mo$modis_doy, collapse = "/"), paste(names(so), collapse = ", "))
})

test("G06", "Probe: SNAP rasdaman, describe one downscaled CMIP6 coverage (axes, grid, time axis, nil value)", {
  cov <- AKCatchments:::snap_cov_id("tasmax", "GFDL-ESM4", "historical")
  d <- AKCatchments:::snap_describe(cov, refresh = TRUE)
  expect(length(d$time) > 15000, "time axis has ", length(d$time), " steps; expected daily 1965-2014")
  expect(abs(d$xres - 4000) < 50, "X resolution ", fmt(d$xres), " m; expected 4 km")
  sprintf("%s: X %s-%s (res %s), Y %s-%s (res %s), time axis '%s' %s to %s (%d steps), nil %s, units %s", cov,
          fmt(d$xmin, 0), fmt(d$xmax, 0), fmt(d$xres, 0), fmt(d$ymin, 0), fmt(d$ymax, 0), fmt(d$yres, 0), d$time_label,
          min(d$time), max(d$time), length(d$time), paste(d$nil, collapse = "/"), paste(d$uom, collapse = "/"))
})

test("G07", "ak_climate_projection() + projection_ensemble(): 3 models, SSP2-4.5, smallest catchment and the gage", uses = "catch", {
  options(AKCatchments.snap_flip = NULL)
  cc <- got("catch")
  cc <- cc[cc$site_code %in% c(cc$site_code[which.min(as.numeric(sf::st_area(cc)))], "USGS-15290000"), ]
  t0 <- Sys.time()
  d <- ak_climate_projection(cc, scenarios = "ssp245", models = c("GFDL-ESM4", "MIROC6", "MRI-ESM2-0"),
                             periods = list(`2041-2070` = 2041:2070), keep_series = TRUE)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  keep("snap", d)
  if (NROW(attr(d, "failed"))) note(nrow(attr(d, "failed")), " SNAP requests failed: ", attr(d, "failed")$error[1])
  e <- projection_ensemble(d)
  saveRDS(list(deltas = d, ensemble = e), file.path(OUT_DIR, "G07_snap.rds"))
  ta <- e[e$var == "tmean" & e$season == "annual", ]
  pj <- e[e$var == "pr" & e$season == "annual", ]
  expect(nrow(ta) == nrow(cc), "annual tmean rows ", nrow(ta), " for ", nrow(cc), " catchments")
  expect(all(ta$delta > 0 & ta$delta < 6), "annual tmean change outside 0-6 C: ", paste(fmt(ta$delta, 2), collapse = ", "))
  expect(all(pj$pct > -20 & pj$pct < 40), "annual precipitation change outside -20 to 40%: ", paste(fmt(pj$pct), collapse = ", "))
  b <- d[d$var == "tmean" & d$season == "m07", ]
  expect(all(b$baseline_mean > 0 & b$baseline_mean < 25), "July baseline tmean outside 0-25 C (units or orientation?): ",
         paste(fmt(b$baseline_mean), collapse = ", "))
  dl <- d[d$var == "pr" & d$season == "annual", ]
  expect(all(dl$baseline_mean > 100 & dl$baseline_mean < 6000), "annual baseline precipitation outside 100-6000 mm: ",
         paste(fmt(dl$baseline_mean, 0), collapse = ", "))
  sprintf("%.0f s; axis flip %s; %s", secs, paste(getOption("AKCatchments.snap_flip"), collapse = "/"),
          paste(sprintf("%s: dT %.2f C (SD %.2f, n %d), dP %+.1f%% (SD %.1f); July baseline %.1f C; flags %s", ta$site_code, ta$delta,
                        ta$delta_se, ta$n_models, pj$pct, pj$pct_se, b$baseline_mean[match(ta$site_code, b$site_code)],
                        ifelse(nzchar(ta$flag), ta$flag, "none")), collapse = "; "))
})


# ---- H: map layers -----------------------------------------------------------------
test("H01", "ak_fire(): AICC fire perimeters by year, static columns and sample windows (cache records its area)", uses = "catch", {
  catch <- got("catch")
  fi <- ak_fire(catch, by_year = TRUE)
  fs <- ak_fire(catch)
  expect(all(fs$fire_burned_pct >= 0 & fs$fire_burned_pct <= 100), "burned % outside 0-100")
  lay <- sf::st_layers(file.path(ak_cache_dir(), "fire_history_ak.gpkg"))$name
  if (!"coverage" %in% lay) note("the fire cache has no coverage layer: it predates this version (geo workflow file), so it was reused as is")
  fw <- ak_fire(catch, samples = data.frame(site_code = catch$site_code, year = 2020L))
  expect(nrow(fw) == nrow(catch), nrow(fw), " window rows for ", nrow(catch), " catchments")
  fr <- unlist(fw[grep("^burn_frac", names(fw))])
  expect(all(fr >= 0 & fr <= 1, na.rm = TRUE), "burn fractions outside 0-1")
  sprintf("%d burned site-years; hist burn frac %s", nrow(fi), paste(fmt(fw$burn_frac_hist, 3), collapse = "/"))
})

test("H02", "ak_soils(): NRCS SSURGO/STATSGO via Soil Data Access", pkgs = "soilDB", uses = "catch_small", {
  so <- ak_soils(got("catch_small"), n = 100, details = TRUE)
  expect(is.list(so) && nrow(so$catchments) == 1, "catchments should have 1 row")
  s1 <- sf::st_drop_geometry(so$catchments)
  if (NROW(attr(so$catchments, "errors"))) note("SDA: ", attr(so$catchments, "errors")$error[1])
  if (nzchar(s1$soil_flag %||% "")) note("soil_flag: ", s1$soil_flag)
  sprintf("%d map units, %d components; columns: %s", nrow(so$mapunits), nrow(so$components), paste(utils::head(names(s1), 10), collapse = ", "))
})

test("H03", "ak_geology(): USGS SIM 3340 geologic map", level = "full", uses = "catch", {
  ge <- ak_geology(got("catch"))
  expect(nrow(ge) == nrow(got("catch")), "rows ", nrow(ge))
  sprintf("%d geol_ columns; flags: %s", sum(startsWith(names(ge), "geol_")), paste(unique(ge$geol_flag), collapse = "/"))
})

test("H04", "ak_landcover(): NLCD 2016 Alaska", level = "full", uses = "catch", {
  lc <- sf::st_drop_geometry(ak_landcover(got("catch")))
  pct <- as.matrix(lc[grep("^nlcd_pct_", setdiff(names(lc), "nlcd_pct_coverage_pct"), value = TRUE)])
  tot <- rowSums(pct[, !grepl("_(se|lo95|hi95)$", colnames(pct)), drop = FALSE], na.rm = TRUE)
  expect(all(abs(tot - 100) < 2), "class shares sum to ", paste(fmt(tot), collapse = "/"))
  sprintf("%d columns; coverage %s%%", ncol(lc), paste(fmt(lc$nlcd_pct_coverage_pct), collapse = "/"))
})

test("H05", "ak_alder(): ACCS Alnus foliar cover", level = "full", uses = "catch", {
  al <- ak_alder(got("catch"))
  expect(all(al$alnus_cover >= 0 & al$alnus_cover <= 100, na.rm = TRUE), "cover outside 0-100")
  sprintf("alder cover %s%% (coverage %s%%)", paste(fmt(al$alnus_cover), collapse = "/"), paste(fmt(al$alnus_coverage_pct), collapse = "/"))
})

test("H06", "ak_permafrost(source = \"pastick2015\"): Pastick et al. 2015 permafrost probability", level = "full", pkgs = "archive", uses = "catch", {
  pp <- ak_permafrost(got("catch"), source = "pastick2015")
  expect(all(pp$pf_prob_mean >= 0 & pp$pf_prob_mean <= 100, na.rm = TRUE), "probability outside 0-100")
  sprintf("pf_prob_mean %s; flags: %s", paste(fmt(pp$pf_prob_mean), collapse = "/"), paste(unique(pp$pf_flag), collapse = "/"))
})

test("H07", "ak_permafrost(): Jorgenson et al. 2008 (manual download)", uses = "catch", skip_if = {
  if (!length(list.files(file.path(ak_cache_dir(), "jorgenson_2008"), "\\.shp$", recursive = TRUE)))
    "no Jorgenson shapefile in the cache's jorgenson_2008 folder (manual download)"
}, {
  pj <- ak_permafrost(got("catch"), n = 100)
  sprintf("%d rows, %d columns; flags: %s", nrow(pj), ncol(pj), paste(unique(pj$totperm_pct_flag), collapse = "/"))
})

test("H08", "ak_glaciers(): RGI 7 region 01 (manual download)", uses = "catch", skip_if = {
  if (!length(list.files(file.path(ak_cache_dir(), "rgi"), "\\.shp$", recursive = TRUE)))
    "no RGI shapefile in the cache's rgi folder (manual download from NSIDC)"
}, {
  gl <- ak_glaciers(got("catch"), n = 100)
  sprintf("glacier %% %s", paste(fmt(gl$glacier_pct), collapse = "/"))
})

test("H10", "Probe: Planetary Computer, Landsat C2 L2 STAC search and SAS token at the gage, summer 2020", {
  bb <- sf::st_bbox(sf::st_transform(pt_buffer(GAGE, 2000), 4326))
  it <- AKCatchments:::lsat_search(bb, 2020, 152:243, 80, c("T1", "T2"))
  expect(length(it) > 0, "no Landsat scenes found for summer 2020")
  tok <- AKCatchments:::lsat_token()
  expect(nzchar(tok), "no SAS token")
  r <- terra::rast(paste0("/vsicurl/", it[[1]]$href[3], "?", tok))
  sprintf("%d scenes (%s); categories %s; cloud %s-%s%%; sun elevation %s; QA_PIXEL raster %d x %d, %s",
          length(it), paste(names(table(purrr::map_chr(it, "platform"))), table(purrr::map_chr(it, "platform")), sep = ":", collapse = ", "),
          paste(unique(purrr::map_chr(it, "category")), collapse = "/"), fmt(min(purrr::map_dbl(it, "cloud_cover")), 0),
          fmt(max(purrr::map_dbl(it, "cloud_cover")), 0), fmt(mean(purrr::map_dbl(it, "sun_elevation"), na.rm = TRUE)),
          terra::nrow(r), terra::ncol(r), terra::crs(r, describe = TRUE)$code)
})

test("H11", "ak_peak_ndvi(): Landsat NDVImax 2013-2020, smallest catchment (L7 + L8, so calibration runs)", level = "full", uses = "catch_small", {
  t0 <- Sys.time()
  g <- ak_peak_ndvi(got("catch_small"), years = 2013:2020)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  keep("green", g)
  saveRDS(g, file.path(OUT_DIR, "H11_greenness.rds"))
  if (NROW(attr(g, "failed"))) note(nrow(attr(g, "failed")), " scenes failed: ", attr(g, "failed")$error[1])
  expect(nrow(g) >= 4, nrow(g), " years with a value; expected most of 2013-2020")
  expect(all(g$ndvi_max > 0.1 & g$ndvi_max < 0.95), "NDVImax outside 0.1-0.95: ", paste(fmt(g$ndvi_max, 3), collapse = ", "))
  xc <- attr(g, "xcal")
  if (!any(xc$calibrated)) note("no sensor could be calibrated (too few points in one small catchment; run it on all catchments together)")
  sprintf("%.0f s; NDVImax %s (SE %s); veg_frac %s; obs/point %s; xcal %s; flags %s", secs, paste(fmt(g$ndvi_max, 3), collapse = "/"),
          paste(fmt(g$ndvi_max_se, 3), collapse = "/"), fmt(g$veg_frac[1], 2), paste(g$obs_per_point, collapse = "/"),
          paste(sprintf("%s %s (bias %s)", xc$sensor, ifelse(xc$calibrated, "ok", "skipped"), fmt(xc$xcal_bias, 4)), collapse = ", "),
          paste(unique(g$flag[nzchar(g$flag)]), collapse = "/"))
})


test("H12", "Rock chemistry: Olson & Hawkins workbooks (audit), SIM 3340 vocabulary match, ak_rock_chem() for N and placebo oxides", level = "full", uses = "catch", pkgs = "readxl", {
  els <- c("N", "S", "CaO", "MgO", "P2O5")
  lt <- rock_chem_lithologies(els, refresh = TRUE)
  utils::write.csv(lt, file.path(OUT_DIR, "H12_lithology_audit.csv"), row.names = FALSE)
  for (b in c("Lith-Nutrient.xls", "Lith-MajorOxides.xls")) {
    f <- file.path(ak_cache_dir("olson_lithology"), b)
    if (file.exists(f)) note(b, ": ", fmt(file.size(f) / 1e6, 1), " MB; tabs ", length(readxl::excel_sheets(f)))
  }
  aud <- lt |> dplyr::summarise(classes = dplyr::n(), with_value = sum(!is.na(central)), with_tab = sum(!is.na(tab)),
                                n_median = stats::median(n, na.rm = TRUE), under5 = sum(n < 5, na.rm = TRUE),
                                sdlog_median = stats::median(sdlog, na.rm = TRUE), .by = "element")
  expect(all(aud$with_value > 100), "fewer than 100 classes with a value: ", paste(aud$element, aud$with_value, collapse = ", "))
  if (any(aud$with_tab < 0.5 * aud$classes)) note("per-class tabs found for few classes (parser?): ", paste(aud$element, aud$with_tab, collapse = ", "))
  sets <- list(test = got("catch"))
  cakn <- file.path(CAKN_DATA_DIR, "catchments.gpkg")
  if (file.exists(cakn)) sets$cakn <- sf::st_read(cakn, layer = "catchments", quiet = TRUE)[, "site_code"]
  msg <- vapply(names(sets), function(nm) {
    x <- sets[[nm]]
    cw <- rock_chem_crosswalk(x, els, file = file.path(OUT_DIR, paste0("H12_crosswalk_", nm, ".csv")))
    st <- cw |> dplyr::filter(element == "N") |> dplyr::count(status)
    nomatch <- unique(cw$lith_term[cw$element == "N" & cw$status == "no matching class"])
    r <- ak_rock_chem(x, els, n = 500)
    utils::write.csv(sf::st_drop_geometry(r)[c("site_code", grep("^rock", names(r), value = TRUE))],
                     file.path(OUT_DIR, paste0("H12_rock_chem_", nm, ".csv")), row.names = FALSE)
    expect(all(r$rockN_pct >= 0 | is.na(r$rockN_pct)), "negative rock N")
    sprintf("%s: %d units, N crosswalk %s; unmatched terms: %s; rock N %% median %s (range %s-%s), median SE %s, median na_frac %s, coarsened %s; flags %s",
            nm, dplyr::n_distinct(cw$NSACLASS), paste(st$status, st$n, sep = " ", collapse = ", "),
            if (length(nomatch)) paste(utils::head(nomatch, 15), collapse = "/") else "none",
            fmt(stats::median(r$rockN_pct, na.rm = TRUE), 4), fmt(min(r$rockN_pct, na.rm = TRUE), 4), fmt(max(r$rockN_pct, na.rm = TRUE), 4),
            fmt(stats::median(r$rockN_pct_se, na.rm = TRUE), 4), fmt(stats::median(r$rockN_na_frac, na.rm = TRUE), 2),
            fmt(stats::median(r$rockN_coarsened_frac, na.rm = TRUE), 2), paste(unique(r$rockN_flag[nzchar(r$rockN_flag)]), collapse = "/"))
  }, "")
  paste(c(sprintf("%s: %d classes, %d with value, %d with records tab, median n %s, %d under 5, median sdlog %s", aud$element, aud$classes,
                  aud$with_value, aud$with_tab, fmt(aud$n_median, 0), aud$under5, fmt(aud$sdlog_median, 2)), msg), collapse = "; ")
})


# ---- I: observations (current functions) ---------------------------------------------
test("H09", "ak_alder(source = \"nps_wrst\"): NPS WRST alder at all CAKN WRST catchments, both unrecorded modes, against ACCS", level = "full", uses = "cakn_sites", {
  h <- sf::st_read(file.path(CAKN_DATA_DIR, "catchments.gpkg"), layer = "catchments", quiet = TRUE)
  h <- h[grepl("^WRST", h$site_code) & !sf::st_is_empty(h), "site_code"]
  accs <- sf::st_drop_geometry(ak_alder(h))[c("site_code", "alnus_cover", "alnus_coverage_pct")]
  modes <- c("class_mean", "zero")
  d <- dplyr::bind_rows(lapply(modes, function(m) dplyr::mutate(sf::st_drop_geometry(ak_alder(h, "nps_wrst", unrecorded = m)), unrecorded = m))) |>
    dplyr::left_join(accs, by = "site_code") |>
    dplyr::mutate(south = !(alnus_coverage_pct >= 90) %in% TRUE, ok = (alnus_wrst_coverage_pct >= 90) %in% TRUE)
  utils::write.csv(d, file.path(OUT_DIR, "H09_alder_wrst.csv"), row.names = FALSE)
  expect(all(d$alnus_wrst_cover >= 0 & d$alnus_wrst_cover <= 100, na.rm = TRUE), "alder outside 0-100")
  expect(all(d$alnus_wrst_cover_se >= 0, na.rm = TRUE), "negative SE")
  grDevices::png(file.path(OUT_DIR, "H09_alder_wrst.png"), width = 3000, height = 1500, res = 220)
  graphics::par(mfrow = c(1, 2))
  msg <- vapply(modes, function(m) {
    x <- d[d$unrecorded == m, ]
    ov <- x[x$ok & !x$south, ]
    so <- x[x$ok & x$south, ]
    r_p <- stats::cor(ov$alnus_wrst_cover, ov$alnus_cover)
    r_s <- stats::cor(ov$alnus_wrst_cover, ov$alnus_cover, method = "spearman")
    fit <- stats::lm(alnus_cover ~ alnus_wrst_cover, ov)
    plot(ov$alnus_wrst_cover, ov$alnus_cover, pch = 19, col = "grey30", xlim = c(0, max(x$alnus_wrst_cover_hi95, na.rm = TRUE)),
         xlab = sprintf("NPS WRST alder %% (unrecorded = %s), with 95%% interval", m), ylab = "ACCS Alnus foliar cover %",
         main = sprintf("%s: n = %d, r = %s (Spearman %s), slope %s", m, nrow(ov), fmt(r_p, 2), fmt(r_s, 2), fmt(stats::coef(fit)[2], 2)))
    graphics::segments(ov$alnus_wrst_cover_lo95, ov$alnus_cover, ov$alnus_wrst_cover_hi95, ov$alnus_cover, col = "grey70")
    graphics::abline(0, 1, lty = 2)
    graphics::abline(fit, col = "steelblue")
    graphics::rug(so$alnus_wrst_cover, col = "firebrick", lwd = 2)
    n_beyond <- sum(so$alnus_wrst_cover > max(ov$alnus_wrst_cover))
    if (n_beyond) note(m, ": ", n_beyond, " southern sites above the overlap maximum (", fmt(max(ov$alnus_wrst_cover)), "%)")
    # Does the scale difference depend on how much alder comes from class means, and do southern sites differ in that?
    fit_u <- stats::lm(alnus_cover ~ alnus_wrst_cover * I(alnus_wrst_unrecorded_pct / 100), ov)
    p_u <- stats::anova(fit, fit_u)[2, "Pr(>F)"]
    sprintf("%s: %d of %d covered >= 90%%; overlap n %d, r %s, Spearman %s, slope %s, intercept %s; southern %d (0-%s%%); SE median %s (max %s); unrecorded %% median overlap %s, southern %s; slope depends on unrecorded share: p = %s",
            m, sum(x$ok), nrow(x), nrow(ov), fmt(r_p, 2), fmt(r_s, 2), fmt(stats::coef(fit)[2], 2), fmt(stats::coef(fit)[1], 2),
            nrow(so), fmt(max(so$alnus_wrst_cover)), fmt(stats::median(x$alnus_wrst_cover_se, na.rm = TRUE), 2),
            fmt(max(x$alnus_wrst_cover_se, na.rm = TRUE), 2), fmt(stats::median(ov$alnus_wrst_unrecorded_pct), 0),
            fmt(stats::median(so$alnus_wrst_unrecorded_pct), 0), fmt(p_u, 3))
  }, "")
  grDevices::dev.off()
  paste(msg, collapse = "; ")
})

test("I01", "AKTEMP-VIZ station list (internal aktemp_stations, behind ak_obs_temperature_stations)", {
  st <- AKCatchments:::aktemp_stations()
  expect(inherits(st, "sf") && nrow(st) > 50, "fewer than 50 stations")
  need <- c("dataset", "provider_station_code", "filename")
  expect(all(need %in% names(st)), "missing fields: ", paste(setdiff(need, names(st)), collapse = ", "))
  g <- sf::st_transform(pt_sf(GAGE$lat, GAGE$lon), sf::st_crs(st))
  i <- sf::st_nearest_feature(g, st)
  keep("aktemp_near", st[i, ])
  sprintf("%d stations from %s; nearest to the gage: %s / %s, %.1f km", nrow(st), paste(unique(st$dataset), collapse = ", "),
          st$dataset[i], st$provider_station_code[i], as.numeric(sf::st_distance(g, st[i, ])) / 1000)
})

test("I02", "AKTEMP-VIZ daily water temperature at the nearest station (internal aktemp_daily)", uses = "aktemp_near", {
  td <- AKCatchments:::aktemp_daily(got("aktemp_near"))
  expect(nrow(td) > 0, "no rows")
  rng <- range(td$temp_c, na.rm = TRUE)
  if (rng[1] < -3 || rng[2] > 30) note("water temperature range ", fmt(rng[1]), " to ", fmt(rng[2]), " C")
  sprintf("%d days, %s to %s; water %.1f-%.1f C", nrow(td), min(td$date), max(td$date), rng[1], rng[2])
})


test("I03", "ak_obs_temperature_stations(), ak_obs_temperature() and ak_obs_link()", uses = "catch", {
  st <- ak_obs_temperature_stations()
  keep("obs_st", st)
  need <- c("obs_dataset", "provider", "station_id", "station_name", "lat", "lon")
  expect(all(need %in% names(st)), "missing station columns: ", paste(setdiff(need, names(st)), collapse = ", "))
  near <- st[sf::st_nearest_feature(sf::st_transform(pt_sf(GAGE$lat, GAGE$lon), sf::st_crs(st)), st), ]
  ob <- ak_obs_temperature(near)
  expect(nrow(ob) > 0 && all(names(ak_obs_schema()) %in% names(ob)), "no rows, or not in the observation schema")
  ln <- ak_obs_link(got("catch"), st, outlet_m = 500, near_m = 1000)
  sprintf("%d stations; %d observations at %s (%s); %d station-catchment links (%s)", nrow(st), nrow(ob), near$station_id[1],
          paste(unique(ob$variable), collapse = "/"), nrow(ln), paste(names(table(ln$relation)), table(ln$relation), collapse = ", "))
})

test("I04", "ak_obs_usgs_stations() and ak_obs_usgs_daily(): dataRetrieval read_waterdata_*, near the gage", pkgs = "dataRetrieval", {
  st <- ak_obs_usgs_stations(pt_buffer(GAGE, 15000), parameter_code = c("00060", "00010"))
  keep("usgs_st", st)
  expect(nrow(st) > 0 && GAGE$site_code %in% st$station_id, "15290000 not among ", nrow(st), " stations: ", paste(utils::head(st$station_id), collapse = ", "))
  dv <- ak_obs_usgs_daily(st[st$station_id == GAGE$site_code, ], parameter_code = "00060", dates = c("2023-06-01", "2023-06-30"))
  expect(nrow(dv) >= 28, nrow(dv), " daily values for June 2023")
  expect(all(dv$variable == "discharge") && all(dv$value > 0), "unexpected variables or values")
  da <- st$drainage_area_km2[st$station_id == GAGE$site_code]
  if (isTRUE(abs(da - GAGE$area_km2) > 2)) note("drainage_area_km2 ", fmt(da), " vs published ", fmt(GAGE$area_km2))
  sprintf("%d stations; June 2023 mean discharge %.0f %s; qualifiers %s", nrow(st), mean(dv$value), dv$unit[1], paste(unique(dv$qualifier), collapse = "/"))
})


# ---- P: probes for the planned modules (not package functions yet) -------------------
# A failure here says which service the observations module and the land-cover
# and weather alternates can't rely on, not that the package is broken.
test("P01", "Probe: USGS Water Data API daily discharge (OGC API 'daily'), 15290000, June 2023", {
  j <- get_json("https://api.waterdata.usgs.gov/ogcapi/v0/collections/daily/items",
                list(monitoring_location_id = "USGS-15290000", parameter_code = "00060", time = "2023-06-01/2023-06-30", f = "json"),
                simplify = TRUE)
  n <- NROW(j$features)
  expect(n > 0, "no daily values returned")
  v <- suppressWarnings(as.numeric(j$features$properties$value))
  sprintf("%d daily values; mean %.0f cfs; property fields: %s", n, mean(v, na.rm = TRUE), paste(utils::head(names(j$features$properties), 10), collapse = ", "))
})

test("P02", "Probe: Water Quality Portal stations near the gage", {
  resp <- httr2::request("https://www.waterqualitydata.us/data/Station/search") |>
    httr2::req_url_query(bBox = sprintf("%.3f,%.3f,%.3f,%.3f", GAGE$lon - 0.3, GAGE$lat - 0.15, GAGE$lon + 0.3, GAGE$lat + 0.15),
                         mimeType = "csv", zip = "no") |>
    httr2::req_timeout(180) |>
    httr2::req_perform()
  x <- utils::read.csv(text = httr2::resp_body_string(resp))
  expect(nrow(x) > 0, "no WQP stations within ~15 km")
  sprintf("%d stations; providers %s", nrow(x), paste(unique(x$ProviderName), collapse = "/"))
})

test("P03", "Probe: NRCS AWDB REST API, Alaska SNOTEL stations", {
  j <- get_json("https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/stations", list(stationTriplets = "*:AK:SNTL", returnForecastPointMetadata = "false"))
  expect(NROW(j) > 10, "fewer than 10 Alaska SNOTEL stations")
  sprintf("%d stations; fields: %s", NROW(j), paste(utils::head(names(j), 10), collapse = ", "))
})

test("P04", "Probe: ACIS (GHCN-Daily and coop) stations near the gage", {
  j <- get_json("https://data.rcc-acis.org/StnMeta",
                list(bbox = sprintf("%.2f,%.2f,%.2f,%.2f", GAGE$lon - 1, GAGE$lat - 0.5, GAGE$lon + 1, GAGE$lat + 0.5),
                     meta = "name,ll,sids,valid_daterange", elems = "maxt"))
  n <- NROW(j$meta)
  expect(n > 0, "no stations")
  sprintf("%d stations with daily maxt within ~1 degree", n)
})

test("P05", "Probe: ESA WorldCover 2021 10 m COG (AWS), window at the gage", {
  u <- "/vsicurl/https://esa-worldcover.s3.eu-central-1.amazonaws.com/v200/2021/map/ESA_WorldCover_10m_2021_v200_N60W150_Map.tif"
  r <- terra::rast(u)
  w <- terra::crop(r, terra::ext(GAGE$lon - 0.05, GAGE$lon + 0.05, GAGE$lat - 0.03, GAGE$lat + 0.03))
  tab <- table(terra::values(w, mat = FALSE))
  expect(length(tab) > 0, "empty window")
  paste("class counts:", paste(names(tab), tab, sep = "=", collapse = ", "))
})

test("P06", "Probe: ERA5-Land daily through the Open-Meteo archive API (one week)", {
  j <- get_json("https://archive-api.open-meteo.com/v1/archive",
                list(latitude = GAGE$lat, longitude = GAGE$lon, start_date = "2020-07-01", end_date = "2020-07-07",
                     daily = "temperature_2m_mean,precipitation_sum", models = "era5_land", timezone = "UTC"))
  t <- unlist(j$daily$temperature_2m_mean)
  expect(length(t) == 7, length(t), " days returned")
  sprintf("tmean %s C; grid cell elevation %s m", paste(round(t, 1), collapse = "/"), j$elevation %||% "?")
})


# NPS WRST land cover (GRS/ABR 2004-2007, Landsat, 28.5 m): candidate alder source for southern WRST,
# where the ACCS map has no data. Probes before any backend (see claude/handoff_wrst_alder.md).
WRST_VEG <- c(detailed = "https://irma.nps.gov/DataStore/DownloadFile/619340?Reference=2171667",
              structure = "https://irma.nps.gov/DataStore/DownloadFile/619313?Reference=2171664")

test("P07", "Probe: NPS WRST land cover (DataStore detailed 2171667, structure 2171664): unpack, grid, class table, alder classes", level = "full", {
  base <- ak_cache_dir("nps_wrst_veg")
  veg <- lapply(setNames(nm = names(WRST_VEG)), function(nm) {
    dir <- file.path(base, nm)
    if (!dir.exists(dir)) AKCatchments:::extract_archive(AKCatchments:::ak_download(WRST_VEG[[nm]], file.path(base, paste0(nm, ".zip"))), dir)
    for (z in list.files(dir, "\\.zip$", recursive = TRUE, full.names = TRUE)) { utils::unzip(z, exdir = sub("\\.zip$", "", z)); unlink(z) }
    f <- list.files(dir, recursive = TRUE, full.names = TRUE)
    grid <- c(unique(dirname(f[basename(f) == "hdr.adf"])), f[grepl("\\.(tif|img)$", f, ignore.case = TRUE)])
    expect(length(grid) > 0, nm, ": no grid (hdr.adf, .tif or .img) among ", length(f), " files: ", paste(head(basename(f), 20), collapse = ", "))
    if (length(grid) > 1) note(nm, ": ", length(grid), " grids, using ", basename(grid[1]), " (others: ", paste(basename(grid[-1]), collapse = ", "), ")")
    r <- terra::rast(grid[1])
    rat <- terra::cats(r)[[1]]
    rat_src <- "terra::cats()"
    if (is.null(rat) || !nrow(rat)) {
      vat <- f[grepl("\\.vat\\.dbf$", f, ignore.case = TRUE)]
      expect(length(vat) > 0, nm, ": no attribute table from terra::cats() and no .vat.dbf; files: ", paste(head(basename(f), 20), collapse = ", "))
      rat <- sf::st_read(vat[1], quiet = TRUE)
      rat_src <- basename(vat[1])
    }
    r0 <- r; levels(r0) <- NULL
    fq <- terra::freq(r0)
    rat[] <- lapply(rat, \(v) if (is.character(v)) iconv(v, "latin1", "UTF-8") else v)  # NPS tables are Latin-1
    cl <- data.frame(value = rat[[1]], label = do.call(paste, c(Filter(is.character, rat), sep = " | "))) |>
      dplyr::left_join(dplyr::transmute(fq, value, cells = count), by = "value") |>
      dplyr::mutate(area_km2 = cells * prod(terra::res(r)) / 1e6, alder = grepl("alder|alnus", label, ignore.case = TRUE))
    utils::write.csv(cl, file.path(OUT_DIR, paste0("P07_wrst_classes_", nm, ".csv")), row.names = FALSE)
    utils::write.csv(cbind(rat, cl[c("cells", "area_km2", "alder")]), file.path(OUT_DIR, paste0("P07_wrst_rat_", nm, ".csv")), row.names = FALSE)
    al <- cl[cl$alder, ]
    note(nm, " alder classes: ", if (nrow(al)) paste(sprintf("%s [%s km2]", al$label, fmt(al$area_km2, 0)), collapse = "; ") else "none")
    list(grid = grid[1], classes = cl, rat = rat, msg = sprintf("%s: %s, RAT from %s, %d classes (%s columns), %d alder (%s km2); CRS EPSG %s, %s m",
         nm, basename(grid[1]), rat_src, nrow(cl), paste(names(rat), collapse = "/"), nrow(al), fmt(sum(al$area_km2, na.rm = TRUE), 0),
         terra::crs(r, describe = TRUE)$code %||% "?", paste(fmt(terra::res(r)), collapse = " x ")))
  })
  keep("wrst_veg", veg)
  paste(vapply(veg, `[[`, "", "msg"), collapse = "; ")
})

test("P08", "Probe: WRST alder (draft Viereck weights), both products, at all CAKN WRST catchments: coverage, and overlap with ACCS", level = "full", uses = c("wrst_veg", "cakn_sites"), {
  veg <- got("wrst_veg")
  h <- sf::st_read(file.path(CAKN_DATA_DIR, "catchments.gpkg"), layer = "catchments", quiet = TRUE)
  h <- h[grepl("^WRST", h$site_code) & !sf::st_is_empty(h), "site_code"]
  accs <- sf::st_drop_geometry(ak_alder(h))[c("site_code", "alnus_cover", "alnus_coverage_pct")]
  one <- function(use) {
    if (use == "detailed_plotcover") {
      # Plot cover per grid value: ALNUS_COVER where the source records cover by species (DU, FirePro, ISO_CL;
      # GRSAIR/GRSSUPP/ABR record life forms only), else PR_COMP_COVER where the primary component is alder,
      # else 0; value 0 (unmapped) and undefined sources NA
      rat <- veg$detailed$rat
      pct <- dplyr::case_when(rat[[1]] == 0 | rat$SOURCE_COVER %in% c("GRSUNDF", "UNCLASS") ~ NA_real_,
                              rat$SOURCE_COVER %in% c("DU", "FirePro", "ISO_CL") ~ as.numeric(rat$ALNUS_COVER),
                              grepl("\\baln", rat$PR_COMP) ~ as.numeric(rat$PR_COMP_COVER), .default = 0)
      cl <- data.frame(value = rat[[1]])
      grid <- veg$detailed$grid
    } else {
      cl <- veg[[use]]$classes
      grid <- veg[[use]]$grid
      # Draft heuristic, to review against the class table: closed 75-100% -> 87.5, open 25-75% -> 50, halved when mixed.
    # Value 0 is the unmapped area outside the park (all-zero attributes): NA, like cloud and unclassified
    w <- ifelse(grepl("closed", cl$label, ignore.case = TRUE), 87.5, 50) * ifelse(grepl("willow|birch|mixed", cl$label, ignore.case = TRUE), 0.5, 1)
    pct <- dplyr::case_when(cl$value == 0 | grepl("cloud|shadow|no data|unclassified|undefined|unknown", cl$label, ignore.case = TRUE) ~ NA_real_,
                            cl$alder ~ w, .default = 0)
    }
    r <- terra::rast(grid); levels(r) <- NULL
    alder <- terra::subst(r, cl$value, pct, others = 0) |>
      terra::mask(r, filename = file.path(ak_cache_dir("nps_wrst_veg"), paste0("wrst_alder_pct_draft_", use, ".tif")), datatype = "FLT4S", overwrite = TRUE)
    e <- exactextractr::exact_extract(alder, sf::st_transform(h, terra::crs(alder)), c("mean", "count"), coverage_area = TRUE, progress = FALSE)
    data.frame(site_code = h$site_code, product = use, wrst_pct = e$mean, wrst_cov = pmin(100, 100 * e$count / as.numeric(sf::st_area(h))))
  }
  products <- c(names(veg), "detailed_plotcover")
  d <- dplyr::bind_rows(lapply(products, one)) |>
    dplyr::left_join(accs, by = "site_code") |>
    dplyr::mutate(south = !(alnus_coverage_pct >= 90) %in% TRUE, wrst_ok = (wrst_cov >= 90) %in% TRUE)
  utils::write.csv(d, file.path(OUT_DIR, "P08_wrst_vs_accs.csv"), row.names = FALSE)
  grDevices::png(file.path(OUT_DIR, "P08_wrst_vs_accs.png"), width = 4200, height = 1500, res = 220)
  graphics::par(mfrow = c(1, length(products)))
  msg <- vapply(products, function(use) {
    x <- d[d$product == use, ]
    ov <- x[x$wrst_ok & !x$south, ]
    so <- x[x$wrst_ok & x$south, ]
    r_p <- if (nrow(ov) > 2) stats::cor(ov$wrst_pct, ov$alnus_cover) else NA
    r_s <- if (nrow(ov) > 2) stats::cor(ov$wrst_pct, ov$alnus_cover, method = "spearman") else NA
    plot(ov$wrst_pct, ov$alnus_cover, pch = 19, col = "grey30", xlim = range(x$wrst_pct[x$wrst_ok], na.rm = TRUE),
         xlab = sprintf("WRST map (%s): alder %%", if (use == "detailed_plotcover") "detailed, plot cover" else paste(use, "draft weights")), ylab = "ACCS Alnus foliar cover %",
         main = sprintf("%s: overlap n = %d, r = %s (Spearman %s)", use, nrow(ov), fmt(r_p, 2), fmt(r_s, 2)))
    graphics::abline(0, 1, lty = 2)
    graphics::rug(so$wrst_pct, col = "firebrick", lwd = 2)
    n_beyond <- if (nrow(ov)) sum(so$wrst_pct > max(ov$wrst_pct)) else NA
    if (is.finite(r_p) && r_p < 0.5) note(use, ": weak overlap correlation (r = ", fmt(r_p, 2), ")")
    if (isTRUE(n_beyond > 0)) note(use, ": ", n_beyond, " southern sites have more WRST alder than any overlap site (calibration extrapolates there)")
    sprintf("%s: %d of %d catchments with WRST coverage >= 90%% (min %s%%); without ACCS %d, covered %d; overlap n %d, r %s (Spearman %s); WRST alder %% overlap 0-%s, southern 0-%s",
            use, sum(x$wrst_ok), nrow(x), fmt(min(x$wrst_cov, na.rm = TRUE), 0), sum(x$south), nrow(so), nrow(ov), fmt(r_p, 2), fmt(r_s, 2),
            fmt(max(ov$wrst_pct)), fmt(max(so$wrst_pct)))
  }, "")
  graphics::mtext("red ticks: sites without ACCS", side = 1, line = 4, adj = 1, col = "firebrick", cex = 0.8)
  grDevices::dev.off()
  paste(msg, collapse = "; ")
})

# ---- Z: cache and unit tests ----------------------------------------------------------
test("Z01", "ak_cache_info() and ak_cache_layout(): manifest and folders", {
  ci <- ak_cache_info()
  lay <- ak_cache_layout()
  expect(is.data.frame(lay) && nrow(lay) > 5, "ak_cache_layout() returned no table")
  dl <- ci$downloads
  if (nrow(dl)) {
    gone <- dl$file[!file.exists(file.path(ci$root, dl$file))]
    if (length(gone)) note(length(gone), " manifest entries point to missing files, e.g. ", gone[1])
  } else note("cache_manifest.csv is empty or missing")
  parts <- list.files(ci$root, "\\.part$", recursive = TRUE)
  if (length(parts)) note(length(parts), " leftover .part files (interrupted downloads): ", paste(utils::head(parts, 3), collapse = ", "))
  sprintf("%d downloads logged; %.2f GB in %d folders", nrow(dl), sum(ci$folders$gb), nrow(ci$folders))
})

test("Z03", "ak_prefetch(): fire, soils and the manual layers for the test catchments", uses = "catch", {
  p <- ak_prefetch(got("catch"), c("fire", "soils", "permafrost", "glaciers"))
  expect(!any(p$status == "failed"), "failed: ", paste(p$layer[p$status == "failed"], p$detail[p$status == "failed"], collapse = "; "))
  paste(sprintf("%s: %s", p$layer, p$status), collapse = "; ")
})

test("Z04", "Your shared cache's old fire perimeter file: what area it covers (read only)", skip_if = {
  root <- Sys.getenv("AKCATCHMENTS_CACHE", Sys.getenv("CAKN_DATABASE_CACHE", ""))
  if (!nzchar(root) || !file.exists(file.path(root, "fire_history_ak.gpkg"))) "no fire_history_ak.gpkg in AKCATCHMENTS_CACHE or CAKN_DATABASE_CACHE"
}, {
  f <- file.path(Sys.getenv("AKCATCHMENTS_CACHE", Sys.getenv("CAKN_DATABASE_CACHE")), "fire_history_ak.gpkg")
  lay <- sf::st_layers(f)
  p <- sf::st_read(f, layer = setdiff(lay$name, "coverage")[1], quiet = TRUE)
  bb <- sf::st_bbox(sf::st_transform(p, 4326))
  yr <- suppressWarnings(range(as.integer(p$FIREYEAR), na.rm = TRUE))
  statewide <- bb[["xmin"]] < -160 && bb[["xmax"]] > -142 && bb[["ymin"]] < 58 && bb[["ymax"]] > 69
  if (!statewide) note("not statewide: catchments outside lon ", fmt(bb[["xmin"]], 2), " to ", fmt(bb[["xmax"]], 2), ", lat ",
                       fmt(bb[["ymin"]], 2), " to ", fmt(bb[["ymax"]], 2), " trigger a new query")
  sprintf("layers %s; %d perimeters, %d-%d; lon %.2f to %.2f, lat %.2f to %.2f; coverage layer: %s", paste(lay$name, collapse = "/"),
          nrow(p), yr[1], yr[2], bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]], "coverage" %in% lay$name)
})

test("Z02", "Package unit tests (offline testthat suite)", pkgs = "testthat",
     skip_if = if (!RUN_UNIT_TESTS) "RUN_UNIT_TESTS is FALSE" else if (!nzchar(AK_PKG_PATH)) "needs AK_PKG_PATH (the package source folder)", {
  cache_now <- ak_cache_dir()
  r <- tryCatch(testthat::test_local(AK_PKG_PATH, reporter = "silent", stop_on_failure = FALSE), finally = ak_set_cache(cache_now))
  res <- as.data.frame(r)
  bad <- res[res$failed > 0 | res$error, ]
  why <- unlist(lapply(r, \(t) vapply(Filter(\(e) inherits(e, c("expectation_failure", "expectation_error")), t$results),
                                      \(e) substr(gsub("\\s+", " ", conditionMessage(e)), 1, 400), "")))
  expect(!nrow(bad), nrow(bad), " failing tests: ", paste(bad$file, bad$test, sep = " > ", collapse = "; "),
         "\nFirst messages:\n", paste(utils::head(why, 4), collapse = "\n"))
  sprintf("%d tests, %d expectations, %d skipped", nrow(res), sum(res$nb), sum(res$skipped))
})


# ==============================================================================
# Done
# ==============================================================================
write_report()
st <- vapply(mget(intersect(ORDER, ls(.R)), envir = .R), `[[`, "", "status")
cat("\n==============================================================\n")
cat(sprintf("PASS %d | WARN %d | FAIL %d | SKIP %d   (%s)\n", sum(st == "PASS"), sum(st == "WARN"), sum(st == "FAIL"), sum(st == "SKIP"),
            format(round(difftime(Sys.time(), RUN_START, units = "mins"), 1))))
cat("Report:  ", REPORT_MD, "\nFigures: ", FIG, "\n")
cat("Paste the whole report into the chat with Claude.\n")
cat("==============================================================\n")
