#' Running AKCatchments on many sites: progress and parallel processing
#'
#' Every function that loops over sites shows a progress bar naming the
#' site it is on (in scripts and knitr the bar is printed every few
#' seconds; `options(AKCatchments.progress = FALSE)` turns bars off). Set `options(AKCatchments.verbose = TRUE)` to
#' also get one line per finished site with its time, which is what you want
#' in the log of a long batch job. A failure at one site is recorded in
#' `attr(x, "errors")` (`site_code`, `step`, `error`) and does not stop the
#' others.
#'
#' **Parallel processing is opt-in and set up by you**, with the mirai
#' package, which purrr (>= 1.1.0) uses for [purrr::in_parallel()]:
#'
#' ```r
#' mirai::daemons(4)     # 4 background R processes
#' x <- ak_catchments(pts)
#' x <- ak_topography(x)
#' mirai::daemons(0)     # stop them
#' ```
#'
#' With daemons running, these loops use them:
#' * [ak_catchments()]: one site (or, with
#'   `regional = TRUE`, one shared window) per process;
#' * [import_catchments()], [ak_topography()], [ak_outlet_slope()]: one
#'   site's DEM per process;
#' * the positional draws of [positional_uncertainty()],
#'   [ak_permafrost()] (Jorgenson map) and [ak_glaciers()]: one site per
#'   process;
#' * [ak_soils()]: one Soil Data Access query per site;
#' * [ak_climate_projection()]: one SNAP coverage (variable, model, run) per
#'   process;
#' * [ak_peak_ndvi()]: one group of nearby catchments (which share Landsat
#'   scenes) per process.
#'
#' Without daemons they run in this session, one at a time, with the same
#' results: random draws are seeded per site, not per session.
#' The package never starts or stops daemons itself.
#'
#' What runs in parallel and what does not, and why:
#' * Statewide downloads (land cover, alder, PRISM, geology, Pastick, fire
#'   perimeters, MODIS years) happen once, in this session, before the
#'   per-site work; workers only read the cache, so two processes never write
#'   the same file. Run [ak_prefetch()] first on a new cache.
#' * Daymet pixels (hundreds of small requests) are requested a few at a
#'   time from this session with [httr2::req_perform_parallel()], daemons or
#'   not (`options(AKCatchments.max_active = 4)`): more connections than
#'   that only gets you throttled. Likewise keep the number of daemons
#'   modest (4-6) for the web-bound loops (SNAP, Landsat, Soil Data Access):
#'   each daemon is another connection to the same server.
#' * Extraction from a statewide raster ([catchment_mean()],
#'   [catchment_class_shares()], land cover, alder) is not parallelised: it
#'   is fast, and reading the same large file from several processes is
#'   slower, not faster.
#' * WhiteboxTools is itself multithreaded. With `n` daemons each process is
#'   given `max(1, cores %/% n)` threads (`options(AKCatchments.wbt_threads
#'   = )` to override), so the machine is not oversubscribed.
#' * Workers load the installed package: after changing the source, install
#'   it again (`devtools::install()`) before running in parallel. Under
#'   `pkgload::load_all()` the loops run sequentially, with a message.
#'
#' Memory: each worker holds one site's DEM window and its derived rasters,
#' up to a few hundred MB for the largest catchments at 5 m. With 16 GB of
#' RAM, 4 workers is a safe start.
#'
#' See `vignette("workflow")` for a complete run with timings and
#' recommendations.
#'
#' @name ak_parallel
#' @aliases AKCatchments-parallel
NULL

# Number of mirai daemons available for purrr::in_parallel(), 0 if none (or if the package is loaded from
# source, where workers would load a different, installed copy)
parallel_workers <- function() {
  if (!requireNamespace("mirai", quietly = TRUE) || !exists("in_parallel", asNamespace("purrr"))) return(0L)
  n <- tryCatch(as.integer(mirai::status()$connections), error = \(e) 0L)
  if (!length(n) || is.na(n) || n < 1) return(0L)
  if (dev_loaded()) {
    if (!isTRUE(getOption("AKCatchments.dev_parallel_warned"))) {
      message("AKCatchments is loaded from source (pkgload); mirai workers would load the installed copy, so loops run ",
              "sequentially. Install the package to run in parallel.")
      options(AKCatchments.dev_parallel_warned = TRUE)
    }
    return(0L)
  }
  n
}

dev_loaded <- function() {
  path <- tryCatch(getNamespaceInfo("AKCatchments", "path"), error = \(e) "")
  length(list.files(file.path(path, "R"), "\\.R$")) > 0 && !isTRUE(getOption("AKCatchments.allow_dev_parallel"))
}

# What a worker needs from this session: the cache folder, the WhiteboxTools executable and its thread count
worker_state <- function(n_workers = 1) {
  exe <- tryCatch(whitebox::wbt_exe_path(shell_quote = FALSE), error = \(e) NULL)
  cores <- tryCatch(parallel::detectCores(logical = FALSE), error = \(e) NA_integer_)
  list(cache = cache_root(), exe = if (length(exe) && file.exists(exe)) exe,
       threads = getOption("AKCatchments.wbt_threads") %||% max(1L, (cores %|NA|% 1L) %/% max(1L, n_workers)),
       verbose = isTRUE(getOption("AKCatchments.verbose")))
}

`%|NA|%` <- function(a, b) if (length(a) && !is.na(a)) a else b

worker_init <- function(s) {
  if (!is.null(s$cache)) options(AKCatchments.cache = s$cache)
  options(AKCatchments.verbose = s$verbose)
  if (!is.null(s$exe)) suppressMessages(whitebox::wbt_init(exe_path = s$exe))
  wbt_threads(s$threads)
  invisible(TRUE)
}

wbt_threads <- function(n) {
  if (!is.null(n) && "max_procs" %in% names(formals(whitebox::wbt_options))) whitebox::wbt_options(max_procs = as.integer(n))
}

# exactextractr's own progress bar (one tick per catchment) for statewide maps
extract_progress <- function(x) show_progress() && interactive() && NROW(x) > 1

# Should progress be shown? Off in tests and when asked
show_progress <- function() !isFALSE(getOption("AKCatchments.progress")) && !identical(Sys.getenv("TESTTHAT"), "true")

# Apply `f(item, ...)` to each site's item, sequentially with a progress bar naming the site, or on mirai
# daemons when they are running and `parallel = TRUE`. Errors are caught per site. Returns a list, named by
# `ids`, of list(value =) or list(error =), each with `seconds`.
site_map <- function(items, f, ..., ids = names(items), label = "Processing", parallel = TRUE) {
  ids <- ids %||% as.character(seq_along(items))
  n <- length(items)
  if (!n) return(list())
  args <- list(...)
  workers <- if (parallel && n > 1) parallel_workers() else 0L
  if (workers > 0) {
    state <- worker_state(workers)
    init <- worker_init
    one <- site_run
    g <- purrr::in_parallel(function(item, id) {
      init(state)
      one(item, id, f, args, state$verbose, label)
    }, init = init, state = state, one = one, f = f, args = args, label = label)
    out <- purrr::map2(items, ids, g, .progress = if (show_progress()) list(name = paste0(label, " (", workers, " workers)")) else FALSE)
  } else {
    verbose <- isTRUE(getOption("AKCatchments.verbose"))
    pb <- if (show_progress() && n > 1) {
      cli::cli_progress_bar(total = n, format = paste0(label, " {cli::pb_bar} {cli::pb_current}/{cli::pb_total} | {site} | ETA {cli::pb_eta}"),
                            .envir = environment(), clear = TRUE)
    }
    out <- vector("list", n)
    for (i in seq_len(n)) {
      site <- ids[i]
      if (!is.null(pb)) cli::cli_progress_update(id = pb, set = i - 1, .envir = environment())
      out[[i]] <- site_run(items[[i]], ids[i], f, args, verbose, label)
    }
    if (!is.null(pb)) cli::cli_progress_done(id = pb, .envir = environment())
  }
  names(out) <- ids
  out
}

# One site: the value or the error, with the time taken
site_run <- function(item, id, f, args, verbose, label) {
  t0 <- Sys.time()
  out <- tryCatch(list(value = do.call(f, c(list(item), args))), error = \(e) list(error = conditionMessage(e)))
  out$seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (verbose) message(sprintf("%s %s: %s (%.0f s)", label, id, if (is.null(out$error)) "done" else paste("failed:", out$error),
                               out$seconds))
  out
}

site_values <- function(res) purrr::compact(purrr::map(res, "value"))

site_errors <- function(res, step) {
  bad <- vapply(res, \(r) !is.null(r$error), TRUE)
  dplyr::tibble(site_code = names(res)[bad], step = rep(step, sum(bad)), error = vapply(res[bad], \(r) r$error, ""))
}

# Append error rows to an object's errors attribute (with a message)
add_errors <- function(x, errors, total = NULL) {
  if (!NROW(errors)) return(x)
  message(nrow(errors), if (!is.null(total)) paste0(" of ", total), " sites failed in ", paste(unique(errors$step), collapse = ", "),
          "; see attr(x, \"errors\")")
  attr(x, "errors") <- dplyr::bind_rows(attr(x, "errors"), errors)
  x
}

# Many small web requests, a few at a time. `reqs`: list of httr2 requests; `paths`: optional files to write
# each body to. Returns the responses (or error conditions) in order.
http_parallel <- function(reqs, paths = NULL, label = "Downloading") {
  if (!length(reqs)) return(list())
  max_active <- getOption("AKCatchments.max_active", 4L)
  args <- list(reqs, paths = paths, on_error = "continue", progress = if (show_progress() && length(reqs) > 1) label else FALSE)
  if ("max_active" %in% names(formals(httr2::req_perform_parallel))) {
    args$max_active <- max_active
    do.call(httr2::req_perform_parallel, args)
  } else {
    # httr2 < 1.1 has no max_active: send in batches of that size
    idx <- split(seq_along(reqs), ceiling(seq_along(reqs) / max_active))
    unlist(lapply(idx, \(i) {
      a <- args
      a[[1]] <- reqs[i]
      a$paths <- paths[i]
      a$progress <- FALSE
      do.call(httr2::req_perform_parallel, a)
    }), recursive = FALSE)
  }
}

# ---- catchments in, catchments out -------------------------------------------------------------------

# The object new columns are appended to: the caller's sf (its CRS and columns kept), or the polygons of
# delineation results
catchment_base <- function(x) {
  y <- catchment_sf(x)
  if (!inherits(y, "sf") || !"site_code" %in% names(y)) stop("`catchments` must be sf polygons with a site_code column (see ak_catchments())", call. = FALSE)
  check_catchments(y)
  if (inherits(x, "ak_catchments") || !is.null(attr(x, "ak_provenance"))) return(y)
  if (is.null(attr(y, "ak_provenance"))) attr(y, "ak_provenance") <- provenance_row(NULL)
  y
}

check_catchments <- function(x) {
  d <- unique(x$site_code[duplicated(x$site_code)])
  if (length(d)) stop("`catchments` has more than one row for: ", paste(utils::head(d, 5), collapse = ", "),
                      ". Union them first, e.g. dplyr::summarise(x, .by = site_code).", call. = FALSE)
  invisible(x)
}

# Append a one-row-per-site table of new columns to the catchments, replacing columns of the same name (with a
# message), and add the provenance row. `row` is a catalog row (layer_row()) or NULL for a user raster/map.
add_columns <- function(catchments, new, row, args = list(), note = "", errors = NULL, provenance = NULL) {
  x <- catchment_base(catchments)
  new <- sf::st_drop_geometry(as.data.frame(new))
  if (!"site_code" %in% names(new)) stop("internal: no site_code in the new columns")
  new <- new[!duplicated(new$site_code), , drop = FALSE]
  cols <- setdiff(names(new), c("site_code", "area_m2"))
  geom <- attr(x, "sf_column")
  clash <- intersect(cols, setdiff(names(x), geom))
  if (length(clash)) message("Replacing ", length(clash), " existing column(s): ", paste(utils::head(clash, 8), collapse = ", "),
                             if (length(clash) > 8) ", ...")
  keep <- attributes(x)[intersect(c("ak_provenance", "errors", "delineations"), names(attributes(x)))]
  i <- match(x$site_code, new$site_code)
  for (k in cols) x[[k]] <- new[[k]][i]
  x <- x[c(setdiff(names(x), geom), geom)]
  for (a in names(keep)) attr(x, a) <- keep[[a]]
  attr(x, "ak_provenance") <- dplyr::bind_rows(keep$ak_provenance, provenance %||% provenance_row(row, cols, deparse_args(args), note))
  x <- ak_class(x)
  add_errors(x, errors, nrow(x))
}

ak_class <- function(x) {
  class(x) <- c("ak_catchments", setdiff(class(x), "ak_catchments"))
  x
}

# A long table (time series, per-model or per-sample values) with its provenance and per-site errors
as_layer_table <- function(tbl, row, args = list(), note = "", errors = NULL) {
  keep <- attributes(tbl)[setdiff(names(attributes(tbl)), c("names", "row.names", "class"))]
  tbl <- dplyr::as_tibble(tbl)
  attr(tbl, "ak_provenance") <- dplyr::bind_rows(attr(tbl, "ak_provenance"), provenance_row(row, setdiff(names(tbl), "site_code"),
                                                                                             deparse_args(args), note))
  for (a in setdiff(names(keep), "ak_provenance")) attr(tbl, a) <- keep[[a]]
  add_errors(tbl, errors)
}

#' Join a one-row-per-site table onto catchments
#'
#' Functions that return long tables (daily weather, snow-off by year,
#' projected change per model, annual peak NDVI, values at sample dates) are
#' summarised to one row per site by their companion functions
#' ([projection_ensemble()] with `wide = TRUE`, [peak_ndvi_summary()]); this
#' puts such a table on the catchments, with its provenance. Columns already
#' on `x` are replaced, with a message. A table with more than one row per
#' site is refused: summarise it first.
#'
#' @param x Catchments: sf polygons with a unique `site_code`, e.g. from
#'   [ak_catchments()].
#' @param tbl Data frame with `site_code` and one row per site.
#' @return `x` with the columns of `tbl` added, and the provenance of `tbl`
#'   appended to [ak_provenance()].
#' @examples
#' \dontrun{
#' x <- catchment_join(x, peak_ndvi_summary(ak_peak_ndvi(x, years = 2000:2024)))
#' }
#' @export
catchment_join <- function(x, tbl) {
  tbl <- sf::st_drop_geometry(as.data.frame(tbl))
  if (!"site_code" %in% names(tbl)) stop("`tbl` has no site_code column", call. = FALSE)
  if (anyDuplicated(tbl$site_code)) stop("`tbl` has more than one row per site; summarise it to one row per site_code first", call. = FALSE)
  prov <- attr(tbl, "ak_provenance")
  if (!NROW(prov)) prov <- provenance_row(list(layer = "user_table", variant = "", fn = "catchment_join"), setdiff(names(tbl), "site_code"))
  add_columns(x, tbl, NULL, provenance = prov, errors = attr(tbl, "errors"))
}
