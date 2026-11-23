#' Area-weighted catchment mean of a raster (your own raster)
#'
#' Partial edge cells count by the area they share with the catchment.
#' `<name>_coverage_pct` is the share of the catchment with data; the mean is
#' over that part only.
#'
#' This is a summary of the map, not of the ground: the error of the map
#' itself does not shrink with the number of cells averaged, because map
#' errors are spatially correlated. Each layer function adds the error
#' columns appropriate to its product; see [ak_layers()] with `details = TRUE`.
#'
#'
#' Use it for any raster the package does not wrap: a local DEM derivative,
#' a snow or vegetation product you have, a model output. The columns are
#' appended to the catchments with a provenance row naming the raster's
#' file (`fn = "catchment_mean"`), so the source travels with the values.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param raster A SpatRaster or file path.
#' @param name Output column name.
#' @return `catchments` with `<name>` and `<name>_coverage_pct` added.
#' @examples
#' \dontrun{
#' x <- catchment_mean(x, "snow_cover_days.tif", "scd_days")
#' }
#' @export
catchment_mean <- function(catchments, raster, name) {
  r <- if (is.character(raster)) terra::rast(raster) else raster
  add_columns(catchments, raster_mean(as_catchments(catchments), r, name), user_row(name, "catchment_mean", r))
}

# A provenance row for a map the package does not catalogue
user_row <- function(name, fn, map) {
  src <- if (inherits(map, "SpatRaster")) paste(basename(terra::sources(map)), collapse = "; ") else if (is.character(map)) basename(map) else "<sf object>"
  list(layer = name, variant = if (nzchar(src)) src else "<in memory>", fn = fn)
}

raster_mean <- function(x, raster, name) {
  r <- if (is.character(raster)) terra::rast(raster) else raster
  e <- exactextractr::exact_extract(r, sf::st_transform(x, terra::crs(r)), c("mean", "count"),
                                    coverage_area = TRUE, progress = extract_progress(x))
  out <- dplyr::tibble(site_code = x$site_code, mean = dplyr::if_else(is.nan(e$mean), NA_real_, e$mean), cov = pmin(100, 100 * e$count / x$area_m2))
  names(out)[2:3] <- c(name, paste0(name, "_coverage_pct"))
  out
}

#' Class shares of a categorical map in each catchment (your own map)
#'
#' Works on a categorical raster (class codes as cell values) or on polygons
#' with a class field. Shares are percent of the mapped part of the
#' catchment; `<prefix>coverage_pct` is the mapped share of the whole.
#'
#'
#' The columns are appended to the catchments with a provenance row naming
#' the map.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param map A SpatRaster, raster path, or sf polygons.
#' @param field Class field, for polygon maps.
#' @param prefix Column prefix.
#' @param labels Optional named vector recoding class values (names) to
#'   labels (values); values not in it are dropped and the rest renormalised.
#' @param of Shares as percent of the `"mapped"` part of the catchment (sum to
#'   100) or of the whole `"catchment"` (unmapped area left out of every
#'   class).
#' @return `catchments` with `<prefix><class>` columns and
#'   `<prefix>coverage_pct` added.
#' @export
catchment_class_shares <- function(catchments, map, field = NULL, prefix = "pct_", labels = NULL, of = c("mapped", "catchment")) {
  add_columns(catchments, class_shares(as_catchments(catchments), map, field, prefix, labels, match.arg(of)),
              user_row(sub("_$", "", prefix), "catchment_class_shares", map))
}

class_shares <- function(x, map, field = NULL, prefix = "pct_", labels = NULL, of = c("mapped", "catchment")) {
  of <- match.arg(of)
  long <- if (inherits(map, "sf")) {
    m <- sf::st_make_valid(sf::st_transform(map[field], akalbers_crs()))
    p <- suppressWarnings(sf::st_intersection(x[c("site_code", "area_m2")], m))
    p$a <- as.numeric(sf::st_area(p))
    sf::st_drop_geometry(p) |>
      dplyr::rename(class = dplyr::all_of(field)) |>
      dplyr::select("site_code", "area_m2", "class", "a")
  } else {
    r <- if (is.character(map)) terra::rast(map) else map
    levels(r) <- NULL
    exactextractr::exact_extract(r, sf::st_transform(x, terra::crs(r)), c("frac", "count"), coverage_area = TRUE,
                                 append_cols = c("site_code", "area_m2"), progress = extract_progress(x)) |>
      tidyr::pivot_longer(dplyr::starts_with("frac_"), names_to = "class", names_prefix = "frac_",
                          values_to = "frac", values_drop_na = TRUE) |>
      dplyr::mutate(a = .data$frac * .data$count) |>
      dplyr::select("site_code", "area_m2", "class", "a")
  }
  long$class <- as.character(long$class)
  if (!is.null(labels)) long <- dplyr::mutate(long, class = unname(labels[.data$class])) |> dplyr::filter(!is.na(.data$class))
  wide <- long |>
    dplyr::summarise(a = sum(.data$a), .by = c("site_code", "area_m2", "class")) |>
    dplyr::mutate(coverage_pct = pmin(100, 100 * sum(.data$a) / .data$area_m2),
                  pct = 100 * .data$a / if (of == "mapped") sum(.data$a) else .data$area_m2, .by = "site_code") |>
    tidyr::pivot_wider(id_cols = c("site_code", "coverage_pct"), names_from = "class", names_prefix = prefix,
                       values_from = "pct", values_fill = 0) |>
    dplyr::rename_with(\(n) paste0(prefix, n), "coverage_pct")
  dplyr::left_join(dplyr::tibble(site_code = x$site_code), wide, by = "site_code") |>
    dplyr::mutate(dplyr::across(dplyr::all_of(paste0(prefix, "coverage_pct")), \(v) dplyr::coalesce(v, 0)))
}

#' Class-share uncertainty from map positional error
#'
#' A polygon map drawn at coarse scale places its boundaries only
#' approximately. This shifts each catchment by random offsets drawn from
#' N(0, `sigma_m`) in x and y (equivalent to shifting the map), recomputes the
#' class shares, and summarises the spread. It answers "how much could the
#' share change if the map boundaries were where they plausibly are": large
#' for small catchments near class boundaries, small for large or uniform
#' ones.
#'
#' `sigma_m` is the SD of the shift in each of x and y, so the distance moved
#' has a Rayleigh distribution: median 1.18 `sigma_m`, 90th percentile 2.15
#' `sigma_m`. Accuracy standards usually quote the 90th percentile (CE90).
#' For a 1:250,000 map the US National Map Accuracy Standard tolerance is 1/50
#' inch at map scale, 127 m, as a CE90 for well-defined points: `sigma_m` =
#' 127 / 2.15 = 59 m. Geologic contacts and vegetation or permafrost
#' boundaries are not well-defined points, so 127 m (about twice the
#' standard) is a reasonable working value, with 2-4 times that as a
#' sensitivity check. The Jorgenson et al. (2008) permafrost map's catalogue
#' entry warns of 1-3 km horizontal error: `sigma_m = 1000` puts the median
#' shift at 1.2 km and the 90th percentile at 2.1 km.
#'
#' The shift moves the whole catchment rigidly, i.e. it treats the map's
#' error as locally uniform across the catchment. Independent errors on each
#' boundary would give less spread for large catchments; see
#' `vignette("uncertainty")`.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param map sf polygons with a class field.
#' @param field Class field.
#' @param sigma_m Standard deviation of the shift, metres.
#' @param n Number of draws.
#' @param summary Return summaries (default) or the draws.
#' @param seed Random seed. Each site gets its own stream from it, so results
#'   are the same run sequentially or in parallel.
#' @param of Shares as percent of the `"mapped"` part of the catchment (for
#'   maps that cover everything, e.g. geology) or of the whole `"catchment"`
#'   (for maps of one feature type, e.g. glacier outlines).
#' @return Long data frame: `site_code`, `class`, and either `pct_mean`,
#'   `pct_sd`, `pct_lo95`, `pct_hi95`, or `draw` and `pct`.
#' @export
positional_uncertainty <- function(catchments, map, field, sigma_m, n = 500, summary = TRUE, seed = 1,
                                   of = c("mapped", "catchment")) {
  draws <- positional_draws(as_catchments(catchments), map, field, sigma_m, n, seed, match.arg(of))
  if (!summary) return(draws)
  draws |>
    dplyr::summarise(pct_mean = mean(.data$pct), pct_sd = stats::sd(.data$pct),
                     pct_lo95 = stats::quantile(.data$pct, 0.025), pct_hi95 = stats::quantile(.data$pct, 0.975),
                     .by = c("site_code", "class"))
}

# Class shares of `n` rigidly shifted copies of each catchment. The map near each catchment is cut here, so a
# worker gets only its piece.
positional_draws <- function(x, map, field, sigma_m, n = 500, seed = 1, of = "mapped") {
  m <- sf::st_make_valid(sf::st_transform(map[field], akalbers_crs()))
  names(m)[1] <- "class"
  items <- lapply(seq_len(nrow(x)), \(i) {
    g <- sf::st_geometry(x[i, ])
    list(site_code = x$site_code[i], g = g, seed = seed + i,
         local = suppressWarnings(sf::st_intersection(m, sf::st_buffer(g, 4 * sigma_m))))
  })
  res <- site_map(items, positional_one, sigma_m = sigma_m, n = n, of = of, ids = x$site_code, label = "Positional draws")
  bad <- site_errors(res, "positional draws")
  if (nrow(bad)) stop("Positional draws failed for ", paste(bad$site_code, bad$error, sep = ": ", collapse = "; "), call. = FALSE)
  purrr::list_rbind(site_values(res))
}

positional_one <- function(it, sigma_m, n, of) {
  g <- it$g
  shift <- with_seed(it$seed, matrix(stats::rnorm(2 * n, 0, sigma_m), ncol = 2))
  # all n shifted copies in one intersection (one GEOS call instead of n)
  shifted <- sf::st_sf(draw = seq_len(n), geometry = sf::st_sfc(lapply(seq_len(n), \(k) g[[1]] + shift[k, ]), crs = akalbers_crs()))
  p <- suppressWarnings(sf::st_intersection(it$local, shifted))
  dplyr::tibble(draw = p$draw, class = as.character(p$class), a = as.numeric(sf::st_area(p))) |>
    dplyr::summarise(a = sum(.data$a), .by = c("draw", "class")) |>
    dplyr::mutate(pct = 100 * .data$a / if (of == "mapped") sum(.data$a) else as.numeric(sf::st_area(g)), .by = "draw") |>
    tidyr::complete(.data$draw, .data$class, fill = list(pct = 0)) |>
    dplyr::mutate(site_code = it$site_code, .before = 1) |>
    dplyr::select("site_code", "draw", "class", "pct")
}

#' Error-adjusted class shares (Olofsson et al. 2014)
#'
#' A classified map's class shares are biased when its errors are
#' asymmetric. Given an accuracy-assessment error matrix (sample counts, rows
#' = map class, columns = reference class), this returns the
#' stratified estimator of the true shares and its standard error for one
#' catchment's mapped composition. The SE reflects the accuracy sample, and
#' assumes the regional error matrix applies to the catchment; small
#' catchments and patch edges are usually worse than the regional figure.
#'
#' For NLCD 2016 Alaska no accuracy assessment exists; the NLCD 2001 Alaska
#' matrix (Selkowitz and Stehman 2011) is the nearest available, and
#' [ak_landcover()] applies it with `error_matrix = "selkowitz2011"`.
#'
#' @param map_pct Named numeric: mapped class shares (%) for one catchment.
#' @param error_matrix Matrix of sample counts with class names on both axes.
#' @return Data frame: `class`, `pct_adjusted`, `se`, `lo95`, `hi95`.
#' @references Olofsson, P. et al. (2014) Good practices for estimating area
#'   and assessing accuracy of land change. Remote Sensing of Environment 148,
#'   42-57.
#' @seealso `vignette("landcover-nlcd")` (download, use, uncertainty, validation)
#' @export
adjust_class_shares <- function(map_pct, error_matrix) {
  cls <- intersect(names(map_pct), rownames(error_matrix))
  w <- map_pct[cls] / sum(map_pct[cls])
  n <- error_matrix[cls, , drop = FALSE]
  if (any(rowSums(n) < 2)) stop("Every map class needs at least 2 reference samples in `error_matrix` (rows are sample counts)")
  q <- n / rowSums(n)
  est <- colSums(w * q)
  se <- sqrt(colSums(w^2 * q * (1 - q) / (rowSums(n) - 1)))
  data.frame(class = colnames(n), pct_adjusted = 100 * est, se = 100 * se,
             lo95 = 100 * pmax(0, est - 1.96 * se), hi95 = 100 * pmin(1, est + 1.96 * se), row.names = NULL)
}

#' Attribute spread across alternative catchment polygons
#'
#' Where a site has more than one plausible polygon (hand-drawn, automated,
#' StreamStats), the attributes computed on each show how much the polygon
#' choice matters. This runs a layer function on every polygon set and
#' reports, per site and numeric column the function added, the number of
#' polygons, the mean, the standard deviation and the range. Report `sd`
#' with the attribute's own `_se` to separate map error from delineation
#' error.
#'
#' @param sets A named list of catchment sets (sf with `site_code`), e.g.
#'   `list(hand = hand, auto = v$catchments, streamstats = v$streamstats)`.
#' @param fun A layer function, such as [ak_alder] or
#'   `\(x) ak_permafrost(x, sigma_m = 0)`. It may return the catchments
#'   with columns added (as the package's layer functions do) or a data
#'   frame with `site_code`.
#' @param ... Passed to `fun`.
#' @return Long data frame: `site_code`, `variable`, `n_polygons`, `mean`,
#'   `sd`, `min`, `max`, plus one column per set with that set's value.
#' @examples
#' \dontrun{
#' polygon_spread(list(hand = hand, auto = x), ak_alder)
#' }
#' @export
polygon_spread <- function(sets, fun, ...) {
  stopifnot(is.list(sets), length(names(sets)) == length(sets))
  purrr::imap(sets, \(s, nm) {
    inp <- as_catchments(s)
    out <- sf::st_drop_geometry(as.data.frame(fun(inp, ...)))
    added <- setdiff(names(out), setdiff(names(inp), "site_code"))
    out[added] |>
      dplyr::select("site_code", dplyr::where(is.numeric)) |>
      tidyr::pivot_longer(-"site_code", names_to = "variable") |>
      dplyr::mutate(set = nm)
  }) |>
    purrr::list_rbind() |>
    dplyr::filter(!grepl("_(se|lo95|hi95|sd)$", .data$variable)) |>
    tidyr::pivot_wider(names_from = "set", values_from = "value") |>
    dplyr::mutate(n_polygons = rowSums(!is.na(dplyr::pick(dplyr::all_of(names(sets))))),
                  mean = rowMeans(dplyr::pick(dplyr::all_of(names(sets))), na.rm = TRUE),
                  sd = apply(dplyr::pick(dplyr::all_of(names(sets))), 1, stats::sd, na.rm = TRUE),
                  min = do.call(pmin, c(dplyr::pick(dplyr::all_of(names(sets))), na.rm = TRUE)),
                  max = do.call(pmax, c(dplyr::pick(dplyr::all_of(names(sets))), na.rm = TRUE))) |>
    dplyr::relocate("n_polygons", "mean", "sd", "min", "max", .after = "variable")
}
