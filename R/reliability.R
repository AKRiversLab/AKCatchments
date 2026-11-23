#' How much of each attribute's spread across sites is error
#'
#' For every column with a standard error alongside it (`<var>_se`, or a
#' 95% interval `<var>_lo95` / `<var>_hi95`, converted as width / 3.92),
#' compares the error with the spread of the values across sites (or rows):
#' * `sd_between`: SD of the values;
#' * `rms_se`: root mean square of the SEs (the typical error);
#' * `reliability`: 1 - mean(SE^2) / var(values), the share of the observed
#'   variance that is not error (0 to 1).
#'
#' Under classical measurement error (observed = true + independent error), a
#' simple regression slope on the variable is shrunk toward zero by about
#' `reliability` (regression dilution). Map products that are model
#' predictions (alder cover, permafrost probability, interpolated weather)
#' carry errors that are closer to Berkson error (true = mapped + error),
#' which does not bias slopes, only widens their uncertainty. So read
#' `reliability` as the worst case for attenuation, and as a ranking of which
#' variables are most affected by error; see `vignette("uncertainty")`.
#'
#' Error columns describe map and sampling error. Delineation error is not in
#' them; [polygon_spread()] measures it.
#'
#' @param x A data frame (or sf) of attributes, e.g. catchments with layers
#'   added, or a `catchment_static.csv` table.
#' @param vars Variables to assess; default every column with an error
#'   column.
#' @return A data frame: `variable`, `se_from` (`se` or `interval`), `n`,
#'   `mean`, `sd_between`, `rms_se`, `reliability`, sorted by reliability.
#' @examples
#' x <- data.frame(alnus_cover = c(2, 10, 25, 40), alnus_cover_se = c(5, 4, 4, 3),
#'                 totperm_pct = c(20, 40, 60, 80), totperm_pct_lo95 = c(5, 20, 40, 65),
#'                 totperm_pct_hi95 = c(35, 60, 80, 95))
#' attribute_reliability(x)
#' @export
attribute_reliability <- function(x, vars = NULL) {
  d <- sf::st_drop_geometry(as.data.frame(x))
  num <- names(d)[vapply(d, is.numeric, TRUE)]
  base <- num[!grepl("_(se|lo95|hi95|sd)$", num)]
  has_se <- base[paste0(base, "_se") %in% num]
  has_ci <- setdiff(base[paste0(base, "_lo95") %in% num & paste0(base, "_hi95") %in% num], has_se)
  vars <- vars %||% c(has_se, has_ci)
  bad <- setdiff(vars, c(has_se, has_ci))
  if (length(bad)) stop("No _se or _lo95/_hi95 columns for: ", paste(bad, collapse = ", "))
  purrr::map(vars, \(v) {
    se <- if (v %in% has_se) d[[paste0(v, "_se")]] else (d[[paste0(v, "_hi95")]] - d[[paste0(v, "_lo95")]]) / (2 * stats::qnorm(0.975))
    ok <- !is.na(d[[v]]) & !is.na(se)
    val <- d[[v]][ok]
    vx <- stats::var(val)
    dplyr::tibble(variable = v, se_from = if (v %in% has_se) "se" else "interval", n = sum(ok), mean = mean(val),
                  sd_between = sqrt(vx), rms_se = sqrt(mean(se[ok]^2)),
                  reliability = if (isTRUE(vx > 0)) max(0, 1 - mean(se[ok]^2) / vx) else NA_real_)
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$reliability)
}
