#' Compare two sets of catchment polygons
#'
#' For each site present in both sets, measures how well the polygons agree:
#' area ratio, intersection over union (IoU, Jaccard), the share of each
#' polygon inside the other, and the Hausdorff distance (the largest gap
#' between the two boundaries). Use it for the two validation pathways:
#' automated delineations against hand-drawn catchments, and against USGS
#' StreamStats ([ak_streamstats()]) or the USGS gaged-basin boundaries.
#'
#' `agree` is `TRUE` when IoU >= `iou_min` and the areas are within a factor
#' of `ratio_max`. Thresholds are judgement calls; the continuous measures
#' are what to report.
#'
#' @param a,b sf polygons with a `site_code` column (any CRS).
#' @param iou_min,ratio_max Agreement thresholds.
#' @return A data frame, one row per shared site.
#' @export
compare_catchments <- function(a, b, iou_min = 0.8, ratio_max = 1.25) {
  a <- as_catchments(a)
  b <- as_catchments(b)
  ids <- intersect(a$site_code, b$site_code)
  purrr::map(ids, \(s) {
    ga <- sf::st_union(sf::st_geometry(a[a$site_code == s, ]))
    gb <- sf::st_union(sf::st_geometry(b[b$site_code == s, ]))
    inter <- as.numeric(sf::st_area(suppressWarnings(sf::st_intersection(ga, gb))))
    inter <- if (length(inter)) sum(inter) else 0
    area_a <- as.numeric(sf::st_area(ga))
    area_b <- as.numeric(sf::st_area(gb))
    dplyr::tibble(site_code = s, area_a_km2 = area_a / 1e6, area_b_km2 = area_b / 1e6, area_ratio = area_a / area_b,
                  iou = inter / (area_a + area_b - inter), a_inside_b = inter / area_a, b_inside_a = inter / area_b,
                  hausdorff_m = as.numeric(sf::st_distance(ga, gb, which = "Hausdorff")))
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(agree = .data$iou >= iou_min & abs(log(.data$area_ratio)) <= log(ratio_max))
}
