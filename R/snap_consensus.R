# Snapping with evidence from the terrain and from a reference stream network
#
# Candidates are the DEM channel cells within the snap radius (as for every rule). Two pieces of evidence:
# * flow path: the cell lies on the sample point's own D8 flow path, so the point drains through it;
# * reference area: the cell's contributing area fits the area range (upstream to downstream end) of a
#   reach of a reference network within `snap_reference_dist_m` of the point, within a factor `tol`.
# "reference" picks the best area fit alone. "consensus" treats the flow path as a constraint: the outlet
# must be downstream of the point, so a snap never crosses into another watershed. Among the downstream
# candidates the reference chooses (best area fit); with no fit, the largest downstream channel (the
# "largest" rule restricted to the point's own drainage). With no channel on the path within the radius,
# the best reference fit, else the largest.

# Upstream-area range of each reach, km2, from whichever attribute the network carries
reach_area_km2 <- function(x) {
  n <- names(x)
  if (all(c("USContArea", "DSContArea") %in% n)) return(data.frame(lo = x$USContArea / 1e6, hi = x$DSContArea / 1e6))
  hi <- if ("uparea" %in% n) x$uparea else if ("UPLAND_SKM" %in% n) x$UPLAND_SKM else if ("area_hi_km2" %in% n) x$area_hi_km2
  if (is.null(hi)) {
    stop("The snap reference has no upstream-area attribute (USContArea/DSContArea, uparea, UPLAND_SKM or area_hi_km2): ",
         "use snap_reference = \"geoglows\", \"tdx\", \"merit\" or \"hydrorivers\"", call. = FALSE)
  }
  lo <- if ("area_lo_km2" %in% n) x$area_lo_km2 else hi / 2
  data.frame(lo = lo, hi = hi)
}

# Reaches of the reference network near the point, with their area range and distance
snap_reaches <- function(pt, snap_reference, dist_m) {
  if (is.null(snap_reference)) return(NULL)
  zone <- sf::st_buffer(pt, dist_m)
  x <- if (is.character(snap_reference)) ak_flowlines(zone, snap_reference) else
    sf::st_filter(sf::st_transform(snap_reference, sf::st_crs(pt)), zone)
  if (!nrow(x)) return(data.frame(lo = numeric(), hi = numeric(), dist_m = numeric()))
  data.frame(reach_area_km2(x), dist_m = as.numeric(sf::st_distance(x, pt)))
}

# Cells along the D8 flow path from xy, k steps at most
d8_path <- function(ptr, xy, k) {
  cell <- terra::cellFromXY(ptr, rbind(xy[1:2]))
  out <- cell
  for (i in seq_len(k)) {
    if (is.na(cell)) break
    j <- match(ptr[cell][1, 1], d8$code)
    if (is.na(j)) break
    rc <- terra::rowColFromCell(ptr, cell)
    cell <- terra::cellFromRowCol(ptr, rc[1] + d8$dr[j], rc[2] + d8$dc[j])
    out <- c(out, cell)
  }
  as.vector(stats::na.omit(out))
}

# Evidence for each candidate cell (rows of `a`: cell, x, y, km2, dist) and the cell each rule picks
snap_evidence <- function(a, xy, pointer_file, reaches, snap_dist_m, tol = 1.5) {
  ptr <- terra::rast(pointer_file)
  path <- d8_path(ptr, xy, ceiling(3 * snap_dist_m / terra::res(ptr)[1]) + 5)
  a$path_pos <- match(a$cell, path)
  a$on_path <- !is.na(a$path_pos)
  has_ref <- NROW(reaches) > 0
  a$misfit <- if (has_ref) vapply(a$km2, \(k) min(pmax(0, log(reaches$lo / k), log(k / reaches$hi))), 0) else Inf
  a$area_match <- a$misfit <= log(tol)
  down <- a[a$on_path, ]
  best_fit <- \(d) d[order(d$misfit, d$dist)[1], ]
  largest <- \(d) d[order(-d$km2, d$dist)[1], ]
  fit_down <- nrow(down) > 0 && any(down$area_match)
  fit_any <- has_ref && any(a$area_match)
  consensus <- if (fit_down) best_fit(down[down$area_match, ]) else if (nrow(down)) largest(down) else if (fit_any) best_fit(a) else largest(a)
  how <- if (fit_down) "flowpath+reference" else if (nrow(down)) "flowpath (largest downstream)" else
    if (fit_any) "reference (no channel on the flow path)" else "largest (no evidence)"
  picks <- list(largest = largest(a), reference = if (has_ref) best_fit(a), consensus = consensus)
  alternatives <- purrr::imap(purrr::compact(picks), \(p, rule) data.frame(rule = rule, cell = p$cell, x = p$x, y = p$y, km2 = p$km2)) |>
    purrr::list_rbind()
  fit_reach <- \(k) if (has_ref) reaches[which.min(pmax(0, log(reaches$lo / k), log(k / reaches$hi))), ]
  list(a = a, picks = picks, alternatives = alternatives, has_ref = has_ref, how = how,
       conflict = fit_any && !isTRUE(consensus$area_match),
       n_reaches = NROW(reaches), ref_dist_m = if (has_ref) min(reaches$dist_m) else NA_real_,
       ref_best = if (has_ref) fit_reach(picks$reference$km2))
}

# Columns describing the evidence behind a pick, added to the snapped point
snap_evidence_columns <- function(ev, pick) {
  area <- \(r) if (is.null(ev$picks[[r]])) NA_real_ else ev$picks[[r]]$km2
  p <- ev$a[ev$a$cell == pick$cell, ][1, ]
  data.frame(snap_on_path = p$on_path, snap_area_match = p$area_match,
             snap_evidence = ev$how, snap_conflict = ev$conflict, snap_n_reaches = ev$n_reaches, snap_ref_dist_m = ev$ref_dist_m,
             snap_ref_lo_km2 = if (is.null(ev$ref_best)) NA_real_ else ev$ref_best$lo,
             snap_ref_hi_km2 = if (is.null(ev$ref_best)) NA_real_ else ev$ref_best$hi,
             snap_area_largest_km2 = area("largest"), snap_area_reference_km2 = area("reference"),
             snap_area_consensus_km2 = area("consensus"))
}

# Catchments of the outlets the other rules would have picked, on the same rasters (only where they differ
# from the pick). `truncated` means the window chosen for the pick cuts that catchment off.
snap_alternative_polygons <- function(snapped, f, wd, tag) {
  alts <- attr(snapped, "alternatives")
  if (!NROW(alts)) return(NULL)
  pick_xy <- sf::st_coordinates(snapped)[1, ]
  alts <- alts[abs(alts$x - pick_xy[1]) > 1e-6 | abs(alts$y - pick_xy[2]) > 1e-6, ]
  if (!nrow(alts)) return(NULL)
  purrr::map(seq_len(nrow(alts)), \(i) {
    p <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(alts$x[i], alts$y[i])), crs = sf::st_crs(snapped)))
    ws <- watershed_raster(p, f, wd, tag = paste0(tag, "_alt_", alts$rule[i]))
    g <- terra::as.polygons(ws) |> sf::st_as_sf() |> sf::st_make_valid() |> sf::st_union()
    sf::st_sf(rule = alts$rule[i], snap_area_km2 = alts$km2[i], area_km2 = as.numeric(sf::st_area(g)) / 1e6,
              truncated = touches_frame(ws), geometry = g)
  }) |>
    dplyr::bind_rows()
}
