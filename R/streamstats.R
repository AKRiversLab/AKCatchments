#' Delineate a catchment with USGS StreamStats
#'
#' Snaps the point to the StreamStats stream grid and returns the StreamStats
#' delineation (the `globalwatershed` feature), for comparison with
#' [ak_delineate()]. In Alaska, StreamStats delineates only in the Cook Inlet
#' Basin (which includes the Susitna); elsewhere it returns `NULL` with a
#' message.
#'
#' Uses the SS-Delineate service that replaced the StreamStats Services API
#' (decommissioned January 2026): `pourpoint/v1/snap` then
#' `ss-delineate/v1/delineate/sshydro/{region}`, and for basin
#' characteristics `ss-hydro/v1/basin-characteristics/calculate-using-ssdelineate`.
#' StreamStats warns that its delineations can be wrong; a `WarningMsg` in the
#' response is returned in `ss_warning` rather than dropped.
#'
#' A point StreamStats cannot use (outside its delineation regions, or no
#' stream to snap to) gives `NULL` with a message. Server and connection
#' errors stop, so an outage is not mistaken for such a point.
#'
#' @param lat,lon Point in decimal degrees.
#' @param site_code Identifier for the output.
#' @param region StreamStats region code.
#' @param server StreamStats server prefix.
#' @param characteristics Also compute every basin characteristic StreamStats
#'   offers for the basin (one more request, often slow)?
#' @return One-row sf polygon (EPSG:3338) with `site_code`, `ss_lon`, `ss_lat`
#'   (the snapped point), `ss_snap_dist_m`, `ss_area_km2` and `ss_warning`,
#'   or `NULL`. With `characteristics = TRUE`, also `ss_characteristics`: a
#'   list column holding a table of `code`, `name`, `unit` and `value`
#'   (`NA` where StreamStats returns -999).
#' @seealso `vignette("delineation")` (download, use, uncertainty, validation)
#' @export
ak_streamstats <- function(lat, lon, site_code = NA_character_, region = "AK", server = "prodweba", characteristics = FALSE) {
  snap <- tryCatch(ak_json("https://streamstats.usgs.gov/pourpoint/v1/snap/str900",
                           list(region = region, lat = lat, lon = lon), simplify = FALSE),
                   httr2_http = \(e) {
                     if (e$status >= 500) stop(e)
                     message(site_code, ": StreamStats could not use the point (outside a delineation region?): ", conditionMessage(e))
                     NULL
                   })
  if (is.null(snap)) return(NULL)
  if (!isTRUE(snap$couldSnap)) { message(site_code, ": StreamStats could not snap the point"); return(NULL) }
  xy <- unlist(snap$output$coordinates)
  ws <- ak_json(sprintf("https://%s.streamstats.usgs.gov/ss-delineate/v1/delineate/sshydro/%s", server, region),
                list(lat = xy[2], lon = xy[1]), simplify = FALSE)
  entries <- ws$bcrequest$wsresp$featurecollection[[1]]
  gw <- purrr::detect(entries, \(e) identical(e$name, "globalwatershed"))
  if (is.null(gw)) { message(site_code, ": StreamStats returned no watershed"); return(NULL) }
  feats <- gw$feature$features
  whole <- purrr::keep(feats, \(f) identical(f$properties$GlobalWshd, 1L) || identical(f$properties$GlobalWshd, 1))
  feat <- if (length(whole)) whole[[1]] else feats[[1]]
  poly <- sf::read_sf(jsonlite::toJSON(list(type = "FeatureCollection", features = list(feat)), auto_unbox = TRUE, digits = NA), quiet = TRUE)
  u <- unlist(ws)
  warn <- u[grepl("WarningMsg$", names(u)) & nzchar(u)]
  sf::st_crs(poly) <- 4326
  poly <- sf::st_transform(sf::st_union(sf::st_make_valid(poly)), akalbers_crs())
  snap_m <- as.numeric(sf::st_distance(sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326), sf::st_sfc(sf::st_point(xy[1:2]), crs = 4326)))
  out <- sf::st_sf(site_code = site_code, ss_lon = xy[1], ss_lat = xy[2], ss_snap_dist_m = snap_m,
                   ss_area_km2 = as.numeric(sf::st_area(poly)) / 1e6,
                   ss_warning = if (length(warn)) paste(unique(warn), collapse = "; ") else NA_character_, geometry = poly)
  if (characteristics) out$ss_characteristics <- list(ss_characteristics(ws$bcrequest, xy, region, server))
  out
}

# Every basin characteristic SS-Hydro computes for a basin delineated by SS-Delineate (its bcrequest posted back)
ss_characteristics <- function(bcrequest, xy, region, server) {
  bc <- ak_request(sprintf("https://%s.streamstats.usgs.gov/ss-hydro/v1/basin-characteristics/calculate-using-ssdelineate/", server),
                   list(region = region, lat = xy[2], lon = xy[1], BCs = "*")) |>
    httr2::req_body_raw(jsonlite::toJSON(bcrequest, auto_unbox = TRUE, null = "null", digits = NA), type = "application/json") |>
    ak_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(simplifyVector = FALSE)
  purrr::map(bc$parameters %||% bc, \(p) dplyr::tibble(code = p$code, name = p$name, unit = as.character(p$unit %||% NA),
                                                       value = as.numeric(p$value %||% NA))) |>
    dplyr::bind_rows() |>
    dplyr::mutate(value = dplyr::if_else(.data$value %in% c(-999, -999.99), NA_real_, .data$value))
}
