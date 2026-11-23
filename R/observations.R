#' The observation schema
#'
#' Observations (stream temperature, discharge, meteorology, water
#' chemistry, invertebrates, ...) are kept apart from catchment layers, in
#' one long table with one row per value:
#'
#' * `dataset`, `provider`: where the record comes from (e.g. `"AKTEMP-VIZ"`
#'   and `"NPS"`, or `"CAKN chemistry"` and `"NPS CAKN"`);
#' * `station_id`, `station_name`, `lat`, `lon` (decimal degrees, WGS84);
#' * `datetime` (POSIXct, UTC) and/or `date` (Date; local day for daily
#'   values);
#' * `variable`, `value` (numeric), `unit`;
#' * `qualifier` (provider's flags, remark or censoring codes, approval
#'   status), `method` (statistic, analytical method), `depth_m`;
#' * `source_url`, `retrieved` (Date).
#'
#' Functions in the package that return observations use it, and
#' [ak_obs_read()] maps your own tables into it, so a CAKN chemistry file and
#' a USGS gage end up in the same shape. [ak_obs_validate()] checks a table.
#'
#' @return A zero-row tibble with the schema's columns and types.
#' @seealso `vignette("observations")` (download, use, uncertainty, validation)
#' @export
ak_obs_schema <- function() {
  dplyr::tibble(dataset = character(), provider = character(), station_id = character(), station_name = character(),
                lat = numeric(), lon = numeric(), datetime = as.POSIXct(character(), tz = "UTC"), date = as.Date(character()),
                variable = character(), value = numeric(), unit = character(), qualifier = character(),
                method = character(), depth_m = numeric(), source_url = character(), retrieved = as.Date(character()))
}

obs_required <- c("dataset", "station_id", "variable", "value", "unit")

#' Check an observation table against the schema
#'
#' Adds missing optional columns (as `NA`), coerces types, orders the
#' columns as in [ak_obs_schema()] (extra columns are kept after them), and
#' checks: required columns present and not missing (`dataset`,
#' `station_id`, `variable`, `value`, `unit`, and `date` or `datetime`);
#' coordinates in range; one unit per variable within a dataset; no
#' duplicated rows (same dataset, station, time, variable, depth and
#' method).
#'
#' @param x A data frame.
#' @param strict Stop on problems (`TRUE`) or warn and attach them as
#'   attribute `problems` (`FALSE`).
#' @return `x`, conformed to the schema.
#' @export
ak_obs_validate <- function(x, strict = TRUE) {
  x <- sf::st_drop_geometry(as.data.frame(x))
  schema <- ak_obs_schema()
  miss_req <- setdiff(obs_required, names(x))
  if (length(miss_req)) stop("Observation table lacks required column(s): ", paste(miss_req, collapse = ", "), call. = FALSE)
  for (k in setdiff(names(schema), names(x))) x[[k]] <- rep(schema[[k]][NA_integer_], nrow(x))
  chr <- names(schema)[vapply(schema, is.character, TRUE)]
  num <- names(schema)[vapply(schema, is.numeric, TRUE)]
  x[chr] <- lapply(x[chr], as.character)
  x[num] <- lapply(x[num], \(v) suppressWarnings(as.numeric(v)))
  x$date <- as.Date(x$date)
  if (!inherits(x$datetime, "POSIXct")) x$datetime <- as.POSIXct(x$datetime, tz = "UTC")
  x$retrieved <- as.Date(x$retrieved)
  x <- dplyr::as_tibble(x[c(names(schema), setdiff(names(x), names(schema)))])
  units <- dplyr::summarise(x, n = dplyr::n_distinct(.data$unit), .by = c("dataset", "variable"))
  problems <- c(
    missing_required = sum(!stats::complete.cases(x[setdiff(obs_required, "value")])),
    missing_time = sum(is.na(x$date) & is.na(x$datetime)),
    value_not_numeric = sum(is.na(x$value)),
    bad_coordinates = sum(isTRUE_v(abs(x$lat) > 90 | abs(x$lon) > 180)),
    variables_with_several_units = sum(units$n > 1),
    duplicated_rows = sum(duplicated(x[c("dataset", "station_id", "datetime", "date", "variable", "depth_m", "method")])))
  problems <- problems[problems > 0]
  if (length(problems)) {
    msg <- paste0(names(problems), ": ", problems, collapse = "; ")
    if (strict && any(names(problems) != "value_not_numeric")) stop("Observation table problems: ", msg, call. = FALSE)
    warning("Observation table: ", msg, call. = FALSE)
    attr(x, "problems") <- problems
  }
  x
}

#' Read your own observations into the schema
#'
#' Maps a table (a data frame or a CSV path) into [ak_obs_schema()] with
#' `name = expression` pairs, as in [dplyr::transmute()]: expressions can be
#' column names, constants or computations. Wide tables (one column per
#' variable) are pivoted first with `.pivot` (tidyselect), which makes
#' `variable` and `value` columns available to the mappings. The result is
#' checked with [ak_obs_validate()].
#'
#' @param x A data frame, or a path to a CSV file.
#' @param ... Schema column = expression pairs.
#' @param .pivot Columns to pivot longer into `variable` and `value`.
#' @param .strict Passed to [ak_obs_validate()].
#' @return A tibble in the observation schema.
#' @examples
#' chem <- data.frame(site = c("DENA-001", "DENA-001"), collected = c("2019-07-01", "2020-07-03"),
#'                    tn_mg_l = c(0.21, 0.18), tp_mg_l = c(0.004, NA))
#' ak_obs_read(chem, .pivot = c(tn_mg_l, tp_mg_l),
#'             dataset = "CAKN chemistry", provider = "NPS CAKN", station_id = site,
#'             date = as.Date(collected), variable = sub("_mg_l$", "", variable), value = value,
#'             unit = "mg/L", .strict = FALSE)
#' @seealso `vignette("observations")` (download, use, uncertainty, validation)
#' @export
ak_obs_read <- function(x, ..., .pivot = NULL, .strict = TRUE) {
  if (is.character(x)) x <- utils::read.csv(x, stringsAsFactors = FALSE, check.names = FALSE)
  x <- as.data.frame(x)
  piv <- rlang::enquo(.pivot)
  if (!rlang::quo_is_null(piv)) {
    x <- tidyr::pivot_longer(x, !!piv, names_to = "variable", values_to = "value", values_drop_na = TRUE,
                             values_transform = list(value = as.character))
  }
  out <- dplyr::transmute(x, ...)
  ak_obs_validate(out, strict = .strict)
}

#' Stations of an observation table
#'
#' @param obs A table in the observation schema.
#' @return sf points (EPSG:3338), one per dataset and station, with the
#'   number of values, the first and last date, and the variables.
#' @export
ak_obs_stations <- function(obs) {
  obs <- ak_obs_validate(obs, strict = FALSE)
  if (anyNA(obs$lat) || anyNA(obs$lon)) stop("Some observations have no lat/lon: stations cannot be placed")
  d <- dplyr::coalesce(obs$date, as.Date(obs$datetime))
  obs |>
    dplyr::mutate(day = d) |>
    dplyr::summarise(provider = dplyr::first(.data$provider), station_name = dplyr::first(.data$station_name),
                     lat = dplyr::first(.data$lat), lon = dplyr::first(.data$lon), n = dplyr::n(),
                     first = min(.data$day, na.rm = TRUE), last = max(.data$day, na.rm = TRUE),
                     variables = paste(sort(unique(.data$variable)), collapse = ", "), .by = c("dataset", "station_id")) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
    sf::st_transform(akalbers_crs())
}

#' Link observation stations to catchments
#'
#' For each catchment, finds the stations at its outlet (within
#' `outlet_m` of the outlet point), inside it (upstream of the outlet), or
#' near it (within `near_m` of the boundary), with the distances. For
#' stations inside, `station_area_km2` is the contributing area at the
#' station read from the catchment's D8 accumulation raster (catchments from
#' [ak_catchments()]; the largest value within `snap_m`), and
#' `station_area_frac` its share of the catchment: a station draining 5% of
#' the catchment describes a headwater, not the outlet.
#'
#' The outlet is the snapped outlet (`snapped_lon`, `snapped_lat`) of
#' catchments from [ak_catchments()], else the sample point (`lon`, `lat`).
#' Without either, only `inside` and `near` are reported. Whether a station
#' outside the catchment is downstream on the same stream is not judged:
#' check `near` stations on the plot.
#'
#' @param catchments sf polygons with `site_code` (from [ak_catchments()] or
#'   your own).
#' @param stations sf points (any CRS) with `station_id`, e.g. from
#'   [ak_obs_stations()] or [ak_obs_temperature_stations()]; a `dataset`
#'   column is carried along.
#' @param outlet_m Distance from the outlet that counts as at the outlet.
#' @param near_m Distance outside the boundary still reported.
#' @param snap_m Search radius for the station's contributing area.
#' @return A data frame: `site_code`, `dataset`, `station_id`, `relation`
#'   (`at_outlet`, `inside`, `near`), `dist_outlet_m`, `dist_catchment_m`
#'   (0 inside), `station_area_km2`, `station_area_frac`.
#' @seealso `vignette("observations")` (download, use, uncertainty, validation)
#' @export
ak_obs_link <- function(catchments, stations, outlet_m = 500, near_m = 0, snap_m = 100) {
  if (!inherits(stations, "sf") || !"station_id" %in% names(stations)) stop("`stations` must be sf points with a station_id column")
  st <- sf::st_transform(stations, akalbers_crs())
  if (!"dataset" %in% names(st)) st$dataset <- NA_character_
  x <- as_catchments(catchments)
  cd <- sf::st_drop_geometry(catchments)
  outlet_cols <- if (all(c("snapped_lon", "snapped_lat") %in% names(cd))) c("snapped_lon", "snapped_lat") else if (all(c("lon", "lat") %in% names(cd))) c("lon", "lat")
  outlets <- if (!is.null(outlet_cols)) {
    sf::st_as_sf(data.frame(site_code = cd$site_code, cd[outlet_cols]), coords = outlet_cols, crs = 4326) |> sf::st_transform(akalbers_crs())
  }
  acc <- carried_rasters(catchments)
  purrr::map(seq_len(nrow(x)), \(i) {
    g <- sf::st_geometry(x[i, ])
    d_poly <- as.numeric(sf::st_distance(st, g))
    o <- if (!is.null(outlets)) outlets[outlets$site_code == x$site_code[i], ]
    d_out <- if (!is.null(o) && nrow(o)) as.numeric(sf::st_distance(st, o)[, 1]) else rep(NA_real_, nrow(st))
    rel <- dplyr::case_when(isTRUE_v(d_out <= outlet_m) ~ "at_outlet", d_poly == 0 ~ "inside", d_poly <= near_m ~ "near")
    k <- which(!is.na(rel))
    if (!length(k)) return(NULL)
    area <- rep(NA_real_, length(k))
    f <- if (!is.null(acc)) acc$accum[acc$site_code == x$site_code[i]][1]
    if (!is.null(f) && !is.na(f) && file.exists(f)) {
      r <- terra::rast(f)
      inside <- k[d_poly[k] == 0]
      if (length(inside)) {
        v <- terra::extract(r, terra::vect(sf::st_buffer(st[inside, ], snap_m)), fun = max, na.rm = TRUE)
        area[match(inside, k)] <- v[[2]] * prod(terra::res(r)) / 1e6
      }
    }
    dplyr::tibble(site_code = x$site_code[i], dataset = st$dataset[k], station_id = as.character(st$station_id[k]), relation = rel[k],
                  dist_outlet_m = round(d_out[k], 1), dist_catchment_m = round(d_poly[k], 1),
                  station_area_km2 = pmin(area, x$area_m2[i] / 1e6), station_area_frac = pmin(1, area / (x$area_m2[i] / 1e6)))
  }) |>
    purrr::list_rbind() |>
    (\(d) if (is.null(d) || !nrow(d)) dplyr::tibble(site_code = character(), dataset = character(), station_id = character(),
                                                     relation = character(), dist_outlet_m = numeric(), dist_catchment_m = numeric(),
                                                     station_area_km2 = numeric(), station_area_frac = numeric()) else d)() |>
    dplyr::arrange(.data$site_code, factor(.data$relation, c("at_outlet", "inside", "near")), .data$dist_outlet_m)
}

#' Stream temperature from AKTEMP-VIZ, in the observation schema
#'
#' AKTEMP-VIZ (Shaftel, Walker and Larson; Alaska CASC) combines daily water
#' temperature from the AKTEMP database (UAA Alaska Center for Conservation
#' Science), USGS NWIS and the NPS Aquarius portal into one set of files,
#' each paired with ERA5-Land air temperature.
#'
#' `ak_obs_temperature_stations()` lists the AKTEMP-VIZ stations with the schema's station
#' columns added (`dataset`, `provider`, `station_id`, `station_name`, `lat`,
#' `lon`). `ak_obs_temperature()` returns their daily records as
#' observations: `water_temperature` (daily mean, degC) and
#' `air_temperature_era5land` (ERA5-Land daily mean air temperature at the
#' station, degC).
#'
#' Uncertainty and use: values are as supplied by each provider after its
#' own QC; ACCS makes no guarantee of accuracy, and data should be reviewed
#' before analysis. Cite as "Alaska Water Temperature Database (AKTEMP)
#' (year). Water temperature data collected by (provider names). Retrieved
#' from https://aktemp.uaa.alaska.edu on (date)".
#'
#' @param base Base URL of the AKTEMP-VIZ data files.
#' @param stations Rows of `ak_obs_temperature_stations()`.
#' @return `ak_obs_temperature_stations()`: sf points. `ak_obs_temperature()`:
#'   a tibble in [ak_obs_schema()].
#' @seealso `vignette("observations")` (download, use, uncertainty, validation)
#' @export
ak_obs_temperature_stations <- function(base = "https://aktemp.uaa.alaska.edu/viz/data/") {
  s <- aktemp_stations(base)
  pick <- \(cands, default = NA_character_) {
    hit <- intersect(cands, names(s))
    if (length(hit)) as.character(s[[hit[1]]]) else rep(default, nrow(s))
  }
  s$obs_dataset <- paste0("AKTEMP-VIZ: ", pick("dataset"))
  s$provider <- pick(c("provider", "provider_code", "organization", "dataset"))
  if (!"station_id" %in% names(s)) s$station_id <- paste(pick("dataset"), pick("provider_station_code"), sep = ":")
  s$station_name <- pick(c("station_name", "name", "description", "provider_station_code"))
  s$lat <- s$latitude
  s$lon <- s$longitude
  s
}

#' @rdname ak_obs_temperature_stations
#' @export
ak_obs_temperature <- function(stations, base = "https://aktemp.uaa.alaska.edu/viz/data/") {
  if (!"obs_dataset" %in% names(stations)) stop("`stations` must come from ak_obs_temperature_stations()")
  s <- sf::st_drop_geometry(stations)
  d <- aktemp_daily(stations, base)
  meta <- dplyr::distinct(s, .data$dataset, .data$provider_station_code, .keep_all = TRUE)[
    c("dataset", "provider_station_code", "obs_dataset", "provider", "station_id", "station_name", "lat", "lon", "filename")]
  d |>
    dplyr::left_join(meta, by = c("dataset", "provider_station_code")) |>
    tidyr::pivot_longer(c("temp_c", "airtemp_c"), names_to = "variable", values_to = "value", values_drop_na = TRUE) |>
    dplyr::transmute(dataset = .data$obs_dataset, .data$provider, station_id = as.character(.data$station_id), .data$station_name,
                     .data$lat, .data$lon, .data$date,
                     variable = dplyr::if_else(.data$variable == "temp_c", "water_temperature", "air_temperature_era5land"),
                     .data$value, unit = "degC", method = "daily mean", source_url = paste0(base, "data/", .data$filename),
                     retrieved = Sys.Date()) |>
    ak_obs_validate(strict = FALSE)
}

#' USGS monitoring locations and daily values, in the observation schema
#'
#' Through the dataRetrieval package's `read_waterdata_*` functions, which
#' use the USGS Water Data APIs (the replacement for the NWIS web services
#' that USGS is retiring; readNWIS functions are not used).
#'
#' `ak_obs_usgs_stations()` finds stream sites (`site_type_code = "ST"` by
#' default) in the bounding box of `aoi` and, with `parameter_code`, keeps
#' those with a daily time series of any of those parameters (with its
#' period of record). `ak_obs_usgs_daily()` returns their daily values (mean
#' by default) as observations. Common parameter codes: 00060 discharge
#' (ft3/s), 00010 water temperature (degC), 00095 specific conductance,
#' 00300 dissolved oxygen, 00400 pH, 63680 turbidity.
#'
#' A USGS API key (free, from api.waterdata.usgs.gov) raises the hourly
#' request limit; set `API_USGS_PAT` in `.Renviron` as dataRetrieval
#' describes.
#'
#' @param aoi Area of interest (sf, sfc or bbox; any CRS), e.g. catchments.
#' @param site_type_code USGS site type code(s).
#' @param parameter_code Parameter code(s); `NULL` keeps every site.
#' @param buffer_m Margin added around `aoi`.
#' @return `ak_obs_usgs_stations()`: sf points (EPSG:3338) with `dataset`,
#'   `provider`, `station_id` (e.g. `"USGS-15290000"`), `station_name`,
#'   `lat`, `lon`, `drainage_area_km2` and, with `parameter_code`,
#'   `parameters`, `begin`, `end`.
#' @seealso `vignette("observations")` (download, use, uncertainty, validation)
#' @export
ak_obs_usgs_stations <- function(aoi, site_type_code = "ST", parameter_code = c("00060", "00010"), buffer_m = 0) {
  if (!requireNamespace("dataRetrieval", quietly = TRUE)) stop("Install the dataRetrieval package (>= 2.7.18, with the read_waterdata functions)")
  bb <- sf::st_bbox(sf::st_transform(sf::st_buffer(as_aoi(aoi), buffer_m), 4326))
  s <- dataRetrieval::read_waterdata_monitoring_location(bbox = as.numeric(bb[c("xmin", "ymin", "xmax", "ymax")]),
                                                         site_type_code = site_type_code)
  if (!NROW(s)) return(obs_station_sf(data.frame()))
  s <- sf::st_as_sf(s)
  ll <- sf::st_coordinates(sf::st_transform(sf::st_geometry(s), 4326))
  out <- data.frame(dataset = "USGS Water Data", provider = "USGS", station_id = s$monitoring_location_id,
                    station_name = s$monitoring_location_name %||% NA_character_, lat = ll[, 2], lon = ll[, 1],
                    drainage_area_km2 = suppressWarnings(as.numeric(s$drainage_area %||% NA)) * 2.589988)
  if (!is.null(parameter_code)) {
    m <- dataRetrieval::read_waterdata_ts_meta(monitoring_location_id = out$station_id, parameter_code = parameter_code,
                                               computation_period_identifier = "Daily")
    m <- sf::st_drop_geometry(as.data.frame(m))
    if (!nrow(m)) return(obs_station_sf(out[0, ]))
    per <- m |>
      dplyr::summarise(parameters = paste(sort(unique(.data$parameter_code)), collapse = ", "),
                       begin = as.Date(min(as.Date(.data$begin), na.rm = TRUE)), end = as.Date(max(as.Date(.data$end), na.rm = TRUE)),
                       .by = "monitoring_location_id")
    out <- dplyr::inner_join(out, per, by = c(station_id = "monitoring_location_id"))
  }
  obs_station_sf(out)
}

obs_station_sf <- function(d) {
  if (!nrow(d)) d <- data.frame(dataset = character(), provider = character(), station_id = character(), station_name = character(),
                                lat = numeric(), lon = numeric())
  sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |> sf::st_transform(akalbers_crs())
}

#' @rdname ak_obs_usgs_stations
#' @param stations Rows of `ak_obs_usgs_stations()`, or a character vector
#'   of monitoring location ids.
#' @param statistic_id Daily statistic: `"00003"` mean, `"00001"` maximum,
#'   `"00002"` minimum.
#' @param dates Start and end dates, e.g. `c("2010-01-01", "2020-12-31")`;
#'   `NULL` for the whole record.
#' @return `ak_obs_usgs_daily()`: a tibble in [ak_obs_schema()] with
#'   `variable` named from the parameter code (`discharge`,
#'   `water_temperature`, ... or `p<code>`) and `qualifier` holding the
#'   approval status and qualifiers.
#' @export
ak_obs_usgs_daily <- function(stations, parameter_code = "00060", statistic_id = "00003", dates = NULL) {
  if (!requireNamespace("dataRetrieval", quietly = TRUE)) stop("Install the dataRetrieval package (>= 2.7.18, with the read_waterdata functions)")
  ids <- if (is.character(stations)) stations else stations$station_id
  meta <- if (inherits(stations, "data.frame")) sf::st_drop_geometry(stations) else data.frame(station_id = ids)
  args <- list(monitoring_location_id = ids, parameter_code = parameter_code, statistic_id = statistic_id, skipGeometry = TRUE)
  if (!is.null(dates)) args$time <- as.character(dates)
  d <- do.call(dataRetrieval::read_waterdata_daily, args)
  d <- as.data.frame(d)
  if (!nrow(d)) return(ak_obs_schema())
  q <- if ("qualifier" %in% names(d)) vapply(d$qualifier, \(v) paste(stats::na.omit(unlist(v)), collapse = ","), "") else ""
  d |>
    dplyr::transmute(dataset = "USGS daily values", provider = "USGS", station_id = .data$monitoring_location_id,
                     date = as.Date(.data$time), variable = usgs_variable(.data$parameter_code), value = as.numeric(.data$value),
                     unit = .data$unit_of_measure,
                     qualifier = trimws(paste(dplyr::coalesce(as.character(.data$approval_status), ""), q), whitespace = "[ ,]"),
                     method = paste0("daily statistic ", .data$statistic_id),
                     source_url = "https://api.waterdata.usgs.gov/ogcapi/v0/collections/daily", retrieved = Sys.Date()) |>
    dplyr::left_join(meta[intersect(c("station_id", "station_name", "lat", "lon"), names(meta))], by = "station_id") |>
    ak_obs_validate(strict = FALSE)
}

usgs_variable <- function(code) {
  known <- c(`00060` = "discharge", `00010` = "water_temperature", `00095` = "specific_conductance", `00300` = "dissolved_oxygen",
             `00400` = "ph", `63680` = "turbidity", `00065` = "gage_height", `00045` = "precipitation", `00020` = "air_temperature")
  dplyr::coalesce(unname(known[code]), paste0("p", code))
}
