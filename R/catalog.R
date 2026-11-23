#' Layers the package can add, with their sources and uncertainty
#'
#' One row per layer variant: the layer, the variant (product, which is the
#' `source` argument of its function), whether it is the default, its kind,
#' extent and access, the function(s) that get it, what they return, the
#' licence, the cache folder and the approximate download.
#'
#' `returns` says what the function gives back:
#' * `"columns"`: one value per site, appended to the catchments you pass in
#'   (with uncertainty columns `_se`, `_lo95`, `_hi95`, `_flag`, and a
#'   provenance row);
#' * `"table"`: a long table (by day, year, model or sample) with its own
#'   provenance; summarise it to one row per site and add it with
#'   [catchment_join()];
#' * `"catchments"`: catchment polygons ([ak_catchments()],
#'   [ak_streamstats()], [ak_gaged_basins()]);
#' * `"function"`: other (reference flowlines).
#'
#' With `details = TRUE` the table also has, for every row, the citation, the
#' URL, the native resolution or scale, the error the producer publishes, the
#' uncertainty columns the package adds and how it computes them, and the
#' assumptions behind them: the metadata table for a data release built with
#' the package. Each layer's vignette (`vignette(package = "AKCatchments")`)
#' explains these in full.
#'
#' @param layer Optional layer name(s) to keep.
#' @param details Add the citation, accuracy and uncertainty columns?
#' @return A data frame.
#' @examples
#' ak_layers()
#' ak_layers("permafrost", details = TRUE)
#' @export
ak_layers <- function(layer = NULL, details = FALSE) {
  s <- sources_table()
  out <- data.frame(layer = s$id, variant = s$variant, default = as.logical(s$default), kind = s$kind,
                    returns = s$returns, fn = s$fn, description = s$product, resolution = s$resolution_or_scale,
                    extent = s$extent, access = s$access, licence = s$license, redistributable = s$redistributable,
                    cache_folder = s$cache_folder, approx_download = s$approx_download, stringsAsFactors = FALSE)
  if (details) {
    out <- cbind(out, data.frame(citation = s$citation, url = s$url, published_error = s$published_error,
                                 uncertainty_columns = s$uncertainty_columns, uncertainty_method = s$uncertainty_method,
                                 assumptions = s$assumptions, stringsAsFactors = FALSE))
  }
  if (!is.null(layer)) {
    bad <- setdiff(layer, out$layer)
    if (length(bad)) stop("Unknown layer: ", paste(bad, collapse = ", "), ". Layers: ", paste(unique(out$layer), collapse = ", "))
    out <- out[out$layer %in% layer, ]
  }
  rownames(out) <- NULL
  out
}

sources_table <- function() {
  utils::read.csv(system.file("extdata", "sources.csv", package = "AKCatchments"), stringsAsFactors = FALSE)
}

# The catalog row for a layer and variant (the default variant if NULL)
layer_row <- function(layer, variant = NULL, fn = NULL) {
  cat <- ak_layers()
  if (!layer %in% cat$layer) stop("Unknown layer \"", layer, "\". See ak_layers(); layers: ", paste(unique(cat$layer), collapse = ", "))
  rows <- cat[cat$layer == layer, ]
  row <- if (is.null(variant)) rows[rows$default, ][1, ] else rows[rows$variant == variant, ][1, ]
  if (is.na(row$layer)) stop("Layer \"", layer, "\" has no variant \"", variant, "\"; variants: ", paste(rows$variant, collapse = ", "))
  if (!is.null(fn)) row$fn <- fn
  row
}

#' Provenance of the attributes on catchments
#'
#' One row per layer on the catchments or in a table (added by
#' [ak_catchments()], a layer function such as [ak_alder()], or
#' [catchment_join()]): layer, variant, function, product,
#' citation, URL, licence and whether derived values may be redistributed,
#' the cache folder, when its files were downloaded (from the cache
#' manifest; `NA` for files that predate it or were not downloaded), when the
#' layer was added, the package version, the arguments and the columns
#' added. Cite from it, and include it with any data you share.
#'
#' @param x Catchments from [ak_catchments()] with layers added, or a table
#'   returned by a layer function (e.g. [ak_weather()]).
#' @return A data frame (empty if `x` carries no provenance).
#' @export
ak_provenance <- function(x) {
  attr(x, "ak_provenance") %||% provenance_row(NULL)
}

provenance_row <- function(row, columns = character(), args = "", note = "") {
  if (is.null(row)) {
    return(dplyr::tibble(layer = character(), variant = character(), fn = character(), product = character(),
                         citation = character(), url = character(), licence = character(), redistributable = character(),
                         cache_folder = character(), downloaded_utc = character(), added_utc = character(),
                         package_version = character(), args = character(), note = character(), columns = character()))
  }
  src <- sources_table()
  s <- src[src$id == row$layer & src$variant == row$variant, ][1, ]
  dplyr::tibble(layer = row$layer, variant = row$variant, fn = row$fn, product = s$product, citation = s$citation, url = s$url,
                licence = s$license, redistributable = s$redistributable, cache_folder = s$cache_folder,
                downloaded_utc = cache_dates(s$cache_folder), added_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                package_version = tryCatch(as.character(utils::packageVersion("AKCatchments")), error = \(e) NA_character_),
                args = args, note = note, columns = paste(columns, collapse = ", "))
}

# Download dates recorded in the cache manifest for a layer's folders ("first - last")
cache_dates <- function(folders) {
  man <- file.path(ak_cache_dir(), "cache_manifest.csv")
  if (!length(folders) || is.na(folders) || !nzchar(folders) || !file.exists(man)) return(NA_character_)
  m <- tryCatch(utils::read.csv(man), error = \(e) NULL)
  if (is.null(m) || !nrow(m)) return(NA_character_)
  f <- trimws(strsplit(folders, ";")[[1]])
  hit <- m$downloaded_utc[Reduce(`|`, lapply(f, \(d) startsWith(m$file, d)))]
  if (!length(hit)) return(NA_character_)
  r <- range(substr(hit, 1, 10))
  if (r[1] == r[2]) r[1] else paste(r, collapse = " - ")
}

deparse_args <- function(a) {
  if (!length(a)) return("")
  vals <- vapply(a, \(v) {
    if (inherits(v, c("sf", "sfc", "data.frame", "SpatRaster"))) return(paste0("<", class(v)[1], ">"))
    paste(deparse(v, width.cutoff = 200L, nlines = 1L), collapse = "")
  }, "")
  paste(names(a), vals, sep = " = ", collapse = ", ")
}

#' Download a set of layers before extracting them
#'
#' Fetches, once, everything the given layers need for the catchments: the
#' statewide files (geology, land cover, alder, Pastick permafrost, PRISM),
#' the fire perimeters for the catchments' area, Daymet pixels and MODIS
#' years (with `years`), and global flowline files. Run it before a long
#' extraction, before working offline, or to fill a shared cache. Layers
#' that need a manual download are checked and explained. A failure is
#' recorded and the next layer is tried.
#'
#' @param x Catchments (sf polygons with `site_code`).
#' @param layers Layer names (see [ak_layers()]), each optionally with a
#'   variant as `"layer/variant"` (e.g. `"permafrost/pastick2015"`); default:
#'   the default variant of every static layer.
#' @param years Years for `"weather_daily"` and `"snowoff"`.
#' @param flowlines Global flowline sources to download as well (`"tdx"`,
#'   `"merit"`, `"hydrorivers"`), or `NULL`.
#' @return A data frame: `layer`, `variant`, `status` (`ok`, `manual`,
#'   `failed`, `not needed`), `detail`, `seconds`.
#' @export
ak_prefetch <- function(x, layers = NULL, years = NULL, flowlines = NULL) {
  cat <- ak_layers()
  if (is.null(layers)) layers <- unique(cat$layer[grepl("columns", cat$returns) & cat$default])
  rows <- purrr::map(layers, \(l) {
    lv <- strsplit(l, "/", fixed = TRUE)[[1]]
    layer_row(lv[1], if (length(lv) > 1) lv[2])
  }) |>
    dplyr::bind_rows()
  if (!is.null(flowlines)) rows <- dplyr::bind_rows(rows, data.frame(layer = "flowlines", variant = flowlines))
  purrr::pmap(rows[c("layer", "variant")], \(layer, variant) {
    t0 <- Sys.time()
    out <- tryCatch(prefetch_one(layer, variant, x, years), error = \(e) c("failed", conditionMessage(e)))
    dplyr::tibble(layer = layer, variant = variant, status = out[1], detail = out[2],
                  seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }) |>
    purrr::list_rbind()
}

prefetch_one <- function(layer, variant, x, years) {
  manual <- \(folder, what) {
    have <- length(list.files(ak_cache_dir(folder), "\\.shp$", recursive = TRUE)) > 0
    if (have) c("ok", paste("found in", ak_cache_dir(folder))) else c("manual", paste0(what, "; put it in ", ak_cache_dir(folder)))
  }
  switch(paste(layer, variant, sep = "/"),
    "alder/accs2021" = { alder_raster(); c("ok", ak_cache_dir(cache_paths$alder)) },
    "alder/nps_wrst" = { wrst_grid(); c("ok", ak_cache_dir(cache_paths$nps_wrst)) },
    "landcover/nlcd2016" = { nlcd_raster(); c("ok", ak_cache_dir(cache_paths$nlcd)) },
    "geology/sim3340" = c("ok", geology_gdb()[1]),
    "rock_chemistry/olson2014" = {
      lt <- rock_chem_lithologies(c("N", "CaO"))
      c("ok", paste(dplyr::n_distinct(lt$lithology), "lithology classes in", ak_cache_dir(cache_paths$olson), "; geology:", geology_gdb()[1]))
    },
    "permafrost/pastick2015" = { pastick_raster(); c("ok", ak_cache_dir(cache_paths$pastick)) },
    "climate_normals/prism8110" = {
      g <- tidyr::expand_grid(var = c("tmean", "tmin", "tmax", "ppt"), period = c(sprintf("%02d", 1:12), "annual"))
      purrr::pwalk(g, \(var, period) prism_raster(var, period))
      c("ok", paste(nrow(g), "grids in", ak_cache_dir(cache_paths$prism)))
    },
    "fire/afs_perimeters" = c("ok", paste(nrow(fire_perimeters(as_catchments(x))), "perimeters")),
    "permafrost/jorgenson2008" = , "ground_ice/jorgenson2008" =
      manual(cache_paths$jorgenson, "download the shapefile from https://catalog.northslopescience.org/dataset/1725"),
    "glaciers/rgi7" = manual(cache_paths$glaciers, "download RGI 7.0 region 01 (NSIDC-0770, Earthdata login)"),
    "soils/statsgo2" = c("not needed", "queried from Soil Data Access by ak_soils()"),
    "topography/delineation_dem" = , "outlet_slope/delineation_dem" = c("not needed", "uses the delineation DEM"),
    "weather_daily/daymet4" = {
      if (is.null(years)) return(c("failed", "give `years`"))
      d <- ak_weather(x, years)
      c("ok", paste(length(attr(d, "pixel_files")), "Daymet pixels"))
    },
    "snowoff/modis_daymet" = {
      if (is.null(years)) return(c("failed", "give `years`"))
      m <- snowoff_modis(x, years = years)
      c("ok", paste(dplyr::n_distinct(m$year), "MODIS years"))
    },
    "climate_projection/snap_cmip6_4km" = {
      d <- ak_climate_projection(x)
      c(if (NROW(attr(d, "failed"))) "partial" else "ok", paste(dplyr::n_distinct(d$model), "models cached in", ak_cache_dir(cache_paths$snap)))
    },
    "greenness/landsat_c2" = {
      g <- ak_peak_ndvi(x, years = years %||% 1984:(as.integer(format(Sys.Date(), "%Y")) - 1))
      c(if (NROW(attr(g, "failed"))) "partial" else "ok", paste(nrow(g), "site-years; readings in", ak_cache_dir(cache_paths$landsat)))
    },
    "flowlines/tdx" = , "flowlines/merit" = , "flowlines/hydrorivers" = {
      p <- ak_download_flowlines(x, variant)
      c(if (all(p$exists)) "ok" else "failed", paste(basename(p$file), collapse = ", "))
    },
    c("not needed", "nothing to download in advance")
  )
}

#' Write catchments, their provenance and tables to one GeoPackage
#'
#' Writes the polygons with all their columns (layer `catchments`), the
#' provenance of everything on them and in the tables (`provenance`), the
#' per-site errors (`errors`), and any long tables you pass (daily weather,
#' snow-off, projections, ...), each as its own layer: the format to hand
#' on or archive. The provenance layer is what makes the file citable: it
#' says which product, version, download date and arguments every column
#' came from.
#'
#' @param x Catchments from [ak_catchments()] with layers added.
#' @param file Path ending in `.gpkg`; replaced if it exists.
#' @param ... Named tables to write as well, e.g. `weather = w, snowoff = s`.
#'   Their provenance is added to the provenance layer.
#' @return `file`, invisibly.
#' @examples
#' \dontrun{
#' ak_write(x, "cakn_catchments.gpkg", weather = w, samples = ws)
#' }
#' @export
ak_write <- function(x, file, ...) {
  if (!grepl("\\.gpkg$", file)) stop("`file` should end in .gpkg")
  tables <- list(...)
  if (length(tables) && (is.null(names(tables)) || any(!nzchar(names(tables))))) stop("Name the tables: ak_write(x, file, weather = w)")
  bad <- intersect(names(tables), c("catchments", "provenance", "errors"))
  if (length(bad)) stop("Table names reserved for the catchments: ", paste(bad, collapse = ", "))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  unlink(file)
  d <- sf::st_drop_geometry(x)
  class(d) <- setdiff(class(d), "ak_catchments")
  sf::st_write(sf::st_sf(d, geometry = sf::st_geometry(x)), file, layer = "catchments", quiet = TRUE)
  prov <- dplyr::bind_rows(dplyr::mutate(ak_provenance(x), table = "catchments"),
                           purrr::imap(tables, \(t, nm) dplyr::mutate(ak_provenance(t), table = nm)) |> purrr::list_rbind())
  errs <- dplyr::bind_rows(attr(x, "errors"), purrr::map(tables, \(t) attr(t, "errors")) |> purrr::list_rbind())
  out <- c(list(provenance = prov, errors = errs), tables)
  for (nm in names(out)) {
    t <- out[[nm]]
    if (is.null(t) || !nrow(t)) next
    t <- as.data.frame(sf::st_drop_geometry(t))
    t[] <- lapply(t, \(v) if (inherits(v, c("Date", "POSIXt"))) as.character(v) else if (is.list(v)) vapply(v, paste, "", collapse = ";") else v)
    sf::st_write(t, file, layer = nm, quiet = TRUE)
  }
  invisible(file)
}
