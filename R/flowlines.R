#' Reference flowlines for an area
#'
#' Fetches mapped stream lines to check a DEM delineation against.
#'
#' * `"3dhp"`: USGS 3D Hydrography Program (3DHP_all flowlines). In Alaska it
#'   carries elevation-derived hydrography from the IfSAR DEM where that has
#'   been produced, and the older NHD elsewhere. Queried from the web service.
#' * `"nhd"`: the legacy National Hydrography Dataset flowlines
#'   (high resolution; layer 6 is the one that covers Alaska). Web service.
#' * `"tdx"`: TDX-Hydro stream network (NGA; 12 m TanDEM-X).
#' * `"geoglows"`: the same TDX-Hydro streams as distributed by GEOGLOWS v2
#'   on AWS (open, no registration; Alaska is in VPUs 801-803). Reaches carry
#'   their upstream area at both ends (`USContArea`, `DSContArea`, m2), so
#'   they can guide snapping (`ak_delineate(snap_reference = "geoglows")`).
#' * `"merit"`: MERIT-Basins flowlines (90 m MERIT Hydro; channels start at
#'   25 km2, so small headwater catchments have no line).
#' * `"hydrorivers"`: HydroRIVERS v1.0 (15 arc-second HydroSHEDS; rivers with
#'   at least 10 km2 upstream; built from lower-quality elevation data north of
#'   60 degrees N, so only a gross check).
#' * `"file"`: any line file you have, read with a spatial filter.
#'
#' The `"tdx"`, `"merit"` and `"hydrorivers"` files are downloaded once by
#' [ak_download_flowlines()] and read from the cache after that.
#'
#' @param aoi An sf, sfc or bbox object.
#' @param source One of `"3dhp"`, `"nhd"`, `"tdx"`, `"geoglows"`, `"merit"`, `"hydrorivers"`, `"file"`.
#' @param path File path for `source = "file"`.
#' @param dir Download folder for the global datasets, passed to [ak_download_flowlines()].
#' @return An sf of lines in EPSG:3338 with a `flowline_source` column.
#' @seealso `vignette("delineation")` (download, use, uncertainty, validation)
#' @export
ak_flowlines <- function(aoi, source = c("3dhp", "nhd", "tdx", "geoglows", "merit", "hydrorivers", "file"), path = NULL,
                         dir = ak_cache_dir("flowlines")) {
  source <- match.arg(source)
  aoi <- as_aoi(aoi)
  x <- switch(source,
              "3dhp" = ak_arcgis_query("https://hydro.nationalmap.gov/arcgis/rest/services/3DHP_all/FeatureServer/50", aoi),
              nhd = ak_arcgis_query("https://hydro.nationalmap.gov/arcgis/rest/services/nhd/MapServer/6", aoi),
              file = flowlines_file(aoi, path),
              ak_download_flowlines(aoi, source, dir)$file |>
                purrr::map(\(f) flowlines_file(aoi, f)) |>
                purrr::map(\(d) sf::st_transform(d, akalbers_crs())) |>
                dplyr::bind_rows())
  if (!nrow(x)) return(sf::st_sf(flowline_source = character(), geometry = sf::st_sfc(crs = akalbers_crs())))
  sf::st_sf(flowline_source = source, sf::st_drop_geometry(x), geometry = sf::st_geometry(sf::st_transform(x, akalbers_crs())))
}

#' Download global flowline datasets for an area
#'
#' Downloads the files of a global river-network dataset that cover `aoi`,
#' once, into `dir`; later calls reuse them. Files are chosen with the
#' HydroBASINS level-2 units (North America and Arctic regions), which index
#' all three datasets; that index is downloaded once too.
#'
#' * `"tdx"`: TDX-Hydro stream network (US National Geospatial-Intelligence
#'   Agency, 2023), from 12 m TanDEM-X elevation, one GeoPackage per
#'   HydroBASINS level-2 unit (`TDX_streamnet_<HYBAS_ID>_01.gpkg`) from
#'   earth-info.nga.mil. Licence CC BY-SA 4.0. Files are large (hundreds of
#'   MB to several GB).
#' * `"geoglows"`: TDX-Hydro streams as republished by GEOGLOWS v2
#'   (doi:10.1111/jfr3.12859) in the public `geoglows-v2` AWS bucket, one GeoPackage per
#'   vector processing unit (`streams_<VPU>.gpkg`); Alaska is VPUs 801-803,
#'   which are all downloaded. Use this when the NGA site is unreachable. The
#'   GEOGLOWS network is a lightly simplified subset of TDX-Hydro (some
#'   headwater lines merged). Licence CC BY 4.0.
#' * `"merit"`: MERIT-Basins flowlines (Lin et al. 2019, from MERIT Hydro,
#'   90 m), one SpatiaLite file per Pfafstetter level-2 basin
#'   (`rivers<PFAF_ID>.db`), from the mghydro.com mirror (the official
#'   distribution is Google Drive / Globus at reachhydro.org, which cannot
#'   be scripted). Channels start at 25 km2. Licence CC BY-NC-SA 4.0
#'   (non-commercial).
#' * `"hydrorivers"`: HydroRIVERS v1.0 (Lehner and Grill 2013), the North
#'   America and Arctic regional files. HydroSHEDS licence.
#'
#' Alaska spans HydroBASINS regions 7 (North America) and 8 (Arctic); an area
#' near a unit boundary gets the files of every unit it touches (within 10 km).
#'
#' @param aoi An sf, sfc or bbox object.
#' @param source `"tdx"`, `"geoglows"`, `"merit"` or `"hydrorivers"`.
#' @param dir Download folder.
#' @param download If `FALSE`, return the plan (units, URLs, destination
#'   files) without downloading the data files.
#' @return A data frame: `source`, `unit` (HYBAS_ID, PFAF_ID or region),
#'   `url`, `file`, `exists`.
#' @examples
#' \dontrun{
#' bb <- sf::st_bbox(c(xmin = -150, ymin = 63, xmax = -148, ymax = 64), crs = 4326)
#' ak_download_flowlines(bb, "tdx", download = FALSE)  # which files, from where
#' ak_download_flowlines(bb, "tdx")
#' }
#' @export
ak_download_flowlines <- function(aoi, source = c("tdx", "geoglows", "merit", "hydrorivers"), dir = ak_cache_dir("flowlines"),
                                  download = TRUE) {
  source <- match.arg(source)
  plan <- if (source == "geoglows") geoglows_plan(dir) else flowline_plan(flowline_units(aoi, hydrobasins_lev02(dir)), source, dir)
  if (!download) return(plan)
  todo <- plan[!plan$exists, ]
  if (nrow(todo)) message("Downloading ", nrow(todo), " ", source, " file(s) to ", dir, "; large files take a while")
  purrr::walk2(todo$url, todo$file, \(u, f) {
    ak_download(u, f)
    unzip_if_zip(f)
  })
  plan$exists <- file.exists(plan$file)
  plan
}

# HydroBASINS level 2 for the North America (7) and Arctic (8) regions: HYBAS_ID and PFAF_ID
hydrobasins_lev02 <- function(dir) {
  purrr::map(c("na", "ar"), \(r) {
    zip <- ak_download(sprintf("https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_%s_lev02_v1c.zip", r),
                       file.path(dir, "hydrobasins", sprintf("hybas_%s_lev02_v1c.zip", r)))
    sf::st_read(vsizip(zip, sprintf("hybas_%s_lev02_v1c.shp", r)), quiet = TRUE)[c("HYBAS_ID", "PFAF_ID")]
  }) |>
    dplyr::bind_rows()
}

flowline_units <- function(aoi, index, buffer_m = 10000) {
  a <- sf::st_transform(sf::st_buffer(as_aoi(aoi), buffer_m), sf::st_crs(index))
  hit <- index[lengths(sf::st_intersects(sf::st_make_valid(index), a)) > 0, ]
  if (!nrow(hit)) stop("No HydroBASINS level-2 unit (North America or Arctic) covers this area")
  sf::st_drop_geometry(hit)
}

flowline_plan <- function(units, source, dir) {
  out <- switch(source,
    tdx = data.frame(unit = as.character(units$HYBAS_ID),
                     url = sprintf("https://earth-info.nga.mil/php/download.php?file=%s-streamnet-gpkg", units$HYBAS_ID),
                     file = file.path(dir, "tdx", sprintf("TDX_streamnet_%s_01.gpkg", units$HYBAS_ID))),
    merit = data.frame(unit = as.character(units$PFAF_ID),
                       url = sprintf("https://mghydro.com/watersheds/data/rivers%s.db", units$PFAF_ID),
                       file = file.path(dir, "merit", sprintf("rivers%s.db", units$PFAF_ID))),
    hydrorivers = {
      reg <- unique(c("7" = "na", "8" = "ar")[substr(as.character(units$HYBAS_ID), 1, 1)])
      data.frame(unit = reg,
                 url = sprintf("https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_%s.gdb.zip", reg),
                 file = file.path(dir, "hydrorivers", sprintf("HydroRIVERS_v10_%s.gdb.zip", reg)))
    })
  out <- unique(out)
  data.frame(source = source, out, exists = file.exists(out$file))
}

# GEOGLOWS v2 streams for the Alaska VPUs (801-803). The object key is found by listing the bucket (anonymous S3
# ListObjectsV2), so a renamed file is still found; files already downloaded need no listing.
geoglows_plan <- function(dir) {
  vpus <- c("801", "802", "803")
  file <- file.path(dir, "geoglows", sprintf("streams_%s.gpkg", vpus))
  key <- purrr::map2_chr(vpus, file, \(v, f) {
    guess <- sprintf("hydrography/vpu=%s/streams_%s.gpkg", v, v)
    if (file.exists(f)) return(guess)
    xml <- tryCatch(ak_request(sprintf("https://geoglows-v2.s3.us-west-2.amazonaws.com/?list-type=2&prefix=hydrography/vpu%%3D%s/", v)) |>
                      ak_perform() |> httr2::resp_body_string(), error = \(e) "")
    keys <- regmatches(xml, gregexpr("(?<=<Key>)[^<]+(?=</Key>)", xml, perl = TRUE))[[1]]
    c(grep("streams[^/]*\\.gpkg$", keys, value = TRUE), guess)[1]
  })
  data.frame(source = "geoglows", unit = vpus, url = paste0("https://geoglows-v2.s3.us-west-2.amazonaws.com/", key),
             file = file, exists = file.exists(file))
}

# NGA has served both bare and zipped GeoPackages; unpack in place if needed
unzip_if_zip <- function(f) {
  if (grepl("\\.zip$", f) || !file.exists(f)) return(invisible(f))
  if (!identical(readBin(f, "raw", 2), charToRaw("PK"))) return(invisible(f))
  tmp <- tempfile()
  utils::unzip(f, exdir = tmp)
  inner <- list.files(tmp, pattern = "\\.(gpkg|db|sqlite)$", recursive = TRUE, full.names = TRUE)
  if (length(inner) != 1) stop("Unexpected contents in ", f, ": ", paste(basename(inner), collapse = ", "))
  file.copy(inner, f, overwrite = TRUE)
  invisible(f)
}

#' Flowline and catchment datasets that can serve as references
#'
#' A table of reference hydrography for Alaska and how to get each with this
#' package: queried from a web service by [ak_flowlines()], or downloaded once
#' by [ak_download_flowlines()] and then read by [ak_flowlines()].
#'
#' @return A data frame.
#' @export
ak_flowline_sources <- function() {
  data.frame(
    dataset = c("USGS 3DHP", "USGS NHD (high resolution)", "HydroRIVERS v1.0", "TDX-Hydro (NGA)", "TDX-Hydro (GEOGLOWS v2)", "MERIT-Basins",
                "USGS gaged basins, Alaska through 2019", "USGS StreamStats, Cook Inlet Basin"),
    scope = c("US incl. Alaska", "US incl. Alaska", "global", "global", "global", "global", "Alaska, 253 USGS gages", "Cook Inlet Basin"),
    basis = c("IfSAR-derived hydrography where produced, NHD elsewhere", "1:63,360 maps in Alaska",
              "15 arc-second HydroSHEDS (coarse above 60 N), >= 10 km2", "12 m TanDEM-X, >= 5 km2",
              "12 m TanDEM-X, >= 5 km2; upstream area at both reach ends; AWS, no registration", "90 m MERIT Hydro, >= 25 km2",
              "hand-checked delineations", "on-demand DEM delineation"),
    access = c("ak_flowlines(source = '3dhp')", "ak_flowlines(source = 'nhd')",
               "ak_download_flowlines(source = 'hydrorivers'); ak_flowlines(source = 'hydrorivers')",
               "ak_download_flowlines(source = 'tdx'); ak_flowlines(source = 'tdx')",
               "ak_download_flowlines(source = 'geoglows'); ak_flowlines(source = 'geoglows')",
               "ak_download_flowlines(source = 'merit'); ak_flowlines(source = 'merit')",
               "ScienceBase item 66440948d34e1955f5a427ff; compare with compare_catchments()",
               "ak_streamstats()"),
    stringsAsFactors = FALSE)
}

#' Query an ArcGIS REST feature layer by bounding box
#'
#' Pages through results (`resultOffset`) until the server reports no more.
#'
#' @param layer_url Layer URL ending in the layer id.
#' @param aoi Area of interest (any sf/sfc/bbox).
#' @param where SQL filter.
#' @param fields Output fields.
#' @param page Records per request.
#' @return sf in EPSG:3338.
#' @export
ak_arcgis_query <- function(layer_url, aoi, where = "1=1", fields = "*", page = 1000) {
  bb <- sf::st_bbox(sf::st_transform(as_aoi(aoi), 4326))
  out <- list()
  offset <- 0
  repeat {
    txt <- ak_request(paste0(layer_url, "/query"), list(
      where = where, geometry = paste(bb, collapse = ","), geometryType = "esriGeometryEnvelope", inSR = 4326,
      spatialRel = "esriSpatialRelIntersects", outFields = fields, outSR = 4326, f = "geojson",
      resultOffset = offset, resultRecordCount = page)) |>
      ak_perform() |>
      httr2::resp_body_string()
    j <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    if (!is.null(j$error)) stop("ArcGIS query to ", layer_url, " failed: ", j$error$message, " ", paste(unlist(j$error$details), collapse = " "))
    n <- length(j$features)
    if (n) out[[length(out) + 1]] <- sf::read_sf(txt, quiet = TRUE)
    more <- isTRUE(j$exceededTransferLimit) || isTRUE(j$properties$exceededTransferLimit)
    if (!n || !more) break
    offset <- offset + n
  }
  if (!length(out)) return(sf::st_sf(geometry = sf::st_sfc(crs = akalbers_crs())))
  dplyr::bind_rows(out) |> sf::st_transform(akalbers_crs())
}

flowlines_file <- function(aoi, path) {
  if (is.null(path)) stop("`path` is needed for source = \"file\"")
  if (grepl("\\.zip$", path)) path <- vsizip(path)
  lay <- sf::st_layers(path)
  i <- which(grepl("line", vapply(lay$geomtype, \(g) paste(g, collapse = " "), ""), ignore.case = TRUE))[1]
  if (is.na(i)) i <- 1
  sf::st_read(path, layer = lay$name[i], wkt_filter = sf::st_as_text(sf::st_transform(aoi, lay$crs[[i]])), quiet = TRUE)
}
