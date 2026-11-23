#' Plot a delineation on satellite imagery
#'
#' Draws the catchment, the DEM-derived channels, the reference flowlines,
#' the original and snapped points, and optional reference polygons (a
#' hand-drawn or StreamStats catchment) over Esri World Imagery, so the
#' delineation can be checked by eye. If the imagery cannot be fetched, a
#' hillshade of the delineation DEM is used instead. The subtitle carries the
#' area, DEM, snap distance and flags.
#'
#' @param x An `ak_delineation` from [ak_delineate()].
#' @param reference Optional sf polygon(s) to overlay as dashed outlines. A
#'   `ref_source` column gives each source its own colour, line type and
#'   legend entry. These names always get the same style: `"Hand-drawn"`,
#'   `"StreamStats"`, `"USGS gaged basin"`, `"WBD HUC-12"`, `"TDX-Hydro
#'   catchments"`, `"MERIT delineator"`; other names get the next free one.
#' @param file PNG path to write, or `NULL` to only return the plot.
#' @param zoom Tile zoom level, or `NULL` to choose from the extent.
#' @param subtitle_extra Text appended to the subtitle (e.g. IoU values).
#' @return The ggplot object, invisibly; if `file` is given, the file path.
#' @export
plot_catchment <- function(x, reference = NULL, file = NULL, zoom = NULL, subtitle_extra = NULL) {
  c1 <- x$catchment
  geoms <- sf::st_geometry(c1)
  if (!is.null(reference) && nrow(reference)) {
    reference <- sf::st_transform(reference, akalbers_crs())
    geoms <- c(geoms, sf::st_geometry(reference))
  }
  bb <- sf::st_bbox(sf::st_buffer(sf::st_union(geoms), max(300, 0.1 * sqrt(c1$area_km2 * 1e6))))
  ext <- sf::st_as_sfc(bb)
  bg <- tryCatch(imagery_layer(ext, zoom), error = \(e) { message(c1$site_code, ": imagery unavailable (", conditionMessage(e), "); using a hillshade"); NULL })
  credit <- "Imagery: Esri, Maxar, Earthstar Geographics, and the GIS User Community."
  if (is.null(bg)) {
    bg <- tryCatch(hillshade_layer(x$files$dem, ext), error = \(e) { message(c1$site_code, ": no hillshade either (", conditionMessage(e), ")"); NULL })
    credit <- "Background: hillshade of the delineation DEM (imagery unavailable)."
  }
  ref_lines <- NULL
  if (!is.null(reference) && nrow(reference)) {
    src <- if ("ref_source" %in% names(reference)) reference$ref_source else rep("Reference catchment", nrow(reference))
    ref_lines <- sf::st_sf(layer = src, geometry = sf::st_cast(sf::st_boundary(sf::st_geometry(reference)), "MULTILINESTRING"))
  }
  lines <- dplyr::bind_rows(
    sf::st_sf(layer = "Delineated catchment", geometry = sf::st_cast(sf::st_boundary(sf::st_geometry(c1)), "MULTILINESTRING")),
    ref_lines,
    if (!is.null(x$streams) && nrow(x$streams)) sf::st_sf(layer = "DEM channels", geometry = sf::st_geometry(x$streams)),
    if (!is.null(x$flowlines) && nrow(x$flowlines)) sf::st_sf(layer = paste("Flowlines:", x$flowlines$flowline_source[1]),
                                                             geometry = sf::st_geometry(suppressWarnings(sf::st_crop(x$flowlines, bb)))))
  lines$layer <- factor(lines$layer, levels = unique(lines$layer))
  sty <- line_styles(levels(lines$layer))
  pts <- rbind(sf::st_sf(layer = "Sample point", geometry = sf::st_geometry(x$point)),
               sf::st_sf(layer = "Snapped outlet", geometry = sf::st_geometry(x$snapped)))
  sub <- sprintf("%.3f km2 | %s | snap %s %.0f m | %s", c1$area_km2, c1$dem_source, c1$snap_method, c1$snap_dist_m,
                 if (nzchar(c1$flags)) paste("flags:", c1$flags) else "no flags")
  if (!is.null(subtitle_extra)) sub <- paste0(sub, "\n", subtitle_extra)
  p <- ggplot2::ggplot()
  if (!is.null(bg)) p <- p + bg
  p <- p +
    ggplot2::geom_sf(data = lines, ggplot2::aes(colour = .data$layer, linetype = .data$layer, linewidth = .data$layer),
                     key_glyph = "path") +
    ggplot2::geom_sf(data = pts, ggplot2::aes(fill = .data$layer), shape = 21, size = 2.5, colour = "black") +
    ggplot2::scale_colour_manual(NULL, values = sty$colour) +
    ggplot2::scale_linetype_manual(NULL, values = sty$linetype) +
    ggplot2::scale_linewidth_manual(NULL, values = sty$linewidth) +
    ggplot2::scale_fill_manual(NULL, values = c("Sample point" = "red", "Snapped outlet" = "yellow")) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 3), fill = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::scale_x_continuous(n.breaks = 4) +
    ggplot2::coord_sf(crs = akalbers_crs(), xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], expand = FALSE) +
    ggplot2::labs(title = c1$site_code, subtitle = sub, caption = credit, x = "Longitude", y = "Latitude") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"), legend.position = "bottom", legend.box = "vertical",
                   legend.key.width = ggplot2::unit(1.2, "cm"), legend.key = ggplot2::element_rect(fill = "grey40", colour = NA),
                   plot.subtitle = ggplot2::element_text(size = 8), plot.caption = ggplot2::element_text(size = 7))
  if (is.null(file)) return(invisible(p))
  ggplot2::ggsave(file, p, width = 7.5, height = 8, dpi = 150)
  file
}

imagery_layer <- function(ext, zoom) {
  tiles <- maptiles::get_tiles(ext, provider = "Esri.WorldImagery", zoom = zoom %||% tile_zoom(ext), crop = TRUE,
                               cachedir = ak_cache_dir("tiles"), forceDownload = FALSE)
  rgb_annotation(tiles)
}

# Web-map zoom giving about 1200 imagery pixels across the plot (older maptiles versions need one)
tile_zoom <- function(ext, px = 1200) {
  bb <- sf::st_bbox(ext)
  lat <- sf::st_coordinates(sf::st_transform(sf::st_centroid(ext), 4326))[1, 2]
  w <- max(bb[["xmax"]] - bb[["xmin"]], bb[["ymax"]] - bb[["ymin"]])
  as.integer(min(17, max(3, round(log2(40075016 * cos(lat * pi / 180) * px / (256 * w))))))
}

hillshade_layer <- function(dem_file, ext) {
  dem <- terra::crop(terra::rast(dem_file), terra::vect(ext))
  hs <- terra::shade(terra::terrain(dem, "slope", unit = "radians"), terra::terrain(dem, "aspect", unit = "radians"))
  v <- terra::values(hs)
  v <- (v - min(v, na.rm = TRUE)) / diff(range(v, na.rm = TRUE))
  terra::values(hs) <- v * 255
  rgb_annotation(c(hs, hs, hs))
}

rgb_annotation <- function(r) {
  a <- terra::as.array(r[[1:3]])
  na <- is.na(a[, , 1])
  a[is.na(a)] <- 255
  m <- matrix(grDevices::rgb(a[, , 1], a[, , 2], a[, , 3], ifelse(na, 0, 255), maxColorValue = 255), nrow = dim(a)[1])
  e <- as.vector(terra::ext(r))
  ggplot2::annotation_raster(m, xmin = e[["xmin"]], xmax = e[["xmax"]], ymin = e[["ymin"]], ymax = e[["ymax"]])
}

line_styles <- function(layers) {
  base <- list(
    "Delineated catchment" = c("yellow", "solid", "0.9"),
    "Hand-drawn" = c("magenta", "dashed", "0.8"),
    "StreamStats" = c("white", "22", "0.8"),
    "Reference catchment" = c("magenta", "dashed", "0.8"),
    "USGS gaged basin" = c("#76FF03", "longdash", "0.8"),
    "WBD HUC-12" = c("#FFAB40", "twodash", "0.8"),
    "TDX-Hydro catchments" = c("#FF5252", "dotted", "0.8"),
    "MERIT delineator" = c("#8C9EFF", "dashed", "0.8"),
    "DEM channels" = c("#4FC3F7", "solid", "0.5"))
  # Other reference sources, in order: a colour and line type each, so several can be told apart
  other <- setdiff(layers[!startsWith(layers, "Flowlines")], names(base))
  extra <- list(c("orchid1", "dotdash"), c("#EEFF41", "longdash"), c("#18FFFF", "twodash"), c("#FF9E80", "dotted"), c("#B9F6CA", "dashed"))
  sty <- lapply(layers, \(l) base[[l]] %||% (if (startsWith(l, "Flowlines")) c("#FF7043", "solid", "0.5")
                                             else c(extra[[(match(l, other) - 1) %% length(extra) + 1]], "0.8")))
  names(sty) <- layers
  list(colour = vapply(sty, `[`, "", 1), linetype = vapply(sty, `[`, "", 2),
       linewidth = as.numeric(vapply(sty, `[`, "", 3)) |> stats::setNames(layers))
}
