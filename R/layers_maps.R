#' Alder (Alnus) cover of each catchment, with map error
#'
#' Catchment mean alder cover from one of two maps, appended to the
#' catchments with its standard error, 95% interval and flags:
#' * `source = "accs2021"` (default): ACCS continuous foliar cover for
#'   *Alnus*, 10 m, arctic and boreal Alaska (columns `alnus_cover*`);
#' * `source = "nps_wrst"`: alder cover of the NPS Wrangell-St. Elias land
#'   cover map, 28.5 m, the park only (columns `alnus_wrst_*`).
#'
#' The two products have different column names, so both can sit on the
#' same catchments, e.g. to calibrate one against the other where they
#' overlap. Each is downloaded once to the cache.
#'
#' @section ACCS foliar cover (`accs2021`):
#' Alder (Alnus) foliar cover with map error
#'
#' Catchment mean of the ACCS continuous foliar cover map for *Alnus* (10 m;
#' Nawrocki et al. 2021, Zenodo record 4770218, CC BY 4.0), downloaded once.
#'
#' Uncertainty: the map's validation reports RMSE of 10.5 % cover at site
#' scale, 4.9 at 10 x 10 km and 1.4 at ecoregion scale. That error barely
#' shrinks with area, so it is spatially correlated; `alnus_cover_se` is the
#' RMSE interpolated on log(area) between those anchors.
#' The 95% interval is the 2.5-97.5% range of a Beta distribution with the
#' map value as mean and the SE as SD, so it stays within 0-100% and is
#' skewed near 0. The validation used non-spatial cross-validation, so these
#' may be optimistic. See `vignette("alder-accs")`.
#'
#' The map covers arctic and boreal Alaska only; its southern edge runs at
#' about 61.0-61.8 N between Cook Inlet and the Canadian border, so much of
#' southern Wrangell-St. Elias is outside it.
#'
#' `alnus_cover_flag`: `outside_map` where the catchment has no map data
#' (`alnus_cover` and its uncertainty are `NA`); `partial_coverage` where
#' 0-95% of it has map data (the mean then covers only the mapped part);
#' `error_exceeds_cover` where the SE is too large for a Beta with that mean
#' (very low cover with a large RMSE), so the interval is the normal one cut
#' at 0 and 100.
#'
#'
#' @section NPS Wrangell-St. Elias map (`nps_wrst`):
#' Alder cover from the NPS Wrangell-St. Elias land cover map.
#'
#' An alternate to the ACCS map for Wrangell-St. Elias, where much of the
#' south is outside the ACCS map. The WRST Land Cover Mapping Project
#' (Geographic Resource Solutions and ABR, 2004-2007; NPS DataStore 2171667)
#' classified Landsat TM into 10,062 spectral signatures at 28.5 m and gave
#' each signature cover attributes from field and aerial-photo plots; the
#' class names were calculated from those attributes. This function reads the
#' alder cover of each signature and averages it over each catchment.
#'
#' Alder per signature: `ALNUS_COVER` where the plot source recorded cover
#' by species (`SOURCE_COVER` DU, FirePro, ISO_CL); otherwise
#' `PR_COMP_COVER` where alder is the primary component; otherwise alder was
#' not recorded (aerial-photo plots, GRSAIR, recorded life forms only).
#' `unrecorded = "class_mean"` gives those signatures the area-weighted mean
#' alder of the recorded signatures of the same class; `"zero"` sets them to
#' 0, which underestimates alder that is present but not dominant. Grid value
#' 0 (outside the park) and undefined sources are unmapped.
#'
#' Uncertainty: each signature's alder value is given the SD of alder among
#' the recorded signatures of its class (its life form, `MAJOR_CLASS`, when
#' the class has fewer than 5; all recorded signatures when that has fewer
#' than 5). For recorded signatures this is conservative (it includes real
#' differences between signatures); for class-mean values it is the
#' prediction error. Errors are independent between signatures and shared by
#' all pixels of a signature, so `alnus_wrst_cover_se` is
#' sqrt(sum_v (a_v / A)^2 sd_v^2) over the signatures v in the catchment
#' (a_v their area, A the mapped area): exact, no simulation needed. The 95%
#' interval is from a Beta distribution as for the ACCS map. Not included:
#' misclassification (no accuracy assessment was found), errors shared by a
#' whole class or plot source, and change since 2004-2007. Calibrating
#' against ACCS where both maps exist absorbs biases common to the region.
#'
#' `alnus_wrst_cover_flag`: `outside_map` (no mapped area: value `NA`),
#' `partial_coverage` (under 95% mapped), `error_exceeds_cover` (no Beta
#' fits; normal interval cut at 0-100). `alnus_wrst_unrecorded_pct` is the
#' share of the mapped area whose alder comes from class means (or is 0 with
#' `unrecorded = "zero"`); it is usually over half, so it is a column, not a
#' flag.
#'
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param source The alder map: `"accs2021"` or `"nps_wrst"`.
#' @param unrecorded For `nps_wrst`: alder for signatures whose plots did
#'   not record it, `"class_mean"` or `"zero"`.
#' @return `catchments` with columns added. `accs2021`: `alnus_cover`,
#'   `alnus_coverage_pct`, `alnus_cover_se`, `alnus_cover_lo95`,
#'   `alnus_cover_hi95`, `alnus_cover_flag`. `nps_wrst`: `alnus_wrst_cover`,
#'   `alnus_wrst_coverage_pct`, `alnus_wrst_cover_se`, `alnus_wrst_cover_lo95`,
#'   `alnus_wrst_cover_hi95`, `alnus_wrst_unrecorded_pct`,
#'   `alnus_wrst_cover_flag`.
#' @examples
#' \dontrun{
#' x <- ak_alder(x)                       # ACCS
#' x <- ak_alder(x, source = "nps_wrst")  # NPS WRST, alongside
#' }
#' @seealso `vignette("alder-accs")`, `vignette("alder-wrst")`
#' @export
ak_alder <- function(catchments, source = c("accs2021", "nps_wrst"), unrecorded = c("class_mean", "zero")) {
  source <- match.arg(source)
  x <- as_catchments(catchments)
  new <- switch(source, accs2021 = alder_accs(x), nps_wrst = alder_wrst(x, match.arg(unrecorded)))
  add_columns(catchments, new, layer_row("alder", source, fn = "ak_alder"),
              c(list(source = source), if (source == "nps_wrst") list(unrecorded = match.arg(unrecorded))))
}

alder_accs <- function(x) {
  out <- raster_mean(x, alder_raster(), "alnus_cover") |>
    dplyr::rename(alnus_coverage_pct = "alnus_cover_coverage_pct")
  se <- ifelse(is.na(out$alnus_cover), NA_real_, alder_se(x$area_m2 / 1e6))
  ci <- beta_interval(out$alnus_cover, se)
  dplyr::mutate(out, alnus_cover_se = se, alnus_cover_lo95 = ci$lo, alnus_cover_hi95 = ci$hi,
                alnus_cover_flag = flag_text(outside_map = .data$alnus_coverage_pct == 0,
                                             partial_coverage = dplyr::between(.data$alnus_coverage_pct, 1e-9, 95 - 1e-9),
                                             error_exceeds_cover = !ci$feasible))
}

# The ACCS Alnus map, downloaded and unpacked once
alder_raster <- function() {
  dir <- ak_cache_dir(cache_paths$alder)
  if (!length(list.files(dir, "\\.tif$", recursive = TRUE))) {
    extract_archive(ak_download("https://zenodo.org/records/4770218/files/alnus.zip?download=1", file.path(ak_cache_dir(), "alnus.zip")), dir)
  }
  terra::vrt(list.files(dir, "\\.tif$", recursive = TRUE, full.names = TRUE), filename = tempfile(fileext = ".vrt"))
}

alder_se <- function(area_km2) {
  stats::approx(log10(c(1e-4, 100, 25000)), c(10.5, 4.9, 1.4), xout = log10(area_km2), rule = 2)$y
}

alder_wrst <- function(x, unrecorded = "class_mean") {
  r <- wrst_grid()
  sig <- wrst_alder_signatures(r, unrecorded)
  levels(r) <- NULL
  a <- exactextractr::exact_extract(r, sf::st_transform(x, terra::crs(r)), include_area = TRUE, progress = extract_progress(x)) |>
    purrr::map2(x$site_code, \(d, s) dplyr::tibble(site_code = s, value = d$value, a = d$area * d$coverage_fraction)) |>
    dplyr::bind_rows() |>
    dplyr::summarise(a = sum(.data$a), .by = c("site_code", "value")) |>
    dplyr::inner_join(sig, by = "value") |>
    dplyr::filter(!is.na(.data$cover)) |>
    dplyr::mutate(w = .data$a / sum(.data$a), .by = "site_code") |>
    dplyr::summarise(mapped_m2 = sum(.data$a), alnus_wrst_cover = sum(.data$w * .data$cover),
                     alnus_wrst_cover_se = sqrt(sum((.data$w * .data$sd)^2)),
                     alnus_wrst_unrecorded_pct = 100 * sum(.data$w[!.data$recorded]), .by = "site_code")
  out <- dplyr::tibble(site_code = x$site_code, area_m2 = x$area_m2) |>
    dplyr::left_join(a, by = "site_code") |>
    dplyr::mutate(alnus_wrst_coverage_pct = pmin(100, 100 * dplyr::coalesce(.data$mapped_m2, 0) / .data$area_m2))
  ci <- beta_interval(out$alnus_wrst_cover, out$alnus_wrst_cover_se)
  out |>
    dplyr::mutate(alnus_wrst_cover_lo95 = ci$lo, alnus_wrst_cover_hi95 = ci$hi,
                  alnus_wrst_cover_flag = flag_text(outside_map = .data$alnus_wrst_coverage_pct == 0,
                                                    partial_coverage = dplyr::between(.data$alnus_wrst_coverage_pct, 1e-9, 95 - 1e-9),
                                                    error_exceeds_cover = !ci$feasible)) |>
    dplyr::select("site_code", "alnus_wrst_cover", "alnus_wrst_coverage_pct", "alnus_wrst_cover_se", "alnus_wrst_cover_lo95",
                  "alnus_wrst_cover_hi95", "alnus_wrst_unrecorded_pct", "alnus_wrst_cover_flag")
}

# The WRST detailed vegetation grid (ESRI grid or GeoTIFF with its attribute table), downloaded and unpacked once
wrst_grid <- function() {
  dir <- file.path(ak_cache_dir(cache_paths$nps_wrst), "detailed")
  if (!dir.exists(dir)) {
    ak_download("https://irma.nps.gov/DataStore/DownloadFile/619340?Reference=2171667",
                file.path(ak_cache_dir(cache_paths$nps_wrst), "detailed.zip")) |>
      extract_archive(dir)
  }
  for (z in list.files(dir, "\\.zip$", recursive = TRUE, full.names = TRUE)) {  # DataStore packages may nest the grid in a zip
    extract_archive(z, sub("\\.zip$", "", z))
    unlink(z)
  }
  f <- list.files(dir, recursive = TRUE, full.names = TRUE)
  grids <- c(unique(dirname(f[basename(f) == "hdr.adf"])), f[grepl("\\.(tif|img)$", f, ignore.case = TRUE)])
  for (g in grids) {
    r <- terra::rast(g)
    if ("ALNUS_COVER" %in% names(terra::cats(r)[[1]])) return(r)
  }
  stop("No grid with an ALNUS_COVER attribute in ", dir, "; it holds: ", paste(utils::head(basename(f), 20), collapse = ", "), call. = FALSE)
}

# Alder cover and its SD per grid value (signature); the cells per value are counted once and cached
wrst_alder_signatures <- function(r, unrecorded = "class_mean") {
  rat <- terra::cats(r)[[1]]
  rat[] <- lapply(rat, \(v) if (is.character(v)) iconv(v, "latin1", "UTF-8") else v)  # NPS tables are Latin-1
  if (length(miss <- setdiff(c("SOURCE_COVER", "CALC_CLASS", "MAJOR_CLASS", "PR_COMP", "PR_COMP_COVER", "ALNUS_COVER"), names(rat)))) stop("The WRST grid's attribute table lacks ", paste(miss, collapse = ", "), call. = FALSE)
  cells_f <- file.path(ak_cache_dir(cache_paths$nps_wrst), "signature_cells.csv")
  if (!file.exists(cells_f)) {
    r0 <- r
    levels(r0) <- NULL
    utils::write.csv(dplyr::transmute(terra::freq(r0), value = .data$value, cells = .data$count), cells_f, row.names = FALSE)
  }
  d <- dplyr::tibble(value = rat[[1]], source = rat$SOURCE_COVER, class = rat$CALC_CLASS, life = rat$MAJOR_CLASS,
                     pr = rat$PR_COMP, pr_cover = as.numeric(rat$PR_COMP_COVER), alnus = as.numeric(rat$ALNUS_COVER)) |>
    dplyr::left_join(utils::read.csv(cells_f), by = "value") |>
    dplyr::mutate(cells = dplyr::coalesce(.data$cells, 0),
                  unmapped = .data$value == 0 | .data$source %in% c("GRSUNDF", "UNCLASS"),
                  species = .data$source %in% c("DU", "FirePro", "ISO_CL"),   # plot sources that recorded cover by species
                  aln = grepl("\\baln", .data$pr, ignore.case = TRUE),
                  recorded = .data$species | .data$aln,
                  cover = dplyr::case_when(.data$species ~ .data$alnus, .data$aln ~ .data$pr_cover))
  rec <- d[d$recorded & !d$unmapped, ]
  stat <- \(by) rec |>
    dplyr::summarise(n = dplyr::n(), m = stats::weighted.mean(.data$cover, pmax(.data$cells, 1)), s = stats::sd(.data$cover), .by = dplyr::all_of(by)) |>
    dplyr::filter(.data$n >= 5)
  cl <- stat("class")
  lf <- stat("life")
  all_m <- stats::weighted.mean(rec$cover, pmax(rec$cells, 1))
  all_s <- stats::sd(rec$cover)
  d |>
    dplyr::left_join(dplyr::select(cl, "class", m_c = "m", s_c = "s"), by = "class") |>
    dplyr::left_join(dplyr::select(lf, "life", m_l = "m", s_l = "s"), by = "life") |>
    dplyr::mutate(m = dplyr::coalesce(.data$m_c, .data$m_l, all_m), sd = dplyr::coalesce(.data$s_c, .data$s_l, all_s),
                  cover = dplyr::case_when(.data$unmapped ~ NA_real_, .data$recorded ~ .data$cover,
                                           unrecorded == "class_mean" ~ .data$m, .default = 0),
                  sd = dplyr::if_else(.data$unmapped, NA_real_, .data$sd)) |>
    dplyr::select("value", "class", "source", "cells", "recorded", "cover", "sd")
}

#' Land cover shares of each catchment
#'
#' Level II class shares (percent of the mapped part), appended to the
#' catchments. `source = "nlcd2016"` (the only source so far; others, such
#' as ESA WorldCover, can be added as sources) is the NLCD 2016
#' Alaska release (Dewitz 2019, doi:10.5066/P96HHBIE), downloaded once from
#' ScienceBase. Codes outside the NLCD legend (fill) are dropped.
#'
#' Uncertainty: no accuracy assessment exists for the 2016 Alaska map. For
#' NLCD 2001 Alaska, overall accuracy was 76.2% (s.e. 2.8%) at Level II with
#' primary-or-alternate reference labels and 59.4% with primary labels only
#' (Selkowitz and Stehman 2011). Errors bias class shares. Give that error
#' matrix (sample counts, rows = map class, columns = reference class, both
#' named with NLCD codes such as `"41"`) as `error_matrix` to add
#' bias-adjusted shares with SE and 95% interval for every catchment
#' (Olofsson et al. 2014, via [adjust_class_shares()]). The adjustment is a
#' regional correction: its SE reflects the reference sample, not
#' catchment-specific error.
#'
#' `nlcd_flag`: `no_2016_assessment` always (the map is unassessed),
#' `small_catchment` under 1 km2 (accuracy was lower near patch edges, which
#' dominate small areas), `partial_coverage` under 95% mapped.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param source Land cover product: `"nlcd2016"`.
#' @param error_matrix Optional confusion matrix of counts, as above.
#' @return `catchments` with columns added: `nlcd_pct_<class>`, `nlcd_pct_coverage_pct`
#'   and `nlcd_flag`; with `error_matrix`, also `nlcd_adj_pct_<class>`,
#'   `nlcd_adj_pct_<class>_se`, `_lo95`, `_hi95`.
#' @seealso `vignette("landcover-nlcd")` (download, use, uncertainty, validation)
#' @export
ak_landcover <- function(catchments, source = "nlcd2016", error_matrix = NULL) {
  source <- match.arg(source)
  x <- as_catchments(catchments)
  r <- nlcd_raster()
  out <- class_shares(x, r, prefix = "nlcd_pct_", labels = nlcd_classes) |>
    dplyr::mutate(nlcd_flag = flag_text(no_2016_assessment = TRUE, small_catchment = x$area_m2 < 1e6,
                                        partial_coverage = .data$nlcd_pct_coverage_pct < 95))
  if (!is.null(error_matrix)) out <- dplyr::left_join(out, landcover_adjust(out, error_matrix), by = "site_code")
  add_columns(catchments, out, layer_row("landcover", source, fn = "ak_landcover"),
              list(source = source, error_matrix = if (!is.null(error_matrix)) "<matrix>"))
}


# NLCD 2016 Alaska, downloaded and unpacked once
nlcd_raster <- function() {
  dir <- ak_cache_dir(cache_paths$nlcd)
  find <- \() list.files(dir, "\\.(tif|img)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (!length(find())) extract_archive(sb_download("5f650b8682ce38aaa23be1bd", "NLCD_2016_Land_Cover_AK_20200724.zip"), dir)
  terra::rast(find()[1])
}

# Olofsson adjustment per catchment; matrix dimnames are NLCD codes
landcover_adjust <- function(shares, error_matrix) {
  dimnames(error_matrix) <- lapply(dimnames(error_matrix), \(code) unname(nlcd_classes[code]))
  cols <- paste0("nlcd_pct_", rownames(error_matrix))
  cols <- cols[cols %in% names(shares)]
  purrr::map(seq_len(nrow(shares)), \(i) {
    v <- unlist(shares[i, cols])
    names(v) <- sub("^nlcd_pct_", "", cols)
    if (!length(v) || !isTRUE(sum(v) > 0)) return(NULL)
    adjust_class_shares(v, error_matrix) |>
      dplyr::transmute(site_code = shares$site_code[i], class = .data$class, est = .data$pct_adjusted,
                       se = .data$se, lo95 = .data$lo95, hi95 = .data$hi95)
  }) |>
    purrr::list_rbind() |>
    tidyr::pivot_wider(id_cols = "site_code", names_from = "class", values_from = c("est", "se", "lo95", "hi95"),
                       names_glue = "nlcd_adj_pct_{class}_{.value}") |>
    dplyr::rename_with(\(n) sub("_est$", "", n))
}

nlcd_classes <- c(`11` = "water", `12` = "icesnow", `21` = "dev_open", `22` = "dev_low", `23` = "dev_med",
                  `24` = "dev_high", `31` = "barren", `41` = "deciduous", `42` = "evergreen", `43` = "mixed",
                  `51` = "dwarf_shrub", `52` = "shrub", `71` = "grassland", `72` = "sedge", `74` = "moss",
                  `81` = "pasture", `82` = "crops", `90` = "woody_wetland", `95` = "herb_wetland")

#' Permafrost of each catchment, with uncertainty
#'
#' Adds permafrost columns from one of two maps:
#' * `source = "jorgenson2008"` (default): Permafrost Characteristics of
#'   Alaska (Jorgenson et al. 2008), extent and ground-ice class shares and
#'   total permafrost with its uncertainty (columns `jorg_pct_*`,
#'   `groundice_pct_*`, `totperm_pct*`). The shapefile is downloaded once.
#' * `source = "pastick2015"`: 30 m probability of near-surface permafrost
#'   (Pastick et al. 2015), downloaded once (columns `pf_*`).
#'
#' The two describe different things (a regional extent class versus a
#' modelled probability at 1 m depth) and have different columns, so both
#' can be added.
#'
#' @section Jorgenson et al. 2008 (`jorgenson2008`):
#' Permafrost extent classes and total permafrost with uncertainty.
#'
#' Reads the Permafrost Characteristics of Alaska map (Jorgenson et al. 2008;
#' shapefile from the North Slope Science Catalog, dataset 1725, downloaded
#' once, 13 MB) and adds, per catchment, the share of each extent class and an
#' estimate of total permafrost cover.
#'
#' Total permafrost combines two uncertainties by simulation: the true
#' permafrost fraction within each class is unknown within the class range
#' (continuous 90-100%, discontinuous 50-90, sporadic 10-50, isolated 1-10;
#' uniform draws, as in the CAKN Monte Carlo), and the map boundaries are
#' placed only to within kilometres (random shifts with SD `sigma_m`, see
#' [positional_uncertainty()]). Set `sigma_m = 0` to keep only the class
#' range.
#'
#' `totperm_pct_flag` is `jitter_dominates` where the positional part of
#' the spread (class midpoints on shifted catchments) is larger than the
#' within-class part (class ranges on the mapped shares): for those sites the
#' map's placement, not its class definitions, sets the uncertainty.
#'
#'
#' Class shares are percent of the whole catchment (as in the NPS per*perm
#' columns and the geo workflow), with `jorg_pct_coverage_pct` the mapped
#' share. The ground-ice classes of the same map are added as
#' `groundice_pct_*`.
#'
#' @section Pastick et al. 2015 (`pastick2015`):
#' Near-surface permafrost probability (Pastick et al. 2015).
#'
#' Catchment summaries of the 30 m probability of near-surface (within 1 m)
#' permafrost in Alaska (Pastick et al. 2015, USGS data release
#' doi:10.5066/F7C53HX6; ~2001-2010), downloaded once from ScienceBase (777
#' MB, 7-Zip; needs the archive package or a 7z program). Values 0-100 are
#' probabilities; 101-105 are masked NLCD 2001 classes (water, ice and snow,
#' developed, barren, cultivated).
#'
#' Uncertainty: about 85% accuracy at the 50% threshold, likely lower for
#' small areas; errors are correlated within a catchment, so no SE is given.
#' `pf_land_lo` / `pf_land_hi` bound permafrost over all non-glacier,
#' non-water land by treating masked land (mostly barren rock) as none / all
#' permafrost. `pf_flag`: `partial_coverage` (under 90% mapped) and
#' `mostly_masked` (more than 25% masked, where `pf_prob_mean` describes only
#' the vegetated part).
#'
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param source `"jorgenson2008"` or `"pastick2015"`.
#' @param path For `jorgenson2008`: the unzipped shapefile, or a folder
#'   containing it (default: the cache's `jorgenson_2008` folder, where it is
#'   downloaded if it isn't there yet).
#' @param sigma_m For `jorgenson2008`: positional error SD of the map,
#'   metres; 0 keeps only the class-range uncertainty.
#' @param n Number of draws.
#' @param ext_field,ice_field For `jorgenson2008`: field names, found from
#'   the data if `NULL`: the extent field is the one naming the classes, or
#'   holding the 2008 shapefile's letter codes (`PF_EXTENT`: C, D, S, I, plus
#'   G glacier, U unfrozen, W water); the ground-ice field is another with
#'   "ice" in its name (`ICECLOWASS`).
#' @param seed Random seed.
#' @return `catchments` with columns added. `jorgenson2008`: `jorg_pct_*`,
#'   `jorg_pct_coverage_pct`, `groundice_pct_*`, `totperm_pct`
#'   (class-midpoint estimate), `totperm_pct_se`, `totperm_pct_lo95`,
#'   `totperm_pct_hi95`, `totperm_pct_flag`. `pastick2015`: `pf_prob_mean`,
#'   `pf_land_lo`, `pf_land_hi`, `pf_extent_pct`, `pf_masked_pct`,
#'   `pf_icesnow_pct`, `pf_barren_pct`, `pf_coverage_pct`, `pf_flag`.
#' @examples
#' \dontrun{
#' x <- ak_permafrost(x)
#' x <- ak_permafrost(x, source = "pastick2015")
#' }
#' @seealso `vignette("permafrost-jorgenson")`, `vignette("permafrost-pastick")`
#' @export
ak_permafrost <- function(catchments, source = c("jorgenson2008", "pastick2015"), path = NULL, sigma_m = 1000, n = 500,
                          ext_field = NULL, ice_field = NULL, seed = 1) {
  source <- match.arg(source)
  x <- as_catchments(catchments)
  new <- switch(source,
                jorgenson2008 = permafrost_jorgenson(x, path %||% ak_cache_dir(cache_paths$jorgenson), sigma_m, n, ext_field, ice_field, seed),
                pastick2015 = permafrost_pastick(x))
  args <- if (source == "jorgenson2008") list(source = source, sigma_m = sigma_m, n = n, seed = seed) else list(source = source)
  add_columns(catchments, new, layer_row("permafrost", source, fn = "ak_permafrost"), args)
}

# The Jorgenson et al. (2008) shapefile in `dir`, downloaded once from the North Slope Science Catalog (dataset 1725)
jorgenson_shapefile <- function(dir) {
  shp <- list.files(dir, "\\.shp$", recursive = TRUE, full.names = TRUE)[1]
  if (!is.na(shp)) return(shp)
  zip <- ak_download(paste0("https://catalog.northslopescience.org/dataset/6803a021-6f72-4aa5-95f6-bc5facd534e8/resource/",
                            "5811669f-860c-450e-9bc0-9dfc21821606/download/233-permafrost-characteristics-of-alaska-2008-sha.zip"),
                     file.path(dir, "permafrost-characteristics-of-alaska-2008.zip"))
  utils::unzip(zip, exdir = dir)
  shp <- list.files(dir, "\\.shp$", recursive = TRUE, full.names = TRUE)[1]
  if (is.na(shp)) stop("The Jorgenson et al. (2008) download held no shapefile (", zip, "); download it from ",
                       "https://catalog.northslopescience.org/dataset/1725 and unzip it into ", dir, call. = FALSE)
  shp
}

permafrost_jorgenson <- function(x, path, sigma_m = 1000, n = 500, ext_field = NULL, ice_field = NULL, seed = 1) {
  shp <- if (dir.exists(path)) jorgenson_shapefile(path) else path
  m <- sf::st_read(shp, quiet = TRUE)
  if (is.na(sf::st_crs(m))) sf::st_crs(m) <- akalbers_crs()
  fields <- sf::st_drop_geometry(m)
  is_ext <- \(v) any(grepl("continuous|sporadic|isolated", v, ignore.case = TRUE)) ||
    (is.character(v) && all(toupper(trimws(stats::na.omit(v))) %in% names(jorgenson_codes)) && any(toupper(v) %in% c("C", "D", "S", "I")))
  ext_field <- ext_field %||% names(fields)[purrr::map_lgl(fields, is_ext)][1]
  ice_field <- ice_field %||% setdiff(grep("ice", names(fields), ignore.case = TRUE, value = TRUE), ext_field)[1]
  if (is.na(ext_field)) stop("Could not find the extent field; set `ext_field`. Fields: ", paste(names(fields), collapse = ", "))
  m$extent <- permafrost_class(m[[ext_field]])
  if (!any(m$extent %in% c("continuous", "discontinuous", "sporadic", "isolated")))   # otherwise total permafrost would silently be 0
    stop("No permafrost extent class recognised in field ", ext_field, "; its values: ",
         paste(utils::head(sort(unique(m[[ext_field]])), 20), collapse = ", "), call. = FALSE)
  m <- sf::st_filter(sf::st_make_valid(sf::st_transform(m, akalbers_crs())), sf::st_buffer(x, 4 * sigma_m + 1))

  shares <- class_shares(x, m, "extent", prefix = "jorg_pct_", of = "catchment")
  if (!is.na(ice_field)) {
    m$ground_ice <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(m[[ice_field]])))
    shares <- dplyr::left_join(shares, dplyr::select(class_shares(x, m, "ground_ice", prefix = "groundice_pct_", of = "catchment"),
                                                     -"groundice_pct_coverage_pct"), by = "site_code")
  }
  lims <- rbind(continuous = c(0.9, 1), discontinuous = c(0.5, 0.9), sporadic = c(0.1, 0.5), isolated = c(0.01, 0.1))
  draws <- if (sigma_m > 0) {
    positional_draws(x, m, "extent", sigma_m, n, seed = seed, of = "catchment")
  } else {
    shares |> dplyr::select("site_code", dplyr::starts_with("jorg_pct_"), -"jorg_pct_coverage_pct") |>
      tidyr::pivot_longer(-"site_code", names_to = "class", names_prefix = "jorg_pct_", values_to = "pct") |>
      tidyr::crossing(draw = seq_len(n))
  }
  set.seed(seed)
  u <- dplyr::tibble(draw = rep(seq_len(n), each = nrow(lims)), class = rep(rownames(lims), n),
                     f = stats::runif(n * nrow(lims), rep(lims[, 1], n), rep(lims[, 2], n)))
  tot <- draws |>
    dplyr::inner_join(u, by = c("draw", "class")) |>
    dplyr::summarise(t = sum(.data$pct * .data$f), .by = c("site_code", "draw")) |>
    dplyr::summarise(totperm_pct_se = stats::sd(.data$t), totperm_pct_lo95 = stats::quantile(.data$t, 0.025),
                     totperm_pct_hi95 = stats::quantile(.data$t, 0.975), .by = "site_code")
  mid <- c(continuous = 0.95, discontinuous = 0.7, sporadic = 0.3, isolated = 0.055)
  cols <- intersect(paste0("jorg_pct_", names(mid)), names(shares))
  shares$totperm_pct <- as.vector(as.matrix(shares[cols]) %*% mid[sub("jorg_pct_", "", cols)])
  # Which part of the spread dominates: shifts at class midpoints vs class ranges on the mapped shares
  jit <- draws |>
    dplyr::filter(.data$class %in% names(mid)) |>
    dplyr::summarise(t = sum(.data$pct * mid[.data$class]), .by = c("site_code", "draw")) |>
    dplyr::summarise(sd_jitter = stats::sd(.data$t), .by = "site_code")
  cls <- shares |>
    dplyr::select("site_code", dplyr::all_of(cols)) |>
    tidyr::pivot_longer(-"site_code", names_to = "class", names_prefix = "jorg_pct_", values_to = "pct") |>
    dplyr::inner_join(u, by = "class", relationship = "many-to-many") |>
    dplyr::summarise(t = sum(.data$pct * .data$f), .by = c("site_code", "draw")) |>
    dplyr::summarise(sd_class = stats::sd(.data$t), .by = "site_code")
  dplyr::left_join(shares, tot, by = "site_code") |>
    dplyr::left_join(dplyr::full_join(jit, cls, by = "site_code"), by = "site_code") |>
    dplyr::mutate(totperm_pct_flag = flag_text(jitter_dominates = .data$sd_jitter > .data$sd_class)) |>
    dplyr::select(-"sd_jitter", -"sd_class")
}

# The 2008 shapefile codes PF_EXTENT as letters (its metadata: C continuous, D discontinuous, S sporadic,
# I isolated, G glacier, U unfrozen, W large waterbodies); other versions spell the classes out
jorgenson_codes <- c(C = "continuous", D = "discontinuous", S = "sporadic", I = "isolated", G = "glacier", U = "unfrozen", W = "water")

permafrost_class <- function(v) {
  code <- unname(jorgenson_codes[toupper(trimws(v))])
  dplyr::case_when(!is.na(code) ~ code,
                   grepl("discontinuous", v, ignore.case = TRUE) ~ "discontinuous",
                   grepl("continuous", v, ignore.case = TRUE) ~ "continuous",
                   grepl("sporadic", v, ignore.case = TRUE) ~ "sporadic",
                   grepl("isolated", v, ignore.case = TRUE) ~ "isolated",
                   .default = gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(v))))
}


#' Fire history of each catchment
#'
#' Burned area from the BLM Alaska Fire Service fire perimeter history
#' (prescribed burns dropped; burns in the same year unioned so overlaps
#' count once), in one of three forms:
#' * default: one row per site, appended to the catchments:
#'   `fire_burned_pct` (share ever burned, 1940 on), `fire_burned_pct_since<since>`
#'   (share burned since `since`), `fire_last_year`, `fire_n_years` (fire
#'   years with burning in the catchment) and `fire_flag` (`small_catchment`,
#'   under 1000 acres, where the mapping thresholds miss most burns);
#' * `by_year = TRUE`: a long table, one row per site and fire year;
#' * `samples` given: a long table, one row per site and sample year, with
#'   the burned share in windows before it.
#'
#' Uncertainty: the record starts in 1940 but mapping emphasis changed: fires
#' of at least 1000 acres before 1987, 100 acres from 1987 to 2015, and more
#' than 10 acres from 2016. Small burns in small catchments are missed early
#' in the record, which makes fire look as if it increases over time; don't
#' read the fire variables as a trend. `mapping_min_acres` (by year) and
#' `fire_min_acres_hist` (windows) carry the threshold.
#'
#' The perimeters for the catchments' bounding box are saved in the cache
#' (`fire_history_ak.gpkg`) with the area they cover; catchments outside that
#' area trigger a new query covering both. Set `refresh = TRUE` after a new
#' fire season.
#'
#' @section Windows before each sample (`samples`):
#' Per site and sampling year, the share of the catchment burned (fire
#' perimeters unioned within the window, so reburns count once):
#' * `burn_frac_5yr`, `burn_frac_15yr`: the 5 and 15 years before the sample
#'   year (time-varying, for trend attribution);
#' * `burn_frac_hist`: from `hist_start` to the year before (fire-regime
#'   trait);
#' * `burn_frac_same_yr`: the sample year, including fires after the sample
#'   date (the layer holds final perimeters only).
#'
#' Uncertainty: mapping emphasis fell from fires of at least 1000 acres
#' before 1987 to 100 acres to 2015 and 10 acres from 2016, so small burns in
#' small catchments are undercounted early and fire looks as if it
#' increases. `fire_min_acres_hist` gives the largest (coarsest) threshold in
#' each site's historical window. Don't read the fire variables as a trend.
#'
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param samples Optional data frame of `site_code` and `year` (or
#'   `sample_date`): returns the burned shares in windows before each.
#' @param by_year Return the burned area by site and fire year?
#' @param since First year of the recent burned share (default 1987, when
#'   the 100-acre mapping threshold began).
#' @param hist_start First year of the historical window for `samples` (the
#'   AFS record starts in 1940).
#' @param refresh Download the perimeters again?
#' @return Default: `catchments` with the `fire_*` columns added. With
#'   `by_year = TRUE`: a table of `site_code`, `fire_year`, `burned_km2`,
#'   `burned_frac`, `mapping_min_acres`. With `samples`: a table of
#'   `site_code`, `year`, `burn_frac_5yr`, `burn_frac_15yr`, `burn_frac_hist`,
#'   `burn_frac_same_yr`, `fire_min_acres_hist`. Tables carry their
#'   provenance ([ak_provenance()]).
#' @examples
#' \dontrun{
#' x <- ak_fire(x)                                   # one row per site
#' fy <- ak_fire(x, by_year = TRUE)                 # site x fire year
#' fw <- ak_fire(x, samples = samples)              # windows before each sample year
#' }
#' @seealso `vignette("fire-afs")`
#' @export
ak_fire <- function(catchments, samples = NULL, by_year = FALSE, since = 1987, hist_start = 1940, refresh = FALSE) {
  x <- as_catchments(catchments)
  row <- layer_row("fire", fn = "ak_fire")
  if (!is.null(samples)) {
    return(as_layer_table(fire_windows(x, samples, hist_start, refresh), row, list(samples = samples, hist_start = hist_start)))
  }
  if (by_year) return(as_layer_table(fire_by_year(x, refresh), row, list(by_year = TRUE)))
  add_columns(catchments, fire_static(x, refresh, since), row, list(since = since))
}

fire_by_year <- function(x, refresh = FALSE) {
  p <- fire_pieces(x, refresh)
  if (!nrow(p)) return(dplyr::tibble(site_code = character(), fire_year = integer(), burned_km2 = numeric(),
                                     burned_frac = numeric(), mapping_min_acres = numeric()))
  p <- p |>
    dplyr::group_by(.data$site_code, .data$area_m2, .data$fire_year) |>
    dplyr::summarise(.groups = "drop")
  p$burned_km2 <- as.numeric(sf::st_area(p)) / 1e6
  sf::st_drop_geometry(p) |>
    dplyr::transmute(.data$site_code, .data$fire_year, .data$burned_km2, burned_frac = .data$burned_km2 * 1e6 / .data$area_m2,
                     mapping_min_acres = fire_min_acres(.data$fire_year))
}

fire_windows <- function(x, site_years, hist_start = 1940, refresh = FALSE) {
  if (!"year" %in% names(site_years)) site_years$year <- as.integer(format(as.Date(site_years$sample_date), "%Y"))
  sy <- dplyr::distinct(site_years, .data$site_code, .data$year) |> dplyr::filter(.data$site_code %in% x$site_code)
  p <- fire_pieces(x, refresh)
  area <- \(s) unname(stats::setNames(x$area_m2, x$site_code)[s])
  burned <- \(s, y0, y1) {
    q <- p[p$site_code == s & p$fire_year >= y0 & p$fire_year <= y1, ]
    if (!nrow(q)) 0 else sum(as.numeric(sf::st_area(sf::st_union(q))))
  }
  sy |>
    dplyr::mutate(burn_frac_5yr = purrr::map2_dbl(.data$site_code, .data$year, \(s, y) burned(s, y - 5, y - 1)) / area(.data$site_code),
                  burn_frac_15yr = purrr::map2_dbl(.data$site_code, .data$year, \(s, y) burned(s, y - 15, y - 1)) / area(.data$site_code),
                  burn_frac_hist = purrr::map2_dbl(.data$site_code, .data$year, \(s, y) burned(s, hist_start, y - 1)) / area(.data$site_code),
                  burn_frac_same_yr = purrr::map2_dbl(.data$site_code, .data$year, \(s, y) burned(s, y, y)) / area(.data$site_code),
                  fire_min_acres_hist = fire_min_acres(hist_start)) |>
    dplyr::arrange(.data$site_code, .data$year)
}

fire_min_acres <- function(year) dplyr::case_when(year < 1987 ~ 1000, year <= 2015 ~ 100, .default = 10)

# Fire perimeters for an area, cached in the cache root (fire_history_ak.gpkg, as the geo workflow kept
# them) with a `coverage` layer recording the area queried. An area outside it is queried again together
# with the old one, so the cache grows to cover every request. A file without a coverage layer (from the
# geo workflow) is taken to cover the bounding box of its perimeters.
fire_perimeters <- function(aoi, refresh = FALSE) {
  cache <- file.path(ak_cache_dir(), cache_paths$fire)
  need <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(as_aoi(aoi), 1000)))
  have <- if (!refresh && file.exists(cache)) fire_coverage(cache)
  if (!is.null(have) && isTRUE(sf::st_covers(have, need, sparse = FALSE)[1, 1])) {
    message("Using cached fire perimeters ", cache, "; refresh = TRUE fetches them again (after a new fire season)")
    return(sf::st_read(cache, layer = fire_layer(cache), quiet = TRUE))
  }
  area <- if (is.null(have)) need else sf::st_as_sfc(sf::st_bbox(c(have, need)))
  f <- ak_arcgis_query("https://fire.ak.blm.gov/arcgis/rest/services/MapAndFeatureServices/FireHistory/FeatureServer/1",
                       area, fields = "FIREID,FIREYEAR,PRESCRIBED,ACRES")
  tmp <- tempfile(fileext = ".gpkg")
  sf::st_write(f, tmp, layer = "perimeters", driver = "GPKG", quiet = TRUE)
  sf::st_write(sf::st_sf(queried_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"), geometry = area),
               tmp, layer = "coverage", driver = "GPKG", quiet = TRUE)
  if (!replace_file(tmp, cache)) stop("Could not write ", cache, " (is it open in another program?)", call. = FALSE)
  log_download("https://fire.ak.blm.gov/arcgis/rest/services/MapAndFeatureServices/FireHistory/FeatureServer/1", cache)
  f
}

fire_layer <- function(cache) setdiff(sf::st_layers(cache)$name, "coverage")[1]

fire_coverage <- function(cache) {
  lay <- sf::st_layers(cache)$name
  if ("coverage" %in% lay) return(sf::st_geometry(sf::st_read(cache, layer = "coverage", quiet = TRUE)) |> sf::st_transform(akalbers_crs()) |> sf::st_union())
  f <- sf::st_read(cache, layer = fire_layer(cache), quiet = TRUE)
  if (!nrow(f)) return(NULL)
  sf::st_as_sfc(sf::st_bbox(sf::st_transform(f, akalbers_crs())))
}

# Fire perimeters (prescribed burns dropped) cut to the catchments
fire_pieces <- function(x, refresh = FALSE) {
  f <- fire_perimeters(x, refresh)
  if (!nrow(f)) return(sf::st_sf(site_code = character(), area_m2 = numeric(), fire_year = integer(), geometry = sf::st_sfc(crs = akalbers_crs())))
  f <- f[is.na(f$PRESCRIBED) | f$PRESCRIBED != "Y", ] |> sf::st_transform(akalbers_crs()) |> sf::st_make_valid()
  suppressWarnings(sf::st_intersection(x[c("site_code", "area_m2")], dplyr::transmute(f, fire_year = as.integer(.data$FIREYEAR))))
}

# Fire as static columns: the burned share over the whole record (reburns counted once)
fire_static <- function(y, refresh = FALSE, since = 1987) {
  p <- fire_pieces(y, refresh)
  burned <- \(pp) if (!nrow(pp)) 0 else sum(as.numeric(sf::st_area(sf::st_union(pp))))
  per <- \(s, y0 = -Inf) burned(p[p$site_code == s & p$fire_year >= y0, ])
  dplyr::tibble(site_code = y$site_code, area_m2 = y$area_m2) |>
    dplyr::mutate(fire_burned_pct = 100 * purrr::map_dbl(.data$site_code, per) / .data$area_m2,
                  fire_burned_pct_since = 100 * purrr::map_dbl(.data$site_code, \(s) per(s, since)) / .data$area_m2,
                  fire_last_year = purrr::map_int(.data$site_code, \(s) {
                    yy <- p$fire_year[p$site_code == s]
                    if (length(yy)) as.integer(max(yy)) else NA_integer_
                  }),
                  fire_n_years = purrr::map_int(.data$site_code, \(s) length(unique(p$fire_year[p$site_code == s]))),
                  fire_flag = flag_text(small_catchment = .data$area_m2 < 1000 * 4046.86)) |>
    dplyr::rename_with(\(n) sub("_since$", paste0("_since", since), n)) |>
    dplyr::select(-"area_m2")
}
