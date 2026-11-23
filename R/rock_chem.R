#' Rock chemistry of each catchment (nitrogen, sulfur, major oxides), with uncertainty
#'
#' The expected whole-rock content of nitrogen, sulfur or a major oxide in
#' the rocks (and mapped surficial deposits) of each catchment, from the
#' lithologies of the Geologic Map of Alaska (SIM 3340) and the lithology
#' geochemistry of Olson and Hawkins (2014). Their CONUS rasters (StreamCat's
#' rock %N and the oxide layers) were built the same way from the CONUS
#' state geologic map databases, which share SIM 3340's lithology vocabulary
#' and rank scheme; this applies their method to Alaska, map unit by map
#' unit, and carries the uncertainty through.
#'
#' Elements: `"N"` and `"S"` (workbook Lith-Nutrient, 284 lithology
#' classes) and `"SiO2"`, `"Al2O3"`, `"Fe2O3"`, `"CaO"`, `"MgO"`, `"Na2O"`,
#' `"K2O"`, `"P2O5"` (Lith-MajorOxides, 279 classes), all in percent of rock
#' mass. Ask for several at once; each gets its own columns. The oxides are
#' the natural placebo exposures for rock N (`vignette("rock-chemistry")`).
#'
#' @section Method:
#' 1. **Lithology classes.** Each lithology of a SIM 3340 map unit
#'    (table NSALith: `LITH1`-`LITH5`, from general to specific, and `RANK`)
#'    is matched to an Olson class by its most specific term; when that term
#'    is not a class, the next more general one is tried, and the match is
#'    counted as `coarsened`. Olson split some lithologies by a modifier
#'    (e.g. carbonaceous vs. non-carbonaceous shale) read from the unit
#'    descriptions; SIM 3340 has no modifier field, so assign them with
#'    `modifiers` (a table you fill in from the Description of Map Units,
#'    one row per unit and lithology, with your rationale; start from
#'    [rock_chem_crosswalk()]). Classes that are not rock types (water,
#'    melange, landslide) and unmatched terms have no value: their share is
#'    reported as `_na_frac`.
#' 2. **Prevalence within a map unit.** SIM 3340 ranks each lithology by
#'    how much of the unit it makes up: Major (at least 33%), Minor (10-33%),
#'    Incidental (under 10%); Indeterminate is taken as 10-100%. The point
#'    estimate uses the midpoint of each range, normalised within the unit;
#'    the uncertainty draws a share uniformly within each range and
#'    normalises again (`rank_shares` to change the ranges). Olson's
#'    "prevalence weighting" is not documented in more detail than this.
#' 3. **Map unit value**: the weighted mean of its lithologies' values.
#'    **Catchment value**: the area-weighted mean over the map units in it
#'    with a value.
#'
#' @section Uncertainty:
#' Rock N within one lithology spans orders of magnitude (organic matter,
#' ammonium in micas and feldspars, metamorphic grade: high-grade rocks
#' have lost much of theirs), so a class value is a rough guess for any one
#' formation. Each draw (`n`, default 1000) combines:
#' * **the class value**: for each class, a shift on the log scale with SD
#'   `sdlog / sqrt(n_records)` (how well the class mean is known), shared by
#'   every unit with that class;
#' * **the formation**: for each map unit and lithology, a lognormal
#'   deviation with SD `formation_sd x sdlog`, mean-preserving, so that a
#'   formation's real content can sit anywhere in its class's spread.
#'   `formation_sd = 1` (default) attributes all of the spread among
#'   records to differences between formations, which is conservative
#'   (part of it is variation within a formation, which averages out over
#'   a catchment); 0 keeps only the uncertainty of the class means;
#' * **the shares within each unit**, drawn within their rank ranges.
#'
#' `sdlog` is the SD of log values among a class's source records in the
#' workbook. Classes with fewer than `min_n` records (or none: values
#' imputed or from one literature figure) get the 90th percentile of
#' `sdlog` among well-sampled classes of that element, so sparse classes
#' carry wide intervals. A unit shared by several catchments has the same
#' draw in all of them, so errors are correlated between catchments on the
#' same rocks, as they should be.
#'
#' `_pct` is the plug-in estimate (class values, rank midpoints), which is
#' what Olson's rasters give; `_se`, `_lo95` and `_hi95` come from the
#' draws, which are right-skewed. Not included: map-boundary placement at
#' 1:1,584,000 (see [positional_uncertainty()]), misassigned lithologies,
#' and how far CONUS rocks represent Alaskan ones of the same name.
#'
#' Flags (`rock<el>_flag`): `no_value` (nothing characterised),
#' `high_na` (over 25% of the mapped area without a value), `sparse_classes`
#' (over half the weight from classes with fewer than `min_n` records),
#' `coarsened_match` (over 25% matched at a more general term),
#' `surficial_dominant` (over half the mapped area in unconsolidated units:
#' the value describes the deposits, not the bedrock beneath),
#' `partial_coverage` (under 95% of the catchment mapped).
#'
#' **Surficial deposits.** SIM 3340, like Olson's maps, shows surficial
#' deposits where they were mapped and bedrock elsewhere.
#' `surficial = "exclude"` drops units made only of unconsolidated
#' lithologies (alluvium, glacial deposits, loess, ...) and averages over
#' the rest, as a sensitivity check; it is not a bedrock map (the rock under
#' the deposits is unknown). `rock_surficial_frac` reports their share.
#'
#' @param catchments Catchments: sf polygons with a unique `site_code` (from
#'   [ak_catchments()], [import_catchments()] or your own; any CRS), an
#'   `ak_delineation`, or a list of them.
#' @param element Elements or oxides, see above.
#' @param source Lithology geochemistry: `"olson2014"` (Olson and Hawkins
#'   2014 data release; method in Olson and Hawkins 2012).
#' @param modifiers Optional data frame (or CSV path) assigning Olson
#'   classes by hand: columns `NSACLASS`, `lithology` (the SIM 3340 term it
#'   applies to, as in [rock_chem_crosswalk()]'s `lith_term`; `"*"` or empty
#'   for every lithology of the unit), `olson_class` (a class name from
#'   [rock_chem_lithologies()]) and `rationale`. A filled-in
#'   [rock_chem_crosswalk()] file works as is: its `modifier_class` column
#'   is read, and rows left empty are ignored.
#' @param surficial `"as_mapped"` or `"exclude"` (see above).
#' @param rank_shares Named list of share ranges (percent) for the ranks
#'   `major`, `minor`, `incidental`, `indeterminate`.
#' @param formation_sd Share (0-1) of a class's spread of records taken as
#'   formation-to-formation variation.
#' @param min_n Records needed for a class's own spread to be used.
#' @param n Number of draws.
#' @param seed Random seed.
#' @param lith_table Optional data frame replacing the workbook: `element`,
#'   `lithology`, `central` (percent), and optionally `n` and `sdlog`.
#' @param details Also return the crosswalk and the map-unit values?
#' @return `catchments` with, for each element `<el>`: `rock<el>_pct`,
#'   `rock<el>_pct_se`, `rock<el>_pct_lo95`, `rock<el>_pct_hi95`,
#'   `rock<el>_na_frac`, `rock<el>_sparse_frac`, `rock<el>_coarsened_frac`,
#'   `rock<el>_modifier_frac`, `rock<el>_flag`; and `rock_surficial_frac`,
#'   `rock_mapped_pct`. With `details = TRUE`, a list: `catchments` (that),
#'   `crosswalk` (one row per map unit, lithology and element, with the
#'   class matched and its value, records and spread) and `units` (map-unit
#'   values with their SD).
#' @references Olson, J.R. and Hawkins, C.P. (2012) Predicting natural
#'   base-flow stream water chemistry in the western United States. Water
#'   Resources Research 48, W02504. Olson, J.R. and Hawkins, C.P. (2014)
#'   Geochemical characteristics of the conterminous United States. USGS
#'   data release, doi:10.5066/F7X0653P. Wilson, F.H. et al. (2015) Geologic
#'   map of Alaska. USGS SIM 3340, doi:10.3133/sim3340.
#' @examples
#' \dontrun{
#' x <- ak_rock_chem(x, element = c("N", "S", "CaO", "MgO", "P2O5"))
#' cw <- rock_chem_crosswalk(x, file = "rock_modifiers.csv")   # fill olson_class, rationale
#' x <- ak_rock_chem(x, "N", modifiers = "rock_modifiers.csv")
#' }
#' @seealso `vignette("rock-chemistry")`, [ak_geology()]
#' @export
ak_rock_chem <- function(catchments, element = "N", source = "olson2014", modifiers = NULL,
                         surficial = c("as_mapped", "exclude"),
                         rank_shares = list(major = c(33, 100), minor = c(10, 33), incidental = c(0, 10), indeterminate = c(10, 100)),
                         formation_sd = 1, min_n = 5,
                         n = 1000, seed = 1, lith_table = NULL, details = FALSE) {
  source <- match.arg(source)
  surficial <- match.arg(surficial)
  element <- rock_element(element)
  x <- as_catchments(catchments)
  g <- geology_tables(x)
  classes <- lith_table %||% rock_chem_lithologies(element, source)
  cw <- rock_crosswalk(g$lith[g$lith$NSACLASS %in% g$units$NSACLASS, ], classes, element, read_modifiers(modifiers), min_n)
  p <- suppressWarnings(sf::st_intersection(x[c("site_code", "area_m2")], g$units["NSACLASS"]))
  area <- dplyr::tibble(site_code = p$site_code, area_m2 = p$area_m2, NSACLASS = p$NSACLASS, a = as.numeric(sf::st_area(p))) |>
    dplyr::summarise(a = sum(.data$a), area_m2 = dplyr::first(.data$area_m2), .by = c("site_code", "NSACLASS"))
  surf_units <- unique(cw$NSACLASS[cw$surficial_unit])
  cols <- dplyr::tibble(site_code = x$site_code) |>
    dplyr::left_join(area |> dplyr::summarise(rock_mapped_pct = 100 * sum(.data$a) / dplyr::first(.data$area_m2),
                                              rock_surficial_frac = sum(.data$a[.data$NSACLASS %in% surf_units]) / sum(.data$a),
                                              .by = "site_code"), by = "site_code")
  if (surficial == "exclude") area <- area[!area$NSACLASS %in% surf_units, ]
  res <- lapply(element, \(el) rock_element_draws(cw[cw$element == el, ], area, x$site_code, el, rank_shares, formation_sd, n, seed))
  for (r in res) cols <- dplyr::left_join(cols, r$sites, by = "site_code")
  shared <- flag_text(surficial_dominant = cols$rock_surficial_frac > 0.5, partial_coverage = !(cols$rock_mapped_pct >= 95))
  cols <- dplyr::mutate(cols, dplyr::across(dplyr::ends_with("_flag"), \(f) paste_flags(f, shared)))
  out <- add_columns(catchments, cols, layer_row("rock_chemistry", source, fn = "ak_rock_chem"),
                     list(element = element, source = source, surficial = surficial, formation_sd = formation_sd, min_n = min_n,
                          n = n, seed = seed, modifiers = !is.null(modifiers), lith_table = !is.null(lith_table)),
                     note = sprintf("%d map units; %d lithology rows; %d with a hand-assigned class", dplyr::n_distinct(cw$NSACLASS),
                                    nrow(cw) / length(element), sum(cw$modifier) / length(element)))
  if (!details) return(out)
  list(catchments = out, crosswalk = cw, units = purrr::map(res, "units") |> purrr::list_rbind())
}

rock_workbooks <- list(nutrient = list(item = "56e47c89e4b0f59b85d4a69a", file = "Lith-Nutrient.xls", elements = c("N", "S")),
                       oxides = list(item = "56e47649e4b0f59b85d4a694", file = "Lith-MajorOxides.xls",
                                     elements = c("SiO2", "Al2O3", "Fe2O3", "CaO", "MgO", "Na2O", "K2O", "P2O5")))

rock_element <- function(element) {
  all <- unlist(purrr::map(rock_workbooks, "elements"))
  out <- all[match(tolower(element), tolower(all))]
  if (anyNA(out)) stop("Unknown element: ", paste(element[is.na(out)], collapse = ", "), ". Available: ",
                       paste(all, collapse = ", "), call. = FALSE)
  unique(out)
}

paste_flags <- function(a, b) {
  out <- ifelse(nzchar(a) & nzchar(b), paste(a, b, sep = ";"), paste0(a, b))
  dplyr::coalesce(out, b)
}

#' Lithology geochemistry: the class values behind the rock chemistry
#'
#' Downloads (once) and reads the Olson and Hawkins (2014) workbooks: the
#' central value of each lithology class from the "Lith Summary" tab, and,
#' from each class's own tab, the number of source records and their spread
#' (mean, median, SD, and SD of log values). This is the audit table: look
#' at `n` and `sdlog` before trusting a class, especially for N, which is
#' rarely part of standard whole-rock analyses.
#'
#' The workbooks are read by position of their headers, which the data
#' release does not document: the lithology column is the one whose header
#' names lithology, and the element column the one whose header names the
#' element. `tab` says which per-class tab was found for each class (`NA`:
#' none, so `n` and the spread are unknown); `parse_note` says what could
#' not be read. Check them, and pass a corrected table to
#' [ak_rock_chem()] as `lith_table` if needed.
#'
#' @param element Elements, as in [ak_rock_chem()].
#' @param source `"olson2014"`.
#' @param refresh Download and parse again?
#' @return A data frame: `element`, `lithology`, `central` (percent),
#'   `n` (records found), `mean`, `median`, `sd`, `sdlog`, `tab`,
#'   `parse_note`.
#' @examples
#' \dontrun{
#' lt <- rock_chem_lithologies("N")
#' lt[order(lt$n), ]               # sparse classes first
#' }
#' @seealso `vignette("rock-chemistry")`
#' @export
rock_chem_lithologies <- function(element = "N", source = "olson2014", refresh = FALSE) {
  source <- match.arg(source)
  element <- rock_element(element)
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Install the readxl package to read the lithology workbooks")
  books <- names(rock_workbooks)[vapply(rock_workbooks, \(b) any(element %in% b$elements), TRUE)]
  purrr::map(books, \(b) {
    wb <- rock_workbooks[[b]]
    f <- sb_download(wb$item, wb$file, file.path(ak_cache_dir(cache_paths$olson), wb$file), pattern = "\\.xlsx?$")
    cache <- file.path(ak_cache_dir(cache_paths$olson), paste0(b, "_parsed.rds"))
    if (!refresh && file.exists(cache) && file.mtime(cache) >= file.mtime(f)) return(readRDS(cache))
    # read by content, not extension (a workbook saved as .xlsx can carry an .xls name)
    fmt <- readxl::format_from_signature(f)
    if (!is.na(fmt) && !identical(tolower(tools::file_ext(f)), fmt)) {
      f2 <- tempfile(fileext = paste0(".", fmt))
      file.copy(f, f2)
      f <- f2
    }
    sheets <- readxl::excel_sheets(f)
    read <- \(s) suppressMessages(readxl::read_excel(f, sheet = s, col_names = FALSE, col_types = "text"))
    summ <- sheets[grepl("lith\\s*summary", sheets, ignore.case = TRUE)][1]
    if (is.na(summ)) stop(basename(f), " has no 'Lith Summary' tab; its tabs: ", paste(utils::head(sheets, 15), collapse = ", "), call. = FALSE)
    tabs <- setdiff(sheets, c(summ, sheets[grepl("readme|read me", sheets, ignore.case = TRUE)]))
    raw_tabs <- stats::setNames(lapply(tabs, read), tabs)
    out <- purrr::map(wb$elements, \(el) attach_tabs(parse_lith_summary(read(summ), el), lith_tab_stats(raw_tabs, el))) |>
      purrr::list_rbind()
    saveRDS(out, cache)
    out
  }) |>
    purrr::list_rbind() |>
    dplyr::filter(.data$element %in% .env$element)
}

# Header matching for the element columns. "N" is ambiguous with a record count, so a column naming nitrogen, or
# "N" with a percent sign, wins over a bare "N".
element_header <- function(h, el) {
  h <- tolower(trimws(dplyr::coalesce(h, "")))
  tok <- tolower(el)
  bare <- grepl(paste0("(^|[^a-z0-9])", tok, "([^a-z0-9]|$)"), h)
  long <- c(n = "nitrogen", s = "sulfur|sulphur")[tok]
  named <- if (!is.na(long)) grepl(long, h) else rep(FALSE, length(h))
  pct <- bare & grepl("%|pct|percent|wt", h)
  which(named | pct)[1] %|NA|% which(bare & !grepl("^n$|count|records|samples|number", h))[1]
}

# "Lith Summary": find the header row (lithology column and element column), return lithology and central value
parse_lith_summary <- function(raw, el) {
  raw <- as.data.frame(raw)
  hdr <- NA_integer_
  for (i in seq_len(min(40, nrow(raw)))) {
    h <- unlist(raw[i, ], use.names = FALSE)
    if (any(grepl("lith", h, ignore.case = TRUE)) && !is.na(element_header(h, el))) { hdr <- i; break }
  }
  if (is.na(hdr)) stop("No header row naming a lithology column and a ", el, " column in the Lith Summary tab", call. = FALSE)
  h <- unlist(raw[hdr, ], use.names = FALSE)
  lc <- which(grepl("lith", h, ignore.case = TRUE))[1]
  ec <- element_header(h, el)
  d <- raw[-seq_len(hdr), c(lc, ec)]
  names(d) <- c("lithology", "central")
  d$lithology <- trimws(d$lithology)
  d <- d[!is.na(d$lithology) & nzchar(d$lithology), ]
  dplyr::tibble(element = el, lithology = d$lithology, central = suppressWarnings(as.numeric(d$central)))
}

norm_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# Per-class tabs: the records in the element's column. Tab names are cut at 31 characters, so a truncated name
# matches the class whose name starts with it.
lith_tab_stats <- function(raw_tabs, el) {
  purrr::imap(raw_tabs, \(raw, tab) {
    raw <- as.data.frame(raw)
    hdr <- NA_integer_
    for (i in seq_len(min(40, nrow(raw)))) if (!is.na(element_header(unlist(raw[i, ], use.names = FALSE), el))) { hdr <- i; break }
    if (is.na(hdr)) return(NULL)
    v <- suppressWarnings(as.numeric(raw[-seq_len(hdr), element_header(unlist(raw[hdr, ], use.names = FALSE), el)]))
    v <- v[is.finite(v)]
    lp <- log(v[v > 0])
    dplyr::tibble(tab = tab, n = length(v), mean = if (length(v)) mean(v) else NA_real_,
                  median = if (length(v)) stats::median(v) else NA_real_,
                  sd = if (length(v) > 1) stats::sd(v) else NA_real_, sdlog = if (length(lp) > 1) stats::sd(lp) else NA_real_)
  }) |>
    purrr::list_rbind() |>
    (\(st) {
      if (is.null(st) || !nrow(st)) return(dplyr::tibble(lithology = character(), tab = character(), n = integer(), mean = numeric(),
                                                          median = numeric(), sd = numeric(), sdlog = numeric(), parse_note = character()))
      st$key <- norm_name(st$tab)
      st
    })()
}

# Join the per-tab stats to the summary classes (exact, then by truncated prefix)
attach_tabs <- function(summ, st) {
  if (!nrow(st) || !"key" %in% names(st)) return(dplyr::mutate(summ, tab = NA_character_, n = NA_integer_, mean = NA_real_,
                                                                median = NA_real_, sd = NA_real_, sdlog = NA_real_,
                                                                parse_note = "no per-class tab with this element"))
  key <- norm_name(summ$lithology)
  i <- match(key, st$key)
  pre <- is.na(i) & !is.na(key)
  i[pre] <- vapply(key[pre], \(k) {
    j <- which(nchar(st$key) >= 20 & startsWith(k, st$key))
    if (length(j)) j[which.max(nchar(st$key[j]))] else NA_integer_
  }, 1L)
  out <- dplyr::bind_cols(summ, st[i, c("tab", "n", "mean", "median", "sd", "sdlog")])
  out$parse_note <- ifelse(is.na(i), "no per-class tab found", "")
  out
}

read_modifiers <- function(m) {
  if (is.null(m)) return(NULL)
  if (is.character(m)) m <- utils::read.csv(m, stringsAsFactors = FALSE, colClasses = "character")
  m <- as.data.frame(m)
  # a filled-in rock_chem_crosswalk() file: modifier_class is the hand assignment (olson_class there is the automatic match)
  if ("modifier_class" %in% names(m)) m$olson_class <- m$modifier_class
  miss <- setdiff(c("NSACLASS", "olson_class"), names(m))
  if (length(miss)) stop("`modifiers` needs columns ", paste(miss, collapse = ", "), call. = FALSE)
  m$lithology <- dplyr::coalesce(as.character(m$lithology %||% "*"), "*")
  m$lithology[!nzchar(m$lithology)] <- "*"
  m <- m[!is.na(m$olson_class) & nzchar(trimws(m$olson_class)), ]
  m$NSACLASS <- as.character(m$NSACLASS)
  m$rationale <- m$rationale %||% NA_character_
  m[c("NSACLASS", "lithology", "olson_class", "rationale")]
}

# One row per map unit, lithology and element: the SIM 3340 terms, the rank range, the Olson class matched (by
# the most specific term, then more general ones; or by hand), and its value, records and spread
rock_crosswalk <- function(lith, classes, element, modifiers = NULL, min_n = 5) {
  lc <- intersect(paste0("LITH", 1:5), names(lith))
  terms <- as.matrix(dplyr::mutate(lith[lc], dplyr::across(dplyr::everything(), \(v) trimws(dplyr::coalesce(as.character(v), "")))))
  depth <- apply(terms, 1, \(r) max(c(0, which(nzchar(r)))))
  base <- dplyr::tibble(NSACLASS = as.character(lith$NSACLASS), row = seq_len(nrow(lith)),
                        lith_text = apply(terms, 1, \(r) paste(r[nzchar(r)], collapse = " > ")),
                        lith_term = vapply(seq_len(nrow(terms)), \(i) if (depth[i]) terms[i, depth[i]] else "", ""),
                        RANK = dplyr::coalesce(as.character(lith$RANK %||% NA), "indeterminate"))
  base$rank <- dplyr::case_when(grepl("incid", base$RANK, ignore.case = TRUE) ~ "incidental",
                                grepl("minor", base$RANK, ignore.case = TRUE) ~ "minor",
                                grepl("major", base$RANK, ignore.case = TRUE) & !grepl("indet", base$RANK, ignore.case = TRUE) ~ "major",
                                .default = "indeterminate")
  base$unconsolidated <- classify_lith(base$lith_text) == "unconsolidated" | grepl("^unconsolidated", base$lith_text, ignore.case = TRUE)
  base <- dplyr::mutate(base, surficial_unit = all(.data$unconsolidated), .by = "NSACLASS")
  purrr::map(element, \(el) {
    cl <- classes[classes$element == el, ]
    if (!nrow(cl)) stop("No lithology classes for ", el, call. = FALSE)
    key <- norm_name(cl$lithology)
    hit <- \(t) match(norm_name(t), key)
    cw <- base
    cw$class_row <- NA_integer_
    cw$match_level <- NA_integer_
    for (i in seq_len(nrow(cw))) {
      for (lv in rev(seq_len(depth[cw$row[i]]))) {
        j <- hit(terms[cw$row[i], lv])
        if (!is.na(j)) { cw$class_row[i] <- j; cw$match_level[i] <- lv; break }
      }
    }
    cw$coarsened <- !is.na(cw$match_level) & cw$match_level < depth[cw$row]
    cw$modifier <- FALSE
    cw$rationale <- NA_character_
    if (!is.null(modifiers) && nrow(modifiers)) {
      bad <- modifiers$olson_class[is.na(match(norm_name(modifiers$olson_class), key))]
      if (length(bad)) stop("`modifiers` names classes that are not in the ", el, " table: ", paste(unique(bad), collapse = ", "),
                            ". See rock_chem_lithologies().", call. = FALSE)
      for (k in seq_len(nrow(modifiers))) {
        sel <- cw$NSACLASS == modifiers$NSACLASS[k] & (modifiers$lithology[k] == "*" | norm_name(cw$lith_term) == norm_name(modifiers$lithology[k]))
        cw$class_row[sel] <- match(norm_name(modifiers$olson_class[k]), key)
        cw$modifier[sel] <- TRUE
        cw$coarsened[sel] <- FALSE
        cw$rationale[sel] <- modifiers$rationale[k]
      }
    }
    v <- cl[cw$class_row, ]
    cw |>
      dplyr::mutate(element = el, olson_class = v$lithology, central = v$central, n = dplyr::coalesce(as.integer(v$n), 0L),
                    sdlog = v$sdlog, characterized = !is.na(.data$central),
                    sparse = .data$characterized & .data$n < min_n,
                    status = dplyr::case_when(is.na(.data$class_row) ~ "no matching class", is.na(.data$central) ~ "class has no value",
                                              .data$sparse ~ "sparse class", .default = "ok")) |>
      dplyr::select(-"class_row")
  }) |>
    purrr::list_rbind()
}

# Draws for one element: unit values (class value x formation x rank shares), then catchment area means
rock_element_draws <- function(cw, area, sites, el, rank_shares, formation_sd, n, seed) {
  tag <- paste0("rock", el, "_pct")
  well <- cw$sdlog[cw$characterized & !cw$sparse & is.finite(cw$sdlog)]
  wide <- if (length(well) >= 3) unname(stats::quantile(well, 0.9)) else 1
  cw$s <- ifelse(cw$characterized & !cw$sparse & is.finite(cw$sdlog), cw$sdlog, wide)
  lim <- do.call(rbind, rank_shares[cw$rank])
  mid <- rowMeans(lim)
  # point estimate: class values, rank midpoints
  units <- cw |>
    dplyr::mutate(w = mid, wv = ifelse(.data$characterized, mid, 0)) |>
    dplyr::summarise(value = sum(.data$wv * dplyr::coalesce(.data$central, 0)) / sum(.data$wv),
                     na_w = 1 - sum(.data$wv) / sum(.data$w),
                     sparse_w = sum(.data$wv[.data$sparse]) / sum(.data$wv),
                     coarse_w = sum(.data$wv[.data$coarsened]) / sum(.data$wv),
                     mod_w = sum(.data$wv[.data$modifier]) / sum(.data$wv), .by = "NSACLASS")
  # draws: one matrix per unit (rows: draws); class shifts shared across units
  dr <- with_seed(seed + match(el, unlist(purrr::map(rock_workbooks, "elements"))), {
    cls <- unique(cw$olson_class[cw$characterized])
    zc <- matrix(stats::rnorm(n * length(cls)), n, dimnames = list(NULL, cls))
    purrr::map(split(seq_len(nrow(cw)), cw$NSACLASS), \(ii) {
      d <- cw[ii, ]
      share <- vapply(seq_along(ii), \(k) stats::runif(n, lim[ii[k], 1], lim[ii[k], 2]), numeric(n))
      share <- matrix(share, n)
      ok <- d$characterized
      if (!any(ok)) return(rep(NA_real_, n))
      val <- vapply(which(ok), \(k) {
        s <- d$s[k]
        f <- formation_sd * s
        d$central[k] * exp(s / sqrt(max(d$n[k], 1)) * zc[, d$olson_class[k]] - f^2 / 2 + f * stats::rnorm(n))
      }, numeric(n))
      val <- matrix(val, n)
      w <- share[, ok, drop = FALSE]
      rowSums(w * val) / rowSums(w)
    })
  })
  udraw <- do.call(cbind, dr)
  units$value_sd <- apply(udraw, 2, stats::sd)[match(units$NSACLASS, colnames(udraw))]
  a <- area[area$NSACLASS %in% units$NSACLASS[is.finite(units$value)], ]
  a_all <- area
  site_pt <- a |>
    dplyr::left_join(units, by = "NSACLASS") |>
    dplyr::summarise(value = sum(.data$a * .data$value) / sum(.data$a),
                     sparse = sum(.data$a * .data$sparse_w) / sum(.data$a), coarse = sum(.data$a * .data$coarse_w) / sum(.data$a),
                     modf = sum(.data$a * .data$mod_w) / sum(.data$a), .by = "site_code")
  na <- a_all |>
    dplyr::left_join(units[c("NSACLASS", "na_w")], by = "NSACLASS") |>
    dplyr::summarise(na_frac = sum(.data$a * dplyr::coalesce(.data$na_w, 1)) / sum(.data$a), .by = "site_code")
  W <- matrix(0, length(sites), ncol(udraw), dimnames = list(sites, colnames(udraw)))
  W[cbind(match(a$site_code, sites), match(a$NSACLASS, colnames(udraw)))] <- a$a
  W <- W / rowSums(W)
  sdraw <- W %*% t(udraw)                     # sites x draws
  sdraw[!is.finite(rowSums(W)), ] <- NA
  sites_out <- dplyr::tibble(site_code = sites) |>
    dplyr::left_join(site_pt, by = "site_code") |>
    dplyr::left_join(na, by = "site_code") |>
    dplyr::mutate(se = apply(sdraw, 1, stats::sd), lo = apply(sdraw, 1, stats::quantile, 0.025, na.rm = TRUE),
                  hi = apply(sdraw, 1, stats::quantile, 0.975, na.rm = TRUE),
                  na_frac = dplyr::coalesce(.data$na_frac, 1),
                  flag = flag_text(no_value = is.na(.data$value), high_na = .data$na_frac > 0.25, sparse_classes = .data$sparse > 0.5,
                                   coarsened_match = .data$coarse > 0.25))
  sites_out <- dplyr::transmute(sites_out, .data$site_code, !!tag := .data$value, !!paste0(tag, "_se") := .data$se,
                                !!paste0(tag, "_lo95") := .data$lo, !!paste0(tag, "_hi95") := .data$hi,
                                !!paste0("rock", el, "_na_frac") := .data$na_frac, !!paste0("rock", el, "_sparse_frac") := .data$sparse,
                                !!paste0("rock", el, "_coarsened_frac") := .data$coarse, !!paste0("rock", el, "_modifier_frac") := .data$modf,
                                !!paste0("rock", el, "_flag") := .data$flag)
  list(sites = sites_out, units = dplyr::mutate(units, element = el, .before = 1))
}

#' How SIM 3340 lithologies map onto the lithology geochemistry classes
#'
#' For the map units in the catchments: each lithology (SIM 3340 terms and
#' rank), the Olson class it was matched to, at which level of the term
#' hierarchy, the class value, its records and spread, and why it has no
#' value if it has none. This is the check of the vocabulary match (which
#' terms fail to match, which match only at a coarser term), and the
#' template for assigning modifier classes by hand: with `file`, it is
#' written as a CSV with empty `modifier_class` and `rationale` columns;
#' fill them from the Description of Map Units (SIM 3340 pamphlet) and pass
#' the file to [ak_rock_chem()] as `modifiers`.
#'
#' Rows whose lithology or unit name mentions carbonaceous, graphitic, coaly
#' or black shale (or calcareous, for the oxides) are marked in `suggest`:
#' those are where Olson's modifier classes matter most.
#'
#' @inheritParams ak_rock_chem
#' @param file Optional CSV path to write the template to.
#' @return A data frame, one row per map unit, lithology and element.
#' @seealso `vignette("rock-chemistry")`
#' @export
rock_chem_crosswalk <- function(catchments, element = "N", source = "olson2014", modifiers = NULL, lith_table = NULL,
                                file = NULL) {
  source <- match.arg(source)
  element <- rock_element(element)
  x <- as_catchments(catchments)
  g <- geology_tables(x)
  classes <- lith_table %||% rock_chem_lithologies(element, source)
  cw <- rock_crosswalk(g$lith[g$lith$NSACLASS %in% g$units$NSACLASS, ], classes, element, read_modifiers(modifiers))
  info <- sf::st_drop_geometry(g$units)
  keep <- intersect(c("STATE_LABEL", "STATE_UNITNAME", "STATE_SYMBOL", "AGE_RANGE"), names(info))
  info <- info[!duplicated(info$NSACLASS), c("NSACLASS", keep), drop = FALSE]
  sites <- suppressWarnings(sf::st_intersection(x["site_code"], g$units["NSACLASS"])) |>
    sf::st_drop_geometry() |>
    dplyr::summarise(sites = paste(sort(unique(.data$site_code)), collapse = ";"), .by = "NSACLASS")
  out <- cw |>
    dplyr::left_join(info, by = "NSACLASS") |>
    dplyr::left_join(sites, by = "NSACLASS")
  unit_name <- if ("STATE_UNITNAME" %in% names(out)) dplyr::coalesce(out$STATE_UNITNAME, "") else ""
  out <- out |>
    dplyr::mutate(suggest = grepl("carbonaceous|graphit|coal|black[ -]+shale|organic[ -]+rich|calcareous",
                                  paste(.data$lith_text, unit_name), ignore.case = TRUE),
                  modifier_class = ifelse(.data$modifier, .data$olson_class, NA_character_),
                  lithology = .data$lith_term) |>
    dplyr::select("element", "NSACLASS", dplyr::any_of(keep), "sites", "lith_text", "lith_term", "lithology", "RANK", "rank",
                  "olson_class", "match_level", "coarsened", "central", "n", "sdlog", "status", "suggest", "modifier",
                  "modifier_class", "rationale")
  if (!is.null(file)) {
    utils::write.csv(out, file, row.names = FALSE, na = "")
    message("Wrote ", nrow(out), " rows to ", file, ": fill modifier_class and rationale, then ak_rock_chem(x, modifiers = \"", file, "\")")
  }
  out
}
