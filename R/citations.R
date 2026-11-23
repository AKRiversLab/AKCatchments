#' References, terms of use and acknowledgements for the layers you used
#'
#' Every layer comes from someone else's data set, and most providers ask to
#' be cited in a particular way; some also ask for an acknowledgement or set
#' terms on sharing derived data. `ak_citations()` gathers these for the
#' layers on your catchments (from their provenance record, see
#' [ak_provenance()]) or for layers you name, so they can go straight into a
#' paper: formatted references, a BibTeX file for a reference manager, each
#' provider's terms and the acknowledgement text they ask for.
#' `vignette("data-sources")` sets out the terms for every layer.
#'
#' References to live services (web services and APIs without a fixed
#' release) carry an access date: the date the layer was added to `x`, or
#' today for layers named in `layers`. Check the dates, the providers' names
#' in the AKTEMP reference, and the terms of anything you redistribute.
#'
#' @param x Optional catchments or layer table made with the package; the
#'   layers in its provenance record are cited.
#' @param layers Optional layers to cite as well, as `"layer"` (its default
#'   variant) or `"layer/variant"`, e.g. `c("permafrost/pastick2015",
#'   "glaciers")`; see [ak_layers()]. With neither `x` nor `layers`, every
#'   layer.
#' @param bibtex Optional path of a BibTeX (`.bib`) file to write the
#'   references to, each once.
#' @return A tibble with one row per layer and reference, AKCatchments itself
#'   first: `layer`, `variant`, `product`, `key` (the BibTeX key),
#'   `reference` (formatted text), `doi`, `url`, `terms` (terms of use, in
#'   short), `terms_url`, and `acknowledgement` (text the provider asks for,
#'   if any). Invisibly if `bibtex` is given.
#' @examples
#' ak_citations(layers = c("permafrost", "glaciers"))
#' \dontrun{
#' refs <- ak_citations(x, bibtex = "catchment_data.bib")
#' unique(refs$reference)
#' unique(refs$acknowledgement[nzchar(refs$acknowledgement)])
#' }
#' @seealso `vignette("data-sources")`, [ak_layers()], [ak_provenance()]
#' @export
ak_citations <- function(x = NULL, layers = NULL, bibtex = NULL) {
  prov <- if (!is.null(x)) ak_provenance(x) else provenance_row(NULL)
  named <- purrr::map(layers, \(l) {
    parts <- strsplit(l, "/", fixed = TRUE)[[1]]
    row <- layer_row(parts[1], if (length(parts) > 1) parts[2])
    dplyr::tibble(layer = row$layer, variant = row$variant)
  })
  everything <- if (is.null(x) && is.null(layers)) dplyr::select(ak_layers(), "layer", "variant")
  used <- dplyr::bind_rows(dplyr::select(prov, "layer", "variant", "added_utc"), named, everything) |>
    dplyr::mutate(accessed = as.Date(substr(.data$added_utc, 1, 10))) |>
    dplyr::summarise(accessed = dplyr::first(sort(.data$accessed)), .by = c("layer", "variant")) |>
    dplyr::mutate(accessed = dplyr::coalesce(.data$accessed, Sys.Date()))
  refs <- utils::read.csv(system.file("extdata", "references.csv", package = "AKCatchments"), colClasses = "character",
                          encoding = "UTF-8")
  rows <- sources_table() |>
    dplyr::select(layer = "id", "variant", "product", "references", "terms", "terms_url", "acknowledgement") |>
    dplyr::inner_join(used, by = c("layer", "variant")) |>
    tidyr::separate_longer_delim("references", "; ") |>
    dplyr::rename(key = "references") |>
    dplyr::mutate(acknowledgement = purrr::map2_chr(.data$acknowledgement, .data$accessed, \(a, d) sub("<date>", format(d), a, fixed = TRUE)))
  entries <- purrr::map2(rows$key, rows$accessed, \(k, d) reference_entry(refs[refs$key == k, ], d))
  pkg <- utils::citation("AKCatchments")
  pkg$key <- "akcatchments"
  out <- dplyr::bind_rows(
    dplyr::tibble(layer = "AKCatchments", variant = "", product = "R package", key = "akcatchments",
                  terms = "MIT licence", terms_url = "", acknowledgement = ""),
    dplyr::select(rows, -"accessed")
  ) |>
    dplyr::mutate(reference = vapply(c(list(pkg), entries), reference_text, ""),
                  doi = vapply(c(list(pkg), entries), \(e) e$doi %||% "", ""),
                  url = vapply(c(list(pkg), entries), \(e) e$url %||% "", ""),
                  .after = "key")
  if (is.null(bibtex)) return(out)
  all_entries <- c(list(pkg), entries)[!duplicated(out$key)]
  writeLines(unlist(lapply(all_entries, \(e) c(utils::toBibtex(e), ""))), bibtex, useBytes = TRUE)
  invisible(out)
}

# A references.csv row as a bibentry. Authors are "Family, Given; ..." with {Corporate names} in braces;
# live services have no year and are cited with the access date.
reference_entry <- function(ref, accessed) {
  authors <- trimws(strsplit(ref$author, ";", fixed = TRUE)[[1]])
  people <- do.call(c, lapply(authors, \(a) {
    if (a == "others") return(utils::person("others"))
    if (startsWith(a, "{")) return(utils::person(family = gsub("^\\{|\\}$", "", a)))
    name <- strsplit(a, ", ", fixed = TRUE)[[1]]
    utils::person(given = strsplit(name[2], " ", fixed = TRUE)[[1]], family = name[1])
  }))
  if (ref$accessed == "yes") {
    if (!nzchar(ref$year)) ref$year <- format(accessed, "%Y")
    ref$note <- trimws(paste0(ref$note, if (nzchar(ref$note)) ". ", "Accessed ", format(accessed)))
  }
  fields <- unlist(ref[c("year", "title", "journal", "volume", "pages", "booktitle", "institution", "type", "number",
                         "howpublished", "publisher", "doi", "url", "note")])
  do.call(utils::bibentry, c(list(bibtype = ref$bibtype, key = ref$key, author = people), as.list(fields[nzchar(fields)])))
}

# Plain text of a bibentry: R's text style marks italics with _..._ and bold with *...*, and wraps lines
reference_text <- function(entry) {
  format(entry, style = "text") |>
    paste(collapse = " ") |>
    gsub(pattern = "\\s*\n\\s*", replacement = " ") |>
    gsub(pattern = "(^|[\\s(])_(.+?)_(?=[\\s.,;:)]|$)", replacement = "\\1\\2", perl = TRUE) |>
    gsub(pattern = "\\*([^*]+)\\*", replacement = "\\1")
}
