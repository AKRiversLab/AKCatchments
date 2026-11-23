# Checks that every package function called in the vignettes, README and live tests exists, is exported, and gets
# only arguments it has. Run from the package root after installing the package: Rscript dev/check_doc_calls.R
library(AKCatchments)
ns <- asNamespace("AKCatchments"); ex <- getNamespaceExports("AKCatchments")
files <- c(list.files("vignettes", "\\.Rmd$", full.names = TRUE), "README.md", "inst/live-tests/ak_live_tests.R")
bad <- character()
walk <- function(e, f) {
  if (is.call(e)) {
    fn <- e[[1]]
    nm <- if (is.name(fn)) as.character(fn) else if (is.call(fn) && identical(fn[[1]], as.name(":::")) || (is.call(fn) && identical(fn[[1]], as.name("::")))) as.character(fn[[3]]) else NULL
    pkg <- if (is.call(fn) && length(fn) == 3) as.character(fn[[2]]) else NULL
    if (!is.null(nm) && (grepl("^ak_|^catchment_|_catchments?$|^peak_ndvi|^projection_|^weather_at|^positional_|^adjust_class|^polygon_spread|^attribute_rel|^akalbers", nm) && (is.null(pkg) || pkg == "AKCatchments"))) {
      if (!exists(nm, ns)) bad <<- c(bad, paste(f, "missing function", nm)) else {
        fm <- names(formals(get(nm, ns)))
        an <- setdiff(names(as.list(e))[-1], "")
        if (!"..." %in% fm) { miss <- setdiff(an, fm); if (length(miss)) bad <<- c(bad, paste(f, nm, "unknown args:", paste(miss, collapse = ","))) }
        if (is.null(pkg) && !nm %in% ex) bad <<- c(bad, paste(f, "not exported:", nm))
      }
    }
    for (a in as.list(e)[-1]) if (!missing(a)) walk(a, f)
  }
}
for (f in files) {
  txt <- readLines(f)
  code <- if (grepl("\\.R$", f)) txt else {
    on <- FALSE; out <- character()
    for (l in txt) { if (grepl("^```\\{?r", l)) { on <- TRUE; next }; if (grepl("^```", l)) { on <- FALSE; next }; if (on) out <- c(out, l) }
    out
  }
  ex_ <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) { bad <<- c(bad, paste(f, "PARSE", conditionMessage(e))); NULL })
  for (e in ex_) walk(e, basename(f))
}
cat(unique(bad), sep = "\n"); cat("checked", length(files), "files\n")
