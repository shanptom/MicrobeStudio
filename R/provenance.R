# Provenance: record what was run so exports are reproducible.

#' Build a provenance record to embed in exports.
#' @param params named list of analysis parameters (including seeds)
#' @param input_files character vector of input file paths to hash (optional)
make_provenance <- function(params = list(), input_files = character(0),
                            app_version = "dev") {
  hashes <- if (length(input_files)) {
    stats::setNames(unname(tools::md5sum(input_files)), basename(input_files))
  } else character(0)
  pkgs <- c("phyloseq", "vegan", "microeco", "xgboost")
  vers <- vapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
  }, character(1))
  list(
    app = "MicrobeStudio", app_version = app_version,
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    r_version = R.version.string,
    platform = R.version$platform,
    package_versions = as.list(vers),
    params = params,
    input_md5 = as.list(hashes)
  )
}

#' Serialize a provenance record as plain-text lines (for a README/report or CSV footer).
format_provenance <- function(prov) {
  flat <- function(x, prefix = "") {
    if (is.list(x)) {
      unlist(lapply(names(x), function(n) flat(x[[n]], paste0(prefix, n, "."))), use.names = FALSE)
    } else paste0(sub("\\.$", "", prefix), ": ", paste(format(x), collapse = ", "))
  }
  flat(prov)
}
