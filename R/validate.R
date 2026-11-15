# Input validation for amplicon count data.
# Base R only, so it is unit-testable without phyloseq or Shiny.

#' Validate a count table and sample metadata.
#'
#' @param counts numeric matrix/data.frame of counts (features x samples or samples x features)
#' @param meta   data.frame of sample metadata; row names are sample IDs
#' @param taxa_are_rows TRUE, FALSE, or NA to auto-detect orientation from sample IDs in `meta`
#' @param group  optional name of a grouping column in `meta`
#' @param min_group_size minimum samples per group (default 3)
#' @return list(ok, errors, warnings, counts, taxa_are_rows, library_sizes);
#'   `counts` is returned as features x samples.
validate_inputs <- function(counts, meta, taxa_are_rows = NA, group = NULL,
                            min_group_size = 3L) {
  errors <- character(0)
  warns <- character(0)
  counts <- as.matrix(counts)
  meta <- as.data.frame(meta)
  sample_ids <- rownames(meta)

  if (!is.numeric(counts)) {
    errors <- c(errors, "Count table contains non-numeric values.")
    return(list(ok = FALSE, errors = errors, warnings = warns, counts = counts,
                taxa_are_rows = taxa_are_rows, library_sizes = NULL))
  }

  # Orientation: samples must match metadata row names
  if (is.na(taxa_are_rows)) {
    cols_match <- sum(colnames(counts) %in% sample_ids)
    rows_match <- sum(rownames(counts) %in% sample_ids)
    if (cols_match == 0 && rows_match == 0) {
      errors <- c(errors, "No sample IDs in the count table match the metadata row names.")
      return(list(ok = FALSE, errors = errors, warnings = warns, counts = counts,
                  taxa_are_rows = NA, library_sizes = NULL))
    }
    taxa_are_rows <- cols_match >= rows_match
  }
  if (!taxa_are_rows) counts <- t(counts)

  if (anyDuplicated(colnames(counts))) {
    errors <- c(errors, paste0("Duplicate sample IDs in count table: ",
                               paste(unique(colnames(counts)[duplicated(colnames(counts))]), collapse = ", ")))
  }
  if (anyDuplicated(rownames(counts))) {
    errors <- c(errors, paste0("Duplicate feature IDs in count table: ",
                               paste(unique(rownames(counts)[duplicated(rownames(counts))]), collapse = ", ")))
  }
  if (anyDuplicated(sample_ids)) {
    errors <- c(errors, "Duplicate sample IDs in metadata.")
  }

  # Sample-ID concordance
  only_counts <- setdiff(colnames(counts), sample_ids)
  only_meta <- setdiff(sample_ids, colnames(counts))
  if (length(only_counts)) {
    errors <- c(errors, paste0(length(only_counts), " sample(s) in counts but not metadata: ",
                               paste(utils::head(only_counts, 5), collapse = ", ")))
  }
  if (length(only_meta)) {
    warns <- c(warns, paste0(length(only_meta), " sample(s) in metadata but not counts (will be dropped): ",
                             paste(utils::head(only_meta, 5), collapse = ", ")))
  }

  # Value checks
  if (anyNA(counts)) errors <- c(errors, "Count table contains NA values.")
  if (any(counts < 0, na.rm = TRUE)) errors <- c(errors, "Count table contains negative values.")
  if (!anyNA(counts) && any(abs(counts - round(counts)) > 1e-8)) {
    warns <- c(warns, paste0("Count table is not integer-valued (looks like relative abundances or ",
                             "normalized data). Richness estimators and rarefaction require raw counts."))
  }

  lib <- colSums(counts, na.rm = TRUE)
  if (any(lib == 0)) {
    warns <- c(warns, paste0(sum(lib == 0), " sample(s) have zero total reads: ",
                             paste(utils::head(names(lib)[lib == 0], 5), collapse = ", ")))
  }

  # Group-size check
  if (!is.null(group)) {
    if (!group %in% colnames(meta)) {
      errors <- c(errors, paste0("Grouping column '", group, "' not found in metadata."))
    } else {
      g <- meta[[group]][match(colnames(counts), sample_ids)]
      if (anyNA(g)) warns <- c(warns, "Some samples have a missing value in the grouping column.")
      sizes <- table(g)
      if (length(sizes) < 2) {
        errors <- c(errors, "Grouping variable needs at least two levels.")
      } else if (any(sizes < min_group_size)) {
        warns <- c(warns, paste0("Groups with fewer than ", min_group_size, " samples: ",
                                 paste(names(sizes)[sizes < min_group_size], collapse = ", "),
                                 ". Permutation tests will have little power."))
      }
    }
  }

  list(ok = length(errors) == 0, errors = errors, warnings = warns,
       counts = counts, taxa_are_rows = taxa_are_rows, library_sizes = lib)
}

#' Does a count matrix look like raw integer counts?
is_count_data <- function(x) {
  x <- as.matrix(x)
  is.numeric(x) && !anyNA(x) && all(x >= 0) && all(abs(x - round(x)) < 1e-8)
}
