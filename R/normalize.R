# Normalisation helpers. Base R only. Always operate on RAW counts and return
# a new object plus a report; callers keep the raw counts for count-based methods.

#' Rarefy a features x samples count matrix to an even depth.
#'
#' @param counts features x samples integer matrix
#' @param depth  target depth; NULL uses the smallest library size
#' @param seed   RNG seed (recorded in the report)
#' @param replace sample with replacement (phyloseq's default) or not
#' @return list(counts, report) where report lists depth, seed, dropped samples/features
rarefy_counts <- function(counts, depth = NULL, seed = 123L, replace = FALSE) {
  counts <- as.matrix(counts)
  if (!is_count_data(counts)) stop("Rarefaction requires raw integer counts.")
  lib <- colSums(counts)
  if (is.null(depth)) depth <- min(lib[lib > 0])
  depth <- as.integer(depth)
  if (is.na(depth) || depth < 1) stop("Rarefaction depth must be a positive integer.")

  dropped <- names(lib)[lib < depth]
  keep <- lib >= depth
  if (!any(keep)) stop("No sample has at least ", depth, " reads.")

  old_seed <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  set.seed(seed)

  out <- counts[, keep, drop = FALSE]
  for (j in seq_len(ncol(out))) {
    reads <- rep.int(seq_len(nrow(out)), out[, j])
    pick <- sample(reads, depth, replace = replace)
    out[, j] <- tabulate(pick, nbins = nrow(out))
  }
  empty <- rowSums(out) == 0
  out <- out[!empty, , drop = FALSE]

  list(counts = out,
       report = list(method = "rarefy", depth = depth, seed = seed, replace = replace,
                     samples_in = ncol(counts), samples_out = ncol(out),
                     dropped_samples = dropped,
                     features_in = nrow(counts), features_out = nrow(out)))
}

#' Total-sum scaling (relative abundance). Samples with zero reads stay zero.
tss_counts <- function(counts) {
  counts <- as.matrix(counts)
  lib <- colSums(counts)
  lib[lib == 0] <- 1
  sweep(counts, 2, lib, "/")
}

#' Centered log-ratio transform with a pseudocount (features x samples).
clr_counts <- function(counts, pseudocount = 1) {
  counts <- as.matrix(counts) + pseudocount
  lg <- log(counts)
  sweep(lg, 2, colMeans(lg), "-")
}

#' Stop when a count-based method (richness estimators, rarefaction curves)
#' is applied to non-count data.
require_counts <- function(x, what = "this analysis") {
  if (!is_count_data(x)) {
    stop(what, " requires raw integer counts, but the data look normalized ",
         "(relative abundances or transformed). Use the raw counts.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Pick the data that count-based richness estimators (Observed, Chao1, ACE,
#' Fisher) should run on. They need integer counts, so TSS/CLR-normalised
#' data must not be used.
#' @return "final" if `final_mat` already holds counts, "counts" if only the
#'   stored count table can be used, "none" if neither is valid.
richness_source <- function(final_mat, count_mat = NULL) {
  if (is_count_data(final_mat)) return("final")
  if (!is.null(count_mat) && is_count_data(count_mat)) return("counts")
  "none"
}
