# PERMANOVA / PERMDISP on a precomputed distance matrix (needs vegan, permute).

#' Global + pairwise PERMANOVA and PERMDISP.
#'
#' @param d            a `dist` object (sample labels = names used for `group`)
#' @param group        grouping vector, same order as labels(d)
#' @param strata       optional blocking vector: permutations are restricted within blocks
#' @param permutations number of permutations
#' @param seed         RNG seed, set before every permutation test and returned
#' @param p_adjust     method for `stats::p.adjust`, applied to the pairwise tests only
#' @param pairwise     also run all pairwise comparisons
#' @param permdisp     also run the test of homogeneity of dispersions (betadisper)
#' @return list(table, permdisp, params). `table` has a global row (p.adjusted = NA)
#'   and one row per pair; columns Groups, F, R2, p.value, p.adjusted, n.
run_permanova <- function(d, group, strata = NULL, permutations = 999L, seed = 123L,
                          p_adjust = "BH", pairwise = TRUE, permdisp = TRUE) {
  if (!requireNamespace("vegan", quietly = TRUE)) stop("vegan is required.")
  group <- factor(group)
  if (nlevels(droplevels(group)) < 2) stop("Grouping variable must have at least two levels.")
  if (length(group) != attr(d, "Size")) stop("`group` must have one value per sample in `d`.")
  if (!is.null(strata) && length(strata) != length(group)) stop("`strata` must have one value per sample.")
  permutations <- as.integer(permutations)
  m <- as.matrix(d)

  perm_ctrl <- function(idx) {
    if (is.null(strata)) permutations
    else permute::how(nperm = permutations, blocks = factor(strata[idx]))
  }
  one_test <- function(idx, label) {
    set.seed(seed)
    di <- stats::as.dist(m[idx, idx, drop = FALSE])
    df <- data.frame(g = droplevels(group[idx]))
    res <- as.data.frame(vegan::adonis2(di ~ g, data = df, permutations = perm_ctrl(idx)))
    data.frame(Groups = label, F = res[["F"]][1], R2 = res[["R2"]][1],
               p.value = res[["Pr(>F)"]][1], n = length(idx), stringsAsFactors = FALSE)
  }

  keep <- which(!is.na(group))
  global <- one_test(keep, "All groups (global)")
  global$p.adjusted <- NA_real_
  tab <- global

  if (pairwise && nlevels(droplevels(group[keep])) > 2) {
    lv <- levels(droplevels(group[keep]))
    rows <- lapply(utils::combn(lv, 2, simplify = FALSE), function(pr) {
      one_test(which(group %in% pr), paste(pr[1], "vs", pr[2]))
    })
    pw <- do.call(rbind, rows)
    pw$p.adjusted <- stats::p.adjust(pw$p.value, method = p_adjust)
    tab <- rbind(global, pw)
  }
  rownames(tab) <- NULL

  pd <- NULL
  if (permdisp) {
    set.seed(seed)
    bd <- vegan::betadisper(stats::as.dist(m[keep, keep]), droplevels(group[keep]))
    pt <- vegan::permutest(bd, permutations = perm_ctrl(keep))
    pd <- list(F = pt$tab[1, "F"], p.value = pt$tab[1, "Pr(>F)"])
  }

  list(table = tab, permdisp = pd,
       params = list(permutations = permutations, seed = seed, p_adjust = p_adjust,
                     strata = !is.null(strata), vegan = as.character(utils::packageVersion("vegan"))))
}

#' Distance used by the app. Jaccard is computed on presence/absence
#' (binary = TRUE), which is what the interface describes; vegan's default
#' would otherwise give the quantitative (abundance-weighted) variant.
beta_distance <- function(ps, method) {
  if (identical(method, "jaccard")) {
    return(stats::as.dist(phyloseq::distance(ps, method = "jaccard", binary = TRUE)))
  }
  stats::as.dist(phyloseq::distance(ps, method = method))
}
