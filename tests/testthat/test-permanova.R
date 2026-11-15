skip_if_not_installed("vegan"); skip_if_not_installed("permute")

set.seed(1)
x <- rbind(matrix(rpois(60, 20), 6), matrix(rpois(60, 5), 6), matrix(rpois(60, 12), 6))
rownames(x) <- paste0("s", 1:18)
grp <- rep(c("a", "b", "c"), each = 6)
d <- vegan::vegdist(x, "bray")

test_that("global PERMANOVA matches vegan::adonis2 and is reproducible", {
  r <- run_permanova(d, grp, permutations = 199, seed = 7)
  set.seed(7)
  ref <- as.data.frame(vegan::adonis2(d ~ g, data = data.frame(g = factor(grp)), permutations = 199))
  expect_equal(r$table$F[1], ref$F[1])
  expect_equal(r$table$R2[1], ref$R2[1])
  expect_equal(r$table$p.value[1], ref[["Pr(>F)"]][1])
  r2 <- run_permanova(d, grp, permutations = 199, seed = 7)
  expect_identical(r$table, r2$table)
  expect_equal(r$params$seed, 7)
})

test_that("pairwise rows exist and only they are adjusted", {
  r <- run_permanova(d, grp, permutations = 199, p_adjust = "bonferroni")
  expect_equal(nrow(r$table), 4)               # global + 3 pairs
  expect_true(is.na(r$table$p.adjusted[1]))
  pw <- r$table[-1, ]
  expect_equal(pw$p.adjusted, pmin(1, pw$p.value * 3))
})

test_that("two groups give a single global row", {
  keep <- grp != "c"
  r <- run_permanova(stats::as.dist(as.matrix(d)[keep, keep]), grp[keep], permutations = 99)
  expect_equal(nrow(r$table), 1)
})

test_that("PERMDISP is reported and strata run", {
  r <- run_permanova(d, grp, strata = rep(1:2, 9), permutations = 99, permdisp = TRUE)
  expect_true(is.finite(r$permdisp$F))
  expect_true(r$permdisp$p.value >= 0 && r$permdisp$p.value <= 1)
  expect_true(r$params$strata)
})

test_that("bad input errors clearly", {
  expect_error(run_permanova(d, rep("a", 18)), "two levels")
  expect_error(run_permanova(d, grp[-1]), "one value per sample")
})

test_that("jaccard uses presence/absence", {
  skip_if_not_installed("phyloseq")
  otu <- phyloseq::otu_table(t(x), taxa_are_rows = TRUE)
  ps <- phyloseq::phyloseq(otu)
  expect_equal(as.vector(beta_distance(ps, "jaccard")), as.vector(vegan::vegdist(x, "jaccard", binary = TRUE)))
  expect_false(isTRUE(all.equal(as.vector(beta_distance(ps, "jaccard")), as.vector(vegan::vegdist(x, "jaccard")))))
})
