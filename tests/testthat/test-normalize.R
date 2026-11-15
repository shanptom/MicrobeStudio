cts <- matrix(c(50L, 30L, 20L, 0L,
                10L, 10L, 5L, 0L,
                100L, 60L, 40L, 1L), nrow = 4,
              dimnames = list(paste0("f", 1:4), paste0("s", 1:3)))

test_that("rarefy equalises depth, is seeded and reports drops", {
  r1 <- rarefy_counts(cts, depth = 25, seed = 1)
  r2 <- rarefy_counts(cts, depth = 25, seed = 1)
  expect_true(all(colSums(r1$counts) == 25))
  expect_identical(r1$counts, r2$counts)
  r3 <- rarefy_counts(cts, depth = 150, seed = 1)
  expect_equal(r3$report$dropped_samples, c("s1", "s2"))
  expect_equal(ncol(r3$counts), 1)
})

test_that("rarefy defaults to the minimum depth and rejects non-counts", {
  r <- rarefy_counts(cts)
  expect_equal(unique(colSums(r$counts)), min(colSums(cts)))
  expect_error(rarefy_counts(cts / 10), "raw integer counts")
  expect_error(rarefy_counts(cts, depth = 0), "positive")
})

test_that("rarefy does not disturb the caller's RNG state", {
  set.seed(42); a <- runif(1)
  set.seed(42); rarefy_counts(cts, depth = 10, seed = 5); b <- runif(1)
  expect_equal(a, b)
})

test_that("TSS sums to one and handles empty samples", {
  m <- cts; m[, 1] <- 0L
  t <- tss_counts(m)
  expect_equal(unname(colSums(t))[2:3], c(1, 1))
  expect_equal(unname(colSums(t))[1], 0)
})

test_that("CLR columns are centred", {
  expect_equal(unname(colSums(clr_counts(cts))), rep(0, 3), tolerance = 1e-10)
})

test_that("require_counts blocks normalised data", {
  expect_silent(require_counts(cts))
  expect_error(require_counts(tss_counts(cts), "Chao1"), "raw integer counts")
})

test_that("richness_source avoids normalised data", {
  expect_equal(richness_source(cts), "final")
  expect_equal(richness_source(tss_counts(cts), cts), "counts")
  expect_equal(richness_source(tss_counts(cts), NULL), "none")
  expect_equal(richness_source(tss_counts(cts), tss_counts(cts)), "none")
})
