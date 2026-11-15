mk <- function() {
  counts <- matrix(c(10L, 0L, 5L, 3L, 8L, 2L, 7L, 1L, 4L, 9L, 6L, 2L), nrow = 3,
                   dimnames = list(paste0("f", 1:3), paste0("s", 1:4)))
  meta <- data.frame(grp = c("a", "a", "b", "b"), row.names = paste0("s", 1:4))
  list(counts = counts, meta = meta)
}

test_that("valid data passes and orientation is auto-detected", {
  d <- mk()
  r <- validate_inputs(d$counts, d$meta, group = "grp", min_group_size = 2)
  expect_true(r$ok)
  expect_true(r$taxa_are_rows)
  r2 <- validate_inputs(t(d$counts), d$meta, group = "grp", min_group_size = 2)
  expect_true(r2$ok)
  expect_false(r2$taxa_are_rows)
  expect_equal(dim(r2$counts), dim(d$counts)) # returned features x samples
})

test_that("sample ID mismatches are errors / warnings", {
  d <- mk()
  colnames(d$counts)[1] <- "zzz"
  r <- validate_inputs(d$counts, d$meta)
  expect_false(r$ok)
  expect_match(paste(r$errors, collapse = " "), "not metadata")
  expect_match(paste(r$warnings, collapse = " "), "not counts")
})

test_that("NA, negative, non-integer values are caught", {
  d <- mk(); d$counts[1, 1] <- NA
  expect_false(validate_inputs(d$counts, d$meta)$ok)
  d <- mk(); d$counts[1, 1] <- -1L
  expect_false(validate_inputs(d$counts, d$meta)$ok)
  d <- mk(); d$counts <- d$counts / colSums(d$counts)[col(d$counts)]
  r <- validate_inputs(d$counts, d$meta)
  expect_true(r$ok)
  expect_match(paste(r$warnings, collapse = " "), "integer-valued")
})

test_that("small groups warn, single-level grouping errors", {
  d <- mk()
  r <- validate_inputs(d$counts, d$meta, group = "grp", min_group_size = 3)
  expect_match(paste(r$warnings, collapse = " "), "fewer than 3")
  d$meta$grp <- "a"
  expect_false(validate_inputs(d$counts, d$meta, group = "grp")$ok)
})

test_that("duplicate IDs are errors", {
  d <- mk(); colnames(d$counts)[2] <- "s1"
  expect_false(validate_inputs(d$counts, d$meta)$ok)
})
