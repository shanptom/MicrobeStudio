test_that("provenance records params, seeds, versions and input hashes", {
  f <- tempfile(fileext = ".csv"); writeLines("a,b\n1,2", f)
  p <- make_provenance(list(seed = 123, depth = 1000), input_files = f)
  expect_equal(p$params$seed, 123)
  expect_equal(names(p$input_md5), basename(f))
  expect_match(p$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
  lines <- format_provenance(p)
  expect_true(any(grepl("params.seed: 123", lines)))
})
