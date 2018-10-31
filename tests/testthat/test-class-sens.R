context("Sensitivity")

# ------------------------------------------------------------------------------

lst <- data_altman()
pathology <- lst$pathology
path_tbl <- lst$path_tbl

pred_ch <- quote(scan)

test_that('Two class', {
  expect_equal(
    sens(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, estimate = scan, truth = pathology)[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, pathology, !! pred_ch)[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, pathology, scan)[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(path_tbl)[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    230/256
  )
  expect_equal(
    sens(as.matrix(path_tbl))[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, pathology, scan_na, na_rm = FALSE)[[".estimate"]],
    NA_real_
  )
})

# ------------------------------------------------------------------------------

multi_ex <- data_three_by_three()

test_that('Three class', {
  # sens = recall
  expect_equal(
    sens(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(recall_binary)
  )
  expect_equal(
    sens(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(recall_binary)
  )

  # Micro?
})
