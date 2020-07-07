context("Detection prevalence")

# ------------------------------------------------------------------------------

test_that('Two class', {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    detection_prevalence(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    ( 231 + 32 ) / 344
  )
  expect_equal(
    detection_prevalence(path_tbl)[[".estimate"]],
    ( 231 + 32 ) / 344
  )
  expect_equal(
    detection_prevalence(pathology, pathology, scan)[[".estimate"]],
    ( 231 + 32 ) / 344
  )
})

test_that("`estimator = 'binary_last'` works", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    detection_prevalence_vec(pathology$pathology, pathology$scan, estimator = "binary_last"),
    1 - detection_prevalence_vec(pathology$pathology, pathology$scan, estimator = "binary")
  )
})

# ------------------------------------------------------------------------------

test_that('Three class', {
  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()

  expect_equal(
    detection_prevalence(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(detection_prevalence_binary)
  )
  expect_equal(
    detection_prevalence(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(detection_prevalence_binary)
  )
  expect_equal(
    detection_prevalence(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro, sum(tp + fp) / sum(n + p))
  )
})
