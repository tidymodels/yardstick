context("Balanced Accuracy")

# ------------------------------------------------------------------------------

test_that('Two class', {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    bal_accuracy(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    ( sens(path_tbl)[[".estimate"]] + spec(path_tbl)[[".estimate"]] )/2
  )
  expect_equal(
    bal_accuracy(path_tbl)[[".estimate"]],
    ( sens(path_tbl)[[".estimate"]] + spec(path_tbl)[[".estimate"]] )/2
  )
  expect_equal(
    bal_accuracy(pathology, pathology, scan)[[".estimate"]],
    ( sens(path_tbl)[[".estimate"]] + spec(path_tbl)[[".estimate"]] )/2
  )
})

test_that("`estimator = 'binary_last'` should be identical to 'binary'", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_identical(
    bal_accuracy_vec(pathology$pathology, pathology$scan, estimator = "binary"),
    bal_accuracy_vec(pathology$pathology, pathology$scan, estimator = "binary_last")
  )
})

# ------------------------------------------------------------------------------

test_that('Three class', {
  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()

  expect_equal(
    bal_accuracy(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(bal_accuracy_binary)
  )
  expect_equal(
    bal_accuracy(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(bal_accuracy_binary)
  )
  expect_equal(
    bal_accuracy(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro, (sum(tp) / sum(p) + sum(tn) / sum(n)) / 2)
  )
})
