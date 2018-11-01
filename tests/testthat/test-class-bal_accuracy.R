context("Balanced Accuracy")

# ------------------------------------------------------------------------------

lst <- data_altman()
pathology <- lst$pathology
path_tbl <- lst$path_tbl

pred_ch <- quote(scan)

test_that('Two class', {
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

# ------------------------------------------------------------------------------

multi_ex <- data_three_by_three()
micro <- data_three_by_three_micro()

test_that('Three class', {

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
