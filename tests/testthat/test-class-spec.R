context("Specificity")

# ------------------------------------------------------------------------------

lst <- data_altman()
pathology <- lst$pathology
path_tbl <- lst$path_tbl

pred_ch <- quote(scan)

test_that('Two class', {
  expect_equal(
    spec(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    54/86
  )
  expect_equal(
    spec(path_tbl)[[".estimate"]],
    54/86
  )
  expect_equal(
    spec(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    53/85
  )
  expect_equal(
    spec(as.matrix(path_tbl))[[".estimate"]],
    54/86
  )
})

# ------------------------------------------------------------------------------

multi_ex <- data_three_by_three()
micro <- data_three_by_three_micro()

test_that('Three class', {
  expect_equal(
    spec(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(spec_binary)
  )
  expect_equal(
    spec(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(spec_binary)
  )
  expect_equal(
    spec(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro, sum(tn) / sum(tn + fn))
  )
})
