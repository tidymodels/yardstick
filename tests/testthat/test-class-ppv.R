context("PPV")

# ------------------------------------------------------------------------------

lst <- data_altman()
pathology <- lst$pathology
path_tbl <- lst$path_tbl

pred_ch <- quote(scan)

test_that('ppv', {
  expect_equal(
    ppv(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    0.87832,
    tolerance = .001
  )
  expect_equal(
    ppv(path_tbl)[[".estimate"]],
    0.87832,
    tolerance = .001
  )
  expect_equal(
    ppv(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    0.87744,
    tolerance = .001
  )
  expect_equal(
    ppv(pathology, truth = pathology, estimate = "scan", prevalence = .5)[[".estimate"]],
    0.70642,
    tolerance = .001
  )
})

# ------------------------------------------------------------------------------

multi_ex <- data_three_by_three()

test_that('Three class', {

  expect_equal(
    ppv(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(ppv_binary)
  )
  expect_equal(
    ppv(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(ppv_binary)
  )
  expect_equal(
    ppv(multi_ex, estimator = "micro")[[".estimate"]],
    precision(multi_ex, estimator = "micro")[[".estimate"]]
  )
})
