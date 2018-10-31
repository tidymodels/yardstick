context("NPV")

# ------------------------------------------------------------------------------

lst <- data_altman()
pathology <- lst$pathology
path_tbl <- lst$path_tbl

pred_ch <- quote(scan)

test_that('npv', {
  expect_equal(
    npv(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    2/3,
    tolerance = .001
  )
  expect_equal(
    npv(path_tbl)[[".estimate"]],
    2/3,
    tolerance = .001
  )
  expect_equal(
    npv(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    0.67088,
    tolerance = .001
  )
  expect_equal(
    npv(pathology, truth = pathology, estimate = "scan", prevalence = .5)[[".estimate"]],
    0.85714,
    tolerance = .001
  )
})

# ------------------------------------------------------------------------------

multi_ex <- data_three_by_three()

test_that('Three class', {

  expect_equal(
    npv(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(npv_binary)
  )
  expect_equal(
    npv(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(npv_binary)
  )
  # good micro test?
})
