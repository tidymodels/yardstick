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
micro <- data_three_by_three_micro()
micro$prev <- (micro$tp + micro$fn) / (micro$p + micro$n)

test_that('Three class', {

  expect_equal(
    npv(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(npv_binary)
  )
  expect_equal(
    npv(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(npv_binary)
  )
  expect_equal(
    npv(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro,
         (sum(tn) / sum(n) * sum((1 - prev))) /
         ( (1 - sum(tp) / sum(p)) * sum(prev) + (sum(tn) / sum(n) * sum((1 - prev))) )
        )
  )
  # Prevalence defined by the user. Defined once for all levels?
  expect_equal(
    npv(multi_ex, estimator = "micro", prevalence = .4)[[".estimate"]],
    with(micro,
         (sum(tn) / sum(n) * sum((1 - .4))) /
           ( (1 - sum(tp) / sum(p)) * sum(.4) + (sum(tn) / sum(n) * sum((1 - .4))) )
    )
  )
})
