test_that("Two class", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    detection_prevalence(pathology, truth = "pathology", estimate = "scan")[[
      ".estimate"
    ]],
    (231 + 32) / 344
  )
  expect_equal(
    detection_prevalence(path_tbl)[[".estimate"]],
    (231 + 32) / 344
  )
  expect_equal(
    detection_prevalence(pathology, pathology, scan)[[".estimate"]],
    (231 + 32) / 344
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    detection_prevalence_vec(
      pathology$pathology,
      pathology$scan,
      event_level = "second"
    ),
    1 -
      detection_prevalence_vec(
        pathology$pathology,
        pathology$scan,
        event_level = "first"
      )
  )
})

# ------------------------------------------------------------------------------

test_that("Three class", {
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

# ------------------------------------------------------------------------------

test_that("two class with case weights is correct", {
  df <- data.frame(
    truth = factor(c("x", "x", "y"), levels = c("x", "y")),
    estimate = factor(c("x", "y", "x"), levels = c("x", "y")),
    case_weights = c(1L, 1L, 2L)
  )

  expect_identical(
    detection_prevalence(df, truth, estimate, case_weights = case_weights)[[
      ".estimate"
    ]],
    3 / 4
  )
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    detection_prevalence_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    detection_prevalence_vec(df$pathology, df$scan, case_weights = freq_wgt)
  )
})

test_that("work with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  cp_estimate <- probably::as_class_pred(two_class_example$predicted, which = 2)

  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  fct_estimate <- two_class_example$predicted
  fct_estimate[2] <- NA

  expect_identical(
    detection_prevalence_vec(fct_truth, cp_estimate),
    detection_prevalence_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    detection_prevalence_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    detection_prevalence_vec(cp_truth, cp_estimate)
  )
})
