test_that("Two class", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    bal_accuracy(pathology, truth = "pathology", estimate = "scan")[[
      ".estimate"
    ]],
    (sens(path_tbl)[[".estimate"]] + spec(path_tbl)[[".estimate"]]) / 2
  )
  expect_equal(
    bal_accuracy(path_tbl)[[".estimate"]],
    (sens(path_tbl)[[".estimate"]] + spec(path_tbl)[[".estimate"]]) / 2
  )
  expect_equal(
    bal_accuracy(pathology, pathology, scan)[[".estimate"]],
    (sens(path_tbl)[[".estimate"]] + spec(path_tbl)[[".estimate"]]) / 2
  )
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    bal_accuracy_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    bal_accuracy_vec(df$pathology, df$scan, case_weights = freq_wgt)
  )
})

test_that("`event_level = 'second'` should be identical to 'first'", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_identical(
    bal_accuracy_vec(
      pathology$pathology,
      pathology$scan,
      event_level = "first"
    ),
    bal_accuracy_vec(
      pathology$pathology,
      pathology$scan,
      event_level = "second"
    )
  )
})

# ------------------------------------------------------------------------------

test_that("Three class", {
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

test_that("work with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  cp_estimate <- probably::as_class_pred(two_class_example$predicted, which = 2)

  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  fct_estimate <- two_class_example$predicted
  fct_estimate[2] <- NA

  expect_identical(
    bal_accuracy_vec(fct_truth, cp_estimate),
    bal_accuracy_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    bal_accuracy_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    bal_accuracy_vec(cp_truth, cp_estimate)
  )
})

# ------------------------------------------------------------------------------

test_that("Two class weighted - sklearn equivalent", {
  py_res <- read_pydata("py-bal-accuracy")
  r_metric <- bal_accuracy

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, predicted, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )
})
