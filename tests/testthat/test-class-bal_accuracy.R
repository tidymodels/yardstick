test_that("Calculations are correct - two class", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    bal_accuracy_vec(truth = pathology$pathology, estimate = pathology$scan),
    (sens_vec(truth = pathology$pathology, estimate = pathology$scan) +
      spec_vec(truth = pathology$pathology, estimate = pathology$scan)) /
      2
  )
})

test_that("Calculations are correct - three class", {
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

test_that("All interfaces gives the same results", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl
  path_mat <- as.matrix(path_tbl)

  exp <- bal_accuracy_vec(pathology$pathology, pathology$scan)

  expect_identical(
    bal_accuracy(path_tbl)[[".estimate"]],
    exp
  )
  expect_identical(
    bal_accuracy(path_mat)[[".estimate"]],
    exp
  )
  expect_identical(
    bal_accuracy(pathology, truth = pathology, estimate = scan)[[".estimate"]],
    exp
  )
})

test_that("Calculations handles NAs", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    bal_accuracy_vec(truth = pathology$pathology, estimate = pathology$scan_na),
    (sens_vec(truth = pathology$pathology, estimate = pathology$scan_na) +
      spec_vec(truth = pathology$pathology, estimate = pathology$scan_na)) /
      2
  )
})

test_that("Case weights calculations are correct", {
  df <- data.frame(
    truth = factor(c("x", "x", "y"), levels = c("x", "y")),
    estimate = factor(c("x", "y", "x"), levels = c("x", "y")),
    case_weights = c(1L, 1L, 2L)
  )

  # 1 correct, 2 wrong. Normally 1/3 accuracy, but one of the wrong
  # values is weighted 2x so we get 1/4.
  expect_identical(
    bal_accuracy_vec(df$truth, df$estimate, case_weights = df$case_weights),
    1 / 4
  )

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

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    bal_accuracy_vec(1, 1, na_rm = "yes")
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
