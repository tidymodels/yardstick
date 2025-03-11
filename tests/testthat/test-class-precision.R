test_that("Two class - Powers paper", {
  lst <- data_powers()
  tabl_2_1 <- lst$tabl_2_1
  df_2_1 <- lst$df_2_1

  expect_equal(
    precision(df_2_1, truth = "truth", estimate = "prediction")[[".estimate"]],
    30 / 42
  )
  expect_equal(
    precision(tabl_2_1)[[".estimate"]],
    30 / 42
  )
  expect_equal(
    precision(df_2_1, truth = truth, estimate = pred_na)[[".estimate"]],
    26 / 37
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    precision_vec(df$pathology, df$scan),
    precision_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that("Binary `precision()` returns `NA` with a warning when undefined (tp + fp = 0) (#98)", {
  truth <- factor("a", levels = c("a", "b"))
  estimate <- factor("b", levels = c("a", "b"))

  expect_snapshot(
    out <- precision_vec(truth, estimate)
  )

  expect_identical(out, NA_real_)
})

test_that("Multiclass `precision()` returns averaged value with `NA`s removed + a warning when undefined (tp + fp = 0) (#98)", {
  levels <- c("a", "b", "c", "d")

  # When `d` is the event we get precision = 0 = 0 / (0 + 3)
  # When `a` is the event we get a warning
  # When `b` is the event we get a warning
  # When `c` is the event we get a warning
  truth <- factor(c("a", "b", "c"), levels = levels)
  estimate <- factor(rep("d", 3), levels)

  expect_snapshot(out <- precision_vec(truth, estimate))
  expect_identical(out, 0)

  # When `d` is the event we get precision = 0
  # When `a` is the event we get precision = 1
  # When `b` is the event we get precision = 0
  # When `c` is the event we get a warning
  truth <- factor(c("a", "b", "c"), levels = levels)
  estimate <- factor(c("a", "d", "b"), levels)

  expect_snapshot(out <- precision_vec(truth, estimate))
  expect_identical(out, 1 / 3)
})

test_that("`NA` is still returned if there are some undefined precision values but `na.rm = FALSE`", {
  levels <- c("a", "b", "c", "d")
  truth <- factor(c("a", "b", "c"), levels = levels)
  estimate <- factor(c("a", NA, "c"), levels)
  expect_equal(precision_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(precision_vec(truth, estimate, na_rm = FALSE), NA)
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    precision_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    precision_vec(df$pathology, df$scan, case_weights = freq_wgt)
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
    precision_vec(fct_truth, cp_estimate),
    precision_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    precision_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    precision_vec(cp_truth, cp_estimate)
  )
})

# sklearn compare --------------------------------------------------------------

test_that("Two class - sklearn equivalent", {
  py_res <- read_pydata("py-precision")
  r_metric <- precision

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that("Multi class - sklearn equivalent", {
  py_res <- read_pydata("py-precision")
  r_metric <- precision

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$macro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, "micro")[[".estimate"]],
    py_res$micro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, "macro_weighted")[[".estimate"]],
    py_res$weighted
  )
})

test_that("Two class case weighted - sklearn equivalent", {
  py_res <- read_pydata("py-precision")
  r_metric <- precision

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, predicted, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )
})

test_that("Multi class case weighted - sklearn equivalent", {
  py_res <- read_pydata("py-precision")
  r_metric <- precision

  hpc_cv$weights <- read_weights_hpc_cv()

  expect_equal(
    r_metric(hpc_cv, obs, pred, case_weights = weights)[[".estimate"]],
    py_res$case_weight$macro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, estimator = "micro", case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$micro
  )
  expect_equal(
    r_metric(
      hpc_cv,
      obs,
      pred,
      estimator = "macro_weighted",
      case_weights = weights
    )[[".estimate"]],
    py_res$case_weight$weighted
  )
})
