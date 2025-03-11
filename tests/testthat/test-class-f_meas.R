test_that("Two class - Powers paper", {
  lst <- data_powers()
  tabl_2_1 <- lst$tabl_2_1
  df_2_1 <- lst$df_2_1

  expect_equal(
    f_meas(df_2_1, truth = "truth", estimate = "prediction")[[".estimate"]],
    0.5882353,
    tolerance = 0.0001
  )
  expect_equal(
    f_meas(tabl_2_1)[[".estimate"]],
    0.5882353,
    tolerance = 0.0001
  )
  expect_equal(
    f_meas(df_2_1, truth = truth, estimate = pred_na)[[".estimate"]],
    0.5652174,
    tolerance = 0.0001
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_powers()
  df <- lst$df_2_1

  df_rev <- df
  df_rev$truth <- stats::relevel(df_rev$truth, "Irrelevant")
  df_rev$prediction <- stats::relevel(df_rev$prediction, "Irrelevant")

  expect_equal(
    f_meas_vec(df$truth, df$prediction),
    f_meas_vec(df_rev$truth, df_rev$prediction, event_level = "second")
  )
})

# ------------------------------------------------------------------------------
# Issue #77

test_that("`NA` values propagate from binary `precision()`", {
  truth <- factor(c(rep("a", 2), rep("b", 2)))
  estimate <- factor(rep("b", length(truth)), levels(truth))

  expect_snapshot({
    out <- precision_vec(truth, estimate)
    expect <- f_meas_vec(truth, estimate)
  })

  expect_identical(out, expect)
})

test_that("`NA` values propagate from binary `recall()`", {
  estimate <- factor(c(rep("a", 2), rep("b", 2)))
  truth <- factor(rep("b", length(estimate)), levels(estimate))

  expect_snapshot({
    out <- recall_vec(truth, estimate)
    expect <- f_meas_vec(truth, estimate)
  })

  expect_identical(out, expect)
})

# ------------------------------------------------------------------------------

test_that("Binary `f_meas()` returns `NA` with a warning when recall is undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b")
  truth <- factor(c("b", "b"), levels = levels)
  estimate <- factor(c("a", "b"), levels = levels)

  expect_snapshot(
    out <- f_meas_vec(truth, estimate)
  )

  expect_identical(out, NA_real_)
})

test_that("Binary `f_meas()` returns `NA` with a warning when precision is undefined (tp + fp = 0) (#98)", {
  levels <- c("a", "b")
  truth <- factor("a", levels = levels)
  estimate <- factor("b", levels = levels)

  expect_snapshot(
    out <- f_meas_vec(truth, estimate)
  )

  expect_identical(out, NA_real_)
})

test_that("Multiclass `f_meas()` returns averaged value with `NA`s removed + a warning when recall is undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b", "c")

  truth <- factor(c("a", "b", "b"), levels = levels)
  estimate <- factor(c("a", "b", "c"), levels = levels)

  expect_snapshot(
    out <- f_meas_vec(truth, estimate)
  )

  expect_identical(out, 5 / 6)
})

test_that("Multiclass `f_meas()` returns averaged value with `NA`s removed + a warning when precision is undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b", "c")

  truth <- factor(c("a", "b", "c"), levels = levels)
  estimate <- factor(c("a", "b", "b"), levels = levels)

  expect_snapshot(
    out <- f_meas_vec(truth, estimate)
  )

  expect_identical(out, 5 / 6)
})

test_that("`NA` is still returned if there are some undefined recall values but `na.rm = FALSE`", {
  levels <- c("a", "b", "c")
  truth <- factor(c("a", "b", "b"), levels = levels)
  estimate <- factor(c("a", NA, "c"), levels = levels)
  expect_equal(f_meas_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(f_meas_vec(truth, estimate, na_rm = FALSE), NA)
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    f_meas_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    f_meas_vec(df$pathology, df$scan, case_weights = freq_wgt)
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
    f_meas_vec(fct_truth, cp_estimate),
    f_meas_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    f_meas_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    f_meas_vec(cp_truth, cp_estimate)
  )
})

# sklearn compare --------------------------------------------------------------

test_that("Two class - sklearn equivalent", {
  py_res <- read_pydata("py-f_meas")
  py_res_.5 <- read_pydata("py-f_meas_beta_.5")
  r_metric <- f_meas

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
  expect_equal(
    r_metric(two_class_example, truth, predicted, beta = .5)[[".estimate"]],
    py_res_.5$binary
  )
})

test_that("Multi class - sklearn equivalent", {
  py_res <- read_pydata("py-f_meas")
  py_res_.5 <- read_pydata("py-f_meas_beta_.5")
  r_metric <- f_meas

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$macro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, estimator = "micro")[[".estimate"]],
    py_res$micro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, estimator = "macro_weighted")[[".estimate"]],
    py_res$weighted
  )

  expect_equal(
    r_metric(hpc_cv, obs, pred, beta = .5)[[".estimate"]],
    py_res_.5$macro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, estimator = "micro", beta = .5)[[".estimate"]],
    py_res_.5$micro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, "macro_weighted", beta = .5)[[".estimate"]],
    py_res_.5$weighted
  )
})

test_that("Two class weighted - sklearn equivalent", {
  py_res <- read_pydata("py-f_meas")
  py_res_.5 <- read_pydata("py-f_meas_beta_.5")
  r_metric <- f_meas

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, predicted, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )
  expect_equal(
    r_metric(
      two_class_example,
      truth,
      predicted,
      case_weights = weights,
      beta = .5
    )[[".estimate"]],
    py_res_.5$case_weight$binary
  )
})

test_that("Multi class weighted - sklearn equivalent", {
  py_res <- read_pydata("py-f_meas")
  py_res_.5 <- read_pydata("py-f_meas_beta_.5")
  r_metric <- f_meas

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

  expect_equal(
    r_metric(hpc_cv, obs, pred, beta = .5, case_weights = weights)[[
      ".estimate"
    ]],
    py_res_.5$case_weight$macro
  )
  expect_equal(
    r_metric(
      hpc_cv,
      obs,
      pred,
      estimator = "micro",
      beta = .5,
      case_weights = weights
    )[[".estimate"]],
    py_res_.5$case_weight$micro
  )
  expect_equal(
    r_metric(
      hpc_cv,
      obs,
      pred,
      estimator = "macro_weighted",
      beta = .5,
      case_weights = weights
    )[[".estimate"]],
    py_res_.5$case_weight$weighted
  )
})
