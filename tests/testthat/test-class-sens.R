test_that("Two class", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    sens(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    231 / 258
  )
  expect_equal(
    sens(pathology, estimate = scan, truth = pathology)[[".estimate"]],
    231 / 258
  )
  expect_equal(
    sens(pathology, pathology, scan)[[".estimate"]],
    231 / 258
  )
  expect_equal(
    sens(path_tbl)[[".estimate"]],
    231 / 258
  )
  expect_equal(
    sens(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    230 / 256
  )
  expect_equal(
    sens(as.matrix(path_tbl))[[".estimate"]],
    231 / 258
  )
  expect_equal(
    sens(pathology, pathology, scan_na, na_rm = FALSE)[[".estimate"]],
    NA_real_
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    sens_vec(df$pathology, df$scan),
    sens_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that("Three class", {
  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()

  # sens = recall
  expect_equal(
    sens(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(recall_binary)
  )
  expect_equal(
    sens(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(recall_binary)
  )
  expect_equal(
    sens(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro, sum(tp) / sum(tp + fp))
  )
})

# ------------------------------------------------------------------------------

test_that("Binary `sens()` returns `NA` with a warning when undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b")
  truth <- factor(c("b", "b"), levels = levels)
  estimate <- factor(c("a", "b"), levels = levels)

  expect_snapshot(out <- sens_vec(truth, estimate))
  expect_identical(out, NA_real_)
})

test_that("Multiclass `sens()` returns averaged value with `NA`s removed + a warning when undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b", "c", "d")

  # When `d` is the event we get sens      = 0.5 = (tp = 1, fn = 1)
  # When `a` is the event we get sens      = 1   = (tp = 1, fn = 0)
  # When `b` is the event we get a warning = NA  = (tp = 0, fn = 0)
  # When `c` is the event we get a warning = NA  = (tp = 0, fn = 0)
  truth <- factor(c("a", "d", "d"), levels = levels)
  estimate <- factor(c("a", "d", "c"), levels = levels)

  expect_snapshot(out <- sens_vec(truth, estimate))
  expect_identical(out, 0.75)
})

test_that("`NA` is still returned if there are some undefined sens values but `na.rm = FALSE`", {
  levels <- c("a", "b", "c", "d")
  truth <- factor(c("a", "d", "d"), levels = levels)
  estimate <- factor(c("a", NA, "c"), levels = levels)
  expect_equal(sens_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(sens_vec(truth, estimate, na_rm = FALSE), NA)
})

# ------------------------------------------------------------------------------

test_that("Two class - sklearn equivalent", {
  # Same as recall
  py_res <- read_pydata("py-recall")
  r_metric <- sens

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that("Multi class - sklearn equivalent", {
  # Same as recall
  py_res <- read_pydata("py-recall")
  r_metric <- sens

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

test_that("Two class weighted - sklearn equivalent", {
  # Same as recall
  py_res <- read_pydata("py-recall")
  r_metric <- sens

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, predicted, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )
})

test_that("Multi class weighted - sklearn equivalent", {
  # Same as recall
  py_res <- read_pydata("py-recall")
  r_metric <- sens

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

# ------------------------------------------------------------------------------

test_that("`sensitivity()` has a metric name unique to it (#232)", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_identical(
    sens(pathology, truth = "pathology", estimate = "scan")[[".metric"]],
    "sens"
  )
  expect_identical(
    sensitivity(pathology, truth = "pathology", estimate = "scan")[[".metric"]],
    "sensitivity"
  )

  expect_identical(
    sens(path_tbl)[[".metric"]],
    "sens"
  )
  expect_identical(
    sensitivity(path_tbl)[[".metric"]],
    "sensitivity"
  )
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    sensitivity_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    sensitivity_vec(df$pathology, df$scan, case_weights = freq_wgt)
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
    sensitivity_vec(fct_truth, cp_estimate),
    sensitivity_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    sensitivity_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    sensitivity_vec(cp_truth, cp_estimate)
  )
})
