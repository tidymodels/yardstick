test_that("Calculations are correct - two class", {
  # powers paper
  lst <- data_powers()
  df_2_1 <- lst$df_2_1

  expect_equal(
    recall_vec(truth = df_2_1$truth, estimate = df_2_1$prediction),
    30 / 60
  )
})

test_that("Calculations are correct - three class", {
  lst <- data_three_class()
  three_class <- lst$three_class

  expect_equal(
    recall_vec(truth = three_class$obs, estimate = three_class$pred),
    11 / 30
  )
})

test_that("All interfaces gives the same results", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl
  path_mat <- as.matrix(path_tbl)

  exp <- recall_vec(pathology$pathology, pathology$scan)

  expect_identical(
    recall(path_tbl)[[".estimate"]],
    exp
  )
  expect_identical(
    recall(path_mat)[[".estimate"]],
    exp
  )
  expect_identical(
    recall(pathology, truth = pathology, estimate = scan)[[".estimate"]],
    exp
  )
})

test_that("Calculations handles NAs", {
  # powers paper
  lst <- data_powers()
  df_2_1 <- lst$df_2_1

  expect_equal(
    recall_vec(truth = df_2_1$truth, estimate = df_2_1$pred_na),
    26 / (26 + 29)
  )
})

test_that("Case weights calculations are correct", {
  py_res <- read_pydata("py-recall")
  r_metric <- recall

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, predicted, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )

  py_res <- read_pydata("py-recall")
  r_metric <- recall

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

test_that("work with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  cp_estimate <- probably::as_class_pred(two_class_example$predicted, which = 2)

  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  fct_estimate <- two_class_example$predicted
  fct_estimate[2] <- NA

  expect_identical(
    recall_vec(fct_truth, cp_estimate),
    recall_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    recall_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    recall_vec(cp_truth, cp_estimate)
  )
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    recall_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    recall_vec(df$pathology, df$scan, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    recall_vec(1, 1, na_rm = "yes")
  )
})

test_that("sklearn equivalent", {
  py_res <- read_pydata("py-recall")
  r_metric <- recall

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )

  py_res <- read_pydata("py-recall")
  r_metric <- recall

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

test_that("`event_level = 'second'` works", {
  lst <- data_powers()
  df <- lst$df_2_1

  df_rev <- df
  df_rev$truth <- stats::relevel(df_rev$truth, "Irrelevant")
  df_rev$prediction <- stats::relevel(df_rev$prediction, "Irrelevant")

  expect_equal(
    recall_vec(df$truth, df$prediction),
    recall_vec(df_rev$truth, df_rev$prediction, event_level = "second")
  )
})

test_that("Binary returns `NA` with a warning when results are undefined (#98)", {
  # recall - (tp + fn = 0)
  levels <- c("a", "b")
  truth <- factor(c("b", "b"), levels = levels)
  estimate <- factor(c("a", "b"), levels = levels)

  expect_snapshot(
    out <- recall_vec(truth, estimate)
  )
  expect_identical(out, NA_real_)
})

test_that("Multiclass returns averaged value a warning when results is undefined (#98)", {
  # recall - (tp + fn = 0)
  levels <- c("a", "b", "c", "d")

  # When `d` is the event we get recall    = 0.5 = (tp = 1, fn = 1)
  # When `a` is the event we get recall    = 1   = (tp = 1, fn = 0)
  # When `b` is the event we get a warning = NA  = (tp = 0, fn = 0)
  # When `c` is the event we get a warning = NA  = (tp = 0, fn = 0)
  truth <- factor(c("a", "d", "d"), levels = levels)
  estimate <- factor(c("a", "d", "c"), levels = levels)

  expect_snapshot(out <- recall_vec(truth, estimate))
  expect_identical(out, 0.75)
})

test_that("`NA` is still returned if there are some undefined values but `na.rm = FALSE`", {
  levels <- c("a", "b", "c", "d")
  truth <- factor(c("a", "d", "d"), levels = levels)
  estimate <- factor(c("a", NA, "c"), levels = levels)
  expect_equal(recall_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(recall_vec(truth, estimate, na_rm = FALSE), NA)
})
