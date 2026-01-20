test_that("Calculations are correct - two class", {
  # Powers paper
  lst <- data_powers()
  df_2_1 <- lst$df_2_1

  expect_equal(
    precision_vec(truth = df_2_1$truth, estimate = df_2_1$prediction),
    30 / 42
  )
})

test_that("Calculations are correct - three class", {
  lst <- data_three_class()
  three_class <- lst$three_class

  expect_equal(
    precision_vec(truth = three_class$obs, estimate = three_class$pred),
    11 / 30
  )
})

test_that("All interfaces gives the same results", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl
  path_mat <- as.matrix(path_tbl)

  exp <- precision_vec(pathology$pathology, pathology$scan)

  expect_identical(
    precision(path_tbl)[[".estimate"]],
    exp
  )
  expect_identical(
    precision(path_mat)[[".estimate"]],
    exp
  )
  expect_identical(
    precision(pathology, truth = pathology, estimate = scan)[[".estimate"]],
    exp
  )
})

test_that("Calculations handles NAs", {
  # Powers paper
  lst <- data_powers()
  df_2_1 <- lst$df_2_1

  expect_equal(
    precision_vec(truth = df_2_1$truth, estimate = df_2_1$pred_na),
    26 / 37
  )
})

test_that("Case weights calculations are correct", {
  py_res <- read_pydata("py-precision")
  r_metric <- precision

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, predicted, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )

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

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    precision_vec(1, 1, na_rm = "yes")
  )
})

test_that("sklearn equivalent", {
  py_res <- read_pydata("py-precision")
  r_metric <- precision

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )

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

test_that("Binary returns `NA` with a warning when results are undefined (#98)", {
  # precision - (tp + fp = 0)
  truth <- factor("a", levels = c("a", "b"))
  estimate <- factor("b", levels = c("a", "b"))

  expect_snapshot(
    out <- precision_vec(truth, estimate)
  )
  expect_identical(out, NA_real_)
})

test_that("Multiclass returns averaged value a warning when results is undefined (#98)", {
  # precision - (tp - fp = 0)
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

test_that("`NA` is still returned if there are some undefined values but `na.rm = FALSE`", {
  levels <- c("a", "b", "c", "d")
  truth <- factor(c("a", "b", "c"), levels = levels)
  estimate <- factor(c("a", NA, "c"), levels)
  expect_equal(precision_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(precision_vec(truth, estimate, na_rm = FALSE), NA)
})

test_that("range values are correct", {
  direction <- metric_direction(precision)
  range <- metric_range(precision)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = factor(c("A", "A", "B", "B", "B")),
    off = factor(c("B", "B", "A", "A", "A"))
  )

  expect_equal(
    precision_vec(df$truth, df$truth),
    perfect
  )

  if (direction == "minimize") {
    expect_gt(precision_vec(df$truth, df$off), perfect)
    expect_lte(precision_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lt(precision_vec(df$truth, df$off), perfect)
    expect_gte(precision_vec(df$truth, df$off), worst)
  }
})
