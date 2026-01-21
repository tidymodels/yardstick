test_that("Calculations are correct - two class", {
  lst <- data_altman()
  pathology <- lst$pathology

  expect_equal(
    accuracy_vec(pathology$pathology, pathology$scan),
    mean(pathology$pathology == pathology$scan)
  )
})

test_that("Calculations are correct - three class", {
  lst <- data_three_class()
  three_class <- lst$three_class

  expect_equal(
    accuracy_vec(truth = three_class$obs, estimate = three_class$pred),
    (24 + 17 + 14) / 150
  )
})

test_that("All interfaces gives the same results", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl
  path_mat <- unclass(path_tbl)

  exp <- accuracy_vec(pathology$pathology, pathology$scan)

  expect_identical(
    accuracy(path_tbl)[[".estimate"]],
    exp
  )
  expect_identical(
    accuracy(path_mat)[[".estimate"]],
    exp
  )
  expect_identical(
    accuracy(pathology, truth = pathology, estimate = scan)[[".estimate"]],
    exp
  )
})

test_that("Calculations handles NAs", {
  lst <- data_altman()
  pathology <- lst$pathology

  expect_equal(
    accuracy_vec(truth = pathology$pathology, estimate = pathology$scan_na),
    mean(pathology$pathology == pathology$scan_na, na.rm = TRUE)
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
    accuracy(df, truth, estimate, case_weights = case_weights)[[".estimate"]],
    1 / 4
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
    accuracy_vec(fct_truth, cp_estimate),
    accuracy_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    accuracy_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    accuracy_vec(cp_truth, cp_estimate)
  )
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    accuracy_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    accuracy_vec(df$pathology, df$scan, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    accuracy_vec(1, 1, na_rm = "yes")
  )
})

test_that("sklearn equivalent", {
  py_res <- read_pydata("py-accuracy")
  r_metric <- accuracy

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )

  py_res <- read_pydata("py-accuracy")
  r_metric <- accuracy

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$multiclass
  )
})

test_that("two class produces identical results regardless of level order", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    accuracy_vec(df$pathology, df$scan),
    accuracy_vec(df_rev$pathology, df_rev$scan)
  )
})

test_that("range values are correct", {
  direction <- metric_direction(accuracy)
  range <- metric_range(accuracy)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = factor(c("A", "A", "B", "B", "B")),
    off = factor(c("B", "B", "A", "A", "A"))
  )

  expect_equal(
    accuracy_vec(df$truth, df$truth),
    perfect
  )

  if (direction == "minimize") {
    expect_gt(accuracy_vec(df$truth, df$off), perfect)
    expect_lte(accuracy_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lt(accuracy_vec(df$truth, df$off), perfect)
    expect_gte(accuracy_vec(df$truth, df$off), worst)
  }
})
