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

test_that("Three class", {
  lst <- data_three_class()
  three_class <- lst$three_class
  three_class_tb <- lst$three_class_tb

  expect_equal(
    accuracy(three_class, truth = "obs", estimate = "pred")[[".estimate"]],
    (24 + 17 + 14) / 150
  )
  expect_equal(
    accuracy(three_class_tb)[[".estimate"]],
    (24 + 17 + 14) / 150
  )
  expect_equal(
    accuracy(as.matrix(three_class_tb))[[".estimate"]],
    (24 + 17 + 14) / 150
  )
  expect_equal(
    accuracy(three_class, obs, pred_na)[[".estimate"]],
    (11 + 10 + 11) / 140
  )
  expect_equal(
    colnames(accuracy(three_class, truth = "obs", estimate = "pred")),
    c(".metric", ".estimator", ".estimate")
  )
  expect_equal(
    accuracy(three_class, truth = "obs", estimate = "pred")[[".metric"]],
    "accuracy"
  )
})

test_that("two class with case weights is correct", {
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

test_that("Two class - sklearn equivalent", {
  py_res <- read_pydata("py-accuracy")
  r_metric <- accuracy

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that("Multi class - sklearn equivalent", {
  py_res <- read_pydata("py-accuracy")
  r_metric <- accuracy

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$multiclass
  )
})
