test_that("Two class", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    mcc(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    ((231 * 54) - (32 * 27)) /
      sqrt((231 + 32) * (231 + 27) * (54 + 32) * (54 + 27))
  )
  expect_equal(
    mcc(path_tbl)[[".estimate"]],
    ((231 * 54) - (32 * 27)) /
      sqrt((231 + 32) * (231 + 27) * (54 + 32) * (54 + 27))
  )
  expect_equal(
    mcc(pathology, truth = pathology, estimate = scan_na)[[".estimate"]],
    ((230 * 53) - (32 * 26)) /
      sqrt((230 + 32) * (230 + 26) * (53 + 32) * (53 + 26))
  )
})

test_that("two class produces identical results regardless of level order", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    mcc_vec(df$pathology, df$scan),
    mcc_vec(df_rev$pathology, df_rev$scan)
  )
})

test_that("doesn't integer overflow (#108)", {
  x <- matrix(c(50122L, 50267L, 49707L, 49904L), ncol = 2L, nrow = 2L)
  expect_equal(
    mcc(x)[[".estimate"]],
    0.00026665430738672
  )
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    mcc_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    mcc_vec(df$pathology, df$scan, case_weights = freq_wgt)
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
    mcc_vec(fct_truth, cp_estimate),
    mcc_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    mcc_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    mcc_vec(cp_truth, cp_estimate)
  )
})

# sklearn compare --------------------------------------------------------------

test_that("Two class - sklearn equivalent", {
  py_res <- read_pydata("py-mcc")
  r_metric <- mcc

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that("Multi class - sklearn equivalent", {
  py_res <- read_pydata("py-mcc")
  r_metric <- mcc

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$multiclass
  )
})

test_that("Two class case weighted - sklearn equivalent", {
  py_res <- read_pydata("py-mcc")
  r_metric <- mcc

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, predicted, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )
})

test_that("Multi class case weighted - sklearn equivalent", {
  py_res <- read_pydata("py-mcc")
  r_metric <- mcc

  hpc_cv$weights <- read_weights_hpc_cv()

  expect_equal(
    r_metric(hpc_cv, obs, pred, case_weights = weights)[[".estimate"]],
    py_res$case_weight$multiclass
  )
})
