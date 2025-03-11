test_that("two class produces identical results regardless of level order", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    kap_vec(df$pathology, df$scan),
    kap_vec(df_rev$pathology, df_rev$scan)
  )
})

test_that("kap errors with wrong `weighting`", {
  lst <- data_three_class()
  three_class <- lst$three_class

  expect_snapshot(
    error = TRUE,
    kap(three_class, truth = "obs", estimate = "pred", weighting = 1)
  )

  expect_snapshot(
    error = TRUE,
    kap(three_class, truth = "obs", estimate = "pred", weighting = "not right")
  )
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    kap_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    kap_vec(df$pathology, df$scan, case_weights = freq_wgt)
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
    kap_vec(fct_truth, cp_estimate),
    kap_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    kap_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    kap_vec(cp_truth, cp_estimate)
  )
})

# ------------------------------------------------------------------------------

# expected results from e1071::classAgreement(three_class_tb)$kappa
# e1071::classAgreement(table(three_class$pred_na, three_class$obs))$kappa

test_that("Three class", {
  lst <- data_three_class()
  three_class <- lst$three_class
  three_class_tb <- lst$three_class_tb

  expect_equal(
    kap(three_class, truth = "obs", estimate = "pred")[[".estimate"]],
    0.05
  )
  expect_equal(
    kap(three_class_tb)[[".estimate"]],
    0.05
  )
  expect_equal(
    kap(as.matrix(three_class_tb))[[".estimate"]],
    0.05
  )
  expect_equal(
    kap(three_class, obs, pred_na)[[".estimate"]],
    -0.1570248,
    tolerance = 0.000001
  )
  expect_equal(
    colnames(kap(three_class, truth = "obs", estimate = "pred")),
    c(".metric", ".estimator", ".estimate")
  )
  expect_equal(
    kap(three_class, truth = "obs", estimate = "pred")[[".metric"]],
    "kap"
  )
})

# sklearn compare --------------------------------------------------------------

test_that("Two class - sklearn equivalent", {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that("Multi class - sklearn equivalent", {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$multiclass
  )
})

test_that("linear weighting - sklearn equivalent", {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  expect_equal(
    r_metric(two_class_example, truth, predicted, weighting = "linear")[[
      ".estimate"
    ]],
    py_res$linear_binary
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, weighting = "linear")[[".estimate"]],
    py_res$linear_multiclass
  )
})

test_that("quadratic weighting - sklearn equivalent", {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  expect_equal(
    r_metric(two_class_example, truth, predicted, weighting = "quadratic")[[
      ".estimate"
    ]],
    py_res$quadratic_binary
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, weighting = "quadratic")[[".estimate"]],
    py_res$quadratic_multiclass
  )
})

test_that("Two class case weighted - sklearn equivalent", {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, predicted, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )
})

test_that("Multi class case weighted - sklearn equivalent", {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  hpc_cv$weights <- read_weights_hpc_cv()

  expect_equal(
    r_metric(hpc_cv, obs, pred, case_weights = weights)[[".estimate"]],
    py_res$case_weight$multiclass
  )
})

test_that("linear weighting case weighted - sklearn equivalent", {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  two_class_example$weights <- read_weights_two_class_example()
  hpc_cv$weights <- read_weights_hpc_cv()

  expect_equal(
    r_metric(
      two_class_example,
      truth,
      predicted,
      weighting = "linear",
      case_weights = weights
    )[[".estimate"]],
    py_res$case_weight$linear_binary
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, weighting = "linear", case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$linear_multiclass
  )
})

test_that("quadratic weighting case weighted - sklearn equivalent", {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  two_class_example$weights <- read_weights_two_class_example()
  hpc_cv$weights <- read_weights_hpc_cv()

  expect_equal(
    r_metric(
      two_class_example,
      truth,
      predicted,
      weighting = "quadratic",
      case_weights = weights
    )[[".estimate"]],
    py_res$case_weight$quadratic_binary
  )
  expect_equal(
    r_metric(
      hpc_cv,
      obs,
      pred,
      weighting = "quadratic",
      case_weights = weights
    )[[".estimate"]],
    py_res$case_weight$quadratic_multiclass
  )
})
