test_that("Calculations are correct - two class", {
  lst <- data_altman()
  pathology <- lst$pathology

  # https://en.wikipedia.org/wiki/Cohen%27s_kappa
  a <- lst$path_tbl[1, 1]
  b <- lst$path_tbl[1, 2]
  c <- lst$path_tbl[2, 1]
  d <- lst$path_tbl[2, 2]

  total <- a + b + c + d
  p_o <- (a + d) / total
  p_yes <- (a + b) / total * (a + c) / total
  p_no <- (c + d) / total * (b + d) / total
  p_e <- p_yes + p_no
  exp <- (p_o - p_e) / (1 - p_e)

  expect_equal(
    kap_vec(truth = pathology$pathology, estimate = pathology$scan),
    exp
  )
})

test_that("Calculations are correct - three class", {
  # expected results from e1071::classAgreement(three_class_tb)$kappa

  lst <- data_three_class()
  three_class <- lst$three_class

  expect_equal(
    kap_vec(truth = three_class$obs, estimate = three_class$pred),
    0.05
  )
})

test_that("All interfaces gives the same results", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl
  path_mat <- as.matrix(path_tbl)

  exp <- kap_vec(pathology$pathology, pathology$scan)

  expect_identical(
    kap(path_tbl)[[".estimate"]],
    exp
  )
  expect_identical(
    kap(path_mat)[[".estimate"]],
    exp
  )
  expect_identical(
    kap(pathology, truth = pathology, estimate = scan)[[".estimate"]],
    exp
  )
})

test_that("Calculations handles NAs", {
  # e1071::classAgreement(table(three_class$pred_na, three_class$obs))$kappa

  lst <- data_three_class()
  three_class <- lst$three_class

  expect_equal(
    kap_vec(truth = three_class$obs, estimate = three_class$pred_na),
    -0.1570248,
    tolerance = 0.000001
  )
})

test_that("Case weights calculations are correct", {
  df <- data.frame(
    truth = factor(c("x", "x", "y"), levels = c("x", "y")),
    estimate = factor(c("x", "y", "x"), levels = c("x", "y")),
    case_weights = c(1L, 10L, 2L)
  )

  expect_equal(
    kap_vec(df$truth, df$estimate, case_weights = df$case_weights),
    -0.344827586
  )

  py_res <- read_pydata("py-kap")
  r_metric <- kap

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, predicted, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )

  py_res <- read_pydata("py-kap")
  r_metric <- kap

  hpc_cv$weights <- read_weights_hpc_cv()

  expect_equal(
    r_metric(hpc_cv, obs, pred, case_weights = weights)[[".estimate"]],
    py_res$case_weight$multiclass
  )

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

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    kap_vec(1, 1, na_rm = "yes")
  )
})

test_that("sklearn equivalent", {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )

  py_res <- read_pydata("py-kap")
  r_metric <- kap

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$multiclass
  )

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

test_that("range values are correct", {
  direction <- metric_direction(kap)
  range <- metric_range(kap)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = factor(c("A", "A", "B", "B", "B")),
    off = factor(c("B", "B", "A", "A", "A"))
  )

  expect_equal(
    kap_vec(df$truth, df$truth),
    perfect
  )

  if (direction == "minimize") {
    expect_gt(kap_vec(df$truth, df$off), perfect)
    expect_lt(kap_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lt(kap_vec(df$truth, df$off), perfect)
    expect_gt(kap_vec(df$truth, df$off), worst)
  }
})
