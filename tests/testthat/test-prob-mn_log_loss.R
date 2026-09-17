test_that("Calculations are correct - two class", {
  df <- data.frame(
    obs = factor(c("A", "A", "A", "B", "B", "B")),
    A = c(1, 0.80, 0.51, 0.1, 0.2, 0.3),
    B = c(0, 0.20, 0.49, 0.9, 0.8, 0.7)
  )

  expect_equal(
    mn_log_loss_vec(df$obs, df$A),
    -(log(1) + log(0.8) + log(0.51) + log(0.9) + log(0.8) + log(0.7)) / 6
  )
})

test_that("Calculations are correct - multi class", {
  ll_dat <- data.frame(
    obs = factor(c("A", "A", "A", "B", "B", "C")),
    A = c(1, 0.80, 0.51, 0.1, 0.2, 0.3),
    B = c(0, 0.05, 0.29, 0.8, 0.6, 0.3),
    C = c(0, 0.15, 0.20, 0.1, 0.2, 0.4)
  )

  expect_equal(
    mn_log_loss(ll_dat, obs, A:C)[[".estimate"]],
    -(log(1) + log(0.8) + log(0.51) + log(0.8) + log(0.6) + log(0.4)) / 6
  )
})

test_that("Calculations handles NAs", {
  ll_dat <- data.frame(
    obs = factor(c("A", "A", "A", "B", "B", "C")),
    A = c(1, 0.80, 0.51, 0.1, 0.2, 0.3),
    B = c(0, NA, 0.29, 0.8, 0.6, 0.3),
    C = c(0, NA, 0.20, 0.1, 0.2, 0.4)
  )

  expect_equal(
    mn_log_loss(ll_dat, obs, A:C)[[".estimate"]],
    -(log(1) + log(0.51) + log(0.8) + log(0.6) + log(0.4)) / 5
  )

  expect_equal(
    mn_log_loss(ll_dat, obs, A:C, na_rm = FALSE)[[".estimate"]],
    NA_real_
  )
})

test_that("Case weights calculations are correct", {
  py_res <- read_pydata("py-mn_log_loss")
  r_metric <- mn_log_loss

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, Class1, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )
  expect_equal(
    r_metric(
      two_class_example,
      truth,
      Class1,
      sum = TRUE,
      case_weights = weights
    )[[".estimate"]],
    py_res$case_weight$binary_sum
  )

  py_res <- read_pydata("py-mn_log_loss")
  r_metric <- mn_log_loss

  hpc_cv$weights <- read_weights_hpc_cv()

  expect_equal(
    r_metric(hpc_cv, obs, VF:L, case_weights = weights)[[".estimate"]],
    py_res$case_weight$multiclass
  )
  expect_equal(
    r_metric(hpc_cv, obs, VF:L, sum = TRUE, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$multiclass_sum
  )
})

test_that("works with hardhat case weights", {
  df <- two_class_example

  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    mn_log_loss_vec(df$truth, df$Class1, case_weights = imp_wgt)
  )

  expect_no_error(
    mn_log_loss_vec(df$truth, df$Class1, case_weights = freq_wgt)
  )
})

test_that("errors with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  estimate <- two_class_example$Class1

  expect_snapshot(
    error = TRUE,
    mn_log_loss_vec(cp_truth, estimate)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    mn_log_loss_vec(1, 1, na_rm = "yes")
  )
})

test_that("bad argument check", {
  expect_snapshot(
    error = TRUE,
    mn_log_loss_vec(1, 1, sum = "yes")
  )
})

test_that("`event_level = 'second'` works", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- stats::relevel(df_rev$truth, "Class2")

  expect_equal(
    mn_log_loss_vec(df$truth, df$Class1),
    mn_log_loss_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )
})

test_that("sklearn equivalent", {
  py_res <- read_pydata("py-mn_log_loss")
  r_metric <- mn_log_loss

  expect_equal(
    r_metric(two_class_example, truth, Class1)[[".estimate"]],
    py_res$binary
  )
  expect_equal(
    r_metric(two_class_example, truth, Class1, sum = TRUE)[[".estimate"]],
    py_res$binary_sum
  )

  py_res <- read_pydata("py-mn_log_loss")
  r_metric <- mn_log_loss

  expect_equal(
    r_metric(hpc_cv, obs, VF:L)[[".estimate"]],
    py_res$multiclass
  )
  expect_equal(
    r_metric(hpc_cv, obs, VF:L, sum = TRUE)[[".estimate"]],
    py_res$multiclass_sum
  )
})

test_that("sum argument works", {
  df <- data.frame(
    obs = factor(c("A", "A", "A", "B", "B", "B")),
    A = c(1, 0.80, 0.51, 0.1, 0.2, 0.3),
    B = c(0, 0.20, 0.49, 0.9, 0.8, 0.7)
  )

  expect_equal(
    mn_log_loss_vec(df$obs, df$A, sum = TRUE),
    -(log(1) + log(0.8) + log(0.51) + log(0.9) + log(0.8) + log(0.7))
  )
})

test_that("Issue (#29)", {
  x <- tibble::tibble(
    No = c(0.860384856004899, 1, 1),
    Yes = c(0.139615143995101, 0, 0),
    prob = c(0.139615143995101, 0, 0),
    estimate = factor("No", levels = c("No", "Yes")),
    truth = factor(c("Yes", "No", "Yes")),
    truth_num = c(1, 0, 1),
  )

  expect_equal(
    mn_log_loss(x[1:2, ], truth = truth, No)[[".estimate"]],
    0.9844328,
    tolerance = 0.0001
  )
  expect_equal(
    mn_log_loss(x, truth = truth, No)[[".estimate"]],
    12.6708396674381,
    tolerance = 0.0001
  )
})

test_that("mn_log_loss() applies the min/max rule when an 'event' has probability 0 (#103)", {
  truth <- factor(c("Yes", "No", "Yes"), levels = c("Yes", "No"))
  estimate <- c(0.5, 0.5, 0)

  expect_equal(
    mn_log_loss_vec(truth, estimate),
    12.476649250079,
    tolerance = 0.0001
  )
})

test_that("mn_log_loss() applies the min/max rule when a 'non-event' has probability 1 (#103)", {
  truth <- factor(c("Yes", "No", "No"), levels = c("Yes", "No"))
  estimate <- c(0.5, 0.5, 1)

  expect_equal(
    mn_log_loss_vec(truth, estimate),
    12.476649250079,
    tolerance = 0.0001
  )
})

test_that("range values are correct", {
  direction <- metric_direction(mn_log_loss)
  range <- metric_range(mn_log_loss)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = factor(c("a", "a", "a", "b", "b"), levels = c("a", "b")),
    perfect = c(1, 1, 1, 0, 0),
    off = c(0.5, 0.5, 0.5, 0.5, 0.5)
  )

  expect_equal(mn_log_loss_vec(df$truth, df$perfect), perfect)

  if (direction == "minimize") {
    expect_gt(mn_log_loss_vec(df$truth, df$off), perfect)
    expect_lte(mn_log_loss_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lt(mn_log_loss_vec(df$truth, df$off), perfect)
    expect_gte(mn_log_loss_vec(df$truth, df$off), worst)
  }
})
