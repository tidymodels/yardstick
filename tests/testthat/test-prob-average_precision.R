test_that("known corner cases are correct", {
  # first value - tp = 1
  truth <- factor("a", levels = c("a", "b"))
  estimate <- .9
  df <- data.frame(truth, estimate)

  expect_equal(
    average_precision(df, truth, estimate)$.estimate,
    1
  )

  # With the recall == 0 case precision value
  # defined to be precision == 1, we also expect
  # these to match pr_auc()

  expect_equal(
    average_precision(df, truth, estimate)$.estimate,
    pr_auc(df, truth, estimate)$.estimate
  )

  # first value - fp = 1, no `truth` events
  truth <- factor("b", levels = c("a", "b"))
  estimate <- .9
  df <- data.frame(truth, estimate)

  expect_snapshot(out <- average_precision(df, truth, estimate)$.estimate)
  expect_identical(out, NaN)

  # Same as pr_auc()
  expect_snapshot(out <- average_precision(df, truth, estimate)$.estimate)
  expect_snapshot(expect <- pr_auc(df, truth, estimate)$.estimate)
  expect_identical(out, expect)
})

test_that("`event_level = 'second'` works", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- stats::relevel(df_rev$truth, "Class2")

  expect_equal(
    average_precision_vec(df$truth, df$Class1),
    average_precision_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )
})

test_that("Two class average precision matches sklearn", {
  py <- read_pydata("py-average-precision")

  expect_equal(
    average_precision(two_class_example, truth, Class1)[[".estimate"]],
    py$binary
  )
})

test_that("Two class weighted average precision matches sklearn", {
  py <- read_pydata("py-average-precision")

  two_class_example$weight <- read_weights_two_class_example()

  expect_equal(
    average_precision(two_class_example, truth, Class1, case_weights = weight)[[
      ".estimate"
    ]],
    py$case_weight$binary
  )
})

test_that("Multiclass average precision matches sklearn", {
  py <- read_pydata("py-average-precision")

  expect_equal(
    average_precision(hpc_cv, obs, VF:L, estimator = "macro")[[".estimate"]],
    py$macro
  )
  expect_equal(
    average_precision(hpc_cv, obs, VF:L, estimator = "macro_weighted")[[
      ".estimate"
    ]],
    py$macro_weighted
  )
})

test_that("Multiclass weighted average precision matches sklearn", {
  py <- read_pydata("py-average-precision")

  hpc_cv$weight <- read_weights_hpc_cv()

  expect_equal(
    average_precision(
      hpc_cv,
      obs,
      VF:L,
      estimator = "macro",
      case_weights = weight
    )[[".estimate"]],
    py$case_weight$macro
  )
  expect_equal(
    average_precision(
      hpc_cv,
      obs,
      VF:L,
      estimator = "macro_weighted",
      case_weights = weight
    )[[".estimate"]],
    py$case_weight$macro_weighted
  )
})

test_that("works with hardhat case weights", {
  df <- two_class_example

  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    average_precision_vec(df$truth, df$Class1, case_weights = imp_wgt)
  )

  expect_no_error(
    average_precision_vec(df$truth, df$Class1, case_weights = freq_wgt)
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
    average_precision_vec(cp_truth, estimate)
  )
})
