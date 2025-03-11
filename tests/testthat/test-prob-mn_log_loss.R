test_that("`event_level = 'second'` works", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- stats::relevel(df_rev$truth, "Class2")

  expect_equal(
    mn_log_loss_vec(df$truth, df$Class1),
    mn_log_loss_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that("Three class", {
  ll_dat <- data.frame(
    obs = factor(c("A", "A", "A", "B", "B", "C")),
    A = c(1, .80, .51, .1, .2, .3),
    B = c(0, .05, .29, .8, .6, .3),
    C = c(0, .15, .20, .1, .2, .4)
  )

  expect_equal(
    mn_log_loss(ll_dat, obs, LETTERS[1:3])[[".estimate"]],
    -(log(1) + log(.8) + log(.51) + log(.8) + log(.6) + log(.4)) / 6
  )
  expect_equal(
    mn_log_loss(ll_dat, truth = "obs", A, B, C, sum = TRUE)[[".estimate"]],
    -(log(1) + log(.8) + log(.51) + log(.8) + log(.6) + log(.4))
  )
})

test_that("Issue #29", {
  x <-
    structure(
      list(
        No = c(0.860384856004899, 1, 1),
        Yes = c(0.139615143995101, 0, 0),
        prob = c(0.139615143995101, 0, 0),
        estimate = structure(
          c(1L, 1L, 1L),
          .Label = c("No", "Yes"),
          class = "factor"
        ),
        truth = structure(
          c(2L, 1L, 2L),
          .Label = c("No", "Yes"),
          class = "factor"
        ),
        truth_num = c(1, 0, 1)
      ),
      row.names = c(NA, -3L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  expect_equal(
    mn_log_loss(x[1:2, ], truth = truth, No)[[".estimate"]],
    0.9844328,
    tolerance = .0001
  )
  expect_equal(
    mn_log_loss(x, truth = truth, No)[[".estimate"]],
    12.6708396674381,
    tolerance = .0001
  )
})

# ------------------------------------------------------------------------------

test_that("mn_log_loss() applies the min/max rule when an 'event' has probability 0 (#103)", {
  truth <- factor(c("Yes", "No", "Yes"), levels = c("Yes", "No"))
  estimate <- c(.5, .5, 0)

  expect_equal(
    mn_log_loss_vec(truth, estimate),
    12.476649250079,
    tolerance = 0.0001
  )
})

test_that("mn_log_loss() applies the min/max rule when a 'non-event' has probability 1 (#103)", {
  truth <- factor(c("Yes", "No", "No"), levels = c("Yes", "No"))
  estimate <- c(.5, .5, 1)

  expect_equal(
    mn_log_loss_vec(truth, estimate),
    12.476649250079,
    tolerance = 0.0001
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

# ------------------------------------------------------------------------------

test_that("Two class - sklearn equivalent", {
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
})

test_that("Multi class - sklearn equivalent", {
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

test_that("Two class case weighted - sklearn equivalent", {
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
})

test_that("Multi class case weighted - sklearn equivalent", {
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
