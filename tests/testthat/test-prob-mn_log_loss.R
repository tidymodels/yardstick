context("Mean Log Loss")

# ------------------------------------------------------------------------------

test_that("`event_level = 'second'` works", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- relevel(df_rev$truth, "Class2")

  expect_equal(
    mn_log_loss_vec(df$truth, df$Class1),
    mn_log_loss_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that('Three class', {
  ll_dat <- data.frame(
    obs  = factor(c("A", "A", "A", "B", "B", "C")),
    A = c(1, .80, .51, .1, .2, .3),
    B = c(0, .05, .29, .8, .6, .3),
    C = c(0, .15, .20, .1, .2, .4)
  )

  expect_equal(
    mn_log_loss(ll_dat, obs, LETTERS[1:3])[[".estimate"]],
    -(log(1) + log(.8) + log(.51) + log(.8) + log(.6) + log(.4))/6
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
      row.names = c(NA,-3L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  expect_equal(
    mn_log_loss(x[1:2,], truth = truth, No)[[".estimate"]],
    0.9844328,
    tol = .0001
  )
  expect_equal(
    mn_log_loss(x, truth = truth, No)[[".estimate"]],
    12.6708396674381,
    tol = .0001
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
