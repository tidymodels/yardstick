library(testthat)
context("Gain and lift curves")

# Basic tests ------------------------------------------------------------------

# known answer
estimate <- c(.9, .8, .7, .68, .5)
truth <- factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No"))
df <- data.frame(truth, estimate)

# caret::lift(truth ~ estimate, df)
lft <- c(NaN, 1.66666666666667, 1.66666666666667, 1.11111111111111, 1.25, 1)
perc_found <- c(0, 33.3333333333333, 66.6666666666667, 66.6666666666667, 100, 100)

test_that("lift_curve() matches known result", {

  expect_is(lift_curve(df, truth, estimate), "lift_df")
  expect_equal(lift_curve(df, truth, estimate)$.lift, lft)

})

test_that("gain_curve() matches known result", {

  expect_is(gain_curve(df, truth, estimate), "gain_df")
  expect_equal(gain_curve(df, truth, estimate)$.percent_found, perc_found)

})

test_that("error handling", {

  expect_error(
    lift_curve(df, estimate, truth),
    "`truth` should be a factor but a numeric was supplied."
  )

})

test_that("quasiquotation works", {
  tru <- as.name("truth")

  expect_error(lift_curve(df, !!tru, estimate), regexp = NA)
  expect_error(gain_curve(df, !!tru, estimate), regexp = NA)
  expect_error(lift_curve(df, "truth", estimate), regexp = NA)
  expect_error(gain_curve(df, "truth", estimate), regexp = NA)
})

# Duplicates -------------------------------------------------------------------

# known answer
dup_estimate <- c(.9, .9, .7, .68, .68)
dup_truth <- factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No"))
dup_df <- data.frame(estimate = dup_estimate, truth = dup_truth)

test_that("duplicates are removed", {

  gain_df <- gain_curve(dup_df, truth, estimate)
  lift_df <- lift_curve(dup_df, truth, estimate)

  expect_equal(nrow(gain_df), 4L)
  expect_equal(nrow(lift_df), 4L)

  # .n_events should be 2 for the 2 .9+Yes predictions
  expect_equal(gain_df$.n_events[2], 2)

})




