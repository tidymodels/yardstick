# Basic tests ------------------------------------------------------------------

# known answers

test_that("lift_curve() matches known result", {
  df <- data.frame(
    truth = factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No")),
    estimate = c(.9, .8, .7, .68, .5)
  )

  # caret::lift(truth ~ estimate, df)
  lft <- c(NaN, 1.66666666666667, 1.66666666666667, 1.11111111111111, 1.25, 1)

  expect_s3_class(lift_curve(df, truth, estimate), "lift_df")
  expect_equal(lift_curve(df, truth, estimate)$.lift, lft)
})

test_that("gain_curve() matches known result", {
  df <- data.frame(
    truth = factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No")),
    estimate = c(.9, .8, .7, .68, .5)
  )

  # caret::lift(truth ~ estimate, df)
  perc_found <- c(0, 33.3333333333333, 66.6666666666667, 66.6666666666667, 100, 100)

  expect_s3_class(gain_curve(df, truth, estimate), "gain_df")
  expect_equal(gain_curve(df, truth, estimate)$.percent_found, perc_found)

})

test_that("error handling", {
  df <- data.frame(truth = 1, estimate = factor("x"))

  expect_error(
    lift_curve(df, truth, estimate),
    "`truth` should be a factor but a numeric was supplied."
  )
})

test_that("quasiquotation works", {
  df <- data.frame(
    truth = factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No")),
    estimate = c(.9, .8, .7, .68, .5)
  )

  tru <- as.name("truth")

  expect_error(lift_curve(df, !!tru, estimate), regexp = NA)
  expect_error(gain_curve(df, !!tru, estimate), regexp = NA)
  expect_error(lift_curve(df, "truth", estimate), regexp = NA)
  expect_error(gain_curve(df, "truth", estimate), regexp = NA)
})

# ------------------------------------------------------------------------------

test_that("`event_level = 'second'` works", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- relevel(df_rev$truth, "Class2")

  expect_equal(
    lift_curve_vec(df$truth, df$Class1),
    lift_curve_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )

  expect_equal(
    gain_curve_vec(df$truth, df$Class1),
    gain_curve_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )
})

# Duplicates -------------------------------------------------------------------

test_that("duplicates are removed", {
  # known answer
  dup_estimate <- c(.9, .9, .7, .68, .68)
  dup_truth <- factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No"))
  dup_df <- data.frame(estimate = dup_estimate, truth = dup_truth)

  gain_df <- gain_curve(dup_df, truth, estimate)
  lift_df <- lift_curve(dup_df, truth, estimate)

  expect_equal(nrow(gain_df), 4L)
  expect_equal(nrow(lift_df), 4L)

  # .n_events should be 2 for the 2 .9+Yes predictions
  expect_equal(gain_df$.n_events[2], 2)
})


# Multiclass -------------------------------------------------------------------

test_that("Multiclass structure is correct", {
  res_gain <- gain_curve(hpc_cv, obs, VF:L)
  res_lift <- lift_curve(hpc_cv, obs, VF:L)

  expect_true(".level" %in% colnames(res_gain))
  expect_true(".level" %in% colnames(res_lift))

  expect_s3_class(res_gain, "gain_df")
  expect_s3_class(res_lift, "lift_df")
})

test_that("Grouped structure is correct", {
  hpc_g <- dplyr::group_by(hpc_cv, Resample)

  res_gain <- gain_curve(hpc_g, obs, VF:L)
  res_lift <- lift_curve(hpc_g, obs, VF:L)

  expect_true("Resample" %in% colnames(res_gain))
  expect_true("Resample" %in% colnames(res_lift))

  expect_s3_class(res_gain, "grouped_gain_df")
  expect_s3_class(res_lift, "grouped_lift_df")
})
