# Basic tests ------------------------------------------------------------------

# known answers

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
    gain_curve(df, truth, estimate),
    "`truth` should be a factor but a numeric was supplied."
  )
})

test_that("quasiquotation works", {
  df <- data.frame(
    truth = factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No")),
    estimate = c(.9, .8, .7, .68, .5)
  )

  tru <- as.name("truth")

  expect_error(gain_curve(df, !!tru, estimate), regexp = NA)
  expect_error(gain_curve(df, "truth", estimate), regexp = NA)
})

# ------------------------------------------------------------------------------

test_that("`event_level = 'second'` works", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- stats::relevel(df_rev$truth, "Class2")

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

  expect_equal(nrow(gain_df), 4L)

  # .n_events should be 2 for the 2 .9+Yes predictions
  expect_equal(gain_df$.n_events[2], 2)
})

# Multiclass -------------------------------------------------------------------

test_that("Multiclass structure is correct", {
  res_gain <- gain_curve(hpc_cv, obs, VF:L)

  expect_true(".level" %in% colnames(res_gain))

  expect_s3_class(res_gain, "gain_df")
})

test_that("Grouped structure is correct", {
  hpc_g <- dplyr::group_by(hpc_cv, Resample)

  res_gain <- gain_curve(hpc_g, obs, VF:L)

  expect_true("Resample" %in% colnames(res_gain))

  expect_s3_class(res_gain, "grouped_gain_df")
})

# Case weights -----------------------------------------------------------------

test_that("gain_curve() works with case weights (ideally, frequency weights)", {
  df <- data.frame(
    truth = factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No")),
    estimate = c(.9, .8, .7, .68, .5),
    weight = c(2, 1, 1, 3, 2)
  )

  out <- gain_curve(df, truth, estimate, case_weights = weight)

  # Manually computed and verified
  expect <- dplyr::tibble(
    .n = c(0, 2, 3, 4, 7, 9),
    .n_events = c(0, 2, 3, 3, 6, 6),
    .percent_tested = .n / 9 * 100,
    .percent_found = .n_events / 6 * 100
  )
  class(expect) <- class(out)

  expect_s3_class(out, "gain_df")
  expect_identical(out, expect)
})

test_that("gain_curve() works with case weights and multiclass (ideally, frequency weights)", {
  df <- data.frame(
    truth = factor(c("Yes", "Yes", "No", "Maybe", "Yes", "Maybe", "No"), levels = c("Yes", "No", "Maybe")),
    Yes = c(.9, .8, .7, .68, .5, .7, .3),
    No = c(.05, .05, .2, .2, .2, .1, .6),
    Maybe = c(.05, .15, .1, .12, .3, .2, .1),
    weight = c(2, 1, 1, 3, 2, 5, 2)
  )

  out <- gain_curve(df, truth, Yes, No, Maybe, case_weights = weight)

  # Manually computed and verified
  expect <- dplyr::tibble(
    .level = c(
      rep("Yes", 7),
      rep("No", 5),
      rep("Maybe", 7)
    ),
    .n = c(
      0, 2, 3, 9, 12, 14, 16,
      0, 2, 8, 13, 16,
      0, 2, 7, 8, 11, 14, 16
    ),
    .n_events = c(
      0, 2, 3, 3, 3, 5, 5,
      0, 2, 3, 3, 3,
      0, 0, 5, 5, 8, 8, 8
    )
  )
  expect <- dplyr::group_by(expect, .level)
  expect <- dplyr::mutate(
    expect,
    .percent_tested = .n / max(.n) * 100,
    .percent_found = .n_events / max(.n_events) * 100
  )
  expect <- dplyr::ungroup(expect)
  class(expect) <- class(out)

  expect_s3_class(out, "gain_df")
  expect_identical(out, expect)
})
