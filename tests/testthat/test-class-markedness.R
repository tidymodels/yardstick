test_that("Calculations are correct - two class", {
  lst <- data_altman()
  pathology <- lst$pathology

  # Markedness = precision + inverse_precision - 1
  # Confusion matrix (prediction rows, truth columns):
  #          abnorm  norm
  # abnorm     231    32
  # norm        27    54
  #
  # precision = TP / (TP + FP) = 231 / (231 + 32) = 231 / 263
  # inverse_precision = TN / (TN + FN) = 54 / (54 + 27) = 54 / 81
  expect_equal(
    markedness_vec(truth = pathology$pathology, estimate = pathology$scan),
    (231 / 263) + (54 / 81) - 1
  )
})

test_that("Calculations are correct - three class", {
  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()

  expect_equal(
    markedness(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(markedness_binary)
  )
  expect_equal(
    markedness(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(markedness_binary)
  )
  # Micro: sum(tp) / sum(tp + fp) + sum(tn) / sum(tn + fn) - 1
  expect_equal(
    markedness(multi_ex, estimator = "micro")[[".estimate"]],
    with(
      micro,
      sum(tp) / (sum(tp) + sum(fp)) + sum(tn) / (sum(tn) + sum(fn)) - 1
    )
  )
})

test_that("All interfaces gives the same results", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl
  path_mat <- unclass(path_tbl)

  exp <- markedness_vec(pathology$pathology, pathology$scan)

  expect_identical(
    markedness(path_tbl)[[".estimate"]],
    exp
  )
  expect_identical(
    markedness(path_mat)[[".estimate"]],
    exp
  )
  expect_identical(
    markedness(pathology, truth = pathology, estimate = scan)[[".estimate"]],
    exp
  )
})

test_that("Calculations handles NAs", {
  lst <- data_altman()
  pathology <- lst$pathology

  # With 3 NAs removed, counts change slightly
  # Confusion matrix (prediction rows, truth columns):
  #          abnorm  norm
  # abnorm     230    32
  # norm        26    53
  #
  # precision = 230 / (230 + 32) = 230 / 262
  # inverse_precision = 53 / (53 + 26) = 53 / 79
  expect_equal(
    markedness_vec(truth = pathology$pathology, estimate = pathology$scan_na),
    (230 / 262) + (53 / 79) - 1
  )
})

test_that("Case weights calculations are correct", {
  df <- data.frame(
    truth = factor(c("x", "x", "y"), levels = c("x", "y")),
    estimate = factor(c("x", "y", "x"), levels = c("x", "y")),
    case_weights = c(1L, 10L, 2L)
  )

  # Weighted confusion matrix (prediction rows, truth columns):
  #        x   y
  # x      1   2
  # y     10   0
  # precision = 1/(1+2) = 1/3
  # inverse_precision = 0/(10+0) = 0
  # markedness = 1/3 + 0 - 1 = -2/3
  expect_equal(
    markedness(df, truth, estimate, case_weights = case_weights)[[".estimate"]],
    -2 / 3
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
    markedness_vec(fct_truth, cp_estimate),
    markedness_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    markedness_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    markedness_vec(cp_truth, cp_estimate)
  )
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    markedness_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    markedness_vec(df$pathology, df$scan, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    markedness_vec(1, 1, na_rm = "yes")
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    markedness_vec(df$pathology, df$scan),
    markedness_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})

test_that("Binary returns `NA` with a warning when precision undefined (#98)", {
  # precision - (tp + fp = 0) - no predicted positives
  levels <- c("a", "b")
  truth <- factor(c("b", "b"), levels = levels)
  estimate <- factor(c("b", "b"), levels = levels)

  expect_snapshot(
    out <- markedness_vec(truth, estimate)
  )
  expect_identical(out, NA_real_)
})

test_that("Binary returns `NA` with a warning when inverse precision undefined (#98)", {
  # inverse precision - (tn + fn = 0) - no predicted negatives
  levels <- c("a", "b")
  truth <- factor(c("a", "a"), levels = levels)
  estimate <- factor(c("a", "a"), levels = levels)

  expect_snapshot(
    out <- markedness_vec(truth, estimate)
  )
  expect_identical(out, NA_real_)
})

test_that("Multiclass returns averaged value with warning when results undefined (#98)", {
  # precision undefined for one class - no predicted events
  levels <- c("a", "b", "c")

  truth <- factor(c("a", "b", "b"), levels = levels)
  estimate <- factor(c("a", "b", "c"), levels = levels)

  expect_snapshot(
    out <- markedness_vec(truth, estimate)
  )
  expect_true(is.finite(out))
})

test_that("`NA` is still returned if there are some undefined values but `na_rm = FALSE`", {
  levels <- c("a", "b", "c")
  truth <- factor(c("a", "b", "b"), levels = levels)
  estimate <- factor(c("a", NA, "c"), levels = levels)
  expect_equal(markedness_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(markedness_vec(truth, estimate, na_rm = FALSE), NA)
})

test_that("range values are correct", {
  direction <- metric_direction(markedness)
  range <- metric_range(markedness)
  perfect <- ifelse(direction == "minimize", range[1], range[2])

  df <- tibble::tibble(
    truth = factor(c("A", "A", "B", "B", "B")),
    off = factor(c("B", "B", "A", "A", "A"))
  )

  expect_equal(
    markedness_vec(df$truth, df$truth),
    perfect
  )

  if (direction == "minimize") {
    expect_gt(markedness_vec(df$truth, df$off), perfect)
  }
  if (direction == "maximize") {
    expect_lt(markedness_vec(df$truth, df$off), perfect)
  }
})
