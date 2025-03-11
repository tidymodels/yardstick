test_that("Two class", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    spec(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    54 / 86
  )
  expect_equal(
    spec(path_tbl)[[".estimate"]],
    54 / 86
  )
  expect_equal(
    spec(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    53 / 85
  )
  expect_equal(
    spec(as.matrix(path_tbl))[[".estimate"]],
    54 / 86
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    spec_vec(df$pathology, df$scan),
    spec_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that("Three class", {
  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()

  expect_equal(
    spec(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(spec_binary)
  )
  expect_equal(
    spec(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(spec_binary)
  )
  expect_equal(
    spec(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro, sum(tn) / sum(tn + fn))
  )
})

# ------------------------------------------------------------------------------

test_that("Binary `spec()` returns `NA` with a warning when undefined (tn + fp = 0) (#98)", {
  levels <- c("a", "b")
  truth <- factor(c("a", "a"), levels = levels)
  estimate <- factor(c("a", "b"), levels = levels)

  expect_snapshot(out <- spec_vec(truth, estimate))
  expect_identical(out, NA_real_)
})

test_that("Multiclass `spec()` returns averaged value with `NA`s removed + a warning when undefined (tn + fp = 0) (#98)", {
  levels <- c("a", "b", "c", "d")

  # When `d` is the event we get spec      = 3/3  = (tn = 3, fp = 0)
  # When `a` is the event we get spec      = NA   = (tn = 0, fp = 0)
  # When `b` is the event we get a warning = 1/3  = (tn = 1, fp = 2)
  # When `c` is the event we get a warning = 3/3  = (tn = 3, fp = 0)
  truth <- factor(c("a", "a", "a"), levels = levels)
  estimate <- factor(c("a", "b", "b"), levels = levels)

  expect_snapshot(out <- spec_vec(truth, estimate))
  expect_equal(out, (1 + 1 / 3 + 1) / 3, tolerance = 0.000001)
})

test_that("`NA` is still returned if there are some undefined spec values but `na.rm = FALSE`", {
  levels <- c("a", "b")
  truth <- factor(c("a", "a"), levels = levels)
  estimate <- factor(c("a", NA), levels = levels)
  expect_equal(spec_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(spec_vec(truth, estimate, na_rm = FALSE), NA)
})

# ------------------------------------------------------------------------------

test_that("two class with case weights is correct", {
  df <- data.frame(
    truth = factor(c("x", "y", "y", "y"), levels = c("x", "y")),
    estimate = factor(c("x", "y", "y", "x"), levels = c("x", "y")),
    case_weights = c(1L, 1L, 2L, 3L)
  )

  expect_identical(
    spec(df, truth, estimate, case_weights = case_weights)[[".estimate"]],
    1 / 2
  )

  expect_identical(
    specificity(df, truth, estimate, case_weights = case_weights)[[
      ".estimate"
    ]],
    1 / 2
  )
})

# ------------------------------------------------------------------------------

test_that("`specificity()` has a metric name unique to it (#232)", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_identical(
    spec(pathology, truth = "pathology", estimate = "scan")[[".metric"]],
    "spec"
  )
  expect_identical(
    specificity(pathology, truth = "pathology", estimate = "scan")[[".metric"]],
    "specificity"
  )

  expect_identical(
    spec(path_tbl)[[".metric"]],
    "spec"
  )
  expect_identical(
    specificity(path_tbl)[[".metric"]],
    "specificity"
  )
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    specificity_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    specificity_vec(df$pathology, df$scan, case_weights = freq_wgt)
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
    specificity_vec(fct_truth, cp_estimate),
    specificity_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    specificity_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    specificity_vec(cp_truth, cp_estimate)
  )
})
