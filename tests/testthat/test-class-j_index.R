test_that("Two class", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    j_index(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    (231 / 258) + (54 / 86) - 1
  )
  expect_equal(
    j_index(path_tbl)[[".estimate"]],
    (231 / 258) + (54 / 86) - 1
  )
  expect_equal(
    j_index(pathology, pathology, scan)[[".estimate"]],
    (231 / 258) + (54 / 86) - 1
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    j_index_vec(df$pathology, df$scan),
    j_index_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that("Three class", {
  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()

  expect_equal(
    j_index(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(j_index_binary)
  )
  expect_equal(
    j_index(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(j_index_binary)
  )
  expect_equal(
    j_index(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro, sum(tp) / sum(p) + sum(tn) / sum(n) - 1)
  )
})

# ------------------------------------------------------------------------------

test_that("two class with case weights is correct", {
  df <- data.frame(
    truth = factor(c("x", "x", "y"), levels = c("x", "y")),
    estimate = factor(c("x", "y", "x"), levels = c("x", "y")),
    case_weights = c(1L, 10L, 2L)
  )

  expect_identical(
    j_index(df, truth, estimate, case_weights = case_weights)[[".estimate"]],
    -10 / 11
  )
})

# ------------------------------------------------------------------------------

test_that("Binary `j_index()` returns `NA` with a warning when sensitivity is undefined (tp + fn = 0) (#265)", {
  levels <- c("a", "b")
  truth <- factor(c("b", "b"), levels = levels)
  estimate <- factor(c("a", "b"), levels = levels)

  expect_snapshot(
    out <- j_index_vec(truth, estimate)
  )

  expect_identical(out, NA_real_)
})

test_that("Binary `j_index()` returns `NA` with a warning when specificity is undefined (tn + fp = 0) (#265)", {
  levels <- c("a", "b")
  truth <- factor("a", levels = levels)
  estimate <- factor("b", levels = levels)

  expect_snapshot(
    out <- j_index_vec(truth, estimate)
  )

  expect_identical(out, NA_real_)
})

test_that("Multiclass `j_index()` returns averaged value with `NA`s removed + a warning when sensitivity is undefined (tp + fn = 0) (#265)", {
  levels <- c("a", "b", "c")

  truth <- factor(c("a", "b", "b"), levels = levels)
  estimate <- factor(c("a", "b", "c"), levels = levels)

  expect_snapshot(
    out <- j_index_vec(truth, estimate)
  )

  expect_identical(out, 3 / 4)
})

test_that("Multiclass `j_index()` returns averaged value with `NA`s removed + a warning when specificity is undefined (tn + fp = 0) (#265)", {
  levels <- c("a", "b", "c")

  truth <- factor(c("a", "a", "a"), levels = levels)
  estimate <- factor(c("a", "b", "c"), levels = levels)

  expect_snapshot(
    out <- j_index_vec(truth, estimate)
  )

  # In this case it removes everything and we get a NaN,
  # I can't think of any way to get a spec warning and not have this
  expect_identical(out, NaN)
})

test_that("`NA` is still returned if there are some undefined sensitivity values but `na_rm = FALSE`", {
  levels <- c("a", "b", "c")
  truth <- factor(c("a", "b", "b"), levels = levels)
  estimate <- factor(c("a", NA, "c"), levels = levels)
  expect_equal(j_index_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(j_index_vec(truth, estimate, na_rm = FALSE), NA)
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    j_index_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    j_index_vec(df$pathology, df$scan, case_weights = freq_wgt)
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
    j_index_vec(fct_truth, cp_estimate),
    j_index_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    j_index_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    j_index_vec(cp_truth, cp_estimate)
  )
})
