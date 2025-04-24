test_that("ppv", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    ppv(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    0.87832,
    tolerance = .001
  )
  expect_equal(
    ppv(path_tbl)[[".estimate"]],
    0.87832,
    tolerance = .001
  )
  expect_equal(
    ppv(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    0.87744,
    tolerance = .001
  )
  expect_equal(
    ppv(pathology, truth = pathology, estimate = "scan", prevalence = .5)[[
      ".estimate"
    ]],
    0.70642,
    tolerance = .001
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    ppv_vec(df$pathology, df$scan),
    ppv_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that("Three class", {
  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()
  micro$prev <- (micro$tp + micro$fn) / (micro$p + micro$n)

  expect_equal(
    ppv(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(ppv_binary)
  )
  expect_equal(
    ppv(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(ppv_binary)
  )
  # PPV = Precision when no prevalence is given
  expect_equal(
    ppv(multi_ex, estimator = "micro")[[".estimate"]],
    precision(multi_ex, estimator = "micro")[[".estimate"]]
  )
  expect_equal(
    ppv(multi_ex, estimator = "micro")[[".estimate"]],
    with(
      micro,
      ((sum(tp) / sum(p)) * sum(prev)) /
        ((sum(tp) / sum(p)) *
          sum(prev) +
          ((1 - sum(tn) / sum(n)) * sum((1 - prev))))
    )
  )
  # Prevalence defined by the user. Defined once for all levels?
  expect_equal(
    ppv(multi_ex, estimator = "micro", prevalence = .4)[[".estimate"]],
    with(
      micro,
      ((sum(tp) / sum(p)) * sum(.4)) /
        ((sum(tp) / sum(p)) *
          sum(.4) +
          ((1 - sum(tn) / sum(n)) * sum((1 - .4))))
    )
  )
})

# ------------------------------------------------------------------------------

test_that("Binary `ppv()` returns `NA` with a warning when `sens()` is undefined (tp + fn = 0) (#101)", {
  levels <- c("a", "b")
  truth <- factor(c("b", "b"), levels = levels)
  estimate <- factor(c("a", "b"), levels = levels)

  expect_snapshot(
    out <- ppv_vec(truth, estimate)
  )

  expect_identical(out, NA_real_)
})

# ------------------------------------------------------------------------------

test_that("Two class weighted - sklearn equivalent", {
  py_res <- read_pydata("py-ppv")
  r_metric <- ppv

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, predicted, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )
})

test_that("Multi class weighted - sklearn equivalent", {
  py_res <- read_pydata("py-ppv")
  r_metric <- ppv

  hpc_cv$weights <- read_weights_hpc_cv()

  expect_equal(
    r_metric(hpc_cv, obs, pred, estimator = "macro", case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$macro
  )
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    ppv_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    ppv_vec(df$pathology, df$scan, case_weights = freq_wgt)
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
    ppv_vec(fct_truth, cp_estimate),
    ppv_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    ppv_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    ppv_vec(cp_truth, cp_estimate)
  )
})
