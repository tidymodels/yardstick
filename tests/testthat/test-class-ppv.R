test_that("Calculations are correct - two class", {
  lst <- data_altman()
  pathology <- lst$pathology

  expect_equal(
    ppv_vec(truth = pathology$pathology, estimate = pathology$scan),
    0.87832,
    tolerance = 0.001
  )
})

test_that("Calculations are correct - three class", {
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
})

test_that("All interfaces gives the same results", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl
  path_mat <- as.matrix(path_tbl)

  exp <- ppv_vec(pathology$pathology, pathology$scan)

  expect_identical(
    ppv(path_tbl)[[".estimate"]],
    exp
  )
  expect_identical(
    ppv(path_mat)[[".estimate"]],
    exp
  )
  expect_identical(
    ppv(pathology, truth = pathology, estimate = scan)[[".estimate"]],
    exp
  )
})

test_that("Calculations handles NAs", {
  lst <- data_altman()
  pathology <- lst$pathology

  expect_equal(
    ppv_vec(truth = pathology$pathology, estimate = pathology$scan_na),
    0.87744,
    tolerance = 0.001
  )

  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()
  micro$prev <- (micro$tp + micro$fn) / (micro$p + micro$n)
  # Prevalence defined by the user. Defined once for all levels?
  expect_equal(
    ppv(multi_ex, estimator = "micro", prevalence = .4)[[".estimate"]],
    with(
      micro,
      ((sum(tp) / sum(p)) * sum(0.4)) /
        ((sum(tp) / sum(p)) *
          sum(0.4) +
          ((1 - sum(tn) / sum(n)) * sum((1 - 0.4))))
    )
  )
})

test_that("Case weights calculations are correct", {
  py_res <- read_pydata("py-ppv")
  r_metric <- ppv

  two_class_example$weights <- read_weights_two_class_example()

  expect_equal(
    r_metric(two_class_example, truth, predicted, case_weights = weights)[[
      ".estimate"
    ]],
    py_res$case_weight$binary
  )

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

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    ppv_vec(1, 1, na_rm = "yes")
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

test_that("prevalence works", {
  lst <- data_altman()
  pathology <- lst$pathology

  expect_equal(
    ppv_vec(
      truth = pathology$pathology,
      estimate = pathology$scan,
      prevalence = 0.5
    ),
    0.70642,
    tolerance = 0.001
  )
})

test_that("Binary returns `NA` with a warning when results are undefined (#98)", {
  # sensitivity - (tp + fn = 0)
  levels <- c("a", "b")
  truth <- factor(c("b", "b"), levels = levels)
  estimate <- factor(c("a", "b"), levels = levels)

  expect_snapshot(
    out <- ppv_vec(truth, estimate)
  )
  expect_identical(out, NA_real_)
})

test_that("bad argument check", {
  expect_snapshot(
    error = TRUE,
    ppv_vec(1, 1, prevalence = "yes")
  )
})

test_that("range values are correct", {
  direction <- metric_direction(ppv)
  range <- metric_range(ppv)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = factor(c("A", "A", "B", "B", "B")),
    off = factor(c("B", "B", "A", "A", "A"))
  )

  expect_equal(
    ppv_vec(df$truth, df$truth),
    perfect
  )

  if (direction == "minimize") {
    expect_gt(ppv_vec(df$truth, df$off), perfect)
    expect_lte(ppv_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lt(ppv_vec(df$truth, df$off), perfect)
    expect_gte(ppv_vec(df$truth, df$off), worst)
  }
})
