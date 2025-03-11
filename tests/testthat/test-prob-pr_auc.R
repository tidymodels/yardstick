test_that("`event_level = 'second'` works", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- stats::relevel(df_rev$truth, "Class2")

  expect_equal(
    pr_auc_vec(df$truth, df$Class1),
    pr_auc_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that("Multiclass PR AUC", {
  hpc_f1 <- data_hpc_fold1()

  expect_equal(
    pr_auc(hpc_f1, obs, VF:L, estimator = "macro")[[".estimate"]],
    hpc_fold1_macro_metric(pr_auc_binary)
  )
  expect_equal(
    pr_auc(hpc_f1, obs, VF:L, estimator = "macro_weighted")[[".estimate"]],
    hpc_fold1_macro_weighted_metric(pr_auc_binary)
  )
})

# ------------------------------------------------------------------------------

test_that("Two class PR AUC matches sklearn", {
  # Note that these values are different from `MLmetrics::PRAUC()`,
  # see #93 about how duplicates and end points are handled

  sklearn_curve <- read_pydata("py-pr-curve")$binary
  sklearn_auc <- auc(sklearn_curve$recall, sklearn_curve$precision)

  expect_equal(
    pr_auc(two_class_example, truth = "truth", "Class1")[[".estimate"]],
    sklearn_auc
  )
  expect_equal(
    pr_auc(two_class_example, truth, Class1)[[".estimate"]],
    sklearn_auc
  )
})

test_that("Two class weighted PR AUC matches sklearn", {
  sklearn_curve <- read_pydata("py-pr-curve")$case_weight$binary
  sklearn_auc <- auc(sklearn_curve$recall, sklearn_curve$precision)

  two_class_example$weight <- read_weights_two_class_example()

  expect_equal(
    pr_auc(two_class_example, truth, Class1, case_weights = weight)[[
      ".estimate"
    ]],
    sklearn_auc
  )
})

test_that("grouped multiclass (one-vs-all) weighted example matches expanded equivalent", {
  hpc_cv$weight <- rep(1, times = nrow(hpc_cv))
  hpc_cv$weight[c(100, 200, 150, 2)] <- 5

  hpc_cv <- dplyr::group_by(hpc_cv, Resample)

  hpc_cv_expanded <- hpc_cv[
    vec_rep_each(seq_len(nrow(hpc_cv)), times = hpc_cv$weight),
  ]

  expect_identical(
    pr_auc(hpc_cv, obs, VF:L, case_weights = weight, estimator = "macro"),
    pr_auc(hpc_cv_expanded, obs, VF:L, estimator = "macro")
  )

  expect_identical(
    pr_auc(
      hpc_cv,
      obs,
      VF:L,
      case_weights = weight,
      estimator = "macro_weighted"
    ),
    pr_auc(hpc_cv_expanded, obs, VF:L, estimator = "macro_weighted")
  )
})

test_that("works with hardhat case weights", {
  df <- two_class_example

  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    pr_auc_vec(df$truth, df$Class1, case_weights = imp_wgt)
  )

  expect_no_error(
    pr_auc_vec(df$truth, df$Class1, case_weights = freq_wgt)
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
    pr_auc_vec(cp_truth, estimate)
  )
})
