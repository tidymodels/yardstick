test_that("roc_curve_auc() calculations", {
  skip_if_not_installed("tidyr")

  survival_curve <- roc_curve_survival(
    lung_surv,
    truth = surv_obj,
    .pred
  ) |>
    dplyr::group_by(.eval_time) |>
    dplyr::summarise(
      .estimate = yardstick:::auc(1 - specificity, sensitivity)
    )

  survival_auc <- roc_auc_survival(
    lung_surv,
    truth = surv_obj,
    .pred
  )

  expect_equal(
    survival_curve$.estimate,
    survival_auc$.estimate
  )
})

# case weights -----------------------------------------------------------------

test_that("case weights are applied", {
  skip_if_not_installed("tidyr")

  wts_res <- lung_surv |>
    dplyr::mutate(wts = hardhat::frequency_weights(rep(1:0, c(128, 100)))) |>
    roc_auc_survival(
      truth = surv_obj,
      .pred,
      case_weights = wts
    )

  subset_res <- lung_surv |>
    dplyr::slice(1:128) |>
    roc_auc_survival(
      truth = surv_obj,
      .pred
    )

  expect_identical(subset_res, wts_res)
})

test_that("works with hardhat case weights", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$case_wts <- rep(2, nrow(lung_surv))

  df <- lung_surv

  df$imp_wgt <- hardhat::importance_weights(lung_surv$case_wts)
  df$freq_wgt <- hardhat::frequency_weights(lung_surv$case_wts)

  expect_no_error(
    roc_auc_survival(df, truth = surv_obj, .pred, case_weights = imp_wgt)
  )

  expect_no_error(
    roc_auc_survival(df, truth = surv_obj, .pred, case_weights = freq_wgt)
  )
})

# self checking ----------------------------------------------------------------

test_that("snapshot equivalent", {
  skip_if_not_installed("tidyr")

  snapshot_res <- readRDS(test_path("data/ref_roc_auc_survival.rds"))

  yardstick_res <- readRDS(test_path("data/tidy_churn.rds")) |>
    roc_auc_survival(
      truth = surv_obj,
      .pred
    )

  expect_equal(
    snapshot_res$.estimate,
    yardstick_res$.estimate
  )

  expect_identical(
    snapshot_res$.eval_time,
    yardstick_res$.eval_time
  )
})

# riskRegression compare -------------------------------------------------------

test_that("riskRegression equivalent", {
  skip_if_not_installed("tidyr")

  riskRegression_res <- readRDS(test_path("data/auc_churn_res.rds"))

  yardstick_res <- readRDS(test_path("data/tidy_churn.rds")) |>
    roc_auc_survival(
      truth = surv_obj,
      .pred
    )

  expect_identical(
    riskRegression_res$times,
    yardstick_res$.eval_time
  )

  expect_true(
    all(abs(riskRegression_res$AUC - yardstick_res$.estimate) < 0.035)
  )
})
