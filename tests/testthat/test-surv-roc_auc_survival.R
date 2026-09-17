test_that("Calculations are correct", {
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

test_that("All interfaces gives the same results", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  expect_equal(
    unclass(roc_curve_survival_vec(
      truth = lung_surv$surv_obj,
      lung_surv$.pred
    )),
    unclass(roc_curve_survival(
      lung_surv,
      truth = surv_obj,
      .pred
    ))
  )
})

test_that("Calculations handles NAs", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$surv_obj[1:10] <- NA

  expect_equal(
    roc_curve_survival_vec(
      truth = lung_surv$surv_obj,
      lung_surv$.pred
    ),
    roc_curve_survival_vec(
      truth = lung_surv$surv_obj[-(1:10)],
      lung_surv$.pred[-(1:10)]
    )
  )
})

test_that("na_rm = FALSE errors if missing values are present", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$surv_obj[1:10] <- NA

  expect_snapshot(
    error = TRUE,
    roc_curve_survival_vec(
      truth = lung_surv$surv_obj,
      lung_surv$.pred,
      na_rm = FALSE
    )
  )
})

test_that("Case weights calculations are correct", {
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

test_that("snapshot equivalent", {
  # self checking to avoid breakage over time
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

test_that("range values are correct", {
  skip_if_not_installed("tidyr")

  direction <- metric_direction(roc_auc_survival)
  range <- metric_range(roc_auc_survival)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  lung_surv <- data_lung_surv()

  result <- roc_auc_survival(
    lung_surv,
    truth = surv_obj,
    .pred
  )

  if (direction == "minimize") {
    expect_true(all(result$.estimate >= perfect))
    expect_true(all(result$.estimate <= worst))
  }
  if (direction == "maximize") {
    expect_true(all(result$.estimate >= worst))
    expect_true(all(result$.estimate <= perfect))
  }
})
