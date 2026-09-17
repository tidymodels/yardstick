test_that("Calculations are correct", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  exp <- tibble::tibble(
    .eval_time = seq(100, 500, by = 100),
    .estimate = c(
      0.109098582291278,
      0.19406636627294568,
      0.2189497730815321,
      0.2221958144612071,
      0.1973541926841589356
    ),
  )

  expect_equal(
    brier_survival_vec(
      truth = lung_surv$surv_obj,
      lung_surv$.pred
    ),
    exp
  )
})

test_that("All interfaces gives the same results", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  expect_equal(
    brier_survival_vec(
      truth = lung_surv$surv_obj,
      lung_surv$.pred
    ),
    brier_survival(
      lung_surv,
      truth = surv_obj,
      .pred
    ) |>
      dplyr::select(.eval_time, .estimate)
  )
})

test_that("Calculations handles NAs", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$surv_obj[1:10] <- NA

  exp <- tibble::tibble(
    .eval_time = seq(100, 500, by = 100),
    .estimate = c(
      0.112923256037501432147,
      0.196848806275167015,
      0.215711528709764982503,
      0.220112187368286139,
      0.19334132135553464
    ),
  )

  expect_equal(
    brier_survival_vec(
      truth = lung_surv$surv_obj,
      lung_surv$.pred
    ),
    exp
  )
})

test_that("Case weights calculations are correct", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$case_wts <- rep(2, nrow(lung_surv))

  brier_res <- brier_survival(
    data = lung_surv,
    truth = surv_obj,
    .pred
  )

  brier_res_case_wts <- brier_survival(
    data = lung_surv,
    truth = surv_obj,
    .pred,
    case_weights = case_wts
  )

  expect_equal(
    brier_res$.estimate,
    brier_res_case_wts$.estimate
  )
})

test_that("works with hardhat case weights", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$case_wts <- rep(2, nrow(lung_surv))

  df <- lung_surv

  df$imp_wgt <- hardhat::importance_weights(lung_surv$case_wts)
  df$freq_wgt <- hardhat::frequency_weights(lung_surv$case_wts)

  expect_no_error(
    brier_survival(df, truth = surv_obj, .pred, case_weights = imp_wgt)
  )

  expect_no_error(
    brier_survival(df, truth = surv_obj, .pred, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    brier_survival_vec(1, 1, na_rm = "yes")
  )
})

test_that("riskRegression equivalent", {
  skip_if_not_installed("tidyr")

  riskRegression_res <- readRDS(test_path("data/brier_churn_res.rds"))

  yardstick_res <- readRDS(test_path("data/tidy_churn.rds")) |>
    brier_survival(
      truth = surv_obj,
      .pred
    )

  expect_identical(
    riskRegression_res$times,
    yardstick_res$.eval_time
  )

  expect_equal(
    riskRegression_res$Brier,
    yardstick_res$.estimate
  )
})

test_that("range values are correct", {
  skip_if_not_installed("tidyr")

  direction <- metric_direction(brier_survival)
  range <- metric_range(brier_survival)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  lung_surv <- data_lung_surv()

  result <- brier_survival_vec(
    truth = lung_surv$surv_obj,
    lung_surv$.pred
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
