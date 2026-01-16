test_that("Calculations are correct", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  brier_res <- brier_survival(
    data = lung_surv,
    truth = surv_obj,
    .pred
  ) |>
    dplyr::summarise(
      .estimate = yardstick:::auc(.eval_time, .estimate) / max(.eval_time)
    )

  brier_integrated_res <- brier_survival_integrated(
    data = lung_surv,
    truth = surv_obj,
    .pred
  )

  expect_equal(
    brier_res$.estimate,
    brier_integrated_res$.estimate
  )
})

test_that("All interfaces gives the same results", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  expect_equal(
    brier_survival_integrated_vec(
      truth = lung_surv$surv_obj,
      lung_surv$.pred
    ),
    brier_survival_integrated(
      lung_surv,
      truth = surv_obj,
      .pred
    )[[".estimate"]]
  )
})

test_that("Calculations handles NAs", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$surv_obj[1:10] <- NA

  expect_equal(
    brier_survival_integrated_vec(
      truth = lung_surv$surv_obj,
      lung_surv$.pred
    ),
    0.157160962
  )
})

test_that("Case weights calculations are correct", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$case_wts <- seq_len(nrow(lung_surv))

  lung_surv <- data_lung_surv()

  brier_res <- brier_survival(
    data = lung_surv,
    truth = surv_obj,
    .pred
  ) |>
    dplyr::summarise(
      .estimate = yardstick:::auc(.eval_time, .estimate) / max(.eval_time)
    )

  brier_integrated_res <- brier_survival_integrated(
    data = lung_surv,
    truth = surv_obj,
    .pred
  )

  expect_equal(
    brier_res$.estimate,
    brier_integrated_res$.estimate
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
    brier_survival_integrated(
      df,
      truth = surv_obj,
      .pred,
      case_weights = imp_wgt
    )
  )

  expect_no_error(
    brier_survival_integrated(
      df,
      truth = surv_obj,
      .pred,
      case_weights = freq_wgt
    )
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    brier_survival_integrated_vec(1, 1, na_rm = "yes")
  )
})

test_that("Errors on too few evaluation times", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  lung_surv$.pred <- lapply(lung_surv$.pred, function(x) x[1, ])

  expect_snapshot(
    error = TRUE,
    brier_survival_integrated(
      data = lung_surv,
      truth = surv_obj,
      .pred
    )
  )
})
