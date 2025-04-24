test_that("brier_survival_integrated calculations", {
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

test_that("brier_survival_integrated calculations", {
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

test_that("case weights", {
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
