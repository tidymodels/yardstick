test_that("brier_survival_integrated calculations", {
  lung_surv <- data_lung_surv()

  brier_res <- brier_survival(
    data = lung_surv,
    truth = surv_obj,
    .pred
  ) %>%
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

test_that("case weights", {
  lung_surv <- data_lung_surv()
  lung_surv$case_wts <- seq_len(nrow(lung_surv))

  lung_surv <- data_lung_surv()

  brier_res <- brier_survival(
    data = lung_surv,
    truth = surv_obj,
    .pred
  ) %>%
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
