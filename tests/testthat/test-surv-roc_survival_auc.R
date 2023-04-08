test_that("roc_curve_auc() calculations", {
  survival_curve <- roc_curve_survival(
    lung_surv,
    truth = surv_obj,
    .pred
  ) %>%
    dplyr::group_by(.eval_time) %>%
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

# riskRegression compare -------------------------------------------------------

test_that('riskRegression equivalent', {
  riskRegression_res <- readRDS(test_path("data/auc_churn_res.rds"))

  yardstick_res <- readRDS(test_path("data/rr_churn_data.rds")) %>%
    dplyr::rename(
      .eval_time = times,
      .pred_survival = surv_prob,
      .weight_censored = ipcw
    ) %>%
    dplyr::mutate(
      .weight_censored = dplyr::if_else(status == 0 & time < .eval_time, NA, .weight_censored)
    ) %>%
    tidyr::nest(.pred = -c(ID, time, status, model)) %>%
    dplyr::mutate(surv_obj = survival::Surv(time, status)) %>%
    roc_auc_survival(
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
