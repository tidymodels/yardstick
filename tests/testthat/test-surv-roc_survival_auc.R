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

# self checking ----------------------------------------------------------------

test_that('snapshot equivalent', {
  snapshot_res <- readRDS(test_path("data/ref_roc_auc_survival.rds"))

  yardstick_res <- readRDS(test_path("data/tidy_churn.rds")) %>%
    roc_auc_survival(
      truth = surv_obj,
      .pred
    )

  expect_identical(snapshot_res, yardstick_res)
})

# riskRegression compare -------------------------------------------------------

test_that('riskRegression equivalent', {
  riskRegression_res <- readRDS(test_path("data/auc_churn_res.rds"))

  yardstick_res <- readRDS(test_path("data/tidy_churn.rds")) %>%
    roc_auc_survival(
      truth = surv_obj,
      .pred
    )

  expect_identical(
    riskRegression_res$times,
    yardstick_res$.eval_time
  )

  expect_true(
    all(abs(riskRegression_res$AUC - yardstick_res$.estimate) < 0.09)
  )
})
