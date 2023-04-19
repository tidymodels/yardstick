test_that("case weights", {
  lung_surv <- data_lung_surv()

  brier_res <- brier_survival(
    data = lung_surv,
    truth = surv_obj,
    .pred
  )

  expect_equal(
    names(brier_res),
    c(".metric", ".estimator", ".eval_time", ".estimate")
  )
})

test_that("case weights", {
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

# riskRegression compare -------------------------------------------------------

test_that("riskRegression equivalent", {
  riskRegression_res <- readRDS(test_path("data/brier_churn_res.rds"))

  yardstick_res <- readRDS(test_path("data/tidy_churn.rds")) %>%
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
