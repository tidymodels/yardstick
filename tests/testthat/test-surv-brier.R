test_that('case weights', {
  lung_surv <- data_lung_surv() %>% dplyr::filter(.time == 100)
  lung_surv$case_wts <- rep(2, nrow(lung_surv))

  brier_res <- brier_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_survival,
    censoring_weights = prob_censored,
    .time = .time
  )

  brier_res_case_wts <- brier_survival(
    data = lung_surv,
    truth =  surv_obj,
    estimate = .pred_survival,
    censoring_weights = prob_censored,
    case_weights = case_wts,
    .time = .time
  )

  expect_equal(
    brier_res$.estimate,
    brier_res_case_wts$.estimate
  )
})

# sklearn compare --------------------------------------------------------------

test_that('sklearn equivalent', {
  skip("wait for lung_surv correction")
  py_res <- read_pydata("py-brier-survival")

  .times <- c(100, 500, 1000)

  lung_surv <- data_lung_surv()

  brier_res <- lung_surv %>%
    dplyr::group_by(.time) %>%
    brier_survival(
    truth = surv_obj,
    estimate = .pred_survival,
    censoring_weights = prob_censored,
    .time = .time
  )

  expect_equal(
    brier_res$.estimate,
    unname(py_res)
  )
})
