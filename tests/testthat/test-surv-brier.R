test_that('case weights', {
  lung_surv <- data_lung_surv() %>% dplyr::filter(eval_time == 100)
  lung_surv$case_wts <- rep(2, nrow(lung_surv))

  brier_res <- brier_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_survival,
    censoring_weights = prob_censored,
    eval_time = eval_time
  )

  brier_res_case_wts <- brier_survival(
    data = lung_surv,
    truth =  surv_obj,
    estimate = .pred_survival,
    censoring_weights = prob_censored,
    case_weights = case_wts,
    eval_time = eval_time
  )

  expect_equal(
    brier_res$.estimate,
    brier_res_case_wts$.estimate
  )
})

# sklearn compare --------------------------------------------------------------

test_that('sklearn equivalent', {
  py_res <- read_pydata("py-brier-survival")

  eval_times <- c(100, 500, 1000)

  lung_surv <- data_lung_surv()

  brier_res <- lung_surv %>%
    dplyr::group_by(eval_time) %>%
    brier_survival(
    truth = surv_obj,
    estimate = .pred_survival,
    censoring_weights = prob_censored,
    eval_time = eval_time
  )

  expect_equal(
    brier_res$.estimate,
    unname(py_res)
  )
})
