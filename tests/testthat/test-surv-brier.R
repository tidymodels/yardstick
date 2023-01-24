test_that('case weights', {
  .times <- c(100, 500, 1000)

  lung_surv <- data_lung_surv()
  lung_surv$case_wts <- rep(2, nrow(lung_surv))

  brier_res <- brier_survival(lung_surv, surv_obj, .pred, prob_censored, .time = .times)

  brier_res_case_wts <- brier_survival(
    data = lung_surv,
    truth =  surv_obj,
    estimate = .pred,
    censoring_weights = prob_censored,
    case_weights = case_wts,
    .time = .times
  )

  expect_equal(
    brier_res$.estimate[[1]]$.estimate,
    brier_res_case_wts$.estimate[[1]]$.estimate
  )
})

# sklearn compare --------------------------------------------------------------

test_that('sklearn equivalent', {
  py_res <- read_pydata("py-brier-survival")

  .times <- c(100, 500, 1000)

  lung_surv <- data_lung_surv()

  brier_res <- brier_survival(lung_surv, surv_obj, .pred, prob_censored, .time = .times)
  brier_res <- brier_res[[".estimate"]][[1]][[".estimate"]]
  brier_res <- setNames(brier_res, .times)

  expect_equal(
    brier_res,
    py_res
  )
})
