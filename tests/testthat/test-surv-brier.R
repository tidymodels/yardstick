test_that('case weights', {
  lung_surv <- data_lung_surv()

  brier_res <- brier_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred
  )

  expect_equal(
    names(brier_res),
    c(".metric", ".estimator", ".eval_time", ".estimate")
  )
})

test_that('case weights', {
  lung_surv <- data_lung_surv()
  lung_surv$case_wts <- rep(2, nrow(lung_surv))

  brier_res <- brier_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred
  )

  brier_res_case_wts <- brier_survival(
    data = lung_surv,
    truth =  surv_obj,
    estimate = .pred,
    case_weights = case_wts
  )

  expect_equal(
    brier_res$.estimate,
    brier_res_case_wts$.estimate
  )
})

# sklearn compare --------------------------------------------------------------

test_that('sklearn equivalent', {
  skip("Wait for pycompare to updated")
  py_res <- read_pydata("py-brier-survival")

  lung_surv <- data_lung_surv()

  brier_res <- lung_surv %>%
    brier_survival(
      truth = surv_obj,
      estimate = .pred
    )

  expect_equal(
    brier_res$.estimate,
    unname(py_res)
  )
})
