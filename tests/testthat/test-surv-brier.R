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
