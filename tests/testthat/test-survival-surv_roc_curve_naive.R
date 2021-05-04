test_that("censored individuals before `.time` are removed", {
  skip_if_not_installed("survival")

  # Expect first element to be dropped (2 < 3 and event = 0)
  truth <- survival::Surv(c(2, 5, 2, 3), c(0, 0, 1, 1))

  estimate <- list(
    data.frame(.time = 3, .pred_survival = 0.2),
    data.frame(.time = 3, .pred_survival = 0.4),
    data.frame(.time = 3, .pred_survival = 0.6),
    data.frame(.time = 3, .pred_survival = 0.8)
  )

  expect_identical(
    surv_roc_curve_naive_vec(truth, estimate),
    surv_roc_curve_naive_vec(truth[-1], estimate[-1])
  )
})

test_that("removing all censored individuals can remove all rows", {
  skip_if_not_installed("survival")

  truth <- survival::Surv(c(1, 2), c(0, 0))

  estimate <- list(
    data.frame(.time = 3, .pred_survival = 0.5),
    data.frame(.time = 3, .pred_survival = 0.5)
  )

  # TODO: `roc_curve_binary()` should handle this case better by returning
  # `NA` with a warning
  expect_error(surv_roc_curve_naive_vec(truth, estimate))
})

test_that("`truth` has to be a Surv object", {
  expect_error(
    surv_roc_curve_naive_vec(integer(), list()),
    "should be a Surv"
  )
})

test_that("`estimate` has to be a list", {
  skip_if_not_installed("survival")

  x <- survival::Surv(1, 2)

  expect_error(
    surv_roc_curve_naive_vec(x, 1),
    "should be a list"
  )
})
