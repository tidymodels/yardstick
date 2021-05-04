test_that("`estimate` has to be structurally correct", {
  expect_error(
    validate_surv_estimate(list(1)),
    "must be a data frame"
  )
  expect_error(
    validate_surv_estimate(list(data.frame(x = 1))),
    "must have two columns"
  )
  expect_error(
    validate_surv_estimate(list(data.frame(x = 1, y = 2))),
    "must have column names of '.time' and '.pred_survival'"
  )
  expect_error(
    validate_surv_estimate(list(data.frame(.time = "x", .pred_survival = 2))),
    "`estimate[$].time` must be numeric"
  )
  expect_error(
    validate_surv_estimate(list(data.frame(.time = 1, .pred_survival = "x"))),
    "`estimate[$].pred_survival` must be numeric"
  )
  expect_error(
    validate_surv_estimate(list(data.frame(.time = NA_integer_, .pred_survival = 1))),
    "`estimate[$].time` must not be missing"
  )
})
