test_that("check_numeric_metric() validates case_weights", {
  expect_snapshot(
    error = TRUE,
    check_numeric_metric(1:10, 1:10, 1:11)
  )
})

test_that("check_numeric_metric() validates inputs", {
  expect_snapshot(
    error = TRUE,
    check_numeric_metric(1, "1", 1)
  )
})

test_that("check_class_metric() validates case_weights", {
  expect_snapshot(
    error = TRUE,
    check_class_metric(letters, letters, 1:5)
  )
})

test_that("check_class_metric() validates inputs", {
  expect_snapshot(
    error = TRUE,
    check_class_metric(1, "1", 1)
  )
})

test_that("check_class_metric() validates estimator", {
  expect_snapshot(
    error = TRUE,
    check_class_metric(
      truth = factor(c("a", "b", "a"), levels = c("a", "b", "c")),
      estimate = factor(c("a", "b", "a"), levels = c("a", "b", "c")),
      case_weights = 1:3,
      estimator = "binary"
    )
  )
})

test_that("check_prob_metric() validates case_weights", {
  expect_snapshot(
    error = TRUE,
    check_prob_metric(
      truth = factor(c("a", "b", "a")),
      estimate = matrix(1:6, nrow = 2),
      case_weights = 1:4,
      estimator = "binary"
    )
  )
})

test_that("check_prob_metric() validates inputs", {
  expect_snapshot(
    error = TRUE,
    check_prob_metric(
      truth = factor(c("a", "b", "a")),
      estimate = matrix(1:6, nrow = 2),
      case_weights = 1:3,
      estimator = "binary"
    )
  )
})

test_that("check_dynamic_survival_metric() validates case_weights", {
  lung_surv <- data_lung_surv()

  expect_snapshot(
    error = TRUE,
    check_dynamic_survival_metric(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred,
      censoring_weights = lung_surv$prob_censored,
      case_weights = 1:51,
      .time = c(100, 500, 1000)
    )
  )
})

test_that("check_dynamic_survival_metric() validates censoring_weights", {
  lung_surv <- data_lung_surv()

  expect_snapshot(
    error = TRUE,
    check_dynamic_survival_metric(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred,
      censoring_weights = lung_surv$prob_censored[-1],
      case_weights = 1:50,
      .time = c(100, 500, 1000)
    )
  )
})

test_that("check_dynamic_survival_metric() validates inputs", {
  lung_surv <- data_lung_surv()

  expect_snapshot(
    error = TRUE,
    check_dynamic_survival_metric(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$inst,
      censoring_weights = lung_surv$prob_censored,
      case_weights = 1:50,
      .time = 1:3
    )
  )
})

test_that("check_static_survival_metric() validates case_weights", {
  lung_surv <- data_lung_surv()

  expect_snapshot(
    error = TRUE,
    check_static_survival_metric(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred,
      case_weights = 1:51
    )
  )
})

test_that("check_static_survival_metric() validates inputs", {
  lung_surv <- data_lung_surv()

  expect_snapshot(
    error = TRUE,
    check_static_survival_metric(
      truth = lung_surv$surv_obj,
      estimate = as.character(lung_surv$inst),
      case_weights = 1:50
    )
  )
})
