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
      factor(c("a", "b", "a"), levels = c("a", "b", "c")),
      factor(c("a", "b", "a"), levels = c("a", "b", "c")),
      case_weights = 1:3,
      estimator = "binary"
    )
  )
})

test_that("check_prob_metric() validates case_weights", {
  expect_snapshot(
    error = TRUE,
    check_prob_metric(
      factor(c("a", "b", "a")),
      matrix(1:6, nrow = 2),
      1:4,
      estimator = "binary"
    )
  )
})

test_that("check_prob_metric() validates inputs", {
  expect_snapshot(
    error = TRUE,
    check_prob_metric(
      factor(c("a", "b", "a")),
      matrix(1:6, nrow = 2),
      1:3,
      estimator = "binary"
    )
  )
})

test_that("check_survival_dynamic_metric() validates case_weights", {
  lung_surv <- data_lung_surv()

  expect_snapshot(
    error = TRUE,
    check_survival_dynamic_metric(
      lung_surv$surv_obj,
      lung_surv$.pred,
      1:51,
      .time = 1:3
    )
  )
})

test_that("check_survival_dynamic_metric() validates inputs", {
  lung_surv <- data_lung_surv()

  expect_snapshot(
    error = TRUE,
    check_survival_dynamic_metric(
      lung_surv$surv_obj,
      lung_surv$inst,
      1:50,
      .time = 1:3
    )
  )
})
