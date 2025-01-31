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

test_that("check_ordered_prob_metric() validates case_weights", {
  expect_snapshot(
    error = TRUE,
    check_ordered_prob_metric(
      ordered(c("a", "b", "a")),
      matrix(1:6, nrow = 2),
      1:4,
      estimator = "binary"
    )
  )
})

test_that("check_ordered_prob_metric() validates inputs", {
  expect_snapshot(
    error = TRUE,
    check_ordered_prob_metric(
      ordered(c("a", "b", "a")),
      matrix(1:6, nrow = 2),
      1:3,
      estimator = "binary"
    )
  )
})

test_that("check_static_survival_metric() validates case_weights", {
  lung_surv <- data_lung_surv()

  expect_snapshot(
    error = TRUE,
    check_static_survival_metric(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred_survival,
      case_weights = 1:151
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
      case_weights = 1:150
    )
  )
})
