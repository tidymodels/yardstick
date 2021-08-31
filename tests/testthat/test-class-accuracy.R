test_that("two class produces identical results regardless of level order", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- relevel(df_rev$pathology, "norm")
  df_rev$scan <- relevel(df_rev$scan, "norm")

  expect_equal(
    accuracy_vec(df$pathology, df$scan),
    accuracy_vec(df_rev$pathology, df_rev$scan)
  )
})

test_that('Three class', {
  lst <- data_three_class()
  three_class <- lst$three_class
  three_class_tb <- lst$three_class_tb

  expect_equal(
    accuracy(three_class, truth = "obs", estimate = "pred")[[".estimate"]],
    (24 + 17 + 14)/150
  )
  expect_equal(
    accuracy(three_class_tb)[[".estimate"]],
    (24 + 17 + 14)/150
  )
  expect_equal(
    accuracy(as.matrix(three_class_tb))[[".estimate"]],
    (24 + 17 + 14)/150
  )
  expect_equal(
    accuracy(three_class, obs, pred_na)[[".estimate"]],
    (11 + 10 + 11)/140
  )
  expect_equal(
    colnames(accuracy(three_class, truth = "obs", estimate = "pred")),
    c(".metric", ".estimator", ".estimate")
  )
  expect_equal(
    accuracy(three_class, truth = "obs", estimate = "pred")[[".metric"]],
    "accuracy"
  )
})

test_that("two class with case weights is correct", {
  df <- data.frame(
    truth = factor(c("x", "x", "y"), levels = c("x", "y")),
    estimate = factor(c("x", "y", "x"), levels = c("x", "y")),
    case_weights = c(1L, 1L, 2L)
  )

  # 1 correct, 2 wrong. Normally 1/3 accuracy, but one of the wrong
  # values is weighted 2x so we get 1/4.
  expect_identical(
    accuracy(df, truth, estimate, case_weights = case_weights)[[".estimate"]],
    1/4
  )
})

test_that("missing values in case weights are considered by `na_rm`", {
  truth <- factor(c("x", "y"), levels = c("x", "y"))
  estimate <- factor(c("x", "x"), levels = c("x", "y"))
  case_weights <- c(1, NA)

  expect_identical(
    accuracy_vec(truth, estimate, case_weights = case_weights),
    1
  )

  expect_identical(
    accuracy_vec(truth, estimate, case_weights = case_weights, na_rm = FALSE),
    NA_real_
  )
})

test_that("case weights are validated", {
  truth <- factor(c("x", "y"), levels = c("x", "y"))
  estimate <- factor(c("x", "x"), levels = c("x", "y"))

  expect_error(
    accuracy_vec(truth, estimate, case_weights = 1),
    "`case_weights` (1) must have the same length as `truth` (2).",
    fixed = TRUE
  )

  expect_error(
    accuracy_vec(truth, estimate, case_weights = c("x", "y")),
    "`case_weights` must be an integer or double vector."
  )
})

test_that('Two class - sklearn equivalent', {
  py_res <- read_pydata("py-accuracy")
  r_metric <- accuracy

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  py_res <- read_pydata("py-accuracy")
  r_metric <- accuracy

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$multiclass
  )
})
