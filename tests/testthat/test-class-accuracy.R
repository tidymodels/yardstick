context("Accuracy")

# ------------------------------------------------------------------------------

lst <- data_three_class()
three_class <- lst$three_class
three_class_tb <- lst$three_class_tb

test_that('Three class', {
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

# sklearn compare --------------------------------------------------------------

py_res <- read_pydata("py-accuracy")
r_metric <- accuracy

test_that('Two class - sklearn equivalent', {
  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$multiclass
  )
})
