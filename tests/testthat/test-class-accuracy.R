# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------

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

# sklearn compare --------------------------------------------------------------

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
