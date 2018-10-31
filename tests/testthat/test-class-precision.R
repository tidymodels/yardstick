context("Precision")

# ------------------------------------------------------------------------------

lst <- data_powers()
tabl_2_1 <- lst$tabl_2_1
df_2_1 <- lst$df_2_1

test_that('Two class - Powers paper', {
  expect_equal(
    precision(df_2_1, truth = "truth", estimate = "prediction")[[".estimate"]],
    30/42
  )
  expect_equal(
    precision(tabl_2_1)[[".estimate"]],
    30/42
  )
  expect_equal(
    precision(df_2_1, truth = truth, estimate = pred_na)[[".estimate"]],
    26/37
  )
})

# sklearn compare --------------------------------------------------------------

py_res <- read_pydata("py-precision")
r_metric <- precision

test_that('Two class - sklearn equivalent', {
  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$macro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, "micro")[[".estimate"]],
    py_res$micro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, "macro_weighted")[[".estimate"]],
    py_res$weighted
  )
})
