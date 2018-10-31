context("Kappa")

# ------------------------------------------------------------------------------

lst <- data_three_class()
three_class <- lst$three_class
three_class_tb <- lst$three_class_tb

# expected results from e1071::classAgreement(three_class_tb)$kappa
# e1071::classAgreement(table(three_class$pred_na, three_class$obs))$kappa

test_that('Three class', {
  expect_equal(
    kap(three_class, truth = "obs", estimate = "pred")[[".estimate"]],
    0.05
  )
  expect_equal(
    kap(three_class_tb)[[".estimate"]],
    0.05
  )
  expect_equal(
    kap(as.matrix(three_class_tb))[[".estimate"]],
    0.05
  )
  expect_equal(
    kap(three_class, obs, pred_na)[[".estimate"]],
    -0.1570248
  )
  expect_equal(
    colnames(kap(three_class, truth = "obs", estimate = "pred")),
    c(".metric", ".estimator", ".estimate")
  )
  expect_equal(
    kap(three_class, truth = "obs", estimate = "pred")[[".metric"]],
    "kap"
  )
})

# sklearn compare --------------------------------------------------------------

py_res <- read_pydata("py-kap")
r_metric <- kap

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
