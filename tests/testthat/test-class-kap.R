context("Kappa")

# ------------------------------------------------------------------------------

test_that("two class produces identical results regardless of level order", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- relevel(df_rev$pathology, "norm")
  df_rev$scan <- relevel(df_rev$scan, "norm")

  expect_equal(
    kap_vec(df$pathology, df$scan),
    kap_vec(df_rev$pathology, df_rev$scan)
  )
})

# ------------------------------------------------------------------------------

# expected results from e1071::classAgreement(three_class_tb)$kappa
# e1071::classAgreement(table(three_class$pred_na, three_class$obs))$kappa

test_that('Three class', {
  lst <- data_three_class()
  three_class <- lst$three_class
  three_class_tb <- lst$three_class_tb

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

test_that('Two class - sklearn equivalent', {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$multiclass
  )
})

test_that("linear weighting - sklearn equivalent", {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  expect_equal(
    r_metric(two_class_example, truth, predicted, weighting = "linear")[[".estimate"]],
    py_res$linear_binary
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, weighting = "linear")[[".estimate"]],
    py_res$linear_multiclass
  )
})

test_that("quadratic weighting - sklearn equivalent", {
  py_res <- read_pydata("py-kap")
  r_metric <- kap

  expect_equal(
    r_metric(two_class_example, truth, predicted, weighting = "quadratic")[[".estimate"]],
    py_res$quadratic_binary
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, weighting = "quadratic")[[".estimate"]],
    py_res$quadratic_multiclass
  )
})
