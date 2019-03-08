context("F Measure")

# ------------------------------------------------------------------------------

lst <- data_powers()
tabl_2_1 <- lst$tabl_2_1
df_2_1 <- lst$df_2_1

test_that('Two class - Powers paper', {
  expect_equal(
    f_meas(df_2_1, truth = "truth", estimate = "prediction")[[".estimate"]],
    0.5882353,
    tol = 0.0001
  )
  expect_equal(
    f_meas(tabl_2_1)[[".estimate"]],
    0.5882353,
    tol = 0.0001
  )
  expect_equal(
    f_meas(df_2_1, truth = truth, estimate = pred_na)[[".estimate"]],
    0.5652174,
    tol = 0.0001
  )
})

# ------------------------------------------------------------------------------
# Issue #77

test_that("`NA` values propagate from binary `precision()`", {

  truth <- factor(c(rep("a", 2), rep("b", 2)))
  estimate <- factor(rep("b", length(truth)), levels(truth))

  expect_equal(
    precision_vec(truth, estimate),
    f_meas_vec(truth, estimate)
  )

})

test_that("`NA` values propagate from binary `recall()`", {

  estimate <- factor(c(rep("a", 2), rep("b", 2)))
  truth <- factor(rep("b", length(estimate)), levels(estimate))

  expect_equal(
    recall_vec(truth, estimate),
    f_meas_vec(truth, estimate)
  )

})

test_that("`NA` values propagate from multiclass `precision()`", {

  truth <- factor(c(rep("a", 2), rep("b", 2)))
  estimate <- factor(rep("b", length(truth)), levels(truth))

  expect_equal(
    precision_vec(truth, estimate, estimator = "macro"),
    f_meas_vec(truth, estimate, estimator = "macro")
  )

})

test_that("`NA` values propagate from multiclass `recall()`", {

  estimate <- factor(c(rep("a", 2), rep("b", 2)))
  truth <- factor(rep("b", length(estimate)), levels(estimate))

  expect_equal(
    recall_vec(truth, estimate, estimator = "macro"),
    f_meas_vec(truth, estimate, estimator = "macro")
  )

})

# sklearn compare --------------------------------------------------------------

py_res <- read_pydata("py-f_meas")
py_res_.5 <- read_pydata("py-f_meas_beta_.5")
r_metric <- f_meas

test_that('Two class - sklearn equivalent', {
  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
  expect_equal(
    r_metric(two_class_example, truth, predicted, beta = .5)[[".estimate"]],
    py_res_.5$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$macro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, estimator = "micro")[[".estimate"]],
    py_res$micro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, estimator = "macro_weighted")[[".estimate"]],
    py_res$weighted
  )

  expect_equal(
    r_metric(hpc_cv, obs, pred, beta = .5)[[".estimate"]],
    py_res_.5$macro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, estimator = "micro", beta = .5)[[".estimate"]],
    py_res_.5$micro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, "macro_weighted", beta = .5)[[".estimate"]],
    py_res_.5$weighted
  )
})


