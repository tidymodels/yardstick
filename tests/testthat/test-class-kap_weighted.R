# sklearn compare --------------------------------------------------------------

# linear -----------------------------------------------------------------------

py_res <- read_pydata("py-kap-weighted-linear")

test_that('Two class - sklearn equivalent', {
  expect_equal(
    kap_weighted(
      two_class_example, truth, predicted, weight = "linear"
    )[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  expect_equal(
    r_metric(hpc_cv, obs, pred, weight = "linear")[[".estimate"]],
    py_res$multiclass
  )
})

# quadratic --------------------------------------------------------------------

py_res <- read_pydata("py-kap-weighted-quadratic")

test_that('Two class - sklearn equivalent', {
  expect_equal(
    kap_weighted(
      two_class_example, truth, predicted, weight = "quadratic"
    )[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  expect_equal(
    r_metric(hpc_cv, obs, pred, weight = "quadratic")[[".estimate"]],
    py_res$multiclass
  )
})
