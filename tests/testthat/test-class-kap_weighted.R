# sklearn compare --------------------------------------------------------------

# I need ordered factors for my functions.
two_class_ordered <- two_class_example
two_class_ordered$truth <- factor(
  two_class_ordered$truth,
  levels = levels(two_class_ordered$truth),
  ordered = TRUE
)
two_class_ordered$predicted <- factor(
  two_class_ordered$predicted,
  levels = levels(two_class_ordered$truth),
  ordered = TRUE
)

hpc_cv_ordered <- hpc_cv
hpc_cv_ordered$pred <- factor(
  hpc_cv_ordered$pred,
  levels = levels(hpc_cv_ordered$pred),
  ordered = TRUE
)
hpc_cv_ordered$obs <- factor(
  hpc_cv_ordered$obs,
  levels = levels(hpc_cv_ordered$pred),
  ordered = TRUE
)

# linear -----------------------------------------------------------------------

py_res <- read_pydata("py-kap-weighted-linear")

test_that('Two class - sklearn equivalent', {
  expect_equal(
    kap_weighted(
      two_class_ordered, truth, predicted, weight = "linear"
    )[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  expect_equal(
    kap_weighted(hpc_cv_ordered, obs, pred, weight = "linear")[[".estimate"]],
    py_res$multiclass
  )
})

# quadratic --------------------------------------------------------------------

py_res <- read_pydata("py-kap-weighted-quadratic")

test_that('Two class - sklearn equivalent', {
  expect_equal(
    kap_weighted(
      two_class_ordered, truth, predicted, weight = "quadratic"
    )[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  expect_equal(
    kap_weighted(hpc_cv_ordered, obs, pred, weight = "quadratic")[[".estimate"]],
    py_res$multiclass
  )
})
