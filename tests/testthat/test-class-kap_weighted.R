# ------------------------------------------------------------------------------

lst <- data_three_class()
three_class <- lst$three_class
three_class_tb <- lst$three_class_tb

# Fail for things other than ordered factors. -----------------------------

test_that('Truth unordered', {
  weight <- "linear"
  expect_error(
    kap_weighted(
      three_class, truth = "obs", estimate = "pred", weight = weight
    )[[".estimate"]],
    "`truth` should be a ordered"
  )
})

three_class$pred <- factor(
  three_class$pred, levels = levels(three_class$pred), ordered = TRUE
)

test_that('Estimate unordered', {
  weight <- "linear"
  expect_error(
    kap_weighted(
      three_class, truth = "pred", estimate = "obs", weight = weight
    )[[".estimate"]],
    "`estimate` should be a ordered"
  )
})


three_class$obs <- factor(
  three_class$obs, levels = levels(three_class$pred), ordered = TRUE
)
three_class$pred_na <- factor(
  three_class$pred_na, levels = levels(three_class$pred), ordered = TRUE
)

# Expected values were hand-calculated for additional comparisons. These will
# mostly protect against regressions.

test_that('Three class, linear', {
  weight <- "linear"

  expect_equal(
    kap_weighted(
      three_class, truth = "obs", estimate = "pred", weight = weight
    )[[".estimate"]],
    kap_weighted(
      three_class, truth = "pred", estimate = "obs", weight = weight
    )[[".estimate"]]
  )
  expect_equal(
    kap_weighted(
      three_class, truth = "obs", estimate = "pred", weight = weight
    )[[".estimate"]],
    0.03940887
  )
  expect_equal(
    kap_weighted(three_class_tb, weight = weight)[[".estimate"]],
    0.03940887
  )
  expect_equal(
    kap_weighted(as.matrix(three_class_tb), weight = weight)[[".estimate"]],
    0.03940887
  )
  # This one is off in a zillionth decimal place for some reason.
  expect_equal(
    kap_weighted(three_class, obs, pred_na, weight = weight)[[".estimate"]],
    -0.1529223,
    tolerance = 1e-7
  )
  expect_equal(
    colnames(
      kap_weighted(
        three_class, truth = "obs", estimate = "pred", weight = weight
      )
    ),
    c(".metric", ".estimator", ".estimate")
  )
  expect_equal(
    kap_weighted(
      three_class, truth = "obs", estimate = "pred", weight = weight
    )[[".metric"]],
    "kap_weighted"
  )
})

test_that('Three class, quadratic', {
  weight <- "quadratic"
  expect_equal(
    kap_weighted(
      three_class, truth = "obs", estimate = "pred", weight = weight
    )[[".estimate"]],
    0.02912621
  )
  expect_equal(
    kap_weighted(three_class_tb, weight = weight)[[".estimate"]],
    0.02912621
  )
  expect_equal(
    kap_weighted(as.matrix(three_class_tb), weight = weight)[[".estimate"]],
    0.02912621
  )
  # This one is off in a zillionth decimal place for some reason.
  expect_equal(
    kap_weighted(three_class, obs, pred_na, weight = weight)[[".estimate"]],
    -0.1488489,
    tolerance = 1e-7
  )
  expect_equal(
    colnames(
      kap_weighted(
        three_class, truth = "obs", estimate = "pred", weight = weight
      )
    ),
    c(".metric", ".estimator", ".estimate")
  )
  expect_equal(
    kap_weighted(
      three_class, truth = "obs", estimate = "pred", weight = weight
    )[[".metric"]],
    "kap_weighted"
  )
})

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
