# Bad input --------------------------------------------------------------------

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    sens(pathology, truth = "patholosgy", estimate = "scan")
  )
})

test_that("`truth` should be factor", {
  df <- dplyr::tibble(truth = 1, estimate = factor("A"))

  expect_snapshot(
    error = TRUE,
    sens(df, truth, estimate)
  )
})

test_that("At least 2 levels in truth", {
  df <- dplyr::tibble(truth = factor("A"), estimate = factor("A"))

  expect_snapshot(
    error = TRUE,
    sens(df, truth, estimate)
  )
})

test_that("Single character values are caught with correct errors", {
  expect_snapshot(
    error = TRUE,
    sens(pathology, "a", scan)
  )
})

test_that("Bad unquoted input is caught", {
  bad <- rlang::expr(c("a", "b"))

  expect_snapshot(
    error = TRUE,
    sens(pathology, !!bad, scan)
  )
})

# Bad estimator ----------------------------------------------------------------

test_that("Non-allowed estimator", {
  expect_snapshot(
    error = TRUE,
    sens(pathology, pathology, scan, estimator = "blah")
  )
})

test_that("Bad estimator + truth combination", {
  expect_snapshot(
    error = TRUE,
    sens(hpc_cv, obs, pred, estimator = "binary")
  )
})

test_that("Bad estimator type", {
  expect_snapshot(
    error = TRUE,
    sens(hpc_cv, obs, pred, estimator = 1)
  )

  expect_snapshot(
    error = TRUE,
    sens(hpc_cv, obs, pred, estimator = c("1", "2"))
  )
})

test_that("Numeric matrix in numeric metric", {
  solubility_test$a <- matrix(rep(1, nrow(solubility_test)), ncol = 1)

  expect_snapshot(
    error = TRUE,
    rmse(solubility_test, a, prediction)
  )

  expect_snapshot(
    error = TRUE,
    rmse(solubility_test, solubility, a)
  )
})

test_that("Factors with non identical levels", {
  df <- data.frame(
    x = factor(c("a", "b", "c")),
    y = factor(c("a", "b", "b"))
  )

  expect_snapshot(
    error = TRUE,
    sens(df, x, y)
  )
})

test_that("Multiple estimate columns for a binary metric", {
  expect_snapshot(
    error = TRUE,
    roc_auc(two_class_example, truth, Class1:Class2)
  )
})

test_that("1 estimate column for a multiclass metric", {
  expect_snapshot(
    error = TRUE,
    roc_auc(hpc_cv, obs, VF)
  )
})

test_that("`truth` and `estimate` of different lengths", {
  expect_snapshot(
    error = TRUE,
    rmse_vec(1:5, 1:6)
  )
})

test_that("Missing arguments", {
  expect_snapshot(
    error = TRUE,
    sens(two_class_example)
  )
  expect_snapshot(
    error = TRUE,
    sens(two_class_example, truth)
  )
})

test_that("Table with bad format", {
  expect_snapshot(
    error = TRUE,
    sens(as.table(matrix(1:6, 2)))
  )

  expect_snapshot(
    error = TRUE,
    sens(as.table(matrix(1:4, 2, dimnames = list(c("A", "B"), c("A", "C")))))
  )
})
