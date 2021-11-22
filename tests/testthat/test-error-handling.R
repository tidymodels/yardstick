# Bad input --------------------------------------------------------------------

test_that('bad args', {
  expect_error(sens(pathology, truth = "pathology", estimate = c("scan", "scan")))
  expect_error(sens(pathology, truth = "patholosgy", estimate = "scan"))
})

test_that("`truth` should be factor", {
  expect_snapshot((expect_error(
    sens(pathology, 1, factor("A"))
  )))
})

test_that("At least 2 levels in truth", {
  expect_snapshot((expect_error(
    sens(pathology, factor("A"), factor("A"))
  )))
})

test_that("Single character values are caught with correct errors", {
  expect_snapshot((expect_error(
    sens(pathology, "a", factor("A"))
  )))
})

test_that("Bad unquoted input is caught", {
  bad <- rlang::expr(c("a", "b"))

  expect_snapshot((expect_error(
    sens(pathology, !! bad, factor("A"))
  )))
})

# Bad estimator ----------------------------------------------------------------

test_that("Non-allowed estimator", {
  expect_snapshot((expect_error(
    sens(pathology, pathology, scan, estimator = "blah")
  )))
})

test_that("Bad estimator + truth combination", {
  expect_snapshot((expect_error(
    sens(hpc_cv, obs, pred, estimator = "binary")
  )))
})

test_that("Bad estimator type", {
  expect_snapshot((expect_error(
    sens(hpc_cv, obs, pred, estimator = 1)
  )))

  expect_snapshot((expect_error(
    sens(hpc_cv, obs, pred, estimator = c("1", "2"))
  )))
})

test_that("Numeric matrix in numeric metric", {
  expect_snapshot((expect_error(
    rmse(solubility_test, matrix(1:5), prediction)
  )))

  expect_snapshot((expect_error(
    rmse(solubility_test, solubility, matrix(1:5))
  )))
})

test_that("Factors with non identical levels", {
  df <- data.frame(
    x = factor(c("a", "b", "c")),
    y = factor(c("a", "b", "b"))
  )

  expect_snapshot((expect_error(
    sens(df, x, y)
  )))
})

test_that("Multiple estimate columns for a binary metric", {
  expect_snapshot((expect_error(
    roc_auc(two_class_example, truth, Class1:Class2)
  )))
})

test_that("1 estimate column for a multiclass metric", {
  expect_snapshot((expect_error(
    roc_auc(hpc_cv, obs, VF)
  )))
})

test_that("`truth` and `estimate` of different lengths", {
  expect_error(
    rmse_vec(1:5, 1:6),
    "Length of `truth` \\(5\\) and `estimate` \\(6\\) must match."
  )
})

test_that("Missing arguments", {
  expect_error(
    sens(two_class_example),
    "`truth` is missing"
  )
  expect_error(
    sens(two_class_example, truth),
    "`estimate` is missing"
  )
})

test_that("Table with bad format", {
  expect_error(
    sens(as.table(matrix(1:6, 2))),
    "the table must have nrow = ncol"
  )

  expect_error(
    sens(as.table(matrix(1:4, 2, dimnames = list(c("A", "B"), c("A", "C"))))),
    "the table must the same groups in the same order"
  )
})
