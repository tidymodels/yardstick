# Bad input --------------------------------------------------------------------

test_that('bad args', {
  expect_error(sens(pathology, truth = "pathology", estimate = c("scan", "scan")))
  expect_error(sens(pathology, truth = "patholosgy", estimate = "scan"))
})

test_that("`truth` should be factor", {
  expect_error(
    sens(pathology, 1, factor("A")),
    "`truth` should be a factor",
    class = "dplyr_error"
  )
})

test_that("At least 2 levels in truth", {
  expect_error(
    sens(pathology, factor("A"), factor("A")),
    "`estimator` is binary, only two class `truth` factors are allowed",
    class = "dplyr_error"
  )
})

test_that("Single character values are caught with correct errors", {
  expect_error(
    sens(pathology, "a", factor("A")),
    "`truth` should be a factor",
    class = "dplyr_error"
  )
})

test_that("Bad unquoted input is caught", {
  bad <- rlang::expr(c("a", "b"))
  expect_error(
    sens(pathology, !! bad, factor("A")),
    "`truth` should be a factor",
    class = "dplyr_error"
  )
})

# Bad estimator ----------------------------------------------------------------

test_that("Non-allowed estimator", {
  expect_error(
    sens(pathology, pathology, scan, estimator = "blah"),
    "`estimator` must be one of",
    class = "dplyr_error"
  )
})

test_that("Bad estimator + truth combination", {
  expect_error(
    sens(hpc_cv, obs, pred, estimator = "binary"),
    "`estimator` is binary",
    class = "dplyr_error"
  )
})

test_that("Bad estimator type", {
  expect_error(
    sens(hpc_cv, obs, pred, estimator = 1),
    "`estimator` must be a character",
    class = "dplyr_error"
  )

  expect_error(
    sens(hpc_cv, obs, pred, estimator = c("1", "2")),
    "`estimator` must be length 1",
    class = "dplyr_error"
  )
})

test_that("Numeric matrix in numeric metric", {
  expect_error(
    rmse(solubility_test, matrix(1:5), prediction),
    "`truth` should be a numeric vector",
    class = "dplyr_error"
  )
  expect_error(
    rmse(solubility_test, solubility, matrix(1:5)),
    "`estimate` should be a numeric vector",
    class = "dplyr_error"
  )
})

test_that("Factors with non identical levels", {

  df <- data.frame(
    x = factor(c("a", "b", "c")),
    y = factor(c("a", "b", "b"))
  )

  expect_error(
    sens(df, x, y),
    "`truth` and `estimate` levels must be equivalent.",
    class = "dplyr_error"
  )
})

test_that("Multiple estimate columns for a binary metric", {
  expect_error(
    roc_auc(two_class_example, truth, Class1:Class2),
    "You are using a `binary` metric",
    class = "dplyr_error"
  )
})

test_that("1 estimate column for a multiclass metric", {
  expect_error(
    roc_auc(hpc_cv, obs, VF),
    "The number of levels in `truth`",
    class = "dplyr_error"
  )
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
