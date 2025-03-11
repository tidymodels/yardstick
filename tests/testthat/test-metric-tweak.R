test_that("can tweak a numeric metric", {
  mase12 <- metric_tweak("mase12", mase, m = 12)
  result <- mase12(solubility_test, solubility, prediction)

  expect_identical(
    result[[".estimate"]],
    mase(solubility_test, solubility, prediction, m = 12)[[".estimate"]]
  )

  expect_identical(
    result[[".metric"]],
    "mase12"
  )
})

test_that("can tweak a class metric", {
  f_meas2 <- metric_tweak("f_meas2", f_meas, beta = 2)
  result <- f_meas2(two_class_example, truth, predicted)

  expect_identical(
    result[[".estimate"]],
    f_meas(two_class_example, truth, predicted, beta = 2)[[".estimate"]]
  )

  expect_identical(
    result[[".metric"]],
    "f_meas2"
  )
})

test_that("can tweak a class metric that doesn't use `estimator`", {
  accuracy2 <- metric_tweak("accuracy2", accuracy)
  result <- accuracy2(two_class_example, truth, predicted)

  expect_identical(
    result[[".estimate"]],
    accuracy(two_class_example, truth, predicted)[[".estimate"]]
  )

  expect_identical(
    result[[".metric"]],
    "accuracy2"
  )
})

test_that("can tweak a class prob metric", {
  two_class_example$truth[1] <- NA

  roc_auc2 <- metric_tweak("roc_auc2", roc_auc, na_rm = FALSE)
  result <- roc_auc2(two_class_example, truth, Class1)

  expect_identical(
    result[[".estimate"]],
    roc_auc(two_class_example, truth, Class1, na_rm = FALSE)[[".estimate"]]
  )

  expect_identical(
    result[[".metric"]],
    "roc_auc2"
  )
})

test_that("can tweak a class prob metric that doesn't use `estimator`", {
  costs <- dplyr::tribble(
    ~truth,
    ~estimate,
    ~cost,
    "Class1",
    "Class2",
    1,
    "Class2",
    "Class1",
    2
  )

  classification_cost2 <- metric_tweak(
    "classification_cost2",
    classification_cost,
    costs = costs
  )

  result <- classification_cost2(two_class_example, truth, Class1)

  expect_identical(
    result[[".estimate"]],
    classification_cost(two_class_example, truth, Class1, costs = costs)[[
      ".estimate"
    ]]
  )

  expect_identical(
    result[[".metric"]],
    "classification_cost2"
  )
})

test_that("can combine tweaked metrics into a metric set", {
  f_meas2 <- metric_tweak("f_meas2", f_meas, beta = 2)
  ppv2 <- metric_tweak("ppv2", ppv, prevalence = .4)
  roc_auc2 <- metric_tweak("roc_auc2", roc_auc)

  set <- metric_set(f_meas2, ppv2, roc_auc2)
  result <- set(two_class_example, truth, Class1, estimate = predicted)

  expect_identical(
    result[[".metric"]],
    c("f_meas2", "ppv2", "roc_auc2")
  )
})

test_that("can set `na_rm` in the tweaked metric", {
  df <- data.frame(x = c(1, 2, NA))
  rmse_na <- metric_tweak("rmse_na", rmse, na_rm = FALSE)

  expect_identical(
    rmse_na(df, x, x)[[".estimate"]],
    NA_real_
  )
})

test_that("can set `estimator` in the tweaked metric", {
  roc_auc_mw <- metric_tweak(
    "roc_auc_mw",
    roc_auc,
    estimator = "macro_weighted"
  )

  expect_identical(
    roc_auc_mw(hpc_cv, obs, VF:L)[[".estimate"]],
    roc_auc(hpc_cv, obs, VF:L, estimator = "macro_weighted")[[".estimate"]]
  )
})

test_that("cannot use protected names", {
  expect_snapshot(
    error = TRUE,
    metric_tweak("f_meas2", f_meas, data = 2)
  )
  expect_snapshot(
    error = TRUE,
    metric_tweak("f_meas2", f_meas, truth = 2)
  )
  expect_snapshot(
    error = TRUE,
    metric_tweak("f_meas2", f_meas, estimate = 2)
  )
})

test_that("`name` must be a string", {
  expect_snapshot(
    error = TRUE,
    metric_tweak(1, f_meas, beta = 2)
  )
})

test_that("`fn` must be a metric function", {
  expect_snapshot(
    error = TRUE,
    metric_tweak(
      "foo",
      function() {
      },
      beta = 2
    )
  )
})

test_that("All `...` must be named", {
  expect_snapshot(
    error = TRUE,
    metric_tweak("foo", accuracy, 1)
  )
})
