library(dplyr)

set.seed(1311)
three_class <- data.frame(obs = iris$Species,
                          pred = sample(iris$Species, replace = TRUE))
probs <- matrix(runif(150 * 3), nrow = 150)
probs <- t(apply(probs, 1, function(x) x/sum(x)))
colnames(probs) <- levels(iris$Species)
three_class <- cbind(three_class, as.data.frame(probs))


###################################################################

test_that('correct metrics returned', {
  expect_equal(
    metrics(two_class_example, truth, predicted)[[".metric"]],
    c("accuracy", "kap")
  )
  expect_equal(
    metrics(two_class_example, truth, predicted, Class1)[[".metric"]],
    c("accuracy", "kap", "mn_log_loss", "roc_auc")
  )
  expect_equal(
    metrics(three_class, "obs", "pred", setosa, versicolor, virginica)[[".metric"]],
    c("accuracy", "kap", "mn_log_loss", "roc_auc")
  )
  expect_equal(
    metrics(three_class, "obs", "pred", setosa, versicolor, virginica)[[".estimator"]],
    c("multiclass", "multiclass", "multiclass", "hand_till")
  )
  expect_equal(
    metrics(solubility_test, solubility, "prediction")[[".metric"]],
    c("rmse", "rsq", "mae")
  )
})

###################################################################

test_that('bad args', {
  expect_error(
    metrics(two_class_example, truth, Class1)
  )
  expect_error(
    metrics(two_class_example, Class1, truth)
  )
  expect_error(
    metrics(three_class, "obs", "pred", setosa, versicolor)
  )
})

###################################################################

class_res_1 <- bind_rows(
  accuracy(two_class_example, truth, predicted),
  kap(two_class_example, truth, predicted),
  mn_log_loss(two_class_example, truth, Class1),
  roc_auc(two_class_example, truth, Class1)
)

reg_res_1 <- bind_rows(
  rmse(solubility_test, solubility, "prediction"),
  rsq(solubility_test, solubility, prediction),
  mae(solubility_test, solubility, prediction)
)


test_that('correct results', {

  class_idx <- which(class_res_1$.metric %in% c("accuracy", "kap"))

  expect_equal(
    metrics(two_class_example, truth, predicted)[[".estimate"]],
    class_res_1[class_idx,][[".estimate"]]
  )
  expect_equal(
    metrics(two_class_example, truth, predicted, Class1)[[".estimate"]],
    class_res_1[[".estimate"]]
  )
  expect_equal(
    metrics(solubility_test, solubility, prediction)[[".estimate"]],
    reg_res_1[[".estimate"]]
  )
})

###################################################################

test_that('numeric metric sets', {

  reg_set <- metric_set(rmse, rsq, mae)

  expect_equal(
    reg_set(solubility_test, solubility, prediction),
    reg_res_1
  )
  # ensure helpful messages are printed
  expect_error(
    metric_set(rmse, "x")
  )

  # Can mix class and class prob together
  mixed_set <- metric_set(accuracy, roc_auc)
  expect_error(
    mixed_set(two_class_example, truth, Class1, estimate = predicted),
    NA
  )
})

test_that('mixing bad metric sets', {
  expect_error(
    metric_set(rmse, accuracy)
  )
})

test_that('can mix class and class prob metrics together', {
  expect_error(
    mixed_set <- metric_set(accuracy, roc_auc),
    NA
  )
  expect_error(
    mixed_set(two_class_example, truth, Class1, estimate = predicted),
    NA
  )
})

test_that("can supply `event_level` even with metrics that don't use it", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- relevel(df_rev$truth, "Class2")
  df_rev$predicted <- relevel(df_rev$predicted, "Class2")

  # accuracy doesn't use it, and doesn't have it as an argument
  set <- metric_set(accuracy, recall, roc_auc)

  expect_equal(
    as.data.frame(set(df, truth, Class1, estimate = predicted)),
    as.data.frame(set(df_rev, truth, Class1, estimate = predicted, event_level = "second"))
  )
})

test_that('metric set functions are classed', {
  expect_is(
    metric_set(accuracy, roc_auc),
    "class_prob_metric_set"
  )
  expect_is(
    metric_set(mae),
    "numeric_metric_set"
  )
  expect_is(
    metric_set(accuracy, roc_auc),
    "metric_set"
  )
  expect_is(
    metric_set(mae),
    "metric_set"
  )
})

test_that('metric set functions retain class/prob metric functions', {
  fns <- attr(metric_set(accuracy, roc_auc), "metrics")

  expect_equal(
    names(fns),
    c("accuracy", "roc_auc")
  )

  expect_equal(
    class(fns[[1]]),
    c("class_metric", "metric", "function")
  )

  expect_equal(
    class(fns[[2]]),
    c("prob_metric", "metric", "function")
  )

  expect_equal(
    vapply(fns, function(fn) attr(fn, "direction"), character(1)),
    c(accuracy = "maximize", roc_auc = "maximize")
  )
})

test_that('metric set functions retain numeric metric functions', {
  fns <- attr(metric_set(mae, rmse), "metrics")

  expect_equal(
    names(fns),
    c("mae", "rmse")
  )

  expect_equal(
    class(fns[[1]]),
    c("numeric_metric", "metric", "function")
  )

  expect_equal(
    class(fns[[2]]),
    c("numeric_metric", "metric", "function")
  )

  expect_equal(
    vapply(fns, function(fn) attr(fn, "direction"), character(1)),
    c(mae = "minimize", rmse = "minimize")
  )
})

test_that("`metric_set()` labeling remove namespaces", {
  x <- metric_set(yardstick::mase, rmse)
  expect_identical(names(attr(x, "metrics")), c("mase", "rmse"))
})

test_that("print metric_set works", {
  verify_output(test_path("test-print-metric_set.txt"), {
    multi_metric <- metric_set(rmse, rsq, ccc)
    print(multi_metric)
  })
})

test_that("metric_set can be coerced to a tibble", {
  x <- metric_set(roc_auc, pr_auc, accuracy)
  expect_s3_class(as_tibble(x), "tbl_df")
})

test_that("`metric_set()` errors contain env name for unknown functions (#128)", {
  foobar <- function() {}

  # Store env name in `name` attribute for `environmentName()` to find it
  env <- rlang::new_environment(parent = globalenv())
  attr(env, "name") <- "test"

  rlang::fn_env(foobar) <- env

  expect_error(
    metric_set(accuracy, foobar, sens, rlang::abort),
    "class [(]accuracy, sens[)]"
  )
  expect_error(
    metric_set(accuracy, foobar, sens, rlang::abort),
    "other [(]foobar <test>, abort <namespace:rlang>[)]"
  )
})

test_that("`metric_set()` gives an informative error for a single non-metric function (#181)", {
  foobar <- function() {}

  # Store env name in `name` attribute for `environmentName()` to find it
  env <- rlang::new_environment(parent = globalenv())
  attr(env, "name") <- "test"
  rlang::fn_env(foobar) <- env

  expect_error(
    metric_set(foobar),
    "other (foobar <test>)",
    fixed = TRUE
  )
})
