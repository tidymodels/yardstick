test_that("metric_set() - class", {
  exp <- dplyr::bind_rows(
    accuracy(two_class_example, truth, predicted),
    sensitivity(two_class_example, truth, predicted),
    specificity(two_class_example, truth, predicted)
  )

  set <- metric_set(accuracy, sensitivity, specificity)

  expect_equal(
    set(two_class_example, truth = truth, estimate = predicted),
    exp
  )
})

test_that("metric_set() - prob", {
  exp <- dplyr::bind_rows(
    mn_log_loss(two_class_example, truth, Class1),
    roc_auc(two_class_example, truth, Class1),
    brier_class(two_class_example, truth, Class1)
  )

  set <- metric_set(mn_log_loss, roc_auc, brier_class)

  expect_equal(
    set(two_class_example, truth = truth, Class1),
    exp
  )
})

test_that("metric_set() - ordered prob", {
  two_class_example$truth <- as.ordered(two_class_example$truth)

  exp <- dplyr::bind_rows(
    ranked_prob_score(two_class_example, truth, Class1:Class2),
    ranked_prob_score(two_class_example, truth, Class1:Class2)
  )

  set <- metric_set(ranked_prob_score, ranked_prob_score)

  expect_equal(
    set(two_class_example, truth = truth, Class1:Class2),
    exp
  )
})

test_that("metric_set() - numeric", {
  exp <- dplyr::bind_rows(
    rmse(solubility_test, solubility, prediction),
    rsq(solubility_test, solubility, prediction),
    mae(solubility_test, solubility, prediction)
  )

  set <- metric_set(rmse, rsq, mae)

  expect_equal(
    set(solubility_test, solubility, prediction),
    exp
  )
})

test_that("metric_set() - static survival", {
  exp <- dplyr::bind_rows(
    concordance_survival(lung_surv, truth = surv_obj, estimate = .pred_time),
    concordance_survival(lung_surv, truth = surv_obj, estimate = .pred_time)
  )

  set <- metric_set(concordance_survival, concordance_survival)

  expect_equal(
    set(lung_surv, truth = surv_obj, estimate = .pred_time),
    exp
  )
})

test_that("metric_set() - dynamic survival", {
  exp <- dplyr::bind_rows(
    brier_survival(lung_surv, truth = surv_obj, .pred),
    roc_auc_survival(lung_surv, truth = surv_obj, .pred)
  )

  set <- metric_set(brier_survival, roc_auc_survival)

  expect_equal(
    set(lung_surv, truth = surv_obj, .pred),
    exp
  )
})

test_that("metric_set() - integrated survival", {
  exp <- dplyr::bind_rows(
    brier_survival_integrated(lung_surv, truth = surv_obj, .pred),
    brier_survival_integrated(lung_surv, truth = surv_obj, .pred)
  )

  set <- metric_set(brier_survival_integrated, brier_survival_integrated)

  expect_equal(
    set(lung_surv, truth = surv_obj, .pred),
    exp
  )
})

test_that("metric_set() - linear predictor survival", {
  exp <- dplyr::bind_rows(
    royston_survival(lung_surv, truth = surv_obj, .pred_linear_pred),
    royston_survival(lung_surv, truth = surv_obj, .pred_linear_pred)
  )

  set <- metric_set(royston_survival, royston_survival)

  expect_equal(
    set(lung_surv, truth = surv_obj, estimate = .pred_linear_pred),
    exp
  )
})

test_that("metric_set() - quantile", {
  quantile_levels <- c(0.2, 0.4, 0.6, 0.8)
  pred1 <- 1:4
  pred2 <- 8:11
  example <- dplyr::tibble(
    preds = hardhat::quantile_pred(rbind(pred1, pred2), quantile_levels),
    truth = c(3.3, 7.1)
  )

  exp <- dplyr::bind_rows(
    weighted_interval_score(example, truth = truth, preds),
    weighted_interval_score(example, truth = truth, preds)
  )

  set <- metric_set(weighted_interval_score, weighted_interval_score)

  expect_equal(
    set(example, truth = truth, estimate = preds),
    exp
  )
})

test_that("metric_set() - can mix class and prob", {
  exp <- dplyr::bind_rows(
    accuracy(two_class_example, truth, predicted),
    roc_auc(two_class_example, truth, Class1)
  )

  set <- metric_set(accuracy, roc_auc)

  expect_equal(
    set(two_class_example, truth = truth, Class1, estimate = predicted),
    exp
  )
})

test_that("metric_set() - can mix class and orderedprob", {
  two_class_example$truth <- as.ordered(two_class_example$truth)

  exp <- dplyr::bind_rows(
    accuracy(two_class_example, truth, predicted),
    ranked_prob_score(two_class_example, truth, Class1:Class2)
  )

  set <- metric_set(accuracy, ranked_prob_score)

  expect_equal(
    set(two_class_example, truth = truth, Class1:Class2, estimate = predicted),
    exp
  )
})

test_that("metric_set() - can mix prob and orderedprob", {
  hpc_cv$obs <- as.ordered(hpc_cv$obs)

  exp <- dplyr::bind_rows(
    roc_auc(hpc_cv, obs, VF:L),
    ranked_prob_score(hpc_cv, obs, VF:L)
  )

  set <- metric_set(roc_auc, ranked_prob_score)

  expect_equal(
    set(hpc_cv, obs, VF:L),
    exp
  )
})

test_that("metric_set() - can mix class, prob, and orderedprob", {
  hpc_cv$obs <- as.ordered(hpc_cv$obs)

  exp <- dplyr::bind_rows(
    accuracy(hpc_cv, obs, pred),
    roc_auc(hpc_cv, obs, VF:L),
    ranked_prob_score(hpc_cv, obs, VF:L)
  )

  set <- metric_set(accuracy, roc_auc, ranked_prob_score)

  expect_equal(
    set(hpc_cv, obs, VF:L, estimate = pred),
    exp
  )
})

test_that("metric_set() - can mix static and dynamic survival", {
  exp <- dplyr::bind_rows(
    brier_survival(lung_surv, truth = surv_obj, .pred),
    concordance_survival(lung_surv, truth = surv_obj, estimate = .pred_time)
  )

  set <- metric_set(concordance_survival, brier_survival)

  expect_equal(
    set(lung_surv, truth = surv_obj, .pred, estimate = .pred_time),
    exp
  )
})

test_that("metric_set() - can mix static and integrated survival", {
  exp <- dplyr::bind_rows(
    brier_survival_integrated(lung_surv, truth = surv_obj, .pred),
    concordance_survival(lung_surv, truth = surv_obj, estimate = .pred_time)
  )

  set <- metric_set(brier_survival_integrated, concordance_survival)

  expect_equal(
    set(lung_surv, truth = surv_obj, .pred, estimate = .pred_time),
    exp
  )
})

test_that("metric_set() - can mix dynamic and integrated survival", {
  exp <- dplyr::bind_rows(
    brier_survival(lung_surv, truth = surv_obj, .pred),
    brier_survival_integrated(lung_surv, truth = surv_obj, .pred)
  )

  set <- metric_set(brier_survival, brier_survival_integrated)

  expect_equal(
    set(lung_surv, truth = surv_obj, .pred, estimate = .pred_time),
    exp
  )
})

test_that("metric_set() - can mix static and linear predictor survival", {
  exp <- dplyr::bind_rows(
    concordance_survival(lung_surv, truth = surv_obj, estimate = .pred_time),
    royston_survival(lung_surv, truth = surv_obj, .pred_linear_pred)
  )

  set <- metric_set(concordance_survival, royston_survival)

  expect_equal(
    set(
      lung_surv,
      truth = surv_obj,
      estimate = c(static = .pred_time, linear_pred = .pred_linear_pred)
    ),
    exp
  )
})

test_that("metric_set() - can mix dynamic and linear predictor survival", {
  exp <- dplyr::bind_rows(
    brier_survival(lung_surv, truth = surv_obj, .pred),
    royston_survival(lung_surv, truth = surv_obj, .pred_linear_pred)
  )

  set <- metric_set(brier_survival, royston_survival)

  expect_equal(
    set(lung_surv, truth = surv_obj, .pred, estimate = .pred_linear_pred),
    exp
  )
})

test_that("metric_set() - can mix integrated and linear predictor survival", {
  exp <- dplyr::bind_rows(
    brier_survival_integrated(lung_surv, truth = surv_obj, .pred),
    royston_survival(lung_surv, truth = surv_obj, .pred_linear_pred)
  )

  set <- metric_set(brier_survival_integrated, royston_survival)

  expect_equal(
    set(lung_surv, truth = surv_obj, .pred, estimate = .pred_linear_pred),
    exp
  )
})

test_that("metric_set() - can mix static, dynamic, integrated, and linear predictor survival", {
  exp <- dplyr::bind_rows(
    brier_survival(lung_surv, truth = surv_obj, .pred),
    brier_survival_integrated(lung_surv, truth = surv_obj, .pred),
    concordance_survival(lung_surv, truth = surv_obj, estimate = .pred_time),
    royston_survival(lung_surv, truth = surv_obj, .pred_linear_pred)
  )

  set <- metric_set(
    concordance_survival,
    brier_survival,
    brier_survival_integrated,
    royston_survival
  )

  expect_equal(
    set(
      lung_surv,
      truth = surv_obj,
      .pred,
      estimate = c(static = .pred_time, linear_pred = .pred_linear_pred)
    ),
    exp
  )
})

test_that("metric set functions are classed", {
  expect_s3_class(
    metric_set(accuracy),
    c("class_prob_metric_set", "metric_set")
  )
  expect_s3_class(
    metric_set(roc_auc),
    c("class_prob_metric_set", "metric_set")
  )
  expect_s3_class(
    metric_set(ranked_prob_score),
    c("class_prob_metric_set", "metric_set")
  )
  expect_s3_class(
    metric_set(rmse),
    c("numeric_metric_set", "metric_set")
  )
  expect_s3_class(
    metric_set(concordance_survival),
    c("survival_metric_set", "metric_set")
  )
  expect_s3_class(
    metric_set(brier_survival),
    c("survival_metric_set", "metric_set")
  )
  expect_s3_class(
    metric_set(brier_survival_integrated),
    c("survival_metric_set", "metric_set")
  )
  expect_s3_class(
    metric_set(royston_survival),
    c("survival_metric_set", "metric_set")
  )
  expect_s3_class(
    metric_set(weighted_interval_score),
    c("quantile_metric_set", "metric_set")
  )
  expect_s3_class(
    metric_set(accuracy, roc_auc, ranked_prob_score),
    c("class_prob_metric_set", "metric_set")
  )
  expect_s3_class(
    metric_set(
      concordance_survival,
      brier_survival,
      brier_survival_integrated,
      royston_survival
    ),
    c("survival_metric_set", "metric_set")
  )
})

test_that("print metric_set works", {
  expect_snapshot(
    metric_set(accuracy)
  )
  expect_snapshot(
    metric_set(roc_auc)
  )
  expect_snapshot(
    metric_set(ranked_prob_score)
  )
  expect_snapshot(
    metric_set(rmse)
  )
  expect_snapshot(
    metric_set(concordance_survival)
  )
  expect_snapshot(
    metric_set(brier_survival)
  )
  expect_snapshot(
    metric_set(brier_survival_integrated)
  )
  expect_snapshot(
    metric_set(royston_survival)
  )
  expect_snapshot(
    metric_set(weighted_interval_score)
  )
  expect_snapshot(
    metric_set(accuracy, roc_auc, ranked_prob_score)
  )
  expect_snapshot(
    metric_set(
      concordance_survival,
      brier_survival,
      brier_survival_integrated,
      royston_survival
    )
  )
})

test_that("metric_tweak and metric_set plays nicely together (#351)", {
  skip_if_not_installed("tidyr")

  # Classification
  multi_ex <- data_three_by_three()

  ref <- dplyr::bind_rows(
    j_index(multi_ex, estimator = "macro"),
    j_index(multi_ex, estimator = "micro")
  )

  j_index_macro <- metric_tweak("j_index", j_index, estimator = "macro")
  j_index_micro <- metric_tweak("j_index", j_index, estimator = "micro")

  expect_identical(
    metric_set(j_index_macro, j_index_micro)(multi_ex),
    ref
  )

  # Probability
  ref <- dplyr::bind_rows(
    roc_auc(two_class_example, truth, Class1, event_level = "first"),
    roc_auc(two_class_example, truth, Class1, event_level = "second")
  )

  roc_auc_first <- metric_tweak("roc_auc", roc_auc, event_level = "first")
  roc_auc_second <- metric_tweak("roc_auc", roc_auc, event_level = "second")

  expect_identical(
    metric_set(roc_auc_first, roc_auc_second)(two_class_example, truth, Class1),
    ref
  )

  # Ordered Probability
  two_class_example_ordered <- two_class_example
  two_class_example_ordered$truth <- as.ordered(two_class_example_ordered$truth)

  ref <- dplyr::bind_rows(
    ranked_prob_score(
      two_class_example_ordered,
      truth,
      Class1:Class2,
      na_rm = TRUE
    ),
    ranked_prob_score(
      two_class_example_ordered,
      truth,
      Class1:Class2,
      na_rm = FALSE
    )
  )

  ranked_prob_score_true <- metric_tweak(
    "ranked_prob_score",
    ranked_prob_score,
    na_rm = TRUE
  )
  ranked_prob_score_false <- metric_tweak(
    "ranked_prob_score",
    ranked_prob_score,
    na_rm = FALSE
  )

  expect_identical(
    metric_set(ranked_prob_score_true, ranked_prob_score_false)(
      two_class_example_ordered,
      truth,
      Class1:Class2
    ),
    ref
  )

  # numeric
  ref <- dplyr::bind_rows(
    ccc(mtcars, truth = mpg, estimate = disp, bias = TRUE),
    ccc(mtcars, truth = mpg, estimate = disp, bias = FALSE)
  )

  ccc_bias <- metric_tweak("ccc", ccc, bias = TRUE)
  ccc_no_bias <- metric_tweak("ccc", ccc, bias = FALSE)

  expect_identical(
    metric_set(ccc_bias, ccc_no_bias)(mtcars, truth = mpg, estimate = disp),
    ref
  )

  # Static survival
  lung_surv_na <- lung_surv
  lung_surv_na$.pred_time[1] <- NA

  ref <- dplyr::bind_rows(
    concordance_survival(lung_surv_na, surv_obj, .pred_time, na_rm = TRUE),
    concordance_survival(lung_surv_na, surv_obj, .pred_time, na_rm = FALSE)
  )

  concordance_survival_na_rm <- metric_tweak(
    "concordance_survival",
    concordance_survival,
    na_rm = TRUE
  )
  concordance_survival_no_na_rm <- metric_tweak(
    "concordance_survival",
    concordance_survival,
    na_rm = FALSE
  )

  expect_identical(
    metric_set(concordance_survival_na_rm, concordance_survival_no_na_rm)(
      lung_surv_na,
      truth = surv_obj,
      estimate = .pred_time
    ),
    ref
  )

  # dynamic survival

  ref <- dplyr::bind_rows(
    brier_survival(lung_surv_na, surv_obj, .pred, na_rm = TRUE),
    brier_survival(lung_surv_na, surv_obj, .pred, na_rm = FALSE)
  )

  brier_survival_na_rm <- metric_tweak(
    "brier_survival",
    brier_survival,
    na_rm = TRUE
  )
  brier_survival_no_na_rm <- metric_tweak(
    "brier_survival",
    brier_survival,
    na_rm = FALSE
  )

  expect_identical(
    metric_set(brier_survival_na_rm, brier_survival_no_na_rm)(
      lung_surv_na,
      truth = surv_obj,
      .pred
    ),
    ref
  )

  # integrated survival

  ref <- dplyr::bind_rows(
    brier_survival_integrated(lung_surv_na, surv_obj, .pred, na_rm = TRUE),
    brier_survival_integrated(lung_surv_na, surv_obj, .pred, na_rm = FALSE)
  )

  brier_survival_integrated_na_rm <- metric_tweak(
    "brier_survival_integrated",
    brier_survival_integrated,
    na_rm = TRUE
  )
  brier_survival_integrated_no_na_rm <- metric_tweak(
    "brier_survival_integrated",
    brier_survival_integrated,
    na_rm = FALSE
  )

  expect_identical(
    metric_set(
      brier_survival_integrated_na_rm,
      brier_survival_integrated_no_na_rm
    )(
      lung_surv_na,
      truth = surv_obj,
      .pred
    ),
    ref
  )

  # linear predictor survival survival
  ref <- dplyr::bind_rows(
    royston_survival(lung_surv_na, surv_obj, .pred_linear_pred, na_rm = TRUE),
    royston_survival(lung_surv_na, surv_obj, .pred_linear_pred, na_rm = FALSE)
  )

  royston_survival_na_rm <- metric_tweak(
    "royston_survival",
    royston_survival,
    na_rm = TRUE
  )
  royston_survival_no_na_rm <- metric_tweak(
    "royston_survival",
    royston_survival,
    na_rm = FALSE
  )

  expect_identical(
    metric_set(
      royston_survival_na_rm,
      royston_survival_no_na_rm
    )(
      lung_surv_na,
      truth = surv_obj,
      estimate = .pred_linear_pred
    ),
    ref
  )
})

test_that("metric_set() errors on bad input", {
  expect_snapshot(
    error = TRUE,
    metric_set("x")
  )
  expect_snapshot(
    error = TRUE,
    metric_set(rmse, "x")
  )
})

test_that("metric_set() errors on empty input", {
  expect_snapshot(
    error = TRUE,
    metric_set()
  )
})

test_that("metric_set() errors on mixing incombatible metrics", {
  expect_snapshot(
    error = TRUE,
    metric_set(rmse, accuracy)
  )
  expect_snapshot(
    error = TRUE,
    metric_set(rmse, accuracy, brier_survival)
  )
  expect_snapshot(
    error = TRUE,
    metric_set(rmse, accuracy, brier_survival, weighted_interval_score)
  )
})

test_that("can supply `event_level` even with metrics that don't use it", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- stats::relevel(df_rev$truth, "Class2")
  df_rev$predicted <- stats::relevel(df_rev$predicted, "Class2")

  # accuracy doesn't use it, and doesn't have it as an argument
  set <- metric_set(accuracy, recall, roc_auc)

  expect_equal(
    as.data.frame(set(df, truth, Class1, estimate = predicted)),
    as.data.frame(
      set(df_rev, truth, Class1, estimate = predicted, event_level = "second")
    )
  )
})

test_that("`metric_set()` labeling remove namespaces", {
  x <- metric_set(yardstick::mase, rmse)
  expect_identical(names(attr(x, "metrics")), c("mase", "rmse"))
})

test_that("metric_set can be coerced to a tibble", {
  x <- metric_set(roc_auc, pr_auc, accuracy)
  expect_s3_class(dplyr::as_tibble(x), "tbl_df")
})

test_that("`metric_set()` errors contain env name for unknown functions (#128)", {
  foobar <- function() {}

  # Store env name in `name` attribute for `environmentName()` to find it
  env <- rlang::new_environment(parent = globalenv())
  attr(env, "name") <- "test"

  rlang::fn_env(foobar) <- env

  expect_snapshot(
    error = TRUE,
    metric_set(accuracy, foobar, sens, rlang::abort)
  )
  expect_snapshot(
    error = TRUE,
    metric_set(accuracy, foobar, sens, rlang::abort)
  )
})

test_that("`metric_set()` gives an informative error for a single non-metric function (#181)", {
  foobar <- function() {}

  # Store env name in `name` attribute for `environmentName()` to find it
  env <- rlang::new_environment(parent = globalenv())
  attr(env, "name") <- "test"
  rlang::fn_env(foobar) <- env

  expect_snapshot(
    error = TRUE,
    metric_set(foobar)
  )
})

test_that("errors informatively for unevaluated metric factories", {
  # one bad metric
  expect_snapshot(
    error = TRUE,
    metric_set(demographic_parity)
  )

  expect_snapshot(
    error = TRUE,
    metric_set(demographic_parity, roc_auc)
  )

  # two bad metrics
  expect_snapshot(
    error = TRUE,
    metric_set(demographic_parity, equal_opportunity)
  )

  expect_snapshot(
    error = TRUE,
    metric_set(demographic_parity, equal_opportunity, roc_auc)
  )
})

test_that("propagates 'caused by' error message when specifying the wrong column name", {
  set <- metric_set(accuracy, kap)

  # There is no `weight` column!
  expect_snapshot(error = TRUE, {
    set(
      two_class_example,
      truth,
      Class1,
      estimate = predicted,
      case_weights = weight
    )
  })
})

test_that("metric set functions retain class/prob metric functions", {
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

test_that("metric set functions retain numeric metric functions", {
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

test_that("all class metrics - `metric_set()` works with `case_weights`", {
  # Mock a metric that doesn't support weights
  accuracy_no_weights <- function(data, truth, estimate, na_rm = TRUE, ...) {
    # Eat the `...` silently
    accuracy(
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      na_rm = na_rm
    )
  }
  accuracy_no_weights <- new_class_metric(accuracy_no_weights, "maximize")

  set <- metric_set(accuracy, accuracy_no_weights)

  df <- data.frame(
    truth = factor(c("x", "x", "y"), levels = c("x", "y")),
    estimate = factor(c("x", "y", "x"), levels = c("x", "y")),
    case_weights = c(1L, 1L, 2L)
  )

  expect_identical(
    set(df, truth, estimate = estimate, case_weights = case_weights)[[
      ".estimate"
    ]],
    c(1 / 4, 1 / 3)
  )
})

test_that("all numeric metrics - `metric_set()` works with `case_weights`", {
  # Mock a metric that doesn't support weights
  rmse_no_weights <- function(data, truth, estimate, na_rm = TRUE, ...) {
    # Eat the `...` silently
    rmse(
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      na_rm = na_rm
    )
  }
  rmse_no_weights <- new_numeric_metric(rmse_no_weights, "minimize")

  set <- metric_set(rmse, rmse_no_weights)

  solubility_test$weight <- read_weights_solubility_test()

  expect <- c(
    rmse(solubility_test, solubility, prediction, case_weights = weight)[[
      ".estimate"
    ]],
    rmse(solubility_test, solubility, prediction)[[".estimate"]]
  )

  expect_identical(
    set(solubility_test, solubility, prediction, case_weights = weight)[[
      ".estimate"
    ]],
    expect
  )
})

test_that("class and prob metrics - `metric_set()` works with `case_weights`", {
  # Mock a metric that doesn't support weights
  accuracy_no_weights <- function(data, truth, estimate, na_rm = TRUE, ...) {
    # Eat the `...` silently
    accuracy(
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      na_rm = na_rm
    )
  }
  accuracy_no_weights <- new_class_metric(accuracy_no_weights, "maximize")

  set <- metric_set(accuracy, accuracy_no_weights, roc_auc)

  two_class_example$weight <- read_weights_two_class_example()

  expect <- c(
    accuracy(two_class_example, truth, predicted, case_weights = weight)[[
      ".estimate"
    ]],
    accuracy(two_class_example, truth, predicted)[[".estimate"]],
    roc_auc(two_class_example, truth, Class1, case_weights = weight)[[
      ".estimate"
    ]]
  )

  expect_identical(
    set(
      two_class_example,
      truth,
      Class1,
      estimate = predicted,
      case_weights = weight
    )[[".estimate"]],
    expect
  )
})
