set.seed(1311)
three_class <- data.frame(
  obs = iris$Species,
  pred = sample(iris$Species, replace = TRUE)
)
probs <- matrix(runif(150 * 3), nrow = 150)
probs <- t(apply(probs, 1, function(x) x / sum(x)))
colnames(probs) <- levels(iris$Species)
three_class <- cbind(three_class, as.data.frame(probs))

###################################################################

test_that("correct metrics returned", {
  expect_equal(
    metrics(two_class_example, truth, predicted)[[".metric"]],
    c("accuracy", "kap")
  )
  expect_equal(
    metrics(two_class_example, truth, predicted, Class1)[[".metric"]],
    c("accuracy", "kap", "mn_log_loss", "roc_auc")
  )
  expect_equal(
    metrics(three_class, "obs", "pred", setosa, versicolor, virginica)[[
      ".metric"
    ]],
    c("accuracy", "kap", "mn_log_loss", "roc_auc")
  )
  expect_equal(
    metrics(three_class, "obs", "pred", setosa, versicolor, virginica)[[
      ".estimator"
    ]],
    c("multiclass", "multiclass", "multiclass", "hand_till")
  )
  expect_equal(
    metrics(solubility_test, solubility, "prediction")[[".metric"]],
    c("rmse", "rsq", "mae")
  )
})

###################################################################

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    metrics(two_class_example, truth, Class1)
  )
  expect_snapshot(
    error = TRUE,
    metrics(two_class_example, Class1, truth)
  )
  expect_snapshot(
    error = TRUE,
    metrics(three_class, "obs", "pred", setosa, versicolor)
  )
})

###################################################################

class_res_1 <- dplyr::bind_rows(
  accuracy(two_class_example, truth, predicted),
  kap(two_class_example, truth, predicted),
  mn_log_loss(two_class_example, truth, Class1),
  roc_auc(two_class_example, truth, Class1)
)

reg_res_1 <- dplyr::bind_rows(
  rmse(solubility_test, solubility, "prediction"),
  rsq(solubility_test, solubility, prediction),
  mae(solubility_test, solubility, prediction)
)

test_that("correct results", {
  class_idx <- which(class_res_1$.metric %in% c("accuracy", "kap"))

  expect_equal(
    metrics(two_class_example, truth, predicted)[[".estimate"]],
    class_res_1[class_idx, ][[".estimate"]]
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

test_that("metrics() - `options` is deprecated", {
  skip_if(
    getRversion() <= "3.5.3",
    "Base R used a different deprecated warning class."
  )
  rlang::local_options(lifecycle_verbosity = "warning")

  expect_snapshot({
    out <- metrics(two_class_example, truth, predicted, Class1, options = 1)
  })

  expect_identical(
    out,
    metrics(two_class_example, truth, predicted, Class1)
  )
})

###################################################################

test_that("numeric metric sets", {
  reg_set <- metric_set(rmse, rsq, mae)

  expect_equal(
    reg_set(solubility_test, solubility, prediction),
    reg_res_1
  )
  # ensure helpful messages are printed
  expect_snapshot(
    error = TRUE,
    metric_set(rmse, "x")
  )

  # Can mix class and class prob together
  mixed_set <- metric_set(accuracy, roc_auc)
  expect_no_error(
    mixed_set(two_class_example, truth, Class1, estimate = predicted)
  )
})

test_that("mixing bad metric sets", {
  expect_snapshot(
    error = TRUE,
    metric_set(rmse, accuracy)
  )
})

test_that("can mix class and class prob metrics together", {
  expect_no_error(
    mixed_set <- metric_set(accuracy, roc_auc)
  )
  expect_no_error(
    mixed_set(two_class_example, truth, Class1, estimate = predicted)
  )
})

test_that("can mix class and class prob and ordered metrics together", {
  expect_no_error(
    mixed_set <- metric_set(kap, roc_auc, ranked_prob_score)
  )

  hpc_cv$obs <- as.ordered(hpc_cv$obs)

  expect_no_error(
    mixed_set(hpc_cv, obs, VF:L, estimate = pred)
  )
})

test_that("dynamic survival metric sets", {
  skip_if_not_installed("tidyr")
  my_set <- metric_set(brier_survival)

  expect_equal(
    my_set(lung_surv, surv_obj, .pred),
    brier_survival(lung_surv, surv_obj, .pred)
  )
})

test_that("can mix dynamic and static survival metric together", {
  skip_if_not_installed("tidyr")
  expect_no_error(
    mixed_set <- metric_set(brier_survival, concordance_survival)
  )
  expect_no_error(
    mixed_set(lung_surv, surv_obj, .pred, estimate = .pred_time)
  )
})

test_that("can mix dynamic and static survival metric together", {
  skip_if_not_installed("tidyr")

  expect_no_error(
    mixed_set <- metric_set(
      brier_survival,
      concordance_survival,
      brier_survival_integrated
    )
  )
  expect_no_error(
    mixed_set(lung_surv, surv_obj, .pred, estimate = .pred_time)
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

test_that("metric set functions are classed", {
  expect_s3_class(
    metric_set(accuracy, roc_auc),
    "class_prob_metric_set"
  )
  expect_s3_class(
    metric_set(mae),
    "numeric_metric_set"
  )
  expect_s3_class(
    metric_set(accuracy, roc_auc),
    "metric_set"
  )
  expect_s3_class(
    metric_set(mae),
    "metric_set"
  )
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

test_that("`metric_set()` labeling remove namespaces", {
  x <- metric_set(yardstick::mase, rmse)
  expect_identical(names(attr(x, "metrics")), c("mase", "rmse"))
})

test_that("print metric_set works", {
  expect_snapshot(metric_set(rmse, rsq, ccc))
})

test_that("metric_set can be coerced to a tibble", {
  x <- metric_set(roc_auc, pr_auc, accuracy)
  expect_s3_class(dplyr::as_tibble(x), "tbl_df")
})

test_that("`metric_set()` errors contain env name for unknown functions (#128)", {
  foobar <- function() {
  }

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
  foobar <- function() {
  }

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

  # regression
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
  lung_surv_na <- lung_surv
  lung_surv_na$surv_obj[1] <- NA

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
})

test_that("metric_set() errors on empty input", {
  expect_snapshot(
    error = TRUE,
    metric_set()
  )
})
