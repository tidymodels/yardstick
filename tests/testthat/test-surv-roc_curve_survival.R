test_that("roc_curve_survival works", {
  skip_if_not_installed("tidyr")

  result <- roc_curve_survival(
    lung_surv,
    truth = surv_obj,
    .pred
  )

  expect_identical(
    names(result),
    c(".threshold", "specificity", "sensitivity", ".eval_time")
  )

  .eval_times <- unique(result$.eval_time)

  eval_time <- 200
  for (eval_time in .eval_times) {
    result_tmp <- dplyr::filter(result, .eval_time == eval_time)

    exp_threshold <- tidyr::unnest(lung_surv, cols = .pred)
    exp_threshold <- dplyr::filter(exp_threshold, .eval_time == eval_time)
    exp_threshold <- exp_threshold$.pred_survival
    exp_threshold <- sort(exp_threshold, decreasing = TRUE)
    exp_threshold <- unique(exp_threshold)
    exp_threshold <- c(Inf, exp_threshold, -Inf)
    expect_identical(
      result_tmp$.threshold,
      exp_threshold
    )

    expect_true(
      all(diff(result_tmp$sensitivity) >= 0)
    )

    expect_true(
      all(diff(result_tmp$specificity) <= 0)
    )
  }
})

# case weights -----------------------------------------------------------------

test_that("case weights are applied", {
  skip_if_not_installed("tidyr")

  wts_res <- lung_surv |>
    dplyr::mutate(wts = hardhat::frequency_weights(rep(1:0, c(128, 100)))) |>
    roc_curve_survival(
      truth = surv_obj,
      .pred,
      case_weights = wts
    )

  subset_res <- lung_surv |>
    dplyr::slice(1:128) |>
    roc_curve_survival(
      truth = surv_obj,
      .pred
    )

  expect_identical(subset_res, wts_res)
})

# self checking ----------------------------------------------------------------

test_that("snapshot equivalent", {
  skip_if_not_installed("tidyr")

  snapshot_res <- readRDS(test_path("data/ref_roc_curve_survival.rds"))

  yardstick_res <- readRDS(test_path("data/tidy_churn.rds")) |>
    roc_curve_survival(
      truth = surv_obj,
      .pred
    )

  expect_equal(
    snapshot_res$.threshold,
    yardstick_res$.threshold
  )

  expect_equal(
    snapshot_res$sensitivity,
    yardstick_res$sensitivity
  )

  expect_equal(
    snapshot_res$specificity,
    yardstick_res$specificity
  )

  expect_identical(
    snapshot_res$.eval_time,
    yardstick_res$.eval_time
  )
})

# Manual checking --------------------------------------------------------------

test_that("hand calculated equivalent", {
  skip_if_not_installed("tidyr")

  my_eval_time <- 300

  lung_surv0 <- lung_surv |>
    dplyr::slice(-14) |>
    tidyr::unnest(.pred) |>
    dplyr::filter(.eval_time == my_eval_time)

  thresholds <- sort(lung_surv0$.pred_survival)

  # Sensitivity
  calc_sensitivity <- function(threshold, data, eval_time) {
    delta <- .extract_surv_status(data$surv_obj)
    event_time <- .extract_surv_time(data$surv_obj)
    res <- dplyr::tibble(
      .threshold = sort(
        unique(c(-Inf, data$.pred_survival, Inf)),
        decreasing = TRUE
      )
    )

    obs_time_le_time <- event_time <= data$.eval_time
    obs_time_gt_time <- event_time > data$.eval_time
    n <- nrow(data)

    sensitivity_denom <- sum(
      obs_time_le_time * delta * data$.weight_censored,
      na.rm = TRUE
    )

    data_df <- data.frame(
      le_time = obs_time_le_time,
      delta = delta,
      weight_censored = data$.weight_censored
    )

    data_split <- vec_split(data_df, data$.pred_survival)
    data_split <- data_split$val[order(data_split$key)]

    sensitivity <- vapply(
      data_split,
      function(x) sum(x$le_time * x$delta * x$weight_censored, na.rm = TRUE),
      FUN.VALUE = numeric(1)
    )

    sensitivity <- cumsum(sensitivity)
    sensitivity <- sensitivity / sensitivity_denom
    sensitivity <- dplyr::if_else(sensitivity > 1, 1, sensitivity)
    sensitivity <- dplyr::if_else(sensitivity < 0, 0, sensitivity)
    sensitivity <- c(0, sensitivity, 1)
    sensitivity
  }

  exp_sens <- calc_sensitivity(thresholds, lung_surv0, my_eval_time)

  yardstick_res <- lung_surv |>
    dplyr::slice(-14) |>
    roc_curve_survival(
      truth = surv_obj,
      .pred
    ) |>
    dplyr::filter(.eval_time == my_eval_time)

  expect_equal(
    yardstick_res$sensitivity,
    exp_sens
  )

  # specificity
  calc_specificity <- function(threshold, data, eval_time) {
    event_time <- yardstick:::.extract_surv_time(data$surv_obj)

    res <- dplyr::tibble(
      .threshold = sort(
        unique(c(-Inf, data$.pred_survival, Inf)),
        decreasing = TRUE
      )
    )

    obs_time_gt_time <- event_time > data$.eval_time
    n <- nrow(data)

    specificity_denom <- sum(
      obs_time_gt_time * data$.weight_censored,
      na.rm = TRUE
    )

    data_df <- data.frame(
      ge_time = obs_time_gt_time,
      weight_censored = data$.weight_censored
    )

    data_split <- vec_split(data_df, data$.pred_survival)
    data_split <- data_split$val[order(data_split$key)]

    specificity <- vapply(
      data_split,
      function(x) sum(x$ge_time * x$weight_censored, na.rm = TRUE),
      FUN.VALUE = numeric(1)
    )
    specificity <- cumsum(specificity)
    specificity <- specificity / specificity_denom
    specificity <- dplyr::if_else(specificity > 1, 1, specificity)
    specificity <- dplyr::if_else(specificity < 0, 0, specificity)
    specificity <- c(0, specificity, 1)
    specificity <- 1 - specificity
    specificity
  }

  exp_spec <- calc_specificity(thresholds, lung_surv0, my_eval_time)

  yardstick_res <- lung_surv |>
    dplyr::slice(-14) |>
    roc_curve_survival(
      truth = surv_obj,
      .pred
    ) |>
    dplyr::filter(.eval_time == my_eval_time)

  expect_equal(
    yardstick_res$specificity,
    exp_spec
  )
})
