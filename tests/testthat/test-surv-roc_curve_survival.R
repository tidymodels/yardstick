test_that("roc_curve_survival works", {
  result <- roc_curve_survival(
    lung_surv,
    truth = surv_obj,
    .pred
  )

  expect_identical(
    names(result),
    c(".threshold", "sensitivity", "specificity", ".eval_time")
  )

  .eval_times <- unique(result$.eval_time)

  eval_time <- 200
  for (eval_time in .eval_times) {
    result_tmp <- dplyr::filter(result, .eval_time == eval_time)

    exp_threshold <- tidyr::unnest(lung_surv, cols = .pred)
    exp_threshold <- dplyr::filter(exp_threshold, .eval_time == eval_time)
    exp_threshold <- exp_threshold$.pred_survival
    exp_threshold <- sort(exp_threshold)
    exp_threshold <- unique(exp_threshold)
    exp_threshold <- c(-Inf, exp_threshold, Inf)
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
  wts_res <- lung_surv %>%
    dplyr::mutate(wts = hardhat::frequency_weights(rep(1:0, c(128, 100)))) %>%
    roc_curve_survival(
      truth = surv_obj,
      .pred,
      case_weights = wts
    )

  subset_res <- lung_surv %>%
    dplyr::slice(1:128) %>%
    roc_curve_survival(
      truth = surv_obj,
      .pred
    )

  expect_identical(subset_res, wts_res)
})

# self checking ----------------------------------------------------------------

test_that("snapshot equivalent", {
  snapshot_res <- readRDS(test_path("data/ref_roc_curve_survival.rds"))

  yardstick_res <- readRDS(test_path("data/tidy_churn.rds")) %>%
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
  my_eval_time <- 300

  lung_surv0 <- lung_surv %>%
    dplyr::slice(-14) %>%
    tidyr::unnest(.pred) %>%
    dplyr::filter(.eval_time == my_eval_time)

  thresholds <- sort(lung_surv0$.pred_survival)
  thresholds <- thresholds[c(1, 10, 100, 200, nrow(lung_surv0))]

  # Sensitivity
  calc_sensitivity <- function(threshold, data, eval_time) {
    n <- nrow(data)
    event_time <- yardstick:::.extract_surv_time(data$surv_obj)
    delta <- yardstick:::.extract_surv_status(data$surv_obj)
    obs_time_le_time <- event_time <= eval_time
    prob_le_thresh <- data$.pred_survival <= threshold

    multiplier <- delta / (n * data$.weight_censored)
    numer <- sum(obs_time_le_time * prob_le_thresh * multiplier, na.rm = TRUE)
    denom <- sum(obs_time_le_time * multiplier, na.rm = TRUE)
    numer / denom
  }

  exp_sens <- vapply(
    thresholds,
    calc_sensitivity,
    data = lung_surv0,
    eval_time = my_eval_time,
    FUN.VALUE = numeric(1)
  )

  yardstick_res <- lung_surv %>%
    dplyr::slice(-14) %>%
    roc_curve_survival(
      truth = surv_obj,
      .pred
    ) %>%
    dplyr::filter(.threshold %in% thresholds)

  expect_equal(
    yardstick_res$sensitivity,
    exp_sens
  )

  # specificity
  calc_specificity <- function(threshold, data, eval_time) {
    event_time <- yardstick:::.extract_surv_time(data$surv_obj)
    delta <- yardstick:::.extract_surv_status(data$surv_obj)
    obs_time_gt_time <- event_time > eval_time
    prob_le_thresh <- data$.pred_survival > threshold
    numer <- sum(obs_time_gt_time * prob_le_thresh, na.rm = TRUE)
    denom <- sum(obs_time_gt_time, na.rm = TRUE)
    numer / denom
  }

  exp_spec <- vapply(
    thresholds,
    calc_specificity,
    data = lung_surv0,
    eval_time = my_eval_time,
    FUN.VALUE = numeric(1)
  )

  yardstick_res <- lung_surv %>%
    dplyr::slice(-14) %>%
    roc_curve_survival(
      truth = surv_obj,
      .pred
    ) %>%
    dplyr::filter(.threshold %in% thresholds)

  expect_equal(
    yardstick_res$specificity,
    exp_spec
  )
})
