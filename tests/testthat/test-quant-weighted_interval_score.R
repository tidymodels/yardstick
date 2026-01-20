test_that("Calculations are correct", {
  quantile_levels <- c(0.2, 0.4, 0.6, 0.8)
  pred1 <- 1:4
  pred2 <- 8:11
  example <- dplyr::tibble(
    preds = hardhat::quantile_pred(rbind(pred1, pred2), quantile_levels),
    truth = c(3.3, 7.1)
  )

  expect_equal(
    weighted_interval_score_vec(example$truth, example$preds),
    1.275
  )
})

test_that("All interfaces gives the same results", {
  quantile_levels <- c(0.2, 0.4, 0.6, 0.8)
  pred1 <- 1:4
  pred2 <- 8:11
  example <- dplyr::tibble(
    preds = hardhat::quantile_pred(rbind(pred1, pred2), quantile_levels),
    truth = c(3.3, 7.1)
  )

  expect_identical(
    weighted_interval_score(example, truth = truth, estimate = preds)[[
      ".estimate"
    ]],
    weighted_interval_score_vec(example$truth, example$preds)
  )
})

test_that("Calculations handles NAs", {
  pred1 <- 1:4
  preds_na <- hardhat::quantile_pred(rbind(pred1, c(1, 2, NA, 4)), 1:4 / 5)
  truth <- c(2.5, 2.5)

  expect_snapshot(
    error = TRUE,
    weighted_interval_score_vec(
      truth,
      preds_na,
      quantile_levels = 1:9 / 10,
      quantile_estimate_nas = "drop"
    )
  )

  expect_identical(
    weighted_interval_score_vec(
      truth,
      preds_na,
      quantile_levels = c(2, 3) / 5,
      quantile_estimate_nas = "drop"
    ),
    0.4
  )

  expect_identical(
    weighted_interval_score_vec(
      truth,
      preds_na,
      na_rm = TRUE,
      quantile_estimate_nas = "propagate"
    ),
    0.5
  )

  expect_identical(
    weighted_interval_score_vec(
      truth,
      preds_na,
      quantile_estimate_nas = "propagate"
    ),
    NA_real_
  )
})

test_that("Case weights calculations are correct", {
  quantile_levels <- c(0.2, 0.4, 0.6, 0.8)
  pred1 <- 1:4
  pred2 <- 8:11
  pred3 <- 8:11
  example <- dplyr::tibble(
    preds = hardhat::quantile_pred(rbind(pred1, pred2, pred3), quantile_levels),
    truth = c(3.3, 7.1, 3),
    weights = c(1, 1, 0)
  )

  expect_equal(
    weighted_interval_score_vec(
      truth = example$truth,
      estimate = example$preds,
      case_weights = example$weights
    ),
    1.275
  )
})

test_that("works with hardhat case weights", {
  quantile_levels <- c(0.2, 0.4, 0.6, 0.8)
  pred1 <- 1:4
  pred2 <- 8:11
  example <- dplyr::tibble(
    preds = hardhat::quantile_pred(rbind(pred1, pred2), quantile_levels),
    truth = c(3.3, 7.1)
  )

  imp_wgt <- hardhat::importance_weights(c(1, 1))
  freq_wgt <- hardhat::frequency_weights(c(1, 1))

  expect_no_error(
    weighted_interval_score_vec(
      truth = example$truth,
      estimate = example$preds,
      case_weights = imp_wgt
    )
  )

  expect_no_error(
    weighted_interval_score_vec(
      truth = example$truth,
      estimate = example$preds,
      case_weights = freq_wgt
    )
  )
})

test_that("quantile_levels argument works", {
  quantile_levels <- c(0.2, 0.4, 0.6, 0.8)
  pred1 <- 1:4
  pred2 <- 8:11
  example <- dplyr::tibble(
    preds = hardhat::quantile_pred(rbind(pred1, pred2), quantile_levels),
    truth = c(3.3, 7.1)
  )

  levels_set <- weighted_interval_score(
    example,
    truth = truth,
    estimate = preds,
    quantile_levels = c(0.25, 0.5, 0.75)
  )

  levels_default <- weighted_interval_score(
    example,
    truth = truth,
    estimate = preds
  )

  expect_true(levels_set$.estimate != levels_default$.estimate)
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    weighted_interval_score_vec(1, 1, na_rm = "yes")
  )
})

test_that("range values are correct", {
  direction <- metric_direction(weighted_interval_score)
  range <- metric_range(weighted_interval_score)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  quantile_levels <- c(0.2, 0.4, 0.6, 0.8)
  truth <- c(3, 7)
  perfect_pred <- hardhat::quantile_pred(
    rbind(rep(3, 4), rep(7, 4)),
    quantile_levels
  )
  off_pred <- hardhat::quantile_pred(
    rbind(c(1, 2, 4, 5), c(5, 6, 8, 9)),
    quantile_levels
  )

  expect_equal(weighted_interval_score_vec(truth, perfect_pred), perfect)

  if (direction == "minimize") {
    expect_gt(weighted_interval_score_vec(truth, off_pred), perfect)
    expect_lte(weighted_interval_score_vec(truth, off_pred), worst)
  }
  if (direction == "maximize") {
    expect_lt(weighted_interval_score_vec(truth, off_pred), perfect)
    expect_gte(weighted_interval_score_vec(truth, off_pred), worst)
  }
})
