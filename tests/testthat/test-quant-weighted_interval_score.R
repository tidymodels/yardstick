test_that("weighted_interval_score_vec works", {
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

test_that("Missing value behaviours works", {
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

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    weighted_interval_score_vec(1, 1, na_rm = "yes")
  )
})
