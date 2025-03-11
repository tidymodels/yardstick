test_that("poisson log-loss", {
  count_results <- data_counts()$basic
  count_missing <- data_counts()$missing
  count_poor <- data_counts()$poor

  expect_equal(
    poisson_log_loss(count_results, count, pred)[[".estimate"]],
    mean(-stats::dpois(count_results$count, count_results$pred, log = TRUE))
  )

  expect_equal(
    poisson_log_loss(count_missing, count, pred)[[".estimate"]],
    mean(
      -stats::dpois(count_results$count[-1], count_results$pred[-1], log = TRUE)
    )
  )

  expect_true(
    poisson_log_loss(count_results, count, pred)[[".estimate"]] <
      poisson_log_loss(count_poor, count, pred)[[".estimate"]]
  )
})

test_that("poisson log-loss handles 0 valued estimates (#513)", {
  count_results <- data_counts()$basic

  count_results$pred <- 0

  expect_false(
    is.nan(poisson_log_loss(count_results, count, pred)[[".estimate"]]),
  )
  expect_false(
    is.infinite(poisson_log_loss(count_results, count, pred)[[".estimate"]]),
  )
})

test_that("weighted results are working", {
  count_results <- data_counts()$basic
  count_results$weights <- c(1, 2, 1, 1, 2, 1)

  expect_identical(
    poisson_log_loss(count_results, count, pred, case_weights = weights)[[
      ".estimate"
    ]],
    yardstick_mean(
      log(gamma(count_results$count + 1)) +
        count_results$pred -
        log(count_results$pred) * count_results$count,
      case_weights = count_results$weights
    )
  )
})

test_that("works with hardhat case weights", {
  count_results <- data_counts()$basic
  count_results$weights <- c(1, 2, 1, 1, 2, 1)

  df <- count_results

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    poisson_log_loss_vec(df$count, df$pred, case_weights = imp_wgt)
  )

  expect_no_error(
    poisson_log_loss_vec(df$count, df$pred, case_weights = freq_wgt)
  )
})
