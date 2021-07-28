count_results <- data_counts()$basic
count_missing <- data_counts()$missing
count_poor    <- data_counts()$poor

test_that('poisson log-loss', {
  expect_equal(
    poisson_log_loss(count_results, count, pred)[[".estimate"]],
    mean(-stats::dpois(count_results$count, count_results$pred, log = TRUE))
  )

  expect_equal(
    poisson_log_loss(count_missing, count, pred)[[".estimate"]],
    mean(-stats::dpois(count_results$count[-1], count_results$pred[-1], log = TRUE))
  )

  expect_true(
    poisson_log_loss(count_results, count, pred)[[".estimate"]] <
      poisson_log_loss(count_poor, count, pred)[[".estimate"]]
  )
})
