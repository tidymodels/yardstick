count_results <- data_counts()$basic
count_missing <- data_counts()$missing
count_poor    <- data_counts()$poor

test_that('poisson log-loss', {
  expect_equal(
    poisson_deviance(count_results, count, pred)[[".estimate"]],
    mean(stats::poisson()$dev.resids(count_results$count, count_results$pred, 1))
  )

  expect_equal(
    poisson_deviance(count_missing, count, pred)[[".estimate"]],
    mean(stats::poisson()$dev.resids(count_results$count[-1], count_results$pred[-1], 1))
  )

  expect_true(
    poisson_deviance(count_results, count, pred)[[".estimate"]] <
      poisson_deviance(count_poor, count, pred)[[".estimate"]]
  )
})
