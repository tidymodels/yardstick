test_that("Calculations are correct", {
  count_results <- data_counts()$basic

  expect_equal(
    poisson_log_loss_vec(count_results$count, count_results$pred),
    mean(-stats::dpois(count_results$count, count_results$pred, log = TRUE))
  )
})

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    poisson_log_loss_vec(ex_dat$obs, ex_dat$pred),
    poisson_log_loss(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  count_results <- data_counts()$basic
  na_ind <- 1:2
  count_results$pred[na_ind] <- NA

  expect_identical(
    poisson_log_loss_vec(
      truth = count_results$count,
      estimate = count_results$pred,
      na_rm = FALSE
    ),
    NA_real_
  )

  expect_equal(
    poisson_log_loss_vec(count_results$count, count_results$pred),
    mean(
      -stats::dpois(count_results$count, count_results$pred, log = TRUE),
      na.rm = TRUE
    )
  )
})

test_that("Case weights calculations are correct", {
  count_results <- data_counts()$basic
  count_results$weights <- c(1, 2, 1, 1, 2, 1)

  exp <- yardstick_mean(
    log(gamma(count_results$count + 1)) +
      count_results$pred -
      log(count_results$pred) * count_results$count,
    case_weights = count_results$weights
  )

  expect_identical(
    poisson_log_loss_vec(
      truth = count_results$count,
      estimate = count_results$pred,
      case_weights = count_results$weights
    ),
    exp
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

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    poisson_log_loss_vec(1, 1, na_rm = "yes")
  )
})

test_that("poisson_log_loss() - handles 0 valued estimates (#513)", {
  count_results <- data_counts()$basic

  count_results$pred <- 0

  expect_false(
    is.nan(poisson_log_loss(count_results, count, pred)[[".estimate"]]),
  )
  expect_false(
    is.infinite(poisson_log_loss(count_results, count, pred)[[".estimate"]]),
  )
})

test_that("poisson_log_loss() - handles poor data", {
  count_results <- data_counts()$basic
  count_poor <- data_counts()$poor

  expect_true(
    poisson_log_loss(count_results, count, pred)[[".estimate"]] <
      poisson_log_loss(count_poor, count, pred)[[".estimate"]]
  )
})

test_that("range values are correct", {
  direction <- metric_direction(poisson_log_loss)
  range <- metric_range(poisson_log_loss)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = c(1L, 2L, 3L, 4L, 5L),
    estimate = c(1, 2, 3, 4, 5),
    off = c(2, 3, 4, 5, 6)
  )

  # polsson_log_loss includes log(gamma(truth+1)) so perfect != 0
  expect_gte(poisson_log_loss_vec(df$truth, df$estimate), perfect)

  if (direction == "minimize") {
    expect_gt(poisson_log_loss_vec(df$truth, df$off), perfect)
    expect_lte(poisson_log_loss_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lt(poisson_log_loss_vec(df$truth, df$off), perfect)
    expect_gte(poisson_log_loss_vec(df$truth, df$off), worst)
  }
})
