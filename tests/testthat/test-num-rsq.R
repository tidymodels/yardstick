test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  expect_equal(
    rsq_vec(truth = ex_dat$obs, ex_dat$pred),
    stats::cor(ex_dat[, 1:2])[1, 2]^2
  )
})

test_that("both interfaces gives the same results", {
  ex_dat <- generate_numeric_test_data()

  expect_identical(
    rsq_vec(ex_dat$obs, ex_dat$pred),
    rsq(ex_dat, obs, pred)[[".estimate"]],
  )
})

test_that("Calculations handles NAs", {
  ex_dat <- generate_numeric_test_data()
  na_ind <- 1:10
  ex_dat$pred[na_ind] <- NA

  expect_identical(
    rsq_vec(ex_dat$obs, ex_dat$pred, na_rm = FALSE),
    NA_real_
  )

  expect_equal(
    rsq_vec(truth = ex_dat$obs, ex_dat$pred),
    stats::cor(ex_dat[-na_ind, 1:2])[1, 2]^2
  )
})

test_that("Case weights calculations are correct", {
  df <- dplyr::tibble(
    truth = c(1, 2, 3, 4, 5),
    estimate = c(1, 3, 1, 3, 2),
    weight = c(1, 0, 1, 0, 1)
  )

  expect_identical(
    rsq(df, truth, estimate, case_weights = weight),
    rsq(df[as.logical(df$weight), ], truth, estimate)
  )
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    rsq_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    rsq_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    rsq_vec(1, 1, na_rm = "yes")
  )
})

test_that("yardstick correlation warnings are thrown", {
  expect_snapshot({
    (expect_warning(
      object = out <- rsq_vec(1, 1),
      class = "yardstick_warning_correlation_undefined_size_zero_or_one"
    ))
  })
  expect_identical(out, NA_real_)

  expect_snapshot({
    (expect_warning(
      object = out <- rsq_vec(double(), double()),
      class = "yardstick_warning_correlation_undefined_size_zero_or_one"
    ))
  })
  expect_identical(out, NA_real_)

  expect_snapshot({
    (expect_warning(
      object = out <- rsq_vec(c(1, 2), c(1, 1)),
      class = "yardstick_warning_correlation_undefined_constant_estimate"
    ))
  })
  expect_identical(out, NA_real_)

  expect_snapshot({
    (expect_warning(
      object = out <- rsq_vec(c(1, 1), c(1, 2)),
      class = "yardstick_warning_correlation_undefined_constant_truth"
    ))
  })
  expect_identical(out, NA_real_)
})

test_that("range values are correct", {
  direction <- metric_direction(rsq)
  range <- metric_range(rsq)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  df <- tibble::tibble(
    truth = c(5, 6, 2, 6, 4, 1, 3)
  )

  df$estimate <- df$truth
  df$off <- df$truth + 1

  expect_equal(
    rsq_vec(df$truth, df$estimate),
    perfect
  )
  if (direction == "minimize") {
    expect_gt(rsq_vec(df$truth, df$off), perfect)
    expect_lt(rsq_vec(df$truth, df$off), worst)
  }
  if (direction == "maximize") {
    expect_lte(rsq_vec(df$truth, df$off), perfect)
    expect_gt(rsq_vec(df$truth, df$off), worst)
  }
})
