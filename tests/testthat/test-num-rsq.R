test_that("R^2", {
  ex_dat <- generate_numeric_test_data()

  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    stats::cor(ex_dat[, 1:2])[1, 2]^2
  )
  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    stats::cor(ex_dat[, c(1, 3)], use = "complete.obs")[1, 2]^2
  )
  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "rand")[[".estimate"]],
    stats::cor(ex_dat[, c(1, 4)])[1, 2]^2
  )
  expect_equal(
    rsq(ex_dat, estimate = rand_na, truth = obs)[[".estimate"]],
    stats::cor(ex_dat[, c(1, 5)], use = "complete.obs")[1, 2]^2
  )
})

test_that("case weights are applied", {
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
