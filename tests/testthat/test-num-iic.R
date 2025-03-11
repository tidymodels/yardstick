# All tests (excepted weighted ones) confirmed against the software:
# http://www.insilico.eu/coral/SOFTWARECORAL.html

test_that("iic() returns known correct results", {
  ex_dat <- generate_numeric_test_data()

  expect_equal(iic(ex_dat, obs, pred)[[".estimate"]], 0.43306222006167)
})

test_that("iic() can be negative", {
  expect_equal(iic_vec(c(1, 2, 3), c(2, 1, 1)), -0.577350269189626)
})

test_that("iic() is NaN if truth/estimate are equivalent", {
  expect_equal(iic_vec(c(1, 2), c(1, 2)), NaN)
})

test_that("case weights are applied", {
  df <- dplyr::tibble(
    truth = c(1, 2, 3, 4, 5),
    estimate = c(1, 3, 1, 3, 2),
    weight = c(1, 2, 1, 2, 0)
  )

  expect_equal(
    iic(df, truth, estimate, case_weights = weight)[[".estimate"]],
    0.4264014327112208846415
  )
})

test_that("yardstick correlation warnings are thrown", {
  cnd <- rlang::catch_cnd(iic_vec(c(1, 2), c(1, 1)))
  expect_s3_class(
    cnd,
    "yardstick_warning_correlation_undefined_constant_estimate"
  )

  cnd <- rlang::catch_cnd(iic_vec(c(1, 1), c(1, 2)))
  expect_s3_class(cnd, "yardstick_warning_correlation_undefined_constant_truth")
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    iic_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    iic_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})
