test_that("Calculations are correct", {
  ex_dat <- generate_numeric_test_data()

  ccc_unbiased <- function(truth, estimate) {
    # Unbiased estimator
    m_e <- mean(estimate)
    m_t <- mean(truth)

    # Uses unbiased `n-1` denom
    v_e <- stats::var(estimate)
    v_t <- stats::var(truth)

    # Uses unbiased `n-1` denom
    cov <- cov(truth, estimate)

    2 * cov / (v_e + v_t + (m_e - m_t)^2)
  }

  expect_equal(
    ccc_vec(ex_dat$obs, ex_dat$pred),
    ccc_unbiased(ex_dat$obs, ex_dat$pred),
    tolerance = 0.001
  )
})

test_that("Case weights calculations are correct", {
  solubility_test$weights <- read_weights_solubility_test()

  # Not correctness tests, just checking against unweighted / unbiased results.
  # We don't have a reference to check against.
  unweighted_unbiased <- ccc(
    solubility_test,
    solubility,
    prediction,
    bias = FALSE
  )[[".estimate"]]
  weighted_unbiased <- ccc(
    solubility_test,
    solubility,
    prediction,
    case_weights = weights,
    bias = FALSE
  )[[".estimate"]]
  weighted_biased <- ccc(
    solubility_test,
    solubility,
    prediction,
    case_weights = weights,
    bias = TRUE
  )[[".estimate"]]

  expect_true(weighted_unbiased != unweighted_unbiased)
  expect_true(weighted_unbiased != weighted_biased)
})

test_that("works with hardhat case weights", {
  solubility_test$weights <- floor(read_weights_solubility_test())
  df <- solubility_test

  imp_wgt <- hardhat::importance_weights(df$weights)
  freq_wgt <- hardhat::frequency_weights(df$weights)

  expect_no_error(
    ccc_vec(df$solubility, df$prediction, case_weights = imp_wgt)
  )

  expect_no_error(
    ccc_vec(df$solubility, df$prediction, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    ccc_vec(1, 1, na_rm = "yes")
  )
})

test_that("bad argument check", {
  expect_snapshot(
    error = TRUE,
    ccc_vec(1, 1, bias = "yes")
  )
})

test_that("ccc() - bias argument works", {
  ex_dat <- generate_numeric_test_data()
  not_na <- !is.na(ex_dat$pred_na)

  expect_equal(
    ccc(ex_dat, truth = "obs", estimate = "pred", bias = TRUE)[[".estimate"]],
    # epiR::epi.ccc(x = ex_dat$obs, y = ex_dat$pred)
    0.8322669,
    tolerance = 0.001
  )
  expect_equal(
    ccc(ex_dat, truth = obs, estimate = "pred_na", bias = TRUE)[[".estimate"]],
    # epiR::epi.ccc(x = ex_dat$obs[not_na], y = ex_dat$pred_na[not_na])
    0.8161879,
    tolerance = 0.001
  )
})
