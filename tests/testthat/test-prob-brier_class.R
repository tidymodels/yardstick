test_that("Calculations are correct - two class", {
  # BrierScore(two_class_example |>
  #   dplyr::select(Class1, Class2) |>
  #   as.matrix, two_class_example$truth)

  expect_equal(
    brier_class_vec(two_class_example$truth, two_class_example$Class1),
    0.10561859,
    tolerance = 0.01
  )
})

test_that("Calculations are correct - multi class", {
  # With the mclust pakcage, BrierScore(hpc_cv |> select(VF:L) |> as.matrix, hpc_cv$obs)
  hpc_exp <- 0.21083946

  expect_equal(
    brier_class(hpc_cv, obs, VF:L)[[".estimate"]],
    hpc_exp,
    tolerance = 0.01
  )
})

test_that("Calculations handles NAs", {
  hpc_cv$VF[1:10] <- NA

  expect_equal(
    brier_class(hpc_cv, obs, VF:L)[[".estimate"]],
    0.21143119
  )

  expect_equal(
    brier_class(hpc_cv, obs, VF:L, na_rm = FALSE)[[".estimate"]],
    NA_real_
  )
})

test_that("Case weights calculations are correct", {
  wts <- rep(1, nrow(hpc_cv))
  wts[1] <- 5
  hpc_wts <- hpc_cv[c(rep(1, 4), seq_len(nrow(hpc_cv))), ]

  expect_equal(
    brier_class(hpc_cv, obs, VF:L),
    brier_class(hpc_wts, obs, VF:L),
    tolerance = 0.01
  )
})

test_that("works with hardhat case weights", {
  df <- two_class_example

  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    brier_class_vec(df$truth, df$Class1, case_weights = imp_wgt)
  )

  expect_no_error(
    brier_class_vec(df$truth, df$Class1, case_weights = freq_wgt)
  )
})

test_that("errors with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  estimate <- two_class_example$Class1

  expect_snapshot(
    error = TRUE,
    brier_class_vec(cp_truth, estimate)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    brier_class_vec(1, 1, na_rm = "yes")
  )
})
