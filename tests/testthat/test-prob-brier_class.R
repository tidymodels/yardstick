test_that("basic results", {
  # With the mclust pakcage, BrierScore(hpc_cv |> select(VF:L) |> as.matrix, hpc_cv$obs)
  hpc_exp <- 0.21083946

  expect_equal(
    yardstick:::brier_factor(hpc_cv$obs, hpc_cv |> dplyr::select(VF:L)),
    hpc_exp,
    tolerance = 0.01
  )

  hpc_inds <- model.matrix(~ . - 1, data = hpc_cv |> dplyr::select(obs))
  expect_equal(
    yardstick:::brier_ind(hpc_inds, hpc_cv |> dplyr::select(VF:L)),
    hpc_exp,
    tolerance = 0.01
  )

  expect_equal(
    yardstick:::brier_class(hpc_cv, obs, VF:L),
    dplyr::tibble(
      .metric = "brier_class",
      .estimator = "multiclass",
      .estimate = hpc_exp
    ),
    tolerance = 0.01
  )

  # ----------------------------------------------------------------------------
  # two classes

  # BrierScore(two_class_example |> dplyr::select(Class1, Class2) |> as.matrix, two_class_example$truth)
  two_cls_exp <- 0.10561859

  expect_equal(
    yardstick:::brier_factor(two_class_example$truth, two_class_example[, 2:3]),
    two_cls_exp,
    tolerance = 0.01
  )
  expect_equal(
    yardstick:::brier_factor(
      two_class_example$truth,
      two_class_example[, 2, drop = TRUE]
    ),
    two_cls_exp,
    tolerance = 0.01
  )

  # ----------------------------------------------------------------------------
  # with missing data
  hpc_miss <- hpc_cv
  hpc_miss$obs[1] <- NA
  hpc_miss$L[2] <- NA

  # With the mclust pakcage, BrierScore(hpc_cv[-(1:2), 3:6]|> as.matrix, hpc_cv$obs[-(1:2)])
  hpc_miss_exp <- 0.21095817
  expect_equal(
    brier_class(hpc_miss, obs, VF:L)$.estimate,
    hpc_miss_exp,
    tolerance = 0.01
  )

  # ----------------------------------------------------------------------------
  # with case weights
  wts <- rep(1, nrow(hpc_cv))
  wts[1] <- 5
  hpc_wts <- hpc_cv[c(rep(1, 4), 1:nrow(hpc_cv)), ]

  expect_equal(
    yardstick:::brier_factor(hpc_cv$obs, hpc_cv |> dplyr::select(VF:L)),
    yardstick:::brier_factor(hpc_wts$obs, hpc_wts |> dplyr::select(VF:L)),
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
