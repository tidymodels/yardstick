test_that("Calculations are correct", {
  hpc_cv$obs <- as.ordered(hpc_cv$obs)

  # With orf:::rps(as.matrix(hpc_cv[, 3:6]), hpc_cv$obs)
  hpc_exp <- 0.08566779

  expect_equal(
    yardstick:::ranked_prob_score_vec(
      hpc_cv$obs,
      as.matrix(hpc_cv |> dplyr::select(VF:L))
    ),
    hpc_exp,
    tolerance = 0.01
  )
})

test_that("All interfaces gives the same results", {
  hpc_cv$obs <- as.ordered(hpc_cv$obs)

  expect_equal(
    yardstick:::ranked_prob_score_vec(
      hpc_cv$obs,
      as.matrix(hpc_cv |> dplyr::select(VF:L))
    ),
    yardstick:::ranked_prob_score(hpc_cv, obs, VF:L)[[".estimate"]]
  )
})

test_that("Calculations handles NAs", {
  hpc_miss <- hpc_cv
  hpc_miss$obs <- as.ordered(hpc_miss$obs)
  hpc_miss$obs[1] <- NA
  hpc_miss$L[2] <- NA

  cmlpt_ind <- complete.cases(hpc_miss)

  # With orf:::rps(as.matrix(hpc_cv[cmlpt_ind, 3:6]), hpc_cv$obs[cmlpt_ind])
  hpc_miss_exp <- 0.08571614
  expect_equal(
    ranked_prob_score(hpc_miss, obs, VF:L)[[".estimate"]],
    hpc_miss_exp,
    tolerance = 0.01
  )

  expect_equal(
    ranked_prob_score(hpc_miss, obs, VF:L, na_rm = FALSE)[[".estimate"]],
    NA_real_
  )
})

test_that("Case weights calculations are correct", {
  hpc_cv$obs <- as.ordered(hpc_cv$obs)
  hpc_cv$weights <- rep(c(0, 1), c(100, nrow(hpc_cv) - 100))
  # With orf:::rps(as.matrix(hpc_cv[-(1:100), 3:6]), hpc_cv$obs[-(1:100)])
  hpc_exp <- 0.0868733

  expect_equal(
    yardstick:::ranked_prob_score_vec(
      hpc_cv$obs,
      as.matrix(hpc_cv |> dplyr::select(VF:L)),
      case_weights = hpc_cv$weights
    ),
    hpc_exp,
    tolerance = 0.000001
  )
})

test_that("works with hardhat case weights", {
  df <- two_class_example
  df$truth <- as.ordered(df$truth)

  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    ranked_prob_score_vec(
      df$truth,
      as.matrix(df[c("Class1", "Class2")]),
      case_weights = imp_wgt
    )
  )

  expect_no_error(
    ranked_prob_score_vec(
      df$truth,
      as.matrix(df[c("Class1", "Class2")]),
      case_weights = freq_wgt
    )
  )
})

test_that("errors with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA
  ord_truth <- as.ordered(two_class_example$truth)

  estimate_1D <- two_class_example$Class1
  estimate <- two_class_example[, 2:3]

  expect_snapshot(
    error = TRUE,
    ranked_prob_score_vec(cp_truth, estimate)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    ranked_prob_score_vec(1, 1, na_rm = "yes")
  )
})
