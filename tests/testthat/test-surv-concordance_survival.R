test_that("Calculations are correct", {
  exp <- survival::concordance(
    surv_obj ~ .pred_time,
    data = lung_surv
  )$concordance

  expect_equal(
    concordance_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred_time
    ),
    exp
  )
})

test_that("All interfaces gives the same results", {
  expect_identical(
    concordance_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred_time
    ),
    concordance_survival(
      lung_surv,
      truth = surv_obj,
      estimate = .pred_time
    )[[".estimate"]]
  )
})

test_that("Calculations handles NAs", {
  lung_surv$.pred_time[1:10] <- NA
  exp <- survival::concordance(
    surv_obj ~ .pred_time,
    data = lung_surv
  )$concordance

  expect_equal(
    concordance_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred_time
    ),
    exp
  )
})

test_that("Case weights calculations are correct", {
  lung_surv$wts <- seq_len(nrow(lung_surv))

  res <- concordance_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_time,
    case_weights = wts
  )

  expect_equal(
    res[[".estimate"]],
    survival::concordance(
      surv_obj ~ .pred_time,
      weights = wts,
      data = lung_surv
    )$concordance
  )
})

test_that("works with infinite time predictions", {
  exp_res <- concordance_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_time
  )

  lung_surv$.pred_time[which.max(lung_surv$.pred_time)] <- Inf

  expect_no_error(
    res <- concordance_survival(
      data = lung_surv,
      truth = surv_obj,
      estimate = .pred_time
    )
  )

  expect_identical(res, exp_res)

  exp_res <- concordance_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_time
  )

  lung_surv$.pred_time[which.min(lung_surv$.pred_time)] <- Inf

  expect_no_error(
    res <- concordance_survival(
      data = lung_surv,
      truth = surv_obj,
      estimate = .pred_time
    )
  )

  expect_true(!identical(res, exp_res))
})

test_that("works with hardhat case weights", {
  lung_surv <- data_lung_surv()
  lung_surv$case_wts <- rep(2, nrow(lung_surv))

  df <- lung_surv

  df$imp_wgt <- hardhat::importance_weights(lung_surv$case_wts)
  df$freq_wgt <- hardhat::frequency_weights(lung_surv$case_wts)

  expect_no_error(
    concordance_survival(
      df,
      truth = surv_obj,
      .pred_time,
      case_weights = imp_wgt
    )
  )

  expect_no_error(
    concordance_survival(
      df,
      truth = surv_obj,
      .pred_time,
      case_weights = freq_wgt
    )
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    concordance_survival_vec(1, 1, na_rm = "yes")
  )
})

test_that("range values are correct", {
  direction <- metric_direction(concordance_survival)
  range <- metric_range(concordance_survival)
  perfect <- ifelse(direction == "minimize", range[1], range[2])
  worst <- ifelse(direction == "minimize", range[2], range[1])

  result <- concordance_survival_vec(
    truth = lung_surv$surv_obj,
    estimate = lung_surv$.pred_time
  )

  if (direction == "minimize") {
    expect_gte(result, perfect)
    expect_lte(result, worst)
  }
  if (direction == "maximize") {
    expect_gte(result, worst)
    expect_lte(result, perfect)
  }
})
