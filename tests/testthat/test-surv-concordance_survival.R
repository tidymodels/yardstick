test_that("comparison test with survival", {
  res <- concordance_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_time
  )

  expect_equal(
    res[[".estimate"]],
    survival::concordance(surv_obj ~ .pred_time, data = lung_surv)$concordance
  )
})

test_that("case weights works", {
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
