test_that("Calculations are correct", {
  lung_data <- survival::lung |>
    dplyr::select(time, status, age, sex, ph.ecog)
  reference_fit <- survival::coxph(
    survival::Surv(time, status) ~ age + sex + ph.ecog,
    data = lung_data
  )
  royston_ref <- survival::royston(reference_fit)

  lung_surv <- data_lung_surv()

  res <- royston_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_linear_pred
  )

  expect_equal(
    res[[".estimate"]],
    royston_ref["R.D"],
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})

test_that("All interfaces gives the same results", {
  lung_data <- survival::lung |>
    dplyr::select(time, status, age, sex, ph.ecog)
  reference_fit <- survival::coxph(
    survival::Surv(time, status) ~ age + sex + ph.ecog,
    data = lung_data
  )

  lung_surv <- data_lung_surv()

  expect_identical(
    royston_survival(
      data = lung_surv,
      truth = surv_obj,
      estimate = .pred_linear_pred
    )[[".estimate"]],
    royston_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred_linear_pred
    )
  )
})

test_that("Calculations handles NAs", {
  lung_surv <- data_lung_surv()
  lung_surv$.pred_linear_pred[1:10] <- NA

  res <- royston_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_linear_pred
  )

  expect_equal(
    res[[".estimate"]],
    0.1218,
    tolerance = 1e-3
  )
})

test_that("Case weights calculations are correct", {
  lung_surv <- data_lung_surv()
  lung_surv$wts <- rep(1, nrow(lung_surv))

  res <- royston_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_linear_pred
  )

  res_wts <- royston_survival(
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_time,
    case_weights = wts
  )

  expect_equal(
    res[[".estimate"]],
    res_wts[[".estimate"]]
  )
})

test_that("works with hardhat case weights", {
  lung_surv <- data_lung_surv()
  lung_surv$case_wts <- c(rep(0, 10), rep(2, nrow(lung_surv) - 10))

  df <- lung_surv

  df$imp_wgt <- hardhat::importance_weights(lung_surv$case_wts)
  df$freq_wgt <- hardhat::frequency_weights(lung_surv$case_wts)

  expect_no_error(
    royston_survival(
      df,
      truth = surv_obj,
      .pred_time,
      case_weights = imp_wgt
    )
  )

  expect_no_error(
    royston_survival(
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
    royston_survival_vec(1, 1, na_rm = "yes")
  )
})

test_that("`normal_score_blom()` works with case weights", {
  # weights without ties
  x <- 1:10 + 0.5
  case_weights <- rep(2, 10)
  nsb <- normal_score_blom(x, case_weights)
  expect_length(nsb, 10)
  expect_equal(
    nsb[1],
    mean(qnorm((1:2 - 3 / 8) / (sum(case_weights) + 0.25)))
  )

  # weights and ties
  x <- c(x, x[1:5], x[1:3])
  case_weights <- c(case_weights, rep(1, 8))
  nsb <- normal_score_blom(x, case_weights)
  expect_length(nsb, 18)
  expect_equal(
    nsb[1],
    mean(qnorm((1:4 - 3 / 8) / (sum(case_weights) + 0.25)))
  )

  # weights of zero
  x <- 1:10 + 0.5
  case_weights <- c(0, 0, rep(2, 8))
  nsb <- normal_score_blom(x, case_weights)
  expect_length(nsb, 10)
  expect_true(all(is.na(nsb[1:2])))
})
