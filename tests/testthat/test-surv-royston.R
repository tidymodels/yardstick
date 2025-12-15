test_that("comparison test with survival::royston", {
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
    tolerance = 1e-4
  )
})

