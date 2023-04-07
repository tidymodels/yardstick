test_that("comparison test with survival", {
  res <- concordance_survival(
    data = lung_surv, truth = surv_obj, estimate = .pred_time
  )

  expect_equal(
    res[[".estimate"]],
    survival::concordance(surv_obj ~ .pred_time, data = lung_surv)$concordance
  )
})

test_that("case weights works", {
  lung_surv$wts <- seq_len(nrow(lung_surv))

  res <- concordance_survival(
    data = lung_surv, truth = surv_obj, estimate = .pred_time, case_weights = wts
  )

  expect_equal(
    res[[".estimate"]],
    survival::concordance(surv_obj ~ .pred_time, weights = wts, data = lung_surv)$concordance
  )
})
