test_that("roc_curve_survival works", {
  result <- roc_curve_survival(
    lung_surv,
    truth = surv_obj,
    .pred
  )

  expect_identical(
    names(result),
    c(".threshold", "sensitivity", "specificity")
  )

  expect_identical(
    result$.threshold,
    c(0, unique(sort(tidyr::unnest(lung_surv, cols = .pred)$.pred_survival)), 1)
  )

  expect_true(
    all(diff(result$sensitivity) >= 0)
  )

  expect_true(
    all(diff(result$specificity) <= 0)
  )
})
