test_that("roc_curve_auc() calculations", {
  survival_curve <- roc_curve_survival(
    lung_surv,
    truth = surv_obj,
    estimate = .pred
  ) %>%
    dplyr::summarise(
      .estimate = yardstick:::auc(1 - specificity, sensitivity)
    )

  survival_auc <- roc_auc_survival(
    lung_surv,
    truth = surv_obj,
    estimate = .pred
  )

  expect_equal(
    survival_curve$.estimate,
    survival_auc$.estimate
  )
})
