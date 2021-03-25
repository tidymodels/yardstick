test_that("`class_pred` can be converted to `factor` when computing metrics", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  cp_estimate <- probably::as_class_pred(two_class_example$predicted, which = 2)

  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  fct_estimate <- two_class_example$predicted
  fct_estimate[2] <- NA

  expect_identical(
    accuracy_vec(cp_truth, cp_estimate),
    accuracy_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    accuracy_vec(cp_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )
})

test_that("`class_pred` can be converted to `factor` with `conf_mat()`", {
  skip_if_not_installed("probably")

  cp_hpc_cv <- hpc_cv
  cp_hpc_cv$obs <- probably::as_class_pred(cp_hpc_cv$obs, which = 1)
  cp_hpc_cv$pred <- probably::as_class_pred(cp_hpc_cv$pred, which = 2)

  fct_hpc_cv <- hpc_cv
  fct_hpc_cv$obs[1] <- NA
  fct_hpc_cv$pred[2] <- NA

  expect_identical(
    conf_mat(cp_hpc_cv, obs, pred),
    conf_mat(fct_hpc_cv, obs, pred)
  )
})
