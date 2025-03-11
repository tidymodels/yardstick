test_that("result matches reference implementation (fairlearn)", {
  data("hpc_cv")
  py_res <- read_pydata("py-equalized_odds")

  hpc_cv$obs_vf = as.factor(hpc_cv$obs == "VF")
  hpc_cv$pred_vf = as.factor(hpc_cv$pred == "VF")
  hpc_cv$case_weights <- read_weights_hpc_cv()

  eo <- equalized_odds(Resample)

  expect_equal(
    eo(
      hpc_cv,
      truth = obs_vf,
      estimate = pred_vf,
      event_level = "second"
    )$.estimate,
    py_res$binary
  )

  expect_equal(
    eo(
      hpc_cv,
      truth = obs_vf,
      estimate = pred_vf,
      event_level = "second",
      case_weights = case_weights
    )$.estimate,
    py_res$weighted
  )
})
