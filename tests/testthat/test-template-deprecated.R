test_that("metric_summarizer() is soft-deprecated", {
  local_options(lifecycle_verbosity = "warning")
  expect_snapshot(
    tmp <- metric_summarizer(
      metric_nm = "rmse",
      metric_fn = rmse_vec,
      data = mtcars,
      truth = mpg,
      estimate = disp,
      na_rm = TRUE,
      case_weights = NULL
    )
  )
})

test_that("metric_summarizer() still works", {
  local_options(lifecycle_verbosity = "quiet")
  rmse_res <- metric_summarizer(
    metric_nm = "rmse",
    metric_fn = rmse_vec,
    data = mtcars,
    truth = mpg,
    estimate = disp,
    na_rm = TRUE,
    case_weights = NULL
  )

  rmse_exp <- dplyr::tibble(
    .metric = "rmse",
    .estimator = "standard",
    .estimate = rmse_vec(mtcars$mpg, mtcars$disp)
  )

  expect_identical(rmse_res, rmse_exp)
})
