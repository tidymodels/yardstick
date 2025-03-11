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

test_that("metric_summarizer()'s na_rm argument work", {
  local_options(lifecycle_verbosity = "quiet")
  mtcars_na <- mtcars
  mtcars_na[1:5, 1] <- NA

  rmse_res <- metric_summarizer(
    metric_nm = "rmse",
    metric_fn = rmse_vec,
    data = mtcars_na,
    truth = mpg,
    estimate = disp,
    na_rm = TRUE,
    case_weights = NULL
  )

  rmse_exp <- dplyr::tibble(
    .metric = "rmse",
    .estimator = "standard",
    .estimate = rmse_vec(mtcars$mpg[-(1:5)], mtcars$disp[-(1:5)])
  )

  expect_identical(rmse_res, rmse_exp)

  rmse_res <- metric_summarizer(
    metric_nm = "rmse",
    metric_fn = rmse_vec,
    data = mtcars_na,
    truth = mpg,
    estimate = disp,
    na_rm = FALSE,
    case_weights = NULL
  )

  rmse_exp <- dplyr::tibble(
    .metric = "rmse",
    .estimator = "standard",
    .estimate = na_dbl
  )

  expect_identical(rmse_res, rmse_exp)
})

test_that("metric_summarizer()'s case_weights argument work", {
  local_options(lifecycle_verbosity = "quiet")
  rmse_res <- metric_summarizer(
    metric_nm = "rmse",
    metric_fn = rmse_vec,
    data = mtcars,
    truth = mpg,
    estimate = disp,
    na_rm = TRUE,
    case_weights = vs
  )

  rmse_exp <- dplyr::tibble(
    .metric = "rmse",
    .estimator = "standard",
    .estimate = rmse_vec(
      mtcars$mpg[mtcars$vs == 1],
      mtcars$disp[mtcars$vs == 1]
    )
  )

  expect_identical(rmse_res, rmse_exp)
})

test_that("metric_summarizer()'s errors when wrong things are passes", {
  local_options(lifecycle_verbosity = "quiet")
  expect_snapshot(
    error = TRUE,
    metric_summarizer(
      metric_nm = "rmse",
      metric_fn = rmse_vec,
      data = mtcars,
      truth = not_a_real_column_name,
      estimate = disp
    )
  )

  expect_snapshot(
    error = TRUE,
    metric_summarizer(
      metric_nm = "rmse",
      metric_fn = rmse_vec,
      data = mtcars,
      truth = mpg,
      estimate = not_a_real_column_name
    )
  )
})

test_that("metric_summarizer() deals with characters in truth and estimate", {
  local_options(lifecycle_verbosity = "quiet")
  rmse_res <- metric_summarizer(
    metric_nm = "rmse",
    metric_fn = rmse_vec,
    data = mtcars,
    truth = "mpg",
    estimate = "disp"
  )

  rmse_exp <- dplyr::tibble(
    .metric = "rmse",
    .estimator = "standard",
    .estimate = rmse_vec(mtcars$mpg, mtcars$disp)
  )

  expect_identical(rmse_res, rmse_exp)
})
