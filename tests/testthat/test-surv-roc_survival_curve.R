test_that("roc_curve_survival works", {
  result <- roc_curve_survival(
    lung_surv,
    truth = surv_obj,
    .pred
  )

  expect_identical(
    names(result),
    c(".threshold", "sensitivity", "specificity", ".eval_time")
  )

  .eval_times <- unique(result$.eval_time)

  eval_time <- 200
  for (eval_time in .eval_times) {
    result_tmp <- dplyr::filter(result, .eval_time == eval_time)

    exp_threshold <- tidyr::unnest(lung_surv, cols = .pred)
    exp_threshold <- dplyr::filter(exp_threshold, .eval_time == eval_time)
    exp_threshold <- exp_threshold$.pred_survival
    exp_threshold <- sort(exp_threshold)
    exp_threshold <- unique(exp_threshold)
    exp_threshold <- c(-Inf, exp_threshold, Inf)
    expect_identical(
      result_tmp$.threshold,
      exp_threshold
    )

    expect_true(
      all(diff(result_tmp$sensitivity) >= 0)
    )

    expect_true(
      all(diff(result_tmp$specificity) <= 0)
    )
  }
})
