test_that("missing values in case weights are considered by `na_rm`", {
  truth <- factor(c("x", "y"), levels = c("x", "y"))
  estimate <- factor(c("x", "x"), levels = c("x", "y"))
  case_weights <- c(1, NA)

  expect_identical(
    accuracy_vec(truth, estimate, case_weights = case_weights),
    1
  )

  expect_identical(
    accuracy_vec(truth, estimate, case_weights = case_weights, na_rm = FALSE),
    NA_real_
  )
})

## numeric_metric_summarizer --------------------------------------------------

test_that("numeric_metric_summarizer() works as expected", {
  rmse_res <- numeric_metric_summarizer(
    name = "rmse",
    fn = rmse_vec,
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


test_that("numeric_metric_summarizer()'s na_rm argument work", {
  mtcars_na <- mtcars
  mtcars_na[1:5, 1] <- NA

  rmse_res <- numeric_metric_summarizer(
    name = "rmse",
    fn = rmse_vec,
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

  rmse_res <- numeric_metric_summarizer(
    name = "rmse",
    fn = rmse_vec,
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

test_that("numeric_metric_summarizer()'s case_weights argument work", {
  rmse_res <- numeric_metric_summarizer(
    name = "rmse",
    fn = rmse_vec,
    data = mtcars,
    truth = mpg,
    estimate = disp,
    na_rm = TRUE,
    case_weights = vs
  )

  rmse_exp <- dplyr::tibble(
    .metric = "rmse",
    .estimator = "standard",
    .estimate = rmse_vec(mtcars$mpg[mtcars$vs == 1], mtcars$disp[mtcars$vs == 1])
  )

  expect_identical(rmse_res, rmse_exp)
})

test_that("numeric_metric_summarizer()'s errors when wrong things are passes", {
  expect_snapshot(error = TRUE,
     numeric_metric_summarizer(
       name = "rmse",
       fn = rmse_vec,
       data = mtcars,
       truth = not_a_real_column_name,
       estimate = disp
     )
  )

  expect_snapshot(error = TRUE,
    numeric_metric_summarizer(
      name = "rmse",
      fn = rmse_vec,
      data = mtcars,
      truth = mpg,
      estimate = not_a_real_column_name
    )
  )

  expect_snapshot(error = TRUE,
    numeric_metric_summarizer(
      name = "rmse",
      fn = rmse_vec,
      data = mtcars,
      truth = mpg,
      estimate = disp,
      obviouslywrong = TRUE
    )
  )
})

test_that("numeric_metric_summarizer() deals with characters in truth and estimate", {
  rmse_res <- numeric_metric_summarizer(
    name = "rmse",
    fn = rmse_vec,
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

## class_metric_summarizer --------------------------------------------------

test_that("class_metric_summarizer() works as expected", {
  three_class <- data_three_class()$three_class

  bal_accuracy_res <- class_metric_summarizer(
    name = "bal_accuracy",
    fn = bal_accuracy_vec,
    data = three_class,
    truth = obs,
    estimate = pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  bal_accuracy_exp <- dplyr::tibble(
    .metric = "bal_accuracy",
    .estimator = "macro",
    .estimate = bal_accuracy_vec(three_class$obs, three_class$pred)
  )

  expect_identical(bal_accuracy_res, bal_accuracy_exp)

  bal_accuracy_res <- class_metric_summarizer(
    name = "bal_accuracy",
    fn = bal_accuracy_vec,
    data = three_class,
    truth = obs,
    estimate = pred,
    estimator = "micro",
    na_rm = TRUE,
    case_weights = NULL
  )

  bal_accuracy_exp <- dplyr::tibble(
    .metric = "bal_accuracy",
    .estimator = "micro",
    .estimate = bal_accuracy_vec(
      truth = three_class$obs,
      estimate = three_class$pred,
      estimator = "micro"
    )
  )

  expect_identical(bal_accuracy_res, bal_accuracy_exp)
})

test_that("class_metric_summarizer()'s event_level works as expected", {
  lst <- data_altman()$pathology

  first_res <- class_metric_summarizer(
    name = "detection_prevalence",
    fn = detection_prevalence_vec,
    data = lst,
    truth = pathology,
    estimate = scan,
    event_level = "first"
  )

  second_res <- class_metric_summarizer(
    name = "detection_prevalence",
    fn = detection_prevalence_vec,
    data = lst,
    truth = pathology,
    estimate = scan,
    event_level = "second"
  )

  first_exp <- dplyr::tibble(
    .metric = "detection_prevalence",
    .estimator = "binary",
    .estimate = detection_prevalence_vec(
      truth = lst$pathology,
      estimate = lst$scan,
      event_level = "first"
    )
  )

  second_exp <- dplyr::tibble(
    .metric = "detection_prevalence",
    .estimator = "binary",
    .estimate = detection_prevalence_vec(
      truth = lst$pathology,
      estimate = lst$scan,
      event_level = "second"
    )
  )

  expect_identical(first_res, first_exp)
  expect_identical(second_res, second_exp)
})

test_that("class_metric_summarizer()'s na_rm argument work", {
  three_class <- data_three_class()$three_class

  accuracy_res <- class_metric_summarizer(
    name = "accuracy",
    fn = accuracy_vec,
    data = three_class,
    truth = obs,
    estimate = pred_na,
    na_rm = TRUE,
    case_weights = NULL
  )

  accuracy_exp <- dplyr::tibble(
    .metric = "accuracy",
    .estimator = "multiclass",
    .estimate = accuracy_vec(three_class$obs, three_class$pred_na)
  )

  expect_identical(accuracy_res, accuracy_exp)

  accuracy_res <- class_metric_summarizer(
    name = "accuracy",
    fn = accuracy_vec,
    data = three_class,
    truth = obs,
    estimate = pred_na,
    na_rm = FALSE,
    case_weights = NULL
  )

  accuracy_exp <- dplyr::tibble(
    .metric = "accuracy",
    .estimator = "multiclass",
    .estimate = na_dbl
  )

  expect_identical(accuracy_res, accuracy_exp)
})

test_that("class_metric_summarizer()'s case_weights argument work", {
  three_class <- data_three_class()$three_class
  three_class$weights <- rep(c(1, 0), c(100, 50))

  accuracy_res <- class_metric_summarizer(
    name = "accuracy",
    fn = accuracy_vec,
    data = three_class,
    truth = obs,
    estimate = pred,
    na_rm = TRUE,
    case_weights = weights
  )

  accuracy_exp <- dplyr::tibble(
    .metric = "accuracy",
    .estimator = "multiclass",
    .estimate = accuracy_vec(three_class$obs[1:100], three_class$pred[1:100])
  )

  expect_identical(accuracy_res, accuracy_exp)
})

test_that("class_metric_summarizer()'s errors when wrong things are passes", {
  three_class <- data_three_class()$three_class

  expect_snapshot(error = TRUE,
    class_metric_summarizer(
      name = "accuracy",
      fn = accuracy_vec,
      data = three_class,
      truth = not_a_real_column_name,
      estimate = pred
    )
  )

  expect_snapshot(error = TRUE,
    class_metric_summarizer(
      name = "accuracy",
      fn = accuracy_vec,
      data = three_class,
      truth = obs,
      estimate = not_a_real_column_name
    )
  )

  expect_snapshot(error = TRUE,
    class_metric_summarizer(
      name = "accuracy",
      fn = accuracy_vec,
      data = three_class,
      truth = obs,
      estimate = pred,
      obviouslywrong = TRUE
    )
  )

})

test_that("class_metric_summarizer() deals with characters in truth and estimate", {
  three_class <- data_three_class()$three_class

  accuracy_res <- class_metric_summarizer(
    name = "accuracy",
    fn = accuracy_vec,
    data = three_class,
    truth = "obs",
    estimate = "pred"
  )


  accuracy_exp <- dplyr::tibble(
    .metric = "accuracy",
    .estimator = "multiclass",
    .estimate = accuracy_vec(three_class$obs, three_class$pred)
  )

  expect_identical(accuracy_res, accuracy_exp)
})

## prob_metric_summarizer --------------------------------------------------

test_that("prob_metric_summarizer() works as expected", {
  hpc_f1 <- data_hpc_fold1()

  roc_auc_res <- prob_metric_summarizer(
    name = "roc_auc",
    fn = roc_auc_vec,
    data = hpc_f1,
    truth = obs,
    VF:L,
    na_rm = TRUE,
    case_weights = NULL
  )

  roc_auc_exp <- dplyr::tibble(
    .metric = "roc_auc",
    .estimator = "hand_till",
    .estimate = roc_auc_vec(hpc_f1$obs, as.matrix(hpc_f1[3:6]))
  )

  expect_identical(roc_auc_res, roc_auc_exp)

  roc_auc_res <- prob_metric_summarizer(
    name = "roc_auc",
    fn = roc_auc_vec,
    data = hpc_f1,
    truth = obs,
    VF:L,
    estimator = "macro",
    na_rm = TRUE,
    case_weights = NULL
  )

  roc_auc_exp <- dplyr::tibble(
    .metric = "roc_auc",
    .estimator = "macro",
    .estimate = roc_auc_vec(hpc_f1$obs, as.matrix(hpc_f1[3:6]), estimator = "macro")
  )

  expect_identical(roc_auc_res, roc_auc_exp)
})

test_that("class_metric_summarizer()'s event_level works as expected", {
  hpc_f1 <- data_hpc_fold1()
  hpc_f1$obs <- factor(hpc_f1$obs == "VF",
                       levels = c(TRUE, FALSE),
                       labels = c("VF", "nVF"))

  first_res <- prob_metric_summarizer(
    name = "gain_capture",
    fn = gain_capture_vec,
    data = hpc_f1,
    truth = obs,
    VF,
    event_level = "first"
  )

  second_res <- prob_metric_summarizer(
    name = "gain_capture",
    fn = gain_capture_vec,
    data = hpc_f1,
    truth = obs,
    VF,
    event_level = "second"
  )

  first_exp <- dplyr::tibble(
    .metric = "gain_capture",
    .estimator = "binary",
    .estimate = gain_capture_vec(
      truth = hpc_f1$obs,
      estimate = hpc_f1$VF,
      event_level = "first"
    )
  )

  second_exp <- dplyr::tibble(
    .metric = "gain_capture",
    .estimator = "binary",
    .estimate = gain_capture_vec(
      truth = hpc_f1$obs,
      estimate = hpc_f1$VF,
      event_level = "second"
    )
  )

  expect_identical(first_res, first_exp)
  expect_identical(second_res, second_exp)
})

test_that("prob_metric_summarizer()'s na_rm argument work", {
  hpc_f1 <- data_hpc_fold1()
  hpc_f1_na <- hpc_f1
  hpc_f1_na$VF[1:5] <- NA

  roc_auc_res <- prob_metric_summarizer(
    name = "roc_auc",
    fn = roc_auc_vec,
    data = hpc_f1_na,
    truth = obs,
    VF:L,
    na_rm = TRUE,
    case_weights = NULL
  )

  roc_auc_exp <- dplyr::tibble(
    .metric = "roc_auc",
    .estimator = "hand_till",
    .estimate = roc_auc_vec(hpc_f1$obs[-(1:5)], as.matrix(hpc_f1[-(1:5), 3:6]))
  )

  expect_identical(roc_auc_res, roc_auc_exp)

  roc_auc_res <- prob_metric_summarizer(
    name = "roc_auc",
    fn = roc_auc_vec,
    data = hpc_f1_na,
    truth = obs,
    VF:L,
    na_rm = FALSE,
    case_weights = NULL
  )

  roc_auc_exp <- dplyr::tibble(
    .metric = "roc_auc",
    .estimator = "hand_till",
    .estimate = na_dbl
  )

  expect_identical(roc_auc_res, roc_auc_exp)
})

test_that("prob_metric_summarizer()'s case_weights argument work", {
  hpc_f1 <- data_hpc_fold1()
  hpc_f1$weights <- rep(c(1, 0), c(340, 7))

  roc_auc_res <- prob_metric_summarizer(
    name = "roc_auc",
    fn = roc_auc_vec,
    data = hpc_f1,
    truth = obs,
    VF:L,
    na_rm = TRUE,
    case_weights = weights
  )

  roc_auc_exp <- dplyr::tibble(
    .metric = "roc_auc",
    .estimator = "hand_till",
    .estimate = roc_auc_vec(
      truth = hpc_f1$obs,
      estimate = as.matrix(hpc_f1[3:6]),
      case_weights = rep(c(1, 0), c(340, 7))
    )
  )

  expect_identical(roc_auc_res, roc_auc_exp)
})

test_that("prob_metric_summarizer()'s errors when wrong things are passes", {
  hpc_f1 <- data_hpc_fold1()

  expect_snapshot(error = TRUE,
    prob_metric_summarizer(
      name = "roc_auc",
      fn = roc_auc_vec,
      data = hpc_f1,
      truth = obs,
      c(HELLO, F, M, L)
    )
  )

  expect_snapshot(error = TRUE,
    prob_metric_summarizer(
      name = "roc_auc",
      fn = roc_auc_vec,
      data = hpc_f1,
      truth = obviouslywrong,
      VF:L
    )
  )

  expect_snapshot(error = TRUE,
    prob_metric_summarizer(
      name = "roc_auc",
      fn = roc_auc_vec,
      data = hpc_f1,
      truth = obs,
      VF:L,
      obviouslywrong = TRUE
    )
  )

})

test_that("prob_metric_summarizer() deals with characters in truth", {
  hpc_f1 <- data_hpc_fold1()

  roc_auc_res <- prob_metric_summarizer(
    name = "roc_auc",
    fn = roc_auc_vec,
    data = hpc_f1,
    truth = "obs",
    VF:L,
    na_rm = TRUE,
    case_weights = NULL
  )

  roc_auc_exp <- dplyr::tibble(
    .metric = "roc_auc",
    .estimator = "hand_till",
    .estimate = roc_auc_vec(hpc_f1$obs, as.matrix(hpc_f1[3:6]))
  )

  expect_identical(roc_auc_res, roc_auc_exp)
})

## curve_metric_summarizer --------------------------------------------------

test_that("curve_metric_summarizer() works as expected", {
  hpc_f1 <- data_hpc_fold1()

  roc_curve_res <- curve_metric_summarizer(
    name = "roc_curve",
    fn = roc_curve_vec,
    data = hpc_f1,
    truth = obs,
    VF:L,
    na_rm = TRUE,
    case_weights = NULL
  )

  roc_curve_exp <- dplyr::tibble(
    .metric = "roc_curve",
    .estimator = "multiclass",
    .estimate = roc_curve_vec(hpc_f1$obs, as.matrix(hpc_f1[3:6]))
  )

  expect_identical(roc_curve_res, roc_curve_exp)
})

test_that("class_metric_summarizer()'s event_level works as expected", {
  hpc_f1 <- data_hpc_fold1()
  hpc_f1$obs <- factor(hpc_f1$obs == "VF",
                       levels = c(TRUE, FALSE),
                       labels = c("VF", "nVF"))

  first_res <- curve_metric_summarizer(
    name = "gain_capture",
    fn = gain_capture_vec,
    data = hpc_f1,
    truth = obs,
    VF,
    event_level = "first"
  )

  second_res <- curve_metric_summarizer(
    name = "gain_capture",
    fn = gain_capture_vec,
    data = hpc_f1,
    truth = obs,
    VF,
    event_level = "second"
  )

  first_exp <- dplyr::tibble(
    .metric = "gain_capture",
    .estimator = "binary",
    .estimate = gain_capture_vec(
      truth = hpc_f1$obs,
      estimate = hpc_f1$VF,
      event_level = "first"
    )
  )

  second_exp <- dplyr::tibble(
    .metric = "gain_capture",
    .estimator = "binary",
    .estimate = gain_capture_vec(
      truth = hpc_f1$obs,
      estimate = hpc_f1$VF,
      event_level = "second"
    )
  )

  expect_identical(first_res, first_exp)
  expect_identical(second_res, second_exp)
})

test_that("curve_metric_summarizer()'s na_rm argument work", {
  hpc_f1 <- data_hpc_fold1()
  hpc_f1_na <- hpc_f1
  hpc_f1_na$VF[1:5] <- NA

  roc_curve_res <- curve_metric_summarizer(
    name = "roc_curve",
    fn = roc_curve_vec,
    data = hpc_f1_na,
    truth = obs,
    VF:L,
    na_rm = TRUE,
    case_weights = NULL
  )

  roc_curve_exp <- dplyr::tibble(
    .metric = "roc_curve",
    .estimator = "multiclass",
    .estimate = roc_curve_vec(hpc_f1$obs[-(1:5)], as.matrix(hpc_f1[-(1:5), 3:6]))
  )

  expect_identical(roc_curve_res, roc_curve_exp)

  roc_curve_res <- curve_metric_summarizer(
    name = "roc_curve",
    fn = roc_curve_vec,
    data = hpc_f1_na,
    truth = obs,
    VF:L,
    na_rm = FALSE,
    case_weights = NULL
  )

  roc_curve_exp <- dplyr::tibble(
    .metric = "roc_curve",
    .estimator = "multiclass",
    .estimate = na_dbl
  )

  expect_identical(roc_curve_res, roc_curve_exp)
})

test_that("curve_metric_summarizer()'s case_weights argument work", {
  hpc_f1 <- data_hpc_fold1()
  hpc_f1$weights <- rep(c(1, 0), c(340, 7))

  roc_curve_res <- curve_metric_summarizer(
    name = "roc_curve",
    fn = roc_curve_vec,
    data = hpc_f1,
    truth = obs,
    VF:L,
    na_rm = TRUE,
    case_weights = weights
  )

  roc_curve_exp <- dplyr::tibble(
    .metric = "roc_curve",
    .estimator = "multiclass",
    .estimate = roc_curve_vec(
      truth = hpc_f1$obs,
      estimate = as.matrix(hpc_f1[3:6]),
      case_weights = rep(c(1, 0), c(340, 7))
    )
  )

  expect_identical(roc_curve_res, roc_curve_exp)
})

test_that("curve_metric_summarizer()'s errors when wrong things are passes", {
  hpc_f1 <- data_hpc_fold1()

  expect_snapshot(error = TRUE,
                  curve_metric_summarizer(
                    name = "roc_curve",
                    fn = roc_curve_vec,
                    data = hpc_f1,
                    truth = obs,
                    c(HELLO, F, M, L)
                  )
  )

  expect_snapshot(error = TRUE,
                  curve_metric_summarizer(
                    name = "roc_curve",
                    fn = roc_curve_vec,
                    data = hpc_f1,
                    truth = obviouslywrong,
                    VF:L
                  )
  )

  expect_snapshot(error = TRUE,
                  curve_metric_summarizer(
                    name = "roc_curve",
                    fn = roc_curve_vec,
                    data = hpc_f1,
                    truth = obs,
                    VF:L,
                    obviouslywrong = TRUE
                  )
  )

})

test_that("curve_metric_summarizer() deals with characters in truth", {
  hpc_f1 <- data_hpc_fold1()

  roc_curve_res <- curve_metric_summarizer(
    name = "roc_curve",
    fn = roc_curve_vec,
    data = hpc_f1,
    truth = "obs",
    VF:L,
    na_rm = TRUE,
    case_weights = NULL
  )

  roc_curve_exp <- dplyr::tibble(
    .metric = "roc_curve",
    .estimator = "multiclass",
    .estimate = roc_curve_vec(hpc_f1$obs, as.matrix(hpc_f1[3:6]))
  )

  expect_identical(roc_curve_res, roc_curve_exp)
})

## dynamic_survival_metric_summarizer -----------------------------------------

# To be removed once brier_survival() is added
brier_survival_vec <- function(truth,
                               estimate,
                               censoring_weights,
                               eval_times,
                               na_rm = TRUE,
                               case_weights = NULL,
                               ...) {
  check_dynamic_survival_metric(
    truth, estimate, censoring_weights, case_weights, eval_times
  )

  n_distinct_time <- dplyr::n_distinct(eval_times)
  if (n_distinct_time != 1) {
    abort(paste0(
      "`eval_times` should have at most 1 unique value. But ", n_distinct_time,
      " was detected."
    ))
  }

  if (na_rm) {
    result <- yardstick_remove_missing(
      truth, estimate, case_weights, censoring_weights, eval_times
    )

    truth <- result$truth
    estimate <- result$estimate
    censoring_weights <- result$censoring_weights
    eval_times <- result$eval_times
    case_weights <- result$case_weights
  } else {
    any_missing <- yardstick_any_missing(
      truth, estimate, case_weights, censoring_weights, eval_times
    )
    if (any_missing) {
      return(NA_real_)
    }
  }

  # non-sensible calculation, just to generate result we can test with
  sum(truth * estimate * case_weights * censoring_weights * eval_times)
}

test_that("dynamic_survival_metric_summarizer() works as expected", {
  lung_surv <- data_lung_surv() %>% dplyr::filter(.time == 100)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_survival,
    censoring_weights = prob_censored,
    eval_times = .time,
    na_rm = TRUE,
    case_weights = NULL
  )

  brier_survival_exp <- dplyr::tibble(
    .metric = "brier_survival",
    .estimator = "standard",
    .estimate = brier_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred_survival,
      censoring_weights = lung_surv$prob_censored,
      eval_times = lung_surv$.time
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})

test_that("dynamic_survival_metric_summarizer()'s na_rm argument work", {
  lung_surv <- data_lung_surv() %>% dplyr::filter(.time == 100)
  lung_surv[1:5, 1] <- NA

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    censoring_weights = prob_censored,
    estimate = .pred_survival,
    eval_times = .time,
    na_rm = TRUE,
    case_weights = NULL
  )

  surv_subset <- function(x, i) {
    res <- x[i, ]
    class(res) <- class(x)
    attr(res, "type") <- attr(x, "type")
    res
  }

  brier_survival_exp <- dplyr::tibble(
    .metric = "brier_survival",
    .estimator = "standard",
    .estimate = brier_survival_vec(
      truth = surv_subset(lung_surv$surv_obj, -c(1:5)),
      estimate = lung_surv$.pred_survival[-c(1:5)],
      censoring_weights = lung_surv$prob_censored[-c(1:5)],
      eval_times = lung_surv$.time[-c(1:5)]
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    censoring_weights = prob_censored,
    estimate = .pred_survival,
    eval_times = .time,
    na_rm = FALSE,
    case_weights = NULL
  )

  brier_survival_exp <- dplyr::tibble(
    .metric = "brier_survival",
    .estimator = "standard",
    .estimate = na_dbl
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})

test_that("dynamic_survival_metric_summarizer()'s case_weights argument work", {
  lung_surv <- data_lung_surv() %>% dplyr::filter(.time == 100)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    censoring_weights = prob_censored,
    estimate = .pred_survival,
    eval_times = .time,
    na_rm = TRUE,
    case_weights = ph.ecog
  )

  brier_survival_exp <- dplyr::tibble(
    .metric = "brier_survival",
    .estimator = "standard",
    .estimate = brier_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred_survival,
      censoring_weights = lung_surv$prob_censored,
      eval_times = lung_surv$.time,
      case_weights = lung_surv$ph.ecog
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})

test_that("dynamic_survival_metric_summarizer()'s errors when wrong things are passes", {
  lung_surv <- data_lung_surv() %>% dplyr::filter(.time == 100)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    censoring_weights = prob_censored,
    estimate = .pred_survival,
    eval_times = .time,
    na_rm = TRUE,
    case_weights = NULL
  )

  expect_snapshot(
    error = TRUE,
    dynamic_survival_metric_summarizer(
      name = "brier_survival",
      fn = brier_survival_vec,
      data = lung_surv,
      truth = inst,
      estimate = .pred_survival,
      censoring_weights = prob_censored,
      eval_times = .time
    )
  )

  expect_snapshot(
    error = TRUE,
    dynamic_survival_metric_summarizer(
      name = "brier_survival",
      fn = brier_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      estimate = surv_obj,
      censoring_weights = prob_censored,
      eval_times = .time
    )
  )

  expect_snapshot(
    error = TRUE,
    dynamic_survival_metric_summarizer(
      name = "brier_survival",
      fn = brier_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      estimate = .pred,
      censoring_weights = prob_censored,
      eval_times = .time,
      obviouslywrong = TRUE
    )
  )
})

test_that("dynamic_survival_metric_summarizer() deals with characters in truth and estimate", {
  lung_surv <- data_lung_surv() %>% dplyr::filter(.time == 100)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = "surv_obj",
    estimate = ".pred_survival",
    censoring_weights = "prob_censored",
    eval_times = ".time",
    na_rm = TRUE,
    case_weights = NULL
  )

  brier_survival_exp <- dplyr::tibble(
    .metric = "brier_survival",
    .estimator = "standard",
    .estimate = brier_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred_survival,
      censoring_weights = lung_surv$prob_censored,
      eval_times = lung_surv$.time
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})
