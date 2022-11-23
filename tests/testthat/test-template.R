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

## numeric_metric_vec_template ------------------------------------------------

test_that("numeric_metric_vec_template() works as expected", {
  res <- numeric_metric_vec_template(
    metric_impl = ccc_impl,
    truth = mtcars$mpg,
    estimate = mtcars$disp,
    na_rm = FALSE,
    case_weights = NULL,
    bias = FALSE
  )

  exp_res <- ccc_impl(
    truth = mtcars$mpg,
    estimate = mtcars$disp,
    bias = FALSE,
    case_weights = NULL
  )

  expect_identical(res, exp_res)
})

test_that("numeric_metric_vec_template() validates case_weights", {
  res <- numeric_metric_vec_template(
    metric_impl = ccc_impl,
    truth = mtcars$mpg,
    estimate = mtcars$disp,
    na_rm = FALSE,
    case_weights = mtcars$mpg,
    bias = FALSE
  )

  exp_res <- ccc_impl(
    truth = mtcars$mpg,
    estimate = mtcars$disp,
    bias = FALSE,
    case_weights = mtcars$mpg
  )

  expect_identical(res, exp_res)

  expect_snapshot(
    error = TRUE,
    numeric_metric_vec_template(
      metric_impl = ccc_impl,
      truth = mtcars$mpg,
      estimate = mtcars$disp,
      na_rm = FALSE,
      case_weights = mtcars$mpg[-1],
      bias = FALSE
    )
  )
})

test_that("numeric_metric_vec_template() na_rm argument works", {
  res <- numeric_metric_vec_template(
    metric_impl = ccc_impl,
    truth = mtcars$mpg,
    estimate = mtcars$disp,
    na_rm = TRUE,
    case_weights = NULL,
    bias = FALSE
  )

  exp_res <- ccc_impl(
    truth = mtcars$mpg,
    estimate = mtcars$disp,
    bias = FALSE,
    case_weights = NULL
  )

  expect_identical(res, exp_res)

  res <- numeric_metric_vec_template(
    metric_impl = ccc_impl,
    truth = c(mtcars$mpg[1:20], rep(NA, 12)),
    estimate = mtcars$disp,
    na_rm = TRUE,
    case_weights = mtcars$mpg,
    bias = FALSE
  )

  exp_res <- ccc_impl(
    truth = mtcars$mpg[1:20],
    estimate = mtcars$disp[1:20],
    bias = FALSE,
    case_weights = mtcars$mpg[1:20]
  )

  expect_identical(res, exp_res)

  res <- numeric_metric_vec_template(
    metric_impl = ccc_impl,
    truth = mtcars$mpg,
    estimate = c(mtcars$disp[1:20], rep(NA, 12)),
    na_rm = TRUE,
    case_weights = mtcars$mpg,
    bias = FALSE
  )

  exp_res <- ccc_impl(
    truth = mtcars$mpg[1:20],
    estimate = mtcars$disp[1:20],
    bias = FALSE,
    case_weights = mtcars$mpg[1:20]
  )

  expect_identical(res, exp_res)

  res <- numeric_metric_vec_template(
    metric_impl = ccc_impl,
    truth = mtcars$mpg,
    estimate = mtcars$disp,
    na_rm = TRUE,
    case_weights = c(mtcars$mpg[1:20], rep(NA, 12)),
    bias = FALSE
  )

  exp_res <- ccc_impl(
    truth = mtcars$mpg[1:20],
    estimate = mtcars$disp[1:20],
    bias = FALSE,
    case_weights = mtcars$mpg[1:20]
  )

  expect_identical(res, exp_res)

  res <- numeric_metric_vec_template(
    metric_impl = ccc_impl,
    truth = c(mtcars$mpg[1:20], rep(NA, 12)),
    estimate = mtcars$disp,
    na_rm = FALSE,
    case_weights = mtcars$mpg,
    bias = FALSE
  )

  expect_identical(res, NA_real_)

  res <- numeric_metric_vec_template(
    metric_impl = ccc_impl,
    truth = mtcars$mpg,
    estimate = c(mtcars$disp[1:20], rep(NA, 12)),
    na_rm = FALSE,
    case_weights = mtcars$mpg,
    bias = FALSE
  )

  expect_identical(res, NA_real_)

  res <- numeric_metric_vec_template(
    metric_impl = ccc_impl,
    truth = mtcars$mpg,
    estimate = mtcars$disp,
    na_rm = FALSE,
    case_weights = c(mtcars$mpg[1:20], rep(NA, 12)),
    bias = FALSE
  )

  expect_identical(res, NA_real_)
})

test_that("numeric_metric_vec_template() dots are passed through", {
  res <- numeric_metric_vec_template(
    metric_impl = ccc_impl,
    truth = mtcars$mpg,
    estimate = mtcars$disp,
    na_rm = FALSE,
    case_weights = NULL,
    bias = TRUE
  )

  exp_res <- ccc_impl(
    truth = mtcars$mpg,
    estimate = mtcars$disp,
    bias = TRUE,
    case_weights = NULL
  )

  expect_identical(res, exp_res)
})

## class_metric_vec_template --------------------------------------------------

test_that("class_metric_vec_template() works as expected", {
  lst <- data_altman()$pathology

  res <- class_metric_vec_template(
    metric_impl = kap_impl,
    truth = lst$pathology,
    estimate = lst$scan,
    na_rm = FALSE,
    case_weights = NULL,
    weighting = "none"
  )

  exp_res <- kap_impl(
    truth = lst$pathology,
    estimate = lst$scan,
    weighting = "none",
    case_weights = NULL
  )

  expect_identical(res, exp_res)
})

test_that("class_metric_vec_template() validates case_weights", {
  lst <- data_altman()$pathology

  res <- class_metric_vec_template(
    metric_impl = kap_impl,
    truth = lst$pathology,
    estimate = lst$scan,
    na_rm = FALSE,
    case_weights = seq_along(lst$pathology),
    weighting = "none"
  )

  exp_res <- kap_impl(
    truth = lst$pathology,
    estimate = lst$scan,
    weighting = "none",
    case_weights = seq_along(lst$pathology)
  )

  expect_identical(res, exp_res)

  expect_snapshot(
    error = TRUE,
    class_metric_vec_template(
      metric_impl = kap_impl,
      truth = lst$pathology,
      estimate = lst$scan,
      na_rm = FALSE,
      case_weights = seq_along(lst$pathology)[-1],
      weighting = "none"
    )
  )
})

test_that("class_metric_vec_template() na_rm argument works", {
  lst <- data_altman()$pathology
  inject_na <- function(x) {
    x[201:344] <- NA
    x
  }

  res <- class_metric_vec_template(
    metric_impl = kap_impl,
    truth = lst$pathology,
    estimate = lst$scan,
    na_rm = TRUE,
    case_weights = NULL,
    weighting = "none"
  )

  exp_res <- kap_impl(
    truth = lst$pathology,
    estimate = lst$scan,
    weighting = "none",
    case_weights = NULL
  )

  expect_identical(res, exp_res)

  res <- class_metric_vec_template(
    metric_impl = kap_impl,
    truth = inject_na(lst$pathology),
    estimate = lst$scan,
    na_rm = TRUE,
    case_weights = seq_along(lst$pathology),
    weighting = "none"
  )

  exp_res <- kap_impl(
    truth = lst$pathology[1:200],
    estimate = lst$scan[1:200],
    weighting = "none",
    case_weights = seq_along(lst$pathology)[1:200]
  )

  expect_identical(res, exp_res)

  res <- class_metric_vec_template(
    metric_impl = kap_impl,
    truth = lst$pathology,
    estimate = inject_na(lst$scan),
    na_rm = TRUE,
    case_weights = seq_along(lst$pathology),
    weighting = "none"
  )

  exp_res <- kap_impl(
    truth = lst$pathology[1:200],
    estimate = lst$scan[1:200],
    weighting = "none",
    case_weights = seq_along(lst$pathology)[1:200]
  )

  expect_identical(res, exp_res)

  res <- class_metric_vec_template(
    metric_impl = kap_impl,
    truth = lst$pathology,
    estimate = lst$scan,
    na_rm = TRUE,
    case_weights = c(seq_along(lst$pathology)[1:200], rep(NA, 144)),
    weighting = "none"
  )

  exp_res <- kap_impl(
    truth = lst$pathology[1:200],
    estimate = lst$scan[1:200],
    weighting = "none",
    case_weights = seq_along(lst$pathology)[1:200]
  )

  expect_identical(res, exp_res)

  res <- class_metric_vec_template(
    metric_impl = kap_impl,
    truth = inject_na(lst$pathology),
    estimate = lst$scan,
    na_rm = FALSE,
    case_weights = seq_along(lst$pathology),
    weighting = "none"
  )

  expect_identical(res, NA_real_)

  res <- class_metric_vec_template(
    metric_impl = kap_impl,
    truth = lst$pathology,
    estimate = inject_na(lst$scan),
    na_rm = FALSE,
    case_weights = seq_along(lst$pathology),
    weighting = "none"
  )

  expect_identical(res, NA_real_)

  res <- class_metric_vec_template(
    metric_impl = kap_impl,
    truth = lst$pathology,
    estimate = lst$scan,
    na_rm = FALSE,
    case_weights = c(seq_along(lst$pathology)[1:200], rep(NA, 144)),
    weighting = "none"
  )

  expect_identical(res, NA_real_)
})

test_that("class_metric_vec_template() dots are passed through", {
  lst <- data_altman()$pathology

  res <- class_metric_vec_template(
    metric_impl = kap_impl,
    truth = lst$pathology,
    estimate = lst$scan,
    na_rm = FALSE,
    case_weights = NULL,
    weighting = "linear"
  )

  exp_res <- kap_impl(
    truth = lst$pathology,
    estimate = lst$scan,
    weighting = "linear",
    case_weights = NULL
  )

  expect_identical(res, exp_res)
})

## prob_metric_vec_template --------------------------------------------------

test_that("prob_metric_vec_template() works as expected", {
  hpc_f1 <- data_hpc_fold1()

  mn_log_loss_impl <- function(truth,
                               estimate,
                               ...,
                               sum = FALSE,
                               case_weights = NULL) {
    check_dots_empty()

    mn_log_loss_estimator_impl(
      truth = truth,
      estimate = estimate,
      estimator = "multiclass",
      event_level = "first",
      sum = sum,
      case_weights = case_weights
    )
  }

  res <- prob_metric_vec_template(
    metric_impl = mn_log_loss_impl,
    truth = hpc_f1$obs,
    estimate = as.matrix(hpc_f1[, 3:6]),
    na_rm = FALSE,
    case_weights = NULL,
    sum = FALSE
  )

  exp_res <- mn_log_loss_impl(
    truth = hpc_f1$obs,
    estimate = as.matrix(hpc_f1[, 3:6]),
    sum = FALSE,
    case_weights = NULL
  )

  expect_identical(res, exp_res)
})

test_that("prob_metric_vec_template() validates case_weights", {
  hpc_f1 <- data_hpc_fold1()

  mn_log_loss_impl <- function(truth,
                               estimate,
                               ...,
                               sum = FALSE,
                               case_weights = NULL) {
    check_dots_empty()

    mn_log_loss_estimator_impl(
      truth = truth,
      estimate = estimate,
      estimator = "multiclass",
      event_level = "first",
      sum = sum,
      case_weights = case_weights
    )
  }

  res <- prob_metric_vec_template(
    metric_impl = mn_log_loss_impl,
    truth = hpc_f1$obs,
    estimate = as.matrix(hpc_f1[, 3:6]),
    na_rm = FALSE,
    case_weights = seq_along(hpc_f1$obs),
    sum = FALSE
  )

  exp_res <- mn_log_loss_impl(
    truth = hpc_f1$obs,
    estimate = as.matrix(hpc_f1[, 3:6]),
    sum = FALSE,
    case_weights = seq_along(hpc_f1$obs)
  )

  expect_identical(res, exp_res)

  expect_snapshot(
    error = TRUE,
    prob_metric_vec_template(
      metric_impl = mn_log_loss_impl,
      truth = hpc_f1$obs,
      estimate = as.matrix(hpc_f1[, 3:6]),
      na_rm = FALSE,
      case_weights = seq_along(lst$pathology)[-1],
      sum = FALSE
    )
  )
})

test_that("prob_metric_vec_template() na_rm argument works", {
  hpc_f1 <- data_hpc_fold1()

  mn_log_loss_impl <- function(truth,
                               estimate,
                               ...,
                               sum = FALSE,
                               case_weights = NULL) {
    check_dots_empty()

    mn_log_loss_estimator_impl(
      truth = truth,
      estimate = estimate,
      estimator = "multiclass",
      event_level = "first",
      sum = sum,
      case_weights = case_weights
    )
  }

  inject_na <- function(x) {
    x[201:347] <- NA
    x
  }

  res <- prob_metric_vec_template(
    metric_impl = mn_log_loss_impl,
    truth = hpc_f1$obs,
    estimate = as.matrix(hpc_f1[, 3:6]),
    na_rm = TRUE,
    case_weights = NULL,
    sum = FALSE
  )

  exp_res <- mn_log_loss_impl(
    truth = hpc_f1$obs,
    estimate = as.matrix(hpc_f1[, 3:6]),
    sum = FALSE,
    case_weights = NULL
  )

  expect_identical(res, exp_res)

  res <- prob_metric_vec_template(
    metric_impl = mn_log_loss_impl,
    truth = inject_na(hpc_f1$obs),
    estimate = as.matrix(hpc_f1[, 3:6]),
    na_rm = TRUE,
    case_weights = seq_along(hpc_f1$obs),
    sum = FALSE
  )

  exp_res <- mn_log_loss_impl(
    truth = hpc_f1$obs[1:200],
    estimate = as.matrix(hpc_f1[, 3:6])[1:200, ],
    sum = FALSE,
    case_weights = seq_along(hpc_f1$obs)[1:200]
  )

  expect_identical(res, exp_res)

  res <- prob_metric_vec_template(
    metric_impl = mn_log_loss_impl,
    truth = hpc_f1$obs,
    estimate = inject_na(as.matrix(hpc_f1[, 3:6])),
    na_rm = TRUE,
    case_weights = seq_along(hpc_f1$obs),
    sum = FALSE
  )

  exp_res <- mn_log_loss_impl(
    truth = hpc_f1$obs[1:200],
    estimate = as.matrix(hpc_f1[, 3:6])[1:200, ],
    sum = FALSE,
    case_weights = seq_along(hpc_f1$obs)[1:200]
  )

  expect_identical(res, exp_res)

  res <- prob_metric_vec_template(
    metric_impl = mn_log_loss_impl,
    truth = hpc_f1$obs,
    estimate = as.matrix(hpc_f1[, 3:6]),
    na_rm = TRUE,
    case_weights = c(seq_along(hpc_f1$obs)[1:200], rep(NA, 147)),
    sum = FALSE
  )

  exp_res <- mn_log_loss_impl(
    truth = hpc_f1$obs[1:200],
    estimate = as.matrix(hpc_f1[, 3:6])[1:200, ],
    sum = FALSE,
    case_weights = seq_along(hpc_f1$obs)[1:200]
  )

  expect_identical(res, exp_res)

  res <- prob_metric_vec_template(
    metric_impl = mn_log_loss_impl,
    truth = inject_na(hpc_f1$obs),
    estimate = as.matrix(hpc_f1[, 3:6]),
    na_rm = FALSE,
    case_weights = seq_along(hpc_f1$obs),
    sum = FALSE
  )

  expect_identical(res, NA_real_)

  res <- prob_metric_vec_template(
    metric_impl = mn_log_loss_impl,
    truth = hpc_f1$obs,
    estimate = inject_na(as.matrix(hpc_f1[, 3:6])),
    na_rm = FALSE,
    case_weights = seq_along(hpc_f1$obs),
    sum = FALSE
  )

  expect_identical(res, NA_real_)

  res <- prob_metric_vec_template(
    metric_impl = mn_log_loss_impl,
    truth = hpc_f1$obs,
    estimate = as.matrix(hpc_f1[, 3:6]),
    na_rm = FALSE,
    case_weights = c(seq_along(hpc_f1$obs)[1:200], rep(NA, 147)),
    sum = FALSE
  )

  expect_identical(res, NA_real_)
})

test_that("prob_metric_vec_template() dots are passed through", {
  hpc_f1 <- data_hpc_fold1()

  mn_log_loss_impl <- function(truth,
                               estimate,
                               ...,
                               sum = FALSE,
                               case_weights = NULL) {
    check_dots_empty()

    mn_log_loss_estimator_impl(
      truth = truth,
      estimate = estimate,
      estimator = "multiclass",
      event_level = "first",
      sum = sum,
      case_weights = case_weights
    )
  }

  res <- prob_metric_vec_template(
    metric_impl = mn_log_loss_impl,
    truth = hpc_f1$obs,
    estimate = as.matrix(hpc_f1[, 3:6]),
    na_rm = FALSE,
    case_weights = NULL,
    sum = TRUE
  )

  exp_res <- mn_log_loss_impl(
    truth = hpc_f1$obs,
    estimate = as.matrix(hpc_f1[, 3:6]),
    sum = TRUE,
    case_weights = NULL
  )

  expect_identical(res, exp_res)
})
