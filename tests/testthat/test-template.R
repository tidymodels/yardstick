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

## dynamic_survival_metric_summarizer --------------------------------------------------

test_that("dynamic_survival_metric_summarizer() works as expected", {
  lung_surv <- data_lung_surv()
  .time <- c(100, 500, 1000)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred,
    censoring_weights = prob_censored,
    .time = .time,
    na_rm = TRUE,
    case_weights = NULL
  )

  brier_survival_exp <- dplyr::tibble(
    .metric = "brier_survival",
    .estimator = "binary",
    .estimate = list(
      dplyr::tibble(
        .time = .time,
        .estimate = brier_survival_vec(
          lung_surv$surv_obj, lung_surv$.pred, lung_surv$prob_censored, .time = .time
        )
      )
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})

test_that("dynamic_survival_metric_summarizer()'s na_rm argument work", {
  lung_surv <- data_lung_surv()
  lung_surv[1:5, 1] <- NA

  .time <- c(100, 500, 1000)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    censoring_weights = prob_censored,
    estimate = .pred,
    .time = .time,
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
    .estimator = "binary",
    .estimate = list(
      dplyr::tibble(
        .time = .time,
        .estimate = brier_survival_vec(
          truth = surv_subset(lung_surv$surv_obj, -c(1:5)),
          estimate = lung_surv$.pred[-c(1:5)],
          censoring_weights = lung_surv$prob_censored[-c(1:5)],
          .time = .time
        )
      )
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    censoring_weights = prob_censored,
    estimate = .pred,
    .time = .time,
    na_rm = FALSE,
    case_weights = NULL
  )

  brier_survival_exp <- dplyr::tibble(
    .metric = "brier_survival",
    .estimator = "binary",
    .estimate = list(
      dplyr::tibble(
        .time = .time,
        .estimate = na_dbl
      )
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})

test_that("dynamic_survival_metric_summarizer()'s case_weights argument work", {
  lung_surv <- data_lung_surv()
  .time <- c(100, 500, 1000)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    censoring_weights = prob_censored,
    estimate = .pred,
    .time = .time,
    na_rm = TRUE,
    case_weights = ph.ecog
  )

  brier_survival_exp <- dplyr::tibble(
    .metric = "brier_survival",
    .estimator = "binary",
    .estimate = list(
      dplyr::tibble(
        .time = .time,
        .estimate = brier_survival_vec(
          lung_surv$surv_obj, lung_surv$.pred, .time = .time,
          censoring_weights = lung_surv$prob_censored,
          case_weights = lung_surv$ph.ecog
        )
      )
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})

test_that("dynamic_survival_metric_summarizer()'s errors when wrong things are passes", {
  lung_surv <- data_lung_surv()
  lung_surv$list <- lapply(seq_len(nrow(lung_surv)), identity)
  lung_surv$list2 <- lapply(
    seq_len(nrow(lung_surv)),
    function(x) data.frame(wrong = 1, names = 2)
  )
  .time <- c(100, 500, 1000)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    censoring_weights = prob_censored,
    estimate = .pred,
    .time = .time,
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
      estimate = .pred,
      censoring_weights = prob_censored,
      .time = .time
    )
  )

  expect_snapshot(
    error = TRUE,
    dynamic_survival_metric_summarizer(
      name = "brier_survival",
      fn = brier_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      estimate = age,
      censoring_weights = prob_censored,
      .time = .time
    )
  )

  expect_snapshot(
    error = TRUE,
    dynamic_survival_metric_summarizer(
      name = "brier_survival",
      fn = brier_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      estimate = list,
      censoring_weights = prob_censored,
      .time = .time
    )
  )

  expect_snapshot(
    error = TRUE,
    dynamic_survival_metric_summarizer(
      name = "brier_survival",
      fn = brier_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      estimate = list2,
      censoring_weights = prob_censored,
      .time = .time
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
      .time = .time,
      obviouslywrong = TRUE
    )
  )
})

test_that("dynamic_survival_metric_summarizer() deals with characters in truth and estimate", {
  lung_surv <- data_lung_surv()
  .time <- c(100, 500, 1000)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = "surv_obj",
    estimate = ".pred",
    censoring_weights = "prob_censored",
    .time = .time,
    na_rm = TRUE,
    case_weights = NULL
  )

  brier_survival_exp <- dplyr::tibble(
    .metric = "brier_survival",
    .estimator = "binary",
    .estimate = list(
      dplyr::tibble(
        .time = .time,
        .estimate = brier_survival_vec(
          lung_surv$surv_obj, lung_surv$.pred, lung_surv$prob_censored,
          .time = .time
        )
      )
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})

## static_survival_metric_summarizer --------------------------------------------------

test_that("static_survival_metric_summarizer() works as expected", {
  lung_surv <- data_lung_surv()

  concordance_survival_res <- static_survival_metric_summarizer(
    name = "concordance_survival",
    fn = concordance_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    estimate = age,
    na_rm = TRUE,
    case_weights = NULL
  )

  concordance_survival_exp <- dplyr::tibble(
    .metric = "concordance_survival",
    .estimator = "binary",
    .estimate = concordance_survival_vec(
      lung_surv$surv_obj, lung_surv$age
    )
  )

  expect_identical(concordance_survival_res, concordance_survival_exp)
})

test_that("static_survival_metric_summarizer()'s na_rm argument work", {
  lung_surv <- data_lung_surv()
  lung_surv[1:5, 1] <- NA

  concordance_survival_res <- static_survival_metric_summarizer(
    name = "concordance_survival",
    fn = concordance_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    estimate = age,
    na_rm = TRUE,
    case_weights = NULL
  )

  surv_subset <- function(x, i) {
    res <- x[i, ]
    class(res) <- class(x)
    attr(res, "type") <- attr(x, "type")
    res
  }

  concordance_survival_exp <- dplyr::tibble(
    .metric = "concordance_survival",
    .estimator = "binary",
    .estimate = concordance_survival_vec(
       truth = surv_subset(lung_surv$surv_obj, -c(1:5)),
       estimate = lung_surv$age[-c(1:5)]
    )
  )

  expect_identical(concordance_survival_res, concordance_survival_exp)

  concordance_survival_res <- static_survival_metric_summarizer(
    name = "concordance_survival",
    fn = concordance_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    estimate = age,
    na_rm = FALSE,
    case_weights = NULL
  )

  concordance_survival_exp <- dplyr::tibble(
    .metric = "concordance_survival",
    .estimator = "binary",
    .estimate = NA_real_
  )

  expect_identical(concordance_survival_res, concordance_survival_exp)
})

test_that("static_survival_metric_summarizer()'s case_weights argument work", {
  lung_surv <- data_lung_surv()

  concordance_survival_res <- static_survival_metric_summarizer(
    name = "concordance_survival",
    fn = concordance_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    estimate = age,
    na_rm = TRUE,
    case_weights = ph.ecog
  )

  concordance_survival_exp <- dplyr::tibble(
    .metric = "concordance_survival",
    .estimator = "binary",
    .estimate = concordance_survival_vec(
       lung_surv$surv_obj, lung_surv$age,
       case_weights = lung_surv$ph.ecog
    )
  )

  expect_identical(concordance_survival_res, concordance_survival_exp)
})

test_that("static_survival_metric_summarizer()'s errors when wrong things are passes", {
  lung_surv <- data_lung_surv()
  lung_surv$list <- lapply(seq_len(nrow(lung_surv)), identity)
  lung_surv$list2 <- lapply(
    seq_len(nrow(lung_surv)),
    function(x) data.frame(wrong = 1, names = 2)
  )

  concordance_survival_res <- static_survival_metric_summarizer(
    name = "concordance_survival",
    fn = concordance_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    estimate = age,
    na_rm = TRUE,
    case_weights = NULL
  )

  expect_snapshot(
    error = TRUE,
    static_survival_metric_summarizer(
      name = "concordance_survival",
      fn = concordance_survival_vec,
      data = lung_surv,
      truth = inst,
      estimate = age
    )
  )

  expect_snapshot(
    error = TRUE,
    static_survival_metric_summarizer(
      name = "concordance_survival",
      fn = concordance_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      estimate = surv_obj
    )
  )

  expect_snapshot(
    error = TRUE,
    static_survival_metric_summarizer(
      name = "concordance_survival",
      fn = concordance_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      estimate = list
    )
  )

  expect_snapshot(
    error = TRUE,
    static_survival_metric_summarizer(
      name = "concordance_survival",
      fn = concordance_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      estimate = age,
      obviouslywrong = TRUE
    )
  )
})

test_that("static_survival_metric_summarizer() deals with characters in truth and estimate", {
  lung_surv <- data_lung_surv()

  concordance_survival_res <- static_survival_metric_summarizer(
    name = "concordance_survival",
    fn = concordance_survival_vec,
    data = lung_surv,
    truth = "surv_obj",
    estimate = "age",
    na_rm = TRUE,
    case_weights = NULL
  )

  concordance_survival_exp <- dplyr::tibble(
    .metric = "concordance_survival",
    .estimator = "binary",
    .estimate = concordance_survival_vec(
       lung_surv$surv_obj, lung_surv$age
    )
  )

  expect_identical(concordance_survival_res, concordance_survival_exp)
})
