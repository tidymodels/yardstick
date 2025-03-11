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

test_that("errors are thrown if truth or estimate selects more than 1 column", {
  expect_snapshot(
    error = TRUE,
    rmse(mtcars, mpg, tidyselect::starts_with("d"))
  )
  expect_snapshot(
    error = TRUE,
    rmse(mtcars, tidyselect::starts_with("d"), mpg)
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

test_that("numeric_metric_summarizer() works with grouped input", {
  rmse_res <- numeric_metric_summarizer(
    name = "rmse",
    fn = rmse_vec,
    data = dplyr::group_by(mtcars, vs),
    truth = mpg,
    estimate = disp,
    na_rm = TRUE,
    case_weights = NULL
  )

  mtcars_split <- vctrs::vec_split(mtcars, mtcars$vs)

  rmse_exp <- dplyr::tibble(
    vs = mtcars_split$key,
    .metric = "rmse",
    .estimator = "standard",
    .estimate = vapply(
      mtcars_split$val,
      function(x) rmse_vec(x$mpg, x$disp),
      FUN.VALUE = numeric(1)
    )
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
    .estimate = rmse_vec(
      mtcars$mpg[mtcars$vs == 1],
      mtcars$disp[mtcars$vs == 1]
    )
  )

  expect_identical(rmse_res, rmse_exp)
})

test_that("numeric_metric_summarizer()'s errors when wrong things are passes", {
  expect_snapshot(
    error = TRUE,
    numeric_metric_summarizer(
      name = "rmse",
      fn = rmse_vec,
      data = mtcars,
      truth = not_a_real_column_name,
      estimate = disp
    )
  )

  expect_snapshot(
    error = TRUE,
    numeric_metric_summarizer(
      name = "rmse",
      fn = rmse_vec,
      data = mtcars,
      truth = mpg,
      estimate = not_a_real_column_name
    )
  )

  expect_snapshot(
    error = TRUE,
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

test_that("numeric_metric_summarizer() handles column name collisions", {
  new_mtcars <- mtcars

  new_mtcars$name <- mtcars$mpg
  new_mtcars$estimator <- mtcars$mpg
  new_mtcars$event_level <- mtcars$mpg
  new_mtcars$na_rm <- mtcars$mpg
  new_mtcars$truth <- mtcars$mpg
  new_mtcars$estimate <- mtcars$mpg

  rmse_res <- numeric_metric_summarizer(
    name = "rmse",
    fn = rmse_vec,
    data = new_mtcars,
    truth = mpg,
    estimate = disp,
    na_rm = TRUE,
    case_weights = NULL
  )

  rmse_exp <- dplyr::tibble(
    .metric = "rmse",
    .estimator = "standard",
    .estimate = rmse_vec(new_mtcars$mpg, new_mtcars$disp)
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

test_that("class_metric_summarizer() works with grouped input", {
  three_class <- data_three_class()$three_class
  three_class$group <- rep(1:2, length.out = nrow(three_class))

  bal_accuracy_res <- class_metric_summarizer(
    name = "bal_accuracy",
    fn = bal_accuracy_vec,
    data = dplyr::group_by(three_class, group),
    truth = obs,
    estimate = pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  three_class_split <- vctrs::vec_split(three_class, three_class$group)

  bal_accuracy_exp <- dplyr::tibble(
    group = three_class_split$key,
    .metric = "bal_accuracy",
    .estimator = "macro",
    .estimate = vapply(
      three_class_split$val,
      function(x) bal_accuracy_vec(x$obs, x$pred),
      FUN.VALUE = numeric(1)
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

  expect_snapshot(
    error = TRUE,
    class_metric_summarizer(
      name = "accuracy",
      fn = accuracy_vec,
      data = three_class,
      truth = not_a_real_column_name,
      estimate = pred
    )
  )

  expect_snapshot(
    error = TRUE,
    class_metric_summarizer(
      name = "accuracy",
      fn = accuracy_vec,
      data = three_class,
      truth = obs,
      estimate = not_a_real_column_name
    )
  )

  expect_snapshot(
    error = TRUE,
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

test_that("class_metric_summarizer() handles column name collisions", {
  three_class <- data_three_class()$three_class

  new_three_class <- three_class
  new_three_class$name <- three_class$obs
  new_three_class$estimator <- three_class$obs
  new_three_class$event_level <- three_class$obs
  new_three_class$na_rm <- three_class$obs
  new_three_class$truth <- three_class$obs
  new_three_class$estimate <- three_class$obs

  accuracy_res <- class_metric_summarizer(
    name = "accuracy",
    fn = accuracy_vec,
    data = new_three_class,
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
    .estimate = roc_auc_vec(
      hpc_f1$obs,
      as.matrix(hpc_f1[3:6]),
      estimator = "macro"
    )
  )

  expect_identical(roc_auc_res, roc_auc_exp)
})

test_that("prob_metric_summarizer() works with grouped input", {
  hpc_f1 <- data_hpc_fold1()
  hpc_f1$group <- rep(1:2, length.out = nrow(hpc_f1))

  roc_auc_res <- prob_metric_summarizer(
    name = "roc_auc",
    fn = roc_auc_vec,
    data = dplyr::group_by(hpc_f1, group),
    truth = obs,
    VF:L,
    na_rm = TRUE,
    case_weights = NULL
  )

  hpc_f1_split <- vctrs::vec_split(hpc_f1, hpc_f1$group)

  roc_auc_exp <- dplyr::tibble(
    group = hpc_f1_split$key,
    .metric = "roc_auc",
    .estimator = "hand_till",
    .estimate = vapply(
      hpc_f1_split$val,
      function(x) roc_auc_vec(x$obs, as.matrix(x[3:6])),
      FUN.VALUE = numeric(1)
    )
  )

  expect_identical(roc_auc_res, roc_auc_exp)
})

test_that("class_metric_summarizer()'s event_level works as expected", {
  hpc_f1 <- data_hpc_fold1()
  hpc_f1$obs <- factor(
    hpc_f1$obs == "VF",
    levels = c(TRUE, FALSE),
    labels = c("VF", "nVF")
  )

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

  expect_snapshot(
    error = TRUE,
    prob_metric_summarizer(
      name = "roc_auc",
      fn = roc_auc_vec,
      data = hpc_f1,
      truth = obs,
      c(HELLO, F, M, L)
    )
  )

  expect_snapshot(
    error = TRUE,
    prob_metric_summarizer(
      name = "roc_auc",
      fn = roc_auc_vec,
      data = hpc_f1,
      truth = obviouslywrong,
      VF:L
    )
  )

  expect_snapshot(
    error = TRUE,
    prob_metric_summarizer(
      name = "roc_auc",
      fn = roc_auc_vec,
      data = hpc_f1,
      truth = obs,
      VF:L,
      obviouslywrong = TRUE
    )
  )

  expect_snapshot(
    error = TRUE,
    prob_metric_summarizer(
      name = "roc_auc",
      fn = roc_auc_vec,
      data = hpc_f1,
      truth = obs,
      estimate = VF:L
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

test_that("prob_metric_summarizer() handles column name collisions", {
  hpc_f1 <- data_hpc_fold1()

  new_hpc_f1 <- hpc_f1
  new_hpc_f1$name <- hpc_f1$VF
  new_hpc_f1$estimator <- hpc_f1$VF
  new_hpc_f1$event_level <- hpc_f1$VF
  new_hpc_f1$na_rm <- hpc_f1$VF
  new_hpc_f1$truth <- hpc_f1$VF
  new_hpc_f1$estimate <- hpc_f1$VF

  roc_auc_res <- prob_metric_summarizer(
    name = "roc_auc",
    fn = roc_auc_vec,
    data = new_hpc_f1,
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

test_that("curve_metric_summarizer() works with grouped input", {
  hpc_f1 <- data_hpc_fold1()
  hpc_f1$group <- rep(1:2, length.out = nrow(hpc_f1))

  roc_curve_res <- curve_metric_summarizer(
    name = "roc_curve",
    fn = roc_curve_vec,
    data = dplyr::group_by(hpc_f1, group),
    truth = obs,
    VF:L,
    na_rm = TRUE,
    case_weights = NULL
  )

  hpc_f1_split <- vctrs::vec_split(hpc_f1, hpc_f1$group)

  estimate_values <- lapply(
    hpc_f1_split$val,
    function(x) roc_curve_vec(x$obs, as.matrix(x[3:6]))
  )

  roc_curve_exp <- dplyr::tibble(
    group = rep(hpc_f1_split$key, lapply(estimate_values, nrow)),
    .metric = "roc_curve",
    .estimator = "multiclass",
    .estimate = dplyr::bind_rows(estimate_values)
  )

  expect_identical(roc_curve_res, roc_curve_exp)
})

test_that("class_metric_summarizer()'s event_level works as expected", {
  hpc_f1 <- data_hpc_fold1()
  hpc_f1$obs <- factor(
    hpc_f1$obs == "VF",
    levels = c(TRUE, FALSE),
    labels = c("VF", "nVF")
  )

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
    .estimate = roc_curve_vec(
      hpc_f1$obs[-(1:5)],
      as.matrix(hpc_f1[-(1:5), 3:6])
    )
  )

  expect_identical(roc_curve_res, roc_curve_exp)

  expect_snapshot(
    error = TRUE,
    curve_metric_summarizer(
      name = "roc_curve",
      fn = roc_curve_vec,
      data = hpc_f1_na,
      truth = obs,
      VF:L,
      na_rm = FALSE,
      case_weights = NULL
    )
  )
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

  expect_snapshot(
    error = TRUE,
    curve_metric_summarizer(
      name = "roc_curve",
      fn = roc_curve_vec,
      data = hpc_f1,
      truth = obs,
      c(HELLO, F, M, L)
    )
  )

  expect_snapshot(
    error = TRUE,
    curve_metric_summarizer(
      name = "roc_curve",
      fn = roc_curve_vec,
      data = hpc_f1,
      truth = obviouslywrong,
      VF:L
    )
  )

  expect_snapshot(
    error = TRUE,
    curve_metric_summarizer(
      name = "roc_curve",
      fn = roc_curve_vec,
      data = hpc_f1,
      truth = obs,
      VF:L,
      obviouslywrong = TRUE
    )
  )

  expect_snapshot(
    error = TRUE,
    curve_metric_summarizer(
      name = "roc_curve",
      fn = roc_curve_vec,
      data = hpc_f1,
      truth = obs,
      estimate = VF:L
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

test_that("curve_metric_summarizer() handles column name collisions", {
  hpc_f1 <- data_hpc_fold1()

  new_hpc_f1 <- hpc_f1
  new_hpc_f1$name <- hpc_f1$VF
  new_hpc_f1$estimator <- hpc_f1$VF
  new_hpc_f1$event_level <- hpc_f1$VF
  new_hpc_f1$na_rm <- hpc_f1$VF
  new_hpc_f1$truth <- hpc_f1$VF
  new_hpc_f1$estimate <- hpc_f1$VF

  roc_curve_res <- curve_metric_summarizer(
    name = "roc_curve",
    fn = roc_curve_vec,
    data = new_hpc_f1,
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

test_that("dynamic_survival_metric_summarizer() works as expected", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  brier_survival_exp <- dplyr::bind_cols(
    dplyr::tibble(
      .metric = "brier_survival",
      .estimator = "standard"
    ),
    brier_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})

test_that("dynamic_survival_metric_summarizer() works with grouped input", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$group <- rep(1:2, length.out = nrow(lung_surv))

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = dplyr::group_by(lung_surv, group),
    truth = surv_obj,
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  lung_surv_split <- vctrs::vec_split(lung_surv, lung_surv$group)

  estimate_values <- lapply(
    lung_surv_split$val,
    function(x) brier_survival_vec(x$surv_obj, x$.pred)
  )
  estimate_values <- vctrs::vec_rbind(!!!estimate_values)

  n_eval_time <- length(lung_surv$.pred[[1]]$.eval_time)

  brier_survival_exp <- dplyr::bind_cols(
    dplyr::tibble(
      group = rep(lung_surv_split$key, each = n_eval_time),
      .metric = "brier_survival",
      .estimator = "standard"
    ),
    estimate_values
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})

test_that("dynamic_survival_metric_summarizer()'s na_rm argument works", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv[1:5, 3] <- NA

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  surv_subset <- function(x, i) {
    res <- x[i, ]
    class(res) <- class(x)
    attr(res, "type") <- attr(x, "type")
    res
  }

  brier_survival_exp <- dplyr::bind_cols(
    dplyr::tibble(
      .metric = "brier_survival",
      .estimator = "standard"
    ),
    brier_survival_vec(
      truth = surv_subset(lung_surv$surv_obj, -c(1:5)),
      estimate = lung_surv$.pred[-c(1:5)]
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    .pred,
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

test_that("dynamic_survival_metric_summarizer()'s case_weights argument works", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$wts <- seq_len(nrow(lung_surv))

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    .pred,
    na_rm = TRUE,
    case_weights = wts
  )

  brier_survival_exp <- dplyr::bind_cols(
    dplyr::tibble(
      .metric = "brier_survival",
      .estimator = "standard"
    ),
    brier_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred,
      case_weights = lung_surv$wts
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})

test_that("dynamic_survival_metric_summarizer()'s errors with bad input", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  expect_snapshot(
    error = TRUE,
    dynamic_survival_metric_summarizer(
      name = "brier_survival",
      fn = brier_survival_vec,
      data = lung_surv,
      truth = .pred_time,
      .pred
    )
  )

  expect_snapshot(
    error = TRUE,
    dynamic_survival_metric_summarizer(
      name = "brier_survival",
      fn = brier_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      surv_obj
    )
  )

  expect_snapshot(
    error = TRUE,
    dynamic_survival_metric_summarizer(
      name = "brier_survival",
      fn = brier_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      estimate = .pred
    )
  )
})

test_that("dynamic_survival_metric_summarizer() deals with characters in truth and estimate", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = lung_surv,
    truth = "surv_obj",
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  brier_survival_exp <- dplyr::bind_cols(
    dplyr::tibble(
      .metric = "brier_survival",
      .estimator = "standard"
    ),
    brier_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred
    )
  )

  expect_identical(brier_survival_res, brier_survival_exp)
})

test_that("dynamic_survival_metric_summarizer() handles column name collisions", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  new_lung_surv <- lung_surv
  new_lung_surv$name <- lung_surv$.pred_time
  new_lung_surv$estimator <- lung_surv$.pred_time
  new_lung_surv$event_level <- lung_surv$.pred_time
  new_lung_surv$na_rm <- lung_surv$.pred_time
  new_lung_surv$truth <- lung_surv$.pred_time
  new_lung_surv$estimate <- lung_surv$.pred_time

  brier_survival_res <- dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = new_lung_surv,
    truth = "surv_obj",
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  brier_survival_exp <- dplyr::bind_cols(
    dplyr::tibble(
      .metric = "brier_survival",
      .estimator = "standard"
    ),
    brier_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred
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
    estimate = .pred_time,
    na_rm = TRUE,
    case_weights = NULL
  )

  concordance_survival_exp <- dplyr::tibble(
    .metric = "concordance_survival",
    .estimator = "standard",
    .estimate = concordance_survival_vec(
      lung_surv$surv_obj,
      lung_surv$.pred_time
    )
  )

  expect_identical(concordance_survival_res, concordance_survival_exp)
})

test_that("static_survival_metric_summarizer() works with grouped input", {
  lung_surv <- data_lung_surv()
  lung_surv$group <- rep(1:2, length.out = nrow(lung_surv))

  concordance_survival_res <- static_survival_metric_summarizer(
    name = "concordance_survival",
    fn = concordance_survival_vec,
    data = dplyr::group_by(lung_surv, group),
    truth = surv_obj,
    estimate = .pred_time,
    na_rm = TRUE,
    case_weights = NULL
  )

  lung_surv_split <- vctrs::vec_split(lung_surv, lung_surv$group)

  concordance_survival_exp <- dplyr::tibble(
    group = lung_surv_split$key,
    .metric = "concordance_survival",
    .estimator = "standard",
    .estimate = vapply(
      lung_surv_split$val,
      function(x) concordance_survival_vec(x$surv_obj, x$.pred_time),
      FUN.VALUE = numeric(1)
    )
  )

  expect_identical(concordance_survival_res, concordance_survival_exp)
})

test_that("static_survival_metric_summarizer()'s na_rm argument works", {
  lung_surv <- data_lung_surv()
  lung_surv[1:5, 3] <- NA

  concordance_survival_res <- static_survival_metric_summarizer(
    name = "concordance_survival",
    fn = concordance_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_time,
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
    .estimator = "standard",
    .estimate = concordance_survival_vec(
      truth = surv_subset(lung_surv$surv_obj, -c(1:5)),
      estimate = lung_surv$.pred_time[-c(1:5)]
    )
  )

  expect_identical(concordance_survival_res, concordance_survival_exp)

  concordance_survival_res <- static_survival_metric_summarizer(
    name = "concordance_survival",
    fn = concordance_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_time,
    na_rm = FALSE,
    case_weights = NULL
  )

  concordance_survival_exp <- dplyr::tibble(
    .metric = "concordance_survival",
    .estimator = "standard",
    .estimate = NA_real_
  )

  expect_identical(concordance_survival_res, concordance_survival_exp)
})

test_that("static_survival_metric_summarizer()'s case_weights argument works", {
  lung_surv <- data_lung_surv()
  lung_surv$wts <- seq_len(nrow(lung_surv))

  concordance_survival_res <- static_survival_metric_summarizer(
    name = "concordance_survival",
    fn = concordance_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    estimate = .pred_time,
    na_rm = TRUE,
    case_weights = wts
  )

  concordance_survival_exp <- dplyr::tibble(
    .metric = "concordance_survival",
    .estimator = "standard",
    .estimate = concordance_survival_vec(
      lung_surv$surv_obj,
      lung_surv$.pred_time,
      case_weights = lung_surv$wts
    )
  )

  expect_identical(concordance_survival_res, concordance_survival_exp)
})

test_that("static_survival_metric_summarizer()'s errors with bad input", {
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
    estimate = .pred_time,
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
      estimate = .pred_time
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
      estimate = .pred_time,
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
    estimate = ".pred_time",
    na_rm = TRUE,
    case_weights = NULL
  )

  concordance_survival_exp <- dplyr::tibble(
    .metric = "concordance_survival",
    .estimator = "standard",
    .estimate = concordance_survival_vec(
      lung_surv$surv_obj,
      lung_surv$.pred_time
    )
  )

  expect_identical(concordance_survival_res, concordance_survival_exp)
})

## curve_survival_metric_summarizer -----------------------------------------

test_that("curve_survival_metric_summarizer() works as expected", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  roc_curve_survival_res <- curve_survival_metric_summarizer(
    name = "roc_curve_survival",
    fn = roc_curve_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  roc_curve_survival_exp <- dplyr::tibble(
    .metric = "roc_curve_survival",
    .estimator = "standard",
    .estimate = roc_curve_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred
    )
  )

  expect_identical(roc_curve_survival_res, roc_curve_survival_exp)
})

test_that("curve_survival_metric_summarizer() works with grouped input", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$group <- rep(1:2, length.out = nrow(lung_surv))

  roc_curve_survival_res <- curve_survival_metric_summarizer(
    name = "roc_curve_survival",
    fn = roc_curve_survival_vec,
    data = dplyr::group_by(lung_surv, group),
    truth = surv_obj,
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  lung_surv_split <- vctrs::vec_split(lung_surv, lung_surv$group)

  estimate_values <- lapply(
    lung_surv_split$val,
    function(x) roc_curve_survival_vec(x$surv_obj, x$.pred)
  )

  roc_curve_survival_exp <- dplyr::tibble(
    group = rep(lung_surv_split$key, lapply(estimate_values, nrow)),
    .metric = "roc_curve_survival",
    .estimator = "standard",
    .estimate = dplyr::bind_rows(estimate_values)
  )

  expect_identical(roc_curve_survival_res, roc_curve_survival_exp)
})

test_that("curve_survival_metric_summarizer()'s na_rm argument works", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv[1:5, 3] <- NA

  roc_curve_survival_res <- curve_survival_metric_summarizer(
    name = "roc_curve_survival",
    fn = roc_curve_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  surv_subset <- function(x, i) {
    res <- x[i, ]
    class(res) <- class(x)
    attr(res, "type") <- attr(x, "type")
    res
  }

  roc_curve_survival_exp <- dplyr::tibble(
    .metric = "roc_curve_survival",
    .estimator = "standard",
    .estimate = roc_curve_survival_vec(
      truth = surv_subset(lung_surv$surv_obj, -c(1:5)),
      estimate = lung_surv$.pred[-c(1:5)]
    )
  )

  expect_identical(roc_curve_survival_res, roc_curve_survival_exp)

  expect_snapshot(
    error = TRUE,
    curve_survival_metric_summarizer(
      name = "roc_curve_survival",
      fn = roc_curve_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      .pred,
      na_rm = FALSE,
      case_weights = NULL
    )
  )
})

test_that("curve_survival_metric_summarizer()'s case_weights argument works", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()
  lung_surv$wts <- seq_len(nrow(lung_surv))

  roc_curve_survival_res <- curve_survival_metric_summarizer(
    name = "roc_curve_survival",
    fn = roc_curve_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    .pred,
    na_rm = TRUE,
    case_weights = wts
  )

  roc_curve_survival_exp <- dplyr::tibble(
    .metric = "roc_curve_survival",
    .estimator = "standard",
    .estimate = roc_curve_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred,
      case_weights = lung_surv$wts
    )
  )

  expect_identical(roc_curve_survival_res, roc_curve_survival_exp)
})

test_that("curve_survival_metric_summarizer()'s errors with bad input", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  roc_curve_survival_res <- curve_survival_metric_summarizer(
    name = "roc_curve_survival",
    fn = roc_curve_survival_vec,
    data = lung_surv,
    truth = surv_obj,
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  expect_snapshot(
    error = TRUE,
    curve_survival_metric_summarizer(
      name = "roc_curve_survival",
      fn = roc_curve_survival_vec,
      data = lung_surv,
      truth = .pred_time,
      .pred
    )
  )

  expect_snapshot(
    error = TRUE,
    curve_survival_metric_summarizer(
      name = "roc_curve_survival",
      fn = roc_curve_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      surv_obj
    )
  )

  expect_snapshot(
    error = TRUE,
    curve_survival_metric_summarizer(
      name = "roc_curve_survival",
      fn = roc_curve_survival_vec,
      data = lung_surv,
      truth = surv_obj,
      estimate = .pred
    )
  )
})

test_that("curve_survival_metric_summarizer() deals with characters n truth and estimate", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  roc_curve_survival_res <- curve_survival_metric_summarizer(
    name = "roc_curve_survival",
    fn = roc_curve_survival_vec,
    data = lung_surv,
    truth = "surv_obj",
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  roc_curve_survival_exp <- dplyr::tibble(
    .metric = "roc_curve_survival",
    .estimator = "standard",
    .estimate = roc_curve_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred
    )
  )

  expect_identical(roc_curve_survival_res, roc_curve_survival_exp)
})

test_that("curve_survival_metric_summarizer() handles column name collisions", {
  skip_if_not_installed("tidyr")

  lung_surv <- data_lung_surv()

  new_lung_surv <- lung_surv
  new_lung_surv$name <- lung_surv$.pred_time
  new_lung_surv$estimator <- lung_surv$.pred_time
  new_lung_surv$event_level <- lung_surv$.pred_time
  new_lung_surv$na_rm <- lung_surv$.pred_time
  new_lung_surv$truth <- lung_surv$.pred_time
  new_lung_surv$estimate <- lung_surv$.pred_time

  roc_survival_curve_res <- curve_survival_metric_summarizer(
    name = "roc_survival_curve",
    fn = roc_curve_survival_vec,
    data = new_lung_surv,
    truth = "surv_obj",
    .pred,
    na_rm = TRUE,
    case_weights = NULL
  )

  roc_survival_curve_exp <- dplyr::tibble(
    .metric = "roc_survival_curve",
    .estimator = "standard",
    .estimate = roc_curve_survival_vec(
      truth = lung_surv$surv_obj,
      estimate = lung_surv$.pred
    )
  )

  expect_identical(roc_survival_curve_res, roc_survival_curve_exp)
})

test_that("known selections don't affect selection without tune", {
  # yardstick's CI reliably does not have tune installed
  skip_if(rlang::is_installed("tune"), "tune is installed")

  expect_silent({
    test_res <- rmse(solubility_test, solubility, prediction)
  })
})
