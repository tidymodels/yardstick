#' Brier score for right censored data
#'
#' Compute the time-dependent Brier score for right censored data. Which is the
#' mean squared error at time point `.time`.
#'
#' @family dynamic survival metrics
#' @templateVar fn brier_survival
#' @template return-dynamic-survival
#' @details
#'
#' Smaller values of the score are associated with better model performance.
#'
#' @inheritParams pr_auc
#'
#' @param data A `data.frame` containing the columns specified by `truth` and
#' `estimate`.
#'
#' @param truth The column identifier for the true class survival result (that
#' is created using [survival::Surv()].). This should be an unquoted column name
#' although this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, an [survival::Surv()] object.
#'
#' @param estimate The column identifier for the survival probabilities. This
#' should be a numeric vector. This should be an unquoted column name although
#' this argument is passed by expression and supports
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, a numeric vector.
#'
#' @param censoring_weights The column identifier for censoring weights. This is
#' expected to a numeric vector. This should be an unquoted column name although
#' this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, a numeric vector.
#'
#' @param .time The column identifier for the time point. This
#' should be a numeric vector, with 1 unique value for each group. This should
#' be an unquoted column name although this argument is passed by expression and
#' supports [quasiquotation][rlang::quasiquotation] (you can unquote column
#' names). For `_vec()` functions, a numeric vector.
#'
#' @param ... Not currently used.
#'
#' @author Emil Hvitfeldt
#'
#' @references
#'   E. Graf, C. Schmoor, W. Sauerbrei, and M. Schumacher, “Assessment and
#'   comparison of prognostic classification schemes for survival data,”
#'   Statistics in Medicine, vol. 18, no. 17-18, pp. 2529–2545, 1999.
#'
#' @examples
#' library(dplyr)
#'
#' lung_surv %>%
#'   group_by(.time) %>%
#'   brier_survival(
#'     truth = surv_obj,
#'     estimate = .pred_survival,
#'     censoring_weights = prob_censored,
#'     .time = .time
#'   )
#' @export
brier_survival <- function(data, ...) {
  UseMethod("brier_survival")
}

brier_survival <- new_dynamic_survival_metric(
  brier_survival,
  direction = "minimize"
)

#' @rdname brier_survival
#' @export
brier_survival.data.frame <- function(data,
                                      truth,
                                      estimate,
                                      censoring_weights,
                                      .time,
                                      na_rm = TRUE,
                                      case_weights = NULL,
                                      ...) {
  dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    censoring_weights = !!enquo(censoring_weights),
    .time = !!enquo(.time),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname brier_survival
brier_survival_vec <- function(truth,
                               estimate,
                               censoring_weights,
                               .time,
                               na_rm = TRUE,
                               case_weights = NULL,
                               ...) {
  check_dynamic_survival_metric(
    truth, estimate, censoring_weights, case_weights, .time
  )

  if (na_rm) {
    estimate_index <- seq_along(estimate)
    result <- yardstick_remove_missing(truth, estimate_index, case_weights)

    truth <- result$truth
    estimate <- estimate[result$estimate]
    censoring_weights <- censoring_weights[result$estimate]
    .time <- .time[result$estimate]
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, seq_along(estimate), case_weights)) {
    return(NA_real_)
  }

  brier_survival_impl(truth, estimate, censoring_weights, case_weights, .time)
}

brier_survival_impl <- function(truth,
                                estimate,
                                censoring_weights,
                                case_weights,
                                .time) {
  surv_time <- truth[, "time"]
  surv_status <- truth[, "status"]

  if (!is.null(case_weights)) {
    norm_const <- sum(case_weights)
    censoring_weights <- censoring_weights / case_weights
  } else {
    case_weights <- rep(1, length(estimate))
    norm_const <- sum(!is.na(truth))
  }

  category_1 <- surv_time < .time & surv_status == 1
  category_2 <- surv_time >= .time

  # (0 - estimate) ^ 2 == estimate ^ 2
  res <- (category_1 * estimate ^ 2 * censoring_weights) +
    (category_2 * (1 - estimate) ^ 2 * censoring_weights)

  res <- res * case_weights
  res <- sum(res, na.rm = TRUE)
  res / norm_const
}

calc_rcbs <- function(surv, pred_val, censoring_weights, case_weights, .time) {
  surv_time <- surv[, "time"]
  surv_status <- surv[, "status"]

  if (!is.null(case_weights)) {
    norm_const <- sum(case_weights)
    censoring_weights <- censoring_weights / case_weights
  } else {
    case_weights <- rep(1, length(pred_val))
    norm_const <- sum(!is.na(surv))
  }

  category_1 <- surv_time < .time & surv_status == 1
  category_2 <- surv_time >= .time

  # (0 - pred_val) ^ 2 == pred_val ^ 2
  res <- (category_1 * pred_val ^ 2 * censoring_weights) +
    (category_2 * (1 - pred_val) ^ 2 * censoring_weights)

  res <- res * case_weights
  res <- sum(res, na.rm = TRUE)
  res / norm_const
}
