#' F Measure
#'
#' These functions calculate the [f_meas()] of a measurement system for
#' finding relevant documents compared to reference results
#' (the truth regarding relevance). Highly related functions are [recall()]
#' and [precision()].
#'
#' The measure "F" is a combination of precision and recall (see below).
#'
#' @family class metrics
#' @family relevance metrics
#' @templateVar fn f_meas
#' @template event_first
#' @template multiclass
#' @template return
#' @template table-relevance
#'
#' @inheritParams sens
#'
#' @param beta A numeric value used to weight precision and
#'  recall. A value of 1 is traditionally used and corresponds to
#'  the harmonic mean of the two values but other values weight
#'  recall beta times more important than precision.
#'
#'
#' @references
#'
#' Buckland, M., & Gey, F. (1994). The relationship
#'  between Recall and Precision. *Journal of the American Society
#'  for Information Science*, 45(1), 12-19.
#'
#' Powers, D. (2007). Evaluation: From Precision, Recall and F
#'  Factor to ROC, Informedness, Markedness and Correlation.
#'  Technical Report SIE-07-001, Flinders University
#'
#' @author Max Kuhn
#'
#' @template examples-class
#'
#' @export
f_meas <- function(data, ...) {
  UseMethod("f_meas")
}
f_meas <- new_class_metric(
  f_meas,
  direction = "maximize"
)

#' @rdname f_meas
#' @export
f_meas.data.frame <- function(
  data,
  truth,
  estimate,
  beta = 1,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  class_metric_summarizer(
    name = "f_meas",
    fn = f_meas_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    event_level = event_level,
    fn_options = list(beta = beta)
  )
}

#' @export
f_meas.table <- function(
  data,
  beta = 1,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "f_meas",
    .estimator = estimator,
    .estimate = f_meas_table_impl(data, estimator, event_level, beta = beta)
  )
}

#' @export
f_meas.matrix <- function(
  data,
  beta = 1,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  f_meas.table(data, beta, estimator, event_level)
}

#' @export
#' @rdname f_meas
f_meas_vec <- function(
  truth,
  estimate,
  beta = 1,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  abort_if_class_pred(truth)
  estimate <- as_factor_from_class_pred(estimate)

  estimator <- finalize_estimator(truth, estimator)

  check_class_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  data <- yardstick_table(truth, estimate, case_weights = case_weights)
  f_meas_table_impl(data, estimator, event_level, beta)
}

f_meas_table_impl <- function(data, estimator, event_level, beta) {
  if (is_binary(estimator)) {
    f_meas_binary(data, event_level, beta)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- f_meas_multiclass(data, estimator, beta)
    stats::weighted.mean(out_vec, w, na.rm = TRUE)
  }
}

f_meas_binary <- function(data, event_level, beta = 1) {
  precision <- precision_binary(data, event_level)
  rec <- recall_binary(data, event_level)

  # if precision and recall are both 0, return 0 not NA
  if (isTRUE(precision == 0 & rec == 0)) {
    return(0)
  }

  (1 + beta^2) * precision * rec / ((beta^2 * precision) + rec)
}

f_meas_multiclass <- function(data, estimator, beta = 1) {
  precision <- precision_multiclass(data, estimator)
  rec <- recall_multiclass(data, estimator)

  res <- (1 + beta^2) * precision * rec / ((beta^2 * precision) + rec)

  # if precision and recall are both 0, define this as 0 not NA
  # this is the case when tp == 0 and is well defined
  # Matches sklearn behavior
  # https://github.com/scikit-learn/scikit-learn/blob/bac89c253b35a8f1a3827389fbee0f5bebcbc985/sklearn/metrics/classification.py#L1150
  where_zero <- which(precision == 0 & rec == 0)

  res[where_zero] <- 0

  res
}
