#' Balanced accuracy
#'
#' Balanced accuracy is computed here as the average of [sens()] and [spec()].
#'
#' @family class metrics
#' @templateVar fn bal_accuracy
#' @template event_first
#' @template multiclass
#' @template return
#'
#' @inheritParams sens
#'
#' @author Max Kuhn
#'
#' @template examples-class
#'
#' @export
bal_accuracy <- function(data, ...) {
  UseMethod("bal_accuracy")
}
bal_accuracy <- new_class_metric(
  bal_accuracy,
  direction = "maximize"
)

#' @export
#' @rdname bal_accuracy
bal_accuracy.data.frame <- function(
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  class_metric_summarizer(
    name = "bal_accuracy",
    fn = bal_accuracy_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    event_level = event_level
  )
}

#' @export
bal_accuracy.table <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "bal_accuracy",
    .estimator = estimator,
    .estimate = bal_accuracy_table_impl(data, estimator, event_level)
  )
}

#' @export
bal_accuracy.matrix <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  bal_accuracy.table(data, estimator, event_level)
}

#' @export
#' @rdname bal_accuracy
bal_accuracy_vec <- function(
  truth,
  estimate,
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
  bal_accuracy_table_impl(data, estimator, event_level)
}

bal_accuracy_table_impl <- function(data, estimator, event_level) {
  if (is_binary(estimator)) {
    bal_accuracy_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- bal_accuracy_multiclass(data, estimator)
    stats::weighted.mean(out_vec, w)
  }
}

bal_accuracy_binary <- function(data, event_level) {
  (sens_binary(data, event_level) + spec_binary(data, event_level)) / 2
}

# Urbanowicz 2015 ExSTraCS 2.0 description and evaluation of a scalable learning.pdf
bal_accuracy_multiclass <- function(data, estimator) {
  (recall_multiclass(data, estimator) + spec_multiclass(data, estimator)) / 2
}
