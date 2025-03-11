#' J-index
#'
#' @description
#' Youden's J statistic is defined as:
#'
#' [sens()] + [spec()] - 1
#'
#' A related metric is Informedness, see the Details section for the relationship.
#'
#' @details
#'
#' The value of the J-index ranges from \[0, 1\] and is `1` when there are
#' no false positives and no false negatives.
#'
#' The binary version of J-index is equivalent to the binary concept of
#' Informedness. Macro-weighted J-index is equivalent to multiclass informedness
#' as defined in Powers, David M W (2011), equation (42).
#'
#' @family class metrics
#' @templateVar fn j_index
#' @template event_first
#' @template multiclass
#' @template return
#'
#' @inheritParams sens
#'
#' @author Max Kuhn
#'
#' @references
#'
#' Youden, W.J. (1950). "Index for rating diagnostic tests". Cancer. 3: 32-35.
#'
#' Powers, David M W (2011). "Evaluation: From Precision, Recall and F-Score to
#' ROC, Informedness, Markedness and Correlation". Journal of Machine Learning
#' Technologies. 2 (1): 37-63.
#'
#' @template examples-class
#'
#' @export
j_index <- function(data, ...) {
  UseMethod("j_index")
}
j_index <- new_class_metric(
  j_index,
  direction = "maximize"
)

#' @rdname j_index
#' @export
j_index.data.frame <- function(
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
    name = "j_index",
    fn = j_index_vec,
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
j_index.table <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "j_index",
    .estimator = estimator,
    .estimate = j_index_table_impl(data, estimator, event_level)
  )
}

#' @export
j_index.matrix <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  j_index.table(data, estimator, event_level)
}

#' @rdname j_index
#' @export
j_index_vec <- function(
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
  j_index_table_impl(data, estimator, event_level)
}

j_index_table_impl <- function(data, estimator, event_level) {
  if (is_binary(estimator)) {
    j_index_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- j_index_multiclass(data, estimator)
    # Set `na.rm = TRUE` to remove undefined values from weighted computation (#265)
    stats::weighted.mean(out_vec, w, na.rm = TRUE)
  }
}

j_index_binary <- function(data, event_level) {
  sens_binary(data, event_level) + spec_binary(data, event_level) - 1
}

j_index_multiclass <- function(data, estimator) {
  sens_multiclass(data, estimator) + spec_multiclass(data, estimator) - 1
}
