#' Detection rate
#'
#' Detection rate is the proportion of all observations that are true positives.
#'
#' @family class metrics
#' @seealso [All class metrics][class-metrics], [detection_prevalence()]
#' @templateVar fn detection_rate
#' @template event_first
#' @template multiclass
#' @template return
#'
#' @inheritParams sens
#'
#' @details
#' Suppose a 2x2 table with notation:
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Positive \tab Negative
#' \cr Positive \tab A \tab B \cr Negative \tab C \tab D \cr }
#'
#' The formula used here is:
#'
#' \deqn{\text{Detection Rate} = \frac{A}{A + B + C + D}}
#'
#' Unlike [sens()], the denominator includes all observations, not just the
#' true positive class. Unlike [detection_prevalence()], the numerator excludes
#' false positives.
#'
#' Detection rate is a metric that should be
#' `r attr(detection_rate, "direction")`d. The output ranges from
#' `r metric_range_chr(detection_rate, 1)` to
#' `r metric_range_chr(detection_rate, 2)`.
#' For binary data, its maximum is the prevalence of the positive class.
#'
#' @section Multiclass:
#' Each class is treated as the positive class in a one-vs-all comparison.
#' Macro and micro averaging both equal overall accuracy divided by the number
#' of class levels. Macro-weighted averaging weights the per-class detection
#' rates by the observed class frequencies. Case weights, when supplied, are
#' used in both the detection rates and these class frequencies.
#'
#' @template examples-class
#'
#' @export
detection_rate <- function(data, ...) {
  UseMethod("detection_rate")
}
detection_rate <- new_class_metric(
  detection_rate,
  direction = "maximize",
  range = c(0, 1)
)

#' @export
#' @rdname detection_rate
detection_rate.data.frame <- function(
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
    name = "detection_rate",
    fn = detection_rate_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    event_level = event_level,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
detection_rate.table <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "detection_rate",
    .estimator = estimator,
    .estimate = detection_rate_table_impl(data, estimator, event_level)
  )
}

#' @export
detection_rate.matrix <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  detection_rate.table(data, estimator, event_level)
}

#' @export
#' @rdname detection_rate
detection_rate_vec <- function(
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_bool(na_rm)
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
  detection_rate_table_impl(data, estimator, event_level)
}

detection_rate_table_impl <- function(data, estimator, event_level) {
  if (is_binary(estimator)) {
    detection_rate_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- detection_rate_multiclass(data, estimator)
    stats::weighted.mean(out_vec, w)
  }
}

detection_rate_binary <- function(data, event_level) {
  pos_level <- pos_val(data, event_level)
  data[pos_level, pos_level] / sum(data)
}

detection_rate_multiclass <- function(data, estimator) {
  numer <- diag(data)
  denom <- rep(sum(data), times = nrow(data))

  denom[denom <= 0] <- NA_real_

  if (is_micro(estimator)) {
    numer <- sum(numer)
    denom <- sum(denom)
  }

  numer / denom
}
