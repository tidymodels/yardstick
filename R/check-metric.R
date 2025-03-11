#' Developer function for checking inputs in new metrics
#'
#' `check_numeric_metric()`, `check_class_metric()`, and `check_prob_metric()`
#' are useful alongside [metric-summarizers] for implementing new custom
#' metrics. [metric-summarizers] call the metric function inside
#' `dplyr::summarise()`. These functions perform checks on the inputs in
#' accordance with the type of metric that is used.
#'
#' @inheritParams rlang::args_error_context
#'
#' @param truth The realized vector of `truth`.
#'   - For `check_numeric_metric()`, a numeric vector.
#'   - For `check_class_metric()`, a factor.
#'   - For `check_prob_metric()`, a factor.
#'   - For `check_ordered_prob_metric()`, an ordered factor.
#'   - For `check_dynamic_survival_metric()`, a Surv object.
#'   - For `check_static_survival_metric()`, a Surv object.
#'
#' @param estimate The realized `estimate` result.
#'   - For `check_numeric_metric()`, a numeric vector.
#'   - For `check_class_metric()`, a factor.
#'   - For `check_prob_metric()`, a numeric vector for binary `truth`,
#'     a numeric matrix for multic-class `truth`.
#'   - For `check_ordered_prob_metric()`, a numeric vector for binary `truth`,
#'     a numeric matrix for multic-class `truth`.
#'   - For `check_dynamic_survival_metric()`, list-column of data.frames.
#'   - For `check_static_survival_metric()`, a numeric vector.
#'
#' @param case_weights The realized case weights, as a numeric vector. This must
#'   be the same length as `truth`.
#'
#' @param estimator This can either be `NULL` for the default auto-selection of
#' averaging (`"binary"` or `"macro"`), or a single character to pass along to
#' the metric implementation describing the kind of averaging to use.
#'
#' @seealso [metric-summarizers]
#'
#' @name check_metric
NULL

#' @rdname check_metric
#' @export
check_numeric_metric <- function(
  truth,
  estimate,
  case_weights,
  call = caller_env()
) {
  validate_case_weights(case_weights, size = length(truth), call = call)
  validate_numeric_truth_numeric_estimate(truth, estimate, call = call)
}

#' @rdname check_metric
#' @export
check_class_metric <- function(
  truth,
  estimate,
  case_weights,
  estimator,
  call = caller_env()
) {
  validate_case_weights(case_weights, size = length(truth), call = call)
  validate_factor_truth_factor_estimate(truth, estimate, call = call)
  validate_binary_estimator(truth, estimator, call = call)
}

#' @rdname check_metric
#' @export
check_prob_metric <- function(
  truth,
  estimate,
  case_weights,
  estimator,
  call = caller_env()
) {
  validate_case_weights(case_weights, size = length(truth), call = call)
  validate_factor_truth_matrix_estimate(truth, estimate, estimator, call = call)
  validate_binary_estimator(truth, estimator, call = call)
}

#' @rdname check_metric
#' @export
check_ordered_prob_metric <- function(
  truth,
  estimate,
  case_weights,
  estimator,
  call = caller_env()
) {
  validate_case_weights(case_weights, size = length(truth), call = call)
  validate_ordered_truth_matrix_estimate(
    truth,
    estimate,
    estimator,
    call = call
  )
  validate_binary_estimator(truth, estimator, call = call)
}

#' @rdname check_metric
#' @export
check_dynamic_survival_metric <- function(
  truth,
  estimate,
  case_weights,
  call = caller_env()
) {
  validate_surv_truth_list_estimate(truth, estimate, call = call)
  validate_case_weights(case_weights, size = nrow(truth), call = call)
}

#' @rdname check_metric
#' @export
check_static_survival_metric <- function(
  truth,
  estimate,
  case_weights,
  call = caller_env()
) {
  validate_case_weights(case_weights, size = nrow(truth), call = call)
  validate_surv_truth_numeric_estimate(truth, estimate, call = call)
}
