#' Developer function for checking inputs in new metrics
#'
#' `check_numeric_metric()`, `check_class_metric()`, and `check_prob_metric()`
#' are useful alongside [metric-summarizers] for implementing new custom
#' metrics. [metric-summarizers] call the metric function inside
#' `dplyr::summarise()`. These functions perform checks on the inputs in
#' accordance with the type of metric that is used.
#'
#' @param truth The realized vector of `truth`.
#'   - For `check_numeric_metric()`, a numeric vector.
#'   - For `check_class_metric()`, a factor.
#'   - For `check_prob_metric()`, a factor.
#'
#' @param estimate The realized `estimate` result.
#'   - For `check_numeric_metric()`, a numeric vector.
#'   - For `check_class_metric()`, a factor.
#'   - For `check_prob_metric()`, a numeric vector for binary `truth`,
#'     a numeric matrix for multic-class `truth`.
#'
#' @param .time Numeric vector.
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
check_numeric_metric <- function(truth, estimate, case_weights) {
  validate_case_weights(case_weights, size = length(truth))
  validate_numeric_truth_numeric_estimate(truth, estimate)
}

#' @rdname check_metric
#' @export
check_class_metric <- function(truth, estimate, case_weights, estimator) {
  validate_case_weights(case_weights, size = length(truth))
  validate_factor_truth_factor_estimate(truth, estimate)
  validate_binary_estimator(truth, estimator)
}

#' @rdname check_metric
#' @export
check_prob_metric <- function(truth, estimate, case_weights, estimator) {
  validate_case_weights(case_weights, size = length(truth))
  validate_factor_truth_matrix_estimate(truth, estimate, estimator)
  validate_binary_estimator(truth, estimator)
}

#' @rdname check_metric
#' @export
check_surv_dynamic_metric <- function(truth, estimate, case_weights, .time) {
  validate_case_weights(case_weights, size = length(truth))
  validate_time(.time)
  validate_surv_truth_numeric_estimate(truth, estimate)
}
