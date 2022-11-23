#' Developer function for calling new metrics
#'
#' `check_numeric_metric()`, `check_class_metric()`, and `check_prob_metric()`
#' are useful alongside the [metric-summarizers()] functions for implementing
#' new custom metrics. [metric-summarizers()] calls the metric function inside
#' `dplyr::summarise()`. These functions perform checks for the input in
#' accordance to the type of metric that is used.
#'
#' @param truth The realized vector of `truth`. This is either a factor or a
#'   numeric.
#'
#' @param estimate The realized `estimate` result. This is either a numeric
#'   vector, a factor vector, or a numeric matrix (in the case of multiple class
#'   probability columns) depending on your metric function.
#'
#' @param case_weights The realized case weights, as a numeric vector. This must
#'   be the same length as `truth`, and will be considered in the `na_rm`
#'   checks. This will be passed on to `metric_impl` as the named argument
#'   `case_weights`.
#'
#' @seealso [metric-summarizers()]
#'
#' @name check_metric
NULL

#' @rdname check_metric
#' @export
check_numeric_metric <- function(truth, estimate, case_weights) {
  validate_case_weights(case_weights, size = length(truth))
  validate_numeric_truth_numeric_estimate(truth, estimate)
}
