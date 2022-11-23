#' Developer function for handling missing values in new metrics
#'
#' `handle_missings()`,  and `detect_missings()` are useful alongside the
#' [metric-summarizers()] functions for implementing new custom metrics.
#' `handle_missings()` removes any observations that contains missing values
#' across, truth, estimate and case_weights. `detect_missings()` returns an
#' indicator if there is any missing values in the inputs.
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
#' @name handle_missings
NULL

#' @rdname handle_missings
#' @export
handle_missings <- function(truth, estimate, case_weights) {
  complete_cases <- stats::complete.cases(truth, estimate, case_weights)
  truth <- truth[complete_cases]

  if (is.matrix(estimate)) {
    estimate <- estimate[complete_cases, , drop = FALSE]
  } else {
    estimate <- estimate[complete_cases]
  }

  case_weights <- case_weights[complete_cases]

  list(truth = truth, estimate = estimate, case_weights = case_weights)
}

#' @rdname handle_missings
#' @export
detect_missings <- function(truth, estimate, case_weights) {
  if (is_class_pred(truth)) {
    truth <- as_factor_from_class_pred(truth)
  }
  if (is_class_pred(estimate)) {
    estimate <- as_factor_from_class_pred(estimate)
  }
  any_na <-
    anyNA(truth) ||
    anyNA(estimate) ||
    (!is.null(case_weights) && anyNA(case_weights))
}
