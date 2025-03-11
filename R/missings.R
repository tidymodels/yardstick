#' Developer function for handling missing values in new metrics
#'
#' `yardstick_remove_missing()`,  and `yardstick_any_missing()` are useful
#' alongside the [metric-summarizers] functions for implementing new custom
#' metrics. `yardstick_remove_missing()` removes any observations that contains
#' missing values across, `truth`, `estimate` and `case_weights`.
#' `yardstick_any_missing()` returns `FALSE` if there is any missing values in
#' the inputs.
#'
#' @param truth,estimate Vectors of the same length.
#'
#' @param case_weights A vector of the same length as `truth` and `estimate`, or
#'   `NULL` if case weights are not being used.
#'
#' @seealso [metric-summarizers]
#'
#' @name yardstick_remove_missing
NULL

#' @rdname yardstick_remove_missing
#' @export
yardstick_remove_missing <- function(truth, estimate, case_weights) {
  complete_cases <- stats::complete.cases(
    truth,
    estimate,
    case_weights
  )

  if (.is_surv(truth, fail = FALSE)) {
    Surv_type <- .extract_surv_type(truth)

    truth <- truth[complete_cases, ]

    attr(truth, "type") <- Surv_type
    attr(truth, "class") <- "Surv"
  } else {
    truth <- truth[complete_cases]
  }

  if (is.matrix(estimate)) {
    estimate <- estimate[complete_cases, , drop = FALSE]
  } else {
    estimate <- estimate[complete_cases]
  }

  case_weights <- case_weights[complete_cases]

  list(
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )
}

#' @rdname yardstick_remove_missing
#' @export
yardstick_any_missing <- function(truth, estimate, case_weights) {
  anyNA(truth) ||
    anyNA(estimate) ||
    (!is.null(case_weights) && anyNA(case_weights))
}
