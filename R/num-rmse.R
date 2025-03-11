#' Root mean squared error
#'
#' Calculate the root mean squared error. `rmse()` is a metric that is in
#' the same units as the original data.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar fn rmse
#' @template return
#'
#' @param data A `data.frame` containing the columns specified by the `truth`
#' and `estimate` arguments.
#'
#' @param truth The column identifier for the true results
#' (that is `numeric`). This should be an unquoted column name although
#' this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column
#' names). For `_vec()` functions, a `numeric` vector.
#'
#' @param estimate The column identifier for the predicted
#' results (that is also `numeric`). As with `truth` this can be
#' specified different ways but the primary method is to use an
#' unquoted variable name. For `_vec()` functions, a `numeric` vector.
#'
#' @param na_rm A `logical` value indicating whether `NA`
#' values should be stripped before the computation proceeds.
#'
#' @param case_weights The optional column identifier for case weights. This
#' should be an unquoted column name that evaluates to a numeric column in
#' `data`. For `_vec()` functions, a numeric vector,
#' [hardhat::importance_weights()], or [hardhat::frequency_weights()].
#'
#' @param ... Not currently used.
#'
#' @author Max Kuhn
#'
#' @template examples-numeric
#'
#' @export
#'
rmse <- function(data, ...) {
  UseMethod("rmse")
}
rmse <- new_numeric_metric(
  rmse,
  direction = "minimize"
)

#' @rdname rmse
#' @export
rmse.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "rmse",
    fn = rmse_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname rmse
rmse_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  rmse_impl(truth, estimate, case_weights = case_weights)
}

rmse_impl <- function(truth, estimate, case_weights) {
  errors <- (truth - estimate)^2
  sqrt(yardstick_mean(errors, case_weights = case_weights))
}
