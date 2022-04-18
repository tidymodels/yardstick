#' Root mean squared error
#'
#' Calculate the root mean squared error. `rmse()` is a metric that is in
#' the same units as the original data.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn rmse
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
#' `data`. For `_vec()` functions, a numeric vector.
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
rmse.data.frame <- function(data,
                            truth,
                            estimate,
                            na_rm = TRUE,
                            case_weights = NULL,
                            ...) {
  metric_summarizer(
    metric_nm = "rmse",
    metric_fn = rmse_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname rmse
rmse_vec <- function(truth,
                     estimate,
                     na_rm = TRUE,
                     case_weights = NULL,
                     ...) {
  metric_vec_template(
    metric_impl = rmse_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    case_weights = case_weights,
    cls = "numeric"
  )
}

rmse_impl <- function(truth, estimate, ..., case_weights = NULL) {
  check_dots_empty()
  errors <- (truth - estimate) ^ 2
  sqrt(yardstick_mean(errors, case_weights = case_weights))
}
