#' Mean absolute percent error
#'
#' Calculate the mean absolute percentage error. This metric is in _relative
#' units_.
#'
#' Note that a value of `Inf` is returned for `mape()` when the
#' observed value is negative.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar fn mape
#' @template return
#'
#' @inheritParams rmse
#'
#' @author Max Kuhn
#'
#' @template examples-numeric
#'
#' @export
#'
mape <- function(data, ...) {
  UseMethod("mape")
}
mape <- new_numeric_metric(
  mape,
  direction = "minimize"
)

#' @rdname mape
#' @export
mape.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "mape",
    fn = mape_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname mape
mape_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  mape_impl(truth, estimate, case_weights)
}

mape_impl <- function(truth, estimate, case_weights) {
  errors <- abs((truth - estimate) / truth)
  out <- yardstick_mean(errors, case_weights = case_weights)
  out <- out * 100
  out
}
