#' Symmetric mean absolute percentage error
#'
#' Calculate the symmetric mean absolute percentage error. This metric is in
#' _relative units_.
#'
#' This implementation of `smape()` is the "usual definition" where the
#' denominator is divided by two.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar fn smape
#' @template return
#'
#' @inheritParams rmse
#'
#' @author Max Kuhn, Riaz Hedayati
#'
#' @template examples-numeric
#'
#' @export
#'
smape <- function(data, ...) {
  UseMethod("smape")
}
smape <- new_numeric_metric(
  smape,
  direction = "minimize"
)

#' @rdname smape
#' @export
smape.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "smape",
    fn = smape_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname smape
smape_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  smape_impl(truth, estimate, case_weights)
}

smape_impl <- function(truth, estimate, case_weights) {
  numer <- abs(estimate - truth)
  denom <- (abs(truth) + abs(estimate)) / 2
  error <- numer / denom

  out <- yardstick_mean(error, case_weights = case_weights)
  out <- out * 100

  out
}
