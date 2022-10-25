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
smape.data.frame <- function(data,
                             truth,
                             estimate,
                             na_rm = TRUE,
                             case_weights = NULL,
                             ...) {
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
smape_vec <- function(truth,
                      estimate,
                      na_rm = TRUE,
                      case_weights = NULL,
                      ...) {

  validate_numeric_truth_numeric_estimate(truth, estimate)

  numeric_metric_vec_template(
    metric_impl = smape_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    case_weights = case_weights
  )
}

smape_impl <- function(truth,
                       estimate,
                       ...,
                       case_weights = NULL) {
  check_dots_empty()

  numer <- abs(estimate - truth)
  denom <- (abs(truth) + abs(estimate)) / 2
  error <- numer / denom

  out <- yardstick_mean(error, case_weights = case_weights)
  out <- out * 100

  out
}
