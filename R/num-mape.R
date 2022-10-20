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
mape.data.frame <- function(data,
                            truth,
                            estimate,
                            na_rm = TRUE,
                            case_weights = NULL,
                            ...) {
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
mape_vec <- function(truth,
                     estimate,
                     na_rm = TRUE,
                     case_weights = NULL,
                     ...) {
  metric_vec_template(
    metric_impl = mape_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    case_weights = case_weights,
    cls = "numeric"
  )
}

mape_impl <- function(truth, estimate, ..., case_weights = NULL) {
  errors <- abs((truth - estimate) / truth)
  out <- yardstick_mean(errors, case_weights = case_weights)
  out <- out * 100
  out
}
