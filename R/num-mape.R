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
#' @templateVar metric_fn mape
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

class(mape) <- c("numeric_metric", "function")
attr(mape, "direction") <- "minimize"

#' @rdname mape
#' @export
mape.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "mape",
    metric_fn = mape_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
#' @rdname mape
mape_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  mape_impl <- function(truth, estimate) {
    mean( abs( (truth - estimate) / truth ) ) * 100
  }

  metric_vec_template(
    metric_impl = mape_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )

}
