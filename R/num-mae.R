#' Mean absolute error
#'
#' Calculate the mean absolute error. This metric is in the same units as the
#' original data.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn mae
#' @template return
#'
#' @inheritParams rmse
#'
#' @author Max Kuhn
#'
#' @template examples-numeric
#'
#' @export
mae <- function(data, ...) {
  UseMethod("mae")
}

class(mae) <- c("numeric_metric", "function")

#' @rdname mae
#' @export
mae.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "mae",
    metric_fn = mae_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname mae
mae_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  mae_impl <- function(truth, estimate) {
    mean( abs(truth - estimate) )
  }

  metric_vec_template(
    metric_impl = mae_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "numeric",
    ...
  )

}
