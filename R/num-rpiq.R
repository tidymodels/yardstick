#' Ratio of performance to inter-quartile
#'
#' These functions are appropriate for cases where the model outcome is a
#' numeric. The ratio of performance to deviation
#' ([rpd()]) and the ratio of performance to inter-quartile ([rpiq()])
#' are both measures of consistency/correlation between observed
#' and predicted values (and not of accuracy).
#'
#' @inherit rpd details
#' @inherit rpd references
#'
#' @family numeric metrics
#' @family consistency metrics
#' @templateVar metric_fn rpd
#' @template return
#'
#' @inheritParams rmse
#'
#' @author Pierre Roudier
#'
#' @seealso
#'
#' The closely related deviation metric: [rpd()]
#'
#' @template examples-numeric
#'
#' @export
rpiq <- function(data, ...) {
  UseMethod("rpiq")
}

class(rpiq) <- c("numeric_metric", "function")

#' @rdname rpiq
#' @export
rpiq.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "rpiq",
    metric_fn = rpiq_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname rpiq
#' @importFrom stats IQR
rpiq_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  rpiq_impl <- function(truth, estimate) {

    IQR(truth) / rmse_vec(truth, estimate)

  }

  metric_vec_template(
    metric_impl = rpiq_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "numeric",
    ...
  )

}
