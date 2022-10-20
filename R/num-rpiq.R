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
#' @templateVar fn rpd
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
rpiq <- new_numeric_metric(
  rpiq,
  direction = "maximize"
)

#' @rdname rpiq
#' @export
rpiq.data.frame <- function(data,
                            truth,
                            estimate,
                            na_rm = TRUE,
                            case_weights = NULL,
                            ...) {
  numeric_metric_summarizer(
    name = "rpiq",
    fn = rpiq_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname rpiq
rpiq_vec <- function(truth,
                     estimate,
                     na_rm = TRUE,
                     case_weights = NULL,
                     ...) {
  metric_vec_template(
    metric_impl = rpiq_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    case_weights = case_weights,
    cls = "numeric"
  )
}

rpiq_impl <- function(truth, estimate, ..., case_weights = NULL) {
  check_dots_empty()

  quantiles <- yardstick_quantile(
    x = truth,
    probabilities = c(0.25, 0.75),
    case_weights = case_weights
  )

  iqr <- quantiles[[2L]] - quantiles[[1L]]
  rmse <- rmse_vec(truth, estimate, case_weights = case_weights)

  iqr / rmse
}
