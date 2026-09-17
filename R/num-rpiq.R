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
#' @seealso [All numeric metrics][numeric-metrics]
#' @templateVar fn rpd
#' @template return
#'
#' @inheritParams rmse
#'
#' @details
#' RPIQ is a metric that should be `r attr(rpiq, "direction")`d. The output
#' ranges from `r metric_range(rpiq)[1]` to `r metric_range(rpiq)[2]`, with
#' higher values indicating better model performance.
#'
#' The formula for RPIQ is:
#'
#' \deqn{\text{RPIQ} = \frac{\text{IQR}(\text{truth})}{\text{RMSE}}}
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
  direction = "maximize",
  range = c(0, Inf)
)

#' @rdname rpiq
#' @export
rpiq.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
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
rpiq_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_bool(na_rm)
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  rpiq_impl(truth, estimate, case_weights)
}

rpiq_impl <- function(truth, estimate, case_weights) {
  quantiles <- yardstick_quantile(
    x = truth,
    probabilities = c(0.25, 0.75),
    case_weights = case_weights
  )

  iqr <- quantiles[[2L]] - quantiles[[1L]]
  rmse <- rmse_vec(truth, estimate, case_weights = case_weights)

  iqr / rmse
}
