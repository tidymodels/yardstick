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
#' @details
#' MAPE is a metric that should be `r attr(mape, "direction")`d. The output
#' ranges from `r metric_range(mape)[1]` to `r metric_range(mape)[2]`, with
#' `r metric_optimal(mape)` indicating perfect predictions.
#'
#' The formula for MAPE is:
#'
#' \deqn{\text{MAPE} = \frac{100}{n} \sum_{i=1}^{n} \left| \frac{\text{truth}_i - \text{estimate}_i}{\text{truth}_i} \right|}
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
  direction = "minimize",
  range = c(0, Inf)
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

  mape_impl(truth, estimate, case_weights)
}

mape_impl <- function(truth, estimate, case_weights) {
  errors <- abs((truth - estimate) / truth)
  out <- yardstick_mean(errors, case_weights = case_weights)
  out <- out * 100
  out
}
