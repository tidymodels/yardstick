#' Mean squared error
#'
#' Calculate the mean squared error.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @seealso [rmse()] for the root mean squared error, which is the square root
#' of MSE and is in the same units as the original data.
#'
#' [All numeric metrics][numeric-metrics]
#' @templateVar fn mse
#' @template return
#'
#' @inheritParams rmse
#'
#' @details
#' MSE is a metric that should be `r attr(mse, "direction")`d. The output
#' ranges from `r metric_range(mse)[1]` to `r metric_range(mse)[2]`, with
#' `r metric_optimal(mse)` indicating perfect predictions.
#'
#' The formula for MSE is:
#'
#' \deqn{\text{MSE} = \frac{1}{n} \sum_{i=1}^{n} (\text{truth}_i - \text{estimate}_i)^2}
#'
#' @template examples-numeric
#'
#' @export
#'
mse <- function(data, ...) {
  UseMethod("mse")
}
mse <- new_numeric_metric(
  mse,
  direction = "minimize",
  range = c(0, Inf)
)

#' @rdname mse
#' @export
mse.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "mse",
    fn = mse_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname mse
mse_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
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

  mse_impl(truth, estimate, case_weights = case_weights)
}

mse_impl <- function(truth, estimate, case_weights) {
  errors <- (truth - estimate)^2
  yardstick_mean(errors, case_weights = case_weights)
}
