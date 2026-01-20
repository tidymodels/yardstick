#' Mean absolute error
#'
#' Calculate the mean absolute error. This metric is in the same units as the
#' original data.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar fn mae
#' @template return
#'
#' @inheritParams rmse
#'
#' @details
#' MAE is a metric that should be `r attr(mae, "direction")`d. The output
#' ranges from `r metric_range(mae)[1]` to `r metric_range(mae)[2]`, with
#' `r metric_optimal(mae)` indicating perfect predictions.
#'
#' @author Max Kuhn
#'
#' @template examples-numeric
#'
#' @export
mae <- function(data, ...) {
  UseMethod("mae")
}
mae <- new_numeric_metric(
  mae,
  direction = "minimize",
  range = c(0, Inf)
)

#' @rdname mae
#' @export
mae.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "mae",
    fn = mae_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname mae
mae_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
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

  mae_impl(truth, estimate, case_weights)
}

mae_impl <- function(truth, estimate, case_weights) {
  errors <- abs(truth - estimate)
  yardstick_mean(errors, case_weights = case_weights)
}
