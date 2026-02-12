#' Relative root mean squared error
#'
#' Calculate the relative root mean squared error. This metric is the root mean
#' squared error normalized by the range of the true values.
#' `rmse_relative()` is sometimes called normalized RMSE (NRMSE) when range
#' normalization is used.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @seealso [All numeric metrics][numeric-metrics]
#' @templateVar fn rmse_relative
#' @template return
#'
#' @inheritParams rmse
#'
#' @details
#' Relative RMSE is a metric that should be
#' `r attr(rmse_relative, "direction")`d. The output ranges from
#' `r metric_range(rmse_relative)[1]` to `r metric_range(rmse_relative)[2]`,
#' with `r metric_optimal(rmse_relative)` indicating perfect predictions.
#'
#' The formula for relative RMSE is:
#'
#' \deqn{\text{RMSE} = \sqrt{\frac{1}{n} \sum_{i=1}^{n} (\text{truth}_i - \text{estimate}_i)^2}}
#'
#' \deqn{\text{Relative RMSE} = \frac{\text{RMSE}}{\text{max}(\text{truth}) -
#' \text{min}(\text{truth})}}
#'
#' Note that if all true values are identical (i.e., the range is zero), the
#' result will be `Inf`.
#'
#' @template examples-numeric
#'
#' @export
#'
rmse_relative <- function(data, ...) {
  UseMethod("rmse_relative")
}
rmse_relative <- new_numeric_metric(
  rmse_relative,
  direction = "minimize",

  range = c(0, Inf)
)

#' @rdname rmse_relative
#' @export
rmse_relative.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "rmse_relative",
    fn = rmse_relative_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname rmse_relative
rmse_relative_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
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

  rmse_relative_impl(truth, estimate, case_weights = case_weights)
}

rmse_relative_impl <- function(truth, estimate, case_weights) {
  truth_range <- max(truth) - min(truth)
  rmse <- rmse_impl(truth, estimate, case_weights)
  rmse / truth_range
}
