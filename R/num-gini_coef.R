#' Normalized Gini coefficient
#'
#' Compute the normalized Gini coefficient, which measures the ranking ability
#' of a regression model based on the Lorenz curve. This metric is useful for
#' evaluating models that predict risk or loss costs, such as insurance pricing
#' models.
#'
#' @family numeric metrics
#' @seealso [All numeric metrics][numeric-metrics]
#' @templateVar fn gini_coef
#' @template return
#'
#' @inheritParams rmse
#'
#' @details
#' The normalized Gini coefficient is a metric that should be
#' `r attr(gini_coef, "direction")`d. The output ranges from
#' `r metric_range(gini_coef)[1]` to `r metric_range(gini_coef)[2]`, with
#' `r metric_optimal(gini_coef)` indicating perfect ranking ability where
#' predicted values perfectly rank the true values.
#'
#' The Gini coefficient is calculated from the Lorenz curve, which plots the
#' cumulative proportion of the total truth values against the cumulative
#' proportion of observations when sorted by predicted values. The raw Gini is
#' the area between the Lorenz curve and the diagonal line of equality. The
#' normalized Gini divides this by the maximum possible Gini (achieved when
#' observations are sorted by the true values).
#'
#' The formula is:
#'
#' \deqn{\text{Normalized Gini} = \frac{G(\text{estimate})}{G(\text{truth})}}
#'
#' where \eqn{G(x)} is the Gini coefficient when sorting by \eqn{x}.
#'
#' Note that `gini_coef()` is a regression metric based on ranking, distinct
#' from [gain_capture()] which is a classification metric.
#'
#' Unlike many other metrics, `gini_coef()` is not symmetric with respect to
#' `truth` and `estimate`. The `estimate` values determine the sorting order,
#' while the `truth` values are accumulated along the Lorenz curve. Swapping
#' them will produce different results.
#'
#' When the true values are constant (zero variance), the Gini coefficient is
#' undefined and `NA` is returned with a warning.
#'
#' @template examples-numeric
#'
#' @export
gini_coef <- function(data, ...) {
  UseMethod("gini_coef")
}
gini_coef <- new_numeric_metric(
  gini_coef,
  direction = "maximize",
  range = c(0, 1)
)

#' @rdname gini_coef
#' @export
gini_coef.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "gini_coef",
    fn = gini_coef_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname gini_coef
gini_coef_vec <- function(
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

  gini_coef_impl(truth, estimate, case_weights)
}

gini_coef_impl <- function(truth, estimate, case_weights) {
  case_weights <- vec_cast(case_weights, to = double())
  n <- length(truth)

  if (n < 2) {
    return(NA_real_)
  }

  # Check for constant truth values
  if (length(unique(truth)) == 1) {
    cli::cli_warn(
      c(
        "Column {.arg truth} has constant values.",
        "i" = "The Gini coefficient is undefined when truth has no variation."
      ),
      class = "yardstick_warning_gini_coef_undefined"
    )
    return(NA_real_)
  }

  # Check for zero sum of truth (which would cause division by zero)
  truth_sum <- sum(truth)
  if (abs(truth_sum) < .Machine$double.eps) {
    cli::cli_warn(
      c(
        "Column {.arg truth} sums to zero.",
        "i" = "The Gini coefficient is undefined when the sum of truth is zero."
      ),
      class = "yardstick_warning_gini_coef_undefined"
    )
    return(NA_real_)
  }

  gini_by_estimate <- gini_raw(truth, estimate, case_weights)
  gini_by_truth <- gini_raw(truth, truth, case_weights)

  # Normalize
  gini_by_estimate / gini_by_truth
}

gini_raw <- function(truth, order_by, case_weights) {
  # Sort by order_by (descending), then by truth (descending) for ties
  ord <- order(order_by, truth, decreasing = TRUE)
  truth_sorted <- truth[ord]

  if (is.null(case_weights)) {
    case_weights <- rep(1, length(truth))
  }
  weights_sorted <- case_weights[ord]

  # Calculate cumulative proportions
  cumulative_weights <- cumsum(weights_sorted)
  total_weights <- sum(weights_sorted)
  weight_proportions <- cumulative_weights / total_weights

  # Weighted cumulative truth
  weighted_truth <- truth_sorted * weights_sorted
  cumulative_truth <- cumsum(weighted_truth)
  total_truth <- sum(weighted_truth)
  truth_proportions <- cumulative_truth / total_truth

  # Calculate area under the Lorenz curve using trapezoidal rule
  # Prepend 0 for the origin
  weight_proportions <- c(0, weight_proportions)
  truth_proportions <- c(0, truth_proportions)

  # Area under Lorenz curve
  n <- length(weight_proportions)
  lorenz_area <- sum(
    (weight_proportions[-1] - weight_proportions[-n]) *
      (truth_proportions[-1] + truth_proportions[-n]) /
      2
  )

  # Gini = 1 - 2 * area under Lorenz curve
  1 - 2 * lorenz_area
}
