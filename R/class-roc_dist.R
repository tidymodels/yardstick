#' Distance to ROC corner
#'
#' @description
#' `roc_dist()` calculates the Euclidean distance from the observed
#' (sensitivity, specificity) point to the ideal corner (1, 1) in ROC space.
#' This is equivalent to the distance from (FPR, TPR) to (0, 1).
#'
#' This metric is sometimes called "closest to top-left" in ROC analysis and
#' provides an alternative to [j_index()] for finding optimal classification
#' thresholds.
#'
#' @details
#' Suppose a 2x2 table with notation:
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Positive \tab Negative
#' \cr Positive \tab A \tab B \cr Negative \tab C \tab D \cr }
#'
#' The formulas used here are:
#'
#' \deqn{\text{Sensitivity} = \frac{A}{A + C}}
#'
#' \deqn{\text{Specificity} = \frac{D}{B + D}}
#'
#' \deqn{\text{roc\_dist} = \sqrt{(1 - \text{Sensitivity})^2 +
#'   (1 - \text{Specificity})^2}}
#'
#' `roc_dist` is a metric that should be `r attr(roc_dist, "direction")`d. The
#' output ranges from `r metric_range(roc_dist)[1]` to
#' `r metric_range(roc_dist)[2]`, with `r metric_optimal(roc_dist)` indicating
#' perfect sensitivity and specificity.
#'
#' @family class metrics
#' @seealso
#'
#' [All class metrics][class-metrics]
#'
#' [j_index()] for Youden's J statistic, another metric for measuring closeness
#' to the ideal classification point.
#'
#' @templateVar fn roc_dist
#' @template event_first
#' @template multiclass
#' @template return
#'
#' @inheritParams sens
#'
#' @template examples-class
#'
#' @export
roc_dist <- function(data, ...) {
  UseMethod("roc_dist")
}
roc_dist <- new_class_metric(
  roc_dist,
  direction = "minimize",
  range = c(0, sqrt(2))
)

#' @rdname roc_dist
#' @export
roc_dist.data.frame <- function(
  data,
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,

  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  class_metric_summarizer(
    name = "roc_dist",
    fn = roc_dist_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    event_level = event_level
  )
}

#' @export
roc_dist.table <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "roc_dist",
    .estimator = estimator,
    .estimate = roc_dist_table_impl(data, estimator, event_level)
  )
}

#' @export
roc_dist.matrix <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  roc_dist.table(data, estimator, event_level)
}

#' @rdname roc_dist
#' @export
roc_dist_vec <- function(
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_bool(na_rm)
  abort_if_class_pred(truth)
  estimate <- as_factor_from_class_pred(estimate)

  estimator <- finalize_estimator(truth, estimator)

  check_class_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  data <- yardstick_table(truth, estimate, case_weights = case_weights)
  roc_dist_table_impl(data, estimator, event_level)
}

roc_dist_table_impl <- function(data, estimator, event_level) {
  if (is_binary(estimator)) {
    roc_dist_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- roc_dist_multiclass(data, estimator)
    # Set `na.rm = TRUE` to remove undefined values from weighted computation
    stats::weighted.mean(out_vec, w, na.rm = TRUE)
  }
}

roc_dist_binary <- function(data, event_level) {
  sensitivity <- sens_binary(data, event_level)
  specificity <- spec_binary(data, event_level)
  sqrt((1 - sensitivity)^2 + (1 - specificity)^2)
}

roc_dist_multiclass <- function(data, estimator) {
  sensitivity <- sens_multiclass(data, estimator)
  specificity <- spec_multiclass(data, estimator)
  sqrt((1 - sensitivity)^2 + (1 - specificity)^2)
}
