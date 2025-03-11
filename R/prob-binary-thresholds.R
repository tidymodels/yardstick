# For use with the `pr_curve()` and `roc_curve()`.
# Returns a data frame with:
# - Unique thresholds
# - Number of true positives per threshold
# - Number of false positives per threshold
binary_threshold_curve <- function(
  truth,
  estimate,
  ...,
  event_level = yardstick_event_level(),
  case_weights = NULL
) {
  check_dots_empty()

  if (is.null(case_weights)) {
    case_weights <- rep(1, times = length(truth))
  }
  case_weights <- vec_cast(case_weights, to = double())

  if (!is.factor(truth)) {
    # should be unreachable
    cli::cli_abort(
      "{.arg truth} must be a factor, not {.obj_type_friendly {truth}}.",
      .internal = TRUE
    )
  }
  if (length(levels(truth)) != 2L) {
    # should be unreachable
    cli::cli_abort(
      "{.arg truth} must have two levels, not {length(levels(truth))}.",
      .internal = TRUE
    )
  }
  if (!is.numeric(estimate)) {
    # should be unreachable
    cli::cli_abort(
      "{.arg estimate} must be numeric vector, not {.obj_type_friendly {estimate}}.",
      .internal = TRUE
    )
  }
  if (length(truth) != length(estimate)) {
    # should be unreachable
    cli::cli_abort(
      "{.arg truth} ({length(truth)}) and
      {.arg estimate} ({length(estimate)}) must be the same length.",
      .internal = TRUE
    )
  }
  if (length(truth) != length(case_weights)) {
    # should be unreachable
    cli::cli_abort(
      "{.arg truth} ({length(truth)}) and
      {.arg case_weights} ({length(case_weights)}) must be the same length.",
      .internal = TRUE
    )
  }

  truth <- unclass(truth)

  # Convert to `1 == event`, `0 == non-event`
  if (is_event_first(event_level)) {
    truth <- as.integer(truth == 1L)
  } else {
    truth <- as.integer(truth == 2L)
  }

  # Drop any `0` weights.
  # These shouldn't affect the result, but can result in divide by zero
  # issues if they are left in.
  detect_zero_weight <- case_weights == 0
  if (any(detect_zero_weight)) {
    detect_non_zero_weight <- !detect_zero_weight
    truth <- truth[detect_non_zero_weight]
    estimate <- estimate[detect_non_zero_weight]
    case_weights <- case_weights[detect_non_zero_weight]
  }

  # Sort by decreasing `estimate`
  order <- order(estimate, decreasing = TRUE)
  truth <- truth[order]
  estimate <- estimate[order]
  case_weights <- case_weights[order]

  # Skip repeated probabilities.
  # We want the last duplicate to ensure that we capture all the events from the
  # `cumsum()`, so we use `fromLast`.
  loc_unique <- which(!duplicated(estimate, fromLast = TRUE))
  thresholds <- estimate[loc_unique]

  case_weights_events <- truth * case_weights
  case_weights_non_events <- (1 - truth) * case_weights

  if (sum(case_weights_events) == 0L) {
    cli::cli_warn(
      "There are 0 event cases in {.arg truth}, results will be meaningless."
    )
  }

  tp <- cumsum(case_weights_events)
  tp <- tp[loc_unique]

  fp <- cumsum(case_weights_non_events)
  fp <- fp[loc_unique]

  dplyr::tibble(
    threshold = thresholds,
    tp = tp,
    fp = fp
  )
}
