#' Miss rate (False Negative Rate)
#'
#' These functions calculate the miss rate (false negative rate) of a
#' measurement system compared to a reference result (the "truth" or gold
#' standard). Miss rate is defined as `1 - sensitivity`, or equivalently,
#' the proportion of positives that are incorrectly classified as negatives.
#'
#' Miss rate is also known as the false negative rate (FNR) or the probability
#' of miss.
#'
#' When the denominator of the calculation is `0`, miss rate is undefined.
#' This happens when both `# true_positive = 0` and `# false_negative = 0`
#' are true, which mean that there were no true events. When computing binary
#' miss rate, a `NA` value will be returned with a warning. When computing
#' multiclass miss rate, the individual `NA` values will be removed, and the
#' computation will proceed, with a warning.
#'
#' @family class metrics
#' @family sensitivity metrics
#' @templateVar fn miss_rate
#' @template event_first
#' @template multiclass
#' @template return
#' @template table-positive
#'
#' @inheritParams sens
#'
#' @details
#' Miss rate is a metric that should be `r attr(miss_rate, "direction")`d. The
#' output ranges from `r metric_range(miss_rate)[1]` to
#' `r metric_range(miss_rate)[2]`, with `r metric_optimal(miss_rate)` indicating
#' that all actual positives were correctly predicted as positive (no false
#' negatives).
#'
#' @author Max Kuhn
#'
#' @references
#'
#' Fawcett, T. (2006). "An introduction to ROC analysis". *Pattern
#' Recognition Letters*, 27(8), 861-874.
#'
#' @template examples-class
#'
#' @export
miss_rate <- function(data, ...) {
  UseMethod("miss_rate")
}
miss_rate <- new_class_metric(
  miss_rate,
  direction = "minimize",
  range = c(0, 1)
)

#' @export
#' @rdname miss_rate
miss_rate.data.frame <- function(
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
    name = "miss_rate",
    fn = miss_rate_vec,
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
miss_rate.table <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "miss_rate",
    .estimator = estimator,
    .estimate = miss_rate_table_impl(data, estimator, event_level)
  )
}

#' @export
miss_rate.matrix <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  miss_rate.table(data, estimator, event_level)
}

#' @export
#' @rdname miss_rate
miss_rate_vec <- function(
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
  miss_rate_table_impl(data, estimator, event_level)
}

# ------------------------------------------------------------------------------

miss_rate_table_impl <- function(data, estimator, event_level) {
  if (is_binary(estimator)) {
    miss_rate_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- miss_rate_multiclass(data, estimator)
    # set `na.rm = TRUE` to remove undefined values from weighted computation (#98)
    stats::weighted.mean(out_vec, w, na.rm = TRUE)
  }
}

miss_rate_binary <- function(data, event_level) {
  relevant <- pos_val(data, event_level)
  not_relevant <- setdiff(colnames(data), relevant)

  # FN = predicted negative but actually positive
  numer <- sum(data[not_relevant, relevant])
  # TP + FN = all actual positives
  denom <- sum(data[, relevant])

  undefined <- denom <= 0
  if (undefined) {
    count <- data[relevant, not_relevant]
    warn_miss_rate_undefined_binary(relevant, count)
    return(NA_real_)
  }

  numer / denom
}

miss_rate_multiclass <- function(data, estimator) {
  tp <- diag(data)
  fn <- colSums(data) - tp

  numer <- fn
  denom <- colSums(data)

  undefined <- denom <= 0
  if (any(undefined)) {
    counts <- rowSums(data) - tp
    counts <- counts[undefined]
    events <- colnames(data)[undefined]
    warn_miss_rate_undefined_multiclass(events, counts)
    numer[undefined] <- NA_real_
    denom[undefined] <- NA_real_
  }

  # set `na.rm = TRUE` to remove undefined values from weighted computation (#98)
  if (is_micro(estimator)) {
    numer <- sum(numer, na.rm = TRUE)
    denom <- sum(denom, na.rm = TRUE)
  }

  numer / denom
}

warn_miss_rate_undefined_binary <- function(event, count) {
  message <- c(
    "While computing binary {.fn miss_rate}, no true events were detected
    (i.e. `true_positive + false_negative = 0`).",
    "Miss rate is undefined in this case, and `NA` will be returned."
  )

  message <- c(
    message,
    paste(
      "Note that",
      count,
      "predicted event(s) actually occurred for the problematic event level,",
      event
    )
  )

  warn_miss_rate_undefined(
    message = message,
    events = event,
    counts = count,
    class = "yardstick_warning_miss_rate_undefined_binary"
  )
}

warn_miss_rate_undefined_multiclass <- function(events, counts) {
  message <- c(
    "While computing multiclass {.fn miss_rate}, some levels had no true events
    (i.e. `true_positive + false_negative = 0`).",
    "Miss rate is undefined in this case, and those levels will be removed
    from the averaged result.",
    "Note that the following number of predicted events actually occurred for
    each problematic event level:",
    paste0("'", events, "': ", counts, collapse = ", ")
  )

  warn_miss_rate_undefined(
    message = message,
    events = events,
    counts = counts,
    class = "yardstick_warning_miss_rate_undefined_multiclass"
  )
}

warn_miss_rate_undefined <- function(
  message,
  events,
  counts,
  ...,
  class = character()
) {
  cli::cli_warn(
    message = message,
    class = c(class, "yardstick_warning_miss_rate_undefined"),
    events = events,
    counts = counts,
    ...
  )
}
