#' Recall
#'
#' These functions calculate the [recall()] of a measurement system for
#' finding relevant documents compared to reference results
#' (the truth regarding relevance). Highly related functions are [precision()]
#' and [f_meas()].
#'
#' The recall (aka sensitivity) is defined as the proportion of
#' relevant results out of the number of samples which were
#' actually relevant. When there are no relevant results, recall is
#' not defined and a value of `NA` is returned.
#'
#' When the denominator of the calculation is `0`, recall is undefined. This
#' happens when both `# true_positive = 0` and `# false_negative = 0` are true,
#' which mean that there were no true events. When computing binary
#' recall, a `NA` value will be returned with a warning. When computing
#' multiclass recall, the individual `NA` values will be removed, and the
#' computation will procede, with a warning.
#'
#' @family class metrics
#' @family relevance metrics
#' @templateVar fn recall
#' @template event_first
#' @template multiclass
#' @template return
#' @template table-relevance
#'
#' @inheritParams sens
#'
#' @references
#'
#' Buckland, M., & Gey, F. (1994). The relationship
#'  between Recall and Precision. *Journal of the American Society
#'  for Information Science*, 45(1), 12-19.
#'
#' Powers, D. (2007). Evaluation: From Precision, Recall and F
#'  Factor to ROC, Informedness, Markedness and Correlation.
#'  Technical Report SIE-07-001, Flinders University
#'
#' @author Max Kuhn
#'
#' @template examples-class
#'
#' @export
recall <- function(data, ...) {
  UseMethod("recall")
}
recall <- new_class_metric(
  recall,
  direction = "maximize"
)

#' @rdname recall
#' @export
recall.data.frame <- function(
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
    name = "recall",
    fn = recall_vec,
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
recall.table <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "recall",
    .estimator = estimator,
    .estimate = recall_table_impl(data, estimator, event_level)
  )
}

#' @export
recall.matrix <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  recall.table(data, estimator, event_level)
}

#' @export
#' @rdname recall
recall_vec <- function(
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
) {
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
  recall_table_impl(data, estimator, event_level)
}

recall_table_impl <- function(data, estimator, event_level) {
  if (is_binary(estimator)) {
    recall_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- recall_multiclass(data, estimator)
    # set `na.rm = TRUE` to remove undefined values from weighted computation (#98)
    stats::weighted.mean(out_vec, w, na.rm = TRUE)
  }
}

recall_binary <- function(data, event_level) {
  relevant <- pos_val(data, event_level)
  numer <- sum(data[relevant, relevant])
  denom <- sum(data[, relevant])

  undefined <- denom <= 0
  if (undefined) {
    not_relevant <- setdiff(colnames(data), relevant)
    count <- data[relevant, not_relevant]
    warn_recall_undefined_binary(relevant, count)
    return(NA_real_)
  }

  numer / denom
}

recall_multiclass <- function(data, estimator) {
  numer <- diag(data)
  denom <- colSums(data)

  undefined <- denom <= 0
  if (any(undefined)) {
    counts <- rowSums(data) - numer
    counts <- counts[undefined]
    events <- colnames(data)[undefined]
    warn_recall_undefined_multiclass(events, counts)
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

warn_recall_undefined_binary <- function(event, count) {
  message <- c(
    "While computing binary {.fn recall}, no true events were detected
    (i.e. `true_positive + false_negative = 0`).",
    "Recall is undefined in this case, and `NA` will be returned."
  )

  message <- c(
    message,
    paste(
      "Note that",
      count,
      "predicted event(s) actually occurred for the problematic event level",
      event
    )
  )

  warn_recall_undefined(
    message = message,
    events = event,
    counts = count,
    class = "yardstick_warning_recall_undefined_binary"
  )
}

warn_recall_undefined_multiclass <- function(events, counts) {
  message <- c(
    "While computing multiclass {.fn recall}, some levels had no true events
    (i.e. `true_positive + false_negative = 0`).",
    "Recall is undefined in this case, and those levels will be removed from
    the averaged result.",
    "Note that the following number of predicted events actually occurred for
    each problematic event level:",
    paste0("'", events, "': ", counts, collapse = ", ")
  )

  warn_recall_undefined(
    message = message,
    events = events,
    counts = counts,
    class = "yardstick_warning_recall_undefined_multiclass"
  )
}

warn_recall_undefined <- function(
  message,
  events,
  counts,
  ...,
  class = character()
) {
  cli::cli_warn(
    message = message,
    class = c(class, "yardstick_warning_recall_undefined"),
    events = events,
    counts = counts,
    ...
  )
}
