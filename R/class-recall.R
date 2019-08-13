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
#' @family class metrics
#' @family relevance metrics
#' @templateVar metric_fn recall
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

class(recall) <- c("class_metric", "function")

#' @rdname recall
#' @export
recall.data.frame <- function(data, truth, estimate,
                              estimator = NULL, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "recall",
    metric_fn = recall_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
recall.table <- function(data, estimator = NULL, ...) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)
  metric_tibbler(
    .metric = "recall",
    .estimator = estimator,
    .estimate = recall_table_impl(data, estimator)
  )
}

#' @export
recall.matrix <- function(data, estimator = NULL, ...) {
  data <- as.table(data)
  recall.table(data, estimator)
}

#' @export
#' @rdname recall
recall_vec <- function(truth, estimate, estimator = NULL, na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  recall_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    recall_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = recall_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

recall_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    recall_binary(data)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- recall_multiclass(data, estimator)
    # set `na.rm = TRUE` to remove undefined values from weighted computation (#98)
    weighted.mean(out_vec, w, na.rm = TRUE)
  }

}

recall_binary <- function(data) {

  relevant <- pos_val(data)
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
  if(is_micro(estimator)) {
    numer <- sum(numer, na.rm = TRUE)
    denom <- sum(denom, na.rm = TRUE)
  }

  numer / denom

}


warn_recall_undefined_binary <- function(event, count) {
  message <- paste0(
    "While computing binary `recall()`, no true events were detected ",
    "(i.e. `true_positive + false_negative = 0`). ",
    "\n",
    "Recall is undefined in this case, and `NA` will be returned.",
    "\n",
    "Note that ", count, " predicted event(s) actually occured for the problematic ",
    "event level, '", event, "'."
  )

  warn_recall_undefined(
    message = message,
    events = event,
    counts = count,
    .subclass = "yardstick_warning_recall_undefined_binary"
  )
}

warn_recall_undefined_multiclass <- function(events, counts) {
  message <- paste0(
    "While computing multiclass `recall()`, some levels had no true events ",
    "(i.e. `true_positive + false_negative = 0`). ",
    "\n",
    "Recall is undefined in this case, and those levels will be removed from the averaged result.",
    "\n",
    "Note that the following number of predicted events actually occured for each problematic event level:",
    "\n",
    paste0("'", events, "': ", counts, collapse = "\n")
  )

  warn_recall_undefined(
    message = message,
    events = events,
    counts = counts,
    .subclass = "yardstick_warning_recall_undefined_multiclass"
  )
}

warn_recall_undefined <- function(message, events, counts, ..., .subclass = character()) {
  rlang::warn(
    message = message,
    .subclass = c(.subclass, "yardstick_warning_recall_undefined"),
    events = events,
    counts = counts,
    ...
  )
}

