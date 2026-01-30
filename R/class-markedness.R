#' Markedness
#'
#' @description
#' Markedness is defined as:
#'
#' [precision()] + "inverse precision" - 1
#'
#' where "inverse precision" is the proportion of true negatives among all
#' predicted negatives. A related metric is Informedness, see the Details
#' section for the relationship.
#'
#' @details
#' Suppose a 2x2 table with notation:
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Positive \tab Negative
#' \cr Positive \tab A \tab B \cr Negative \tab C \tab D \cr }
#'
#' The formulas used here are:
#'
#' \deqn{\text{Precision} = \frac{A}{A + B}}
#'
#' \deqn{\text{Inverse Precision} = \frac{D}{C + D}}
#'
#' \deqn{\text{Markedness} = \text{Precision} + \text{Inverse Precision} - 1}
#'
#' Markedness is a metric that should be `r attr(markedness, "direction")`d. The
#' output ranges from `r metric_range(markedness)[1]` to
#' `r metric_range(markedness)[2]`, with `r metric_optimal(markedness)` indicating
#' perfect predictions.
#'
#' Markedness is to the predicted condition (precision and inverse precision)
#' what Informedness ([j_index()]) is to the actual condition (sensitivity and
#' specificity).
#'
#' @family class metrics
#' @seealso [All class metrics][class-metrics]
#' @templateVar fn markedness
#' @template event_first
#' @template multiclass
#' @template return
#'
#' @inheritParams sens
#'
#' @references
#'
#' Powers, David M W (2011). "Evaluation: From Precision, Recall and F-Score to
#' ROC, Informedness, Markedness and Correlation". Journal of Machine Learning
#' Technologies. 2 (1): 37-63.
#'
#' @template examples-class
#'
#' @export
markedness <- function(data, ...) {
  UseMethod("markedness")
}
markedness <- new_class_metric(
  markedness,
  direction = "maximize",
  range = c(-1, 1)
)

#' @rdname markedness
#' @export
markedness.data.frame <- function(
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
    name = "markedness",
    fn = markedness_vec,
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
markedness.table <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "markedness",
    .estimator = estimator,
    .estimate = markedness_table_impl(data, estimator, event_level)
  )
}

#' @export
markedness.matrix <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  markedness.table(data, estimator, event_level)
}

#' @rdname markedness
#' @export
markedness_vec <- function(
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
  markedness_table_impl(data, estimator, event_level)
}

markedness_table_impl <- function(data, estimator, event_level) {
  if (is_binary(estimator)) {
    markedness_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- markedness_multiclass(data, estimator)
    # Set `na.rm = TRUE` to remove undefined values from weighted computation (#265)
    stats::weighted.mean(out_vec, w, na.rm = TRUE)
  }
}

markedness_binary <- function(data, event_level) {
  precision_binary(data, event_level) +
    inverse_precision_binary(data, event_level) -
    1
}

markedness_multiclass <- function(data, estimator) {
  precision_multiclass(data, estimator) +
    inverse_precision_multiclass(data, estimator) -
    1
}

# Inverse precision: D / (C + D) - proportion of true negatives among
# predicted negatives
inverse_precision_binary <- function(data, event_level) {
  relevant <- pos_val(data, event_level)
  not_relevant <- setdiff(colnames(data), relevant)

  numer <- data[not_relevant, not_relevant]
  denom <- sum(data[not_relevant, ])

  undefined <- denom <= 0
  if (undefined) {
    count <- data[relevant, not_relevant]
    warn_inverse_precision_undefined_binary(not_relevant, count)
    return(NA_real_)
  }

  numer / denom
}

inverse_precision_multiclass <- function(data, estimator) {
  # For each class i, inverse precision is TN_i / (TN_i + FN_i)
  # where TN_i = sum of all cells not in row i or column i
  # and FN_i = sum of column i excluding diagonal
  n_classes <- ncol(data)
  numer <- numeric(n_classes)
  denom <- numeric(n_classes)

  for (i in seq_len(n_classes)) {
    # True negatives for class i: all predictions not in row i that were
    # correctly not class i
    tn_i <- sum(data[-i, -i, drop = FALSE])
    # False negatives for class i: predictions in row != i that were class i
    fn_i <- sum(data[-i, i])
    numer[i] <- tn_i
    denom[i] <- tn_i + fn_i
  }

  undefined <- denom <= 0
  if (any(undefined)) {
    # Count false positives (actual negatives that were incorrectly predicted
    # as positive)
    counts <- rowSums(data) - diag(data)
    counts <- counts[undefined]
    events <- colnames(data)[undefined]
    warn_inverse_precision_undefined_multiclass(events, counts)
    numer[undefined] <- NA_real_
    denom[undefined] <- NA_real_
  }

  if (is_micro(estimator)) {
    numer <- sum(numer, na.rm = TRUE)
    denom <- sum(denom, na.rm = TRUE)
  }

  numer / denom
}

warn_inverse_precision_undefined_binary <- function(event, count) {
  message <- c(
    "While computing binary {.fn markedness}, no predicted non-events were
    detected (i.e. `true_negative + false_negative = 0`).",
    "Markedness is undefined in this case, and `NA` will be returned."
  )

  message <- c(
    message,
    paste(
      "Note that",
      count,
      "true non-event(s) actually occurred for the problematic event level,",
      event
    )
  )

  cli::cli_warn(
    message = message,
    class = "yardstick_warning_markedness_undefined_binary"
  )
}

warn_inverse_precision_undefined_multiclass <- function(events, counts) {
  message <- c(
    "While computing multiclass {.fn markedness}, some levels had no predicted
    non-events (i.e. `true_negative + false_negative = 0`).",
    "Markedness is undefined in this case, and those levels will be removed
    from the averaged result.",
    "Note that the following number of true non-events actually occurred for
    each problematic event level:",
    paste0("'", events, "': ", counts, collapse = ", ")
  )

  cli::cli_warn(
    message = message,
    class = "yardstick_warning_markedness_undefined_multiclass"
  )
}
