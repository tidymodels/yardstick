#' Fall-out (False Positive Rate)
#'
#' These functions calculate the fall-out (false positive rate) of a
#' measurement system compared to a reference result (the "truth" or gold
#' standard). Fall-out is defined as `1 - specificity`, or equivalently,
#' the proportion of negatives that are incorrectly classified as positives.
#'
#' Fall-out is also known as the false positive rate (FPR) or the probability
#' of false alarm.
#'
#' When the denominator of the calculation is `0`, fall-out is undefined.
#' This happens when both `# true_negative = 0` and `# false_positive = 0`
#' are true, which means that there were no negatives. When computing binary
#' fall-out, a `NA` value will be returned with a warning. When computing
#' multiclass fall-out, the individual `NA` values will be removed, and the
#' computation will proceed, with a warning.
#'
#' @family class metrics
#' @family sensitivity metrics
#' @seealso [All class metrics][class-metrics]
#' @templateVar fn fall_out
#' @template event_first
#' @template multiclass
#' @template return
#' @inheritParams sens
#'
#' @details
#' Suppose a 2x2 table with notation:
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Positive \tab Negative
#' \cr Positive \tab A \tab B \cr Negative \tab C \tab D \cr }
#'
#' The formula used here is:
#'
#' \deqn{\text{Fall-out} = \frac{B}{B + D}}
#'
#' Fall-out is a metric that should be `r attr(fall_out, "direction")`d. The
#' output ranges from `r metric_range(fall_out)[1]` to
#' `r metric_range(fall_out)[2]`, with `r metric_optimal(fall_out)` indicating
#' that all actual negatives were correctly predicted as negative (no false
#' positives).
#'
#' @template examples-class
#'
#' @export
fall_out <- function(data, ...) {
  UseMethod("fall_out")
}
fall_out <- new_class_metric(
  fall_out,
  direction = "minimize",
  range = c(0, 1)
)

#' @export
#' @rdname fall_out
fall_out.data.frame <- function(
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
    name = "fall_out",
    fn = fall_out_vec,
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
fall_out.table <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "fall_out",
    .estimator = estimator,
    .estimate = fall_out_table_impl(data, estimator, event_level)
  )
}

#' @export
fall_out.matrix <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  fall_out.table(data, estimator, event_level)
}

#' @export
#' @rdname fall_out
fall_out_vec <- function(
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
  fall_out_table_impl(data, estimator, event_level)
}

# ------------------------------------------------------------------------------

fall_out_table_impl <- function(data, estimator, event_level) {
  if (is_binary(estimator)) {
    fall_out_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- fall_out_multiclass(data, estimator)
    # set `na.rm = TRUE` to remove undefined values from weighted computation (#98)
    stats::weighted.mean(out_vec, w, na.rm = TRUE)
  }
}

fall_out_binary <- function(data, event_level) {
  negative <- neg_val(data, event_level)
  positive <- setdiff(colnames(data), negative)

  # FP = predicted positive but actually negative

  numer <- sum(data[positive, negative])
  # TN + FP = all actual negatives

  denom <- sum(data[, negative])

  undefined <- denom <= 0
  if (undefined) {
    count <- data[negative, positive]
    warn_fall_out_undefined_binary(positive, count)
    return(NA_real_)
  }

  numer / denom
}

fall_out_multiclass <- function(data, estimator) {
  n <- sum(data)

  tp <- diag(data)
  tpfp <- rowSums(data)
  tpfn <- colSums(data)
  tn <- n - (tpfp + tpfn - tp)
  fp <- tpfp - tp

  numer <- fp
  denom <- tn + fp

  undefined <- denom <= 0
  if (any(undefined)) {
    counts <- tpfn - tp
    counts <- counts[undefined]
    events <- colnames(data)[undefined]
    warn_fall_out_undefined_multiclass(events, counts)
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

warn_fall_out_undefined_binary <- function(event, count) {
  message <- c(
    "While computing binary {.fn fall_out}, no true negatives were detected
    (i.e. `true_negative + false_positive = 0`).",
    "Fall-out is undefined in this case, and `NA` will be returned."
  )

  message <- c(
    message,
    paste(
      "Note that",
      count,
      "predicted negatives(s) actually occurred for the problematic event level,",
      event
    )
  )

  warn_fall_out_undefined(
    message = message,
    events = event,
    counts = count,
    class = "yardstick_warning_fall_out_undefined_binary"
  )
}

warn_fall_out_undefined_multiclass <- function(events, counts) {
  message <- c(
    "While computing multiclass {.fn fall_out}, some levels had no true negatives
    (i.e. `true_negative + false_positive = 0`).",
    "Fall-out is undefined in this case, and those levels will be removed
    from the averaged result.",
    "Note that the following number of predicted negatives actually occurred
    for each problematic event level:",
    paste0("'", events, "': ", counts, collapse = ", ")
  )

  warn_fall_out_undefined(
    message = message,
    events = events,
    counts = counts,
    class = "yardstick_warning_fall_out_undefined_multiclass"
  )
}

warn_fall_out_undefined <- function(
  message,
  events,
  counts,
  ...,
  class = character()
) {
  cli::cli_warn(
    message = message,
    class = c(class, "yardstick_warning_fall_out_undefined"),
    events = events,
    counts = counts,
    ...
  )
}
