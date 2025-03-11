#' Specificity
#'
#' These functions calculate the [spec()] (specificity) of a measurement system
#' compared to a reference result (the "truth" or gold standard).
#' Highly related functions are [sens()], [ppv()], and [npv()].
#'
#' The specificity measures the proportion of negatives that are correctly
#' identified as negatives.
#'
#' When the denominator of the calculation is `0`, specificity is undefined.
#' This happens when both `# true_negative = 0` and `# false_positive = 0`
#' are true, which mean that there were no true negatives. When computing binary
#' specificity, a `NA` value will be returned with a warning. When computing
#' multiclass specificity, the individual `NA` values will be removed, and the
#' computation will procede, with a warning.
#'
#' @family class metrics
#' @family sensitivity metrics
#' @templateVar fn spec
#' @template event_first
#' @template multiclass
#' @template return
#' @template table-positive
#'
#' @inheritParams sens
#'
#'
#' @author Max Kuhn
#'
#' @references
#'
#' Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 1:
#' sensitivity and specificity,'' *British Medical Journal*,
#' vol 308, 1552.
#'
#' @template examples-class
#'
#' @export
spec <- function(data, ...) {
  UseMethod("spec")
}
spec <- new_class_metric(
  spec,
  direction = "maximize"
)

#' @export
#' @rdname spec
spec.data.frame <- function(
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
    name = "spec",
    fn = spec_vec,
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
spec.table <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "spec",
    .estimator = estimator,
    .estimate = spec_table_impl(data, estimator, event_level)
  )
}

#' @export
spec.matrix <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  spec.table(data, estimator, event_level)
}

#' @export
#' @rdname spec
spec_vec <- function(
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
  spec_table_impl(data, estimator, event_level)
}

# ------------------------------------------------------------------------------

#' @rdname spec
#' @export
specificity <- function(data, ...) {
  UseMethod("specificity")
}
specificity <- new_class_metric(
  specificity,
  direction = "maximize"
)

#' @rdname spec
#' @export
specificity.data.frame <- function(
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
    name = "specificity",
    fn = spec_vec,
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
specificity.table <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "specificity",
    .estimator = estimator,
    .estimate = spec_table_impl(data, estimator, event_level)
  )
}

#' @export
specificity.matrix <- function(
  data,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)
  specificity.table(data, estimator, event_level)
}

#' @rdname spec
#' @export
specificity_vec <- spec_vec

# ------------------------------------------------------------------------------

spec_table_impl <- function(data, estimator, event_level) {
  if (is_binary(estimator)) {
    spec_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- spec_multiclass(data, estimator)
    # set `na.rm = TRUE` to remove undefined values from weighted computation (#98)
    stats::weighted.mean(out_vec, w, na.rm = TRUE)
  }
}

spec_binary <- function(data, event_level) {
  negative <- neg_val(data, event_level)

  numer <- sum(data[negative, negative])
  denom <- sum(data[, negative])

  undefined <- denom <= 0
  if (undefined) {
    positive <- setdiff(colnames(data), negative)
    count <- data[negative, positive]
    warn_spec_undefined_binary(positive, count)
    return(NA_real_)
  }

  numer / denom
}

spec_multiclass <- function(data, estimator) {
  n <- sum(data)

  tp <- diag(data)
  tpfp <- rowSums(data)
  tpfn <- colSums(data)
  tn <- n - (tpfp + tpfn - tp)
  fp <- tpfp - tp

  numer <- tn
  denom <- tn + fp

  undefined <- denom <= 0
  if (any(undefined)) {
    counts <- tpfn - tp
    counts <- counts[undefined]
    events <- colnames(data)[undefined]
    warn_spec_undefined_multiclass(events, counts)
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

warn_spec_undefined_binary <- function(event, count) {
  message <- c(
    "While computing binary {.fn spec}, no true negatives were detected
    (i.e. `true_negative + false_positive = 0`).",
    "Specificity is undefined in this case, and `NA` will be returned."
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

  warn_spec_undefined(
    message = message,
    events = event,
    counts = count,
    class = "yardstick_warning_spec_undefined_binary"
  )
}

warn_spec_undefined_multiclass <- function(events, counts) {
  message <- c(
    "While computing multiclass {.fn spec}, some levels had no true negatives
    (i.e. `true_negative + false_positive = 0`).",
    "Specificity is undefined in this case, and those levels will be removed
    from the averaged result.",
    "Note that the following number of predicted negatives actually occurred
    for each problematic event level:",
    paste0("'", events, "': ", counts, collapse = ", ")
  )

  warn_spec_undefined(
    message = message,
    events = events,
    counts = counts,
    class = "yardstick_warning_spec_undefined_multiclass"
  )
}

warn_spec_undefined <- function(
  message,
  events,
  counts,
  ...,
  class = character()
) {
  cli::cli_warn(
    message = message,
    class = c(class, "yardstick_warning_spec_undefined"),
    events = events,
    counts = counts,
    ...
  )
}
