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
#' @templateVar metric_fn spec
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
spec <-  function(data, ...) {
  UseMethod("spec")
}

class(spec) <- c("class_metric", "function")

#' @export
#' @rdname spec
spec.data.frame <- function(data, truth, estimate,
                            estimator = NULL, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "spec",
    metric_fn = spec_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
spec.table <- function(data, estimator = NULL, ...) {

  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "spec",
    .estimator = estimator,
    .estimate = spec_table_impl(data, estimator)
  )

}

#' @export
spec.matrix <- function(data, estimator = NULL, ...) {

  data <- as.table(data)
  spec.table(data, estimator)

}

#' @export
#' @rdname spec
spec_vec <- function(truth, estimate, estimator = NULL, na_rm = TRUE,...) {

  estimator <- finalize_estimator(truth, estimator)

  spec_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    spec_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = spec_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

#' @importFrom stats weighted.mean
spec_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    spec_binary(data)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- spec_multiclass(data, estimator)
    # set `na.rm = TRUE` to remove undefined values from weighted computation (#98)
    weighted.mean(out_vec, w, na.rm = TRUE)
  }

}

spec_binary <- function(data) {

  negative <- neg_val(data)

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

  tp   <- diag(data)
  tpfp <- rowSums(data)
  tpfn <- colSums(data)
  tn   <- n - (tpfp + tpfn - tp)
  fp   <- tpfp - tp

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
  if(is_micro(estimator)) {
    numer <- sum(numer, na.rm = TRUE)
    denom <- sum(denom, na.rm = TRUE)
  }

  numer / denom
}


warn_spec_undefined_binary <- function(event, count) {
  message <- paste0(
    "While computing binary `spec()`, no true negatives were detected ",
    "(i.e. `true_negative + false_positive = 0`). ",
    "\n",
    "Specificity is undefined in this case, and `NA` will be returned.",
    "\n",
    "Note that ", count, " predicted negatives(s) actually occured for the problematic ",
    "event level, '", event, "'."
  )

  warn_spec_undefined(
    message = message,
    events = event,
    counts = count,
    .subclass = "yardstick_warning_spec_undefined_binary"
  )
}

warn_spec_undefined_multiclass <- function(events, counts) {
  message <- paste0(
    "While computing multiclass `spec()`, some levels had no true negatives ",
    "(i.e. `true_negative + false_positive = 0`). ",
    "\n",
    "Specificity is undefined in this case, and those levels will be removed from the averaged result.",
    "\n",
    "Note that the following number of predicted negatives actually occured for each problematic event level:",
    "\n",
    paste0("'", events, "': ", counts, collapse = "\n")
  )

  warn_spec_undefined(
    message = message,
    events = events,
    counts = counts,
    .subclass = "yardstick_warning_spec_undefined_multiclass"
  )
}

warn_spec_undefined <- function(message, events, counts, ..., .subclass = character()) {
  rlang::warn(
    message = message,
    .subclass = c(.subclass, "yardstick_warning_spec_undefined"),
    events = events,
    counts = counts,
    ...
  )
}

