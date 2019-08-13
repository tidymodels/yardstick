#' Sensitivity
#'
#' These functions calculate the [sens()] (sensitivity) of a measurement system
#' compared to a reference result (the "truth" or gold standard).
#' Highly related functions are [spec()], [ppv()], and [npv()].
#'
#' The sensitivity (`sens()`) is defined as the proportion of positive
#' results out of the number of samples which were actually
#' positive.
#'
#' When the denominator of the calculation is `0`, sensitivity is undefined.
#' This happens when both `# true_positive = 0` and `# false_negative = 0`
#' are true, which mean that there were no true events. When computing binary
#' sensitivity, a `NA` value will be returned with a warning. When computing
#' multiclass sensitivity, the individual `NA` values will be removed, and the
#' computation will procede, with a warning.
#'
#' @family class metrics
#' @family sensitivity metrics
#' @templateVar metric_fn sens
#' @template event_first
#' @template multiclass
#' @template return
#' @template table-positive
#'
#' @param data Either a `data.frame` containing the `truth` and `estimate`
#' columns, or a `table`/`matrix` where the true class results should be
#' in the columns of the table.
#'
#' @param truth The column identifier for the true class results
#'  (that is a `factor`). This should be an unquoted column name although
#'  this argument is passed by expression and supports
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column
#'  names). For `_vec()` functions, a `factor` vector.
#'
#' @param estimate The column identifier for the predicted class
#'  results (that is also `factor`). As with `truth` this can be
#'  specified different ways but the primary method is to use an
#'  unquoted variable name. For `_vec()` functions, a `factor` vector.
#'
#' @param estimator One of: `"binary"`, `"macro"`, `"macro_weighted"`,
#' or `"micro"` to specify the type of averaging to be done. `"binary"` is
#' only relevant for the two class case. The other three are general methods for
#' calculating multiclass metrics. The default will automatically choose `"binary"`
#' or `"macro"` based on `estimate`.
#'
#' @param na_rm A `logical` value indicating whether `NA`
#'  values should be stripped before the computation proceeds.
#'
#' @param ... Not currently used.
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
#'
sens <- function(data, ...) {
  UseMethod("sens")
}

class(sens) <- c("class_metric", "function")

#' @export
#' @rdname sens
sens.data.frame <- function(data, truth, estimate,
                            estimator = NULL, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "sens",
    metric_fn = sens_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
sens.table <- function(data, estimator = NULL, ...) {

  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "sens",
    .estimator = estimator,
    .estimate = sens_table_impl(data, estimator)
  )

}

#' @export
sens.matrix <- function(data, estimator = NULL, ...) {

  data <- as.table(data)
  sens.table(data, estimator)

}

#' @export
#' @rdname sens
sens_vec <- function(truth, estimate, estimator = NULL, na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  sens_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    sens_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = sens_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

# sens() == recall(), so this is a copy paste from there, with altered warning
# classes

sens_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    sens_binary(data)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- sens_multiclass(data, estimator)
    # set `na.rm = TRUE` to remove undefined values from weighted computation (#98)
    weighted.mean(out_vec, w, na.rm = TRUE)
  }

}

sens_binary <- function(data) {

  relevant <- pos_val(data)
  numer <- sum(data[relevant, relevant])
  denom <- sum(data[, relevant])

  undefined <- denom <= 0
  if (undefined) {
    not_relevant <- setdiff(colnames(data), relevant)
    count <- data[relevant, not_relevant]
    warn_sens_undefined_binary(relevant, count)
    return(NA_real_)
  }

  numer / denom

}

sens_multiclass <- function(data, estimator) {

  numer <- diag(data)
  denom <- colSums(data)

  undefined <- denom <= 0
  if (any(undefined)) {
    counts <- rowSums(data) - numer
    counts <- counts[undefined]
    events <- colnames(data)[undefined]
    warn_sens_undefined_multiclass(events, counts)
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


warn_sens_undefined_binary <- function(event, count) {
  message <- paste0(
    "While computing binary `sens()`, no true events were detected ",
    "(i.e. `true_positive + false_negative = 0`). ",
    "\n",
    "Sensitivity is undefined in this case, and `NA` will be returned.",
    "\n",
    "Note that ", count, " predicted event(s) actually occured for the problematic ",
    "event level, '", event, "'."
  )

  warn_sens_undefined(
    message = message,
    events = event,
    counts = count,
    .subclass = "yardstick_warning_sens_undefined_binary"
  )
}

warn_sens_undefined_multiclass <- function(events, counts) {
  message <- paste0(
    "While computing multiclass `sens()`, some levels had no true events ",
    "(i.e. `true_positive + false_negative = 0`). ",
    "\n",
    "Sensitivity is undefined in this case, and those levels will be removed from the averaged result.",
    "\n",
    "Note that the following number of predicted events actually occured for each problematic event level:",
    "\n",
    paste0("'", events, "': ", counts, collapse = "\n")
  )

  warn_sens_undefined(
    message = message,
    events = events,
    counts = counts,
    .subclass = "yardstick_warning_sens_undefined_multiclass"
  )
}

warn_sens_undefined <- function(message, events, counts, ..., .subclass = character()) {
  rlang::warn(
    message = message,
    .subclass = c(.subclass, "yardstick_warning_sens_undefined"),
    events = events,
    counts = counts,
    ...
  )
}
