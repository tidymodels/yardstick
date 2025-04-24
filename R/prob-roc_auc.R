#' Area under the receiver operator curve
#'
#' @description
#' `roc_auc()` is a metric that computes the area under the ROC curve. See
#' [roc_curve()] for the full curve.
#'
#' @details
#' Generally, an ROC AUC value is between `0.5` and `1`, with `1` being a
#' perfect prediction model. If your value is between `0` and `0.5`, then
#' this implies that you have meaningful information in your model, but it
#' is being applied incorrectly because doing the opposite of what the model
#' predicts would result in an AUC `>0.5`.
#'
#' Note that you can't combine `estimator = "hand_till"` with `case_weights`.
#'
#' @family class probability metrics
#' @templateVar fn roc_auc
#' @template return
#' @template event_first
#'
#' @section Multiclass:
#' The default multiclass method for computing `roc_auc()` is to use the
#' method from Hand, Till, (2001). Unlike macro-averaging, this method is
#' insensitive to class distributions like the binary ROC AUC case.
#' Additionally, while other multiclass techniques will return `NA` if any
#' levels in `truth` occur zero times in the actual data, the Hand-Till method
#' will simply ignore those levels in the averaging calculation, with a warning.
#'
#' Macro and macro-weighted averaging are still provided, even though they are
#' not the default. In fact, macro-weighted averaging corresponds to the same
#' definition of multiclass AUC given by Provost and Domingos (2001).
#'
#' @inheritParams pr_auc
#'
#' @param options `[deprecated]`
#'
#'   No longer supported as of yardstick 1.0.0. If you pass something here it
#'   will be ignored with a warning.
#'
#'   Previously, these were options passed on to `pROC::roc()`. If you need
#'   support for this, use the pROC package directly.
#'
#' @param estimator One of `"binary"`, `"hand_till"`, `"macro"`, or
#'   `"macro_weighted"` to specify the type of averaging to be done. `"binary"`
#'   is only relevant for the two class case. The others are general methods for
#'   calculating multiclass metrics. The default will automatically choose
#'   `"binary"` if `truth` is binary, `"hand_till"` if `truth` has >2 levels and
#'   `case_weights` isn't specified, or `"macro"` if `truth` has >2 levels and
#'   `case_weights` is specified (in which case `"hand_till"` isn't
#'   well-defined).
#'
#' @references
#' Hand, Till (2001). "A Simple Generalisation of the Area Under the
#' ROC Curve for Multiple Class Classification Problems". _Machine Learning_.
#' Vol 45, Iss 2, pp 171-186.
#'
#' Fawcett (2005). "An introduction to ROC analysis". _Pattern Recognition
#' Letters_. 27 (2006), pp 861-874.
#'
#' Provost, F., Domingos, P., 2001. "Well-trained PETs: Improving probability
#' estimation trees", CeDER Working Paper #IS-00-04, Stern School of Business,
#' New York University, NY, NY 10012.
#'
#' @seealso
#' [roc_curve()] for computing the full ROC curve.
#'
#' @author Max Kuhn
#'
#' @template examples-binary-prob
#' @template examples-multiclass-prob
#' @export
roc_auc <- function(data, ...) {
  UseMethod("roc_auc")
}
roc_auc <- new_prob_metric(
  roc_auc,
  direction = "maximize"
)

#' @export
#' @rdname roc_auc
roc_auc.data.frame <- function(
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  options = list()
) {
  check_roc_options_deprecated("roc_auc", options)

  case_weights_quo <- enquo(case_weights)

  out <- prob_metric_summarizer(
    name = "roc_auc",
    fn = roc_auc_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    case_weights = !!case_weights_quo
  )

  out <- roc_auc_adjust_result_estimator(
    out = out,
    estimator = estimator,
    case_weights_quo = case_weights_quo
  )

  out
}

#' @rdname roc_auc
#' @export
roc_auc_vec <- function(
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  options = list(),
  ...
) {
  abort_if_class_pred(truth)

  check_roc_options_deprecated("roc_auc_vec", options)

  estimator <- finalize_estimator_roc_auc(
    x = truth,
    estimator = estimator,
    metric_class = "roc_auc",
    case_weights = case_weights
  )

  check_prob_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  roc_auc_estimator_impl(
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    event_level = event_level,
    case_weights = case_weights
  )
}

roc_auc_estimator_impl <- function(
  truth,
  estimate,
  estimator,
  event_level,
  case_weights
) {
  if (is_binary(estimator)) {
    roc_auc_binary(truth, estimate, event_level, case_weights)
  } else if (estimator == "hand_till") {
    if (!is.null(case_weights)) {
      # should be unreachable
      cli::cli_abort(
        "{.arg case_weights} should be `NULL` at this point for hand-till.",
        .internal = TRUE
      )
    }

    roc_auc_hand_till(truth, estimate)
  } else {
    # weights for macro / macro_weighted are based on truth frequencies
    # (this is the usual definition)
    truth_table <- yardstick_truth_table(truth, case_weights = case_weights)
    w <- get_weights(truth_table, estimator)
    out_vec <- roc_auc_multiclass(truth, estimate, case_weights)
    stats::weighted.mean(out_vec, w)
  }
}

roc_auc_binary <- function(truth, estimate, event_level, case_weights) {
  lvls <- levels(truth)

  if (!is_event_first(event_level)) {
    lvls <- rev(lvls)
  }

  event <- lvls[[1]]
  control <- lvls[[2]]

  if (compute_n_occurrences(truth, event) == 0L) {
    # Warn here and return `NA`.
    # The curve computation would error and we can be slightly more forgiving.
    warn_roc_truth_no_event(event)
    return(NA_real_)
  }
  if (compute_n_occurrences(truth, control) == 0L) {
    # Warn here and return `NA`.
    # The curve computation would error and we can be slightly more forgiving.
    warn_roc_truth_no_control(control)
    return(NA_real_)
  }

  curve <- roc_curve_vec(
    truth = truth,
    estimate = estimate,
    na_rm = FALSE,
    event_level = event_level,
    case_weights = case_weights
  )

  sensitivity <- curve$sensitivity
  specificity <- curve$specificity

  auc(
    x = specificity,
    y = sensitivity,
    na_rm = FALSE
  )
}

roc_auc_multiclass <- function(truth, estimate, case_weights) {
  results <- one_vs_all_impl(
    fn = roc_auc_binary,
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )

  vapply(results, FUN.VALUE = numeric(1), function(x) x)
}

# ------------------------------------------------------------------------------

finalize_estimator_roc_auc <- function(
  x,
  estimator,
  metric_class,
  case_weights
) {
  # This is the `roc_auc_vec()` side of the hack we have to do to go from
  # hand_till -> macro when case weights are supplied. See
  # `roc_auc_adjust_result_estimator()` for all of the details.

  automatic_estimator <- is.null(estimator)

  estimator <- finalize_estimator(
    x = x,
    estimator = estimator,
    metric_class = metric_class
  )

  if (identical(estimator, "hand_till") && !is.null(case_weights)) {
    if (automatic_estimator) {
      # Automatically chose hand-till. Adjust automatic decision to "macro"
      estimator <- "macro"
    } else {
      # Manually chose hand-till and specified case weights. Not compatible!
      cli::cli_abort(
        "Can't specify both {.code estimator = 'hand_till'} and
        {.code case_weights}."
      )
    }
  }

  estimator
}

roc_auc_adjust_result_estimator <- function(out, estimator, case_weights_quo) {
  # This is a horrible hack that we have to do to support the fact that
  # `"hand_till"` can be chosen automatically, but doesn't support case weights.
  # In that case, `roc_auc_vec()` will switch to `"macro"`, but we need that
  # to propagate up into the data frame method's `.estimator` column.
  # The alternative is to adjust `finalize_estimator()` to know about the
  # `case_weights` just for this one metric, and that seemed like too much work.
  automatically_chose_hand_till_but_also_used_case_weights <-
    is.null(estimator) &&
    !quo_is_null(case_weights_quo) &&
    identical(out[[".estimator"]][[1]], "hand_till")

  if (automatically_chose_hand_till_but_also_used_case_weights) {
    # `roc_auc_vec()` actually "automatically" used `"macro"` weighting here
    out[[".estimator"]] <- "macro"
  }

  out
}

# ------------------------------------------------------------------------------

roc_auc_hand_till <- function(truth, estimate) {
  lvls <- levels(truth)

  # We want to reference the levels by name in the function below, so we
  # force the column names to be the same as the levels
  # (and assume the prob matrix columns are given in the same
  # order as the levels of `truth`)
  colnames(estimate) <- lvls

  # Check for levels with no observations in `truth`. Generally this would
  # return `NA`, but to match pROC and HandTill2001 we remove them with a
  # warning and proceed with the remaining levels (#123)
  lvls_loc <- match(lvls, truth)

  if (anyNA(lvls_loc)) {
    indicator_missing <- is.na(lvls_loc)

    lvls_missing <- lvls[indicator_missing]

    cli::cli_warn(
      c(
        x = "No observations were detected in {.arg truth} for level{?s}:
          {lvls_missing}.",
        i = "Computation will proceed by ignoring those levels."
      )
    )

    # Proceed with non-missing levels
    lvls <- lvls[!indicator_missing]
  }

  C <- length(lvls)

  multiplier <- 2 / (C * (C - 1))

  sum_val <- 0

  for (i_lvl in lvls) {
    # Double sum:
    # (sum i<j)
    cutpoint <- which(lvls == i_lvl)
    j_lvls <- lvls[-seq_len(cutpoint)]

    for (j_lvl in j_lvls) {
      A_hat_i_given_j <- roc_auc_subset(i_lvl, j_lvl, truth, estimate)
      A_hat_j_given_i <- roc_auc_subset(j_lvl, i_lvl, truth, estimate)

      A_hat_ij <- mean(c(A_hat_i_given_j, A_hat_j_given_i))

      # sum A_hat(i, j)
      sum_val <- sum_val + A_hat_ij
    }
  }

  multiplier * sum_val
}

# A_hat(i | j) in the paper
roc_auc_subset <- function(lvl1, lvl2, truth, estimate) {
  # Subset where truth is one of the two current levels
  subset_idx <- which(truth == lvl1 | truth == lvl2)

  # Use estimate based on lvl1 being the relevant level
  # Estimate for lvl2 is just 1-lvl1 rather than the value that
  # is actually there for the multiclass case
  estimate_lvl1 <- estimate[, lvl1, drop = TRUE]

  # subset and recode truth to only have 2 levels
  truth_subset <- factor(truth[subset_idx], levels = c(lvl1, lvl2))
  estimate_subset <- estimate_lvl1[subset_idx]

  # Hand Till method ignores event level (like macro-average).
  # As far as we know, using case weights doesn't make any sense with Hand Till.
  # See also: https://github.com/scikit-learn/scikit-learn/pull/12789
  auc_val <- roc_auc_binary(
    truth = truth_subset,
    estimate = estimate_subset,
    event_level = "first",
    case_weights = NULL
  )

  auc_val
}

# ------------------------------------------------------------------------------

compute_n_occurrences <- function(x, what) {
  # `NA` values have already been removed by `roc_auc_vec()`
  sum(x == what)
}

msg_roc_truth_no_control <- function(control) {
  paste0(
    "No control observations were detected in {.arg truth} ",
    "with control level '",
    control,
    "'."
  )
}
warn_roc_truth_no_control <- function(control) {
  cli::cli_warn(
    msg_roc_truth_no_control(control),
    class = "yardstick_warning_roc_truth_no_control"
  )
}
stop_roc_truth_no_control <- function(control) {
  cli::cli_abort(
    msg_roc_truth_no_control(control),
    class = "yardstick_error_roc_truth_no_control",
    call = call("roc_curve")
  )
}

msg_roc_truth_no_event <- function(event) {
  paste0(
    "No event observations were detected in {.arg truth} ",
    "with event level '",
    event,
    "'."
  )
}
warn_roc_truth_no_event <- function(event) {
  cli::cli_warn(
    msg_roc_truth_no_event(event),
    class = "yardstick_warning_roc_truth_no_event"
  )
}
stop_roc_truth_no_event <- function(event, call = caller_env()) {
  cli::cli_abort(
    msg_roc_truth_no_event(event),
    class = "yardstick_error_roc_truth_no_event",
    call = call
  )
}
