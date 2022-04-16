#' Area under the receiver operator curve
#'
#' `roc_auc()` is a metric that computes the area under the ROC curve. See
#' [roc_curve()] for the full curve.
#'
#' The underlying `direction` option in [pROC::roc()] is forced to
#' `direction = "<"`. This computes the ROC curve assuming that the `estimate`
#' values are the probability that the "event" occurred, which is what they
#' are always assumed to be in yardstick.
#'
#' Generally, an ROC AUC value is between `0.5` and `1`, with `1` being a
#' perfect prediction model. If your value is between `0` and `0.5`, then
#' this implies that you have meaningful information in your model, but it
#' is being applied incorrectly because doing the opposite of what the model
#' predicts would result in an AUC `>0.5`.
#'
#' @family class probability metrics
#' @templateVar metric_fn roc_auc
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
#' `"macro_weighted"` to specify the type of averaging to be done. `"binary"`
#' is only relevant for the two class case. The others are general methods for
#' calculating multiclass metrics. The default will automatically choose
#' `"binary"` or `"hand_till"` based on `truth`.
#'
#' @references
#'
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
#'
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
roc_auc.data.frame <- function(data,
                               truth,
                               ...,
                               estimator = NULL,
                               na_rm = TRUE,
                               event_level = yardstick_event_level(),
                               options = list()) {
  check_roc_options_deprecated("roc_auc", options)

  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "roc_auc",
    metric_fn = roc_auc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level
  )
}

#' @rdname roc_auc
#' @export
roc_auc_vec <- function(truth,
                        estimate,
                        estimator = NULL,
                        na_rm = TRUE,
                        event_level = yardstick_event_level(),
                        options = list(),
                        ...) {
  check_roc_options_deprecated("roc_auc_vec", options)

  estimator <- finalize_estimator(truth, estimator, "roc_auc")

  roc_auc_impl <- function(truth, estimate) {
    roc_auc_estimator_impl(truth, estimate, estimator, event_level)
  }

  metric_vec_template(
    metric_impl = roc_auc_impl,
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    na_rm = na_rm,
    cls = c("factor", "numeric")
  )
}

roc_auc_estimator_impl <- function(truth, estimate, estimator, event_level) {
  if (is_binary(estimator)) {
    roc_auc_binary(truth, estimate, event_level)
  }
  else if (estimator == "hand_till") {
    roc_auc_hand_till(truth, estimate)
  }
  else {
    # weights for macro / macro_weighted are based on truth frequencies
    # (this is the usual definition)
    truth_table <- matrix(table(truth), nrow = 1)
    w <- get_weights(truth_table, estimator)
    out_vec <- roc_auc_multiclass(truth, estimate)
    stats::weighted.mean(out_vec, w)
  }
}

roc_auc_binary <- function(truth, estimate, event_level) {
  lvls <- levels(truth)

  if (is_event_first(event_level)) {
    lvls <- rev(lvls)
  }

  control <- lvls[[1]]
  event <- lvls[[2]]

  if (compute_n_occurrences(truth, control) == 0L) {
    warn_roc_truth_no_control(control)
    return(NA_real_)
  }
  if (compute_n_occurrences(truth, event) == 0L) {
    warn_roc_truth_no_event(event)
    return(NA_real_)
  }

  args <- quos(
    response = truth,
    predictor = estimate,
    levels = lvls,
    quiet = TRUE,
    direction = "<"
  )

  pROC_auc <- eval_tidy(call2("auc", !!! args, .ns = "pROC"))

  as.numeric(pROC_auc)
}

roc_auc_multiclass <- function(truth, estimate) {
  res_lst <- one_vs_all_impl(roc_auc_binary, truth, estimate)
  rlang::flatten_dbl(res_lst)
}

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
    lvls_missing <- single_quote(lvls_missing)
    lvls_missing <- paste0(lvls_missing, collapse = ", ")

    msg <- paste0(
      "No observations were detected in `truth` for level(s): ",
      lvls_missing,
      "\n",
      "Computation will proceed by ignoring those levels."
    )

    rlang::warn(msg)

    # Proceed with non-missing levels
    lvls <- lvls[!indicator_missing]
  }

  C <- length(lvls)

  multiplier <- 2 / (C * (C - 1))

  sum_val <- 0

  for(i_lvl in lvls) {
    # Double sum:
    # (sum i<j)
    cutpoint <- which(lvls == i_lvl)
    j_lvls <- lvls[-seq_len(cutpoint)]

    for(j_lvl in j_lvls) {
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

  # Hand Till method ignores event level (like macro-average)
  auc_val <- roc_auc_binary(
    truth = truth_subset,
    estimate = estimate_subset,
    event_level = "first"
  )

  auc_val
}

# ------------------------------------------------------------------------------

compute_n_occurrences <- function(x, what) {
  # `NA` values have already been removed by `metric_vec_template()`
  sum(x == what)
}

msg_roc_truth_no_control <- function(control) {
  paste0(
    "No control observations were detected in `truth` ",
    "with control level '", control, "'."
  )
}
warn_roc_truth_no_control <- function(control) {
  rlang::warn(
    msg_roc_truth_no_control(control),
    class = "yardstick_warning_roc_truth_no_control"
  )
}
stop_roc_truth_no_control <- function(control) {
  rlang::abort(
    msg_roc_truth_no_control(control),
    class = "yardstick_error_roc_truth_no_control"
  )
}

msg_roc_truth_no_event <- function(event) {
  paste0(
    "No event observations were detected in `truth` ",
    "with event level '", event, "'."
  )
}
warn_roc_truth_no_event <- function(event) {
  rlang::warn(
    msg_roc_truth_no_event(event),
    class = "yardstick_warning_roc_truth_no_event"
  )
}
stop_roc_truth_no_event <- function(event) {
  rlang::abort(
    msg_roc_truth_no_event(event),
    class = "yardstick_error_roc_truth_no_event"
  )
}

# ------------------------------------------------------------------------------

single_quote <- function(x) {
  encodeString(x, quote = "'", na.encode = FALSE)
}
