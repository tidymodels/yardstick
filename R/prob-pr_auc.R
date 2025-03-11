#' Area under the precision recall curve
#'
#' `pr_auc()` is a metric that computes the area under the precision
#' recall curve. See [pr_curve()] for the full curve.
#'
#'
#' @family class probability metrics
#' @templateVar fn pr_auc
#' @template return
#' @template multiclass-prob
#' @template event_first
#'
#' @inheritParams sens
#'
#' @param data A `data.frame` containing the columns specified by `truth` and
#' `...`.
#'
#' @param estimate If `truth` is binary, a numeric vector of class probabilities
#' corresponding to the "relevant" class. Otherwise, a matrix with as many
#' columns as factor levels of `truth`. _It is assumed that these are in the
#' same order as the levels of `truth`._
#'
#' @param ... A set of unquoted column names or one or more
#' `dplyr` selector functions to choose which variables contain the
#' class probabilities. If `truth` is binary, only 1 column should be selected,
#' and it should correspond to the value of `event_level`. Otherwise, there
#' should be as many columns as factor levels of `truth` and the ordering of
#' the columns should be the same as the factor levels of `truth`.
#'
#' @param estimator One of `"binary"`, `"macro"`, or `"macro_weighted"` to
#' specify the type of averaging to be done. `"binary"` is only relevant for
#' the two class case. The other two are general methods for calculating
#' multiclass metrics. The default will automatically choose `"binary"` or
#' `"macro"` based on `truth`.
#'
#' @seealso
#'
#' [pr_curve()] for computing the full precision recall curve.
#'
#' @author Max Kuhn
#'
#' @template examples-binary-prob
#' @template examples-multiclass-prob
#'
#' @export
pr_auc <- function(data, ...) {
  UseMethod("pr_auc")
}
pr_auc <- new_prob_metric(
  pr_auc,
  direction = "maximize"
)

#' @export
#' @rdname pr_auc
pr_auc.data.frame <- function(
  data,
  truth,
  ...,
  estimator = NULL,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL
) {
  prob_metric_summarizer(
    name = "pr_auc",
    fn = pr_auc_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname pr_auc
pr_auc_vec <- function(
  truth,
  estimate,
  estimator = NULL,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  ...
) {
  abort_if_class_pred(truth)

  estimator <- finalize_estimator(truth, estimator, "pr_auc")

  check_prob_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  pr_auc_estimator_impl(
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    event_level = event_level,
    case_weights = case_weights
  )
}

pr_auc_estimator_impl <- function(
  truth,
  estimate,
  estimator,
  event_level,
  case_weights
) {
  if (is_binary(estimator)) {
    pr_auc_binary(truth, estimate, event_level, case_weights)
  } else {
    # weights for macro / macro_weighted are based on truth frequencies
    # (this is the usual definition)
    truth_table <- yardstick_truth_table(truth, case_weights = case_weights)
    w <- get_weights(truth_table, estimator)
    out_vec <- pr_auc_multiclass(truth, estimate, case_weights)
    stats::weighted.mean(out_vec, w)
  }
}

# Don't remove NA values so any errors propagate
# (i.e. no if `truth` has no "events")
pr_auc_binary <- function(truth, estimate, event_level, case_weights) {
  curve <- pr_curve_vec(
    truth = truth,
    estimate = estimate,
    na_rm = TRUE,
    event_level = event_level,
    case_weights = case_weights
  )

  auc(
    x = curve[["recall"]],
    y = curve[["precision"]],
    na_rm = FALSE
  )
}

pr_auc_multiclass <- function(truth, estimate, case_weights) {
  results <- one_vs_all_impl(
    fn = pr_auc_binary,
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )

  vapply(results, FUN.VALUE = numeric(1), function(x) x)
}
