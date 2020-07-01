#' Area under the precision recall curve
#'
#' `pr_auc()` is a metric that computes the area under the precision
#' recall curve. See [pr_curve()] for the full curve.
#'
#'
#' @family class probability metrics
#' @templateVar metric_fn pr_auc
#' @template return
#' @template multiclass-prob
#' @template event_first
#'
#' @inheritParams sens
#'
#' @param data A `data.frame` containing the `truth` and `estimate`
#' columns.
#'
#' @param estimate If `truth` is binary, a numeric vector of class probabilities
#' corresponding to the "relevant" class. Otherwise, a matrix with as many
#' columns as factor levels of `truth`. _It is assumed that these are in the
#' same order as the levels of `truth`._
#'
#' @param ... A set of unquoted column names or one or more
#' `dplyr` selector functions to choose which variables contain the
#' class probabilities. If `truth` is binary, only 1 column should be selected.
#' Otherwise, there should be as many columns as factor levels of `truth`.
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
pr_auc.data.frame  <- function(data, truth, ...,
                               estimator = NULL,
                               na_rm = TRUE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "pr_auc",
    metric_fn = pr_auc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    estimator = estimator,
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
#' @rdname pr_auc
pr_auc_vec <- function(truth, estimate,
                       estimator = NULL, na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator, "pr_auc")

  pr_auc_impl <- function(truth, estimate) {
    pr_auc_estimator_impl(truth, estimate, estimator)
  }

  metric_vec_template(
    metric_impl = pr_auc_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = c("factor", "numeric"),
    ...
  )

}

pr_auc_estimator_impl <- function(truth, estimate, estimator) {

  if (is_binary(estimator)) {
    pr_auc_binary(truth, estimate)
  }
  else {
    # weights for macro / macro_weighted are based on truth frequencies
    # (this is the usual definition)
    truth_table <- matrix(table(truth), nrow = 1)
    w <- get_weights(truth_table, estimator)
    out_vec <- pr_auc_multiclass(truth, estimate)
    weighted.mean(out_vec, w)
  }

}

# Don't remove NA values so any errors propagate
# (i.e. no if `truth` has no "events")
pr_auc_binary <- function(truth, estimate) {
  pr_list <- pr_curve_vec(truth, estimate)
  auc(pr_list[["recall"]], pr_list[["precision"]], na_rm = FALSE)
}

pr_auc_multiclass <- function(truth, estimate) {
  res_lst <- one_vs_all_impl(pr_auc_binary, truth, estimate)
  rlang::flatten_dbl(res_lst)
}
