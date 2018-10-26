#' Precision recall curve
#'
#' `pr_curve()` constructs the full precision recall curve and returns a
#' tibble. `pr_auc()` is a metric that computes the area under the precision
#' recall curve.
#'
#' For `pr_curve()`, if a multiclass `truth` column is provided, a one-vs-all
#' approach will be taken to calculate multiple curves, one per level.
#' In this case, there will be an additional column, `.level`,
#' identifying the "one" column in the one-vs-all calculation.
#'
#' @family class probability metrics
#' @aliases pr_curve
#' @templateVar metric_fn pr_auc
#' @template return
#' @template multiclass-prob
#'
#' @inheritParams sens
#'
#' @param data A `data.frame` containing the `truth` and `estimate`
#' columns.
#'
#' @param estimate If `truth` is binary, a numeric vector of class probabilities
#' corresponding to the "relevant" class. Otherwise, a matrix with as many
#' columns as factor levels of `truth`.
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
#' @param object The data frame returned from `pr_curve()`.
#'
#' @return
#' For `pr_curve()`, a tibble with class `pr_df` or `pr_grouped_df` having
#' columns `recall`, `precision`, and `.threshold`.
#'
#' @author Max Kuhn
#'
#' @examples
#' # PR Curve examples ---------------------------------------------------------
#' library(ggplot2)
#'
#' # Two class
#' pr_curve(two_class_example, truth, Class1) %>%
#'   ggplot(aes(x = recall, y = precision)) +
#'   geom_path() +
#'   coord_equal() +
#'   theme_bw()
#'
#' # Or use autoplot
#' autoplot(pr_curve(two_class_example, truth, Class1))
#'
#' # Multiclass one-vs-all approach
#' # One curve per level
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   pr_curve(obs, VF:L) %>%
#'   autoplot()
#'
#' # Same as above, but will all of the resamples
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   pr_curve(obs, VF:L) %>%
#'   autoplot()
#'
#' # PR AUC examples -----------------------------------------------------------
#'
#' @template examples-prob
#'
#' @export
#'
#' @name pr_curve
NULL

# PR Curve ---------------------------------------------------------------------

#' @export
#' @rdname pr_curve
pr_curve <- function(data, ...) {
  UseMethod("pr_curve")
}

#' @export
#' @rdname pr_curve
#' @importFrom stats relevel
pr_curve.data.frame <- function(data, truth, ..., na.rm = TRUE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))
  truth <- enquo(truth)

  validate_not_missing(truth, "truth")

  # Explicit handling of length 1 character vectors as column names
  truth <- handle_chr_names(truth)

  res <- dplyr::do(
    data,
    pr_curve_vec(
      truth = rlang::eval_tidy(truth, data = .),
      estimate = rlang::eval_tidy(estimate, data = .),
      na.rm = na.rm
    )
  )

  if (dplyr::is_grouped_df(res)) {
    class(res) <- c("grouped_pr_df", "pr_df", class(res))
  }
  else {
    class(res) <- c("pr_df", class(res))
  }

  res
}

# Undecided of whether to export this or not
pr_curve_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, metric_class = "pr_curve")

  # estimate here is a matrix of class prob columns
  pr_curve_impl <- function(truth, estimate) {
    pr_curve_estimator_impl(truth, estimate, estimator)
  }

  metric_vec_template(
    metric_impl = pr_curve_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    estimator = estimator,
    cls = c("factor", "numeric"),
    ...
  )

}

pr_curve_estimator_impl <- function(truth, estimate, estimator) {

  if (is_binary(estimator)) {
    pr_curve_binary(truth, estimate)
  }
  else {
    pr_curve_multiclass(truth, estimate)
  }

}

pr_curve_binary <- function(truth, estimate) {

  lvls <- levels(truth)

  # Relevel if event_first = FALSE
  # The second level becomes the first so as.integer()
  # holds the 1s and 2s in the correct slot
  if (!getOption("yardstick.event_first")) {
    truth <- relevel(truth, lvls[2])
  }

  # quicker to convert to integer now rather than letting rcpp do it
  # 1=good, 2=bad
  truth <- as.integer(truth)

  pr_list <- pr_curve_cpp(truth, estimate)

  dplyr::tibble(!!!pr_list)
}

# One vs all approach
pr_curve_multiclass <- function(truth, estimate) {
  one_vs_all_with_level(pr_curve_binary, truth, estimate)
}

# PR AUC -----------------------------------------------------------------------

#' @export
#' @rdname pr_curve
pr_auc <- function(data, ...) {
  UseMethod("pr_auc")
}

class(pr_auc) <- c("prob_metric", "function")

#' @export
#' @rdname pr_curve
pr_auc.data.frame  <- function(data, truth, ...,
                               estimator = NULL,
                               na.rm = TRUE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "pr_auc",
    metric_fn = pr_auc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    estimator = estimator,
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname pr_curve
pr_auc_vec <- function(truth, estimate,
                       estimator = NULL, na.rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator, "pr_auc")

  pr_auc_impl <- function(truth, estimate) {
    pr_auc_estimator_impl(truth, estimate, estimator)
  }

  metric_vec_template(
    metric_impl = pr_auc_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
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

pr_auc_binary <- function(truth, estimate) {
  pr_list <- pr_curve_vec(truth, estimate)
  auc(pr_list[["recall"]], pr_list[["precision"]])
}

pr_auc_multiclass <- function(truth, estimate) {
  res_lst <- one_vs_all_impl(pr_auc_binary, truth, estimate)
  rlang::flatten_dbl(res_lst)
}
