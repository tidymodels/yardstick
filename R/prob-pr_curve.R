#' Precision recall curve
#'
#' `pr_curve()` constructs the full precision recall curve and returns a
#' tibble. See [pr_auc()] for the area under the precision recall curve.
#'
#' `pr_curve()` computes the precision at every unique value of the
#'  probability column (in addition to infinity).
#'
#'  There is a [ggplot2::autoplot()]
#'  method for quickly visualizing the curve. This works for
#'  binary and multiclass output, and also works with grouped data (i.e. from
#'  resamples). See the examples.
#'
#' @family curve metrics
#' @templateVar metric_fn pr_curve
#' @template multiclass-curve
#' @template event_first
#'
#' @inheritParams pr_auc
#' @param object The `pr_df` data frame returned from `pr_curve()`.
#'
#' @return
#' A tibble with class `pr_df` or `pr_grouped_df` having
#' columns `.threshold`, `recall`, and `precision`.
#'
#' @seealso
#' Compute the area under the precision recall curve with [pr_auc()].
#'
#' @author Max Kuhn
#' @template examples-binary-prob
#' @examples
#' # ---------------------------------------------------------------------------
#' # `autoplot()`
#'
#' # Visualize the curve using ggplot2 manually
#' library(ggplot2)
#' library(dplyr)
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
#' @export
#'
pr_curve <- function(data, ...) {
  UseMethod("pr_curve")
}

#' @export
#' @rdname pr_curve
#' @importFrom stats relevel
pr_curve.data.frame <- function(data,
                                truth,
                                ...,
                                na_rm = TRUE,
                                event_level = yardstick_event_level()) {
  estimate <- dots_to_estimate(data, !!! enquos(...))

  result <- metric_summarizer(
    metric_nm = "pr_curve",
    metric_fn = pr_curve_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    na_rm = na_rm,
    event_level = event_level
  )

  curve_finalize(result, data, "pr_df", "grouped_pr_df")
}

# Undecided of whether to export this or not
pr_curve_vec <- function(truth,
                         estimate,
                         na_rm = TRUE,
                         event_level = yardstick_event_level(),
                         ...) {
  estimator <- finalize_estimator(truth, metric_class = "pr_curve")

  # `estimate` here is a matrix of class prob columns
  pr_curve_impl <- function(truth, estimate) {
    pr_curve_estimator_impl(truth, estimate, estimator, event_level)
  }

  metric_vec_template(
    metric_impl = pr_curve_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = c("factor", "numeric")
  )

}

pr_curve_estimator_impl <- function(truth, estimate, estimator, event_level) {
  if (is_binary(estimator)) {
    pr_curve_binary(truth, estimate, event_level)
  }
  else {
    pr_curve_multiclass(truth, estimate)
  }
}

pr_curve_binary <- function(truth, estimate, event_level) {
  lvls <- levels(truth)

  # Relevel if `event_level = "second"`
  # The second level becomes the first so as.integer()
  # holds the 1s and 2s in the correct slot
  if (!is_event_first(event_level)) {
    truth <- relevel(truth, lvls[2])
  }

  # Quicker to convert to integer now
  # 1 = event, 2 = non-event
  truth <- as.integer(truth)

  # Sort at the R level
  ord <- order(estimate, decreasing = TRUE)
  truth <- truth[ord]
  estimate <- estimate[ord]

  # Algorithm skips repeated probabilities.
  # Call unique() from the R level.
  thresholds <- unique(estimate)

  # First row always has `threshold = Inf`, we handle first
  # row of `recall` and `precision` at the C level
  thresholds <- c(Inf,  thresholds)

  pr_list <- pr_curve_binary_impl(truth, estimate, thresholds)

  dplyr::tibble(!!!pr_list)
}

pr_curve_binary_impl <- function(truth, estimate, thresholds) {
  .Call(yardstick_pr_curve_binary_impl, truth, estimate, thresholds)
}

# One vs all approach
pr_curve_multiclass <- function(truth, estimate) {
  one_vs_all_with_level(pr_curve_binary, truth, estimate)
}


# Dynamically exported
#' @rdname pr_curve
autoplot.pr_df <- function(object, ...) {

  `%+%` <- ggplot2::`%+%`

  # Base chart
  pr_chart <- ggplot2::ggplot(data = object)

  # Add in group interactions if required
  if (inherits(object, "grouped_pr_df")) {

    grps <- dplyr::groups(object)

    grps_chr <- paste0(dplyr::group_vars(object), collapse = "_")

    interact_expr <- list(
      color = rlang::expr(interaction(!!! grps, sep = "_"))
    )

    pr_chart <- pr_chart %+%
      ggplot2::labs(color = grps_chr)

  }
  else {

    interact_expr <- list()

  }

  # splice in the group interactions, or do nothing
  aes_spliced <- ggplot2::aes(
    x = recall,
    y = precision,
    !!! interact_expr
  )

  # build the graph
  pr_chart <- pr_chart %+%
    ggplot2::geom_path(mapping = aes_spliced) %+%
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) %+%
    ggplot2::coord_equal(ratio = .75) %+%
    ggplot2::theme_bw()

  # If we have .level, that means this was multiclass
  # and we want to show 1 vs all graphs
  if (".level" %in% colnames(object)) {
    pr_chart <- pr_chart %+%
      ggplot2::facet_wrap(~.level)
  }

  pr_chart
}
