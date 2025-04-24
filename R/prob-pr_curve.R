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
#' @templateVar fn pr_curve
#' @template multiclass-curve
#' @template event_first
#'
#' @inheritParams pr_auc
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
#' @examplesIf rlang::is_installed(c("ggplot2"))
#' # ---------------------------------------------------------------------------
#' # `autoplot()`
#'
#' # Visualize the curve using ggplot2 manually
#' library(ggplot2)
#' library(dplyr)
#' pr_curve(two_class_example, truth, Class1) |>
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
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   pr_curve(obs, VF:L) |>
#'   autoplot()
#'
#' # Same as above, but will all of the resamples
#' hpc_cv |>
#'   group_by(Resample) |>
#'   pr_curve(obs, VF:L) |>
#'   autoplot()
#'
#' @export
#'
pr_curve <- function(data, ...) {
  UseMethod("pr_curve")
}

#' @export
#' @rdname pr_curve
pr_curve.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL
) {
  result <- curve_metric_summarizer(
    name = "pr_curve",
    fn = pr_curve_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    event_level = event_level,
    case_weights = !!enquo(case_weights)
  )

  curve_finalize(result, data, "pr_df", "grouped_pr_df")
}

# Undecided of whether to export this or not
pr_curve_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  ...
) {
  abort_if_class_pred(truth)

  estimator <- finalize_estimator(truth, metric_class = "pr_curve")

  check_prob_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    cli::cli_abort(
      c(
        x = "Missing values were detected and {.code na_ra = FALSE}.",
        i = "Not able to perform calculations."
      )
    )
  }

  pr_curve_estimator_impl(
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    event_level = event_level,
    case_weights = case_weights
  )
}

pr_curve_estimator_impl <- function(
  truth,
  estimate,
  estimator,
  event_level,
  case_weights
) {
  if (is_binary(estimator)) {
    pr_curve_binary(truth, estimate, event_level, case_weights)
  } else {
    pr_curve_multiclass(truth, estimate, case_weights)
  }
}

pr_curve_binary <- function(truth, estimate, event_level, case_weights) {
  # Algorithm modified from page 866 of
  # http://people.inf.elte.hu/kiss/12dwhdm/roc.pdf

  # P = #positives (sum of case weights when truth == event)
  # N = #elements (sum of case weights)
  #
  # At the start of the curve (we force this):
  # threshold = infinity
  # recall    = TP / P = 0, if P > 0
  # precision = TP / (TP + FP) = undefined b/c we haven't seen any values yet
  #             but we need to put 1 here so we can start the graph in the top
  #             left corner and compute PR AUC correctly
  #
  # At the end of the curve:
  # threshold = last estimate
  # recall    = TP / P = 1, P > 0
  # precision = TP / (TP + FP) = P / N

  curve <- binary_threshold_curve(
    truth = truth,
    estimate = estimate,
    event_level = event_level,
    case_weights = case_weights
  )

  threshold <- curve$threshold
  tp <- curve$tp
  fp <- curve$fp

  recall <- tp / tp[length(tp)]
  precision <- tp / (tp + fp)

  # First row always has `threshold = Inf`.
  # First recall is always `0`.
  # First precision is always `1`.
  threshold <- c(Inf, threshold)
  recall <- c(0, recall)
  precision <- c(1, precision)

  out <- list(
    .threshold = threshold,
    recall = recall,
    precision = precision
  )

  dplyr::tibble(!!!out)
}

# One vs all approach
pr_curve_multiclass <- function(truth, estimate, case_weights) {
  one_vs_all_with_level(
    fn = pr_curve_binary,
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )
}

# Dynamically exported
autoplot.pr_df <- function(object, ...) {
  `%+%` <- ggplot2::`%+%`

  # Base chart
  pr_chart <- ggplot2::ggplot(data = object)

  # Add in group interactions if required
  if (inherits(object, "grouped_pr_df")) {
    grps <- dplyr::groups(object)

    grps_chr <- paste0(dplyr::group_vars(object), collapse = "_")

    interact_expr <- list(
      color = expr(interaction(!!!grps, sep = "_"))
    )

    pr_chart <- pr_chart %+%
      ggplot2::labs(color = grps_chr)
  } else {
    interact_expr <- list()
  }

  # splice in the group interactions, or do nothing
  aes_spliced <- ggplot2::aes(
    x = recall,
    y = precision,
    !!!interact_expr
  )

  # build the graph
  pr_chart <- pr_chart %+%
    ggplot2::geom_path(mapping = aes_spliced) %+%
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) %+%
    ggplot2::coord_equal(ratio = 1) %+%
    ggplot2::theme_bw()

  # If we have .level, that means this was multiclass
  # and we want to show 1 vs all graphs
  if (".level" %in% colnames(object)) {
    pr_chart <- pr_chart %+%
      ggplot2::facet_wrap(~.level)
  }

  pr_chart
}
