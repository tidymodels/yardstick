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
#' @inheritParams mn_log_loss
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
pr_curve.data.frame <- function(data,
                                truth,
                                ...,
                                na_rm = TRUE,
                                event_level = yardstick_event_level(),
                                case_weights = NULL) {
  estimate <- dots_to_estimate(data, !!! enquos(...))

  result <- metric_summarizer(
    metric_nm = "pr_curve",
    metric_fn = pr_curve_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    na_rm = na_rm,
    event_level = event_level,
    case_weights = !!enquo(case_weights)
  )

  curve_finalize(result, data, "pr_df", "grouped_pr_df")
}

# Undecided of whether to export this or not
pr_curve_vec <- function(truth,
                         estimate,
                         na_rm = TRUE,
                         event_level = yardstick_event_level(),
                         case_weights = NULL,
                         ...) {
  estimator <- finalize_estimator(truth, metric_class = "pr_curve")

  # `estimate` here is a matrix of class prob columns
  pr_curve_impl <- function(truth,
                            estimate,
                            ...,
                            case_weights = NULL) {
    check_dots_empty()

    pr_curve_estimator_impl(
      truth = truth,
      estimate = estimate,
      estimator = estimator,
      event_level = event_level,
      case_weights = case_weights
    )
  }

  metric_vec_template(
    metric_impl = pr_curve_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    case_weights = case_weights,
    cls = c("factor", "numeric")
  )
}

pr_curve_estimator_impl <- function(truth,
                                    estimate,
                                    estimator,
                                    event_level,
                                    case_weights) {
  if (is_binary(estimator)) {
    pr_curve_binary(truth, estimate, event_level, case_weights)
  } else {
    pr_curve_multiclass(truth, estimate, case_weights)
  }
}

pr_curve_binary <- function(truth,
                            estimate,
                            event_level,
                            case_weights) {
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

  if (is.null(case_weights)) {
    case_weights <- rep(1, times = length(truth))
  }

  truth <- unclass(truth)

  # Convert to `1 == event`, `0 == non-event`
  if (is_event_first(event_level)) {
    truth <- as.integer(truth == 1L)
  } else {
    truth <- as.integer(truth == 2L)
  }

  # Drop any `0` weights. These shouldn't affect the result.
  detect_zero_weight <- case_weights == 0
  if (any(detect_zero_weight)) {
    detect_non_zero_weight <- !detect_zero_weight
    truth <- truth[detect_non_zero_weight]
    estimate <- estimate[detect_non_zero_weight]
    case_weights <- case_weights[detect_non_zero_weight]
  }

  # Sort by decreasing `estimate`
  order <- order(estimate, decreasing = TRUE)
  truth <- truth[order]
  estimate <- estimate[order]
  case_weights <- case_weights[order]

  # Algorithm skips repeated probabilities.
  # We want the last duplicate to ensure that we capture all the events from the
  # `cumsum()`, so we use `fromLast`.
  loc_unique <- which(!duplicated(estimate, fromLast = TRUE))
  thresholds <- estimate[loc_unique]

  case_weights_events <- truth * case_weights
  case_weights_non_events <- (1 - truth) * case_weights

  if (sum(case_weights_events) == 0L) {
    warn("There are `0` event cases in `truth`, results will be meaningless.")
  }

  tp <- cumsum(case_weights_events)
  tp <- tp[loc_unique]

  fp <- cumsum(case_weights_non_events)
  fp <- fp[loc_unique]

  recall <- tp / tp[length(tp)]
  precision <- tp / (tp + fp)

  # First row always has `threshold = Inf`.
  # First recall is always `0`.
  # First precision is always `1`.
  thresholds <- c(Inf, thresholds)
  recall <- c(0, recall)
  precision <- c(1, precision)

  out <- list(
    .threshold = thresholds,
    recall = recall,
    precision = precision
  )

  dplyr::tibble(!!!out)
}

# One vs all approach
pr_curve_multiclass <- function(truth, estimate, case_weights) {
  one_vs_all_with_level_case_weights(
    metric_fn = pr_curve_binary,
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
