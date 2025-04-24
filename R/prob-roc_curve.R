#' Receiver operator curve
#'
#' @description
#' `roc_curve()` constructs the full ROC curve and returns a
#' tibble. See [roc_auc()] for the area under the ROC curve.
#'
#' @details
#' `roc_curve()` computes the sensitivity at every unique
#'  value of the probability column (in addition to infinity and
#'  minus infinity).
#'
#'  There is a [ggplot2::autoplot()] method for quickly visualizing the curve.
#'  This works for binary and multiclass output, and also works with grouped
#'  data (i.e. from resamples). See the examples.
#'
#' @family curve metrics
#' @templateVar fn roc_curve
#' @template multiclass-curve
#' @template event_first
#'
#' @inheritParams roc_auc
#'
#' @return
#' A tibble with class `roc_df` or `roc_grouped_df` having
#' columns `.threshold`, `specificity`, and `sensitivity`.
#'
#' @seealso
#' Compute the area under the ROC curve with [roc_auc()].
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
#' roc_curve(two_class_example, truth, Class1) |>
#'   ggplot(aes(x = 1 - specificity, y = sensitivity)) +
#'   geom_path() +
#'   geom_abline(lty = 3) +
#'   coord_equal() +
#'   theme_bw()
#'
#' # Or use autoplot
#' autoplot(roc_curve(two_class_example, truth, Class1))
#'
#' \dontrun{
#'
#' # Multiclass one-vs-all approach
#' # One curve per level
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   roc_curve(obs, VF:L) |>
#'   autoplot()
#'
#' # Same as above, but will all of the resamples
#' hpc_cv |>
#'   group_by(Resample) |>
#'   roc_curve(obs, VF:L) |>
#'   autoplot()
#' }
#'
#' @export
roc_curve <- function(data, ...) {
  UseMethod("roc_curve")
}

#' @export
#' @rdname roc_curve
roc_curve.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  options = list()
) {
  check_roc_options_deprecated("roc_curve", options)

  result <- curve_metric_summarizer(
    name = "roc_curve",
    fn = roc_curve_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    event_level = event_level,
    case_weights = !!enquo(case_weights)
  )

  curve_finalize(result, data, "roc_df", "grouped_roc_df")
}

roc_curve_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  ...
) {
  abort_if_class_pred(truth)

  estimator <- finalize_estimator(truth, metric_class = "roc_curve")

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

  # estimate here is a matrix of class prob columns
  roc_curve_estimator_impl(
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    event_level = event_level,
    case_weights = case_weights
  )
}

roc_curve_estimator_impl <- function(
  truth,
  estimate,
  estimator,
  event_level,
  case_weights,
  call = caller_env()
) {
  if (is_binary(estimator)) {
    roc_curve_binary(truth, estimate, event_level, case_weights, call)
  } else {
    roc_curve_multiclass(truth, estimate, case_weights, call)
  }
}

roc_curve_binary <- function(truth, estimate, event_level, case_weights, call) {
  lvls <- levels(truth)

  if (!is_event_first(event_level)) {
    lvls <- rev(lvls)
  }

  event <- lvls[[1]]
  control <- lvls[[2]]

  if (compute_n_occurrences(truth, event) == 0L) {
    stop_roc_truth_no_event(event, call)
  }
  if (compute_n_occurrences(truth, control) == 0L) {
    stop_roc_truth_no_control(control)
  }

  curve <- binary_threshold_curve(
    truth = truth,
    estimate = estimate,
    event_level = event_level,
    case_weights = case_weights
  )

  threshold <- curve$threshold
  tp <- curve$tp
  fp <- curve$fp

  tpr <- tp / tp[length(tp)]
  fpr <- fp / fp[length(fp)]

  sensitivity <- tpr
  specificity <- 1 - fpr

  # In order of increasing specificity
  threshold <- rev(threshold)
  sensitivity <- rev(sensitivity)
  specificity <- rev(specificity)

  # Add first/last rows to the data frame to ensure the curve and
  # AUC metrics are computed correctly
  threshold <- c(-Inf, threshold, Inf)
  sensitivity <- c(1, sensitivity, 0)
  specificity <- c(0, specificity, 1)

  dplyr::tibble(
    .threshold = threshold,
    specificity = specificity,
    sensitivity = sensitivity
  )
}

# One-VS-All approach
roc_curve_multiclass <- function(truth, estimate, case_weights, call) {
  one_vs_all_with_level(
    fn = roc_curve_binary,
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )
}

check_roc_options_deprecated <- function(what, options) {
  if (!identical(options, list())) {
    warn_roc_options_deprecated(what)
  }
}
warn_roc_options_deprecated <- function(what) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = I(sprintf("The `options` argument of `%s()`", what)),
    details = paste(
      "This argument no longer has any effect, and is being ignored.",
      "Use the pROC package directly if you need these features."
    )
  )
}

# Dynamically exported
autoplot.roc_df <- function(object, ...) {
  `%+%` <- ggplot2::`%+%`

  # Base chart
  roc_chart <- ggplot2::ggplot(data = object)

  # Add in group interactions if required
  if (inherits(object, "grouped_roc_df")) {
    grps <- dplyr::groups(object)

    grps_chr <- paste0(dplyr::group_vars(object), collapse = "_")

    interact_expr <- list(
      color = expr(interaction(!!!grps, sep = "_"))
    )

    roc_chart <- roc_chart %+%
      ggplot2::labs(color = grps_chr)
  } else {
    interact_expr <- list()
  }

  # splice in the group interactions, or do nothing
  aes_spliced <- ggplot2::aes(
    x = 1 - specificity,
    y = sensitivity,
    !!!interact_expr
  )

  # build the graph
  roc_chart <- roc_chart %+%
    ggplot2::geom_path(mapping = aes_spliced) %+%
    ggplot2::geom_abline(lty = 3) %+%
    ggplot2::coord_equal() %+%
    ggplot2::theme_bw()

  # If we have .level, that means this was multiclass
  # and we want to show 1 vs all graphs
  if (".level" %in% colnames(object)) {
    roc_chart <- roc_chart %+%
      ggplot2::facet_wrap(~.level)
  }

  roc_chart
}
