#' Receiver operator curve
#'
#' `roc_curve()` constructs the full ROC curve and returns a
#' tibble. See [roc_auc()] for the area under the ROC curve.
#'
#' `roc_curve()` computes the sensitivity at every unique
#'  value of the probability column (in addition to infinity and
#'  minus infinity). If a smooth ROC curve was produced, the unique
#'  observed values of the specificity are used to create the curve
#'  points. In either case, this may not be efficient for large data
#'  sets.
#'
#'  There is a [ggplot2::autoplot()]
#'  method for quickly visualizing the curve. This works for
#'  binary and multiclass output, and also works with grouped data (i.e. from
#'  resamples). See the examples.
#'
#' @family curve metrics
#' @templateVar metric_fn roc_curve
#' @template multiclass-curve
#' @template event_first
#'
#' @inheritParams roc_auc
#' @param object The `roc_df` data frame returned from `roc_curve()`.
#'
#' @return
#' A tibble with class `roc_df` or `roc_grouped_df` having
#' columns `specificity` and `sensitivity`.
#'
#' If an ordinary (i.e. non-smoothed) curve
#' is used, there is also a column for `.threshold`.
#'
#' @seealso
#' Compute the area under the ROC curve with [roc_auc()].
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
#' roc_curve(two_class_example, truth, Class1) %>%
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
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   roc_curve(obs, VF:L) %>%
#'   autoplot()
#'
#' # Same as above, but will all of the resamples
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   roc_curve(obs, VF:L) %>%
#'   autoplot()
#' }
#'
#' @export
#'
roc_curve <- function(data, ...)
  UseMethod("roc_curve")

#' @export
#' @rdname roc_curve
#' @importFrom pROC coords
#' @importFrom rlang exec
#' @importFrom dplyr arrange as_tibble %>%
roc_curve.data.frame <- function (data,
                                  truth,
                                  ...,
                                  options = list(),
                                  na_rm = TRUE,
                                  event_level = yardstick_event_level()) {
  estimate <- dots_to_estimate(data, !!! enquos(...))

  result <- metric_summarizer(
    metric_nm = "roc_curve",
    metric_fn = roc_curve_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    na_rm = na_rm,
    event_level = event_level,
    metric_fn_options = list(options = options)
  )

  curve_finalize(result, data, "roc_df", "grouped_roc_df")
}

roc_curve_vec <- function(truth,
                          estimate,
                          options = list(),
                          na_rm = TRUE,
                          event_level = yardstick_event_level(),
                          ...) {
  estimator <- finalize_estimator(truth, metric_class = "roc_curve")

  # estimate here is a matrix of class prob columns
  roc_curve_impl <- function(truth, estimate, options = list()) {
    roc_curve_estimator_impl(truth, estimate, estimator, event_level, options)
  }

  metric_vec_template(
    metric_impl = roc_curve_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = c("factor", "numeric"),
    options = options
  )
}

roc_curve_estimator_impl <- function(truth, estimate, estimator, event_level, options) {
  if (is_binary(estimator)) {
    roc_curve_binary(truth, estimate, event_level, options)
  }
  else {
    roc_curve_multiclass(truth, estimate, options)
  }
}

roc_curve_binary <- function(truth, estimate, event_level, options) {
  lvls <- levels(truth)

  # pROC will actually expect the levels in the order of control, then event
  # and confusingly that aligns with our interpretation of event being
  # the first level
  if (is_event_first(event_level)) {
    lvls <- rev(lvls)
  }

  control <- lvls[[1]]
  event <- lvls[[2]]

  if (compute_n_occurrences(truth, control) == 0L) {
    stop_roc_truth_no_control(control)
  }
  if (compute_n_occurrences(truth, event) == 0L) {
    stop_roc_truth_no_event(event)
  }

  # working on a better way of doing this
  options$response <- truth
  options$predictor <- estimate
  options$levels <- lvls
  options$quiet <- TRUE
  options$direction <- "<"

  curv <- exec(pROC::roc, !!!options)

  if (!inherits(curv, "smooth.roc")) {
    res <- coords(
      curv,
      x = unique(c(-Inf, options$predictor, Inf)),
      input = "threshold",
      transpose = FALSE
    )
  }
  else {
    res <- coords(
      curv,
      x = unique(c(0, curv$specificities, 1)),
      input = "specificity",
      transpose = FALSE
    )
  }

  res <- dplyr::as_tibble(res)

  if (!inherits(curv, "smooth.roc")) {
    res <- dplyr::arrange(res, threshold)
    res <- dplyr::rename(res, .threshold = threshold)
  }
  else {
    res <- dplyr::arrange(res, specificity)
  }

  res
}

# One-VS-All approach
roc_curve_multiclass <- function(truth, estimate, options) {
  one_vs_all_with_level(roc_curve_binary, truth, estimate, options = options)
}


# Dynamically exported
#' @rdname roc_curve
autoplot.roc_df <- function(object, ...) {

  `%+%` <- ggplot2::`%+%`

  # Base chart
  roc_chart <- ggplot2::ggplot(data = object)

  # Add in group interactions if required
  if (inherits(object, "grouped_roc_df")) {

    grps <- dplyr::groups(object)

    grps_chr <- paste0(dplyr::group_vars(object), collapse = "_")

    interact_expr <- list(
      color = rlang::expr(interaction(!!! grps, sep = "_"))
    )

    roc_chart <- roc_chart %+%
      ggplot2::labs(color = grps_chr)

  }
  else {

    interact_expr <- list()

  }

  # splice in the group interactions, or do nothing
  aes_spliced <- ggplot2::aes(
    x = 1 - specificity,
    y = sensitivity,
    !!! interact_expr
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
