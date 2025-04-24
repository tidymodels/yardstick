#' Gain curve
#'
#' `gain_curve()` constructs the full gain curve and returns a tibble. See
#' [gain_capture()] for the relevant area under the gain curve. Also see
#' [lift_curve()] for a closely related concept.
#'
#' There is a [ggplot2::autoplot()] method for quickly visualizing the curve.
#' This works for binary and multiclass output, and also works with grouped data
#' (i.e. from resamples). See the examples.
#'
#' The greater the area between the gain curve and the baseline, the better the
#' model.
#'
#' Gain curves are identical to CAP curves (cumulative accuracy profile). See
#' the Engelmann reference for more information on CAP curves.
#'
#' @section Gain and Lift Curves:
#'
#' The motivation behind cumulative gain and lift charts is as a visual method to
#' determine the effectiveness of a model when compared to the results one
#' might expect without a model. As an example, without a model, if you were
#' to advertise to a random 10% of your customer base, then you might expect
#' to capture 10% of the of the total number of positive responses had you
#' advertised to your entire customer base. Given a model that predicts
#' which customers are more likely to respond, the hope is that you can more
#' accurately target 10% of your customer base and capture
#' `>`10% of the total number of positive responses.
#'
#' The calculation to construct gain curves is as follows:
#'
#' 1. `truth` and `estimate` are placed in descending order by the `estimate`
#' values (`estimate` here is a single column supplied in `...`).
#'
#' 2. The cumulative number of samples with true results relative to the
#' entire number of true results are found. This is the y-axis in a gain chart.
#'
#' @family curve metrics
#' @templateVar fn gain_curve
#' @template multiclass-curve
#' @template event_first
#'
#' @inheritParams pr_auc
#'
#' @return
#' A tibble with class `gain_df` or `gain_grouped_df` having columns:
#'
#' - `.n` The index of the current sample.
#'
#' - `.n_events` The index of the current _unique_ sample. Values with repeated
#' `estimate` values are given identical indices in this column.
#'
#' - `.percent_tested` The cumulative percentage of values tested.
#'
#' - `.percent_found` The cumulative percentage of true results relative to the
#' total number of true results.
#'
#' If using the `case_weights` argument, all of the above columns will be
#' weighted. This makes the most sense with frequency weights, which are integer
#' weights representing the number of times a particular observation should be
#' repeated.
#'
#' @references
#'
#' Engelmann, Bernd & Hayden, Evelyn & Tasche, Dirk (2003).
#' "Measuring the Discriminative Power of Rating Systems,"
#' Discussion Paper Series 2: Banking and Financial Studies 2003,01,
#' Deutsche Bundesbank.
#'
#' @seealso
#' Compute the relevant area under the gain curve with [gain_capture()].
#'
#' @author Max Kuhn
#'
#' @template examples-binary-prob
#' @examplesIf rlang::is_installed(c("ggplot2"))
#' # ---------------------------------------------------------------------------
#' # `autoplot()`
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' # Use autoplot to visualize
#' # The top left hand corner of the grey triangle is a "perfect" gain curve
#' autoplot(gain_curve(two_class_example, truth, Class1))
#'
#' # Multiclass one-vs-all approach
#' # One curve per level
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   gain_curve(obs, VF:L) |>
#'   autoplot()
#'
#' # Same as above, but will all of the resamples
#' # The resample with the minimum (farthest to the left) "perfect" value is
#' # used to draw the shaded region
#' hpc_cv |>
#'   group_by(Resample) |>
#'   gain_curve(obs, VF:L) |>
#'   autoplot()
#'
#' @export
#'
gain_curve <- function(data, ...) {
  UseMethod("gain_curve")
}

#' @rdname gain_curve
#' @export
gain_curve.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL
) {
  result <- curve_metric_summarizer(
    name = "gain_curve",
    fn = gain_curve_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    event_level = event_level,
    case_weights = !!enquo(case_weights)
  )

  curve_finalize(result, data, "gain_df", "grouped_gain_df")
}

# dont export gain_curve_vec / lift_curve_vec
# not as meaningful to return a list of vectors
# maybe it could return the tibble?
gain_curve_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  ...
) {
  abort_if_class_pred(truth)

  estimator <- finalize_estimator(truth, metric_class = "gain_curve")

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

  gain_curve_estimator_impl(
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    event_level = event_level,
    case_weights = case_weights
  )
}

gain_curve_estimator_impl <- function(
  truth,
  estimate,
  estimator,
  event_level,
  case_weights
) {
  if (is_binary(estimator)) {
    gain_curve_binary(truth, estimate, event_level, case_weights)
  } else {
    gain_curve_multiclass(truth, estimate, case_weights)
  }
}

gain_curve_binary <- function(truth, estimate, event_level, case_weights) {
  gain_list <- gain_curve_binary_impl(
    truth,
    estimate,
    event_level,
    case_weights
  )
  dplyr::tibble(!!!gain_list)
}

gain_curve_multiclass <- function(truth, estimate, case_weights) {
  one_vs_all_with_level(
    fn = gain_curve_binary,
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )
}

# Following the Example Problem 2 of:
# http://www2.cs.uregina.ca/~dbd/cs831/notes/lift_chart/lift_chart.html
gain_curve_binary_impl <- function(truth, estimate, event_level, case_weights) {
  truth <- unclass(truth)

  # Events are re-coded as 1, non-events are 0. Easier for cumulative calcs.
  if (is_event_first(event_level)) {
    truth <- as.integer(truth == 1L)
  } else {
    truth <- as.integer(truth == 2L)
  }

  if (is.null(case_weights)) {
    case_weights <- rep(1, times = length(truth))
  }
  case_weights <- vec_cast(case_weights, to = double())

  # arrange in decreasing order by class probability score
  estimate_ord <- order(estimate, decreasing = TRUE)
  estimate <- estimate[estimate_ord]
  truth <- truth[estimate_ord]
  case_weights <- case_weights[estimate_ord]

  case_weights_event <- ifelse(truth, case_weights, 0)

  n_events <- sum(case_weights_event)
  n_predictions <- sum(case_weights)

  # uninformative model x and y axis
  # this is also the x axis in the gain / lift charts
  cumulative_tested <- cumsum(case_weights)
  cumulative_percent_tested <- (cumulative_tested / n_predictions) * 100

  cumulative_found <- cumsum(case_weights_event)
  cumulative_percent_found <- (cumulative_found / n_events) * 100

  # remove all but the last of any duplicated estimates
  # doing this after the fact allows us to still use cumsum()
  where_dups <- duplicated(estimate, fromLast = TRUE)
  cumulative_found <- cumulative_found[!where_dups]
  cumulative_tested <- cumulative_tested[!where_dups]
  cumulative_percent_tested <- cumulative_percent_tested[!where_dups]
  cumulative_percent_found <- cumulative_percent_found[!where_dups]

  # edge values
  cumulative_found <- c(0, cumulative_found)
  cumulative_tested <- c(0, cumulative_tested)
  cumulative_percent_tested <- c(0, cumulative_percent_tested)
  cumulative_percent_found <- c(0, cumulative_percent_found)

  list(
    .n = cumulative_tested,
    .n_events = cumulative_found,
    .percent_tested = cumulative_percent_tested,
    .percent_found = cumulative_percent_found
  )
}

# autoplot ---------------------------------------------------------------------

# dynamically exported in .onLoad()

autoplot.gain_df <- function(object, ...) {
  `%+%` <- ggplot2::`%+%`

  # Base chart
  chart <- ggplot2::ggplot(data = object)

  # Grouped specific chart features
  if (dplyr::is_grouped_df(object)) {
    # Construct the color interaction group
    grps <- dplyr::groups(object)
    interact_expr <- list(
      color = expr(interaction(!!!grps, sep = "_"))
    )

    # Add group legend label
    grps_chr <- paste0(dplyr::group_vars(object), collapse = "_")
    chart <- chart %+%
      ggplot2::labs(color = grps_chr)
  } else {
    interact_expr <- list()
  }

  # Generic enough to be used in the pipe chain
  # for multiclass and binary curves
  maybe_group_by_level <- function(object, with_old = TRUE) {
    if (with_old) {
      grps <- dplyr::groups(object)
    } else {
      grps <- list()
    }

    if (".level" %in% colnames(object)) {
      # .level should be the first group b/c of how summarise()
      # drops a group
      object <- dplyr::group_by(object, .level, !!!grps)
    }
    object
  }

  # Construct poly_data
  # If grouped (ie resamples), we take the min of all "perfect" values
  #   to ensure we capture all lines in the polygon
  # If multiclass, we calculate each level separately
  poly_data <- object |>
    maybe_group_by_level() |>
    dplyr::summarise(slope = 1 / (max(.n_events) / dplyr::last(.n))) |>
    dplyr::mutate(perfect = 100 / slope) |>
    maybe_group_by_level(with_old = FALSE) |>
    dplyr::summarise(perfect = min(perfect)) |>
    maybe_group_by_level() |>
    dplyr::do(
      dplyr::tibble(
        x = c(0, .$perfect, 100),
        y = c(0, 100, 100)
      )
    )

  # Avoid cran check for "globals"
  .percent_tested <- as.name(".percent_tested")
  .percent_found <- as.name(".percent_found")
  x <- as.name("x")
  y <- as.name("y")

  chart <- chart %+%

    # boundary poly
    ggplot2::geom_polygon(
      mapping = ggplot2::aes(
        x = !!x,
        y = !!y
      ),
      data = poly_data,
      # fill
      fill = "lightgrey",
      alpha = 0.4
    ) %+%

    # gain curve
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = !!.percent_tested,
        y = !!.percent_found,
        !!!interact_expr
      ),
      data = object
    ) %+%

    ggplot2::labs(
      x = "% Tested",
      y = "% Found"
    ) %+%

    ggplot2::theme_bw()

  # facet by .level if this was a multiclass computation
  if (".level" %in% colnames(object)) {
    chart <- chart %+%
      ggplot2::facet_wrap(~.level)
  }

  chart
}
