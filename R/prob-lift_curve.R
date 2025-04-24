#' Lift curve
#'
#' `lift_curve()` constructs the full lift curve and returns a tibble. See
#' [gain_curve()] for a closely related concept.
#'
#' There is a [ggplot2::autoplot()] method for quickly visualizing the curve.
#' This works for binary and multiclass output, and also works with grouped data
#' (i.e. from resamples). See the examples.
#'
#' @section Gain and Lift Curves:
#'
#' The motivation behind cumulative gain and lift charts is as a visual method
#' to determine the effectiveness of a model when compared to the results one
#' might expect without a model. As an example, without a model, if you were to
#' advertise to a random 10% of your customer base, then you might expect to
#' capture 10% of the of the total number of positive responses had you
#' advertised to your entire customer base. Given a model that predicts which
#' customers are more likely to respond, the hope is that you can more
#' accurately target 10% of your customer base and capture `>`10% of the total
#' number of positive responses.
#'
#' The calculation to construct lift curves is as follows:
#'
#' 1. `truth` and `estimate` are placed in descending order by the `estimate`
#' values (`estimate` here is a single column supplied in `...`).
#'
#' 2. The cumulative number of samples with true results relative to the
#' entire number of true results are found.
#'
#' 3. The cumulative `%` found is divided by the cumulative `%` tested
#' to construct the lift value. This ratio represents the factor of improvement
#' over an uninformed model. Values `>`1 represent a valuable model. This is the
#' y-axis of the lift chart.
#'
#' @family curve metrics
#' @templateVar fn lift_curve
#' @template multiclass-curve
#' @template event_first
#'
#' @inheritParams pr_auc
#'
#' @return
#' A tibble with class `lift_df` or `lift_grouped_df` having
#' columns:
#'
#' - `.n` The index of the current sample.
#'
#' - `.n_events` The index of the current _unique_ sample. Values with repeated
#' `estimate` values are given identical indices in this column.
#'
#' - `.percent_tested` The cumulative percentage of values tested.
#'
#' - `.lift` First calculate the cumulative percentage of true results relative
#' to the total number of true results. Then divide that by `.percent_tested`.
#'
#' If using the `case_weights` argument, all of the above columns will be
#' weighted. This makes the most sense with frequency weights, which are integer
#' weights representing the number of times a particular observation should be
#' repeated.
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
#' autoplot(lift_curve(two_class_example, truth, Class1))
#'
#' # Multiclass one-vs-all approach
#' # One curve per level
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   lift_curve(obs, VF:L) |>
#'   autoplot()
#'
#' # Same as above, but will all of the resamples
#' hpc_cv |>
#'   group_by(Resample) |>
#'   lift_curve(obs, VF:L) |>
#'   autoplot()
#'
#' @export
#'
lift_curve <- function(data, ...) {
  UseMethod("lift_curve")
}

#' @rdname lift_curve
#' @export
lift_curve.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL
) {
  result <- curve_metric_summarizer(
    name = "lift_curve",
    fn = lift_curve_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    event_level = event_level,
    case_weights = !!enquo(case_weights)
  )

  curve_finalize(result, data, "lift_df", "grouped_lift_df")
}

lift_curve_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  ...
) {
  # Doesn't validate inputs here since it is done in gain_curve_vec()

  # tibble result, possibly grouped
  res <- gain_curve_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    event_level = event_level,
    case_weights = case_weights
  )

  if (identical(res, NA_real_)) {
    return(res)
  }

  res <- dplyr::mutate(res, .lift = .percent_found / .percent_tested)

  res[[".percent_found"]] <- NULL

  res
}

# autoplot ---------------------------------------------------------------------

# dynamically exported in .onLoad()

autoplot.lift_df <- function(object, ...) {
  `%+%` <- ggplot2::`%+%`

  # Remove data before first event (is this okay?)
  object <- dplyr::filter(object, .n_events > 0)

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

  baseline <- data.frame(
    x = c(0, 100),
    y = c(1, 1)
  )

  # Avoid cran check for "globals"
  .percent_tested <- as.name(".percent_tested")
  .lift <- as.name(".lift")
  x <- as.name("x")
  y <- as.name("y")

  chart <- chart %+%

    # gain curve
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = !!.percent_tested,
        y = !!.lift,
        !!!interact_expr
      ),
      data = object
    ) %+%

    # baseline
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = !!x,
        y = !!y
      ),
      data = baseline,
      colour = "grey60",
      linetype = 2
    ) %+%

    ggplot2::labs(
      x = "% Tested",
      y = "Lift"
    ) %+%

    ggplot2::theme_bw()

  # facet by .level if this was a multiclass computation
  if (".level" %in% colnames(object)) {
    chart <- chart %+%
      ggplot2::facet_wrap(~.level)
  }

  chart
}
