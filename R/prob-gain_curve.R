#' Gain curve
#'
#' `gain_curve()` constructs the full gain curve and returns a
#' tibble. See [gain_capture()] for the relevant area under the gain curve.
#' Also see [lift_curve()] for a closely related concept.
#'
#' There is a [ggplot2::autoplot()]
#' method for quickly visualizing the curve. This works for
#' binary and multiclass output, and also works with grouped data (i.e. from
#' resamples). See the examples.
#'
#' The greater the area between the gain curve and the baseline, the better
#' the model.
#'
#' Gain curves are identical to CAP curves (cumulative accuracy profile).
#' See the Engelmann reference for more information on CAP curves.
#'
#' @section Gain and Lift Curves:
#'
#' The motivation behind cumulative gain and lift charts is as a visual method to
#' determine the effectiveness of a model when compared to the results one
#' might expect without a model. As an example, without a model, if you were
#' to advertise to a random 10\% of your customer base, then you might expect
#' to capture 10\% of the of the total number of positive responses had you
#' advertised to your entire customer base. Given a model that predicts
#' which customers are more likely to respond, the hope is that you can more
#' accurately target 10\% of your customer base and capture
#' \>10\% of the total number of positive responses.
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
#' @templateVar metric_fn gain_curve
#' @template multiclass-curve
#' @template event_first
#'
#' @inheritParams pr_auc
#' @param object The `gain_df` data frame returned from `gain_curve()`.
#'
#' @return
#' A tibble with class `gain_df` or `gain_grouped_df` having
#' columns:
#'
#' - `.n` - The index of the current sample.
#' - `.n_events` - The index of the current _unique_ sample. Values with repeated
#'   `estimate` values are given identical indices in this column.
#' - `.percent_tested` - The cumulative percentage of values tested.
#' - `.percent_found` - The cumulative percentage of true results relative to the
#'   total number of true results.
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
#' @template examples-two-class-example
#' @template examples-two-class-prob
#' @examples
#' # Use autoplot to visualize
#' # The top left hand corner of the grey triangle is a "perfect" gain curve
#' library(ggplot2)
#' library(dplyr)
#' autoplot(gain_curve(two_class_example, truth, Class1))
#'
#' # Multiclass one-vs-all approach
#' # One curve per level
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   gain_curve(obs, VF:L) %>%
#'   autoplot()
#'
#' # Same as above, but will all of the resamples
#' # The resample with the minimum (farthest to the left) "perfect" value is
#' # used to draw the shaded region
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   gain_curve(obs, VF:L) %>%
#'   autoplot()
#'
#' @export
#'
gain_curve <- function(data, ...) {
  UseMethod("gain_curve")
}

#' @rdname gain_curve
#' @export
gain_curve.data.frame <- function(data, truth, ..., na_rm = TRUE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))
  truth <- enquo(truth)

  validate_not_missing(truth, "truth")

  # Explicit handling of length 1 character vectors as column names
  truth <- handle_chr_names(truth, colnames(data))

  res <- dplyr::do(
    data,
    gain_curve_vec(
      truth = rlang::eval_tidy(truth, data = .),
      estimate = rlang::eval_tidy(estimate, data = .),
      na_rm = na_rm
    )
  )

  if (dplyr::is_grouped_df(res)) {
    class(res) <- c("grouped_gain_df", "gain_df", class(res))
  }
  else {
    class(res) <- c("gain_df", class(res))
  }

  res
}

# dont export gain_curve_vec / lift_curve_vec
# not as meaningful to return a list of vectors
# maybe it could return the tibble?

#' @importFrom stats relevel complete.cases
gain_curve_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, metric_class = "gain_curve")

  # estimate here is a matrix of class prob columns
  gain_curve_impl <- function(truth, estimate) {
    gain_curve_estimator_impl(truth, estimate, estimator)
  }

  metric_vec_template(
    metric_impl = gain_curve_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = c("factor", "numeric"),
    ...
  )

}

gain_curve_estimator_impl <- function(truth, estimate, estimator) {

  if (is_binary(estimator)) {
    gain_curve_binary(truth, estimate)
  }
  else {
    gain_curve_multiclass(truth, estimate)
  }

}

gain_curve_binary <- function(truth, estimate) {

  # Relevel if event_first = FALSE
  # The second level becomes the first so as.integer()
  # holds the 1s and 2s in the correct slot
  if (!getOption("yardstick.event_first", default = TRUE)) {
    lvls <- levels(truth)
    truth <- relevel(truth, lvls[2])
  }

  # truth is now either 1 or 2
  truth <- as.integer(truth)

  gain_list <- gain_curve_binary_impl(truth, estimate)

  dplyr::tibble(!!!gain_list)
}

gain_curve_multiclass <- function(truth, estimate) {
  one_vs_all_with_level(gain_curve_binary, truth, estimate)
}

# Following the Example Problem 2 of:
# http://www2.cs.uregina.ca/~dbd/cs831/notes/lift_chart/lift_chart.html
gain_curve_binary_impl <- function(truth, estimate) {

  # arrange in decreasing order by class probability score
  estimate_ord <- order(estimate, decreasing = TRUE)
  estimate <- estimate[estimate_ord]
  truth <- truth[estimate_ord]

  n_events <- sum(truth == 1)
  n_predictions <- length(truth)

  # uninformative model x and y axis
  # this is also the x axis in the gain / lift charts
  cumulative_tested <- seq_along(truth)
  cumulative_percent_tested <- (cumulative_tested / n_predictions) * 100

  # recode events as 1 and non-events as 0 for cumsum
  truth <- ifelse(truth == 1, 1, 0)

  cumulative_found <- cumsum(truth)
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
  cumulative_percent_found  <- c(0, cumulative_percent_found)

  list(
    .n = cumulative_tested,
    .n_events = cumulative_found,
    .percent_tested = cumulative_percent_tested,
    .percent_found = cumulative_percent_found
  )
}

# autoplot ---------------------------------------------------------------------

# dynamically exported in .onLoad()

#' @rdname gain_curve
autoplot.gain_df <- function(object, ...) {

  `%+%` <- ggplot2::`%+%`
  `%>%` <- dplyr::`%>%`

  # Base chart
  chart <- ggplot2::ggplot(data = object)

  # Grouped specific chart features
  if (dplyr::is_grouped_df(object)) {

    # Construct the color interaction group
    grps <- dplyr::groups(object)
    interact_expr <- list(
      color = rlang::expr(interaction(!!! grps, sep = "_"))
    )

    # Add group legend label
    grps_chr <- paste0(dplyr::group_vars(object), collapse = "_")
    chart <- chart %+%
      ggplot2::labs(color = grps_chr)

  }
  else {
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
      object <- dplyr::group_by(object, .level, !!! grps)
    }
    object
  }

  # Construct poly_data
  # If grouped (ie resamples), we take the min of all "perfect" values
  #   to ensure we capture all lines in the polygon
  # If multiclass, we calculate each level separately
  poly_data <- object %>%
    maybe_group_by_level() %>%
    dplyr::summarise(slope = 1 / (max(.n_events) / dplyr::last(.n))) %>%
    dplyr::mutate(perfect = 100 / slope) %>%
    maybe_group_by_level(with_old = FALSE) %>%
    dplyr::summarise(perfect = min(perfect)) %>%
    maybe_group_by_level() %>%
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

    # gain curve
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = !!.percent_tested,
        y = !!.percent_found,
        !!! interact_expr
      ),
      data = object
    ) %+%

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
