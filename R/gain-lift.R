#' Cumulative gain and lift charts
#'
#' `gain_curve()` calculates the information required for cumulative gain charts,
#' and `lift_curve()` calculates the information for lift charts. Both are visual
#' aids for measuring model performance. `gain_capture()` is a measure of
#' performance similar to an AUC calculation.
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
#' The calculation to construct gain and lift curves is as follows:
#'
#' 1. `truth` and `estimate` are placed in descending order by the `estimate`
#' values. (`estimate` here is a single column supplied in `...`)
#'
#' 2. The cumulative number of samples with true results relative to the
#' entire number of true results are found. This is the y-axis in a gain chart.
#'
#' 3. The cumulative \% found is divided by the cumulative \% tested
#' to construct the lift value. This ratio represents the factor of improvement
#' over an uninformed model. Values >1 represent a valuable model. This is the
#' y-axis of the lift chart.
#'
#' The greater the area between the gain curve and the baseline, the better
#' the model.
#'
#' The output of `gain_curve()` and `lift_curve()` both have
#' [ggplot2::autoplot()] methods for easy visualization. See the examples.
#'
#' For `gain_curve()` and `lift_curve()`, if a multiclass `truth` column is
#' provided, a one-vs-all approach will be taken to calculate multiple curves,
#' one per level. In this case, there will be an additional column, `.level`,
#' identifying the "one" column in the one-vs-all calculation.
#'
#' `gain_capture()` calculates the area _under_ the gain curve, but _above_
#' the baseline, and then divides that by the area _under_ a perfect gain curve,
#' but _above_ the baseline. It is meant to represent the amount of potential
#' gain "captured" by the model.
#'
#' There is no common convention on which factor level should
#' automatically be considered the "relevant" or "positive" results.
#' In `yardstick`, the default is to use the _first_ level. To
#' change this, a global option called `yardstick.event_first` is
#' set to `TRUE` when the package is loaded. This can be changed
#' to `FALSE` if the last level of the factor is considered the
#' level of interest.
#'
#' @inheritParams roc_curve
#'
#' @param object The data frame output of `gain_curve()` or `lift_curve()`
#' to plot.
#'
#' @param estimator One of `"binary"`, `"macro"`, or `"macro_weighted"` to
#' specify the type of estimator to be done. `"binary"` is only relevant for
#' the two class case. The other two are general methods for calculating
#' multiclass metrics. The default will automatically choose `"binary"` or
#' `"macro"` based on `truth`.
#'
#' @aliases gain_curve lift_curve gain_capture
#'
#' @examples
#' library(ggplot2)
#' data(two_class_example)
#'
#' # Calculate the cumulative gain information given a 2 class factor
#' # and the corresponding predicted class probabilities
#' gn <- gain_curve(two_class_example, truth, Class1)
#'
#' # Plot it
#' autoplot(gn)
#'
#' # Similar calculation, but for lift
#' lif <- lift_curve(two_class_example, truth, Class1)
#'
#' autoplot(lif)
#'
#' gain_capture(two_class_example, truth, Class1)
#'
#' # Visually, this represents the area under the black curve, but above the
#' # 45 degree dotted line, divided by the area of the shaded triangle.
#' library(ggplot2)
#' autoplot(gain_curve(two_class_example, truth, Class1))
#'
#' @name gain_curve
NULL

# Gain -------------------------------------------------------------------------

#' @export
#' @rdname gain_curve
gain_curve <- function(data, ...) {
  UseMethod("gain_curve")
}

#' @rdname gain_curve
#' @export
gain_curve.data.frame <- function(data, truth, ..., na.rm = TRUE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))
  truth <- enquo(truth)

  validate_not_missing(truth, "truth")

  # Explicit handling of length 1 character vectors as column names
  truth <- handle_chr_names(truth)

  res <- dplyr::do(
    data,
    gain_curve_vec(
      truth = rlang::eval_tidy(truth, data = .),
      estimate = rlang::eval_tidy(estimate, data = .),
      na.rm = na.rm
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
gain_curve_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, metric_class = "gain_curve")

  # estimate here is a matrix of class prob columns
  gain_curve_impl <- function(truth, estimate) {
    gain_curve_estimator_impl(truth, estimate, estimator)
  }

  metric_vec_template(
    metric_impl = gain_curve_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
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

# Lift -------------------------------------------------------------------------

#' @rdname gain_curve
#' @export
lift_curve <- function(data, ...) {
  UseMethod("lift_curve")
}

#' @rdname gain_curve
#' @export
lift_curve.data.frame <- function(data, truth, ..., na.rm = TRUE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))
  truth <- enquo(truth)

  validate_not_missing(truth, "truth")

  # Explicit handling of length 1 character vectors as column names
  truth <- handle_chr_names(truth)

  res <- dplyr::do(
    data,
    lift_curve_vec(
      truth = rlang::eval_tidy(truth, data = .),
      estimate = rlang::eval_tidy(estimate, data = .),
      na.rm = na.rm
    )
  )

  if (dplyr::is_grouped_df(res)) {
    class(res) <- c("grouped_lift_df", "lift_df", class(res))
  }
  else {
    class(res) <- c("lift_df", class(res))
  }

  res
}

lift_curve_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  # tibble result, possibly grouped
  res <- gain_curve_vec(truth, estimate, na.rm)

  res <- dplyr::mutate(res, .lift = .percent_found / .percent_tested)

  res[[".percent_found"]] <- NULL

  res
}

# Gain Capture -----------------------------------------------------------------

#' @rdname gain_curve
#' @export
gain_capture <- function(data, ...) {
  UseMethod("gain_capture")
}

class(gain_capture) <- c("prob_metric", "function")

#' @rdname gain_curve
#' @export
gain_capture.data.frame <- function(data, truth, ...,
                                    estimator = NULL,
                                    na.rm = TRUE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "gain_capture",
    metric_fn = gain_capture_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! estimate,
    estimator = estimator,
    na.rm = na.rm,
    ... = ...
  )

}

gain_capture_vec <- function(truth, estimate,
                             estimator = NULL,
                             na.rm = TRUE,
                             ...) {

  estimator <- finalize_estimator(truth, estimator, "gain_capture")

  gain_capture_impl <- function(truth, estimate) {
    gain_capture_estimator_impl(truth, estimate, estimator)
  }

  metric_vec_template(
    metric_impl = gain_capture_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    estimator = estimator,
    cls = c("factor", "numeric"),
    ...
  )

}

gain_capture_estimator_impl <- function(truth, estimate, estimator) {

  if(is_binary(estimator)) {
    gain_capture_binary(truth, estimate)
  } else {
    truth_table <- matrix(table(truth), nrow = 1)
    w <- get_weights(truth_table, estimator)
    out_vec <- gain_capture_multiclass(truth, estimate)
    weighted.mean(out_vec, w)
  }

}

gain_capture_binary <- function(truth, estimate) {

  gain_list <- gain_curve_vec(truth, estimate)

  gain_to_0_auc <- auc(
    x = gain_list[[".percent_tested"]],
    y = gain_list[[".percent_found"]]
  )

  scaler <- 1 / (100 ^ 2)
  height <- 100
  width <- 100
  baseline <- 0.5

  gain_to_0_auc <- gain_to_0_auc * scaler

  # perfect = value at the elbow of the perfect gain chart
  .n_events <- gain_list[[".n_events"]]
  slope <- 1 / (max(.n_events) / length(.n_events))
  perfect <- height / slope

  # perfect triangle
  perf_triang <- (perfect * height) / 2

  # perfect rectangle
  perf_rect <- (width - perfect) * height

  perf_auc <- (perf_rect + perf_triang) * scaler

  # calculate capture ratio = fraction of area captured
  # under the gain curve, but above the baseline, and relative to a
  # perfect capture score
  (gain_to_0_auc - baseline) / (perf_auc - baseline)

}

gain_capture_multiclass <- function(truth, estimate) {
  res_lst <- one_vs_all_impl(gain_capture_binary, truth, estimate)
  rlang::flatten_dbl(res_lst)
}
