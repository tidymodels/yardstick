#' Cumulative gain and lift charts
#'
#' `gain_curve()` calculates the information required for cumulative gain charts,
#' and `lift_curve()` calculates the information for lift charts. Both are visual
#' aids for measuring model performance.
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
#' values.
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
#' The output of `gain_curve()` and `lift_curve()` both have [ggplot2::autoplot()] methods
#' for easy visualization.
#'
#' `gain_capture()` calculates the area _under_ the gain curve, but _above_
#' the baseline, and then divides that by the area _under_ a perfect gain curve,
#' but _above_ the baseline. It is meant to represent the amount of potential
#' gain "captured" by the model.
#'
#' There is no common convention on which factor level should
#'  automatically be considered the "relevant" or "positive" results.
#'  In `yardstick`, the default is to use the _first_ level. To
#'  change this, a global option called `yardstick.event_first` is
#'  set to `TRUE` when the package is loaded. This can be changed
#'  to `FALSE` if the last level of the factor is considered the
#'  level of interest.
#'
#' @param data A `data.frame` containing the truth and estimate columns.
#'
#' @param estimate The column identifier for the predicted class probabilities
#' (that is a `numeric`) corresponding to the "positive" result. See Details.
#'
#' @param truth The column identifier for the true class results
#' (that is a `factor`). This should be an unquoted column name although this
#' argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column names).
#'
#' @param na.rm A `logical` value indicating whether `NA`
#'  values should be stripped before the computation proceeds.
#'
#' @param ... Not currently used.
#'
#' @param object The data frame output of `gain_curve()` or `lift_curve()`
#' to plot.
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
gain_curve.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  vars <- prob_select(
    data = data,
    truth = !! enquo(truth),
    !! enquo(estimate)
  )

  truth <- data[[vars$truth]]
  estimate <- data[[vars$probs]]

  pr_list <- gain_curve_vec(truth, estimate, na.rm)

  gain_df <- dplyr::tibble(!!!pr_list)
  class(gain_df) <- c("gain_df", class(gain_df))

  gain_df
}

# dont export gain_curve_vec / lift_curve_vec
# not as meaningful to return a list of vectors
# maybe it could return the tibble?

#' @importFrom stats relevel complete.cases
gain_curve_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  lvls <- levels(truth)

  if(length(lvls) != 2L) {
    stop("`truth` must be a two level factor.", call. = FALSE)
  }

  # Relevel if event_first = FALSE
  # The second level becomes the first so as.integer()
  # holds the 1s and 2s in the correct slot
  if (!getOption("yardstick.event_first", default = TRUE)) {
    truth <- relevel(truth, lvls[2])
  }

  if(na.rm) {
    complete_idx <- complete.cases(truth, estimate)
    truth <- truth[complete_idx]
    estimate <- estimate[complete_idx]
  }

  # truth is now either 1 or 2
  truth <- as.integer(truth)

  gain_curve_vec_impl(truth, estimate)
}

# Following the Example Problem 2 of:
# http://www2.cs.uregina.ca/~dbd/cs831/notes/lift_chart/lift_chart.html
gain_curve_vec_impl <- function(truth, estimate) {

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
lift_curve.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  vars <- prob_select(
    data = data,
    truth = !! enquo(truth),
    !! enquo(estimate)
  )

  truth <- data[[vars$truth]]
  estimate <- data[[vars$probs]]

  pr_list <- lift_curve_vec(truth, estimate, na.rm)

  lift_df <- dplyr::tibble(!!!pr_list)
  class(lift_df) <- c("lift_df", class(lift_df))

  lift_df
}

lift_curve_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  res <- gain_curve_vec(truth, estimate, na.rm)
  res[[".lift"]] <- res[[".percent_found"]] / res[[".percent_tested"]]
  res[[".percent_found"]] <- NULL

  res
}

# autoplot() -------------------------------------------------------------------

# dynamically exported in .onLoad()

#' @rdname gain_curve
autoplot.gain_df <- function(object, ...) {

  `%+%` <- ggplot2::`%+%`

  # slope of the line from (0,0) to the elbow
  slope <- 1 / (max(object[[".n_events"]]) / nrow(object))
  # x-axis point of the elbow
  perfect <- 100 / slope

  poly_data <- data.frame(
    x = c(0, perfect, 100),
    y = c(0, 100, 100)
  )

  # Avoid cran check for "globals"
  .percent_tested <- as.name(".percent_tested")
  .percent_found <- as.name(".percent_found")
  x <- as.name("x")
  y <- as.name("y")

  ggplot2::ggplot(object) %+%

    # gain curve
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = !!.percent_tested,
        y = !!.percent_found
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
      alpha = 0.2,
      # border
      colour = "grey60",
      linetype = 2
    ) %+%

    ggplot2::labs(
      x = "% Tested",
      y = "% Found"
    )
}

# dynamically exported in .onLoad()

#' @rdname gain_curve
autoplot.lift_df <- function(object, ...) {

  `%+%` <- ggplot2::`%+%`

  baseline <- data.frame(
    x = c(0, 100),
    y = c(1, 1)
  )

  # Avoid cran check for "globals"
  .percent_tested <- as.name(".percent_tested")
  .lift <- as.name(".lift")
  x <- as.name("x")
  y <- as.name("y")

  ggplot2::ggplot(object) %+%

    # gain curve
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = !!.percent_tested,
        y = !!.lift
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
    )
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
gain_capture.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "gain_capture",
    metric_fn = gain_capture_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

gain_capture_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  # currently only binary is allowed
  averaging <- "binary"

  gain_capture_impl <- function(truth, estimate) {
    gain_capture_averaging_impl(truth, estimate, averaging)
  }

  metric_vec_template(
    metric_impl = gain_capture_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    averaging = averaging,
    cls = c("factor", "numeric"),
    ...
  )

}

gain_capture_averaging_impl <- function(truth, estimate, averaging) {

  if(is_binary(averaging)) {
    gain_capture_binary(truth, estimate)
  } else {
    # should there be a multiclass case?
    # could do macro?
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
