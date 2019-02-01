#' Confusion Matrix for Categorical Data
#'
#' Calculates a cross-tabulation of observed and predicted classes.
#'
#' For [conf_mat()] objects, a `broom` `tidy()` method has been created
#' that collapses the cell counts by cell into a data frame for
#' easy manipulation.
#'
#' There is also a `summary()` method that computes various classification
#' metrics at once. See [summary.conf_mat()]
#'
#' There is a [ggplot2::autoplot()]
#' method for quickly visualizing the matrix. Both a heatmap and mosaic type
#' is implemented.
#'
#' The function requires that the factors have exactly the same levels.
#'
#' @aliases conf_mat.table conf_mat.default conf_mat
#'
#' @inheritParams sens
#'
#' @param data A data frame or a [base::table()].
#'
#' @param dnn A character vector of dimnames for the table.
#'
#' @param ... Options to pass to [base::table()] (not including
#'  `dnn`). This argument is not currently used for the `tidy`
#'  method.
#'
#' @return
#' `conf_mat()` produces an object with class `conf_mat`. This contains the
#' table and other objects. `tidy.conf_mat()` generates a tibble with columns
#' `name` (the cell identifier) and `value` (the cell count).
#'
#' When used on a grouped data frame, `conf_mat()` returns a tibble containing
#' columns for the groups along with `conf_mat`, a list-column
#' where each element is a `conf_mat` object.
#'
#' @seealso
#'
#' [summary.conf_mat()] for computing a large number of metrics from one
#' confusion matrix.
#'
#' @examples
#' library(dplyr)
#' data("hpc_cv")
#'
#' # The confusion matrix from a single assessment set (i.e. fold)
#' cm <- hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   conf_mat(obs, pred)
#' cm
#'
#' # Now compute the average confusion matrix across all folds in
#' # terms of the proportion of the data contained in each cell.
#' # First get the raw cell counts per fold using the `tidy` method
#' library(purrr)
#' library(tidyr)
#'
#' cells_per_resample <- hpc_cv %>%
#'   group_by(Resample) %>%
#'   conf_mat(obs, pred) %>%
#'   mutate(tidied = map(conf_mat, tidy)) %>%
#'   unnest(tidied)
#'
#' # Get the totals per resample
#' counts_per_resample <- hpc_cv %>%
#'   group_by(Resample) %>%
#'   summarize(total = n()) %>%
#'   left_join(cells_per_resample, by = "Resample") %>%
#'   # Compute the proportions
#'   mutate(prop = value/total) %>%
#'   group_by(name) %>%
#'   # Average
#'   summarize(prop = mean(prop))
#'
#' counts_per_resample
#'
#' # Now reshape these into a matrix
#' mean_cmat <- matrix(counts_per_resample$prop, byrow = TRUE, ncol = 4)
#' rownames(mean_cmat) <- levels(hpc_cv$obs)
#' colnames(mean_cmat) <- levels(hpc_cv$obs)
#'
#' round(mean_cmat, 3)
#'
#' # The confusion matrix can quickly be visualized using autoplot()
#' library(ggplot2)
#'
#' autoplot(cm, type = "mosaic")
#' autoplot(cm, type = "heatmap")
#'
#' @export
conf_mat <- function(data, ...) {
  UseMethod("conf_mat")
}

#' @export
#' @rdname conf_mat
conf_mat.data.frame <- function(data, truth, estimate,
                                dnn = c("Prediction", "Truth"), ...) {

  vars <-
    factor_select(
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      ...
    )

  xtab <- vec2table(
    truth = data[[vars$truth]],
    estimate = data[[vars$estimate]],
    dnn = dnn,
    ...
  )
  conf_mat.table(xtab, ...)

}

#' @export
conf_mat.grouped_df <- function(data, truth, estimate,
                                dnn = c("Prediction", "Truth"), ...) {

  vars <-
    factor_select(
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      ...
    )

  truth <- as.name(vars$truth)
  estimate <- as.name(vars$estimate)

  dplyr::summarise(
    data,
    conf_mat = {
      xtab <- vec2table(
        truth = !! truth,
        estimate = !! estimate,
        dnn = dnn,
        ...
      )
      list(conf_mat.table(xtab, ...))
    }
  )

}

#' @export
conf_mat.table <- function(data, ...) {

  check_table(data)

  class_lev <- rownames(data)
  num_lev <- length(class_lev)

  if (num_lev < 2) {
    abort("There must be at least 2 factors levels in the `data`")
  }

  structure(
    list(table = data, dots = list(...)),
    class = "conf_mat"
  )

}

#' @export
conf_mat.matrix <- function(data, ...) {

  data <- as.table(data)
  conf_mat.table(data, ...)

}


#' @export
print.conf_mat <- function(x, ...)
  print(x$table)

#' @export
#' @rdname conf_mat
#' @param x A `conf_mat` object.
tidy.conf_mat <- function(x, ...) {
  y <- flatten(x$table)
  dplyr::tibble(
    name = names(y),
    value = unname(y)
  )
}

flatten <- function(xtab) {
  p <- ncol(xtab)
  if(nrow(xtab) != p)
    stop("table must have equal dimensions")
  flat <- as.vector(xtab)
  names(flat) <- paste("cell", rep(1:p, p), rep(1:p, each = p), sep = "_")
  flat
}

#' Summary Statistics for Confusion Matrices
#'
#' Various statistical summaries of confusion matrices are
#' produced and returned in a tibble. These include those shown in the help
#' pages for [sens()], [recall()], and [accuracy()], among others.
#'
#' @template event_first
#'
#' @inheritParams sens
#'
#' @param object An object of class [conf_mat()].
#'
#' @param prevalence A number in `(0, 1)` for the prevalence (i.e.
#'  prior) of the event. If left to the default, the data are used
#'  to derive this value.
#'
#' @param beta A numeric value used to weight precision and
#'  recall for [f_meas()].
#'
#' @param ... Not currently used.
#'
#' @return
#'
#' A tibble containing various classification metrics.
#'
#' @seealso
#'
#' [conf_mat()]
#'
#' @examples
#' data("two_class_example")
#'
#' cmat <- conf_mat(two_class_example, truth = "truth", estimate = "predicted")
#' summary(cmat)
#' summary(cmat, prevalence = 0.70)
#'
#' library(dplyr)
#' library(purrr)
#' library(tidyr)
#' data("hpc_cv")
#'
#' # Compute statistics per resample then summarize
#' all_metrics <- hpc_cv %>%
#'   group_by(Resample) %>%
#'   conf_mat(obs, pred) %>%
#'   mutate(summary_tbl = map(conf_mat, summary)) %>%
#'   unnest(summary_tbl)
#'
#' all_metrics %>%
#'   group_by(.metric) %>%
#'   summarise(
#'     mean = mean(.estimate, na.rm = TRUE),
#'     sd = sd(.estimate, na.rm = TRUE)
#'   )
#'
#'
#' @export
#' @importFrom dplyr bind_rows
summary.conf_mat <- function(object,
                             prevalence = NULL,
                             beta = 1,
                             estimator = NULL,
                             ...) {

  xtab <- object$table

  stats <- bind_rows(
    # known multiclass extension
    accuracy(xtab),
    # known multiclass extension
    kap(xtab),
    sens(xtab, estimator = estimator),
    spec(xtab, estimator = estimator),
    ppv(xtab, prevalence = prevalence, estimator = estimator),
    npv(xtab, prevalence = prevalence, estimator = estimator),
    # known multiclass extension
    mcc(xtab),
    j_index(xtab, estimator = estimator),
    bal_accuracy(xtab, estimator = estimator),
    detection_prevalence(xtab, estimator = estimator),
    precision(xtab, estimator = estimator),
    recall(xtab, estimator = estimator),
    f_meas(xtab, beta = beta, estimator = estimator)
  )

  stats
}

conf_mat_plot_types <- c("mosaic", "heatmap")

# Dynamically exported
#' @rdname conf_mat
#' @param type Type of plot desired, must be "mosaic" or "heatmap",
#'   defaults to "mosaic".
#' @param object The `conf_mat` data frame returned from `conf_mat()`.
autoplot.conf_mat <- function(object, type = "mosaic", ...) {

  if (!(type %in% conf_mat_plot_types)) {
    stop("`type` must be one of: ",
         paste(conf_mat_plot_types, collapse = ", "), call. = FALSE)
  }

  switch(type,
         mosaic = cm_mosaic(object),
         heatmap = cm_heat(object))
}



cm_heat <- function(x) {

  `%+%` <- ggplot2::`%+%`

  as.data.frame.table(x$table) %>%
    ggplot2::ggplot(ggplot2::aes(Prediction, Truth, fill = Freq)) %+%
    ggplot2::geom_tile() %+%
    ggplot2::scale_fill_gradient(low = "white", high = "black") %+%
    ggplot2::theme_bw()
}

space_fun <- function(x, adjustment, rescale = FALSE) {

  if(rescale) {
    x <- x / sum(x)
  }

  adjustment <- sum(x) / adjustment

  xmax <- cumsum(x) + seq(0, length(x) - 1) * adjustment
  xmin <- cumsum(x) - x + seq(0, length(x) - 1) * adjustment
  dplyr::tibble(xmin = xmin,
         xmax = xmax)
}

space_y_fun <- function(data, id, x_data) {
  out <- (space_fun(data[, id], 100, rescale = TRUE) * -1)
  names(out) <- c("ymin", "ymax")
  out$xmin = x_data[[id, 1]]
  out$xmax = x_data[[id, 2]]
  out
}

cm_mosaic <- function(x) {

  `%+%` <- ggplot2::`%+%`

  cm_zero <- (as.numeric(x$table == 0) / 2) + x$table

  x_data <- space_fun(rowSums(cm_zero), 200)

  full_data_list <- purrr::map(seq_len(ncol(cm_zero)),
                        ~ space_y_fun(cm_zero, .x, x_data))

  full_data <- dplyr::bind_rows(full_data_list)

  y1_data <- full_data_list[[1]]

  tick_labels <- colnames(cm_zero)

  ggplot2::ggplot(full_data) %+%
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) %+%
    ggplot2::scale_x_continuous(breaks = (x_data$xmin + x_data$xmax) / 2,
                       labels = tick_labels) %+%
    ggplot2::scale_y_continuous(breaks = (y1_data$ymin + y1_data$ymax) / 2,
                       labels = tick_labels) %+%
    ggplot2::labs(x = "Predicted",
         y = "Truth") %+%
    ggplot2::theme(panel.background = ggplot2::element_blank())
}
