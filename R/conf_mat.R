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
#' @param ... Not used.
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
#' @examplesIf rlang::is_installed(c("tidyr", "ggplot2"))
#' library(dplyr)
#' data("hpc_cv")
#'
#' # The confusion matrix from a single assessment set (i.e. fold)
#' cm <- hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   conf_mat(obs, pred)
#' cm
#'
#' # Now compute the average confusion matrix across all folds in
#' # terms of the proportion of the data contained in each cell.
#' # First get the raw cell counts per fold using the `tidy` method
#' library(tidyr)
#'
#' cells_per_resample <- hpc_cv |>
#'   group_by(Resample) |>
#'   conf_mat(obs, pred) |>
#'   mutate(tidied = lapply(conf_mat, tidy)) |>
#'   unnest(tidied)
#'
#' # Get the totals per resample
#' counts_per_resample <- hpc_cv |>
#'   group_by(Resample) |>
#'   summarize(total = n()) |>
#'   left_join(cells_per_resample, by = "Resample") |>
#'   # Compute the proportions
#'   mutate(prop = value / total) |>
#'   group_by(name) |>
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
#' @export
conf_mat <- function(data, ...) {
  UseMethod("conf_mat")
}

#' @export
#' @rdname conf_mat
conf_mat.data.frame <- function(
  data,
  truth,
  estimate,
  dnn = c("Prediction", "Truth"),
  case_weights = NULL,
  ...
) {
  if (dots_n(...) != 0L) {
    warn_conf_mat_dots_deprecated()
  }

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  case_weights <- enquo(case_weights)

  truth <- yardstick_eval_select(
    expr = truth,
    data = data,
    arg = "truth"
  )
  truth <- data[[truth]]

  estimate <- yardstick_eval_select(
    expr = estimate,
    data = data,
    arg = "estimate"
  )
  estimate <- data[[estimate]]

  if (quo_is_null(case_weights)) {
    case_weights <- NULL
  } else {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights"
    )

    case_weights <- data[[case_weights]]
  }

  table <- conf_mat_impl(
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )

  dimnames <- dimnames(table)
  names(dimnames) <- dnn
  dimnames(table) <- dimnames

  conf_mat.matrix(table)
}

#' @export
conf_mat.grouped_df <- function(
  data,
  truth,
  estimate,
  dnn = c("Prediction", "Truth"),
  case_weights = NULL,
  ...
) {
  if (dots_n(...) != 0L) {
    warn_conf_mat_dots_deprecated()
  }

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  case_weights <- enquo(case_weights)

  truth <- yardstick_eval_select(
    expr = truth,
    data = data,
    arg = "truth"
  )
  estimate <- yardstick_eval_select(
    expr = estimate,
    data = data,
    arg = "estimate"
  )

  if (quo_is_null(case_weights)) {
    group_case_weights <- NULL
  } else {
    case_weights <- yardstick_eval_select(
      expr = case_weights,
      data = data,
      arg = "case_weights",
      error_call = error_call
    )
  }

  group_rows <- dplyr::group_rows(data)
  group_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)
  groups <- vec_chop(data, indices = group_rows)
  out <- vector("list", length = length(groups))

  for (i in seq_along(groups)) {
    group <- groups[[i]]

    group_truth <- group[[truth]]
    group_estimate <- group[[estimate]]

    if (is_string(case_weights)) {
      group_case_weights <- group[[case_weights]]
    }

    table <- conf_mat_impl(
      truth = group_truth,
      estimate = group_estimate,
      case_weights = group_case_weights
    )

    dimnames <- dimnames(table)
    names(dimnames) <- dnn
    dimnames(table) <- dimnames

    out[[i]] <- conf_mat.matrix(table)
  }

  out <- vec_cbind(group_keys, conf_mat = out)
  out
}

conf_mat_impl <- function(truth, estimate, case_weights, call = caller_env()) {
  abort_if_class_pred(truth, call = call)
  estimate <- as_factor_from_class_pred(estimate, call = call)

  estimator <- "not binary"
  check_class_metric(truth, estimate, case_weights, estimator, call = call)

  if (length(levels(truth)) < 2) {
    cli::cli_abort(
      "{.arg truth} must have at least 2 factor levels.",
      call = call
    )
  }

  yardstick_table(
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )
}

#' @export
conf_mat.table <- function(data, ...) {
  check_table(data)

  # To ensure that we always have a consistent output type, whether or not
  # case weights were used when constructing the table
  storage.mode(data) <- "double"

  class_lev <- rownames(data)
  num_lev <- length(class_lev)

  if (num_lev < 2) {
    cli::cli_abort(
      "There must be at least 2 factors levels in the {.arg data}."
    )
  }

  structure(
    list(table = data),
    class = "conf_mat"
  )
}

#' @export
conf_mat.matrix <- function(data, ...) {
  # We want the conversion from a yardstick_table() result (i.e. a double
  # matrix) to a table to occur. tune relies on the `as.data.frame.table()`
  # method to run, so it has to be a table.
  data <- as.table(data)
  conf_mat.table(data)
}

warn_conf_mat_dots_deprecated <- function() {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = I("The `...` argument of `conf_mat()`"),
    details = "This argument no longer has any effect, and is being ignored."
  )
}

#' @export
print.conf_mat <- function(x, ...) {
  print(x$table)
}

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

flatten <- function(xtab, call = caller_env()) {
  n_col <- ncol(xtab)
  n_row <- nrow(xtab)
  if (n_row != n_col) {
    cli::cli_abort(
      "{.arg x} must have equal dimensions.
      {.arg x} has {n_col} columns and {n_row} rows.",
      call = call
    )
  }
  flat <- as.vector(xtab)
  names(flat) <- paste(
    "cell",
    rep(seq_len(n_col), n_col),
    rep(seq_len(n_col), each = n_col),
    sep = "_"
  )

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
#' @examplesIf rlang::is_installed(c("tidyr"))
#' data("two_class_example")
#'
#' cmat <- conf_mat(two_class_example, truth = "truth", estimate = "predicted")
#' summary(cmat)
#' summary(cmat, prevalence = 0.70)
#'
#' library(dplyr)
#' library(tidyr)
#' data("hpc_cv")
#'
#' # Compute statistics per resample then summarize
#' all_metrics <- hpc_cv |>
#'   group_by(Resample) |>
#'   conf_mat(obs, pred) |>
#'   mutate(summary_tbl = lapply(conf_mat, summary)) |>
#'   unnest(summary_tbl)
#'
#' all_metrics |>
#'   group_by(.metric) |>
#'   summarise(
#'     mean = mean(.estimate, na.rm = TRUE),
#'     sd = sd(.estimate, na.rm = TRUE)
#'   )
#' @export
summary.conf_mat <- function(
  object,
  prevalence = NULL,
  beta = 1,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  xtab <- object$table

  stats <- dplyr::bind_rows(
    # known multiclass extension
    accuracy(xtab),
    # known multiclass extension
    kap(xtab),
    sens(xtab, estimator = estimator, event_level = event_level),
    spec(xtab, estimator = estimator, event_level = event_level),
    ppv(
      xtab,
      prevalence = prevalence,
      estimator = estimator,
      event_level = event_level
    ),
    npv(
      xtab,
      prevalence = prevalence,
      estimator = estimator,
      event_level = event_level
    ),
    # known multiclass extension
    mcc(xtab),
    j_index(xtab, estimator = estimator, event_level = event_level),
    bal_accuracy(xtab, estimator = estimator, event_level = event_level),
    detection_prevalence(
      xtab,
      estimator = estimator,
      event_level = event_level
    ),
    precision(xtab, estimator = estimator, event_level = event_level),
    recall(xtab, estimator = estimator, event_level = event_level),
    f_meas(xtab, beta = beta, estimator = estimator, event_level = event_level)
  )

  stats
}

# Dynamically exported
autoplot.conf_mat <- function(object, type = "mosaic", ...) {
  type <- arg_match(type, conf_mat_plot_types)

  switch(type, mosaic = cm_mosaic(object), heatmap = cm_heat(object))
}

conf_mat_plot_types <- c("mosaic", "heatmap")

cm_heat <- function(x) {
  `%+%` <- ggplot2::`%+%`

  df <- as.data.frame.table(x$table)
  # Force specific column names for referencing in ggplot2 code
  names(df) <- c("Prediction", "Truth", "Freq")

  # Have prediction levels going from high to low so they plot in an
  # order that matches the LHS of the confusion matrix
  lvls <- levels(df$Prediction)
  df$Prediction <- factor(df$Prediction, levels = rev(lvls))

  # For case weighted confusion matrices this looks a little better
  df$Freq <- round(df$Freq, digits = 3)

  axis_labels <- get_axis_labels(x)

  df |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = Truth,
        y = Prediction,
        fill = Freq
      )
    ) %+%
    ggplot2::geom_tile() %+%
    ggplot2::scale_fill_gradient(
      low = "grey90",
      high = "grey40"
    ) %+%
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      legend.position = "none"
    ) %+%
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = Freq)
    ) %+%
    ggplot2::labs(
      x = axis_labels$x,
      y = axis_labels$y
    )
}

space_fun <- function(x, adjustment, rescale = FALSE) {
  if (rescale) {
    x <- x / sum(x)
  }

  adjustment <- sum(x) / adjustment

  xmax <- cumsum(x) + seq(0, length(x) - 1) * adjustment
  xmin <- cumsum(x) - x + seq(0, length(x) - 1) * adjustment

  dplyr::tibble(xmin = xmin, xmax = xmax)
}

space_y_fun <- function(data, id, x_data) {
  out <- space_fun(data[, id], 100, rescale = TRUE) * -1

  names(out) <- c("ymin", "ymax")

  out$xmin <- x_data[[id, 1]]
  out$xmax <- x_data[[id, 2]]

  out
}

cm_mosaic <- function(x) {
  `%+%` <- ggplot2::`%+%`

  cm_zero <- (as.numeric(x$table == 0) / 2) + x$table

  x_data <- space_fun(colSums(cm_zero), 200)

  full_data_list <- lapply(
    seq_len(ncol(cm_zero)),
    FUN = function(.x) space_y_fun(cm_zero, .x, x_data)
  )

  full_data <- dplyr::bind_rows(full_data_list)

  y1_data <- full_data_list[[1]]

  tick_labels <- colnames(cm_zero)
  axis_labels <- get_axis_labels(x)

  ggplot2::ggplot(full_data) %+%
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      )
    ) %+%
    ggplot2::scale_x_continuous(
      breaks = (x_data$xmin + x_data$xmax) / 2,
      labels = tick_labels
    ) %+%
    ggplot2::scale_y_continuous(
      breaks = (y1_data$ymin + y1_data$ymax) / 2,
      labels = tick_labels
    ) %+%
    ggplot2::labs(
      y = axis_labels$y,
      x = axis_labels$x
    ) %+%
    ggplot2::theme(panel.background = ggplot2::element_blank())
}

# Note: Always assumes predictions are on the LHS of the table
get_axis_labels <- function(x) {
  table <- x$table

  labels <- names(dimnames(table))

  if (is.null(labels)) {
    labels <- c("Prediction", "Truth")
  }

  list(
    y = labels[[1]],
    x = labels[[2]]
  )
}
