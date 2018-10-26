#' Other Classification Metrics
#'
#' Other metrics for classification problems that are not already in
#'  [sens()] or [recall()] are here, such as the Matthews
#'  correlation coefficient, Youden's J, the balanced accuracy (the
#'  average between sensitivity and specificity), and the detection
#'  prevalence (the rate of _predicted_ events).
#'
#' There is no common convention on which factor level should
#'  automatically be considered the "event" or "positive" results.
#'  In `yardstick`, the default is to use the _first_ level. To
#'  change this, a global option called `yardstick.event_first` is
#'  set to `TRUE` when the package is loaded. This can be changed
#'  to `FALSE` if the last level of the factor is considered the
#'  level of interest.
#'
#' If more than one statistic is required, it is more
#'  computationally efficient to create the confusion matrix using
#'  [conf_mat()] and applying the corresponding `summary` method
#'  ([summary.conf_mat()]) to get the values at once.
#'
#'
#' @section Multiclass:
#'
#' Macro, micro, and macro-weighted averaging are available for these metrics.
#' The default is to select macro averaging if an `estimate` factor with more
#' than 2 levels is provided. See `vignette("averaging", "yardstick")` for more
#' information.
#'
#' `mcc()` has a known multiclass generalization and that is computed
#' automatically if a factor with more than 2 levels is provided. Because
#' of this, no averaging methods are provided.
#'
#' @inheritParams sens
#'
#' @author Max Kuhn
#'
#' @references Giuseppe, J. (2012). "A Comparison of MCC and CEN Error
#' Measures in Multi-Class Prediction". _PLOS ONE_. Vol 7, Iss 8, e41882.
#'
#' @seealso [conf_mat()], [summary.conf_mat()], [recall()], [sens()], [spec()]
#'
#' @examples
#' data("two_class_example")
#'
#' mcc(two_class_example, truth, predicted)
#'
#' j_index(two_class_example, truth, predicted)
#'
#' bal_accuracy(two_class_example, truth, predicted)
#'
#' detection_prevalence(two_class_example, truth, predicted)
#'
#' @name mcc
#'
NULL

# MCC --------------------------------------------------------------------------

#' @export
#' @rdname mcc
mcc <- function(data, ...) {
  UseMethod("mcc")
}

class(mcc) <- c("class_metric", "function")

#' @export
#' @rdname mcc
mcc.data.frame <- function(data, truth, estimate,
                           na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "mcc",
    metric_fn = mcc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    # do not pass dots through
    # (could allow estimator to be set. unwanted!)
  )

}

#' @export
mcc.table <- function(data, ...) {
  check_table(data)
  estimator <- finalize_estimator(data, metric_class = "mcc")

  metric_tibbler(
    .metric = "mcc",
    .estimator = estimator,
    .estimate = mcc_table_impl(data, estimator)
  )

}

#' @export
mcc.matrix <- function(data, ...) {
  data <- as.table(data)
  mcc.table(data)
}

#' @export
#' @rdname mcc
mcc_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, metric_class = "mcc")

  mcc_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    mcc_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = mcc_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

mcc_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    mcc_binary(data)
  } else {
    mcc_multiclass(data)
  }

}

mcc_binary <- function(data) {

  positive <- pos_val(data)
  negative <- neg_val(data)

  # This and `prod` below to deal with integer overflow
  data <- as.matrix(data)

  tp <- data[positive, positive]
  tn <- data[negative, negative]
  fp <- data[positive, negative]
  fn <- data[negative, positive]
  d1 <- tp + fp
  d2 <- tp + fn
  d3 <- tn + fp
  d4 <- tn + fn
  if (d1 == 0 | d2 == 0 | d3 == 0 | d4 == 0)
    return(NA)
  ((tp * tn) - (fp * fn)) / sqrt(prod(d1, d2, d3, d4))

}

mcc_multiclass <- function(data) {
  stopifnot(is.table(data))
  mcc_multiclass_cpp(data)
}

# J Index ----------------------------------------------------------------------

#' @export
#' @rdname mcc
j_index <- function(data, ...) {
  UseMethod("j_index")
}

class(j_index) <- c("class_metric", "function")

#' @export
#' @rdname mcc
j_index.data.frame <- function(data, truth, estimate,
                               estimator = NULL,
                               na.rm = TRUE,
                               ...) {

  metric_summarizer(
    metric_nm = "j_index",
    metric_fn = j_index_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
j_index.table <- function(data, estimator = NULL, ...) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "j_index",
    .estimator = estimator,
    .estimate = j_index_table_impl(data, estimator)
  )

}

#' @export
j_index.matrix <- function(data, estimator = NULL, ...) {

  data <- as.table(data)
  j_index.table(data, estimator)

}

#' @export
#' @rdname mcc
j_index_vec <- function(truth, estimate, estimator = NULL,
                        na.rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  j_index_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    j_index_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = j_index_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    estimator = estimator,
    ...
  )

}

j_index_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    j_index_binary(data)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- j_index_multiclass(data, estimator)
    weighted.mean(out_vec, w)
  }

}

j_index_binary <- function(data) {
  # sens + spec - 1
  recall_binary(data) + spec_binary(data) - 1
}

j_index_multiclass <- function(data, estimator) {
  recall_multiclass(data, estimator) + spec_multiclass(data, estimator) - 1
}

# Balanced Accuracy ------------------------------------------------------------

#' @export
#' @rdname mcc
bal_accuracy <- function(data, ...) {
  UseMethod("bal_accuracy")
}

class(bal_accuracy) <- c("class_metric", "function")

#' @export
#' @rdname mcc
bal_accuracy.data.frame <- function(data, truth, estimate,
                                    estimator = NULL, na.rm = TRUE, ...) {

    metric_summarizer(
      metric_nm = "bal_accuracy",
      metric_fn = bal_accuracy_vec,
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      estimator = estimator,
      na.rm = na.rm,
      ... = ...
    )

}

#' @export
bal_accuracy.table <- function(data, estimator = NULL, ...) {

  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "bal_accuracy",
    .estimator = estimator,
    .estimate = bal_accuracy_table_impl(data, estimator)
  )

}

#' @export
bal_accuracy.matrix <- function(data, estimator = NULL, ...) {

  data <- as.table(data)
  bal_accuracy.table(data, estimator)

}

#' @export
#' @rdname mcc
bal_accuracy_vec <- function(truth, estimate, estimator = NULL,
                             na.rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  bal_accuracy_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    bal_accuracy_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = bal_accuracy_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

bal_accuracy_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    bal_accuracy_binary(data)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- bal_accuracy_multiclass(data, estimator)
    weighted.mean(out_vec, w)
  }

}

bal_accuracy_binary <- function(data) {

  # (sens + spec) / 2
  ( recall_binary(data) + spec_binary(data) ) / 2

}

# Urbanowicz 2015 ExSTraCS 2.0 description and evaluation of a scalable learning.pdf
bal_accuracy_multiclass <- function(data, estimator) {
  ( recall_multiclass(data, estimator) + spec_multiclass(data, estimator) ) / 2
}

# Detection prevalence ---------------------------------------------------------

#' @export
#' @rdname mcc
detection_prevalence <- function(data, ...) {
  UseMethod("detection_prevalence")
}

class(detection_prevalence) <- c("class_metric", "function")

#' @export
#' @rdname mcc
detection_prevalence.data.frame <- function(data, truth, estimate,
                                            estimator = NULL,
                                            na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "detection_prevalence",
    metric_fn = detection_prevalence_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
detection_prevalence.table <- function(data, estimator = NULL, ...) {

  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "detection_prevalence",
    .estimator = estimator,
    .estimate = detection_prevalence_table_impl(data, estimator)
  )

}

#' @export
detection_prevalence.matrix <- function(data, estimator = NULL, ...) {

  data <- as.table(data)
  detection_prevalence.table(data, estimator)

}

#' @export
#' @rdname mcc
detection_prevalence_vec <- function(truth, estimate, estimator = NULL,
                                     na.rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  detection_prevalence_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    detection_prevalence_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = detection_prevalence_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

detection_prevalence_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    detection_prevalence_binary(data)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- detection_prevalence_multiclass(data, estimator)
    weighted.mean(out_vec, w)
  }

}

detection_prevalence_binary <- function(data) {

  pos_level <- pos_val(data)
  sum(data[pos_level, ]) / sum(data)

}

detection_prevalence_multiclass <- function(data, estimator) {

  numer <- rowSums(data)
  denom <- rep(sum(data), times = nrow(data))

  if(any(denom <= 0)) {
    res <- rep(NA_real_, times = nrow(data))
    return(res)
  }

  if(is_micro(estimator)) {
    numer <- sum(numer)
    denom <- sum(denom)
  }

  numer / denom

}
