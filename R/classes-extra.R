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
#' @inheritSection sens Multiclass
#'
#' @inheritParams sens
#'
#' @author Max Kuhn
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

#' @export
#' @rdname mcc
mcc.data.frame <- function(data, truth, estimate, averaging = NULL,
                           na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "mcc",
    metric_fn = mcc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    averaging = averaging,
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
mcc.table <- function(data, averaging = NULL, ...) {
  check_table(data)

  metric_tibbler(
    .metric = construct_name("mcc", averaging, data),
    .estimate = mcc_table_impl(data, averaging)
  )

}

#' @export
mcc.matrix <- function(data, averaging = NULL, ...) {
  data <- as.table(data)
  mcc.table(data, averaging)
}

#' @export
#' @rdname mcc
mcc_vec <- function(truth, estimate, averaging = NULL, na.rm = TRUE, ...) {

  mcc_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    mcc_table_impl(xtab, averaging)

  }

  metric_vec_template(
    metric_impl = mcc_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...
  )

}

mcc_table_impl <- function(data, averaging) {

  averaging <- finalize_averaging(data, averaging)

  if(is_binary(averaging)) {
    mcc_binary(data)
  } else {
    w <- get_weights(data, averaging)
    out_vec <- mcc_multiclass(data, averaging)
    weighted.mean(out_vec, w)
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

# implement the true multiclass generalization
mcc_multiclass <- function(data, averaging) {

  n     <- sum(data)
  tp    <- diag(data)
  tp_fp <- rowSums(data)
  tp_fn <- colSums(data)

  fp    <- tp_fp - tp
  fn    <- tp_fn - tp
  tn    <- n - (tp + fp + fn)

  tn_fp <- tn + fp
  tn_fn <- tn + fn

  if (
    any(tp_fp == 0) |
    any(tp_fn == 0) |
    any(tn_fp == 0) |
    any(tn_fn == 0)
  ) {
    ret <- rep(NA_real_, times = nrow(data))
    return(ret)
  }

  if(is_micro(averaging)) {
    tp <- sum(tp)
    fp <- sum(fp)
    fn <- sum(fn)
    tn <- sum(tn)
    tp_fp <- sum(tp_fp)
    tp_fn <- sum(tp_fn)
    tn_fp <- sum(tn_fp)
    tn_fn <- sum(tn_fn)
  }

  # mcc calc
  ((tp * tn) - (fp * fn)) / sqrt(tp_fp * tp_fn * tn_fp * tn_fn)

}

# J Index ----------------------------------------------------------------------

#' @export
#' @rdname mcc
j_index <- function(data, ...) {
  UseMethod("j_index")
}

#' @export
#' @rdname mcc
j_index.data.frame <- function(data, truth, estimate,
                               averaging = NULL, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "j_index",
    metric_fn = j_index_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    averaging = averaging,
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
j_index.table <- function(data, averaging = NULL, ...) {
  check_table(data)

  metric_tibbler(
    .metric = construct_name("j_index", averaging, data),
    .estimate = j_index_table_impl(data, averaging)
  )

}

#' @export
j_index.matrix <- function(data, averaging = NULL, ...) {

  data <- as.table(data)
  j_index.table(data, averaging)

}

#' @export
#' @rdname mcc
j_index_vec <- function(truth, estimate, averaging = NULL,
                        na.rm = TRUE, ...) {

  j_index_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    j_index_table_impl(xtab, averaging)

  }

  metric_vec_template(
    metric_impl = j_index_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...
  )

}

j_index_table_impl <- function(data, averaging) {

  averaging <- finalize_averaging(data, averaging)

  if(is_binary(averaging)) {
    j_index_binary(data)
  } else {
    w <- get_weights(data, averaging)
    out_vec <- j_index_multiclass(data, averaging)
    weighted.mean(out_vec, w)
  }

}

j_index_binary <- function(data) {
  # sens + spec - 1
  recall_binary(data) + spec_binary(data) - 1
}

j_index_multiclass <- function(data, averaging) {
  recall_multiclass(data, averaging) + spec_multiclass(data, averaging) - 1
}

# Balanced Accuracy ------------------------------------------------------------

#' @export
#' @rdname mcc
bal_accuracy <- function(data, ...) {
  UseMethod("bal_accuracy")
}

#' @export
#' @rdname mcc
bal_accuracy.data.frame <- function(data, truth, estimate,
                                    averaging = NULL, na.rm = TRUE, ...) {

    metric_summarizer(
      metric_nm = "bal_accuracy",
      metric_fn = bal_accuracy_vec,
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      averaging = averaging,
      na.rm = na.rm,
      ... = ...
    )

}

#' @export
bal_accuracy.table <- function(data, averaging = NULL, ...) {

  ## "truth" in columns, predictions in rows
  check_table(data)

  metric_tibbler(
    .metric = construct_name("bal_accuracy", averaging, data),
    .estimate = bal_accuracy_table_impl(data, averaging)
  )

}

#' @export
bal_accuracy.matrix <- function(data, averaging = NULL, ...) {

  data <- as.table(data)
  bal_accuracy.table(data, averaging)

}

#' @export
#' @rdname mcc
bal_accuracy_vec <- function(truth, estimate, averaging = NULL,
                             na.rm = TRUE, ...) {

  bal_accuracy_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    bal_accuracy_table_impl(xtab, averaging)

  }

  metric_vec_template(
    metric_impl = bal_accuracy_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...
  )

}

bal_accuracy_table_impl <- function(data, averaging) {

  averaging <- finalize_averaging(data, averaging)

  if(is_binary(averaging)) {
    bal_accuracy_binary(data)
  } else {
    w <- get_weights(data, averaging)
    out_vec <- bal_accuracy_multiclass(data, averaging)
    weighted.mean(out_vec, w)
  }

}

bal_accuracy_binary <- function(data) {

  # (sens + spec) / 2
  ( recall_binary(data) + spec_binary(data) ) / 2

}

# Urbanowicz 2015 ExSTraCS 2.0 description and evaluation of a scalable learning.pdf
bal_accuracy_multiclass <- function(data, averaging) {
  ( recall_multiclass(data, averaging) + spec_multiclass(data, averaging) ) / 2
}

# Detection prevalence ---------------------------------------------------------

#' @export
#' @rdname mcc
detection_prevalence <- function(data, ...) {
  UseMethod("detection_prevalence")
}

#' @export
#' @rdname mcc
detection_prevalence.data.frame <- function(data, truth, estimate,
                                            averaging = NULL,
                                            na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "detection_prevalence",
    metric_fn = detection_prevalence_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    averaging = averaging,
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
detection_prevalence.table <- function(data, averaging = NULL, ...) {

  ## "truth" in columns, predictions in rows
  check_table(data)

  metric_tibbler(
    .metric = construct_name("detection_prevalence", averaging, data),
    .estimate = detection_prevalence_table_impl(data, averaging)
  )

}

#' @export
detection_prevalence.matrix <- function(data, averaging = NULL, ...) {

  data <- as.table(data)
  detection_prevalence.table(data, averaging)

}

#' @export
#' @rdname mcc
detection_prevalence_vec <- function(truth, estimate, averaging = NULL,
                                     na.rm = TRUE, ...) {

  detection_prevalence_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    detection_prevalence_table_impl(xtab, averaging)

  }

  metric_vec_template(
    metric_impl = detection_prevalence_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...
  )

}

detection_prevalence_table_impl <- function(data, averaging) {

  averaging <- finalize_averaging(data, averaging)

  if(is_binary(averaging)) {
    detection_prevalence_binary(data)
  } else {
    w <- get_weights(data, averaging)
    out_vec <- detection_prevalence_multiclass(data, averaging)
    weighted.mean(out_vec, w)
  }

}

detection_prevalence_binary <- function(data) {

  pos_level <- pos_val(data)
  sum(data[pos_level, ]) / sum(data)

}

detection_prevalence_multiclass <- function(data, averaging) {

  numer <- rowSums(data)
  denom <- rep(sum(data), times = nrow(data))

  if(any(denom <= 0)) {
    res <- rep(NA_real_, times = nrow(data))
    return(res)
  }

  if(is_micro(averaging)) {
    numer <- sum(numer)
    denom <- sum(denom)
  }

  numer / denom

}
