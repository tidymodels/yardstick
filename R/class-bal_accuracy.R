#' Balanced accuracy
#'
#' @family class metrics
#' @templateVar metric_fn bal_accuracy
#' @template event_first
#' @template multiclass
#' @template return
#'
#' @inheritParams sens
#'
#' @author Max Kuhn
#'
#' @template examples-class
#'
#' @export
bal_accuracy <- function(data, ...) {
  UseMethod("bal_accuracy")
}

class(bal_accuracy) <- c("class_metric", "function")

#' @export
#' @rdname bal_accuracy
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
#' @rdname bal_accuracy
bal_accuracy_vec <- function(truth, estimate, estimator = NULL,
                             na.rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  bal_accuracy_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
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
