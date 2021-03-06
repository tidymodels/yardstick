#' Balanced accuracy
#'
#' Balanced accuracy is computed here as the average of [sens()] and [spec()].
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
bal_accuracy <- new_class_metric(
  bal_accuracy,
  direction = "maximize"
)

#' @export
#' @rdname bal_accuracy
bal_accuracy.data.frame <- function(data,
                                    truth,
                                    estimate,
                                    estimator = NULL,
                                    na_rm = TRUE,
                                    event_level = yardstick_event_level(),
                                    ...) {

  metric_summarizer(
    metric_nm = "bal_accuracy",
    metric_fn = bal_accuracy_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level
  )

}

#' @export
bal_accuracy.table <- function(data,
                               estimator = NULL,
                               event_level = yardstick_event_level(),
                               ...) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "bal_accuracy",
    .estimator = estimator,
    .estimate = bal_accuracy_table_impl(data, estimator, event_level)
  )
}

#' @export
bal_accuracy.matrix <- function(data,
                                estimator = NULL,
                                event_level = yardstick_event_level(),
                                ...) {
  data <- as.table(data)
  bal_accuracy.table(data, estimator, event_level)
}

#' @export
#' @rdname bal_accuracy
bal_accuracy_vec <- function(truth,
                             estimate,
                             estimator = NULL,
                             na_rm = TRUE,
                             event_level = yardstick_event_level(),
                             ...) {
  estimator <- finalize_estimator(truth, estimator)

  bal_accuracy_impl <- function(truth, estimate) {
    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    bal_accuracy_table_impl(xtab, estimator, event_level)
  }

  metric_vec_template(
    metric_impl = bal_accuracy_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = "factor"
  )
}

bal_accuracy_table_impl <- function(data, estimator, event_level) {
  if(is_binary(estimator)) {
    bal_accuracy_binary(data, event_level)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- bal_accuracy_multiclass(data, estimator)
    weighted.mean(out_vec, w)
  }
}

bal_accuracy_binary <- function(data, event_level) {
  ( sens_binary(data, event_level) + spec_binary(data, event_level) ) / 2
}

# Urbanowicz 2015 ExSTraCS 2.0 description and evaluation of a scalable learning.pdf
bal_accuracy_multiclass <- function(data, estimator) {
  ( recall_multiclass(data, estimator) + spec_multiclass(data, estimator) ) / 2
}
