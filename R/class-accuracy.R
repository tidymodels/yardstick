#' Accuracy
#'
#' Accuracy is the proportion of the data that are predicted correctly.
#'
#' @family class metrics
#' @templateVar metric_fn accuracy
#' @template return
#'
#' @section Multiclass:
#'
#' Accuracy extends naturally to multiclass scenarios. Because
#' of this, macro and micro averaging are not implemented.
#'
#' @inheritParams sens
#'
#' @author Max Kuhn
#'
#' @template examples-class
#'
#' @export
accuracy <- function(data, ...) {
  UseMethod("accuracy")
}
accuracy <- new_class_metric(
  accuracy,
  direction = "maximize"
)

#' @export
#' @rdname accuracy
accuracy.data.frame <- function(data, truth, estimate,
                                na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "accuracy",
    metric_fn = accuracy_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm
    # do not pass dots through (could allow averaging to be set. unwanted!)
  )

}

#' @export
accuracy.table <- function(data, ...) {
  check_table(data)
  estimator <- finalize_estimator(data, metric_class = "accuracy")
  metric_tibbler(
    .metric = "accuracy",
    .estimator = estimator,
    .estimate = accuracy_table_impl(data)
  )
}

#' @export
accuracy.matrix <- function(data, ...) {
  data <- as.table(data)
  accuracy.table(data)
}

#' @export
#' @rdname accuracy
accuracy_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, metric_class = "accuracy")

  accuracy_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    accuracy_table_impl(xtab)

  }

  metric_vec_template(
    metric_impl = accuracy_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

accuracy_table_impl <- function(data) {
  accuracy_binary(data)
}

# binary and multiclass case are equivalent
accuracy_binary <- function(data) {
  sum(diag(data)) / sum(data)
}
