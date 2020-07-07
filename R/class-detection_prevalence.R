#' Detection prevalence
#'
#' Detection prevalence is defined as the number of _predicted_ positive events (both
#' true positive and false positive) divided by the total number of predictions.
#'
#' @family class metrics
#' @templateVar metric_fn detection_prevalence
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
detection_prevalence <- function(data, ...) {
  UseMethod("detection_prevalence")
}
detection_prevalence <- new_class_metric(
  detection_prevalence,
  direction = "maximize"
)

#' @export
#' @rdname detection_prevalence
detection_prevalence.data.frame <- function(data, truth, estimate,
                                            estimator = NULL,
                                            na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "detection_prevalence",
    metric_fn = detection_prevalence_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
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
#' @rdname detection_prevalence
detection_prevalence_vec <- function(truth, estimate, estimator = NULL,
                                     na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  detection_prevalence_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    detection_prevalence_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = detection_prevalence_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

detection_prevalence_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    detection_prevalence_binary(data, estimator)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- detection_prevalence_multiclass(data, estimator)
    weighted.mean(out_vec, w)
  }

}

detection_prevalence_binary <- function(data, estimator) {

  pos_level <- pos_val(data, estimator)
  sum(data[pos_level, ]) / sum(data)

}

detection_prevalence_multiclass <- function(data, estimator) {

  numer <- rowSums(data)
  denom <- rep(sum(data), times = nrow(data))

  denom[denom <= 0] <- NA_real_

  if(is_micro(estimator)) {
    numer <- sum(numer)
    denom <- sum(denom)
  }

  numer / denom

}
