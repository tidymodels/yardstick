#' Precision
#'
#' These functions calculate the [precision()] of a measurement system for
#' finding relevant documents compared to reference results
#' (the truth regarding relevance). Highly related functions are [recall()]
#' and [f_meas()].
#'
#' The precision is the percentage of predicted truly relevant results
#' of the total number of predicted relevant results and
#' characterizes the "purity in retrieval performance" (Buckland
#' and Gey, 1994).
#'
#' @family class metrics
#' @family relevance metrics
#' @templateVar metric_fn precision
#' @template event_first
#' @template multiclass
#' @template return
#' @template table-relevance
#'
#' @inheritParams sens
#'
#' @references
#'
#' Buckland, M., & Gey, F. (1994). The relationship
#'  between Recall and Precision. *Journal of the American Society
#'  for Information Science*, 45(1), 12-19.
#'
#' Powers, D. (2007). Evaluation: From Precision, Recall and F
#'  Factor to ROC, Informedness, Markedness and Correlation.
#'  Technical Report SIE-07-001, Flinders University
#'
#' @author Max Kuhn
#'
#' @template examples-class
#'
#' @export
precision <- function(data, ...) {
  UseMethod("precision")
}

class(precision) <- c("class_metric", "function")

#' @rdname precision
#' @export
precision.data.frame <- function(data, truth, estimate,
                                 estimator = NULL,
                                 na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "precision",
    metric_fn = precision_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
precision.table <- function (data, estimator = NULL, ...) {

  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "precision",
    .estimator = estimator,
    .estimate = precision_table_impl(data, estimator)
  )

}

#' @export
precision.matrix <- function(data, estimator = NULL, ...) {

  data <- as.table(data)
  precision.table(data, estimator)

}

#' @export
#' @rdname precision
precision_vec <- function(truth, estimate,
                          estimator = NULL, na.rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  precision_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    precision_table_impl(xtab, estimator)
  }

  metric_vec_template(
    metric_impl = precision_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

precision_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    precision_binary(data)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- precision_multiclass(data, estimator)
    weighted.mean(out_vec, w)
  }

}

precision_binary <- function(data) {

  relevant <- pos_val(data)
  numer <- data[relevant, relevant]
  denom <- sum(data[relevant, ])
  precision <- ifelse(denom > 0, numer / denom, NA_real_)
  precision

}

precision_multiclass <- function(data, estimator) {

  numer <- diag(data)
  denom <- rowSums(data)

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
