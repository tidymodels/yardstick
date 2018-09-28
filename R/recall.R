#' Calculate recall, precision and F values
#'
#' These functions calculate the recall, precision or F values of
#'  a measurement system for finding/retrieving relevant documents
#'  compared to reference results (the truth regarding relevance).
#'  The measurement and "truth" data must have the same two possible
#'  outcomes and one of the outcomes must be thought of as
#'  "relevant" results.
#'
#' The recall (aka specificity) is defined as the proportion of
#'  relevant results out of the number of samples which were
#'  actually relevant. When there are no relevant results, recall is
#'  not defined and a value of `NA` is returned.
#'
#' The precision is the percentage of predicted truly relevant results
#'  of the total number of predicted relevant results and
#'  characterizes the "purity in retrieval performance" (Buckland
#'  and Gey, 1994).
#'
#' The measure "F" is a combination of precision and recall (see
#'  below).
#'
#' There is no common convention on which factor level should
#'  automatically be considered the relevant result.
#'  In `yardstick`, the default is to use the _first_ level. To
#'  change this, a global option called `yardstick.event_first` is
#'  set to `TRUE` when the package is loaded. This can be changed
#'  to `FALSE` if the last level of the factor is considered the
#'  level of interest.
#'
#' Suppose a 2x2 table with notation:
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Relevant \tab
#' Irrelevant \cr Relevant \tab A \tab B \cr Irrelevant \tab C \tab D \cr }
#'
#' The formulas used here are: \deqn{recall = A/(A+C)} \deqn{precision =
#' A/(A+B)} \deqn{F_i = (1+i^2)*prec*recall/((i^2 * precision)+recall)}
#'
#' See the references for discussions of the statistics.
#'
#' If more than one statistic is required, it is more
#'  computationally efficient to create the confusion matrix using
#'  [conf_mat()] and applying the corresponding `summary` method
#'  ([summary.conf_mat()]) to get the values at once.
#'
#' @inheritParams sens
#' @aliases recall recall.default recall.table precision
#'  precision.default precision.table precision.matrix f_meas
#'  f_meas.default f_meas.table
#' @param beta A numeric value used to weight precision and
#'  recall. A value of 1 is traditionally used and corresponds to
#'  the harmonic mean of the two values but other values weight
#'  recall beta times more important than precision.
#' @seealso [conf_mat()], [summary.conf_mat()], [sens()], [mcc()]
#' @references Buckland, M., & Gey, F. (1994). The relationship
#'  between Recall and Precision. *Journal of the American Society
#'  for Information Science*, 45(1), 12-19.
#'
#' Powers, D. (2007). Evaluation: From Precision, Recall and F
#'  Factor to ROC, Informedness, Markedness and Correlation.
#'  Technical Report SIE-07-001, Flinders University
#' @keywords manip
#' @examples
#' data("two_class_example")
#'
#' # Different methods for calling the functions:
#' precision(two_class_example, truth = truth, estimate = predicted)
#'
#' recall(two_class_example, truth = "truth", estimate = "predicted")
#'
#' truth_var <- quote(truth)
#' f_meas(two_class_example, !! truth_var, predicted)
#' @export recall
recall <- function(data, ...) {
  UseMethod("recall")
}

#' @rdname recall
#' @export
recall.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "recall",
    metric_fn = recall_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @rdname recall
#' @export
recall.table <- function(data, ...) {

  check_table(data)

  metric_tibbler(
    .metric = "recall",
    .estimate = recall_table_impl(data)
  )

}

#' @export
#' @rdname recall
recall_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  recall_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = na.rm,
      dnn = c("Prediction", "Truth"),
      ...
    )
    recall_table_impl(xtab)

  }

  metric_vec_template(
    metric_impl = recall_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...
  )

}

recall_table_impl <- function(data) {

  relevant <- pos_val(data)

  numer <- data[relevant, relevant]
  denom <- sum(data[, relevant])
  rec <- ifelse(denom > 0, numer / denom, NA)
  rec

}

#' @rdname recall
#' @export
precision <- function(data, ...) {
  UseMethod("precision")
}

#' @rdname recall
#' @export
precision.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "precision",
    metric_fn = precision_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @rdname recall
#' @export
precision.table <- function (data, ...) {

  check_table(data)

  metric_tibbler(
    .metric = "precision",
    .estimate = precision_table_impl(data)
  )

}

#' @export
#' @rdname recall
precision_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  precision_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = na.rm,
      dnn = c("Prediction", "Truth"),
      ...
    )
    precision_table_impl(xtab)

  }

  metric_vec_template(
    metric_impl = precision_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...
  )

}

precision_table_impl <- function(data) {

  relevant <- pos_val(data)
  numer <- data[relevant, relevant]
  denom <- sum(data[relevant, ])
  precision <- ifelse(denom > 0, numer / denom, NA)
  precision

}

#' @rdname recall
#' @export
f_meas <- function(data, ...) {
  UseMethod("f_meas")
}

#' @rdname recall
#' @export
f_meas.data.frame <- function(data, truth, estimate, beta = 1, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "f_meas",
    metric_fn = f_meas_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...,
    metric_fn_options = list(beta = beta)
  )

}

#' @rdname recall
#' @export
f_meas.table <- function (data, beta = 1, ...) {
  check_table(data)

  metric_tibbler(
    .metric = "f_meas",
    .estimate = f_meas_table_impl(data, beta = beta)
  )
}

#' @export
#' @rdname recall
f_meas_vec <- function(truth, estimate, beta = 1, na.rm = TRUE, ...) {

  f_meas_impl <- function(truth, estimate, beta) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = na.rm,
      dnn = c("Prediction", "Truth"),
      ...
    )
    f_meas_table_impl(xtab, beta = beta)

  }

  metric_vec_template(
    metric_impl = f_meas_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "factor",
    ...,
    beta = beta
  )

}

f_meas_table_impl <- function(data, beta = 1) {

  relevant <- pos_val(data)
  precision <- precision_table_impl(data)
  rec <- recall_table_impl(data)
  (1 + beta ^ 2) * precision * rec / ((beta ^ 2 * precision) + rec)

}
