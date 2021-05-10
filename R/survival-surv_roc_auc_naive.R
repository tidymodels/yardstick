#' Naive area under the survival receiver operator curve
#'
#' @description
#' TODO
#'
#' @inheritParams sens
#'
#' @param data A `data.frame` containing the `truth` and `estimate` columns.
#'
#' @param truth A `Surv` object. TODO
#'
#' @param estimate A list of tibbles. TODO
#'
#' @return
#' TODO
#'
#' @export
#' @examples
#' # TODO
surv_roc_auc_naive <- function(data, ...) {
  UseMethod("surv_roc_auc_naive")
}

#' @export
#' @rdname surv_roc_auc_naive
surv_roc_auc_naive.data.frame <- function(data,
                                          truth,
                                          estimate,
                                          na_rm = TRUE,
                                          event_level = yardstick_event_level(),
                                          ...) {
  out <- metric_summarizer(
    metric_nm = "surv_roc_auc_naive",
    metric_fn = surv_roc_auc_naive_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    event_level = event_level
  )

  estimate <- dplyr::pull(out, .estimate)
  out <- dplyr::select(out, -.estimate)

  estimate <- dplyr::select(estimate, .time, .estimate)

  out <- dplyr::bind_cols(out, estimate)

  out
}

# TODO: Should you have to supply a single `time`, which means the _vec
# version could return a single number? Here it returns a data frame, but isn't
# appropriate for `_vec` right now because the `.metric` name and `.estimator`
# columns are wrong.

surv_roc_auc_naive_vec <- function(truth,
                                   estimate,
                                   na_rm = TRUE,
                                   event_level = yardstick_event_level(),
                                   ...) {

  estimator <- finalize_estimator(truth)

  surv_roc_auc_naive_impl <- function(truth, estimate) {
    df <- prepare_naive_surv_tbl(truth, estimate)
    df <- dplyr::group_by(df, .time)

    out <- roc_auc(
      data = df,
      truth = indicator,
      pred_event,
      na_rm = na_rm,
      event_level = event_level
    )

    out
  }

  metric_vec_template(
    metric_impl = surv_roc_auc_naive_impl,
    truth = truth,
    estimate = estimate,
    na_rm = "skip",
    estimator = estimator,
    cls = c("Surv", "list")
  )
}
