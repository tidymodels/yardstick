#' Mean absolute scaled error
#'
#' Calculate the mean absolute scaled error. This metric is _scale independent_
#' and _symmetric_. It is generally used for comparing forecast error in
#' time series settings. Due to the time series nature of this metric, it
#' is neccesary to order observations in ascending order by time.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn mase
#' @template return
#'
#' @inheritParams rmse
#'
#' @param mae_train A numeric value which allows the user to provide the
#' in-sample seasonal naive mean absolute error. If this value is not provided,
#' then the out-of-sample seasonal naive mean absolute error will be calculated
#' from `truth` and will be used instead.
#'
#' @param m An integer value of the number of lags used to calculate the
#' in-sample seasonal naive error. The default is used for non-seasonal time
#' series. If each observation was at the daily level and the data showed weekly
#' seasonality, then `m = 7L` would be a reasonable choice for a 7-day seasonal
#' naive calculation.
#'
#' @author Alex Hallam
#'
#' @references
#'
#' Rob J. Hyndman (2006). ANOTHER LOOK AT FORECAST-ACCURACY METRICS OR
#' INTERMITTENT DEMAND. _Foresight_, 4, 46.
#'
#' @template examples-numeric
#'
#' @export
#'
mase <- function(data, ...) {
  UseMethod("mase")
}

class(mase) <- c("numeric_metric", "function")

#' @rdname mase
#' @export
mase.data.frame <- function(data, truth, estimate, m = 1L,
                            mae_train = NULL, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "mase",
    metric_fn = mase_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ... = ...,
    # Extra argument for huber_loss_impl()
    metric_fn_options = list(mae_train = mae_train, m = m)
  )

}

#' @export
#' @rdname mase
mase_vec <- function(truth, estimate, m = 1L,
                     mae_train = NULL, na_rm = TRUE, ...) {

  mase_impl <- function(truth, estimate, m = 1L, mae_train = NULL) {

    if (!is.integer(m)) {
      abort("`m` must be a single integer value.")
    }

    if (!(m >= 0)) {
      abort("`delta` must be a positive value.")
    }

    # use out-sample snaive if mae_train is not provided
    if (rlang::is_null(mae_train)){
    snaive <- dplyr::lag(truth, m)
    }else{
    snaive <- mae_train
    }

    e <- truth - estimate
    snaive_mae <- mean(abs(truth - snaive), na.rm = TRUE)
    q <- e / snaive_mae
    mase <- mean(abs(q), na.rm = TRUE)
    mase
  }

  metric_vec_template(
    metric_impl = mase_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...,
    mae_train = mae_train,
    m = m
  )

}
