#' Mean absolute scaled error
#'
#' Calculate the mean absolute scaled error. This metric is _scale independent_
#' and _symmetric_. It is generally used for comparing forecast error in
#' time series settings. Due to the time series nature of this metric, it
#' is neccesary to order observations in ascending order by time.
#'
#' `mase()` is different from most numeric metrics. The original implementation
#' of `mase()` calls for using the _in-sample_ naive mean absolute error to
#' compute scaled errors with. It uses this instead of the out-of-sample error
#' because there is a chance that the out-of-sample error cannot be computed
#' when forecasting a very short horizon (i.e. the out of sample size is only
#' 1 or 2). However, `yardstick` only knows about the out-of-sample `truth` and
#' `estimate` values. Because of this, the out-of-sample error is used in the
#' computation by default. If the in-sample naive mean absolute error is
#' required and known, it can be passed through in the `mae_train` argument
#' and it will be used instead. If the in-sample data is available, the
#' naive mean absolute error can easily be computed with
#' `mae(data, truth, lagged_truth)`.
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
#' Rob J. Hyndman (2006). ANOTHER LOOK AT FORECAST-ACCURACY METRICS FOR
#' INTERMITTENT DEMAND. _Foresight_, 4, 46.
#'
#' @template examples-numeric
#'
#' @export
#'
mase <- function(data, ...) {
  UseMethod("mase")
}
mase <- new_numeric_metric(
  mase,
  direction = "minimize"
)

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
    # Extra argument for mase_impl()
    metric_fn_options = list(mae_train = mae_train, m = m)
  )

}

#' @export
#' @rdname mase
mase_vec <- function(truth, estimate, m = 1L,
                     mae_train = NULL, na_rm = TRUE, ...) {

  mase_impl <- function(truth, estimate, m, mae_train) {

    validate_m(m)
    validate_mae_train(mae_train)

    if (is.null(mae_train)) {
      validate_truth_m(truth, m)
    }

    # use out-of-sample snaive if mae_train is not provided
    if (rlang::is_null(mae_train)) {

      truth_lag <- dplyr::lag(truth, m)
      naive_error <- truth - truth_lag
      mae_denom <- mean(abs(naive_error), na.rm = TRUE)

    }
    else {

      mae_denom <- mae_train

    }

    error <- truth - estimate

    # scaled errors
    q <- error / mae_denom

    mase <- mean(abs(q))

    mase
  }

  metric_vec_template(
    metric_impl = mase_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    mae_train = mae_train,
    m = m
  )

}

validate_m <- function(m) {

  abort_msg <- "`m` must be a single positive integer value."

  if (!rlang::is_integerish(m, n = 1L)) {
    abort(abort_msg)
  }

  if (!(m > 0)) {
    abort(abort_msg)
  }

  invisible(m)
}

validate_mae_train <- function(mae_train) {

  if (is.null(mae_train)) {
    return(invisible(mae_train))
  }

  is_single_numeric <- rlang::is_bare_numeric(mae_train, n = 1L)
  abort_msg <- "`mae_train` must be a single positive numeric value."

  if (!is_single_numeric) {
    abort(abort_msg)
  }

  if (!(mae_train > 0)) {
    abort(abort_msg)
  }

  invisible(mae_train)
}

validate_truth_m <- function(truth, m) {

  if (length(truth) <= m) {
    abort(paste0(
      "`truth` must have a length greater than `m` ",
      "to compute the out-of-sample naive mean absolute error."
    ))
  }

  invisible(truth)
}
