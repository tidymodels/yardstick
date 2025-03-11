#' Mean absolute scaled error
#'
#' Calculate the mean absolute scaled error. This metric is _scale independent_
#' and _symmetric_. It is generally used for comparing forecast error in
#' time series settings. Due to the time series nature of this metric, it
#' is necessary to order observations in ascending order by time.
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
#' @templateVar fn mase
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
mase.data.frame <- function(
  data,
  truth,
  estimate,
  m = 1L,
  mae_train = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "mase",
    fn = mase_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    # Extra argument for mase_impl()
    fn_options = list(mae_train = mae_train, m = m)
  )
}

#' @export
#' @rdname mase
mase_vec <- function(
  truth,
  estimate,
  m = 1L,
  mae_train = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  mase_impl(
    truth = truth,
    estimate = estimate,
    m = m,
    mae_train = mae_train,
    case_weights = case_weights
  )
}

mase_impl <- function(
  truth,
  estimate,
  m = 1L,
  mae_train = NULL,
  case_weights = NULL,
  call = caller_env()
) {
  check_number_whole(m, min = 0, call = call)
  check_number_decimal(mae_train, min = 0, allow_null = TRUE, call = call)

  if (is.null(mae_train)) {
    validate_truth_m(truth, m, call = call)
  }

  # Use out-of-sample snaive if mae_train is not provided
  if (is.null(mae_train)) {
    truth_lag <- dplyr::lag(truth, m)
    naive_error <- truth - truth_lag
    mae_denom <- mean(abs(naive_error), na.rm = TRUE)
  } else {
    mae_denom <- mae_train
  }

  error <- truth - estimate
  error <- error / mae_denom
  error <- abs(error)

  out <- yardstick_mean(error, case_weights = case_weights)

  out
}

validate_truth_m <- function(truth, m, call = caller_env()) {
  if (length(truth) <= m) {
    cli::cli_abort(
      "{.arg truth} ({length(truth)}) must have a length greater than
      {.arg m} ({m}) to compute the out-of-sample naive mean absolute error.",
      call = call
    )
  }
}
