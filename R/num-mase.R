#' Mean absolute scaled error
#'
#' Calculate the mean absolute scaled error. This metric is _scale independent_ and _symmetric_.
#' It is ideal for comparing forecast error in time series settings. Due to the time series nature
#' of this error metric it is neccesary to order observation ascending by time.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn mase
#' @template return
#'
#' @inheritParams rmse
#'
#' @param is_insample A logical vector indicating if an observation is considered insample (TRUE)
#' or outsample (FALSE)
#'
#' @param m An integer value of lags used to calculate the insample seasonal/nonseasonal naive error.
#' For example m = 1L for non seasonal time series. If each observation was at the daily level and
#' the data should weekly seasonality then m = 7L would be a reasonable choice for a 7-day seasonal
#' naive calculation.
#'
#' @author Alex Hallam
#'
#' #' @references
#'
#' Rob J. Hyndman (2006). ANOTHER LOOK AT FORECAST-ACCURACY METRICS OR INTERMITTENT DEMAND.
#' _Foresight_, 4, 46.
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
mase.data.frame <- function(data, truth, estimate, is_insample, m = 1, na_rm = TRUE, ...) {
  metric_summarizer(
    metric_nm = "mase",
    metric_fn = mase_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ... = ...,
    # Extra argument for huber_loss_impl()
    metric_fn_options = list(is_insample = !!enquo(is_insample), m = 1)
  )
}

#' @export
#' @rdname mase
mase_vec <- function(truth, estimate, is_insample, m = 1, na_rm = TRUE, ...) {
  mase_impl <- function(truth, estimate, is_insample, m = 1) {
    if (!rlang::is_bare_logical(is_insample, n = length(truth))) {
      abort("`is_insample` must be a logical vector.")
    }

    if (!rlang::is_bare_numeric(m, n = 1L)) {
      abort("`m` must be a single integer value.")
    }

    mase_tibble <- tibble(truth, estimate, is_insample)

    out_truth <- mase_tibble[mase_tibble$is_insample == FALSE, ]$truth
    out_estimate <- mase_tibble[mase_tibble$is_insample == FALSE, ]$estimate

    in_truth <- mase_tibble[mase_tibble$is_insample == TRUE, ]$truth
    in_snaive <- lag(in_truth, m)

    out_mae <- mean(abs(out_truth - out_estimate))
    in_snaive_mae <- mean(abs(in_truth - in_snaive), na.rm = TRUE) # removes the na values from m lags

    mase <- out_mae / in_snaive_mae
    mase
  }
  metric_vec_template(
    metric_impl = mase_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...,
    is_insample = is_insample,
    m = m
  )
}




