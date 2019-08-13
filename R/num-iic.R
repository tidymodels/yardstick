#' Index of ideality of correlation
#'
#' @description
#'
#' Calculate the index of ideality of correlation. This metric has been
#' studied in QSPR/QSAR models as a good criterion for the predictive
#' potential of these models. It is highly dependent on the correlation
#' coefficient as well as the mean absolute error.
#'
#' Note the application of IIC is useless under two conditions:
#'
#'   * When the negative mean absolute error and positive mean absolute
#'     error are both zero.
#'
#'   * When the outliers are symmetric. Since outliers are context
#'     dependent, please use your own checks to validate whether this
#'     restriction holds and whether the resulting IIC has
#'     interpretative value.
#'
#' The IIC is seen as an alternative to the traditional correlation
#' coefficient and is in the same units as the original data.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn iic
#' @template return
#'
#' @inheritParams rmse
#'
#' @references Toropova, A. and Toropov, A. (2017). "The index of ideality
#'   of correlation. A criterion of predictability of QSAR models for skin
#'   permeability?" _Science of the Total Environment_. 586: 466-472.
#'
#' @author Joyce Cahoon
#'
#' @template examples-numeric
#'
#' @export
iic <- function(data, ...) {
  UseMethod("iic")
}

class(iic) <- c("numeric_metric", "function")

#' @rdname iic
#' @export
iic.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "iic",
    metric_fn = iic_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
#' @rdname iic
iic_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  iic_impl <- function(truth, estimate) {
    deltas <- truth - estimate

    delta_neg <- deltas[deltas < 0]
    delta_pos <- deltas[deltas >= 0]

    mae_neg <- mean(abs(delta_neg))
    mae_pos <- mean(abs(delta_pos))

    delta_strictly_pos <- delta_pos[delta_pos > 0]
    mae_strictly_pos <- mean(abs(delta_strictly_pos))

    if (isTRUE(mae_neg == mae_strictly_pos)) {
      warning("Index of ideality of correlation is useless when outliers are symmetric.")
    }

    adjustment <- min(mae_neg, mae_pos) / max(mae_neg, mae_pos)

    try_cor(truth, estimate) * adjustment
  }

  metric_vec_template(
    metric_impl = iic_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )

}

try_cor <- function(truth, estimate) {
  handler <- make_cor_handler(truth, estimate)

  withCallingHandlers(
    expr = cor(truth, estimate),
    simpleWarning = handler
  )
}

make_cor_handler <- function(truth, estimate) {
  handle_zero_variance <- function(cnd) {
    if (cnd$message != "the standard deviation is zero") {
      return(invisible())
    }

    n_unique_truth <- length(unique(truth))
    n_unique_estimate <- length(unique(estimate))

    if (n_unique_truth == 1L) {
      warn_correlation_undefined_constant_truth(truth)
      rlang::cnd_muffle(cnd)
    }

    if (n_unique_estimate == 1L) {
      warn_correlation_undefined_constant_estimate(estimate)
      rlang::cnd_muffle(cnd)
    }

    invisible()
  }

  handle_zero_variance
}

warn_correlation_undefined_constant_truth <- function(truth) {
  warn_correlation_undefined(
    what = "truth",
    truth = truth,
    .subclass = "yardstick_warning_correlation_undefined_constant_truth"
  )
}

warn_correlation_undefined_constant_estimate <- function(estimate) {
  warn_correlation_undefined(
    what = "estimate",
    estimate = estimate,
    .subclass = "yardstick_warning_correlation_undefined_constant_estimate"
  )
}

warn_correlation_undefined <- function(what, ..., .subclass = character()) {
  message <- paste0(
    "A correlation computation is required, but `", what, "` is constant ",
    "and has 0 standard deviation, resulting in a divide by 0 error. ",
    "`NA` will be returned."
  )

  rlang::warn(
    message = message,
    .subclass = c(.subclass, "yardstick_warning_correlation_undefined"),
    ...
  )
}

