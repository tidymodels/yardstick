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

    symmetry_check <- mean(abs(deltas[deltas > 0]))

    if (isTRUE(mae_neg == symmetry_check)) {
      warning("Index of ideality of correlation is useless when
          outliers are symmetric.")
    }

    adjustment <- min(mae_neg, mae_pos) / max(mae_neg, mae_pos)

    cor(truth, estimate) * adjustment
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
