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
#' @templateVar fn iic
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
iic <- new_numeric_metric(
  iic,
  direction = "maximize"
)

#' @rdname iic
#' @export
iic.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "iic",
    fn = iic_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname iic
iic_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  iic_impl(truth, estimate, case_weights)
}

iic_impl <- function(truth, estimate, case_weights) {
  deltas <- truth - estimate

  neg <- deltas < 0
  pos <- deltas >= 0

  delta_neg <- deltas[neg]
  delta_pos <- deltas[pos]

  if (is.null(case_weights)) {
    case_weights_neg <- NULL
    case_weights_pos <- NULL
  } else {
    case_weights_neg <- case_weights[neg]
    case_weights_pos <- case_weights[pos]
  }

  # Using a best guess that weighted means are computed from sliced weights
  mae_neg <- yardstick_mean(abs(delta_neg), case_weights = case_weights_neg)
  mae_pos <- yardstick_mean(abs(delta_pos), case_weights = case_weights_pos)

  adjustment <- min(mae_neg, mae_pos) / max(mae_neg, mae_pos)
  correlation <- yardstick_cor(truth, estimate, case_weights = case_weights)

  correlation * adjustment
}
