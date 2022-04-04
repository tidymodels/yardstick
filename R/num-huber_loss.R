#' Huber loss
#'
#' Calculate the Huber loss, a loss function used in robust regression. This
#' loss function is less sensitive to outliers than [rmse()]. This function is
#' quadratic for small residual values and linear for large residual values.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn huber_loss
#' @template return
#'
#' @inheritParams rmse
#'
#' @param delta A single `numeric` value. Defines the boundary where the loss function
#' transitions from quadratic to linear. Defaults to 1.
#'
#' @author James Blair
#'
#' @references
#'
#' Huber, P. (1964). Robust Estimation of a Location Parameter.
#' _Annals of Statistics_, 53 (1), 73-101.
#'
#' @template examples-numeric
#'
#' @export
huber_loss <- function(data, ...) {
  UseMethod("huber_loss")
}
huber_loss <- new_numeric_metric(
  huber_loss,
  direction = "minimize"
)

#' @rdname huber_loss
#' @export
huber_loss.data.frame <- function(data,
                                  truth,
                                  estimate,
                                  delta = 1,
                                  na_rm = TRUE,
                                  case_weights = NULL,
                                  ...) {
  metric_summarizer(
    metric_nm = "huber_loss",
    metric_fn = huber_loss_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    # Extra argument for huber_loss_impl()
    metric_fn_options = list(delta = delta)
  )
}

#' @export
#' @rdname huber_loss
huber_loss_vec <- function(truth,
                           estimate,
                           delta = 1,
                           na_rm = TRUE,
                           case_weights = NULL,
                           ...) {
  metric_vec_template(
    metric_impl = huber_loss_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    case_weights = case_weights,
    cls = "numeric",
    delta = delta
  )
}

huber_loss_impl <- function(truth,
                            estimate,
                            ...,
                            delta = 1,
                            case_weights = NULL) {
  # Weighted Huber Loss implementation confirmed against matlab:
  # https://www.mathworks.com/help/deeplearning/ref/dlarray.huber.html

  if (!rlang::is_bare_numeric(delta, n = 1L)) {
    abort("`delta` must be a single numeric value.")
  }
  if (!(delta >= 0)) {
    abort("`delta` must be a positive value.")
  }

  a <- truth - estimate
  abs_a <- abs(a)

  loss <- ifelse(
    abs_a <= delta,
    0.5 * a^2,
    delta * (abs_a - 0.5 * delta)
  )

  yardstick_mean(loss, case_weights = case_weights)
}
