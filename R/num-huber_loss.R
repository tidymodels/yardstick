#' Huber loss
#'
#' Calculate the Huber loss, a loss function used in robust regression. This
#' loss function is less sensitive to outliers than [rmse()]. This function is
#' quadratic for small residual values and linear for large residual values.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar fn huber_loss
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
huber_loss.data.frame <- function(
  data,
  truth,
  estimate,
  delta = 1,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "huber_loss",
    fn = huber_loss_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    # Extra argument for huber_loss_impl()
    fn_options = list(delta = delta)
  )
}

#' @export
#' @rdname huber_loss
huber_loss_vec <- function(
  truth,
  estimate,
  delta = 1,
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

  huber_loss_impl(truth, estimate, delta, case_weights)
}

huber_loss_impl <- function(
  truth,
  estimate,
  delta,
  case_weights,
  call = caller_env()
) {
  # Weighted Huber Loss implementation confirmed against matlab:
  # https://www.mathworks.com/help/deeplearning/ref/dlarray.huber.html

  check_number_decimal(delta, min = 0, call = call)

  a <- truth - estimate
  abs_a <- abs(a)

  loss <- ifelse(
    abs_a <= delta,
    0.5 * a^2,
    delta * (abs_a - 0.5 * delta)
  )

  yardstick_mean(loss, case_weights = case_weights)
}
