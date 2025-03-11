#' Psuedo-Huber Loss
#'
#' Calculate the Pseudo-Huber Loss, a smooth approximation of [huber_loss()].
#' Like [huber_loss()], this is less sensitive to outliers than [rmse()].
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar fn huber_loss_pseudo
#' @template return
#'
#' @inheritParams huber_loss
#'
#' @author James Blair
#'
#' @references
#'
#' Huber, P. (1964). Robust Estimation of a Location Parameter.
#' _Annals of Statistics_, 53 (1), 73-101.
#'
#' Hartley, Richard (2004). Multiple View Geometry in Computer Vision.
#' (Second Edition). Page 619.
#'
#' @template examples-numeric
#'
#' @export
huber_loss_pseudo <- function(data, ...) {
  UseMethod("huber_loss_pseudo")
}
huber_loss_pseudo <- new_numeric_metric(
  huber_loss_pseudo,
  direction = "minimize"
)

#' @rdname huber_loss_pseudo
#' @export
huber_loss_pseudo.data.frame <- function(
  data,
  truth,
  estimate,
  delta = 1,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "huber_loss_pseudo",
    fn = huber_loss_pseudo_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    # Extra argument for huber_loss_pseudo_impl()
    fn_options = list(delta = delta)
  )
}

#' @export
#' @rdname huber_loss_pseudo
huber_loss_pseudo_vec <- function(
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

  huber_loss_pseudo_impl(
    truth = truth,
    estimate = estimate,
    delta = delta,
    case_weights = case_weights
  )
}

huber_loss_pseudo_impl <- function(
  truth,
  estimate,
  delta,
  case_weights,
  call = caller_env()
) {
  check_number_decimal(delta, min = 0, call = call)

  a <- truth - estimate
  loss <- delta^2 * (sqrt(1 + (a / delta)^2) - 1)

  yardstick_mean(loss, case_weights = case_weights)
}
