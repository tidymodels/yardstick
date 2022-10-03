#' Mean log loss for Poisson data
#'
#' Calculate the loss function for the Poisson distribution.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn poisson_log_loss
#' @template return
#'
#' @inheritParams rmse
#'
#' @param truth The column identifier for the true counts (that is `integer`).
#'   This should be an unquoted column name although this argument is passed by
#'   expression and supports [quasiquotation][rlang::quasiquotation] (you can
#'   unquote column names). For `_vec()` functions, an `integer` vector.
#'
#' @author Max Kuhn
#'
#' @template examples-counts
#'
#' @export
#'
poisson_log_loss <- function(data, ...) {
  UseMethod("poisson_log_loss")
}
poisson_log_loss <- new_numeric_metric(
  poisson_log_loss,
  direction = "minimize"
)

#' @rdname poisson_log_loss
#' @export
poisson_log_loss.data.frame <- function(data,
                                        truth,
                                        estimate,
                                        na_rm = TRUE,
                                        case_weights = NULL,
                                        ...) {
  numeric_metric_summarizer(
    metric_nm = "poisson_log_loss",
    metric_fn = poisson_log_loss_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname poisson_log_loss
poisson_log_loss_vec <- function(truth,
                                 estimate,
                                 na_rm = TRUE,
                                 case_weights = NULL,
                                 ...) {
  metric_vec_template(
    metric_impl = poisson_log_loss_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    case_weights = case_weights,
    cls = "numeric"
  )
}

poisson_log_loss_impl <- function(truth,
                                  estimate,
                                  ...,
                                  case_weights = NULL) {
  check_dots_empty()

  if (!is.integer(truth)) {
    truth <- as.integer(truth)
  }

  loss <- -stats::dpois(truth, estimate, log = TRUE)

  yardstick_mean(loss, case_weights = case_weights)
}
