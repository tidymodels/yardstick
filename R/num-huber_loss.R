#' Huber loss
#'
#' Calculate the Huber loss
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn huber_loss
#' @template return
#'
#' @author James Blair
#'
#' @template examples-numeric
#'
#' @export
huber_loss <- function(data, ...) {
  UseMethod("huber_loss")
}

class(huber_loss) <- c("numeric_metric", "function")

#' @rdname huber_loss
#' @export
huber_loss.data.frame <- function(data, truth, estimate, delta = 1, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "huber_loss",
    metric_fn = huber_loss_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ... = ...,
    # Extra argument for huber_loss_impl()
    metric_fn_options = list(delta = delta)
  )

}

#' @export
#' @rdname huber_loss
huber_loss_vec <- function(truth, estimate, delta = 1, na_rm = TRUE, ...) {

  huber_loss_impl <- function(truth, estimate, delta) {
    # Logic for Huber loss formula
    a <- truth - estimate
    mean(
      ifelse(abs(a) <= delta,
             0.5 * a^2,
             delta * (abs(a) - 0.5 * delta)
      )
    )
  }

  metric_vec_template(
    metric_impl = huber_loss_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...,
    delta = delta
  )

}
