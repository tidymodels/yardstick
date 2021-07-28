#'  Mean log loss for Poisson data
#'
#' Calculate the loss function for the Poisson distribution.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn poisson_log_loss
#' @template return
#'
#' @param data A `data.frame` containing the `truth` and `estimate`
#' columns.
#'
#' @param truth The column identifier for the true counts
#' (that is `numeric`). This should be an unquoted column name although
#' this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column
#' names). For `_vec()` functions, a `numeric` vector.
#'
#' @param estimate The column identifier for the predicted
#' results (that is also `numeric`). As with `truth` this can be
#' specified different ways but the primary method is to use an
#' unquoted variable name. For `_vec()` functions, a `numeric` vector.
#'
#' @param na_rm A `logical` value indicating whether `NA`
#' values should be stripped before the computation proceeds.
#'
#' @param ... Not currently used.
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
poisson_log_loss.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "poisson_log_loss",
    metric_fn = poisson_log_loss_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm
  )

}

#' @export
#' @importFrom stats dpois
#' @rdname poisson_log_loss
poisson_log_loss_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  poisson_log_loss_impl <- function(truth, estimate) {
    mean(-stats::dpois(truth, estimate, log = TRUE))
  }

  metric_vec_template(
    metric_impl = poisson_log_loss_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric"
  )

}
