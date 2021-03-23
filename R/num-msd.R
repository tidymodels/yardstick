#' Mean signed deviation
#'
#' @description
#' Mean signed deviation (also known as mean signed difference, or mean signed
#' error) computes the average differences between `truth` and `estimate`. A
#' related metric is the mean absolute error ([mae()]).
#'
#' @details
#' Mean signed deviation is rarely used, since positive and negative errors
#' cancel each other out. For example, `msd_vec(c(100, -100), c(0, 0))` would
#' return a seemingly "perfect" value of `0`, even though `estimate` is wildly
#' different from `truth`. [mae()] attempts to remedy this by taking the
#' absolute value of the differences before computing the mean.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn msd
#' @template return
#'
#' @inheritParams rmse
#'
#' @author Thomas Bierhance
#'
#' @template examples-numeric
#'
#' @export
msd <- function(data, ...) {
  UseMethod("msd")
}
msd <- new_numeric_metric(
  msd,
  direction = "zero"
)

#' @rdname msd
#' @export
msd.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "msd",
    metric_fn = msd_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
#' @rdname msd
msd_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  msd_impl <- function(truth, estimate) {
    mean(estimate - truth)
  }

  metric_vec_template(
    metric_impl = msd_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )

}
