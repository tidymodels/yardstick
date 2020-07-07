#' Mean percentage error
#'
#' Calculate the mean percentage error. This metric is in _relative
#' units_. It can be used as a measure of the `estimate`'s bias.
#'
#' Note that when the observed `truth` value is `0`, a value of:
#' `-Inf` (`estimate > 0`), `Inf` (`estimate < 0`), or `NaN` (`estimate == 0`)
#' is returned for `mpe()`.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn mpe
#' @template return
#'
#' @inheritParams rmse
#'
#' @author Thomas Bierhance
#'
#' @template examples-numeric
#'
#' @export
mpe <- function(data, ...) {
  UseMethod("mpe")
}
mpe <- new_numeric_metric(
  mpe,
  direction = "zero"
)

#' @rdname mpe
#' @export
mpe.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "mpe",
    metric_fn = mpe_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
#' @rdname mpe
mpe_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  mpe_impl <- function(truth, estimate) {
    mean( (truth - estimate) / truth ) * 100
  }

  metric_vec_template(
    metric_impl = mpe_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )

}
