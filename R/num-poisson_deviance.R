#' Mean deviance residuals for Poisson data
#'
#' Calculate the deviance for the Poisson distribution comparing to a saturaed
#' model.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @templateVar metric_fn poisson_deviance
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
#' @details
#' In general, the deviance statistic is a method for comparing two models
#'  that are fit using maximum likelihood. While the likelihood statistic is
#'  absolute, deviance is defined as a comparative statistic between two models.
#'  In this function, the deviance for a model fit is compared to a model with
#'  perfect fit (i.e., the saturated model).
#'
#' The value returned is the average of the _deviance residuals_ computed using
#' the function provided by [stats::poisson()]. For zero counts, the residual is
#' the predicted value. For non-zero counts, the deviance residual is
#'
#' \deqn{residual = (count * log(count/prediction) - (count - prediction))}
#'
#' The average of these values is returned.
#'
#' @export
#'
poisson_deviance <- function(data, ...) {
  UseMethod("poisson_deviance")
}
poisson_deviance <- new_numeric_metric(
  poisson_deviance,
  direction = "minimize"
)

#' @rdname poisson_deviance
#' @export
poisson_deviance.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "poisson_deviance",
    metric_fn = poisson_deviance_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm
  )

}

#' @export
#' @importFrom stats poisson
#' @rdname poisson_deviance
poisson_deviance_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  poisson_deviance_impl <- function(truth, estimate) {
    mean(stats::poisson()$dev.resids(truth, estimate, wt = 1))
  }

  metric_vec_template(
    metric_impl = poisson_deviance_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric"
  )

}
