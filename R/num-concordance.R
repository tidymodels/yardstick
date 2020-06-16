#' C-index/Concordance statistic
#'
#' Calculate the concordance statistic. [c_index()] is a metric of
#'  agreement between an observed response and a predictor. It is equal to the
#'  area under the Receiver Operating Characteristic (ROC) curve and ranges
#'  from 0.5 to 1.
#'
#' For more details see the help vignette:
#' \code{vignette("concordance", package = "survival")}
#'
#' @family numeric metrics
#' @templateVar metric_fn c_index
#' @template return
#'
#' @inheritParams rmse
#'
#' @author Emil Hvitfeldt
#'
#' @references
#'
#' Frank E Harrell, Kerry L Lee, and Daniel B Mark. Multivariable prognostic
#'  models: issues in developing models, evaluating assumptions and adequacy,
#'  and measuring and reducing errors. Stat. in Medicine, 15:361–387, 1996.
#'
#' @template examples-numeric
#'
#' @export
#'
c_index <- function(data, ...) {
  UseMethod("c_index")
}

class(c_index) <- c("numeric_metric", "function")
attr(c_index, "direction") <- "maximize"

#' @rdname c_index
#' @export
c_index.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "c_index",
    metric_fn = c_index_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
#' @rdname c_index
#' @importFrom stats var
c_index_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  c_index_impl <- function(truth, estimate) {

    df <- data.frame(truth = truth, estimate = estimate)
    survival::concordance(truth ~ estimate, data = df)$concordance

  }

  metric_vec_template(
    metric_impl = c_index_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )

}
