#' Gini coefficient
#'
#' `gini()` is a metric that computes the Gini coefficient for measuring the
#' discriminatory power of a predictive model. Also referred to as accuracy
#' ratio (AR), the Gini coefficient is a rescale and shift of the Area Under the ROC
#' curve (AUC): \eqn{Gini + 1 = 2AUC}.
#'
#' See \code{?roc_auc} for an explanation of computing AUC for the multiclass case.
#'
#' @family class probability metrics
#' @templateVar metric_fn gini
#' @template return
#' @template event_first
#'
#' @inheritParams roc_auc
#'
#' @references
#'
#' Engelmann, Bernd & Hayden, Evelyn & Tasche, Dirk (2003).
#' "Measuring the Discriminative Power of Rating Systems,"
#' Discussion Paper Series 2: Banking and Financial Studies 2003,01, Deutsche Bundesbank.
#'
#' Hand, Till (2001). "A Simple Generalisation of the Area Under the
#' ROC Curve for Multiple Class Classification Problems". _Machine Learning_.
#' Vol 45, Iss 2, pp 171-186.
#'
#' @seealso
#'
#' [roc_auc()] for computing the area under the ROC curve.
#'
#' @template examples-prob
#' @examples
#' two_class_example %>% gini(truth, Class1)
#'
#' @export
gini <- function(data, ...) {
  UseMethod("gini")
}

class(gini) <- c("prob_metric", "function")

#' @export
#' @rdname gini
gini.data.frame  <- function(data, truth, ..., options = list(),
                             estimator = NULL, na_rm = TRUE) {


  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "gini",
    metric_fn = gini_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    estimator = estimator,
    na_rm = na_rm,
    ... = ...,
    metric_fn_options = list(options = options)
  )

}

#' @rdname gini
#' @export
gini_vec <- function(truth, estimate, options = list(),
                        estimator = NULL, na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator, "gain_capture")

  gini_impl <- function(truth, estimate) {
    gini_estimator_impl(truth, estimate, options, estimator)
  }

  metric_vec_template(
    metric_impl = gini_impl,
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    na_rm = na_rm,
    cls = c("factor", "numeric"),
    ...
  )
}

gini_estimator_impl <- function(truth, estimate, options, estimator) {

  if(is_binary(estimator)) {
    gini_binary(truth, estimate, options)
  } else {
    truth_table <- matrix(table(truth), nrow = 1)
    w <- get_weights(truth_table, estimator)
    out_vec <- gini_multiclass(truth, estimate, options)
    weighted.mean(out_vec, w)
  }

}

gini_binary <- function(truth, estimate, options) {
  2 * roc_auc_binary(truth, estimate, options) - 1
}

gini_multiclass <- function(truth, estimate, options) {
  2 * roc_auc_multiclass(truth, estimate, options) - 1
}
