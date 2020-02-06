#' Area under the ROC curve of each class against the rest, using the uniform
#' class distribution
#'
#' `roc_aunu()` is a multiclass metric that computes the area under the ROC
#' curve of each class against the rest, using the uniform class distribution.
#' This is equivalent to `roc_auc(estimator = "macro")`.
#'
#' Like the other ROC AUC metrics, `roc_aunu()` defaults to allowing
#' `pROC::roc()` control the direction of the computation, but allows you to
#' control this by passing `options = list(direction = "<")` or any other
#' allowed direction value. pROC advises setting the `direction` when doing
#' resampling so that the AUC values are not biased upwards.
#'
#' Generally, an ROC AUC value is between `0.5` and `1`, with `1` being a
#' perfect prediction model. If your value is between `0` and `0.5`, then
#' this implies that you have meaningful information in your model, but it
#' is being applied incorrectly because doing the opposite of what the model
#' predicts would result in an AUC `>0.5`.
#'
#' @family class probability metrics
#' @templateVar metric_fn roc_aunu
#' @template return
#' @template event_first
#'
#' @section Multiclass:
#' This multiclass method for computing the area under the ROC curve uses the
#' uniform class distribution and is equivalent to
#' `roc_auc(estimator = "macro")`.
#'
#' @inheritParams pr_auc
#'
#' @param ... A set of unquoted column names or one or more `dplyr` selector
#' functions to choose which variables contain the class probabilities. There
#' should be as many columns as factor levels of `truth`.
#'
#' @param estimate A matrix with as many
#' columns as factor levels of `truth`. _It is assumed that these are in the
#' same order as the levels of `truth`._
#'
#' @param options A `list` of named options to pass to [pROC::roc()]
#' such as `direction` or `smooth`. These options should not include `response`,
#' `predictor`, `levels`, or `quiet`.
#'
#' @references
#'
#' Ferri, C., Hernández-Orallo, J., & Modroiu, R. (2009). "An experimental
#' comparison of performance measures for classification". _Pattern Recognition
#' Letters_. 30 (1), pp 27-38.
#'
#' @seealso
#'
#' [roc_aunp()] for computing the area under the ROC curve of each class against
#' the rest, using the a priori class distribution.
#'
#' @author Julia Silge
#'
#' @examples
#' # Multiclass example
#'
#' # `obs` is a 4 level factor. The first level is `"VF"`, which is the
#' # "event of interest" by default in yardstick. See the Relevant Level
#' # section above.
#' data(hpc_cv)
#'
#' # You can use the col1:colN tidyselect syntax
#' library(dplyr)
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   roc_aunu(obs, VF:L)
#'
#' # Change the first level of `obs` from `"VF"` to `"M"` to alter the
#' # event of interest. The class probability columns should be supplied
#' # in the same order as the levels.
#' hpc_cv %>%
#'  filter(Resample == "Fold01") %>%
#'   mutate(obs = relevel(obs, "M")) %>%
#'   roc_aunu(obs, M, VF:L)
#'
#' # Groups are respected
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   roc_aunu(obs, VF:L)
#'
#' # Vector version
#' # Supply a matrix of class probabilities
#' fold1 <- hpc_cv %>%
#'   filter(Resample == "Fold01")
#'
#' roc_aunu_vec(
#'   truth = fold1$obs,
#'   matrix(
#'     c(fold1$VF, fold1$F, fold1$M, fold1$L),
#'     ncol = 4
#'   )
#' )
#'
#' # ---------------------------------------------------------------------------
#' # Options for `pROC::roc()`
#'
#' # Pass options via a named list and not through `...`!
#' roc_aunu(
#'   hpc_cv,
#'   obs,
#'   VF:L,
#'   options = list(smooth = TRUE)
#' )
#'
#' @export
roc_aunu <- function(data, ...) {
  UseMethod("roc_aunu")
}

class(roc_aunu) <- c("prob_metric", "function")
attr(roc_aunu, "direction") <- "maximize"

#' @export
#' @rdname roc_aunu
roc_aunu.data.frame  <- function(data, truth, ..., options = list(),
                                 na_rm = TRUE) {

  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "roc_aunu",
    metric_fn = roc_aunu_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    estimator = NULL,
    na_rm = na_rm,
    ... = ...,
    metric_fn_options = list(options = options)
  )

}

#' @rdname roc_aunu
#' @export
#' @importFrom rlang call2
#' @importFrom pROC roc auc
roc_aunu_vec <- function(truth, estimate, options = list(),
                         na_rm = TRUE, ...) {

  estimator <- "macro"
  estimator <- finalize_estimator(truth, estimator, "roc_auc")

  roc_aunu_impl <- function(truth, estimate) {
    roc_auc_vec(truth, estimate, options, estimator)
  }

  metric_vec_template(
    metric_impl = roc_aunu_impl,
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    na_rm = na_rm,
    cls = c("factor", "numeric"),
    ... = ...
  )
}
