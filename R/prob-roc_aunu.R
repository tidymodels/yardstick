#' Area under the ROC curve of each class against the rest, using the uniform
#' class distribution
#'
#' `roc_aunu()` is a multiclass metric that computes the area under the ROC
#' curve of each class against the rest, using the uniform class distribution.
#' This is equivalent to `roc_auc(estimator = "macro")`.
#'
#' @family class probability metrics
#' @templateVar fn roc_aunu
#' @template return
#' @template event_first
#'
#' @section Multiclass:
#' This multiclass method for computing the area under the ROC curve uses the
#' uniform class distribution and is equivalent to
#' `roc_auc(estimator = "macro")`.
#'
#' @inheritParams roc_auc
#'
#' @param ... A set of unquoted column names or one or more `dplyr` selector
#' functions to choose which variables contain the class probabilities. There
#' should be as many columns as factor levels of `truth`.
#'
#' @param estimate A matrix with as many
#' columns as factor levels of `truth`. _It is assumed that these are in the
#' same order as the levels of `truth`._
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
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   roc_aunu(obs, VF:L)
#'
#' # Change the first level of `obs` from `"VF"` to `"M"` to alter the
#' # event of interest. The class probability columns should be supplied
#' # in the same order as the levels.
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   mutate(obs = relevel(obs, "M")) |>
#'   roc_aunu(obs, M, VF:L)
#'
#' # Groups are respected
#' hpc_cv |>
#'   group_by(Resample) |>
#'   roc_aunu(obs, VF:L)
#'
#' # Vector version
#' # Supply a matrix of class probabilities
#' fold1 <- hpc_cv |>
#'   filter(Resample == "Fold01")
#'
#' roc_aunu_vec(
#'   truth = fold1$obs,
#'   matrix(
#'     c(fold1$VF, fold1$F, fold1$M, fold1$L),
#'     ncol = 4
#'   )
#' )
#' @export
roc_aunu <- function(data, ...) {
  UseMethod("roc_aunu")
}
roc_aunu <- new_prob_metric(
  roc_aunu,
  direction = "maximize"
)

#' @export
#' @rdname roc_aunu
roc_aunu.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  case_weights = NULL,
  options = list()
) {
  check_roc_options_deprecated("roc_aunu", options)

  prob_metric_summarizer(
    name = "roc_aunu",
    fn = roc_aunu_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    estimator = NULL,
    na_rm = na_rm,
    event_level = NULL,
    case_weights = !!enquo(case_weights)
  )
}

#' @rdname roc_aunu
#' @export
roc_aunu_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  options = list(),
  ...
) {
  abort_if_class_pred(truth)

  check_roc_options_deprecated("roc_aunu_vec", options)

  estimator <- "macro"

  check_prob_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  # `event_level` doesn't really matter, but we set it anyways
  roc_auc_vec(
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    na_rm = FALSE,
    event_level = "first",
    case_weights = case_weights
  )
}
