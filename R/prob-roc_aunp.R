#' Area under the ROC curve of each class against the rest, using the a priori
#' class distribution
#'
#' `roc_aunp()` is a multiclass metric that computes the area under the ROC
#' curve of each class against the rest, using the a priori class distribution.
#' This is equivalent to `roc_auc(estimator = "macro_weighted")`.
#'
#' @family class probability metrics
#' @templateVar metric_fn roc_aunp
#' @template return
#' @template event_first
#'
#' @section Multiclass:
#' This multiclass method for computing the area under the ROC curve uses the
#' a priori class distribution and is equivalent to
#' `roc_auc(estimator = "macro_weighted")`.
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
#' [roc_aunu()] for computing the area under the ROC curve of each class against
#' the rest, using the uniform class distribution.
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
#'   roc_aunp(obs, VF:L)
#'
#' # Change the first level of `obs` from `"VF"` to `"M"` to alter the
#' # event of interest. The class probability columns should be supplied
#' # in the same order as the levels.
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   mutate(obs = relevel(obs, "M")) %>%
#'   roc_aunp(obs, M, VF:L)
#'
#' # Groups are respected
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   roc_aunp(obs, VF:L)
#'
#' # Vector version
#' # Supply a matrix of class probabilities
#' fold1 <- hpc_cv %>%
#'   filter(Resample == "Fold01")
#'
#' roc_aunp_vec(
#'   truth = fold1$obs,
#'   matrix(
#'     c(fold1$VF, fold1$F, fold1$M, fold1$L),
#'     ncol = 4
#'   )
#' )
#' @export
roc_aunp <- function(data, ...) {
  UseMethod("roc_aunp")
}
roc_aunp <- new_prob_metric(
  roc_aunp,
  direction = "maximize"
)

#' @export
#' @rdname roc_aunp
roc_aunp.data.frame <- function(data,
                                truth,
                                ...,
                                na_rm = TRUE,
                                case_weights = NULL,
                                options = list()) {
  check_roc_options_deprecated("roc_aunp", options)

  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "roc_aunp",
    metric_fn = roc_aunp_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    estimator = NULL,
    na_rm = na_rm,
    event_level = NULL,
    case_weights = !!enquo(case_weights)
  )
}

#' @rdname roc_aunp
#' @export
roc_aunp_vec <- function(truth,
                         estimate,
                         na_rm = TRUE,
                         case_weights = NULL,
                         options = list(),
                         ...) {
  check_roc_options_deprecated("roc_aunp_vec", options)

  estimator <- "macro_weighted"

  # `event_level` doesn't really matter, but we set it anyways
  roc_aunp_impl <- function(truth,
                            estimate,
                            ...,
                            case_weights = NULL) {
    check_dots_empty()

    roc_auc_vec(
      truth = truth,
      estimate = estimate,
      estimator = estimator,
      na_rm = FALSE,
      event_level = "first",
      case_weights = case_weights
    )
  }

  metric_vec_template(
    metric_impl = roc_aunp_impl,
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    na_rm = na_rm,
    case_weights = case_weights,
    cls = c("factor", "numeric")
  )
}
