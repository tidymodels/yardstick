#' Brier score for classification models
#'
#' Compute the Brier score for a classification model.
#'
#' @family class probability metrics
#' @templateVar metric_fn brier_class
#' @template return
#' @details
#'
#' The Brier score is analogous to the mean squared error in regression models.
#' The difference between a binary indicator for a class and its corresponding
#' class probability are squared and averaged.
#'
#' This function uses the convention in Kruppa _et al_ (2014) and divides the
#' result by two.
#'
#' Smaller values of the score are associated with better model performance.
#'
#' @section Multiclass:
#' Brier scores can be computed in the same way for any number of classes.
#' Because of this, no averaging types are supported.
#'
#' @inheritParams pr_auc
#'
#' @author Max Kuhn
#'
#' @references Kruppa, J., Liu, Y., Diener, H.-C., Holste, T., Weimar, C.,
#' Koonig, I. R., and Ziegler, A. (2014) Probability estimation with machine
#' learning methods for dichotomous and multicategory outcome: Applications.
#' Biometrical Journal, 56 (4): 564-583.
#' @examples
#' # Two class
#' data("two_class_example")
#' brier_class(two_class_example, truth, Class1)
#'
#' # Multiclass
#' library(dplyr)
#' data(hpc_cv)
#'
#' # You can use the col1:colN tidyselect syntax
#' hpc_cv %>%
#'   filter(Resample == "Fold01") %>%
#'   brier_class(obs, VF:L)
#'
#' # Groups are respected
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   brier_class(obs, VF:L)
#'
#' @export
brier_class <- function(data, ...) {
  UseMethod("brier_class")
}
brier_class <- new_prob_metric(
  brier_class,
  direction = "minimize"
)

#' @export
#' @rdname brier_class
brier_class.data.frame <- function(data,
                                   truth,
                                   ...,
                                   na_rm = TRUE,
                                   case_weights = NULL) {
  estimate <- dots_to_estimate(data, !!! enquos(...))

  metric_summarizer(
    metric_nm = "brier_class",
    metric_fn = brier_class_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @rdname brier_class
#' @export
brier_class_vec <- function(truth,
                            estimate,
                            na_rm = TRUE,
                            case_weights = NULL,
                            ...) {
  estimator <- finalize_estimator(truth, metric_class = "brier_class")

  # estimate here is a matrix of class prob columns
  brier_class_impl <- function(truth,
                               estimate,
                               ...,
                               case_weights = NULL) {
    check_dots_empty()

    brier_class_estimator_impl(
      truth = truth,
      estimate = estimate,
      case_weights = case_weights
    )
  }

  metric_vec_template(
    metric_impl = brier_class_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    case_weights = case_weights,
    cls = c("factor", "numeric")
  )
}

brier_class_estimator_impl <- function(truth,
                                       estimate,
                                       case_weights) {
  brier_factor(
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )
}

# If `truth` is already a vector or matrix of binary data
brier_ind <- function(truth, estimate, case_weights = NULL) {
  if (is.vector(truth)) {
    truth <- matrix(truth, ncol = 1)
  }

  if (is.vector(estimate)) {
    estimate <- matrix(estimate, ncol = 1)
  }
  # In the binary case:
  if (ncol(estimate) == 1 & ncol(truth) == 2) {
    estimate <- cbind(estimate, 1 - estimate)
  }

  resids <- (truth - estimate)^2

  if (is.null(case_weights)) {
    case_weights <- rep(1, nrow(resids))
  }

  not_missing <- complete.cases(resids) & !is.na(case_weights)
  resids <- resids[not_missing,, drop = FALSE]
  case_weights <- case_weights[not_missing]

  # Normalize weights (in case negative weights)
  case_weights <- exp(case_weights) / sum(exp(case_weights))

  res <- sum(resids * case_weights) / (2 * sum(case_weights) )
  res
}

# When `truth` is a factor
brier_factor <- function(truth, estimate, case_weights = NULL) {
  if (!is.factor(truth)) {
    rlang::abort("'truth' should be a factor.")
  }
  inds <- stats::model.matrix(~ . -1, data = data.frame(y = truth))
  colnames(inds) <- levels(truth)
  brier_ind(inds, estimate, case_weights)
}

