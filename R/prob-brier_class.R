#' Brier score for classification models
#'
#' Compute the Brier score for a classification model.
#'
#' @family class probability metrics
#' @templateVar fn brier_class
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
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   brier_class(obs, VF:L)
#'
#' # Groups are respected
#' hpc_cv |>
#'   group_by(Resample) |>
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
brier_class.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  case_weights = NULL
) {
  case_weights_quo <- enquo(case_weights)

  prob_metric_summarizer(
    name = "brier_class",
    fn = brier_class_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    case_weights = !!case_weights_quo
  )
}

#' @rdname brier_class
#' @export
brier_class_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  abort_if_class_pred(truth)

  estimator <- finalize_estimator(truth, metric_class = "brier_class")

  check_prob_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  brier_class_estimator_impl(
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )
}

brier_class_estimator_impl <- function(truth, estimate, case_weights) {
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
  if (ncol(estimate) == 1 && ncol(truth) == 2) {
    estimate <- unname(estimate)
    estimate <- vec_cbind(estimate, 1 - estimate, .name_repair = "unique_quiet")
  }

  resids <- (truth - estimate)^2

  if (is.null(case_weights)) {
    case_weights <- rep(1, nrow(resids))
  }

  not_missing <- !is.na(case_weights)
  resids <- resids[not_missing, , drop = FALSE]
  case_weights <- case_weights[not_missing]

  # Normalize weights (in case negative weights)
  case_weights <- exp(case_weights) / sum(exp(case_weights))

  res <- sum(resids * case_weights) / (2 * sum(case_weights))
  res
}

# When `truth` is a factor
brier_factor <- function(truth, estimate, case_weights = NULL) {
  inds <- hardhat::fct_encode_one_hot(truth)

  case_weights <- vctrs::vec_cast(case_weights, to = double())

  brier_ind(inds, estimate, case_weights)
}
