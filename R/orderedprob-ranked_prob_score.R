#' Ranked probability scores for ordinal classification models
#'
#' Compute the ranked probability score (RPS) for a classification model using
#' ordered classes.
#'
#' @param truth The column identifier for the true class results
#'   (that is an _ordered_ `factor`). This should be an unquoted column name
#'   although this argument is passed by expression and supports
#'   [quasiquotation][rlang::quasiquotation] (you can unquote column names).
#'   For `_vec()` functions, a factor vector with class `ordered`.
#' @param estimate A matrix with as many columns as factor levels of `truth`. _It
#' is assumed that these are in the same order as the levels of `truth`._
#' @family class probability metrics
#' @templateVar fn ranked_prob_score
#' @template return
#' @details
#'
#' The ranked probability score is a Brier score for ordinal data that uses the
#' _cumulative_ probability of an event (i.e. `Pr[class <= i]` for `i` = 1,
#' 2, ..., `C` classes). These probabilities are compared to indicators for the
#' truth being less than or equal to class `i`.
#'
#' Since the cumulative sum of a vector of probability predictions add up to
#' one, there is an embedded redundancy in the data. For this reason, the raw
#' mean is divided by the number of classes minus one.
#'
#' Smaller values of the score are associated with better model performance.
#'
#' @section Multiclass:
#' Ranked probability scores can be computed in the same way for any number of
#' classes. Because of this, no averaging types are supported.
#'
#' @inheritParams pr_auc
#'
#' @author Max Kuhn
#'
#' @references
#'
#' Wilks, D. S. (2011). _Statistical Methods in the Atmospheric Sciences_.
#' Academic press. (see Chapter 7)
#'
#' Janitza, S., Tutz, G., & Boulesteix, A. L. (2016). Random forest for ordinal
#' responses: prediction and variable selection. Computational Statistics and
#' Data Analysis, 96, 57-73. (see Section 2)
#'
#' Lechner, M., & Okasa, G. (2019). Random forest estimation of the ordered
#' choice model. arXiv preprint arXiv:1907.02436. (see Section 5)
#'
#' @examples
#' library(dplyr)
#' data(hpc_cv)
#'
#' hpc_cv$obs <- as.ordered(hpc_cv$obs)
#'
#' # You can use the col1:colN tidyselect syntax
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   ranked_prob_score(obs, VF:L)
#'
#' # Groups are respected
#' hpc_cv |>
#'   group_by(Resample) |>
#'   ranked_prob_score(obs, VF:L)
#'
#' @export
ranked_prob_score <- function(data, ...) {
  UseMethod("ranked_prob_score")
}
ranked_prob_score <- new_ordered_prob_metric(
  ranked_prob_score,
  direction = "minimize"
)

#' @export
#' @rdname ranked_prob_score
ranked_prob_score.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  case_weights = NULL
) {
  case_weights_quo <- enquo(case_weights)

  ordered_prob_metric_summarizer(
    name = "ranked_prob_score",
    fn = ranked_prob_score_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    case_weights = !!case_weights_quo
  )
}

#' @rdname ranked_prob_score
#' @export
ranked_prob_score_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  abort_if_class_pred(truth)

  estimator <- finalize_estimator(truth, metric_class = "ranked_prob_score")

  check_ordered_prob_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  ranked_prob_score_estimator_impl(
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )
}

ranked_prob_score_estimator_impl <- function(truth, estimate, case_weights) {
  rps_factor(
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )
}

cumulative_rows <- function(x) {
  t(apply(x, 1, cumsum))
}

# When `truth` is a factor
rps_factor <- function(truth, estimate, case_weights = NULL) {
  num_class <- length(levels(truth))
  inds <- hardhat::fct_encode_one_hot(truth)
  cum_ind <- cumulative_rows(inds)
  cum_estimate <- cumulative_rows(estimate)

  case_weights <- vctrs::vec_cast(case_weights, to = double())

  # RPS divides by the number of classes minus one since the cumulative
  # probabilities always sum to one and this "pads" the differences by 1, .i.e
  # there are num_class - 1 independent pieces of information.
  # Also brier_ind() divides the raw mean by 2 so we take that out
  brier_ind(cum_ind, cum_estimate, case_weights) / (num_class - 1) * 2
}
