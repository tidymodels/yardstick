#' Compute weighted interval score
#'
#' Weighted interval score (WIS), a well-known quantile-based approximation of
#' the commonly-used continuous ranked probability score (CRPS). WIS is a proper
#' score, and can be thought of as a distributional generalization of absolute
#' error. For example, see
#' [Bracher et al. (2020)](https://arxiv.org/abs/2005.12881) for discussion in
#' the context of COVID-19 forecasting.
#'
#' @param data A `data.frame` containing the columns specified by the `truth`
#'   and `estimate` arguments.
#'
#' @param truth The column identifier for the true class results
#'   (that is a `numeric`). This should be an unquoted column name although
#'   this argument is passed by expression and supports
#'   [quasiquotation][rlang::quasiquotation] (you can unquote column
#'   names). For `_vec()` functions, a `factor` vector.
#'
#' @param estimate The column identifier for the predicted class results
#'   (that is also `quantile_pred`). As with `truth` this can be specified
#'   different ways but the primary method is to use an unquoted variable name.
#'   For `_vec()` functions, a `quantile_pred` vector.
#'
#' @param quantile_levels probabilities. If specified, the score will be
#'   computed at this set of levels. Otherwise, those present in `x` will be
#'   used. If `quantile_levels` do not exactly match those available in `x`,
#'   then some quantiles will have implicit missingness. Handling of these is
#'   determined by `quantile_estimate_nas`.
#'
#' @param quantile_estimate_nas character. This argument applies only to `x`.
#'   It handles imputation of individual `quantile_levels` that are necessary to
#'   compute a score. Because each element of `x` is a [hardhat::quantile_pred],
#'   it is possible for these to be missing for particular
#'   `quantile_levels`. There are a number of different possibilities for such
#'   missingness. The options are as follows:
#'   * For `"impute"`, both explicit and implicit missing values will be imputed
#'   using [hardhat::impute_quantiles()] prior to the calculation of the score.
#'   So the score will be `NA` only if imputation fails.
#'   * For `"drop"`, any explicit missing values will be removed
#'   before calculating the score for a particular prediction. This may be
#'   reasonable due to the weighting. For example, if the estimate has
#'   `quantile_levels = c(.25, .5, .75)` but the median is `NA` for a particular
#'   prediction, it may be reasonable to average the accuracy of `c(.25, .75)`
#'   for that prediction with others that don't have missingness. This option
#'   is only works if `quantile_levels = NULL` or is a subset of the
#'   `quantile_levels` in `x`.
#'   * For `"propagate"`, any missing value predictions will result in that
#'   element of `x` having a score of `NA`. If `na_rm = TRUE`, then these will
#'   be removed before averaging.
#'
#' @param na_rm logical. If `TRUE`, missing values in `actual` or both implicit
#'   and explicit (values of `NA` present in `x`), will be ignored (dropped) in
#'   the calculation of the summary score. If `FALSE` (the default), any `NA`s
#'   will result in the summary being `NA`.
#'
#' @param case_weights The optional column identifier for case weights. This
#'   should be an unquoted column name that evaluates to a numeric column in
#'   `data`. For `_vec()` functions, a numeric vector,
#'   [hardhat::importance_weights()], or [hardhat::frequency_weights()].
#'
#' @param ... not used
#'
#' @return a vector of nonnegative scores.
#'
#' @export
#' @examples
#' library(hardhat)
#'
#' quantile_levels <- c(.2, .4, .6, .8)
#' pred1 <- 1:4
#' pred2 <- 8:11
#' preds <- quantile_pred(rbind(pred1, pred2), quantile_levels)
#' truth <- c(3.3, 7.1)
#' weighted_interval_score_vec(truth, preds)
#' weighted_interval_score_vec(truth, preds, quantile_levels = c(.25, .5, .75))
#'
#' # Missing value behaviours
#'
#' preds_na <- quantile_pred(rbind(pred1, c(1, 2, NA, 4)), 1:4 / 5)
#' truth <- c(2.5, 2.5)
#' weighted_interval_score_vec(truth, preds_na)
#' weighted_interval_score_vec(truth, preds_na, quantile_levels = 1:9 / 10)
#' try(weighted_interval_score_vec(
#'   truth,
#'   preds_na,
#'   quantile_levels = 1:9 / 10,
#'   quantile_estimate_nas = "drop"
#' ))
#' weighted_interval_score_vec(
#'   truth,
#'   preds_na,
#'   quantile_levels = c(2, 3) / 5,
#'   quantile_estimate_nas = "drop"
#' )
#' weighted_interval_score_vec(
#'   truth, preds_na, na_rm = TRUE, quantile_estimate_nas = "propagate"
#' )
#' weighted_interval_score_vec(
#'   truth, preds_na, quantile_estimate_nas = "propagate"
#' )
#'
weighted_interval_score <- function(data, ...) {
  UseMethod("weighted_interval_score")
}
weighted_interval_score <- new_numeric_metric(
  weighted_interval_score,
  direction = "minimize"
)

#' @export
#' @rdname weighted_interval_score
weighted_interval_score_vec <- function(
  truth,
  estimate,
  quantile_levels = NULL,
  na_rm = FALSE,
  quantile_estimate_nas = c("impute", "drop", "propagate"),
  case_weights = NULL,
  ...
) {
  check_quantile_metric(truth, estimate, case_weights)
  estimate_quantile_levels <- hardhat::extract_quantile_levels(estimate)
  quantile_estimate_nas <- rlang::arg_match(quantile_estimate_nas)
  if (!is.null(quantile_levels)) {
    hardhat::check_quantile_levels(quantile_levels)
    all_levels_estimated <- all(quantile_levels %in% estimate_quantile_levels)
    if (quantile_estimate_nas == "drop" && !all_levels_estimated) {
      cli::cli_abort(
        "When `quantile_levels` is not a subset of those available in `estimate`, 
      `quantile_estimate_nas` may not be `'drop'`."
      )
    }
    if (!all_levels_estimated && (quantile_estimate_nas == "propagate")) {
      # We requested particular levels, but the levels aren't all there,
      # and NAs propagate, so return NA
      return(NA_real_)
    }
  }

  quantile_levels <- quantile_levels %||% estimate_quantile_levels
  if (quantile_estimate_nas %in% c("drop", "propagate")) {
    levels_estimated <- estimate_quantile_levels %in% quantile_levels
    estimate <- as.matrix(estimate)[, levels_estimated, drop = FALSE]
  } else {
    estimate <- as.matrix(hardhat::impute_quantiles(estimate, quantile_levels))
  }

  vec_wis <- wis_impl(
    truth = truth,
    estimate = estimate,
    quantile_levels = quantile_levels,
    rowwise_na_rm = (quantile_estimate_nas == "drop")
  )

  if (na_rm) {
    result <- yardstick_remove_missing(truth, vec_wis, case_weights)

    truth <- result$truth
    vec_wis <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, vec_wis, case_weights)) {
    return(NA_real_)
  }

  yardstick_mean(vec_wis, case_weights = case_weights)
}

wis_impl <- function(
  truth,
  estimate,
  quantile_levels,
  rowwise_na_rm = TRUE
) {
  as.vector(
    mapply(
      FUN = function(.x, .y) {
        wis_one_quantile(.x, quantile_levels, .y, rowwise_na_rm)
      },
      vctrs::vec_chop(estimate),
      truth
    ),
    "double"
  )
}

wis_one_quantile <- function(values, quantile_levels, truth, na_rm) {
  2 *
    mean(
      pmax(
        quantile_levels * (truth - values),
        (1 - quantile_levels) * (values - truth)
      ),
      na.rm = na_rm
    )
}
