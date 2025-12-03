#' Compute weighted interval score
#'
#' Weighted interval score (WIS), a well-known quantile-based
#' approximation of the commonly-used continuous ranked probability score
#' (CRPS). WIS is a proper score, and can be thought of as a distributional
#' generalization of absolute error. For example, see [Bracher et
#' al. (2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
#' of COVID-19 forecasting.
#'
#' @param x A vector of class `quantile_pred`.
#' @param actual double. Actual value(s)
#' @param quantile_levels probabilities. If specified, the score will be
#'   computed at this set of levels. Otherwise, those present in `x` will be
#'   used.
#' @param na_handling character. Determines how missing values are handled.
#'   For `"impute"`, missing values will be
#'   calculated if possible using the available quantiles. For `"drop"`,
#'   explicitly missing values are ignored in the calculation of the score, but
#'   implicitly missing values are imputed if possible.
#'   For `"propogate"`, the resulting score will be `NA` if any missing values
#'   exist. Finally, if
#'   `quantile_levels` is specified, `"fail"` will result in
#'   the score being `NA` when any required quantile levels (implicit or explicit)
#'   do not have corresponding values.
#' @param ... not used
#'
#' @return a vector of nonnegative scores.
#'
#' @export
#' @examples
#' quantile_levels <- c(.2, .4, .6, .8)
#' predq1 <- 1:4 #
#' predq2 <- 8:11
#' dstn <- quantile_pred(rbind(predq1, predq2), quantile_levels)
#' actual <- c(3.3, 7.1)
#' weighted_interval_score(dstn, actual)
#' weighted_interval_score(dstn, actual, c(.25, .5, .75))
#'
#' # Missing value behaviours
#' dstn <- quantile_pred(matrix(c(1, 2, NA, 4), nrow = 1), 1:4 / 5)
#' weighted_interval_score(dstn, 2.5)
#' weighted_interval_score(dstn, 2.5, 1:9 / 10)
#' weighted_interval_score(dstn, 2.5, 1:9 / 10, na_handling = "drop")
#' weighted_interval_score(dstn, 2.5, na_handling = "propagate")
#' weighted_interval_score(
#'   quantile_pred(matrix(1:4, nrow = 1), 1:4 / 5),
#'   actual = 2.5,
#'   quantile_levels = 1:9 / 10,
#'   na_handling = "fail"
#' )
#'
#'
weighted_interval_score <- function(data, ...) {
  UseMethod("weighted_interval_score")
}
weighted_interval_score <- new_numeric_metric(
  mae,
  direction = "minimize"
)

#' @export
#' @rdname weighted_interval_score
weighted_interval_score_vec <- function(
  truth,
  estimate,
  quantile_levels = NULL,
  na_rm = TRUE,
  na_impute = c("none", "explicit", "any"),
  case_weights = NULL,
  ...
) {
  check_quantile_metric(truth, estimate, case_weights)
  estimate_quantile_levels <- hardhat::extract_quantile_levels(estimate)
  na_impute <- rlang::arg_match(na_impute)

  # If we requested particular levels, but we don't impute, and the levels
  # aren't all there, then return NA
  if (!is.null(quantile_levels)) {
    hardhat::check_quantile_levels(quantile_levels)
    if (
      na_impute == "none" &&
        !(all(quantile_levels %in% estimate_quantile_levels))
    ) {
      return(NA_real_)
    }
  }
  quantile_levels <- quantile_levels %||% estimate_quantile_levels

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  wis_impl(
    truth = truth,
    estimate = estimate,
    quantile_levels = quantile_levels,
    na_impute = na_impute,
    case_weights = case_weights
  )
}

wis_impl <- function(
  truth,
  estimate,
  quantile_levels = NULL,
  na_impute = c("none", "explicit", "any"),
  case_weights = NULL,
  ...
) {
  estimate <- impute_quantiles(
    estimate,
    quantile_levels,
    replace_na = (na_handling == "impute")
  )
  estimate <- as.matrix(estimate)[,
    attr(x, "quantile_levels") %in% quantile_levels,
    drop = FALSE
  ]
  na_rm <- (na_handling == "drop")
  errors <- as.vector(
    mapply(
      FUN = function(.x, .y) wis_one_quantile(.x, quantile_levels, .y, na_rm),
      vctrs::vec_chop(estimate),
      truth
    ),
    "double"
  )

  yardstick_mean(vec_score, case_weights = case_weights)
}


wis_one_quantile <- function(q, tau, actual, na_rm) {
  2 * mean(pmax(tau * (actual - q), (1 - tau) * (q - actual)), na.rm = na_rm)
}
