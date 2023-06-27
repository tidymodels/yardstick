max_positive_rate_diff <- function(x) {
  metric_values <- vec_split(x, x$.metric)

  positive_rate_diff <- vapply(metric_values$val, diff_range, numeric(1))

  max(positive_rate_diff)
}

#' Equalized odds
#'
#' @description
#'
#' Equalized odds is satisfied when a model's predictions have the same false
#' positive, true positive, false negative, and true negative rates across
#' protected groups. A value of 0 indicates parity across groups.
#'
#' By default, this function takes the maximum difference in range of [sens()]
#' and [spec()] `.estimate`s across groups. That is, the maximum pair-wise
#' disparity in [sens()] or [spec()] between groups is the return value of
#' `equalized_odds()`'s `.estimate`.
#'
#' Equalized odds is sometimes referred to as conditional procedure accuracy
#' equality or disparate mistreatment.
#'
#' See the "Measuring disparity" section for details on implementation.
#'
#' @inheritParams demographic_parity
#'
#' @templateVar fn equalized_odds
#' @templateVar internal_fn [sens()] and [spec()]
#' @template return-fair
#' @template examples-fair
#'
#' @section Measuring Disparity:
#' For finer control of group treatment, construct a context-aware fairness
#' metric with the [new_groupwise_metric()] function by passing a custom `aggregate`
#' function:
#'
#' ```
#' # see yardstick:::max_positive_rate_diff for the actual `aggregate()`
#' diff_range <- function(x, ...) {diff(range(x$.estimate))}
#'
#' equalized_odds_2 <-
#'   new_groupwise_metric(
#'     fn = metric_set(sens, spec),
#'     name = "equalized_odds_2",
#'     aggregate = diff_range
#'   )
#' ```
#'
#' In `aggregate()`, `x` is the [metric_set()] output with [sens()] and [spec()]
#' values for each group, and `...` gives additional arguments (such as a grouping
#' level to refer to as the "baseline") to pass to the function outputted
#' by `equalized_odds_2()` for context.
#'
#' @family fairness metrics
#'
#' @references
#'
#' Agarwal, A., Beygelzimer, A., Dudik, M., Langford, J., & Wallach, H. (2018).
#' "A Reductions Approach to Fair Classification." Proceedings of the 35th
#' International Conference on Machine Learning, in Proceedings of Machine
#' Learning Research. 80:60-69.
#'
#' Verma, S., & Rubin, J. (2018). "Fairness definitions explained". In
#' Proceedings of the international workshop on software fairness (pp. 1-7).
#'
#' Bird, S., Dudík, M., Edgar, R., Horn, B., Lutz, R., Milan, V., ... & Walker,
#' K. (2020). "Fairlearn: A toolkit for assessing and improving fairness in AI".
#' Microsoft, Tech. Rep. MSR-TR-2020-32.
#'
#' @export
equalized_odds <-
  new_groupwise_metric(
    fn = metric_set(sens, spec),
    name = "equalized_odds",
    aggregate = max_positive_rate_diff
  )
