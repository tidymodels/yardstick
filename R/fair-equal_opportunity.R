#' Equal opportunity
#'
#' @description
#'
#' Equal opportunity is satisfied when a model's predictions have the same
#' true positive and false negative rates across protected groups. A value of
#' 0 indicates parity across groups.
#'
#' `equal_opportunity()` is calculated as the difference between the largest
#' and smallest value of [sens()] across groups.
#'
#' Equal opportunity is sometimes referred to as equality of opportunity.
#'
#' See the "Measuring Disparity" section for details on implementation.
#'
#' @inheritParams demographic_parity
#'
#' @templateVar fn equal_opportunity
#' @templateVar internal_fn sens
#' @template return-fair
#' @template event-fair
#' @template examples-fair
#'
#' @family fairness metrics
#'
#' @references
#'
#' Hardt, M., Price, E., & Srebro, N. (2016). "Equality of opportunity in
#' supervised learning". Advances in neural information processing systems, 29.
#'
#' Verma, S., & Rubin, J. (2018). "Fairness definitions explained". In
#' Proceedings of the international workshop on software fairness (pp. 1-7).
#'
#' Bird, S., Dudík, M., Edgar, R., Horn, B., Lutz, R., Milan, V., ... & Walker,
#' K. (2020). "Fairlearn: A toolkit for assessing and improving fairness in AI".
#' Microsoft, Tech. Rep. MSR-TR-2020-32.
#'
#' @export
equal_opportunity <-
  new_groupwise_metric(
    fn = sens,
    name = "equal_opportunity",
    aggregate = diff_range
  )
