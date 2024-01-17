#' Demographic parity
#'
#' @description
#' Demographic parity is satisfied when a model's predictions have the
#' same predicted positive rate across groups. A value of 0 indicates parity
#' across groups. Note that this definition does not depend on the true
#' outcome; the `truth` argument is included in outputted metrics
#' for consistency.
#'
#' `demographic_parity()` is calculated as the difference between the largest
#' and smallest value of [detection_prevalence()] across groups.
#'
#' Demographic parity is sometimes referred to as group fairness,
#' disparate impact, or statistical parity.
#'
#' See the "Measuring Disparity" section for details on implementation.
#'
#' @param by The column identifier for the sensitive feature. This should be an
#' unquoted column name referring to a column in the un-preprocessed data.
#'
#' @templateVar fn demographic_parity
#' @templateVar internal_fn detection_prevalence
#' @template return-fair
#' @template event-fair
#' @template examples-fair
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
demographic_parity <-
  new_groupwise_metric(
    fn = detection_prevalence,
    name = "demographic_parity",
    aggregate = diff_range
  )
