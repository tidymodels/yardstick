#' Ratio of performance to deviation
#'
#' These functions are appropriate for cases where the model outcome is a
#' numeric. The ratio of performance to deviation
#' ([rpd()]) and the ratio of performance to inter-quartile ([rpiq()])
#' are both measures of consistency/correlation between observed
#' and predicted values (and not of accuracy).
#'
#' In the field of spectroscopy in particular, the ratio
#' of performance to deviation (RPD) has been used as the standard
#' way to report the quality of a model. It is the ratio between
#' the standard deviation of a variable and the standard error of
#' prediction of that variable by a given model. However, its
#' systematic use has been criticized by several authors, since
#' using the standard deviation to represent the spread of a
#' variable can be misleading on skewed dataset. The ratio of
#' performance to inter-quartile has been introduced by
#' Bellon-Maurel et al. (2010) to address some of these issues, and
#' generalise the RPD to non-normally distributed variables.
#'
#' @family numeric metrics
#' @family consistency metrics
#' @templateVar fn rpd
#' @template return
#'
#' @inheritParams rmse
#'
#' @author Pierre Roudier
#'
#' @seealso
#'
#' The closely related inter-quartile metric: [rpiq()]
#'
#' @references
#'
#' Williams, P.C. (1987) Variables affecting near-infrared
#'  reflectance spectroscopic analysis. In: Near Infrared Technology
#'  in the Agriculture and Food Industries. 1st Ed. P.Williams and
#'  K.Norris, Eds. Am. Cereal Assoc. Cereal Chem., St. Paul, MN.
#'
#' Bellon-Maurel, V., Fernandez-Ahumada, E., Palagos, B., Roger,
#'  J.M. and McBratney, A., (2010). Critical review of chemometric
#'  indicators commonly used for assessing the quality of the
#'  prediction of soil attributes by NIR spectroscopy. TrAC Trends
#'  in Analytical Chemistry, 29(9), pp.1073-1081.
#'
#' @template examples-numeric
#'
#' @export
rpd <- function(data, ...) {
  UseMethod("rpd")
}
rpd <- new_numeric_metric(
  rpd,
  direction = "maximize"
)

#' @rdname rpd
#' @export
rpd.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "rpd",
    fn = rpd_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname rpd
rpd_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  rpd_impl(truth, estimate, case_weights)
}

rpd_impl <- function(truth, estimate, case_weights) {
  sd <- yardstick_sd(truth, case_weights = case_weights)
  rmse <- rmse_vec(truth, estimate, case_weights = case_weights)

  sd / rmse
}
