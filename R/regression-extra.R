#' Extra Metrics for Numeric Outcomes
#'
#' These functions are appropriate for cases where the model
#'  outcome is a number. The ratio of performance to deviation
#'  (`rpd()`) and the ratio of performance to inter-quartile (`rpiq()`)
#'  are both measures of consistency/correlation between observed
#'  and predicted values (and not of accuracy).
#'
#' @inheritParams rmse
#'
#' @inherit sens return
#'
#' @author Pierre Roudier
#'
#' @details In the field of spectroscopy in particular, the ratio
#'  of performance to deviation (RPD) has been used as the standard
#'  way to report the quality of a model. It is the ratio between
#'  the standard deviation of a variable and the standard error of
#'  prediction of that variable by a given model. However, its
#'  systematic use has been criticized by several authors, since
#'  using the standard deviation to represent the spread of a
#'  variable can be misleading on skewed dataset. The ratio of
#'  performance to inter-quartile has been introduced by
#'  Bellon-Maurel et al. (2010) to address some of these issues, and
#'  generalise the RPD to non-normally distributed variables.
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
#' @keywords manip
#' @examples
#'
#' rpd(solubility_test, truth = solubility, estimate = prediction)
#' rpiq(solubility_test, truth = solubility, estimate = prediction)
#'
#' @export
#' @rdname rpd
rpd <- function(data, ...) {
  UseMethod("rpd")
}

#' @rdname rpd
#' @export
rpd.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "rpd",
    metric_fn = rpd_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname rpd
#' @importFrom stats sd
rpd_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  rpd_impl <- function(truth, estimate) {

    sd(truth) / rmse_vec(truth, estimate)

  }

  metric_vec_template(
    metric_impl = rpd_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "numeric",
    ...
  )

}

#' @export
#' @rdname rpd
rpiq <- function(data, ...) {
  UseMethod("rpiq")
}

#' @rdname rpd
#' @export
rpiq.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "rpiq",
    metric_fn = rpiq_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname rpd
#' @importFrom stats IQR
rpiq_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  rpiq_impl <- function(truth, estimate) {

    IQR(truth) / rmse_vec(truth, estimate)

  }

  metric_vec_template(
    metric_impl = rpiq_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "numeric",
    ...
  )

}
